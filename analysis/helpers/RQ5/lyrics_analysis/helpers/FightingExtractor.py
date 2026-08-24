"""
Monroe et al. (2008) n-gram feature extraction with fighting words method.

Implements discriminating n-gram selection using Bayesian-smoothed log-odds
ratios with empirical Bayes priors estimated from the full corpus.

Reference:
    Monroe, B. L., Colaresi, M. P., & Quinn, K. M. (2008).
    Fightin' Words: Lexical Feature Selection and Evaluation for
    Identifying the Content of Political Conflict.
    Political Analysis, 16(4), 372-403.
"""

import pandas as pd
import numpy as np
import pickle
import re

from functools import partial
from tqdm.auto import tqdm
from pathlib import Path
from joblib import hash as joblib_hash
from typing import Tuple
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.base import BaseEstimator, TransformerMixin

from .extractor_utils import (
    _boundary_aware_analyzer,
    count_artists_per_ngram,
    extract_ngrams,
    strip_boundary_ngrams,
)
from .StopwordFilter import StopwordFilter
from .monroe_logodds import (
    compute_monroe_statistics,
    compute_pvalues_from_zscores,
    apply_benjamini_hochberg_correction,
)


class FightingExtractor(BaseEstimator, TransformerMixin):
    """
    Monroe et al. n-gram extractor with fighting words z-scores.

    Extracts unigrams, bigrams, trigrams, and quadgrams using Dirichlet-smoothed log-odds
    ratios. Uses empirical Bayes prior estimated from full corpus frequencies.
    Checkpoints z-scores for all n-grams to allow p-value threshold exploration.

    Parameters
    ----------
    min_artists : int, default=MIN_ARTISTS
        Minimum number of unique artists that must use an n-gram for inclusion.
    p_value : float, default=0.01
        Significance level for one-sided z-test (FDR-corrected).
    prior_concentration : float, default=0.01
        Dirichlet prior strength (alpha). Lower values = stronger smoothing.
    use_stopword_filter : bool, default=ENABLE_STOPWORD_FILTER
        Whether to filter stopword-only n-grams.
    use_bigram_boundary_filter : bool, default=ENABLE_BIGRAM_BOUNDARY_FILTER
        Whether to filter bigrams that are subsets of unigrams.
    min_char : int, default=0
        Minimum character length for 1-grams to be included (default 0 for all).
    ngram_types : Tuple, default=(1, 2, 3, 4)
        Which types of ngrams to include in the extraction (default is maximum available).
    random_state : int, default=42
        Random seed for reproducibility.
    checkpoint_dir : str or Path, optional
        Directory to store checkpoints. If None, no checkpointing.

    Attributes
    ----------
    vocabulary_ : list of str
        Selected n-gram vocabulary passing significance threshold.
    vectorizer_ : CountVectorizer
        Fitted vectorizer for transforming new data.
    z_scores_df_ : pd.DataFrame
        All computed z-scores with columns ['ngram', 'genre', 'z_score',
        'p', 'passes_bh', 'bh_threshold'] - checkpointed for FDR exploration.
    _cache_key : str or None
        Joblib hash of input data for checkpointing (computed during fit).
    _is_fitted : bool
        Whether the extractor has been fitted.
    """

    def __init__(
        self,
        min_artists: int = 2,
        p_value: float = 0.001,
        prior_concentration: float = 1.0,
        use_stopword_filter: bool = True,
        use_bigram_boundary_filter: bool = True,
        min_char: int = 0,
        ngram_types: Tuple = (1, 2, 3, 4),
        random_state: int = 42,
        checkpoint_dir: str = None,
    ):
        self.min_artists = min_artists
        self.p_value = p_value
        self.prior_concentration = prior_concentration
        self.ngram_types = ngram_types
        self.min_char = min_char
        self.use_stopword_filter = use_stopword_filter
        self.use_bigram_boundary_filter = use_bigram_boundary_filter
        self.random_state = random_state
        self.checkpoint_dir = Path(checkpoint_dir) if checkpoint_dir else None
        self._is_fitted = False

        if self.use_stopword_filter:
            self.stopword_filter_ = StopwordFilter()

        if self.checkpoint_dir:
            self.checkpoint_dir.mkdir(parents=True, exist_ok=True)

    def _apply_gluing(self, X_series, sig_ngrams):
        """Highly optimized string replacement for thousands of n-grams."""
        if not sig_ngrams:
            return X_series

        replace_dict = {ng: ng.replace(" ", "_") for ng in sig_ngrams}
        return self._batch_replace(X_series, replace_dict)

    def _apply_ungluing(self, X_series, failed_glues):
        """Reverts non-significant glued tokens back to their space-separated forms."""
        if not failed_glues:
            return X_series

        replace_dict = {ng: ng.replace("_", " ") for ng in failed_glues}
        return self._batch_replace(X_series, replace_dict)

    def _batch_replace(self, X_series, replace_dict):
        """Core batch regex replacement logic."""
        if not replace_dict:
            return X_series

        X_list = X_series.tolist()
        keys = list(replace_dict.keys())

        # Chunk to avoid python's maximum regex group limits
        chunk_size = 1000
        for i in tqdm(
            range(0, len(keys), chunk_size),
            desc="Applying text replacements",
            leave=False,
        ):
            chunk = keys[i : i + chunk_size]
            escaped = [re.escape(k) for k in chunk]
            pattern_str = rf"(?<!\w)(?:{'|'.join(escaped)})(?!\w)"
            compiled_re = re.compile(pattern_str)

            X_list = [
                compiled_re.sub(lambda m: replace_dict[m.group(0)], text)
                for text in X_list
            ]

        return pd.Series(X_list)

    def fit_transform(self, X, y, artist=None):
        """
        Learn vocabulary from training data, filter invalid tokens,
        and return the transformed count matrix.

        Parameters
        ----------
        X : pd.Series or array-like
            Lyrics text, one entry per track.
        y : pd.Series or np.ndarray
            Genre labels for each track.
        artist : pd.Series, optional
            Artist names for each track. Required for min_artists filtering.

        Returns
        -------
        X_transformed : scipy.sparse matrix
            The document-term matrix aligned with the final vocabulary.
        """
        if artist is None:
            raise ValueError(
                "FightingExtractor requires 'artist' metadata for min_artists filtering"
            )

        X = pd.Series(X).reset_index(drop=True)
        X_original = X.copy()
        y = pd.Series(y).reset_index(drop=True)
        artist = pd.Series(artist).reset_index(drop=True)

        # Handle NaN and empty texts before processing
        X = self._handle_nan_and_empty_texts(X)
        self._validate_empty_texts(X)

        self._cache_key = self._compute_cache_key(X, y, artist)

        if self.checkpoint_dir:
            if self._load_checkpoint():
                print(
                    f"Loaded checkpoints for iterative fitting: {self._cache_key[:8]}..."
                )
                self._is_fitted = True

                # Rebuild vectorizer
                self.vectorizer_ = CountVectorizer(
                    vocabulary=self.vocabulary_,
                    analyzer=partial(
                        _boundary_aware_analyzer,
                        orders=(1,),  # Only looking for unigrams in the glued text!
                    ),
                )
                return self.transform(X_original)
            print("No checkpoint found, computing iterative z-scores...")

        # Initialize tracking
        self.glue_operations_ = []
        self.unglue_operations_ = []
        all_zscores_list = []
        X_current = X.copy()

        # Mapping for order names
        order_to_name = {1: "unigrams", 2: "bigrams", 3: "trigrams", 4: "quadgrams"}

        # Iterate top-down
        for order in sorted(self.ngram_types, reverse=True):
            name = order_to_name.get(order, f"{order}-grams")
            print(f"\n--- Processing {name} (Order {order}) ---")

            mat, feats = extract_ngrams(
                X_current, order, name, self.random_state, boundary_aware=True
            )

            if len(feats) == 0:
                print(f"  No {name} extracted. Skipping.")
                continue

            # Filtering
            if order == 1 and self.min_char > 0:
                mask = np.array([len(ng) >= self.min_char for ng in feats])
                feats = feats[mask]
                mat = mat[:, mask]

            artist_counts = count_artists_per_ngram(artist, mat, feats)
            mask = np.array([artist_counts[ng] >= self.min_artists for ng in feats])
            feats = feats[mask]
            mat = mat[:, mask]
            print(f"  After min_artists filtering: {len(feats):,} left")

            if self.use_stopword_filter:
                kept_ngrams = self.stopword_filter_.filter_ngrams(set(feats))
                mask = np.array([ng in kept_ngrams for ng in feats])
                feats = feats[mask]
                mat = mat[:, mask]
                print(f"  After stopword filtering: {len(feats):,} left")

            if order == 2 and self.use_bigram_boundary_filter:
                bigram_tuples = [tuple(ng.split()) for ng in feats]
                kept_bigrams = strip_boundary_ngrams(bigram_tuples)
                kept_strings = set(" ".join(ng) for ng in kept_bigrams)
                # Keep bigrams that contain the glue character '_'
                kept_strings.update([ng for ng in feats if "_" in ng])

                mask = np.array([ng in kept_strings for ng in feats])
                feats = feats[mask]
                mat = mat[:, mask]
                print(
                    f"  After boundary filter (preserving glued): {len(feats):,} left"
                )

            # Filter single letters
            kept_ngrams = self._filter_disallowed_single_letters(set(feats))
            mask = np.array([ng in kept_ngrams for ng in feats])
            feats = feats[mask]
            mat = mat[:, mask]

            if len(feats) == 0:
                print(f"  No {name} left after filtering. Skipping.")
                continue

            print("  Computing z-scores and FDR...")
            df_order = self._compute_all_zscores(y, {name: mat}, {name: feats})

            # Apply FDR for this specific order
            num_genres = len(y.unique())
            p_matrix = df_order["p"].values.reshape(-1, num_genres)
            passes_bh, bh_thresh = apply_benjamini_hochberg_correction(
                p_matrix, fdr=self.p_value
            )
            df_order["passes_bh"] = passes_bh.flatten()
            df_order["bh_threshold"] = bh_thresh.flatten()

            all_zscores_list.append(df_order)
            significant = df_order[df_order["passes_bh"] & (df_order["z_score"] > 0)]

            if order > 1:
                # Group by ngram, get max absolute z-score across genres, then sort
                if not significant.empty:
                    sig_ngrams = (
                        significant.assign(abs_z=significant["z_score"].abs())
                        .groupby("ngram")["abs_z"]
                        .max()
                        .sort_values(ascending=False)
                        .index.tolist()
                    )
                    print(f"  Gluing {len(sig_ngrams):,} significant {name}...")
                    self.glue_operations_.append(sig_ngrams)
                    X_current = self._apply_gluing(X_current, sig_ngrams)
            else:
                self.vocabulary_ = significant["ngram"].unique().tolist()
                print(
                    f"  Final vocabulary size: {len(self.vocabulary_):,} unigrams (including glued)"
                )

                # Identify glued phrases that failed the final FDR test
                all_glued = [
                    ng.replace(" ", "_")
                    for chunk in self.glue_operations_
                    for ng in chunk
                ]
                self.unglue_operations_ = [
                    g for g in all_glued if g not in self.vocabulary_
                ]
                print(
                    f"  Identified {len(self.unglue_operations_):,} non-significant glued phrases to break up."
                )

        self.z_scores_df_ = pd.concat(all_zscores_list, ignore_index=True)

        self.vectorizer_ = CountVectorizer(
            vocabulary=self.vocabulary_,
            analyzer=partial(
                _boundary_aware_analyzer,
                orders=(1,),  # Only look for unigrams in the glued text
            ),
        )

        self._is_fitted = True

        # Evaluate exact transformations to find and remove empty/0-occurrence terms
        X_transformed = self.transform(X_original)
        col_sums = np.asarray(X_transformed.sum(axis=0)).flatten()

        valid_indices = [
            i
            for i, v in enumerate(self.vocabulary_)
            if v != "[empty]" and col_sums[i] > 0
        ]

        # Update vocabulary and vectorizer to only contain guaranteed valid terms
        self.vocabulary_ = [self.vocabulary_[i] for i in valid_indices]
        self.vectorizer_ = CountVectorizer(
            vocabulary=self.vocabulary_,
            analyzer=partial(
                _boundary_aware_analyzer,
                orders=(1,),
            ),
        )

        # Subset the return matrix to match the final vocabulary
        X_in_vocab_aligned = X_transformed[:, valid_indices]

        if self.checkpoint_dir:
            self._save_checkpoint()
            self._save_vocab_information(X_original)
            print(f"Saved checkpoint: {self._cache_key[:8]}...")

        return X_in_vocab_aligned

    def transform(self, X):
        """Transform lyrics to n-gram count matrix."""
        if not hasattr(self, "vocabulary_") or not self._is_fitted:
            raise ValueError("Must call fit() before transform()")

        X = pd.Series(X).reset_index(drop=True)
        X = self._handle_nan_and_empty_texts(X)

        for sig_ngrams in self.glue_operations_:
            X = self._apply_gluing(X, sig_ngrams)

        if hasattr(self, "unglue_operations_") and self.unglue_operations_:
            X = self._apply_ungluing(X, self.unglue_operations_)

        return self.vectorizer_.transform(X)

    def _compute_all_zscores(self, genres, matrices, features):
        """Compute Monroe z-scores for all n-grams across all genres.

        Implements multi-class extension of Monroe et al.'s method:
        - Empirical Bayes prior from corpus-wide frequencies
        - One-vs-rest comparison for each genre
        - FDR correction for multiple testing
        """
        unique_genres = sorted(genres.unique())
        num_genres = len(unique_genres)

        results = []
        for name in matrices.keys():
            matrix = matrices[name]
            ngrams = features[name]

            if len(ngrams) == 0:
                continue

            corpus_counts = np.array(matrix.sum(axis=0)).flatten()
            total_corpus_counts = corpus_counts.sum()

            self.priors = self.prior_concentration * (
                corpus_counts / total_corpus_counts
            )

            genre_matrices = np.zeros((len(ngrams), num_genres))
            genre_totals = np.zeros(num_genres)

            for idx, genre in enumerate(unique_genres):
                genre_mask = (genres == genre).values
                genre_matrix = matrix[genre_mask, :]
                genre_matrices[:, idx] = np.array(genre_matrix.sum(axis=0)).flatten()
                genre_totals[idx] = genre_matrices[:, idx].sum()

            _, _, z_scores = compute_monroe_statistics(
                genre_matrices,
                genre_totals,
                corpus_counts,
                total_corpus_counts,
                len(ngrams),
                self.priors,
            )

            for ng_idx, ngram in enumerate(ngrams):
                for g_idx, genre in enumerate(unique_genres):
                    results.append(
                        {
                            "ngram": ngram,
                            "genre": genre,
                            "z_score": z_scores[ng_idx, g_idx],
                        }
                    )

        df = pd.DataFrame(results)

        z_matrix = df["z_score"].values.reshape(-1, num_genres)
        p_values = compute_pvalues_from_zscores(z_matrix)
        df["p"] = p_values.flatten()

        passes_bh, bh_threshold = apply_benjamini_hochberg_correction(
            p_values, fdr=self.p_value
        )
        df["passes_bh"] = passes_bh.flatten()
        df["bh_threshold"] = bh_threshold.flatten()

        return df

    def _compute_cache_key(self, X, y, artist):
        """Compute hash for caching (excludes p_value for flexibility)."""
        data_tuple = (
            tuple(X.index),
            tuple(X.values),
            tuple(y.values),
            tuple(artist.values),
            self.min_artists,
            self.prior_concentration,
            self.use_stopword_filter,
            self.random_state,
        )
        return joblib_hash(data_tuple)

    def _get_checkpoint_paths(self):
        """Get path for z-scores checkpoint file."""
        zscores_path = self.checkpoint_dir / f"zscores_{self._cache_key}.pkl"
        return zscores_path

    def _save_checkpoint(self):
        """Save z-scores to checkpoint file."""
        zscores_path = self._get_checkpoint_paths()

        checkpoint_data = {
            "z_scores_df_": self.z_scores_df_,
            "glue_operations_": getattr(self, "glue_operations_", []),
            "unglue_operations_": getattr(self, "unglue_operations_", []),
            "vocabulary_": getattr(self, "vocabulary_", []),
        }

        with open(zscores_path, "wb") as f:
            pickle.dump(checkpoint_data, f)

    def _save_vocab_information(self, X_original):
        """Saves absolute and relative frequencies of n-grams to a CSV."""
        if not self.checkpoint_dir or not hasattr(self, "vocabulary_"):
            return

        # Get exact final token counts relying on our robust transform method
        X_counts = self.transform(X_original)
        term_counts = np.array(X_counts.sum(axis=0)).flatten()

        # Calculate lengths of each n-gram (1 + number of underscores)
        lengths = np.array([term.count("_") + 1 for term in self.vocabulary_])

        total_types = len(self.vocabulary_)
        total_tokens = term_counts.sum()

        rows = []
        rows.append({"type": "Total Types", "freq": total_types, "relfreq": 1.0})
        rows.append({"type": "Total Tokens", "freq": total_tokens, "relfreq": 1.0})

        max_len = lengths.max() if total_types > 0 else 0
        for k in range(1, max_len + 1):
            k_mask = lengths == k
            k_types = k_mask.sum()
            if k_types > 0:
                k_tokens = term_counts[k_mask].sum()
                rows.append(
                    {
                        "type": f"{k}-gram Types",
                        "freq": k_types,
                        "relfreq": k_types / total_types,
                    }
                )
                rows.append(
                    {
                        "type": f"{k}-gram Tokens",
                        "freq": k_tokens,
                        "relfreq": k_tokens / total_tokens if total_tokens > 0 else 0,
                    }
                )

        df_stats = pd.DataFrame(rows)
        csv_path = self.checkpoint_dir / f"vocab_stats_{self._cache_key}.csv"
        df_stats.to_csv(csv_path, index=False)
        print(f"  Saved vocabulary statistics to {csv_path.name}")

    def _load_checkpoint(self):
        """Load z-scores from checkpoint file if it exists."""
        zscores_path = self._get_checkpoint_paths()

        if zscores_path.exists():
            with open(zscores_path, "rb") as f:
                checkpoint_data = pickle.load(f)

            # Handle legacy format where just DF was stored
            if isinstance(checkpoint_data, pd.DataFrame):
                return False

            self.z_scores_df_ = checkpoint_data["z_scores_df_"]
            self.glue_operations_ = checkpoint_data["glue_operations_"]
            self.unglue_operations_ = checkpoint_data.get("unglue_operations_", [])
            self.vocabulary_ = checkpoint_data["vocabulary_"]
            return True

        return False

    def _handle_nan_and_empty_texts(self, X):
        """Replace NaN and empty/whitespace-only documents with empty string.

        This ensures all documents are valid strings for extract_ngrams(),
        which will then treat them as zero-count documents.

        Parameters
        ----------
        X : pd.Series
            Text documents (may contain NaN values).

        Returns
        -------
        X_cleaned : pd.Series
            Text with NaN replaced by empty string.
        """
        X_cleaned = X.copy()
        # Replace NaN with empty string
        X_cleaned = X_cleaned.fillna("")
        return X_cleaned

    def _validate_empty_texts(self, X):
        """Report statistics about empty/whitespace-only documents.

        Empty documents will contribute zero counts to n-gram matrices during fit,
        which is correct for z-score calculations.

        Parameters
        ----------
        X : pd.Series
            Text documents (after NaN handling).
        """
        is_empty = X.fillna("").str.strip().eq("")
        num_empty = is_empty.sum()

        if num_empty > 0:
            pct_empty = 100 * num_empty / len(X)
            print(
                f"Information: Found {num_empty:,} empty documents ({pct_empty:.2f}%). "
                f"These will have zero counts in n-gram matrices."
            )

    def _filter_disallowed_single_letters(self, ngrams):
        """Filter n-grams containing disallowed single-letter words.

        Keeps n-grams only if all single-letter words are "a" or "i".
        Multi-letter words are always allowed.

        Parameters
        ----------
        ngrams : set of str
            N-grams to filter (space-separated words).

        Returns
        -------
        kept_ngrams : set of str
            N-grams passing the single-letter filter.
        """
        allowed_single_letters = {"a", "i"}
        kept_ngrams = set()

        for ngram in ngrams:
            words = ngram.split()
            if all(
                len(word) > 1 or word.lower() in allowed_single_letters
                for word in words
            ):
                kept_ngrams.add(ngram)

        return kept_ngrams
