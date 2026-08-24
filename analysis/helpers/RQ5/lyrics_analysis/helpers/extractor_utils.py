import random
import re

from functools import partial
from typing import List, Tuple
from sklearn.feature_extraction.text import CountVectorizer

from .StopwordFilter import StopwordFilter


BOUNDARY_SPLIT_PATTERN = re.compile(r"[\n,.:!?;()]+")
TOKEN_PATTERN_BOUNDARY_AWARE = re.compile(r"(?u)\b\w+(?:['-]\w+)*\b")


def _extract_ngrams_from_tokens(tokens, order):
    """Extract fixed-order n-grams from a list of tokens."""
    if order <= 0 or len(tokens) < order:
        return []

    return [" ".join(tokens[i : i + order]) for i in range(len(tokens) - order + 1)]


def _extract_boundary_aware_ngrams_from_text(text, orders):
    """Extract n-grams without crossing sentence/newline boundaries."""
    if text is None:
        text = ""

    text = str(text).lower()
    segments = BOUNDARY_SPLIT_PATTERN.split(text)

    ngrams = []
    for segment in segments:
        tokens = TOKEN_PATTERN_BOUNDARY_AWARE.findall(segment)
        if not tokens:
            continue

        for order in orders:
            ngrams.extend(_extract_ngrams_from_tokens(tokens, order))

    return ngrams


def _boundary_aware_analyzer(text, orders):
    """Analyzer callable for CountVectorizer with boundary-aware n-grams."""
    return _extract_boundary_aware_ngrams_from_text(text, orders)


def extract_ngrams(texts, order, name, random_state, boundary_aware=False):
    """Extract n-grams using CountVectorizer."""
    if boundary_aware:
        vectorizer = CountVectorizer(
            analyzer=partial(_boundary_aware_analyzer, orders=(order,)),
        )
    else:
        vectorizer = CountVectorizer(
            ngram_range=(order, order),
            token_pattern=r"\b[\w']+\b",
            lowercase=True,
        )

    matrix = vectorizer.fit_transform(texts)
    features = vectorizer.get_feature_names_out()

    rng = random.Random(random_state)
    sample = rng.sample(list(features), k=min(5, len(features)))

    print(f"Extracted {name}:")
    print(f"  - Unique: {len(features):,}")
    print(f"  - Shape: {matrix.shape}")
    print(f"  - Examples: {sample}")

    return matrix, features


def count_artists_per_ngram(artists, ngram_matrix, ngram_features):
    """Count unique artists per n-gram using pandas groupby."""
    import pandas as pd

    binary_matrix = (ngram_matrix > 0).astype(int).tocsc()
    artist_array = artists.to_numpy()

    rows, cols = binary_matrix.nonzero()

    df = pd.DataFrame({"ngram_idx": cols, "artist": artist_array[rows]})

    counts = df.groupby("ngram_idx")["artist"].nunique()

    artist_count = dict(zip(ngram_features[counts.index], counts.values))
    print(f"Counted unique artists for {len(artist_count):,} n-grams")
    return artist_count


def strip_boundary_ngrams(ngrams: List[Tuple[str, ...]]) -> List[Tuple[str, ...]]:
    """Remove n-grams starting with articles or infinitive markers."""
    banned_starts = {"a", "an", "the", "to", "der", "die", "das", "ein", "eine", "zu"}
    return [ng for ng in ngrams if ng and ng[0].lower() not in banned_starts]


def filter_stopword_only(
    ngrams: List[Tuple[str, ...]], stopword_filter: StopwordFilter
) -> List[Tuple[str, ...]]:
    """Remove n-grams containing only stopwords."""
    return [ng for ng in ngrams if not stopword_filter.is_stopword_only(" ".join(ng))]
