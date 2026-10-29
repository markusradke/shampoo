"""
Structural Topic Model (STM) wrapper using R via rpy2.

Handles fitting the model with a given number of topics on full training data
and transforming new documents to topic proportions. Uses the stm R package with genre as a prevalence covariate.
"""

import numpy as np
import pandas as pd
from pathlib import Path
from scipy.sparse import issparse

try:
    import rpy2.rinterface_lib.embedded as r_embedded

    r_embedded.set_initoptions(
        ("rpy2", "--no-save", "--no-restore", "--max-ppsize=500000")
    )

    import rpy2.robjects as ro
    from rpy2.robjects import pandas2ri
    from rpy2.robjects.packages import importr
    from rpy2.robjects.conversion import localconverter
except ImportError:
    raise ImportError(
        "rpy2 is required for STM topic modeling. " "Install with: pip install rpy2"
    )


class STMTopicModeler:
    """
    Structural Topic Model wrapper for Python-R integration.

    Uses the R stm package to fit topic models with genre as a prevalence
    covariate and fits the model on full training data.

    Parameters
    ----------
    use_genre_prevalence : bool, default=True
        Whether to include genre as a prior prevalence covariate in the STM model.
    random_state : int, default=42
        Random seed for reproducibility.
    model_dir : str or Path, optional
        Directory to save fitted model and checkpoints. If None, checkpointing
        is disabled.

    Attributes
    ----------
    K_ : int
        Number of topics.
    stm_model_ : R object
        Fitted STM model (R stm object).
    vocab_ : list of str
        Vocabulary (feature names) used by the model.
    _is_fitted : bool
        Whether the model has been fitted.
    """

    def __init__(
        self,
        use_genre_prevalence=True,
        random_state=42,
        model_dir=None,
    ):
        self.random_state = random_state
        self.use_genre_prevalence = use_genre_prevalence
        self.model_dir = Path(model_dir) if model_dir else None
        self._is_fitted = False

        self.stm = importr("stm")
        self.base = importr("base")

        if self.model_dir:
            self.model_dir.mkdir(parents=True, exist_ok=True)

    def fit(self, k, X_artist=None, artist_genres=None, vocab=None):
        """Fit STM model with selected K on full training data."""
        if X_artist is not None and artist_genres is not None and vocab is not None:
            self.vocab_ = [str(v) for v in list(vocab)]
            print("Preparing data for final model fit...")
            self.documents, self.meta = self._prepare_stm_data(X_artist, artist_genres)
        elif not hasattr(self, "documents") or not hasattr(self, "meta"):
            raise ValueError(
                "Must provide X_artist, artist_genres, and vocab to fit(), "
                "to fit()."
            )
        self.K_ = k
        print(f"Fitting STM model with K = {self.K_}...")
        self.stm_model_ = self._fit_stm(self.documents, self.meta, self.K_)

        self._is_fitted = True

        if self.model_dir:
            self._save_model()
            print(f"Model saved to {self.model_dir}")

        return self

    def transform(self, X, vocab):
        """
        Transform documents to topic proportions.

        Parameters
        ----------
        X : sparse matrix, shape (n_docs, n_features)
            Document-term matrix (track-level or artist-level).
        vocab : array-like of str
            Feature names (must match training vocabulary).

        Returns
        -------
        theta : np.ndarray, shape (n_docs, K)
            Topic proportions for each document.
        """
        if not self._is_fitted:
            raise ValueError("Must call tune_and_fit() before transform()")

        vocab = [str(v) for v in list(vocab)]
        if list(vocab) != self.vocab_:
            raise ValueError("Vocabulary mismatch between training and transform")

        print("Converting data to STM format for inference...")
        documents, _ = self._prepare_stm_data(X, genre=None)

        print("Estimating topic proportions for new documents...")
        fit_result = self.stm.fitNewDocuments(
            model=self.stm_model_,
            documents=documents,
            newData=ro.NULL,
            prevalence=ro.NULL,
        )

        theta = np.array(fit_result.rx2("theta"))
        return theta

    def _prepare_stm_data(self, X, genre=None):
        """
        Convert sparse matrix to STM document format.

        STM expects documents as a list of matrices, where each document is
        a 2xN matrix: row 1 = term indices (1-indexed), row 2 = counts.

        Parameters
        ----------
        X : sparse matrix, shape (n_docs, n_features)
            Document-term matrix.
        genre : pd.Series, optional
            Genre labels for metadata (only for training).

        Returns
        -------
        documents : R list
            STM-formatted documents.
        meta : R DataFrame or NULL
            Metadata with genre column, or NULL if genre is None.
        """
        if issparse(X):
            X = X.tocsr()
        else:
            from scipy.sparse import csr_matrix

            X = csr_matrix(X)

        ro.globalenv["._indptr"] = ro.IntVector((X.indptr).tolist())
        ro.globalenv["._indices"] = ro.FloatVector(
            (X.indices + 1).astype(float).tolist()
        )
        ro.globalenv["._data"] = ro.FloatVector(X.data.astype(float).tolist())
        ro.globalenv["._n_docs"] = ro.IntVector([X.shape[0]])

        documents = ro.r(
            """
local({
  n <- ._n_docs[1]
  docs <- vector("list", n)
  for (i in seq_len(n)) {
    start <- ._indptr[i] + 1L
    end   <- ._indptr[i + 1]
    if (start > end) {
      docs[[i]] <- matrix(numeric(0), nrow = 2L, ncol = 0L)
    } else {
      docs[[i]] <- matrix(
        c(._indices[start:end], ._data[start:end]),
        nrow = 2L, byrow = TRUE
      )
    }
  }
  docs
})
"""
        )
        for key in ("._indptr", "._indices", "._data", "._n_docs"):
            ro.r(f"rm({key})")

        if genre is not None:
            with localconverter(ro.default_converter + pandas2ri.converter):
                meta = ro.conversion.py2rpy(pd.DataFrame({"genre": genre.values}))
        else:
            meta = ro.NULL

        return documents, meta

    def _fit_stm(self, documents, meta, K):
        """
        Fit STM model with specified K.

        Parameters
        ----------
        documents : R list
            STM-formatted documents.
        meta : R DataFrame
            Metadata with genre column.
        K : int
            Number of topics.

        Returns
        -------
        model : R object
            Fitted STM model.
        """
        vocab_r = ro.StrVector(self.vocab_)
        if self.use_genre_prevalence:
            prevalence = ro.Formula("~genre")
        else:
            prevalence = ro.NULL

        model = self.stm.stm(
            documents=documents,
            vocab=vocab_r,
            K=K,
            prevalence=prevalence,
            data=meta,
            max_em_its=500,
            init_type="Spectral",
            seed=self.random_state,
            verbose=True,
        )

        return model

    def _save_model(self):
        """Save fitted model and metadata to model directory."""
        model_path = self.model_dir / "stm_model.rds"
        if hasattr(self, "stm_model_"):
            self.base.saveRDS(self.stm_model_, str(model_path))
