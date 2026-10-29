import sys

import pandas as pd
from scipy.sparse import load_npz

# run from the repo root; the lyrics helpers live next to the R helpers
sys.path.insert(0, "analysis/helpers/RQ5/lyrics_analysis")
from helpers.STMTopicModeler import STMTopicModeler
from helpers.aggregate_artist_dtm import aggregate_dtm_by_artist

# number of latent expression types, found optimal in preceding experiments
# (held-out likelihood search over K = 2, ..., 30)
N_EXPRESSION_TYPES = 30
LYRICS_DIR = "data/modeled_features/lyrics"


def fit_and_transform_stm_model(X_train, X_holdout, artists, genres, vocab):
    X_train_artist_agg, genres_agg = aggregate_dtm_by_artist(X_train, artists, genres)
    modeler = STMTopicModeler(
        use_genre_prevalence=True,
        random_state=42,
        model_dir="models/lyrics",
    )
    modeler.fit(
        k=N_EXPRESSION_TYPES,
        X_artist=X_train_artist_agg,
        artist_genres=genres_agg,
        vocab=vocab,
    )
    for X, name in ((X_train, "train"), (X_holdout, "test")):
        pd.DataFrame(modeler.transform(X, vocab)).to_csv(
            f"{LYRICS_DIR}/stm_{name}.csv", index=False
        )


if __name__ == "__main__":
    meta_train = pd.read_csv(f"{LYRICS_DIR}/meta_train.csv")
    vocab = (
        pd.read_csv(f"{LYRICS_DIR}/fighting_discourseanalysis_vocabulary.csv")["ngram"]
        .to_numpy()
        .flatten()
    )
    print("FITTING STM MODEL...")
    fit_and_transform_stm_model(
        load_npz(f"{LYRICS_DIR}/DTM_train_discourseanalysis_fighting.npz"),
        load_npz(f"{LYRICS_DIR}/DTM_holdout_discourseanalysis_fighting.npz"),
        meta_train["track.s.firstartist.name"],
        meta_train["cat"],
        vocab,
    )
