import pandas as pd
import numpy as np
import os

from scipy import sparse

from helpers.FightingExtractor import FightingExtractor


# runs for approx 1 hour
RANDOM_STATE = 42
MIN_ARTISTS = 2


def extract_and_save_fighting_vocabularies(X_train, meta_train, name):
    ngram_types = (1, 2, 3, 4)
    min_char = 0
    print(f"Extracting Fighting vocabulary for {name}...")
    extractor = FightingExtractor(
        min_artists=MIN_ARTISTS,
        p_value=0.001,
        prior_concentration=1.0,
        use_stopword_filter=True,
        ngram_types=ngram_types,
        min_char=min_char,
        random_state=42,
        checkpoint_dir=f"data/checkpoints/fighting_extractor_{name}",
    )

    X_train.reset_index(drop=True, inplace=True)
    meta_train.reset_index(drop=True, inplace=True)
    X_train_fighting = extractor.fit_transform(
        X_train["lyrics"],
        meta_train["cat"],
        meta_train["track.s.firstartist.name"],
    )
    z_scores = extractor.z_scores_df_
    if not os.path.exists(f"models/lyrics"):
        os.makedirs(f"models/lyrics")
    z_scores.to_csv(f"models/lyrics/fighting_{name}_z_scores.csv", index=False)

    # save language prevalences with the vocabulary (weight by bilingual artists prevalance in the training set)
    vocab = pd.DataFrame({"ngram": extractor.vocabulary_})
    vocab.to_csv(
        f"data/modeled_features/lyrics/fighting_{name}_vocabulary.csv", index=False
    )

    sparse.save_npz(
        f"data/modeled_features/lyrics/DTM_train_{name}_fighting.npz", X_train_fighting
    )
    return extractor


if __name__ == "__main__":
    train_corpus = pd.read_csv("data/modeled_features/lyrics/lyrics_train_corpus.csv")
    X_train = train_corpus[["lyrics"]]
    meta_train = train_corpus[["track.s.id", "cat", "track.s.firstartist.name"]]
    meta_train.to_csv("data/modeled_features/lyrics/meta_train.csv", index=False)

    holdout_corpus = pd.read_csv(
        "data/modeled_features/lyrics/lyrics_holdout_corpus.csv"
    )
    X_holdout = holdout_corpus[["lyrics"]]
    meta_holdout = holdout_corpus[["track.s.id"]]
    meta_holdout.to_csv("data/modeled_features/lyrics/meta_holdout.csv", index=False)

    extractor = extract_and_save_fighting_vocabularies(
        X_train, meta_train, "discourseanalysis"
    )
    DTM_holdout = extractor.transform(X_holdout["lyrics"])

    sparse.save_npz(
        "data/modeled_features/lyrics/DTM_holdout_discourseanalysis_fighting.npz",
        DTM_holdout,
    )

    print("All done!")
