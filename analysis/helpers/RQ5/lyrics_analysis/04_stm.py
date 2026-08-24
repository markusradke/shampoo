import os
import pandas as pd

from scipy.sparse import load_npz

from helpers.STMTopicModeler import STMTopicModeler
from helpers.aggregate_artist_dtm import aggregate_dtm_by_artist

# runs for approx 5 h 45 min on 64-core CPU for, searching K in 2-20
K_GRID = [x for x in range(2, 31)]


def tune_and_fit_stm_model(
    X_train,
    X_test,
    artists,
    genres,
    vocab,
    train_out_path,
    test_out_path,
    name: str,
) -> None:
    model_dir = f"models/lyrics"
    X_train_artist_agg, genres_agg = aggregate_dtm_by_artist(X_train, artists, genres)
    modeler = STMTopicModeler(
        use_genre_prevalence=True,
        random_state=42,
        model_dir=model_dir,
    )
    modeler.search_k_with_heldout(
        X_train_artist_agg,
        genres_agg,
        vocab,
        K_GRID,
    )
    best_k = modeler.get_suggested_k()
    modeler.fit(
        k=best_k,
    )
    X_train_transformed = modeler.transform(X_train, vocab)
    pd.DataFrame(X_train_transformed).to_csv(train_out_path, index=False)

    X_test_transformed_temp = modeler.transform(X_test, vocab)
    pd.DataFrame(X_test_transformed_temp).to_csv(test_out_path, index=False)


if __name__ == "__main__":

    if not os.path.exists("data/modeled_features/lyrics/stm_features"):
        os.makedirs("data/modeled_features/lyrics/stm_features")

    X_train = load_npz(
        "data/modeled_features/lyrics/DTM_train_discourseanalysis_fighting.npz"
    )

    meta_train = pd.read_csv("data/modeled_features/lyrics/meta_train.csv")
    X_holdout = load_npz(
        "data/modeled_features/lyrics/DTM_holdout_discourseanalysis_fighting.npz"
    )

    vocab = (
        pd.read_csv(
            "data/modeled_features/lyrics/fighting_discourseanalysis_vocabulary.csv"
        )["ngram"]
        .to_numpy()
        .flatten()
    )

    print("TUNING STM MODEL...")
    tune_and_fit_stm_model(
        X_train,
        X_holdout,
        meta_train["track.s.firstartist.name"],
        meta_train["cat"],
        vocab,
        "data/modeled_features/lyrics/stm_train.csv",
        "data/modeled_features/lyrics/stm_test.csv",
        "discourseanalysis",
    )
