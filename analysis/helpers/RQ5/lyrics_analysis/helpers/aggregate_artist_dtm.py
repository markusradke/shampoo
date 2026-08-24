import numpy as np
import pandas as pd
from scipy.sparse import csr_matrix


def aggregate_dtm_by_artist(X, artist, genre):
    """
    Aggregate track-level DTM to artist-genre-combination level.

    Reduces sparsity by combining all tracks that share the same artist-genre
    combination into a single document.

    Parameters
    ----------
    X : sparse matrix, shape (n_tracks, n_features)
        Track-level document-term matrix.
    artist : pd.Series or array-like
        Artist names for each track.
    genre : pd.Series or array-like
        Genre labels for each track.

    Returns
    -------
    X_artist : sparse matrix, shape (n_artist_genre_combinations, n_features)
        Artist-genre-combination-level aggregated DTM.
    artist_genres : pd.Series
        Genre label for each aggregated row (index = flat artist-genre key,
        sorted alphabetically).
    """
    artist = pd.Series(artist).reset_index(drop=True)
    genre = pd.Series(genre).reset_index(drop=True)

    if len(artist) != len(genre):
        raise ValueError("`artist` and `genre` must have the same length.")

    if not hasattr(X, "shape") or X.shape[0] != len(artist):
        raise ValueError("`X` rows must match the length of `artist` and `genre`.")

    combo_df = pd.DataFrame(
        {
            "artist": artist.astype(str),
            "genre": genre.astype(str),
        }
    )
    combo_df["artist_genre"] = combo_df["artist"] + "_" + combo_df["genre"]

    unique_combos = sorted(combo_df["artist_genre"].unique())
    combo_to_idx = {combo: i for i, combo in enumerate(unique_combos)}

    artist_genres = (
        combo_df[["artist_genre", "genre"]]
        .drop_duplicates("artist_genre")
        .set_index("artist_genre")
        .reindex(unique_combos)["genre"]
    )

    if not hasattr(X, "tocsr"):
        X = csr_matrix(X)
    else:
        X = X.tocsr()

    n_tracks = len(artist)
    n_artist_genre_combinations = len(unique_combos)

    artist_idx_array = combo_df["artist_genre"].map(combo_to_idx).to_numpy()

    row_indices = np.arange(n_tracks)
    col_indices = artist_idx_array
    data = np.ones(n_tracks)

    aggregation_matrix = csr_matrix(
        (data, (row_indices, col_indices)),
        shape=(n_tracks, n_artist_genre_combinations),
        dtype=np.float64,
    )

    X_artist = (aggregation_matrix.T @ X).astype(np.float64)

    return X_artist, artist_genres
