import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path
from typing import Tuple
from sklearn.model_selection import train_test_split, StratifiedKFold


def split_group_stratified_and_join(
    labels_and_group: pd.DataFrame,
    X: pd.DataFrame,
    test_size: float = 0.2,
    random_state: int = 42,
) -> Tuple[pd.DataFrame, pd.DataFrame, pd.Series, pd.Series]:
    """Split X and labels by groups ensuring stratification by group's dominant label.
    Dataset is split on group level and then joined back to features and labels.

    Args:
        labels_and_group: DataFrame with columns 'group' and 'label'.
        X: Feature DataFrame aligned with labels_and_group by row order.
        test_size: Fraction of groups to use for test.
        random_state: Random state for reproducibility (seed 42 by convention).

    Returns:
        X_train, X_test, y_train, y_test
    """
    group_train, group_test = _split_by_group(labels_and_group, test_size, random_state)

    train_mask, test_mask = _create_train_test_masks(
        labels_and_group, group_train, group_test
    )

    X_train, X_test, y_train, y_test = _split_X_labels(
        X, labels_and_group, train_mask, test_mask
    )
    validate_artist_split(
        labels_and_group.loc[train_mask],
        labels_and_group.loc[test_mask],
        group_col="group",
    )
    return X_train, X_test, y_train, y_test


def _split_by_group(
    labels_and_group: pd.DataFrame, test_size: float, random_state: int
) -> Tuple[pd.Index, pd.Index]:
    """Return group indices for train and test using stratification by dominant label."""
    group_labels = _get_group_labels(labels_and_group)
    group_train, group_test = train_test_split(
        group_labels.index.to_numpy(),
        test_size=test_size,
        stratify=group_labels.values,
        random_state=random_state,
    )
    assert (
        len(set(group_train).intersection(group_test)) == 0
    ), "Split failed: Groups overlap between train and test!"
    return group_train, group_test


def _get_group_labels(labels_and_group: pd.DataFrame) -> pd.Series:
    """Map each group to its most frequent label (mode).

    Assumes labels_and_group has columns 'group' and 'label'.
    """
    return labels_and_group.groupby("group")["label"].agg(
        lambda x: x.value_counts().idxmax()
    )


def _create_train_test_masks(
    labels_and_group: pd.DataFrame, group_train: pd.Index, group_test: pd.Index
) -> Tuple[pd.Series, pd.Series]:
    """Create boolean masks for rows that belong to train/test groups."""
    train_mask = labels_and_group["group"].isin(group_train)
    test_mask = labels_and_group["group"].isin(group_test)
    return train_mask, test_mask


def _split_X_labels(
    X: pd.DataFrame,
    labels_and_group: pd.DataFrame,
    train_mask: pd.Series,
    test_mask: pd.Series,
) -> Tuple[pd.DataFrame, pd.DataFrame, pd.Series, pd.Series]:
    """Split and reset indices for features X and labels Series."""
    X_train = X.loc[train_mask].reset_index(drop=True)
    X_test = X.loc[test_mask].reset_index(drop=True)
    y_train = labels_and_group.loc[train_mask, "label"].reset_index(drop=True)
    y_test = labels_and_group.loc[test_mask, "label"].reset_index(drop=True)
    return X_train, X_test, y_train, y_test


def validate_artist_split(
    train: pd.DataFrame, test: pd.DataFrame, group_col: str = "artist"
) -> None:
    """
    Verify no artist appears in both train and test sets.
    Raises ValueError if overlap detected.
    """
    train_artists = set(train[group_col].unique())
    test_artists = set(test[group_col].unique())

    overlap = train_artists.intersection(test_artists)

    if len(overlap) > 0:
        raise ValueError(
            f"Artist overlap detected: {len(overlap)} artists in both splits.\n"
            f"Examples: {list(overlap)[:5]}"
        )

    print(
        f"Artist split validated: {len(train_artists)} train, {len(test_artists)} test (disjoint)"
    )


def plot_comparison_genre_distributions(
    y_train: pd.Series, y_test: pd.Series
) -> plt.Figure:
    """Plot relative label frequencies for train and test as grouped horizontal bars.

    Args:
        y_train: Series of labels for the training set.
        y_test: Series of labels for the test set.

    Returns:
        Matplotlib Figure with horizontal grouped bars (train grey, test #c20d40).
    """
    train_rel = y_train.value_counts(normalize=True)
    test_rel = y_test.value_counts(normalize=True)

    labels = train_rel.index.union(test_rel.index)
    train_aligned = train_rel.reindex(labels, fill_value=0)
    test_aligned = test_rel.reindex(labels, fill_value=0)

    combined = train_aligned + test_aligned
    labels_sorted = combined.sort_values(ascending=False).index

    train_vals = train_aligned.reindex(labels_sorted, fill_value=0).to_numpy()
    test_vals = test_aligned.reindex(labels_sorted, fill_value=0).to_numpy()

    n = len(labels_sorted)
    y_pos = np.arange(n)
    bar_height = 0.4

    fig, ax = plt.subplots(figsize=(6, max(4, n * 0.3)))
    ax.barh(
        y_pos + bar_height / 2,
        test_vals,
        height=bar_height,
        color="#c20d40",
        label="test",
    )
    ax.barh(
        y_pos - bar_height / 2,
        train_vals,
        height=bar_height,
        color="#c1c1c1",
        label="train",
    )

    ax.set_yticks(y_pos)
    ax.set_yticklabels(labels_sorted)
    ax.set_xlabel("Relative frequency")
    ax.set_xlim(0, 1.0)
    ax.spines[["top", "right"]].set_visible(False)
    ax.legend(frameon=False)
    fig.tight_layout()
    return fig


def create_artist_separated_folds(
    labels_and_group: pd.DataFrame,
    X: pd.DataFrame,
    n_splits: int = 5,
    random_state: int = 42,
) -> list[tuple[np.ndarray, np.ndarray]]:
    """Create artist-separated folds stratified by each artist's dominant label.

    Returns list of (train_idx, test_idx) indices aligned to X rows.
    """
    group_labels = _get_group_labels(labels_and_group)
    skf = StratifiedKFold(n_splits=n_splits, shuffle=True, random_state=random_state)

    folds: list[tuple[np.ndarray, np.ndarray]] = []
    all_idx = np.arange(len(X))

    for group_train, group_test in skf.split(
        group_labels.index.to_numpy(), group_labels.values
    ):
        train_groups = group_labels.index.to_numpy()[group_train]
        test_groups = group_labels.index.to_numpy()[group_test]

        train_mask = labels_and_group["group"].isin(train_groups)
        test_mask = labels_and_group["group"].isin(test_groups)

        train_idx = all_idx[train_mask.to_numpy()]
        test_idx = all_idx[test_mask.to_numpy()]
        folds.append((train_idx, test_idx))

    return folds


def plot_and_save_fold_label_prevalence(
    y: pd.Series,
    folds: list[tuple[np.ndarray, np.ndarray]],
    output_dir: str | Path = "models",
    file_prefix: str = "fold_genre_prevalence",
) -> plt.Figure:
    """Plot label prevalence per fold (grey) vs overall training prevalence (red).

    Also writes two CSV files to output_dir:
    - {file_prefix}_relative.csv  (genre, fold1..foldN; relative frequencies)
    - {file_prefix}_absolute.csv  (genre, fold1..foldN; absolute counts)
    """
    overall = y.value_counts(normalize=True)

    fold_prevalences = []
    fold_counts = []
    for _, test_idx in folds:
        y_fold = y.iloc[test_idx]
        fold_prevalences.append(y_fold.value_counts(normalize=True))
        fold_counts.append(y_fold.value_counts())

    labels = overall.index
    for fold_rel in fold_prevalences:
        labels = labels.union(fold_rel.index)

    overall_aligned = overall.reindex(labels, fill_value=0)
    labels_sorted = overall_aligned.sort_values(ascending=False).index
    overall_vals = overall_aligned.reindex(labels_sorted).to_numpy()

    # Save fold-wise relative and absolute values to CSV
    rel_export = pd.DataFrame({"genre": labels_sorted})
    abs_export = pd.DataFrame({"genre": labels_sorted})

    for i, (fold_rel, fold_abs) in enumerate(
        zip(fold_prevalences, fold_counts), start=1
    ):
        rel_export[f"fold{i}"] = fold_rel.reindex(
            labels_sorted, fill_value=0
        ).to_numpy()
        abs_export[f"fold{i}"] = fold_abs.reindex(
            labels_sorted, fill_value=0
        ).to_numpy()

    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    rel_export.to_csv(output_dir / f"{file_prefix}_relative.csv", index=False)
    abs_export.to_csv(output_dir / f"{file_prefix}_absolute.csv", index=False)

    fold_aligned = [
        fold_rel.reindex(labels, fill_value=0) for fold_rel in fold_prevalences
    ]

    n = len(labels_sorted)
    y_pos = np.arange(n)

    fig, ax = plt.subplots(figsize=(6, max(4, n * 0.3)))

    n_folds = len(fold_aligned)
    n_bars = n_folds + 1
    bar_height = 0.8 / max(n_bars, 1)
    offsets = (np.arange(n_bars) - (n_bars - 1) / 2) * bar_height

    for i, fold_rel in enumerate(fold_aligned):
        fold_vals = fold_rel.reindex(labels_sorted).to_numpy()
        ax.barh(
            y_pos + offsets[i],
            fold_vals,
            height=bar_height,
            color="#c1c1c1",
            alpha=0.7,
        )

    ax.barh(
        y_pos + offsets[-1],
        overall_vals,
        height=bar_height,
        color="#c20d40",
    )

    ax.set_yticks(y_pos)
    ax.set_yticklabels(labels_sorted)
    ax.set_xlabel("Relative frequency")
    ax.set_xlim(0, 1.0)
    ax.spines[["top", "right"]].set_visible(False)
    fig.tight_layout()
    return fig
