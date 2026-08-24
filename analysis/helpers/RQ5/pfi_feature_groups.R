library(dplyr)

folds <- list.files(
  path = "data/classification_ready_data/",
  pattern = "^folds_k\\d+\\.rds$",
  full.names = TRUE
) |>
  first() |>
  readRDS()
all_features <- folds$X_train[[1]] |> colnames()
all_features <- setdiff(all_features, "track.s.id")
all_features

feature_groups <- list(
  "Artist Metadata" = c(
    "artist.mb.birthyear",
    "track.dz.firstartist.nalbums",
    stringr::str_subset(all_features, "artist.mb.type_*"),
    stringr::str_subset(all_features, "artist.mb.dead_*"),
    stringr::str_subset(all_features, "artist.mb.origin_*"),
    stringr::str_subset(all_features, "artist.mb.area_*"),
    stringr::str_subset(all_features, "artist.mb.gender_*")
  ),
  "Spotify Artist Distribution Genre Map" = stringr::str_subset(
    all_features,
    "^sp_*"
  ),
  "Release Metadata" = c(
    "album.s.releaseyear",
    "album.s.totaltracks",
    "track.dz.album.duration",
    stringr::str_subset(all_features, "is.major.label_*"),
    stringr::str_subset(all_features, "track.is.dach_*")
  ),
  "Lyrics Code System" = c(
    stringr::str_subset(all_features, "track.s.explicitlyrics*"),
    stringr::str_subset(all_features, "track.language_*"),
    stringr::str_subset(all_features, "track.dz.album.explicitlyrics*"),
    stringr::str_subset(all_features, "track.is.instrumental*")
  ),
  "Lyrics Information Content" = c(
    "lyrics.distinct_words_ratio",
    "lyrics.repeated_lines_ratio"
  ),
  "Lyrics Sentiment" = c(
    stringr::str_subset(all_features, "lyrics.nrc_*"),
    "lyrics.sentiment"
  ),
  "Lyrics Expression Types" = c(
    stringr::str_subset(all_features, "lyrics.expr_type_*")
  ),
  "ML-Audio-Features Genre Sounds" = c(
    stringr::str_subset(all_features, "track.ab.p.drtmd_*"),
    stringr::str_subset(all_features, "track.ab.p.eltrc_*"),
    stringr::str_subset(all_features, "track.ab.p.rosa_*"),
    stringr::str_subset(all_features, "track.ab.p.tzntk_*"),
    stringr::str_subset(all_features, "track.ab.p.ismir_*")
  ),
  "ML-Audio-Features Production Sounds" = c(
    "track.s.loudness",
    "track.ab.low.loudness",
    "track.ab.low.dynamiccomplexity",
    "track.s.acousticness",
    "track.ab.p.acoustic",
    "track.ab.p.bright",
    "track.s.liveness",
    "track.s.speechiness",
    "track.ab.p.electronic",
    "track.dz.loudness"
  ),
  "ML-Audio-Features Instrumentation" = c(
    "track.s.instrumentalness",
    "track.ab.p.voice",
    "track.ab.p.female",
    "track.ab.p.tonal"
  ),
  "ML-Audio-Features Composition" = c(
    "track.s.danceability",
    "track.s.energy",
    "track.s.tempo",
    "track.s.valence",
    "track.s.duration",
    "track.ab.p.danceable",
    "track.ab.p.aggressive",
    "track.ab.p.happy",
    "track.ab.p.party",
    "track.ab.p.relaxed",
    "track.ab.p.sad",
    "track.ab.rhythm.danceability",
    "track.ab.rhythm.tempo",
    "track.ab.rhythm.onsetrate",
    stringr::str_subset(all_features, "track.ab.p.mirex.*"),
    "track.dz.tempo",
    "track.ab.tonal.chordchangerate",
    "track.ab.tonal.chordsnumberrate",
    stringr::str_subset(all_features, "track.s.key_*"),
    stringr::str_subset(all_features, "track.s.mode_*"),
    stringr::str_subset(all_features, "track.s.timesignature_*"),
    stringr::str_subset(all_features, "track.ab.tonal.key_*"),
    stringr::str_subset(all_features, "track.ab.tonal.mode_*")
  ),
  "Track Popularity" = c(
    "track.s.popularity",
    "track.dz.rank"
  ),
  "Artist Popularity" = c(
    "artist.s.popularity",
    "artist.s.followers",
    "track.dz.firstartist.followers"
  ),
  "Release Popularity" = c(
    "album.s.popularity",
    "track.dz.album.followers"
  ),
  "Label Popularity" = c(
    "label.med.artist.popularity",
    "label.med.album.popularity",
    "label.med.track.popularity"
  )
)
