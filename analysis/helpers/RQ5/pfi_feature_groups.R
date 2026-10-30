get_feature_groups <- function(feature_names) {
  #' Group the features of the evaluated data for the permutation feature importance. Groups without features (the Spotify genres in the ablation) are dropped.
  #' @return A named list of feature name vectors. Stops if a listed feature is missing or a feature is in no group.
  select_features <- function(pattern) {
    stringr::str_subset(feature_names, pattern)
  }
  feature_groups <- list(
    "Artist Metadata" = c(
      "artist.mb.birthyear",
      "track.dz.firstartist.nalbums",
      select_features("artist.mb.type_*"),
      select_features("artist.mb.dead_*"),
      select_features("artist.mb.origin_*"),
      select_features("artist.mb.area_*"),
      select_features("artist.mb.gender_*")
    ),
    "Spotify Artist Distribution Genre Map" = select_features("^sp_*"),
    "Release Metadata" = c(
      "album.s.releaseyear",
      "album.s.totaltracks",
      "track.dz.album.duration",
      select_features("is.major.label_*"),
      select_features("track.is.dach_*")
    ),
    "Lyrics Code System" = c(
      select_features("track.s.explicitlyrics*"),
      select_features("track.language_*"),
      select_features("track.dz.album.explicitlyrics*"),
      select_features("track.is.instrumental*")
    ),
    "Lyrics Information Content" = c(
      "lyrics.distinct_words_ratio",
      "lyrics.repeated_lines_ratio"
    ),
    "Lyrics Sentiment" = c(
      select_features("lyrics.nrc_*"),
      "lyrics.sentiment"
    ),
    "Lyrics Expression Types" = select_features("lyrics.expr_type_*"),
    "ML-Audio-Features Genre Sounds" = c(
      select_features("track.ab.p.drtmd_*"),
      select_features("track.ab.p.eltrc_*"),
      select_features("track.ab.p.rosa_*"),
      select_features("track.ab.p.tzntk_*"),
      select_features("track.ab.p.ismir_*")
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
      select_features("track.ab.p.mirex.*"),
      "track.dz.tempo",
      "track.ab.tonal.chordchangerate",
      "track.ab.tonal.chordsnumberrate",
      select_features("track.s.key_*"),
      select_features("track.s.mode_*"),
      select_features("track.s.timesignature_*"),
      select_features("track.ab.tonal.key_*"),
      select_features("track.ab.tonal.mode_*")
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
  missing_features <- setdiff(unlist(feature_groups), feature_names)
  ungrouped_features <- setdiff(feature_names, unlist(feature_groups))
  if (length(missing_features) > 0 || length(ungrouped_features) > 0) {
    stop(sprintf(
      "Feature groups do not match the data. Missing in data: %s. Not in any group: %s.",
      paste(missing_features, collapse = ", "),
      paste(ungrouped_features, collapse = ", ")
    ))
  }
  Filter(length, feature_groups)
}
