library(dplyr)
library(ggplot2)
library(see)
source("analysis/helpers/shared_helpers.R")

prepare_poptrag_selected <- function(poptrag_path) {
  message("Importing POPTRAG dataset and selecting variables ...")
  poptrag <- readRDS(poptrag_path)
  poptrag_selected <- select_relevant_columns(poptrag)
  poptrag_selected
}

save_tags_csv <- function(tags, path) {
  readr::write_csv(tags, path)
  message(sprintf(
    "Saved N=%s tracks with their tags in long format to %s",
    format(nrow(distinct(tags, track.s.id)), big.mark = ","),
    path
  ))
}

prepare_musicbrainz_corpus <- function(
  poptrag_selected,
  mb_whitelist_path,
  mb_non_music_tags,
  min_artist_prev
) {
  mb_whitelist <- readRDS(mb_whitelist_path)
  unique_mb_tags <- get_unique_mb_tags(poptrag_selected)
  mb_non_valid_tags <- union(
    mb_non_music_tags,
    setdiff(unique_mb_tags, mb_whitelist)
  )
  combined_mb_genres <- combine_mb_genres(poptrag_selected)
  mb_long <- get_long_genre_tags(combined_mb_genres, "mb.genres")
  mb_long_music <- filter_non_valid_tags(mb_long, mb_non_valid_tags)
  mb_long_vote_denoise <- filter_min_votes(mb_long_music, n_min_votes = 2)
  mb_long_tag_denoise <- filter_artist_prevalence(
    mb_long_vote_denoise,
    min_prevalence = min_artist_prev,
    message_template = paste0(
      "Unique artists threshold for tags: %d ",
      "(%.4f%% in data set)"
    )
  )

  mb_final <- mb_long_tag_denoise |>
    dplyr::select(-dplyr::matches("genres$"))

  message(sprintf(
    "Retaind %d MusicBrainz tags after denoising.",
    nrow(distinct(mb_final, tag_name))
  ))
  mb_final
}

prepare_discogs_corpus <- function(
  poptrag_selected,
  mb_final,
  min_artist_prev
) {
  mb_final_ids <- dplyr::distinct(mb_final, track.s.id)
  dc <- poptrag_selected |>
    dplyr::filter(!is.na(album.dc.genres_str)) |>
    dplyr::semi_join(mb_final_ids, by = "track.s.id")

  dc_long <- dc |>
    tidyr::separate_rows(album.dc.genres_str, sep = "; ") |>
    dplyr::rename(tag_name = album.dc.genres_str) |>
    dplyr::select(track.s.id, track.s.firstartist.id, tag_name) |>
    dplyr::mutate(tag_count = 1L) |>
    dplyr::filter(!tag_name %in% c("Non-Music", "Children's", ""))

  dc_long_tag_denoise <- filter_artist_prevalence(
    dc_long,
    min_prevalence = min_artist_prev,
    message_template = paste0(
      "Unique artists threshold for Discogs tags: %d ",
      "(%.3f%% in data set)"
    )
  )
  message(sprintf(
    "Retaind %d Discogs tags after denoising.",
    nrow(distinct(dc_long_tag_denoise, tag_name))
  ))
  dc_long_tag_denoise
}

prepare_deezer_corpus <- function(
  poptrag_selected,
  mb_final,
  dz_non_music_tags,
  min_artist_prev
) {
  mb_final_ids <- dplyr::distinct(mb_final, track.s.id)
  dz <- poptrag_selected |>
    dplyr::filter(!is.na(album.dz.genres_str)) |>
    dplyr::semi_join(mb_final_ids, by = "track.s.id")

  dz_long <- dz |>
    tidyr::separate_rows(album.dz.genres_str, sep = "; ") |>
    dplyr::rename(tag_name = album.dz.genres_str) |>
    dplyr::select(track.s.id, track.s.firstartist.id, tag_name) |>
    dplyr::mutate(tag_count = 1L) |>
    dplyr::filter(!tag_name %in% dz_non_music_tags)

  dz_long_tag_denoise <- filter_artist_prevalence(
    dz_long,
    min_prevalence = min_artist_prev,
    message_template = paste0(
      "Unique artists threshold for Deezer tags: %d ",
      "(%.3f%% in data set)"
    )
  )
  message(sprintf(
    "Retaind %d Deezer tags after denoising.",
    nrow(distinct(dz_long_tag_denoise, tag_name))
  ))
  dz_long_tag_denoise
}

prepare_spotify_corpus <- function(
  poptrag_selected,
  s_non_music_tags,
  min_artist_prev
) {
  poptrag_selected$artist.s.genre_str <- sapply(
    poptrag_selected$artist.s.genres,
    function(x) paste(x$genre, collapse = "; ")
  )
  poptrag_selected$artist.s.genre_str[
    poptrag_selected$artist.s.genre_str == ""
  ] <- NA

  s_long <- dplyr::select(
    poptrag_selected,
    track.s.id,
    track.s.firstartist.id,
    artist.s.genre_str
  ) |>
    tidyr::separate_rows(artist.s.genre_str, sep = "; ") |>
    dplyr::rename(tag_name = artist.s.genre_str) |>
    dplyr::mutate(tag_count = 1L) |>
    dplyr::filter(!is.na(tag_name))

  s_long <- dplyr::filter(s_long, !tag_name %in% s_non_music_tags)

  s_long_tag_denoise <- filter_artist_prevalence(
    s_long,
    min_prevalence = min_artist_prev,
    message_template = paste0(
      "Unique artists threshold for Spotify tags: %d ",
      "(%.4f%% in data set)"
    )
  )
  message(sprintf(
    "Retaind %d Spotify tags after denoising.",
    nrow(distinct(s_long_tag_denoise, tag_name))
  ))
  s_long_tag_denoise
}

plot_corpus_composition <- function(mb_long) {
  selected <- dplyr::distinct(
    mb_long,
    track.s.id,
    album.s.releaseyear,
    artist.s.popularity
  )

  breaks <- c(
    0,
    quantile(selected$artist.s.popularity, probs = c(0.2, 0.4, 0.6, 0.8, 1))
  )
  binned_popularity <- selected |>
    dplyr::mutate(
      popularity_bin = cut(
        artist.s.popularity,
        breaks = breaks,
        include.lowest = TRUE,
        right = FALSE
      )
    )

  count_data <- dplyr::count(binned_popularity, popularity_bin) |>
    dplyr::mutate(relfreq = n / sum(n) * 100)
  data_labelled <- binned_popularity |>
    dplyr::left_join(count_data, by = "popularity_bin") |>
    dplyr::mutate(
      popularity_bin = paste0(
        popularity_bin,
        " (",
        round(relfreq, 1),
        "%)"
      )
    )

  data_labelled$popularity_bin <- factor(data_labelled$popularity_bin)
  data_labelled$popularity_bin <- forcats::fct_rev(data_labelled$popularity_bin)

  ggplot(data_labelled, aes(x = album.s.releaseyear, fill = popularity_bin)) +
    geom_area(stat = "count") +
    labs(x = "Album Release Year", y = "Number of Artists") +
    scale_fill_okabeito(name = "Spotify Artist Popularity") +
    scale_y_continuous(
      breaks = seq(0, 7000, by = 1000),
      labels = c(0, sprintf("%dk", seq(7)))
    ) +
    guides(fill = guide_legend(nrow = 3)) +
    theme_minimal() +
    xlim(1960, 2024) +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    )
}

select_relevant_columns <- function(input) {
  input |>
    dplyr::select(
      track.s.id,
      track.s.title,
      track.s.artists,
      track.s.firstartist.id,
      track.s.firstartist.name,
      track.s.popularity,
      track.s.previewurl,
      artist.s.genres,
      artist.s.popularity,
      album.s.id,
      album.s.title,
      album.s.releaseyear,
      album.s.popularity,
      album.s.coverurl,
      album.dc.id,
      album.dc.genres,
      album.dc.styles,
      track.dz.album.genres,
      track.mb.genres,
      album.mb.genres,
      artist.mb.genres,
      source.officialcharts,
      source.recommendations,
      source.featuredplaylists,
      source.spotifycharts
    ) |>
    dplyr::mutate(
      trackartists.s.id = sapply(.data$track.s.artists, function(x) {
        paste(x$id, collapse = ";")
      }),
      n_trackartists = sapply(.data$track.s.artists, function(x) {
        as.integer(nrow(x))
      }),
      album.dc.genres_str = sapply(.data$album.dc.genres, function(x) {
        if (is.list(x)) {
          paste(unlist(x), collapse = "; ")
        } else {
          NA
        }
      }),
      album.dc.styles_str = sapply(.data$album.dc.styles, function(x) {
        if (is.list(x)) {
          paste(unlist(x), collapse = "; ")
        } else {
          NA
        }
      }),
      album.dz.genres_str = track.dz.album.genres
    ) |>
    dplyr::select(
      -dplyr::all_of(c(
        "track.s.artists",
        "album.dc.genres",
        "album.dc.styles",
        "track.dz.album.genres"
      ))
    )
}

combine_mb_genres <- function(input) {
  combine_genres(input, get_combined_mb_tags, "mb.genres")
}

combine_genres <- function(
  input,
  combine_fun,
  genrecol,
  ...
) {
  combined <- combine_fun(input, ...)
  non_empty <- filter_non_empty_tags(combined, genrecol)
  message("Done.")
  non_empty
}

get_combined_mb_tags <- function(input) {
  combine_tag_columns(
    input = input,
    cols = c("track.mb.genres", "album.mb.genres", "artist.mb.genres"),
    builder = choose_mb_tag_set,
    outcol = "mb.genres",
    msg = "Combining MusicBrainz tracks ..."
  )
}

combine_tag_columns <- function(input, cols, builder, outcol, msg = "") {
  message(msg)
  n <- nrow(input)
  out_list <- vector("list", length = ifelse(is.null(n), 0, n))
  source_list <- vector("character", length = ifelse(is.null(n), 0, n))

  if (is.null(n) || n == 0) {
    input[[outcol]] <- out_list
    input[[paste0(outcol, ".source")]] <- source_list
    return(input)
  }

  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  on.exit(close(pb), add = TRUE)

  results <- lapply(seq_len(n), function(i) {
    utils::setTxtProgressBar(pb, i)
    args <- lapply(cols, function(col) input[[col]][[i]])
    do.call(builder, args)
  })

  out_list <- lapply(results, function(x) x$tags)
  source_list <- sapply(results, function(x) x$source)

  input[[outcol]] <- out_list
  input[[paste0(outcol, ".source")]] <- source_list
  input
}

choose_mb_tag_set <- function(
  track_tags,
  album_tags,
  artist_tags
) {
  n_track_tags <- track_tags |> nrow()
  n_album_tags <- album_tags |> nrow()
  if (is.null(n_track_tags)) {
    n_track_tags <- 0
  }
  if (is.null(n_album_tags)) {
    n_album_tags <- 0
  }

  if (n_track_tags != 0) {
    return(list(tags = track_tags, source = "track"))
  }
  if (n_album_tags != 0) {
    return(list(tags = album_tags, source = "album"))
  }
  list(tags = artist_tags, source = "artist")
}

filter_non_empty_tags <- function(input, genrecol) {
  input$is.nonempty <- lapply(input[[genrecol]], function(x) nrow(x) > 0) |>
    as.logical() |>
    tidyr::replace_na(FALSE)
  input$is.tag_name <- lapply(input[[genrecol]], function(x) {
    "tag_name" %in% colnames(x)
  }) |>
    as.logical()
  dplyr::filter(input, .data$is.nonempty & .data$is.tag_name) |>
    dplyr::select(-"is.nonempty", -"is.tag_name")
}

get_long_genre_tags <- function(
  input,
  genrecol,
  caluclate_tag_count = character()
) {
  unpacked_tags <- unpack_genre_tags(input[[genrecol]])

  source_col <- paste0(genrecol, ".source")
  has_source <- source_col %in% colnames(input)

  input$join_id <- seq_len(nrow(input))

  message("Merging genre tags with track info ...")
  result <- dplyr::inner_join(
    input,
    unpacked_tags,
    by = "join_id"
  )

  if (length(caluclate_tag_count) > 0) {
    message("Calculating tag counts ...")
    if (
      !all(caluclate_tag_count %in% c("artist", "total", "ones")) ||
        length(caluclate_tag_count) > 1
    ) {
      stop("If caluclate_tag_count is used, it must be 'artist' or 'total'.")
    } else {
      result <- calculate_tag_counts(result, caluclate_tag_count, has_source)
    }
  }
  message("Done.")
  result
}

unpack_genre_tags <- function(tags) {
  tagged_frames <- vector("list", length(tags))
  message("Unpacking genre tags ...")
  pb <- utils::txtProgressBar(min = 0, max = length(tags), style = 3)
  on.exit(close(pb), add = TRUE)

  for (i in seq_along(tags)) {
    idx <- tags[i]
    df <- idx[[1]]
    df$join_id <- i
    tagged_frames[[i]] <- df
    utils::setTxtProgressBar(pb = pb, value = i)
  }
  message("\nBinding tags ...")
  do.call(rbind, tagged_frames)
}

calculate_tag_counts <- function(tags, method, preserve_source = FALSE) {
  source_col <- if (preserve_source && "tag_source" %in% colnames(tags)) {
    tags$tag_source
  } else {
    NULL
  }

  tags <- tags |> dplyr::select(-"tag_count")
  if (!is.null(source_col)) {
    tags <- tags |> dplyr::select(-"tag_source")
  }

  if (method == "artist") {
    counts <- tags |>
      dplyr::group_by(.data$tag_name, .data$track.s.firstartist.name) |>
      dplyr::summarize(tag_count = dplyr::n(), .groups = "drop")
    result <- dplyr::right_join(
      tags,
      counts,
      by = c("tag_name", "track.s.firstartist.name")
    )
  } else if (method == "total") {
    counts <- tags |>
      dplyr::group_by(.data$tag_name) |>
      dplyr::summarize(tag_count = dplyr::n(), .groups = "drop")
    result <- dplyr::right_join(
      tags,
      counts,
      by = c("tag_name")
    )
  } else if (method == "ones") {
    tags$tag_count <- 1
    result <- tags
  } else {
    stop("Unknown method for calculating tag counts.")
  }

  select_cols <- c(
    "track.s.id",
    "album.dc.id",
    "trackartists.s.id",
    "track.s.title",
    "track.s.firstartist.id",
    "track.s.firstartist.name",
    "n_trackartists",
    "tag_name",
    "tag_count"
  )

  if (!is.null(source_col)) {
    result$tag_source <- source_col
    select_cols <- c(select_cols, "tag_source")
  }

  result |> dplyr::select(dplyr::all_of(select_cols))
}


get_unique_mb_tags <- function(mb_input) {
  all_tags <- c(
    unlist_mb_tags_dataframe(mb_input$track.mb.genres),
    unlist_mb_tags_dataframe(mb_input$album.mb.genres),
    unlist_mb_tags_dataframe(mb_input$artist.mb.genres)
  )
  unique(all_tags)
}

unlist_mb_tags_dataframe <- function(mb_tags) {
  unlist(lapply(mb_tags, function(x) {
    if (is.data.frame(x) && "tag_name" %in% colnames(x)) {
      as.character(x$tag_name)
    } else {
      character(0)
    }
  }))
}

filter_non_valid_tags <- function(long, non_music_tags) {
  dplyr::filter(
    long,
    !(.data$tag_name %in% non_music_tags)
  )
}

filter_min_votes <- function(
  mb_long,
  n_min_votes
) {
  i <- 0
  while (TRUE) {
    message(
      sprintf(
        "Denoising MusicBrainz tags: iteration %d, votes >= %d",
        i,
        n_min_votes
      )
    )
    mb_long_denoise_tracks <- filter_tracks_by_min_votes(
      mb_long,
      min_votes = n_min_votes
    )
    if (nrow(mb_long_denoise_tracks) == nrow(mb_long)) {
      break
    }
    mb_long <- mb_long_denoise_tracks
    i <- i + 1
  }
  return(mb_long_denoise_tracks)
}

filter_tracks_by_min_votes <- function(mb_long, min_votes = 2) {
  mb_total_votes <- mb_long |>
    dplyr::group_by(.data$track.s.id) |>
    dplyr::summarize(
      n_votes = sum(.data$tag_count),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_votes >= min_votes) |>
    dplyr::select(track.s.id)

  mb_long_filtered <- mb_long |>
    dplyr::inner_join(mb_total_votes, by = "track.s.id")

  return(mb_long_filtered)
}
