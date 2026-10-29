source("analysis/helpers/shared_helpers.R")

get_dc_equiv_sets <- function(mb_genres, dc_genres) {
  equiv <- vector(mode = "list", length = length(dc_genres))
  names(equiv) <- dc_genres

  fully_eq <- get_mb_intersection(mb_genres, dc_genres)
  equiv <- add_to_equiv_mapping(equiv, fully_eq)

  # get genres combined in Discogs but equivalent single MusicBrainz genre
  broken_eq <- get_mb_intersection(
    mb_genres,
    c(
      "Folk, World, & Country",
      "Folk, World, & Country",
      "Folk, World, & Country",
      "Funk / Soul",
      "Funk / Soul"
    ),
    proxy_genres = c(
      "folk",
      "world",
      "country",
      "funk",
      "soul"
    )
  )
  equiv <- add_to_equiv_mapping(equiv, broken_eq)

  manual_eq <- c(
    "Funk / Soul" = "r&b",
    "Stage & Screen" = "cinematic classical"
  )
  equiv <- add_to_equiv_mapping(equiv, manual_eq)

  equiv_found <- equiv[sapply(equiv, function(x) length(x) > 0)]
  # remove no equivalent and print
  message(sprintf(
    "Did not find equivalent for: %s",
    paste(setdiff(names(equiv), names(equiv_found)), collapse = ", ")
  ))

  equiv_found[sort(names(equiv_found))]
}

get_dz_equiv <- function(mb_genres, dz_genres) {
  equiv <- vector(mode = "list", length = length(dz_genres))
  names(equiv) <- dz_genres

  fully_eq <- get_mb_intersection(mb_genres, dz_genres)
  equiv <- add_to_equiv_mapping(equiv, fully_eq)
  # get genres combined in Deezer but equivalent single MusicBrainz genre
  broken_eq <- get_mb_intersection(
    mb_genres,
    c(
      "Techno/House",
      "Techno/House",
      "Rap/Hip Hop",
      "Rap/Hip Hop",
      "Singer & Songwriter",
      "Singer & Songwriter",
      "Indie Rock/Rock Pop",
      "Indie Rock/Rock Pop",
      "Schlager & Volksmusik",
      "Schlager & Volksmusik",
      "Movies/Video Games",
      "Movies/Video Games"
    ),
    proxy_genres = c(
      "techno",
      "house",
      "rap",
      "hip hop",
      "singer",
      "songwriter",
      "indie rock",
      "rock pop",
      "schlager",
      "volksmusik",
      "movies",
      "video games"
    )
  )
  equiv <- add_to_equiv_mapping(equiv, broken_eq)
  manual_eq <- c(
    "Alternative" = "alternative rock",
    "Heavy Metal" = "metal",
    "Indie Rock/Rock Pop" = "pop rock",
    "Singer & Songwriter" = "singer-songwriter",
    "Latin Music" = "latin",
    "Electro" = "electronic",
    "Soundtrack" = "classical",
    "Soundtrack" = "cinematic classical"
  )
  equiv <- add_to_equiv_mapping(equiv, manual_eq)
  equiv_found <- equiv[sapply(equiv, function(x) length(x) > 0)]
  # remove no equivalent and print
  message(sprintf(
    "Did not find equivalent for: %s",
    paste(setdiff(names(equiv), names(equiv_found)), collapse = ", ")
  ))

  equiv_found[sort(names(equiv_found))]
}

run_external_compare <- function(equiv, mb, external, dag, taxonomy_label) {
  #' Aggregate SHAMPOO to the MusicBrainz equivalents of an external taxonomy and compare hard assignments in a contingency table.
  #' @param equiv A named list mapping external genres to their MusicBrainz equivalents.
  #' @param mb The long MusicBrainz data (columns id, tag_name, tag_count).
  #' @param external The long single-label external data (columns id, tag_name).
  #' @param dag The SHAMPOO genre DAG of the MusicBrainz data.
  #' @param taxonomy_label The name of the external taxonomy.
  #' @return A list with the hard class contingency data and the plot.
  cat_states_target <- shampoo::aggregate_genre_categories(
    dag,
    target_set = unique(unlist(equiv))
  )
  hard_classes <- shampoo::assign_genre_categories(
    mb,
    cat_states_target,
    method = "hard"
  ) |>
    select(id, cat) |>
    inner_join(
      distinct(external, id, ext_cat = tag_name),
      by = "id"
    )
  plot <- plot_hard_classes_contingency(
    hard_classes,
    taxonomy_label,
    unique(cat_states_target$target_set),
    right_margin_pt = 50
  )
  list(hard_classes = hard_classes, plot = plot)
}

calc_ginis <- function(external, hard_classes, mb, cat_states) {
  #' Compare the Gini coefficient of the external hard classes with the SHAMPOO solution of the same granularity (aggregation order by size and generality) on the same tracks.
  external_levels <- unique(external$tag_name)
  shampoo_classes <- shampoo::assign_genre_categories(
    filter(mb, id %in% hard_classes$id),
    cat_states,
    length(external_levels),
    method = "both"
  )
  list(
    external_gini = gini_vec(hard_classes$ext_cat, external_levels),
    shampoo_gini = gini_vec(
      shampoo_classes$cat,
      get_category_columns(shampoo_classes)
    ),
    n_categories = length(external_levels)
  )
}

get_tag_names <- function(dt, col) {
  dt[[col]] |> unique()
}

get_mb_intersection <- function(mb_genres, ext_genres, proxy_genres = NULL) {
  if (is.null(proxy_genres)) {
    # return a mapping to ext_genres that are compeltely equivalent to a single genre in MusicBrainz
    res <- sapply(ext_genres, function(x) {
      lower <- tolower(x)
      if (lower %in% mb_genres) {
        lower
      } else {
        NA
      }
    })
  } else {
    # return mapping to ext_genres for mb_genres completely equivalent to proxy_genre at same position
    res <- sapply(seq_along(proxy_genres), function(i) {
      proxy <- tolower(proxy_genres[i])
      ext <- ext_genres[i]
      names(proxy) <- ext
      if (proxy %in% mb_genres) {
        proxy
      } else {
        NA
      }
    })
  }
  res <- res[!is.na(res)]
}

add_to_equiv_mapping <- function(equiv, mapping) {
  for (i in seq_along(mapping)) {
    ext_genre <- names(mapping)[i]
    mb_genre <- mapping[i]
    names(mb_genre) <- NULL
    equiv[[ext_genre]] <- c(equiv[[ext_genre]], mb_genre)
  }
  equiv
}

apply_ext_translation <- function(dz_dt, trans_map) {
  dz_dt |>
    mutate(
      tag_name = ifelse(
        tag_name %in% names(trans_map),
        trans_map[tag_name],
        tag_name
      )
    )
}

get_dz_translation_map <- function() {
  c(
    "Latin Musik" = "Latin Music",
    "Deutsche Musik" = "German Music",
    "Deutsch-Rap" = "German Rap",
    "Asiatische Musik" = "Asian Music",
    "Afrikanische Musik" = "African Music",
    "Brasilianische Musik" = "Brazilian Music",
    "Filme/Videospiele" = "Movies/Video Games",
    "Filmmusik" = "Soundtrack",
    "Klassik" = "Classical"
  )
}

gini_vec <- function(vec, levels = NULL) {
  counts <- if (is.null(levels)) {
    table(vec)
  } else {
    table(factor(vec, levels = levels))
  }
  DescTools::Gini(as.numeric(counts), unbiased = TRUE)
}


plot_hard_classes_contingency <- function(
  contingency,
  taxonomy_label,
  mb_order,
  right_margin_pt = 0
) {
  n_label <- sprintf("N=%s", format(nrow(contingency), big.mark = ","))
  mb_prev <- contingency$cat |> table() |> format(big.mark = ",") |> trimws()
  zero_pad <- setdiff(mb_order, names(mb_prev))
  mb_prev <- c(mb_prev, setNames(rep("0", length(zero_pad)), zero_pad))

  ext_prev <- contingency$ext_cat |>
    table() |>
    format(big.mark = ",") |>
    trimws()

  ctable <- count(contingency, ext_cat, cat) |>
    group_by(ext_cat) |>
    mutate(prop = round(n / sum(n) * 100)) |>
    ungroup() |>
    mutate(
      cat = factor(cat, levels = mb_order),
      ext_cat = forcats::fct_rev(forcats::fct_inorder(ext_cat)),
      text_color = ifelse(prop > 50, "white", "black"),
      text_color = ifelse(prop == 50, "white", text_color),
      label = paste0(ifelse(prop == 0, "~", prop))
    )
  # zero pad all combinations of mb and ext_cat
  ctable <- tidyr::complete(
    ctable,
    ext_cat = unique(ctable$ext_cat),
    cat = unique(ctable$cat),
    fill = list(n = 0, prop = 0, text_color = "black", label = "0")
  )

  # add prevalences to the axis labels
  ctable$cat <- factor(
    ctable$cat,
    levels = mb_order,
    labels = sprintf("%s (%s)", mb_order, mb_prev[mb_order])
  )
  ctable$ext_cat <- factor(
    ctable$ext_cat,
    levels = levels(ctable$ext_cat),
    labels = sprintf(
      "%s (%s)",
      levels(ctable$ext_cat),
      ext_prev[levels(ctable$ext_cat)]
    )
  )

  ggplot(
    ctable,
    aes(
      x = .data$cat,
      y = .data$ext_cat,
      fill = .data$prop
    )
  ) +
    geom_tile(color = "white") +
    geom_text(
      aes(label = label, color = .data$text_color),
      size = 5
    ) +
    scale_fill_gradient(low = "white", high = "#c40d20") +
    scale_color_manual(values = c("black", "white")) +
    scale_x_discrete(position = "top", expand = expansion(mult = 0)) +
    scale_y_discrete(expand = expansion(mult = 0)) +
    labs(
      title = n_label,
      x = "SHAMPOO-Categories",
      y = taxonomy_label
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = ggtext::element_markdown(
        size = 16,
        face = "bold",
        color = "black",
        hjust = 0,
        vjust = -0.5,
        lineheight = 1.5,
        margin = margin(b = -16)
      ),
      plot.title.position = "plot",
      plot.margin = margin(
        t = 0,
        r = right_margin_pt,
        b = 0,
        l = 0,
        unit = "pt"
      ),
      legend.position = "none",
      axis.text.x = element_text(
        angle = 70,
        hjust = 0,
        vjust = 0,
        size = 14
      ),
      axis.text.y = element_text(hjust = 1, vjust = 0, size = 14),
      axis.title.x = element_text(hjust = 0),
      axis.title.y = element_text(hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(
        size = 16,
        face = "bold",
        color = "grey45"
      )
    )
}
