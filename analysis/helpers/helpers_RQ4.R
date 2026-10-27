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

  manual_eq <- c("Funk / Soul" = "r&b")
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
  # some German genre names in Deezer mapped back to English
  dz_map <- get_dz_translation_map()
  dz <- apply_ext_translation(dz, dz_map)

  mb_genres <- get_tag_names(mb, "tag_name")
  dz_genres <- get_tag_names(dz, "tag_name")

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
      "Dancehall/Ragga",
      "Dancehall/Ragga",
      "Rock & Roll/Rockabilly",
      "Rock & Roll/Rockabilly",
      "Schlager & Volksmusik",
      "Schlager & Volksmusik",
      "Movies/Video Games",
      "Movies/Video Games",
      "Soul & Funk",
      "Soul & Funk"
    ),
    proxy_genres = c(
      "techno",
      "house",
      "rap",
      "hip hop",
      "singer",
      "songwriter",
      "rock pop",
      "dancehall",
      "ragga",
      "rock & roll",
      "rockabilly",
      "schlager",
      "volksmusik",
      "movies",
      "video games",
      "soul",
      "funk"
    )
  )
  equiv <- add_to_equiv_mapping(equiv, broken_eq)
  manual_eq <- c(
    "Alternative" = "alternative rock",
    "Heavy Metal" = "metal",
    "Indie Rock/Rock Pop" = "pop rock",
    "Singer & Songwriter" = "singer-songwriter",
    "Latin Music" = "latin",
    "Asian Music" = "k-pop",
    "Asian Music" = "j-pop",
    "Asian Music" = "j-rock",
    "Rock & Roll/Rockabilly" = "rock and roll",
    "Brazilian Music" = "bossa nova",
    "Brazilian Music" = "mpb",
    "Dance" = "electronic"
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

run_dc_compare <- function(
  dc_equiv,
  mb,
  dc,
  P,
  dag,
  cat_states,
  load_states_if_exists = TRUE
) {
  mb_genres <- get_tag_names(mb, "tag_name")
  dc_genres <- get_tag_names(dc, "tag_name")

  proc_order <- get_proc_order(mb_genres, dc_equiv)

  if (
    load_states_if_exists && file.exists("models/dag/cat_states_discogs.rds")
  ) {
    cat_states_dc <- readRDS("models/dag/cat_states_discogs.rds")
  } else {
    cat_states_dc <- build_cat_states(
      dag,
      P,
      proc_order,
      "models/dag/cat_states_discogs.rds"
    )
  }

  hard_dc <- get_hard_classes(
    mb,
    dc,
    P,
    cat_states_dc,
    tax_label = "Discogs",
    k = length(unique(unlist(dc_equiv)))
  )
  hard_dc$ext_cat <- hard_dc$discogs_cat

  hard_dc_enriched <- add_preview_urls(
    hard_classes = hard_dc,
    mb = mb,
    by_col = "track.s.id",
    cat_col = "discogs_cat"
  )

  plot <- plot_conting(
    hard_dc,
    "Discogs",
    get_equiv_plot_order(dc_equiv),
    margin_pt = 20
  )

  list(hard_dc = hard_dc, hard_dc_enriched = hard_dc_enriched, plot = plot)
}

run_dz_compare <- function(
  dz_equiv,
  mb,
  dz,
  P,
  dag,
  cat_states,
  load_states_if_exists = TRUE
) {
  k <- length(unique(unlist(dz_equiv)))
  proc_order <- get_proc_order(mb_genres, dz_equiv)

  if (
    load_states_if_exists && file.exists("models/dag/cat_states_deezer.rds")
  ) {
    cat_states_dz <- readRDS("models/dag/cat_states_deezer.rds")
  } else {
    cat_states_dz <- build_cat_states(
      dag,
      P,
      proc_order,
      "models/dag/cat_states_deezer.rds"
    )
  }

  hard_dz <- get_hard_classes(
    mb,
    dz,
    P,
    cat_states_dz,
    tax_label = "Deezer",
    k = k
  )
  hard_dz$ext_cat <- ifelse(
    hard_dz$deezer_cat %in% names(get_dz_translation_map()),
    get_dz_translation_map()[hard_dz$deezer_cat],
    hard_dz$deezer_cat
  )

  hard_dz_enriched <- add_preview_urls(
    hard_classes = hard_dz,
    mb = mb,
    by_col = "track.s.id",
    cat_col = "deezer_cat"
  )

  plot <- plot_conting(
    hard_dz,
    "Deezer",
    get_equiv_plot_order(dz_equiv),
    margin_pt = 25
  )
  list(hard_dz = hard_dz, hard_dz_enriched = hard_dz_enriched, plot = plot)
}


calc_ginis <- function(res_dc, res_dz, dc, dz, P, cat_states) {
  dc_levels <- unique(dc$tag_name)
  dz_levels <- unique(dz$tag_name)
  list(
    dc_gini = gini_vec(res_dc$hard_dc$discogs_cat, dc_levels),
    dz_gini = gini_vec(res_dz$hard_dz$deezer_cat, dz_levels),
    mb_gini_dc = get_mb_gini(
      P,
      cat_states,
      length(dc_levels),
      res_dc$hard_dc$track.s.id
    ),
    mb_gini_dz = get_mb_gini(
      P,
      cat_states,
      length(dz_levels),
      res_dz$hard_dz$track.s.id
    ),
    n_dc = length(dc_levels),
    n_dz = length(dz_levels)
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

get_ext_setdiff <- function(ext_genres, equiv) {
  setdiff(ext_genres, equiv)
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

get_proc_order <- function(mb_genres, equiv) {
  flat_equiv <- unlist(equiv)
  c(setdiff(mb_genres, flat_equiv), flat_equiv)
}

get_equiv_plot_order <- function(equiv) {
  setNames(unlist(equiv), NULL)
}

build_cat_states <- function(dag, P, proc_order, rds_path) {
  cat_states_comp <- get_genre_categories_from_graph(
    dag,
    P,
    processing_order = proc_order,
    fast = TRUE
  )
  saveRDS(cat_states_comp, rds_path)
  readRDS(rds_path)
}

get_hard_classes <- function(mb, ext, P, cat_states_comp, tax_label, k) {
  get_hard_class_contingency(
    mb,
    ext,
    P,
    cat_states_comp,
    taxonomy_label = tax_label,
    chosen_k = k
  )
}

enrich_hard_classes <- function(hard_classes, ext_cat_col, mb, cat_col) {
  ext_cat <- hard_classes[[ext_cat_col]]
  hc <- hard_classes
  hc$ext_cat <- ext_cat
  hc
}

add_preview_urls <- function(hard_classes, mb, by_col, cat_col) {
  hard_classes |>
    left_join(
      distinct(mb, track.s.id, .keep_all = TRUE),
      by = "track.s.id"
    ) |>
    select(cat_col, cat, track.s.previewurl, everything())
}

plot_conting <- function(hard_classes, tax_label, order_vec, margin_pt) {
  plot_hard_classes_contingency(
    hard_classes,
    tax_label,
    order_vec,
    right_margin_pt = margin_pt
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


count_unique <- function(vec) {
  length(unique(vec))
}

get_mb_gini <- function(P, cat_states, n_cat, track_ids = NULL) {
  if (!is.null(track_ids)) {
    P <- P[rownames(P) %in% track_ids, , drop = FALSE]
  }
  tm <- get_track_category_probabilities(P, cat_states, n_cat)
  retained <- setdiff(colnames(tm), c("cat", "cat_prob"))
  gini_vec(tm$cat, levels = retained)
}

get_hard_class_contingency <- function(
  mb,
  filtered_cat,
  P_mb,
  cat_states_mb,
  taxonomy_label,
  chosen_k
) {
  mb_map <- get_track_category_probabilities(
    P_mb,
    cat_states_mb,
    chosen_k
  )
  mb_map_id <- add_track_map_to_long(mb, mb_map) |> distinct(track.s.id, cat)
  cat_wide <- filtered_cat |>
    dplyr::distinct(track.s.id, tag_name) |>
    dplyr::transmute(
      track.s.id,
      !!paste0(tolower(taxonomy_label), "_cat") := tag_name
    )

  dplyr::inner_join(
    cat_wide,
    mb_map_id,
    by = "track.s.id"
  ) |>
    as.data.frame()
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
