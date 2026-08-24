# Clean workspace and set parameters ----
rm(list = ls())
gc()
library(ggplot2)
source("analysis/helpers/helpers_prepare_corpora.R")

# USER SETTINGS ----
MIN_VOTES <- 2
MIN_ARTIST_PREV_MODEL <- 0.0005
MIN_ARTIST_PREV_COMPARISON <- 0.001

MB_WHITELIST_PATH <- "data-raw/musicbrainz_genre_whitelist_20250616.rds"
MB_NON_MUSIC_TAGS <- c(
  "non-music",
  "interview",
  "comedy",
  "nature sounds",
  "children's music",
  "spoken word",
  "standup comedy",
  "audio drama"
)
saveRDS(MB_NON_MUSIC_TAGS, "data/long_data/mb_non_music_tags.rds")

DZ_NON_MUSIC_TAGS <- c(
  "Comedy",
  "Hörbücher",
  "Kids",
  "Hörbücher auf Deutsch",
  "Schlaflieder"
)
saveRDS(DZ_NON_MUSIC_TAGS, "data/long_data/dz_non_music_tags.rds")

S_NON_MUSIC_TAGS <- c("talent show")
saveRDS(S_NON_MUSIC_TAGS, "data/long_data/s_non_music_tags.rds")


# PREPARE CORPORA ----
poptrag_selected <- prepare_poptrag_selected("data-raw/poptrag.rds")
# saveRDS(poptrag_selected, "data/poptrag_selected.rds")

mb_long <- prepare_musicbrainz_corpus(
  poptrag_selected,
  MB_WHITELIST_PATH,
  MB_NON_MUSIC_TAGS,
  MIN_ARTIST_PREV_MODEL
)
save_tags_csv(mb_long, "data/long_data/mb_long.csv")

discogs_long <- prepare_discogs_corpus(
  poptrag_selected,
  mb_long,
  MIN_ARTIST_PREV_COMPARISON
)
save_tags_csv(discogs_long, "data/long_data/dc_long.csv")

deezer_long <- prepare_deezer_corpus(
  poptrag_selected,
  mb_long,
  DZ_NON_MUSIC_TAGS,
  MIN_ARTIST_PREV_COMPARISON
)
save_tags_csv(deezer_long, "data/long_data/dz_long.csv")

spotify_long <- prepare_spotify_corpus(
  poptrag_selected,
  S_NON_MUSIC_TAGS,
  0 # Done later with the ML training corpus
)
save_tags_csv(spotify_long, "data/long_data/s_long.csv")

plot_corpus_composition(mb_long)
ggsave(
  "reports/figures/fig02_corpus_composition.png",
  width = 6,
  height = 5,
  dpi = 600
)
