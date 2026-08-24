library(stringr)

preprocess_lyrics <- function(lyrics) {
  tolower(lyrics) |>
    trimws() |>
    remove_bracket_content() |>
    unify_asterisks() |>
    remove_repitition_indicators() |>
    remove_song_structure_indicators() |>
    fix_english_apostrophes()
}


remove_bracket_content <- function(lyrics) {
  lyrics |>
    stringr::str_replace_all("\\[.*?\\]", "") |>
    stringr::str_replace_all("\\{.*?\\}", "")
}

unify_asterisks <- function(lyrics) {
  # english domain + remove all content between two asterisks + asterisks with optional following number
  lyrics |>
    stringr::str_replace_all("f\\*\\*k", "fuck") |>
    stringr::str_replace_all("f\\*\\*\\*", "fuck") |>
    stringr::str_replace_all("f\\*ck", "fuck") |>
    stringr::str_replace("s\\*\\*\\*", "shit") |>
    stringr::str_replace("s\\*\\*t", "shit") |>
    stringr::str_replace("sh\\*t", "shit") |>
    stringr::str_replace("s\\*\\*", "sex") |>
    stringr::str_replace("d\\*\\*\\*", "dick") |>
    stringr::str_replace("p\\*\\*\\*\\*", "pussy") |>
    stringr::str_replace("p\\*ssy", "pussy") |>
    stringr::str_replace("nigg\\*", "nigga") |>
    stringr::str_replace("b\\*\\*\\*\\*", "bitch") |>
    stringr::str_replace_all("\\*.*?\\*", "") |>
    stringr::str_replace_all("\\s*\\*\\s*\\d*", "")
}

remove_repitition_indicators <- function(lyrics) {
  lyrics |>
    stringr::str_replace_all("\\s(\\d+)x$", "") |> # "word 2x"
    stringr::str_replace_all("^x(\\d+)\\s", "") |> # "x2 word"
    stringr::str_replace_all("\\s\\((\\d+)\\)$", "") |> # "word (2)"
    stringr::str_replace_all("^\\((\\d+)\\)\\s", "") |> # "(2) word"
    stringr::str_replace_all("\\s(\\d+) x$", "") |> # "word 2 x"
    stringr::str_replace_all("\\s*\\(.*(\\d+)\\s*x.*\\)\\s*", "") |> # "word (2x)" and everything in the bracket
    stringr::str_replace_all("\\s*\\(.*x\\s*(\\d+)\\s*\\)\\s*", "") # "word (x2)" and everything in the bracket
}

remove_song_structure_indicators <- function(lyrics) {
  lyrics |>
    stringr::str_replace_all(
      "\\s*\\(\\s*(refrain|strophe|wiederholung|rap-verser|chorus|verse|hook|intro|outro|bridge|pre-chorus|pre chorus|repeat chorus|repeat|repeat-chorus)\\s*\\d*\\s*\\:*\\)\\s*\\:*\\s*",
      ""
    ) |> # indicators in round brackets, e.g. "word (chorus)", "word (chorus 2)", "word (chorus:)", "word (chorus 2:)", "word (chorus) :", "word (chorus 2) :"
    stringr::str_replace_all(
      "(^|\\n)\\s*(refrain|strophe|wiederholung|rap-verser|chorus|verse|hook|intro|outro|bridge|pre-chorus|pre chorus|repeat chorus|repeat|repeat-chorus)\\s*\\d*\\s*\\:*\\s*\\n",
      "\n"
    ) # indicators as whole line, e.g. "\nchorus\n", "\nchorus 2\n", "\nchorus:\n", "\nchorus 2:\n"
}

fix_english_apostrophes <- function(lyrics) {
  lyrics |>
    # unify different apostrophes
    stringr::str_replace_all("[‘’`´′]", "'") |>
    # 't cases
    stringr::str_replace_all("\\b(ain)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(can)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(couldn)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(didn)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(doesn)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(don)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(hadn)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(hasn)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(haven)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(isn)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(shouldn)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(wasn)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(weren)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(won)\\s*t\\b", "\\1't") |>
    stringr::str_replace_all("\\b(wouldn)\\s*t\\b", "\\1't") |>
    # 's cases
    stringr::str_replace_all("\\b(he)\\s*s\\b", "\\1's") |>
    stringr::str_replace_all("\\b(how)\\s*s\\b", "\\1's") |>
    stringr::str_replace_all("\\b(it)\\s*s\\b", "\\1's") |>
    stringr::str_replace_all("\\b(let)\\s*s\\b", "\\1's") |>
    stringr::str_replace_all("\\b(she)\\s*s\\b", "\\1's") |>
    stringr::str_replace_all("\\b(that)\\s*s\\b", "\\1's") |>
    stringr::str_replace_all("\\b(there)\\s*s\\b", "\\1's") |>
    stringr::str_replace_all("\\b(what)\\s*s\\b", "\\1's") |>
    stringr::str_replace_all("\\b(where)\\s*s\\b", "\\1's") |>
    stringr::str_replace_all("\\b(who)\\s*s\\b", "\\1's") |>
    # 're cases
    stringr::str_replace_all("\\b(you)\\s*re\\b", "\\1're") |>
    stringr::str_replace_all("\\b(they)\\s*re\\b", "\\1're") |>
    stringr::str_replace_all("\\b(we)\\s*re\\b", "\\1're") |>
    # 'll cases
    stringr::str_replace_all("\\b(i)\\s*ll\\b", "\\1'll") |>
    stringr::str_replace_all("\\b(you)\\s*ll\\b", "\\1'll") |>
    stringr::str_replace_all("\\b(he)\\s*ll\\b", "\\1'll") |>
    stringr::str_replace_all("\\b(she)\\s*ll\\b", "\\1'll") |>
    stringr::str_replace_all("\\b(we)\\s*ll\\b", "\\1'll") |>
    stringr::str_replace_all("\\b(they)\\s*ll\\b", "\\1'll") |>
    stringr::str_replace_all("\\b(who)\\s*ll\\b", "\\1'll") |>
    # 've cases
    stringr::str_replace_all("\\b(i)\\s*ve\\b", "\\1've") |>
    stringr::str_replace_all("\\b(you)\\s*ve\\b", "\\1've") |>
    stringr::str_replace_all("\\b(we)\\s*ve\\b", "\\1've") |>
    stringr::str_replace_all("\\b(they)\\s*ve\\b", "\\1've") |>
    stringr::str_replace_all("\\b(who)\\s*ve\\b", "\\1've") |>
    # 'd cases
    stringr::str_replace_all("\\b(i)\\s*d\\b", "\\1'd") |>
    stringr::str_replace_all("\\b(you)\\s*d\\b", "\\1'd") |>
    stringr::str_replace_all("\\b(he)\\s*d\\b", "\\1'd") |>
    stringr::str_replace_all("\\b(she)\\s*d\\b", "\\1'd") |>
    stringr::str_replace_all("\\b(we)\\s*d\\b", "\\1'd") |>
    stringr::str_replace_all("\\b(they)\\s*d\\b", "\\1'd") |>
    stringr::str_replace_all("\\b(who)\\s*d\\b", "\\1'd")
}


build_english_corpus <- function(df) {
  df |>
    filter(track.language == "English") |>
    mutate(
      lyrics = map_chr(track.g.lyrics, ~ paste(.$line, collapse = "\n"))
    ) |>
    select(
      track.s.id,
      track.s.title,
      track.s.firstartist.id,
      track.s.firstartist.name,
      album.s.releaseyear,
      lyrics,
      cat,
      n_lines
    )
}

build_german_corpus <- function(df) {
  df |>
    filter(track.language == "German" & track.is.deepltranslation) |>
    mutate(
      lyrics_de = map_chr(track.g.lyrics, ~ paste(.$line, collapse = "\n")),
      lyrics_en = map_chr(track.g.lyrics_en, ~ paste(.$line, collapse = "\n"))
    ) |>
    select(
      track.s.id,
      track.s.title,
      track.s.firstartist.id,
      track.s.firstartist.name,
      album.s.releaseyear,
      lyrics_de,
      lyrics_en,
      cat,
      n_lines
    )
}


get_language_confidence <- function(
  corpus,
  lyrics_col,
  target_language = "en"
) {
  fasttext_modelpath <- file.path("data-raw/lid.176.bin")
  ids <- corpus$track.s.id
  n_lines <- corpus$n_lines
  result_trackids <- rep(ids, n_lines, each = TRUE)
  res <- fastText::language_identification(
    corpus[[lyrics_col]],
    pre_trained_language_model_path = fasttext_modelpath,
    k = 1,
    th = 0,
    threads = 1,
    verbose = TRUE
  )
  res$track.s.id <- result_trackids
  res_aggregated <- res |>
    group_by(track.s.id) |>
    summarise(
      n_lines = n(),
      n_de = sum(iso_lang_1 == target_language),
      mean_prob_target = mean(
        prob_1[iso_lang_1 == target_language],
        na.rm = TRUE
      )
    ) |>
    mutate(share_target = n_de / n_lines) |>
    left_join(corpus |> select(-n_lines), by = "track.s.id")
  res_aggregated
}
