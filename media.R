#install.packages(c("tidyverse","jsonlite","data.table","lubridate","tidytext","stringi","fs", "quanteda", "stm", "seededlda"))
library(tidyverse)
library(jsonlite)
library(data.table)
library(lubridate)
library(tidytext)
library(tokenizers)
library(quanteda)
library(quanteda.textplots)
library(stringi)
library(fs)
library(stm)
library(seededlda)


# Canonical column set (pad to these if absent)
canon_cols <- c(
  "SpeechID", "POTUS", "Date", "SpeechTitle", "RawText",
  "SpeechURL", "Summary", "Source", "Type",
  "Original Source", "Location", "CleanText"
)

# Directory 
setwd("~/Desktop/DSC161/DSC161-POLI176-Final-Project")
DATA_DIR <- "us2020data/data_clean"

# JSONL Reader
read_one_jsonl <- function(path) {
  con <- file(path, open = "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  df <- jsonlite::stream_in(con, verbose = FALSE)
  
  # replace NULL-like columns 
  df <- as_tibble(df)
  
  # Add missing canonical columns as NA (and keep any extra columns if present)
  missing <- setdiff(canon_cols, names(df))
  if (length(missing) > 0) {
    for (m in missing) df[[m]] <- NA
  }
  # Reorder so canon first, extras later
  df <- df %>% relocate(any_of(canon_cols), .before = 1)
  
  df$source_path <- path
  df$speaker_inferred <- str_match(path, "data_clean/[^/]+/([^/]+)/")[,2]  # e.g., DonaldTrump
  df$source_inferred  <- str_match(path, "data_clean/([^/]+)/")[,2]        # e.g., cspan/medium/...
  
  df
}

# Find all cleantext jsonl files
jsonl_files <- dir_ls(DATA_DIR, recurse = TRUE, regexp = "cleantext_.*\\.jsonl$")

# Read everything (padding columns as needed)
all_rows <- map_dfr(jsonl_files, read_one_jsonl)

# Normalize speaker labels (folder names are CamelCase in this dataset)
normalize_speaker <- function(x) {
  case_when(
    str_detect(x, regex("^donaldtrump$", ignore_case = TRUE)) ~ "DonaldTrump",
    str_detect(x, regex("^joebiden$",     ignore_case = TRUE)) ~ "JoeBiden",
    str_detect(x, regex("^kamalaharris$", ignore_case = TRUE)) ~ "KamalaHarris",
    str_detect(x, regex("^mikepence$",    ignore_case = TRUE)) ~ "MikePence",
    TRUE ~ x
  )
}

all_rows <- all_rows %>%
  mutate(
    speaker_inferred = normalize_speaker(speaker_inferred)
  )

# # Function to group and sort
# group_by_potus <- function(df, potus_name) {
#   df %>%
#     filter(POTUS == potus_name) %>%
#     arrange(Date)
# }
# 
# # Create individual datasets
# trump_df  <- group_by_potus(all_rows, "Donald Trump")
# pence_df  <- group_by_potus(all_rows, "Mike Pence")
# harris_df <- group_by_potus(all_rows, "Kamala Harris")
# biden_df  <- group_by_potus(all_rows, "Joe Biden")
# 
# # Sanity check
# summary_table <- all_rows %>%
#   count(POTUS, sort = TRUE)

all_rows

# PROBLEM SET 1 

## 1) Prep clean text field for pre-processing
# Prefer CleanText; fall back to RawText if CleanText is missing/blank
all_rows_for_lda <- all_rows %>%
  mutate(
    text_for_model = if_else(
      is.na(CleanText) | str_squish(CleanText) == "",
      RawText,
      CleanText
    ),
    text_for_model = str_squish(text_for_model)
  ) %>%
  filter(!is.na(text_for_model), nchar(text_for_model) > 0)

# ## 2) Build quanteda corpus
# corpus_all <- quanteda::corpus(
#   all_rows_for_lda,
#   text_field = "text_for_model"
# )
# 
# # Keep some handy document-level vars
# docvars(corpus_all, "speaker") <- all_rows_for_lda$speaker_inferred %||% all_rows_for_lda$POTUS
# docvars(corpus_all, "date")    <- all_rows_for_lda$Date
# docvars(corpus_all, "title")   <- all_rows_for_lda$SpeechTitle
# docvars(corpus_all, "source")  <- all_rows_for_lda$Source
# 
# ## 3) Tokenize + preprocess
# toks <- tokens(
#   corpus_all,
#   remove_punct   = TRUE,
#   remove_numbers = TRUE,
#   remove_symbols = TRUE,
#   remove_url     = TRUE
# )
# 
# # Lowercase
# toks <- tokens_tolower(toks)
# 
# # Stemming 
# toks <- tokens_wordstem(toks, language = "en")
# 
# # Remove stopwords and very short tokens (e.g., 1–2 chars)
# toks <- tokens_select(
#   toks,
#   pattern = stopwords("en"),
#   selection = "remove"
# )
# toks <- tokens_keep(toks, min_nchar = 3)
# 
# # Create DFM
# dfm_all <- dfm(toks)
# 
# # Trim rare words (keep terms that appear in >= 5% of documents)
# dfm_trimmed <- dfm_trim(dfm_all, min_docfreq = 0.05, docfreq_type = "prop")
# 
# ## 4) LDA (k = 10 topics)
# set.seed(135262007)
# lda_10 <- textmodel_lda(dfm_trimmed, k = 10)
# 
# # Top 10 terms per topic
# lda_terms <- terms(lda_10, 10)
# lda_terms


all_rows_for_lda

## HAND CODING
## ==== Paragraphize, label, and stratify sample ====
## ==== Chunk speeches into fixed word windows (e.g., 40 words) ====

library(dplyr)
library(stringr)
library(tidyr)
library(tokenizers)
library(purrr)
library(readr)

# helper: split one character vector into chunks of n words and return a tibble of chunks
chunk_by_words <- function(text, n = 40) {
  if (is.na(text) || !nzchar(str_squish(text))) {
    return(tibble(chunk_text = character(), chunk_index = integer()))
  }
  # tokenize -> vector of words (lower-level tokens, keep words only)
  w <- tokenizers::tokenize_words(text, simplify = TRUE)
  if (length(w) == 0) {
    return(tibble(chunk_text = character(), chunk_index = integer()))
  }
  # number of chunks
  k <- ceiling(length(w) / n)
  idx <- rep(seq_len(k), each = n)[seq_along(w)]
  # reassemble words back into chunk strings
  tibble(
    chunk_index = seq_len(k),
    chunk_text  = tapply(w, idx, function(x) str_squish(paste(x, collapse = " "))) |> as.character()
  )
}

# choose your window size here
WORDS_PER_CHUNK <- 40  # <- adjust to 30/50/etc if you prefer

# Build chunked dataframe: one row per word-window "paragraph"
chunked_df <- all_rows_for_lda %>%
  mutate(text_for_model = str_squish(text_for_model)) %>%
  # map over rows, create chunks, and keep doc-level vars for traceability
  mutate(chunks = map(text_for_model, ~ chunk_by_words(.x, n = WORDS_PER_CHUNK))) %>%
  select(
    SpeechID, POTUS, Date, SpeechTitle, SpeechURL, Source, CleanText, RawText,
    source_inferred, speaker_inferred, text_for_model, chunks
  ) %>%
  unnest(chunks) %>%
  rename(paragraph_text = chunk_text,
         paragraph_in_speech = chunk_index) %>%
  filter(nchar(paragraph_text) > 0)

# Global unique paragraph id
chunked_df <- chunked_df %>% mutate(paragraph_id = row_number())

# Global unique id 
chunked_df$id <- 1:nrow(chunked_df)
chunked_df

# Media-mention flag
media_pattern <- regex(paste0(
  "\\b(",
  paste(c(
    # Core media / channels
    "media","news","press","newsroom","outlets?","newspapers?","magazines?","tabloids?",
    "publishers?","publications?","headlines?","bylines?","op-?eds?","editorials?",
    "articles?","coverage","reporting",
    # Actors
    "reporters?","journalists?","commentators?","pundits?",
    "broadcasters?","press\\s+corps",
    # Formats / events
    "press\\s+conference(?:s)?","press\\s+briefing(?:s)?","press\\s+release(?:s)?","interviews?",
    "talk\\s+shows?",
    # Disinfo / evaluative
    "fake\\s+news","misinformation","disinformation","propaganda","hoax(?:es)?","smears?","spin",
    "hit\\s+piece(?:s)?","puff\\s+piece(?:s)?","soundbites?","ratings?","readership","viewership",
    "fact-?check(?:ing|er|ers)?"
  ), collapse = "|"),
  ")\\b"
), ignore_case = TRUE)


chunked_df <- chunked_df %>%
  mutate(media_mention = as.integer(str_detect(paragraph_text, media_pattern)))

# Check
chunked_df

# Stratified sample by speaker × media_mention
set.seed(92073)
n_per_stratum <- 25  # adjust as needed

sampled_paragraphs <- chunked_df %>%
  group_by(speaker_inferred, media_mention) %>%
  group_split() %>%
  purrr::map_dfr(~ {
    take <- min(n_per_stratum, nrow(.x))
    .x %>% dplyr::slice_sample(n = take)
  }) %>%
  ungroup()

# Check balance
sampled_paragraphs
# Count by speaker and overall
sampled_paragraphs %>%
  dplyr::summarise(total_media = sum(media_mention == 1, na.rm = TRUE)) 

sampled_paragraphs %>%
  dplyr::count(speaker_inferred, media_mention) %>%
  tidyr::pivot_wider(names_from = media_mention, values_from = n, values_fill = 0) %>%
  dplyr::rename(non_media = `0`, media = `1`)


# Hand-coding export (lean fields)
handcoding_set <- sampled_paragraphs %>%
  transmute(
    id,
    paragraph_id,
    speaker   = speaker_inferred,
    date      = as.character(Date),
    title     = SpeechTitle,
    source    = Source %||% source_inferred,
    url       = SpeechURL,
    paragraph_in_speech,           # position within speech (1..k)
    paragraph_text
  )

# Sort handcoding for randomization
handcoding_set
set.seed(92073)
handcoding_set <- handcoding_set %>% dplyr::slice_sample(prop = 1)


# Write csv for coding
write_csv(handcoding_set, "handcoding_forcoding.csv")


# Analysis
## ==== Hand-coding reliability analysis ====

library(tidyverse)
library(irr)

# 1) Read combined handcoding results
handcoding <- read_csv("handcoding_combined.csv")

# 2) Quick structure check
glimpse(handcoding)
summary(handcoding[c("Media1", "Media2")])

# 3) Confusion matrix (cross-tab)
conf_mat <- table(handcoding$Media1, handcoding$Media2)
print(conf_mat)

# Optional: nicer labeled display
addmargins(conf_mat)

# 4) Krippendorff's alpha (nominal scale)
# irr::kripp.alpha() expects a matrix or data.frame with coders in columns and items in rows
alpha_result <- irr::kripp.alpha(t(handcoding[, c("Media1", "Media2")]))
print(alpha_result)

# 5) Percent agreement
agree_pct <- mean(handcoding$Media1 == handcoding$Media2, na.rm = TRUE)
cat("\nPercent agreement:", round(100 * agree_pct, 1), "%\n")

# 6) Inspect disagreements
disagreements <- handcoding %>%
  filter(Media1 != Media2) %>%
  select(id, paragraph_id, speaker, paragraph_text, Media1, Media2)

cat("\nNumber of disagreements:", nrow(disagreements), "\n")
print(disagreements %>% slice_head(n = 10))  # preview first 10

# 7) Agreement by speaker (optional stratified check)
by_speaker <- handcoding %>%
  group_by(speaker) %>%
  summarise(
    n = n(),
    agreement = mean(Media1 == Media2, na.rm = TRUE),
    .groups = "drop"
  )
print(by_speaker)


