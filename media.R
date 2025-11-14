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

## Prep clean text field for pre-processing
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

# Read combined handcoding results
handcoding <- read_csv("handcoding_coded.csv")

# Quick structure check
glimpse(handcoding)
summary(handcoding[c("Media1", "Media2")])

# Confusion matrix
conf_mat <- table(handcoding$Media1, handcoding$Media2)
print(conf_mat)

# nicer labeled CM
addmargins(conf_mat)

# Krippendorff's alpha (nominal scale)
alpha_result <- irr::kripp.alpha(t(handcoding[, c("Media1", "Media2")]))
print(alpha_result)

# Percent agreement
agree_pct <- mean(handcoding$Media1 == handcoding$Media2, na.rm = TRUE)
cat("\nPercent agreement:", round(100 * agree_pct, 1), "%\n")

# Inspect disagreements
disagreements <- handcoding %>%
  filter(Media1 != Media2) %>%
  select(id, paragraph_id, speaker, paragraph_text, Media1, Media2)

cat("\nNumber of disagreements:", nrow(disagreements), "\n")
print(disagreements %>% slice_head(n = 10))  # preview first 10

# Save full disagreements list
write_csv(disagreements, "coder_disagreements.csv")

###############################################
###############################################
###############################################

# Merge final coder agreements
library(readr)
library(dplyr)

# Read in final decisions for disagreements
coder_agreements <- read_csv("coder_agreements.csv")
# Expecting columns at least: id, Media3

# Merge into original handcoding data
handcoding_gold <- handcoding %>%
  left_join(coder_agreements %>% select(id, Media3), by = "id") %>%
  mutate(
    # Gold-standard media variable:
    # - if Media3 (final decision) exists, use it
    # - else if coders agreed, use their shared value
    # - else NA (should be rare if all disagreements resolved)
    Media_gold = case_when(
      !is.na(Media3) ~ Media3,
      Media1 == Media2 ~ Media1,
      TRUE ~ NA_real_
    )
  )

# Optional sanity check
cat("\n=== Gold-standard coding summary ===\n")
table(handcoding_gold$Media_gold, useNA = "ifany")

## ==== MODEL / LLM STEP (to be added) ====
# Here we would:
# 1) Use `handcoding_gold` (Media_gold) as training data
# 2) Train a classifier label all paragraphs
# 3) Produce a fully labeled dataset called `classified_full`
#
# Assume that after this step we have:
#   classified_full
# with at least:
#   - paragraph_text
#   - speaker
#   - (optionally) id, paragraph_id, date, etc.
#   - Media_final (gold or model/LLM-assigned media-mention variable)

# 4) Cross-validate model predictions
# 5) Assess and report your out of sample precision and recall. [table with precision and recall]

# Sebastian Naive Bayes




# Christina Logistic Models






## ==== Sentiment analysis with Quanteda + Lexicoder (LSD2015) ====

library(quanteda)
library(quanteda.corpora)  # for data_dictionary_LSD2015

# Ensure party variable in classified_full
classified_full <- classified_full %>%
  mutate(
    party = case_when(
      speaker %in% c("DonaldTrump", "MikePence") ~ "Republican",
      speaker %in% c("JoeBiden", "KamalaHarris") ~ "Democrat",
      TRUE ~ "Other"
    )
  )

# Build corpus using paragraph_text
corpus_full <- corpus(
  classified_full,
  text_field = "paragraph_text"
)

# Attach docvars
docvars(corpus_full, "speaker") <- classified_full$speaker
docvars(corpus_full, "party")   <- classified_full$party

# Tokenize and pre-process (light, like your example)
toks_full <- tokens(
  corpus_full,
  remove_punct   = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE,
  remove_url     = TRUE
)

toks_full <- tokens_tolower(toks_full)
toks_full <- tokens_select(toks_full, stopwords("en"), selection = "remove")

# Apply Lexicoder Sentiment Dictionary (LSD2015)
# Use only positive + negative categories (first two elements)
data_dictionary_LSD2015_pos_neg <- data_dictionary_LSD2015[1:2]

toks_sent <- tokens_lookup(toks_full, dictionary = data_dictionary_LSD2015_pos_neg)
dfm_sent  <- dfm(toks_sent)

# Attach sentiment scores back to classified_full
classified_full <- classified_full %>%
  mutate(
    sent_pos = as.numeric(dfm_sent[, "positive"]),
    sent_neg = as.numeric(dfm_sent[, "negative"]),
    sent_net = sent_pos - sent_neg
  )

# Summaries by party
sent_by_party <- classified_full %>%
  group_by(party) %>%
  summarise(
    n_paragraphs = n(),
    avg_pos      = mean(sent_pos, na.rm = TRUE),
    avg_neg      = mean(sent_neg, na.rm = TRUE),
    avg_net      = mean(sent_net, na.rm = TRUE),
    .groups      = "drop"
  )

cat("\n=== Sentiment by Party ===\n")
print(sent_by_party)

# Summaries by speaker
sent_by_speaker <- classified_full %>%
  group_by(speaker, party) %>%
  summarise(
    n_paragraphs = n(),
    avg_pos      = mean(sent_pos, na.rm = TRUE),
    avg_neg      = mean(sent_neg, na.rm = TRUE),
    avg_net      = mean(sent_net, na.rm = TRUE),
    .groups      = "drop"
  )

cat("\n=== Sentiment by Speaker ===\n")
print(sent_by_speaker)

## ==== Media mention frequency by party and speaker ====
## FOR THIS SECTION, CHANGE Media_final to Media_gold

# Overall media mention rate
cat("\n=== Overall Media Mention Rate ===\n")
overall_media <- classified_full %>%
  summarise(
    n_paragraphs   = n(),
    media_mentions = sum(Media_final == 1, na.rm = TRUE),
    prop_media     = media_mentions / n_paragraphs
  )
print(overall_media)

# By party
cat("\n=== Media Mentions by Party ===\n")
media_by_party <- classified_full %>%
  group_by(party) %>%
  summarise(
    n_paragraphs   = n(),
    media_mentions = sum(Media_final == 1, na.rm = TRUE),
    prop_media     = media_mentions / n_paragraphs,
    .groups        = "drop"
  )
print(media_by_party)

# By speaker
cat("\n=== Media Mentions by Speaker ===\n")
media_by_speaker <- classified_full %>%
  group_by(speaker, party) %>%
  summarise(
    n_paragraphs   = n(),
    media_mentions = sum(Media_final == 1, na.rm = TRUE),
    prop_media     = media_mentions / n_paragraphs,
    .groups        = "drop"
  )
print(media_by_speaker)


# Visualizations
#build ggplot visualizations of avg_net by party/speaker.
