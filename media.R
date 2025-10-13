#install.packages(c("tidyverse","jsonlite","data.table","lubridate","tidytext","stringi","fs"))
library(tidyverse)
library(jsonlite)
library(data.table)
library(lubridate)
library(tidytext)
library(stringi)
library(fs)

# Canonical column set (pad to these if absent)
canon_cols <- c(
  "SpeechID", "POTUS", "Date", "SpeechTitle", "RawText",
  "SpeechURL", "Summary", "Source", "Type",
  "Original Source", "Location", "CleanText"
)

# Directory 
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

# Function to group and sort
group_by_potus <- function(df, potus_name) {
  df %>%
    filter(POTUS == potus_name) %>%
    arrange(Date)
}

# Create individual datasets
trump_df  <- group_by_potus(all_rows, "Donald Trump")
pence_df  <- group_by_potus(all_rows, "Mike Pence")
harris_df <- group_by_potus(all_rows, "Kamala Harris")
biden_df  <- group_by_potus(all_rows, "Joe Biden")

# Sanity check
summary_table <- all_rows %>%
  count(POTUS, sort = TRUE)

summary_table

# ###
# SUMMARY STATS AND PLOTS
# ###

# "media" wordcount by POTUS

# Use CleanText if available, otherwise RawText
all_rows <- all_rows %>%
  mutate(
    text = coalesce(CleanText, RawText),
    text_lower = str_to_lower(text)
  )

# Define regex for exact word 'media' (word boundaries prevent matches like 'median')
media_pattern <- "\\bmedia\\b"

# Count occurrences of "media" in each speech
media_counts <- all_rows %>%
  mutate(
    media_mentions = str_count(text_lower, regex(media_pattern, ignore_case = TRUE)),
    total_words = str_count(text_lower, boundary("word"))
  ) %>%
  group_by(POTUS) %>%
  summarise(
    n_speeches = n(),
    total_media_mentions = sum(media_mentions, na.rm = TRUE),
    total_words = sum(total_words, na.rm = TRUE),
    media_per_10k_words = 10000 * total_media_mentions / total_words
  ) %>%
  arrange(desc(media_per_10k_words))

print(media_counts)

# ============================================
# Plot: Media mentions per 10k words
# ============================================

ggplot(media_counts, aes(x = reorder(POTUS, media_per_10k_words),
                         y = media_per_10k_words,
                         fill = POTUS)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  coord_flip() +
  labs(
    title = "Mentions of 'media' per 10,000 words in campaign speeches (2020)",
    x = NULL,
    y = "Mentions per 10,000 words"
  ) +
  theme_minimal(base_size = 14)
