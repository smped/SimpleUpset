## The system file movies.tsv.gz was produced using the following code
library(tidyverse)
sets <- sets <- c("Action", "Comedy", "Drama", "Thriller", "Romance")
read_delim(system.file("extdata", "movies.csv", package = "UpSetR"), delim = ";") %>%
  dplyr::select(Name, ReleaseDate, all_of(sets), AvgRating, Watches) %>%
  mutate(
    Decade = cut(
      ReleaseDate, breaks = seq(1910, 2000, by = 10),
      labels = paste(seq(1911, 1991, by = 10), seq(1920, 2000, by = 10), sep = "-")
    )
  ) %>%
  arrange(ReleaseDate) %>%
  write_tsv("inst/extdata/movies.tsv.gz")
