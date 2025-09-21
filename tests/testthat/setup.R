library(tidyverse)
theme_set(theme_bw())
movies <- readr::read_delim(system.file("extdata", "movies.csv", package = "UpSetR"), delim = ";") %>%
  mutate(
    Decade = cut(
      ReleaseDate, breaks = seq(1910, 2000, by = 10),
      labels = paste(seq(1911, 1991, by = 10), seq(1920, 2000, by = 10), sep = "-")
    ) %>% fct_rev()
  )
sets <- c("Action", "Comedy", "Drama", "Thriller", "Romance")
