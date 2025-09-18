library(tidyverse)
library(patchwork)
theme_set(theme_bw())
movies <- readr::read_delim(system.file("extdata", "movies.csv", package = "UpSetR"), delim = ";") %>%
  # dplyr::slice(1:500) %>%
  dplyr::filter(ReleaseDate >= 1990) %>%
  mutate(ReleaseDate = as.factor(ReleaseDate))
## Basic
simpleUpSet(
  movies, sets = c("Action", "Adventure", "Comedy", "Crime", "Drama"),
)
## Fill with fewer intersects
simpleUpSet(
  movies, sets = c("Action", "Adventure", "Comedy", "Crime", "Drama"),
  n_intersect = 10, fill_intersect = "ReleaseDate"
)
## Add the boxplot
simpleUpSet(
  movies, sets = c("Action", "Adventure", "Comedy", "Crime", "Drama"),
  n_intersect = 10, fill_intersect = "ReleaseDate",
  annotations = list(list(aes(y = AvgRating), geom_boxplot()))
)
