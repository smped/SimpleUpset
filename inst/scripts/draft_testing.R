library(tidyverse)
library(patchwork)
theme_set(theme_bw())
movies <- readr::read_delim(system.file("extdata", "movies.csv", package = "UpSetR"), delim = ";") %>%
  # dplyr::slice(1:500) %>%
  dplyr::filter(ReleaseDate >= 1990) %>%
  mutate(ReleaseDate = as.factor(ReleaseDate))
simpleUpSet(
  movies, sets = c("Action", "Adventure", "Comedy", "Crime", "Drama"),
  # n_intersect = 10, fill_intersect = "ReleaseDate",
  # annotations = list(list(aes(y = AvgRating), geom_boxplot())),
  # upper_var = "AvgRating", upper_geom = "boxplot", upper_args = list(fill = "white"),
  # guides = "collect", mat_args = list(size = 5, shape = 21, fill = "red")
)
