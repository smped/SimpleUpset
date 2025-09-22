library(tidyverse)
library(patchwork)
theme_set(theme_bw())
movies <- readr::read_delim(system.file("extdata", "movies.csv", package = "UpSetR"), delim = ";") %>%
  mutate(
    Decade = cut(
      ReleaseDate, breaks = seq(1910, 2000, by = 10),
      labels = paste(seq(1911, 1991, by = 10), seq(1920, 2000, by = 10), sep = "-")
    ) %>% fct_rev()
  )
sets <- c("Action", "Comedy", "Drama", "Thriller", "Romance")

## Basic
simpleUpSet(movies, sets)

## Fill with fewer intersects
simpleUpSet(
  movies, sets, min_size = 20,
  intersect_layers = default_intersect_layers(
    fill = "Decade",
    scale_fill_brewer(palette = "Paired"),
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.99, 0.99),
      legend.justification.inside = c(1, 1)
    )
  ),
  set_layers = default_set_layers(
    fill = "Decade", scale_fill_brewer(palette = "Paired"),
    guides(fill = guide_none())
  )
)

## Add a simple boxplot
simpleUpSet(
  movies, sets, n_intersect = 10,
  annotations = list(geom_boxplot(aes(y = AvgRating))),
)

## Add a detailed upper plot
simpleUpSet(
  movies, sets, n_intersect = 10,
  annotations = list(
    list(
      aes(y = AvgRating),
      geom_jitter(aes(colour = Decade), height = 0, width = 0.3, alpha = 0.5),
      geom_violin(fill = NA, quantiles = 0.5, quantile.linetype = 1),
      scale_colour_brewer(palette = "Paired"),
      guides(colour = guide_legend(nrow = 1, reverse = TRUE))
    )
  ), guides = "collect"
) &
  theme(legend.position = "bottom")


## Test Highlighting
set_cols <- c(Action = "red", Comedy = "grey23", Drama = "red", Romance = "grey23", Thriller = "grey23")
simpleUpSet(
  movies, sets, min_size = 20,
  set_layers = default_set_layers(fill = "set", scale_fill_manual(values = set_cols)),
  intersect_layers = default_intersect_layers(fill = "highlight", scale_fill_manual(values = "red", na.value = "grey23")),
  grid_layers = default_grid_layers(colour = "highlight", scale_colour_manual(values = "red", na.value = "grey23")),
  highlight = case_when(Action & Drama ~ TRUE)

) &
  plot_annotation(title = "Using Highlights") &
  theme(legend.position = "none", plot.title = element_text(hjust = 2/3))
