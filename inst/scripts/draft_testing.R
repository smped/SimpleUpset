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
simpleUpSet(movies, sets, comma = FALSE)

## Fill with fewer intersects
simpleUpSet(
  movies, sets, min_size = 20,
  geom_intersect = geom_bar(aes(fill = Decade)),
  geom_sets = geom_bar(aes(fill = Decade)),
  scale_fill_intersect = scale_fill_brewer(palette = "Paired"),
  scale_fill_sets = scale_fill_brewer(palette = "Paired"),
  theme_intersect = theme(
    legend.position = "inside",
    legend.position.inside = c(0.99, 0.99),
    legend.justification.inside = c(1, 1)
  ),
  theme_sets = theme(legend.position = "none")
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

## Modify the grid
simpleUpSet(
  movies, sets,
  grid_points = geom_point(shape = 15, size = 5, colour = "navyblue"),
  empty_grid_points = geom_point(shape = 15, size = 5, colour = "navyblue", alpha = 0.2),
  grid_segments = geom_segment(colour = "navyblue")
)

## Test Highlighting
set_cols <- c(Action = "red", Comedy = "grey30", Drama = "red", Romance = "grey30", Thriller = "grey30")
simpleUpSet(
  movies, sets, min_size = 20,
  geom_sets = geom_bar(aes(fill = set)),
  geom_intersect = geom_bar(aes(fill = highlight)),
  highlight = case_when(Action & Drama ~ TRUE),
  scale_fill_sets = scale_fill_manual(values = set_cols),
  scale_fill_intersect = scale_fill_manual(values = "red", na.value = "grey30"),
  scale_grid_fill = scale_fill_manual(values = "red", na.value = "grey30"),
  grid_points = geom_point(aes(fill = highlight), size = 4, shape = 21),
) &
  plot_annotation(title = "Using Highlights") &
  theme(legend.position = "none", plot.title = element_text(hjust = 2/3))
