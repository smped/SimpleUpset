library(tidyverse)
theme_set(theme_bw())
sets <- c("Action", "Comedy", "Drama", "Thriller", "Romance")
movies <- system.file("extdata", "movies.tsv.gz", package = "SimpleUpset") %>%
  read_tsv() %>%
  mutate(
    Decade = fct_inorder(Decade) %>% fct_rev()
  )
