#' Make simple UpSet plots
#'
#' Make simple UpSet plots using ggplot2 and patchwork
#'
#' @details
#' Taking a subset of columns from a data.frame, create an UpSet plot showing
#' all intersections as specified.
#' Columns chosen for the sets and intersections must contain logical values
#' or be strictly 0/1 values.
#'
#' Additional columns can be used where appropriate for creating boxplots etc.
#' Internally, data objects will have the variables `set` and `intersect` which
#' can be referred to when passing custom aes() mappings to various layers.
#' Any additional layers passed using `annotations()` will have layers added
#' after an initial, internal call to `ggplot(data, aes(x = intersect))`.
#'
#'
#' @param x Input data frame
#' @param sets Character vector listing columns of x to plot
#' @param sort_sets Logical, if TRUE sort sets by size
#' @param sort_intersect Show intersections in decreasing order
#' @param n_intersect Maximum number of intersections to show
#' @param min_size Only show intersections larger than this value
#' @param set_layers List of `ggplot2` layers, scales and themes to define the
#' appearance of the sets panel. Can be obtained and extended using
#' [default_set_layers()]
#' @param intersect_layers List of `ggplot2` layers, scales and themes to define
#' the appearance of the intersections panel. Can be obtained and extended
#' using [default_intersect_layers()]
#' @param grid_layers List of `ggplot2` layers, scales & themes
#' @param annotations list where each element is a list of ggplot2 layers.
#' Each element will be added as an upper annotation panel above the
#' intersections plot. All layer types (geom, scale, aes, stat, labs etc) can be
#' passed with the exception of facets.
#' @param highlight [case_when()] statement defining all intersections to
#' highlight using `geom_intersect` and `scale_fill/colour_intersect`.
#' Will add a column named `highlight` which can be called from any geom
#' @param width,height Proportional width and height of the intersection panel
#' @param stripe_colours Colours for background stripes in the lower two panels.
#' For no stripes, set as NULL
#' @param vjust_ylab Used to nudge the y-axis labels closer to the axis
#' @param guides Passed to [plot_layout()]
#' @param top_left Optional ggplot object to show in the top left panel. Will
#' default to an empty ggplot object
#' @param ... Not used
#' @param na.rm `NA` handling
#'
#' @return Object of class 'patchwork' containing multiple ggplot panels
#'
#' @examples
#' ## Use the movies data from the package UpSetR
#' library(tidyverse)
#' theme_set(theme_bw())
#' f <- system.file("extdata", "movies.csv", package = "UpSetR")
#' movies <- read_delim(f, delim = ";") %>%
#'  mutate(
#'    ## Break the Release dates into decades
#'    Decade = cut(
#'      ReleaseDate, breaks = seq(1910, 2000, by = 10),
#'      labels = paste(seq(1911, 1991, by = 10), seq(1920, 2000, by = 10), sep = "-")
#'    ) %>% fct_rev()
#'  )
#'  sets <- c("Action", "Comedy", "Drama", "Thriller", "Romance")
#' ## A basic UpSet plot
#' simpleUpSet(movies, sets)
#'
#' ## Fill intersects & sets by decade, showing fewer intersects
#' simpleUpSet(
#'   movies, sets, min_size = 20,
#'   geom_intersect = geom_bar(aes(fill = Decade)),
#'   geom_sets = geom_bar(aes(fill = Decade)),
#'   scale_fill_intersect = scale_fill_brewer(palette = "Paired"),
#'   scale_fill_sets = scale_fill_brewer(palette = "Paired"),
#'   theme_intersect = theme(
#'     legend.position = "inside",
#'     legend.position.inside = c(0.99, 0.99),
#'     legend.justification.inside = c(1, 1)
#'   ),
#'  theme_sets = theme(legend.position = "none")
#' )
#'
#' ## Add a simple boxplot as an additional upper panel
#' simpleUpSet(
#'   movies, sets, n_intersect = 10,
#'   annotations = list(geom_boxplot(aes(y = AvgRating))),
#' )
#'
#' ## Add a far more detailed upper plot
#' simpleUpSet(
#'   movies, sets, n_intersect = 10,
#'   annotations = list(
#'     list(
#'       aes(y = AvgRating),
#'       geom_jitter(aes(colour = Decade), height = 0, width = 0.3, alpha = 0.5),
#'       geom_violin(fill = NA, quantiles = 0.5, quantile.linetype = 1),
#'       scale_colour_brewer(palette = "Paired"),
#'       guides(colour = guide_legend(nrow = 2, reverse = TRUE))
#'     )
#'   ), guides = "collect"
#' ) &
#'   theme(legend.position = "bottom")
#'
#' ## Modify set colours
#' set_cols <- c("darkred", "forestgreen", "blue", "blue", "blue")
#' names(set_cols) <- sets
#' simpleUpSet(
#'   movies, sets,
#'   geom_sets = geom_bar(aes(fill = set)),
#'   scale_fill_sets = scale_fill_manual(values = set_cols),
#'   theme_sets = theme(legend.position = "none")
#' )
#'
#' @import patchwork
#' @import ggplot2
#' @importFrom rlang enquo
#' @export
simpleUpSet <- function(
    x,
    sets = NULL, sort_sets = TRUE, sort_intersect = TRUE,
    n_intersect = 20, min_size = 0,
    set_layers = default_set_layers(),
    intersect_layers = default_intersect_layers(),
    grid_layers = default_grid_layers(),
    highlight = NULL,
    annotations = list(),
    width = 0.75, height = 0.75, vjust_ylab = 0.8,
    stripe_colours = c("grey90", "white"),
    guides = "keep", top_left = NULL, ..., na.rm = TRUE
){


  sets <- .check_sets(x, sets, na.rm)
  stopifnot(all(c(width, height) < 1))

  ## Get intersections table
  intersect_tbl <- .add_intersections(
    x, sets, sort_intersect, na.rm, enquo(highlight)
  )

  ## Sets panel
  p_sets <- .plot_sets(
    intersect_tbl, sets, sort_sets, set_layers, stripe_colours
  )

  ## Intersections panel
  intersect_tbl <- subset(intersect_tbl, as.integer(intersect) <= n_intersect)
  vjust <- max(nchar(sets)) * vjust_ylab ## Place labels closer to y
  p_int <- .plot_intersect(intersect_tbl, min_size, intersect_layers, vjust)

  ## Intersections matrix
  p_mat <- .plot_grid(p_int, p_sets, grid_layers, stripe_colours)

  ## Blank plot
  p_null <- ggplot() + theme_void() + theme(margins = margin())
  if (is_ggplot(top_left)) p_null <- top_left

  ## Additional annotation figures
  keep_intersect <- droplevels(p_int@data$intersect)
  intersect_tbl <- dplyr::filter(intersect_tbl, intersect %in% keep_intersect)
  p_upper <- .add_upper_plots(intersect_tbl, annotations, vjust)
  if (length(p_upper)) {
    p_int <- p_int + theme(plot.margin = margin(0, 5.5, 0, 0))
    p_int <- wrap_plots(c(p_upper, list(p_int)), ncol = 1)
  }

  wrap_plots(c(p_null, p_int, p_sets, p_mat), ncol= 2) +
    plot_layout(
      axes = "collect", guides = guides,
      widths = c(1 - width, width), heights = c(height, 1 - height)
    )

}

#' @import ggplot2
#' @keywords internal
.add_upper_plots <- function(tbl, annotations, vj){

  if (!length(annotations)) return(list())

  ## I can imagine some people wanting a single panel passing a list of layers
  ## Format these into a list
  if (!all(vapply(annotations, is.list, logical(1)))) {
    if (all(vapply(annotations, .check_gg_layers, logical(1)))){
      annotations <- list(annotations)
    }
  }
  ## Check we only have ggplot elements here
  valid <- vapply(
    annotations, \(x) all(vapply(x, .check_gg_layers, logical(1))), logical(1)
  )
  if (!all(valid))
    stop("All elements of annotations must be a list of ggplot2 layers etc")

  ## Make a list for panels for wrapping
  lapply(
    annotations, \(x) {
      p <- ggplot(tbl, aes(x = intersect))
      for (i in seq_along(x)) p <- p + x[[i]] ## Add layers sequentially
      p + theme(
        axis.ticks.x.bottom = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(vjust = -vj),
        margins = margin(0, 5.5, 5.5, 0)
      )
    }
  )
}

#' @importFrom dplyr distinct summarise anti_join join_by
#' @importFrom rlang !!! syms
#' @importFrom tidyr pivot_longer complete
#' @importFrom tidyselect any_of
#' @importFrom methods is
#' @import ggplot2
#' @keywords internal
#' .plot_grid(p_int, p_sets, grid_points, grid_layers, stripe_colours)
.plot_grid <- function(p_int, p_sets, layers, stripe_colours){

  ## Check the layers
  if (!is(layers, "default_layers")) {
    is_gg <- vapply(layers, .check_gg_layers, logical(1))
    stopifnot(all(is_gg))
  }

  sets <-levels(p_sets@data$set)
  ## The grid tbl will contain all intersections
  df <- p_int@data
  groups <- intersect(c(sets, "intersect", "highlight"), colnames(df))
  grid_tbl <- distinct(df, !!!syms(groups))
  grid_tbl <- pivot_longer(
    grid_tbl, all_of(sets), names_to = "set", values_to = "in_group"
  )
  grid_tbl <- grid_tbl[grid_tbl[["in_group"]],]
  grid_tbl$set <- factor(grid_tbl$set, levels = sets)
  grid_tbl$set_int <- as.integer(grid_tbl$set)
  grid_tbl <- droplevels(grid_tbl)

  ## For the segments, we only need the outer intersections on the grid
  seg_tbl <- summarise(
    grid_tbl,
    y_max = max(!!sym("set_int")), y_min = min(!!sym("set_int")),
    .by = any_of(c("intersect", "highlight"))
  )

  ## These layers are more challenging to wrangle using defaults. However
  ## assume if any are GeomSegment the should have data = seg_tbl
  is_segments <- vapply(layers, \(x){
    if (!is(x, "LayerInstance")) return(FALSE)
    is (x$geom, "GeomSegment")
  }, logical(1))
  for (i in which(is_segments)) layers[[i]]$data <- seg_tbl

  is_points <- vapply(layers, \(x){
    if (!is(x, "LayerInstance")) return(FALSE)
    is (x$geom, "GeomPoint")
  }, logical(1))
  if (sum(is_points) > 1) {
    empty_tbl <- complete(grid_tbl, intersect, set)
    empty_tbl <- empty_tbl[is.na(empty_tbl[["in_group"]]),]
    for (i in which(is_points)[-1]) {
      layers[[i]]$data <- empty_tbl
    }
  }

  ## And the main figure
  stripe_geom <- .bg_stripes(sets, stripe_colours)
  p <- ggplot(grid_tbl) + stripe_geom
  for (i in seq_along(layers)) p <- p + layers[[i]]
  p

}

#' @importFrom rlang !! sym
#' @importFrom dplyr summarise
#' @importFrom methods is
#' @import ggplot2
#' @keywords internal
.plot_intersect <- function(tbl, min_size, layers, vj){

  if (!is(layers, "default_layers")) {
    is_gg <- vapply(layers, .check_gg_layers, logical(1))
    stopifnot(all(is_gg))
  }

  ## The totals summarised by intersect (ignoring any fill columns)
  totals_df <- summarise(tbl, n = dplyr::n(), .by = all_of("intersect"))
  totals_df <- dplyr::filter(totals_df, n > min_size)
  totals_df$intersect <- droplevels(totals_df$intersect)
  tbl <- dplyr::filter(tbl, intersect %in% levels(totals_df$intersect))

  ## Check for labels
  is_labels <- vapply(
    layers, \(x) {
      if (!is(x, "LayerInstance")) return(FALSE)
      is(x$geom, "GeomLabel") | is(x$geom, "GeomText")
    }, logical(1)
  )
  for (i in which(is_labels)) layers[[i]]$data <- totals_df

  ## Add the vjust as a new theme
  n_layers <- length(layers)
  layers[[n_layers + 1]] <- theme(axis.title.y = element_text(vjust = -vj))

  ## The intial plot
  p <- ggplot(tbl)
  for (i in seq_along(layers)) p <- p + layers[[i]]
  p

}

#' @importFrom dplyr across summarise
#' @importFrom tidyr pivot_longer
#' @importFrom methods is
#' @import ggplot2
#' @keywords internal
.plot_sets <- function(tbl, sets, sort, layers, stripe_colours){

  ## Check the layers
  if (!is(layers, "default_layers")) {
    is_gg <- vapply(layers, .check_gg_layers, logical(1))
    stopifnot(all(is_gg))
  }

  ## Get the set levels
  col_sums <- colSums(tbl[sets])
  set_levels <- sets
  stopifnot(is.logical(sort))
  if (sort[[1]]) set_levels <- names(sort(col_sums))

  ## Sets will contain logical values here
  sets_tbl <- pivot_longer(tbl, all_of(sets), names_to = "set")
  sets_tbl <- sets_tbl[sets_tbl$value,]
  sets_tbl$set <- factor(sets_tbl$set, set_levels)

  ## Calculate set totals for optional labels
  counts_tbl <- data.frame(
    set = factor(names(col_sums), levels = set_levels), n = col_sums
  )
  ## Check for labels
  is_labels <- vapply(
    layers, \(x) {
      if (!is(x, "LayerInstance")) return(FALSE)
      is(x$geom, "GeomLabel") | is(x$geom, "GeomText")
    }, logical(1)
  )

  for (i in which(is_labels)) layers[[i]]$data <- counts_tbl

  ## The main plot
  stripe_geom <- .bg_stripes(set_levels, stripe_colours)
  p <- ggplot(sets_tbl) + stripe_geom
  for (i in seq_along(layers)) p <- p + layers[[i]]
  p

}


#' @importFrom dplyr summarise
#' @importFrom tidyselect all_of
#' @keywords internal
.check_sets <- function(x, sets, na.rm){

  ## Get intersections
  if (is.null(sets)) sets <- colnames(x)
  stopifnot(all(sets %in% colnames(x)))

  ## Error if character/factors
  col_types <- vapply(sets, \(i) typeof(x[[i]]), character(1))
  stopifnot(all(!col_types %in% c("character", "factor")))
  ## From here we can assume compatible columns

  ## Check each column only has 0,1 entries
  is_lgl <- vapply(x[sets], \(x) all(x %in% c(0, 1, NA)), logical(1))
  if (!all(is_lgl)) stop(
    "Column(s) ", names(which(!is_lgl)), " not logical or strictly in 0,1"
  )
  ## Check for non-zero values in at least one position
  ## Drop these from the sets going forward
  has_non_zero <- vapply(x[sets], \(x) any(x == 1), logical(1))
  sets[has_non_zero]
}

#' @keywords internal
#' @importFrom dplyr if_any summarise left_join mutate
#' @importFrom rlang quo_is_missing !!
#' @importFrom tidyselect all_of
.add_intersections <- function(x, sets, sort, na.rm, hl){


  ## Coerce all set columns to be logical & remove rows where all are FALSE
  x[sets] <- lapply(x[sets], as.logical)
  x <- dplyr::filter(x, if_any(all_of(sets)))
  x <- droplevels(x)
  ## Add highlights if supplied
  if (!quo_is_missing(hl)) x <- mutate(x, highlight = !!hl)

  ## Determine the intersect number to a column in the original
  set_tbl <- summarise(x, n = dplyr::n(), .by = c(all_of(sets)))
  if (sort) set_tbl <- set_tbl[order(set_tbl$n, decreasing = TRUE),]
  set_tbl$intersect <- as.factor(seq_len(nrow(set_tbl)))
  set_tbl$n <- NULL

  ## Return the original table with intersect numbers & logical columns
  left_join(x, set_tbl, by = sets)

}

#' @keywords internal
#' @import ggplot2
.bg_stripes <- function(sets, stripe_colours){
  if (is.null(stripe_colours)) return(NULL)
  stripe_tbl <- data.frame(
    set = factor(sets, levels = sets),
    xmin = -Inf, xmax = Inf,
    col = rep_len(stripe_colours, length(sets))
  )
  geom_rect(
    mapping = aes(xmin = !!sym("xmin"), xmax = !!sym("xmax"), y = set),
    data = stripe_tbl, height = 1, fill = stripe_tbl$col, inherit.aes = FALSE
  )
}

#' @keywords internal
#' @import ggplot2
.check_gg_layers <- \(x) {
  any(
    is_theme(x), is_scale(x), is_position(x), is_guides(x), is_layer(x),
    is_mapping(x), is_coord(x), is_stat(x), is_facet(x)
  )
}

## Key options to develop
## Highlighting sets or bars using queries & passing colours to them
##   - Perhaps these could be set directly in a column of actual colours?
## Highlighting points/segments on the grid plot



