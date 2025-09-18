#' Make simple UpSet plots
#'
#' Make simple UpSet plots using ggplot2 and patchwork
#'
#' @param x Input data frame
#' @param sets Character vector listing columns of x to plot
#' @param sort_sets Logical, if TRUE sort sets by size
#' @param sort_intersect Show intersections in decreasing order
#' @param n_intersect Maximum number of intersections to show
#' @param min_size Only show intersections larger than this value
#' @param fill_sets,fill_intersect Additional column name in x to fill either
#' sets or intersections by. Each must be categorical
#' @param annotations list where each element is a list of ggplot2 layers.
#' Each element will be added as an upper annotation panel above the
#' intersections plot. All layer types (geom, scale, aes, stat, labs etc) can be
#' passed with the exception of facets. Elements will be added after an initial,
#' internal call to `ggplot(data, aes(x = intersect))`
#' @param set_labels,intersect_labels One of geom_text() or geom_label() with
#' arguments set as preferred. Use `NULL` to hide set or intersect labels
#' @param intersect_points,intersect_segments,empty_intersect_points
#' Specifications of [geom_point()] and [geom_segment()] which can be used to
#' modify the defaults in the matrix panel
#' @param set_label_fun,intersect_label_fun Functions applied to set labels and
#' intersection labels
#' @param scale_fill_sets,scale_fill_intersect Discrete scales to use when
#' optionally filling bars by an additional column in 'x'
#' @param scale_x_sets,scale_y_intersect Optional scales to pass to these plots.
#' Can be used to over-ride default axis names, labelling and expansions
#' @param width,height Proportional width and height of the intersection panel
#' @param stripe_colours Colours for background stripes in the lower two panels.
#' For no stripes, set as NULL
#' @param vjust_ylab Used to nudge the y-axis labels closer to the axis
#' @param thm_intersect,thm_sets,thm_mat list objects containing
#' additional theme arguments for each panel
#' @param guides Passed to [patchwork::plot_layout()]
#' @param top_left Optional ggplot object to show in the top left panel. Will
#' default to an empty ggplot object
#' @param ... Not used
#' @param na.rm `NA` handling
#' @param set_x_title,intersect_y_title,grid_x_title Axis titles for various
#' panels. Set to NULL to remove titles
#'
#' @return Object of class 'patchwork' containing multiple ggplot panels
#'
#' @import patchwork
#' @import ggplot2
#' @export
simpleUpSet <- function(
    x,
    sets = NULL, sort_sets = TRUE, sort_intersect = TRUE,
    n_intersect = 20, min_size = 0,
    fill_sets = NULL, fill_intersect = NULL,
    annotations = list(),
    set_labels = geom_text(hjust = 1.1, size = 4),
    intersect_labels = geom_text(vjust = -1, size = 4),
    intersect_points, intersect_segments, empty_intersect_points,
    set_label_fun = scales::comma, intersect_label_fun = set_label_fun,
    scale_x_sets, scale_y_intersect, scale_fill_sets, scale_fill_intersect,
    width = 0.75, height = 0.75, vjust_ylab = 0.8,
    stripe_colours = c("grey90", "white"),
    thm_intersect = list(), thm_sets = list(), thm_mat = list(),
    set_x_title = "Set Size", intersect_y_title = "Intersection Size",
    grid_x_title = "Intersection", guides = "keep", top_left = NULL,
    ..., na.rm = TRUE
){


  sets <- .check_sets(x, sets, na.rm)
  stopifnot(all(c(width, height) < 1))

  ## Sets panel
  p_sets <- .plot_sets(
    x, sets, sort_sets, set_labels, scale_x_sets, set_label_fun, fill_sets,
    scale_fill_sets, thm_sets, set_x_title, stripe_colours
  )

  ## Get intersections table
  intersect_tbl <- .add_intersections(
    x, sets, sort_intersect, fill_intersect, na.rm
  )
  intersect_tbl <- dplyr::filter(
    intersect_tbl, as.integer(intersect) <= n_intersect
  )

  ## Intersections panel
  vjust <- max(nchar(sets)) * vjust_ylab ## Place labels closer to y
  p_int <- .plot_intersect(
    intersect_tbl, min_size, fill_intersect, scale_y_intersect,
    intersect_labels, intersect_label_fun, scale_fill_intersect, vjust,
    thm_intersect, intersect_y_title
  )

  ## Intersections matrix
  keep_intersect <- droplevels(p_int@data$intersect)
  intersect_tbl <- dplyr::filter(intersect_tbl, intersect %in% keep_intersect)
  p_mat <- .plot_grid(
    p_int, p_sets, intersect_points, intersect_segments, thm_mat, grid_x_title,
    stripe_colours, empty_intersect_points
  )

  ## Blank plot
  p_null <- ggplot() + theme_void() + theme(margins = margin())
  if (is_ggplot(top_left)) p_null <- top_left

  ## Additional annotation figures
  p_upper <- .add_upper_plots(intersect_tbl, annotations, vjust)
  if (length(p_upper)) {
    p_int <- p_int + theme(plot.margin = margin(0, 5.5, 0, 0))
  }

  wrap_plots(
    c(
      p_null, wrap_plots(c(p_upper, list(p_int)), ncol = 1), p_sets, p_mat
    ),
    ncol= 2
  ) +
    plot_layout(
      axes = "collect", guides = guides,
      widths = c(1 - width, width), heights = c(height, 1 - height)
    )

}

#' @import ggplot2
#' @keywords internal
.add_upper_plots <- function(tbl, annotations, vj){

  if (!length(annotations)) return(list())

  ## Run some checks to see if they are all ggplot layers etc
  .check_gg_layers <- \(x) {
    any(
      is_theme(x), is_scale(x), is_position(x), is_guides(x), is_layer(x),
      is_mapping(x)
    )
  }
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

#' @importFrom dplyr distinct summarise
#' @importFrom rlang !!! syms
#' @importFrom tidyr pivot_longer complete
#' @importFrom methods is
#' @import ggplot2
#' @keywords internal
.plot_grid <- function(
    p_int, p_sets, points_geom, segment_geom, thm, xlab, stripe_colours,
    empty_geom
){

  sets <-levels(p_sets@data$set)
  ## The grid tbl will contain all intersections
  grid_tbl <- distinct(p_int@data, !!!syms(c(sets, "intersect")))
  grid_tbl <- pivot_longer(
    grid_tbl, all_of(sets), names_to = "set", values_to = "in_group"
  )
  grid_tbl <- grid_tbl[grid_tbl[["in_group"]],]
  grid_tbl$set <- factor(grid_tbl$set, levels = sets)
  grid_tbl$set_int <- as.integer(grid_tbl$set)
  ## For the segments, we only need the outer intersections on the grid
  seg_tbl <- summarise(
    grid_tbl, y_max = max(!!sym("set_int")), y_min = min(!!sym("set_int")),
    .by = intersect
  )

  ## Set default geoms for points, empty intersections & segment
  if (missing(points_geom)) points_geom <- geom_point(size = 4, shape = 19)
  stopifnot(
    is(points_geom, "LayerInstance") & is(points_geom$geom, "GeomPoint")
  )

  if (missing(segment_geom)) segment_geom <- geom_segment()
  stopifnot(
    is(segment_geom, "LayerInstance") & is(segment_geom$geom, "GeomSegment")
  )
  if (is.null(segment_geom$mapping)) segment_geom$mapping <- aes()
  segment_geom$mapping[c("y", "yend")] <- list(sym("y_min"), sym("y_max"))
  segment_geom$data <- seg_tbl

  if (missing(empty_geom)) empty_geom <- geom_point(
    size = 4, shape = 19, colour = "grey70", alpha = 0.7
  )
  empty_geom$data <- complete(grid_tbl, intersect, set)
  stopifnot(
    is(empty_geom, "LayerInstance") & is(empty_geom$geom, "GeomPoint")
  )

  ## And the main figure
  stripe_geom <- .bg_stripes(sets, stripe_colours)
  p <- ggplot(grid_tbl, aes(intersect, set)) +
    stripe_geom + empty_geom + segment_geom + points_geom +
    scale_y_discrete(name = NULL) +
    scale_x_discrete(name = xlab, labels = NULL) +
    theme(
      margins = margin(5.5, 5.5, 5.5, 0),
      axis.text.y = element_text(hjust = 0.5),
      axis.ticks = element_blank()
    )
  ## Optional theme modifications
  if (length(thm) & is.list(thm)) {
    valid <- intersect(names(thm), names(formals(theme)))
    p <- p + do.call("theme", thm[valid])
  }
  p
}

#' @importFrom rlang !! sym
#' @importFrom dplyr summarise
#' @importFrom methods is
#' @import ggplot2
#' @keywords internal
.plot_intersect <- function(
    tbl, min_size, fill, y_scale, label_geom, label_fun,
    fill_scale, vj, thm, ylab
){

  ## Setup the default scale for y
  if (missing(y_scale)) y_scale <- scale_y_continuous(
    expand = expansion(c(0, 0.1)), labels = scales::comma
  )
  stopifnot(is(y_scale, "ScaleContinuousPosition"))
  if (is_waiver(y_scale$name)) y_scale$name <- ylab
  ## The deafult fill scale. Will be ignored if no fill value is passed
  if (missing(fill_scale)) fill_scale <- scale_fill_discrete()
  stopifnot(is(fill_scale, "ScaleDiscrete") & fill_scale$aesthetics == "fill")

  ## The totals summarised by intersect (ignoring any fill columns)
  totals_df <- summarise(tbl, n = dplyr::n(), .by = all_of("intersect"))
  totals_df <- dplyr::filter(totals_df, n > min_size)
  totals_df$intersect <- droplevels(totals_df$intersect)
  tbl <- dplyr::filter(tbl, intersect %in% levels(totals_df$intersect))

  ## The mappings for geom_bar
  bar_geom <- geom_bar()
  if (!is.null(fill)) {
    fill <- intersect(fill, colnames(tbl))[[1]]
    stopifnot(length(fill) == 1)
    bar_geom <- geom_bar(aes(fill = !!sym(fill)))
  }
  ## The intial plot
  p <- ggplot(tbl, aes(!!sym("intersect"))) +
    bar_geom + fill_scale + y_scale +
    scale_x_discrete(name = NULL, labels = NULL) +
    theme(
      axis.title.y = element_text(vjust = -vj),
      axis.ticks.x.bottom = element_blank(),
      margins = margin(5.5, 5.5, 0, 0)
    )
  ## Add labels if chosen
  if (!is.null(label_geom)) {

    totals_df$label <- totals_df$n
    if (is.function(label_fun)) totals_df$label <- label_fun(totals_df$n)
    ## This should be a geom_text/label instance set in the initial arguments
    stopifnot(
      is(label_geom, "LayerInstance") &
        is(label_geom$geom) %in% c("GeomLabel", "GeomText")
    )
    label_geom$mapping <- aes(y = !!sym("n"), label = !!sym("label"))
    label_geom$data <- totals_df
    p <- p + label_geom

  }
  ## Optional theme modifications
  if (length(thm) & is.list(thm)) {
    valid <- intersect(names(thm), names(formals(theme)))
    p <- p + do.call("theme", thm[valid])
  }
  p

}

#' @importFrom rlang !! sym
#' @importFrom dplyr across summarise
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @keywords internal
.plot_sets <- function(
    tbl, sets, sort, set_labels, x_scale, lab_fun, fill, fill_scale, thm,
    xlab, stripe_colours
){

  ## Get the set levels
  col_sums <- colSums(tbl[sets])
  set_levels <- sets
  if (sort) set_levels <- names(sort(col_sums))

  ## Form the basic object with sets, but retaining th2 fill column if passed
  if (!is.null(fill)) stopifnot(fill %in% colnames(tbl))
  sets_tbl <- summarise(tbl, across(all_of(sets), sum), .by = all_of(fill))
  sets_tbl <- pivot_longer(
    sets_tbl, all_of(sets), names_to = "set", values_to = "n"
  )
  sets_tbl <- sets_tbl[sets_tbl[["n"]] > 0,]
  sets_tbl$set <- factor(sets_tbl$set, levels = set_levels)
  sets_tbl <- dplyr::arrange(sets_tbl, set)

  ## Aesthetic mappings
  mapping <- aes(x = !!sym("n"), y = !!sym("set"))
  if (!is.null(fill)) mapping$fill <- sym(fill)
  ## The default fill_scale
  if (missing(fill_scale)) fill_scale <- scale_fill_discrete()
  stopifnot(is(fill_scale, "ScaleDiscrete") & fill_scale$aesthetics == "fill")

  x_exp <- 0.1
  if (!is.null(set_labels)) {
    ## Setup the labels for plotting using colSums
    counts_tbl <- data.frame(
      set = factor(names(col_sums), levels = set_levels), n = col_sums
    )
    counts_tbl$label <- counts_tbl$n
    if (is.function(lab_fun)) counts_tbl$label <- lab_fun(counts_tbl$label)

    ## This should be a geom_text/label instance set in the initial arguments
    stopifnot(
      is(set_labels, "LayerInstance") &
        is(set_labels$geom) %in% c("GeomLabel", "GeomText")
    )
    set_labels$mapping <- aes(x = n, y = set, label = label)
    set_labels$data <- counts_tbl
    ## Expand x based on the maximum value. Might go weird if transforming
    x_exp <- x_exp + max(nchar(counts_tbl$label)) * 0.03
  }

  ## Set the default x-axis
  if (missing(x_scale)) x_scale <- scale_x_reverse(
    expand = expansion(c(x_exp, 0)), labels = scales::comma
  )
  stopifnot(is(x_scale, "ScaleContinuousPosition"))
  if (is_waiver(x_scale$name)) x_scale$name <- xlab

  ## The main plot
  stripe_geom <- .bg_stripes(set_levels, stripe_colours)
  p <- ggplot(sets_tbl) +
    stripe_geom + geom_col(mapping) + set_labels + fill_scale +
    scale_y_discrete(position = "right", name = NULL, labels = NULL) +
    x_scale +
    theme(
      axis.text.y.right = element_text(hjust = 0.5),
      axis.ticks.y.right = element_blank(),
      margins = margin(5.5, 0, 5.5, 5.5)
    )

  ## Optionally modify the theme
  if (length(thm) & is.list(thm)) {
    valid <- intersect(names(thm), names(formals(theme)))
    p <- p + do.call("theme", thm[valid])
  }

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
    names(which(!is_lgl)), "are logical or strictly in 0,1"
  )
  ## Check for non-zero values in at least one position
  ## Drop these from the sets going forward
  has_non_zero <- vapply(x[sets], \(x) any(x == 1), logical(1))
  sets[has_non_zero]
}

#' @keywords internal
#' @importFrom dplyr if_any summarise left_join
#' @importFrom tidyselect all_of
.add_intersections <- function(x, sets, sort, fill, na.rm){

  ## This simply adds the intersect number as a new column 'intersect'
  if (!is.null(fill)) {
    ## Ensure only discrete columns can be used
    fill <- intersect(fill, colnames(x))[1]
    if (!is.character(x[[fill]])) x[[fill]] <- as.factor(x[[fill]])
    if (!is.factor(x[[fill]])) stop(
      "Requested fill column must be passed as a character or factor"
    )
  }

  ## Coerce all set columns to be logical & remove rows where all are FALSE
  x[sets] <- lapply(x[sets], as.logical)
  x <- dplyr::filter(x, if_any(all_of(sets)))
  x <- droplevels(x)

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

## Key options to develop
## Highlighting sets or bars using queries & passing colours to them
##   - Perhaps these could be set directly in a column of actual colours?
## Highlighting points/segments on the grid plot



