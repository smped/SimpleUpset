#' Define default layers for individual UpSet components
#'
#' Define and modify default layers for individual UpSet components
#'
#' @details
#' These functions define the default layers for inclusion on UpSet plots.
#'
#' The returned object is a list with a series of layers, scales, themes etc
#' which represent the default plotting layers for each of the sets,
#' intersections and intersections matrix (grid) panels.
#'
#' A series of common arguments have been defined to enable common modifications
#' without recreating the list from scratch. These include modifying the mapping
#' to fill, axis expansion to better accommodate labels, labelling functions for
#' set/intersection sizes, and axis titles.
#'
#' Additional layers, such as scale_fill_* elements, guides or themes, can be
#' simply included by passing to the function, without any requirement for
#' naming, and are handled by the ellipsis.
#'
#' The entire command used to create default layers can be shown by calling each
#' function using the argument `dry_run = TRUE`.
#' This can be helpful for creating custom layers, by starting with then
#' modifying the defaults.
#'
#' The returned object is a simple list, and are easily modifiable using
#' simple list operations.
#'
#' @param fill Column to fill set bars by. Can be 'set' or another column within
#' the main data object
#' @param colour Primarily used for highlighting points and segments in the
#' intersections matrix
#' @param f Function for labelling set sizes and the set x-axis
#' @param expand axis expansion
#' @param name Main axis title
#' @param ... additional layers to include alongside default layers.
#' Will be added after the default layers
#' @param dry_run Set as TRUE to view the unevaluated layers which are defined
#' as the defaults. Additional layers passed through the ellipsis will not be
#' included as part of a dry_run
#' @param light,dark default colours for empty intersections (light) and for
#' both non-empty intersections and segments (dark)
#' @param shape Point shape passed to the intersections matrix
#' @param size Passed to labels for sets and intersections, and to point sizes
#' for the intersections matrix
#' @param hjust,vjust Passed to respective elements for simple adjustment of
#' either set or intersection sizes
#'
#' @return List of ggplot2 elements
#'
#' @examples
#' # View the un-evaluated list of default layers for the sets
#' default_set_layers(dry_run = TRUE)
#'
#' # Create set layers colouring by set name, and hiding the legend
#' set_list <- default_set_layers(
#'   fill = "set", scale_fill_brewer(palette = "Set1"), guides(fill = guide_none())
#' )
#' sapply(set_list, is)
#'
#'
#' @import ggplot2
#' @importFrom scales comma
#' @importFrom rlang expr sym
#' @export
#' @rdname default-layers
default_set_layers <- function(
    ..., fill = NULL, f = comma, expand = 0.2, hjust = 1.1, size = 3.5,
    name = "Set Size", dry_run = FALSE
){

  bar_aes <- aes()
  if (!is.null(fill)) bar_aes$fill <- sym(fill)

  ## Check the labeller
  if (is.null(f)) f <- c
  stopifnot(is(f, "function"))

  ## Check all dotargs for validity
  dot_list <- list(...)
  valid_gg <- vapply(dot_list, .check_gg_layers, logical(1))
  dot_list <- dot_list[valid_gg]

  layers <- expr(
    list(
      aes(y = set),
      geom_bar(bar_aes),
      geom_text(aes(x = n, label = f(n)), hjust = hjust, size = size),
      scale_x_reverse(expand = c(expand, 0, 0, 0), name = name, labels = f),
      scale_y_discrete(position = "right", name = NULL, labels = NULL),
      theme(
        axis.text.y.right = element_text(hjust = 0.5),
        axis.ticks.y.right = element_blank(),
        margins = margin(5.5, 0, 5.5, 5.5)
      )
    )
  )
  if (dry_run) return(layers)
  out <- c(eval(layers), dot_list)
  class(out) <- "default_layers"
  out

}
#' @import ggplot2
#' @importFrom scales comma
#' @importFrom rlang sym
#' @export
#' @rdname default-layers
default_intersect_layers <- function(
    ..., fill = NULL, f = comma, expand = 0.05, vjust = -0.5, size = 3.5,
    name = "Intersection Size", dry_run = FALSE
){

  bar_aes <- aes()
  if (!is.null(fill)) bar_aes$fill <- sym(fill)

  ## Check the labeller
  if (is.null(f)) f <- c
  stopifnot(is(f, "function"))

  ## Check all dotargs for validity
  dot_list <- list(...)
  valid_gg <- vapply(dot_list, .check_gg_layers, logical(1))
  dot_list <- dot_list[valid_gg]

  layers <- expr(
    list(
      aes(x = intersect),
      geom_bar(bar_aes),
      geom_text(aes(y = n, label = f(n)), vjust = vjust, size = size),
      scale_x_discrete(name = NULL, labels = NULL),
      scale_y_continuous(name = name, expand = c(0, 0, expand, 0), labels = f),
      theme(
        axis.ticks.x.bottom = element_blank(),
        margins = margin(5.5, 5.5, 0, 0)
      )
    )
  )
  if (dry_run) return(layers)
  out <- c(eval(layers), dot_list)
  class(out) <- "default_layers"
  out

}
#' @import ggplot2
#' @importFrom scales comma
#' @importFrom rlang !! sym
#' @export
#' @rdname default-layers
default_grid_layers <- function(
    ..., colour = NULL, fill = NULL, light = "grey80", dark = "grey23",
    shape = 19, size = 4, name = NULL, dry_run = FALSE
){

  ## Set aesthetics manually before producing complete layers
  points_aes <- aes()
  segment_aes <- aes(y = !!sym("y_min"), yend = !!sym("y_max"))
  if (!is.null(colour)) {
    points_aes$colour <- sym(colour)
    segment_aes$colour <- sym(colour)
  }
  if (!is.null(fill)) points_aes$fill <- sym(fill)

  ## Check all dotargs for validity
  dot_list <- list(...)
  valid_gg <- vapply(dot_list, .check_gg_layers, logical(1))
  dot_list <- dot_list[valid_gg]

  layers <- expr(
    list(
      aes(x = intersect, y = set),
      if (!is.null(colour)) {
        geom_point(mapping = points_aes, size = size, shape = shape)
      } else {
        geom_point(mapping = points_aes, size = size, shape = shape, colour = dark)
      },
      geom_point(size = size, shape = shape, colour = light), # Empty intersections
      if (!is.null(colour)) geom_segment(segment_aes) else geom_segment(segment_aes, colour = dark),
      scale_y_discrete(name = NULL),
      scale_x_discrete(name = name, labels = NULL),
      guides(colour = guide_none()),
      theme(
        margins = margin(5.5, 5.5, 5.5, 0),
        axis.text.y = element_text(hjust = 0.5),
        axis.ticks = element_blank()
      )
    )
  )
  if (dry_run) return(layers)
  out <- c(eval(layers), dot_list)
  class(out) <- "default_layers"
  out

}
