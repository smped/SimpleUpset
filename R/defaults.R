#' Define default layers for individual UpSet components
#'
#' Define and modify default layers for individual UpSet components
#'
#' @details
#' These functions define the default layers for inclusion on UpSet plots.
#' Given the returned object is a simple list, these are easily modifiable using
#' simple list operations.
#' Additional themes, guides, scales etc can be easily added using the dot
#' arguments.
#'
#' The underlying code for simple modification can be obtained using `dry_run = FALSE`
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
#'
#' @return List of ggplot2 elements
#'
#' @examples
#' # View the unevaluated list of default layers for the sets
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
#' @importFrom rlang expr !! sym
#' @export
#' @rdname default-layers
default_set_layers <- function(
    ..., fill = NULL, f = comma, expand = 0.2, name = "Set Size", dry_run = FALSE
){

  bar_geom <- geom_bar()
  if (!is.null(fill)) {
    if (is.null(bar_geom$mapping)) {
      bar_geom$mapping <- aes(fill = !!sym(fill))
    }
  }

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
      bar_geom,
      geom_text(aes(x = n, label = f(n)), hjust = 1.1, size = 3.5),
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
  c(eval(layers), dot_list)

}
#' @import ggplot2
#' @importFrom scales comma
#' @export
#' @rdname default-layers
default_intersect_layers <- function(
    ..., fill = NULL, f = comma, expand = 0.05, name = "Intersection Size",
    dry_run = FALSE
){

  bar_geom <- geom_bar()
  if (!is.null(fill)) {
    if (is.null(bar_geom$mapping)) {
      bar_geom$mapping <- aes(fill = !!sym(fill))
    }
  }

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
      bar_geom,
      geom_text(aes(y = n, label = f(n)), vjust = -0.5, size = 3.5),
      scale_x_discrete(name = NULL, labels = NULL),
      scale_y_continuous(name = name, expand = c(0, 0, expand, 0)),
      theme(
        axis.ticks.x.bottom = element_blank(),
        margins = margin(5.5, 5.5, 0, 0)
      )
    )
  )
  if (dry_run) return(layers)
  c(eval(layers), dot_list)

}
#' @import ggplot2
#' @importFrom scales comma
#' @importFrom rlang !! sym
#' @export
#' @rdname default-layers
default_grid_layers <- function(
    ..., colour = NULL, fill = NULL, light = "grey80", dark = "grey23", dry_run = FALSE
){

  points_geom <- geom_point(size = 4, shape = 19, colour = dark)
  segment_geom <- geom_segment(
    aes(y = !!sym("y_min"), yend = !!sym("y_max")), colour = dark
  )
  if (!is.null(colour)) {

    if (is.null(points_geom$mapping)) points_geom$mapping <- aes()
    points_geom$aes_params$colour <- NULL
    points_geom$mapping$colour <- sym(colour)

    if (is.null(segment_geom$mapping)) segment_geom$mapping <- aes()
    segment_geom$aes_params$colour <- NULL
    segment_geom$mapping$colour <- sym(colour)

  }
  if (!is.null(fill)) {
    if (is.null(points_geom$mapping)) points_geom$mapping <- aes()
    points_geom$mapping$fill <- sym(fill)
  }

  ## Check all dotargs for validity
  dot_list <- list(...)
  valid_gg <- vapply(dot_list, .check_gg_layers, logical(1))
  dot_list <- dot_list[valid_gg]

  layers <- expr(
    list(
      aes(x = intersect, y = set),
      points_geom,
      geom_point(size = 4, shape = 19, colour = light), # Empty intersections
      segment_geom,
      scale_y_discrete(name = NULL),
      scale_x_discrete(name = NULL, labels = NULL),
      guides(colour = guide_none()),
      theme(
        margins = margin(5.5, 5.5, 5.5, 0),
        axis.text.y = element_text(hjust = 0.5),
        axis.ticks = element_blank()
      )
    )
  )
  if (dry_run) return(layers)
  c(eval(layers), dot_list)

}
