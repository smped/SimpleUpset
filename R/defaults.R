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
#' Each list of default layers is described clearly below.
#' If passing additional scales, themes, layers or guides using the ellipsis,
#' these additional elements will automatically be placed after the defaults.
#' Importantly, these will be created as lists, then can be re-ordered using
#' standard list manipulation.
#'
#' ## Default Layers For Sets
#'
#' | **ggplot2 element** | **Comment** |
#' | :------------------ | :---------- |
#' | `aes(y = set)` | Sets are placed on the y-axis |
#' | `geom_bar(bar_aes)` | If `fill = NULL`, `bar_aes` is the call to `aes()` otherwise it is `aes(fill = !!sym(fill))`. This object is created internally by `default_set_layers()` |
#' | `geom_text(aes(x = size, label = f(size)), hjust = hjust, size = label_size)` | Adds set totals using the labelling function provided as `f`. A data.frame is created when calling `simpleUpSet` which summarises set totals as the column `size` |
#' | `scale_x_reverse(expand = c(expand, 0, 0, 0), name = name, labels = f)` | Ensures bars go in reverse order along the x-axis, with the expansion providing room for set totals |
#' | `scale_y_discrete(position = "right", name = NULL, labels = NULL)` | Tidies up the y-axis with set names |
#' | `theme(axis.text.y.right = element_text(hjust = 0.5), axis.ticks.y.right = element_blank(), margins = margin(5.5, 0, 5.5, 5.5))` | Ensures margins and tick marks are suitable for the UpSet layout |
#'
#' ## Default Layers For Intersections
#'
#' | **ggplot2 element** | **Comment** |
#' | :------------------ | :---------- |
#' | `aes(x = intersect)` | Intersections are placed along the x-axis |
#' | `geom_bar(bar_aes)` | If `fill = NULL`, `bar_aes` is the call to `aes()` otherwise it is `aes(fill = !!sym(fill))`. This object is created internally by `default_intersect_layers()` |
#' | `geom_text(aes(y = size, label = f(size)), vjust = vjust, size = label_size)` | Adds intersection totals using the labelling function provided as `f`. A data.frame is created when calling `simpleUpSet` which summarises intersection totals as the column `size` |
#' | `scale_x_discrete(name = NULL, labels = NULL)` | Tidies up the x-axis, hiding intersection names |
#' | `scale_y_continuous(name = name, expand = c(0, 0, expand, 0), labels = f)` | Standard y-axis with name provided and with the expansion able to be easily set to accommodate labels |
#' | `theme(axis.ticks.x.bottom = element_blank(), margins = margin(5.5, 5.5, 0, 0))` | Ensures margins and tick marks are suitable for the UpSet layout |
#'
#' ## Default Layers For Intersections Matrix (i.e. Grid)
#'
#' | **ggplot2 element** | **Comment** |
#' | :------------------ | :---------- |
#' | `aes(x = intersect, y = set)` | Defines the grid layout to match the sets and intersections panels |
#' | `if (!is.null(colour)) geom_point(mapping = points_aes, size = size, shape = shape) else geom_point(mapping = points_aes, size = size, shape = shape, colour = dark)` | These points represent the main intersections. In general, colour/fill will only be required if highlighting points and the constant value is removed using an `ifelse` statement is colour is set as a mapping aesthetic |
#' | `geom_point(size = size, shape = shape, colour = light)` | These points represent the background or empty intersections |
#' | `if (!is.null(colour)) geom_segment(segment_aes) else geom_segment(segment_aes, colour = dark)` | The segments joining intersections, again removing colour if set as an aesthetic mapping |
#' | `scale_y_discrete(name = NULL)` | Ensures the y-axis matches that of the sets panel |
#' | `scale_x_discrete(name = name, labels = NULL)` | Ensures the x-axis matches that of the intersection panel, with an optional name |
#' | `guides(colour = guide_none())` | By default, don't include a legend for any optionally coloured points |
#' | `theme(margins = margin(5.5, 5.5, 5.5, 0), axis.text.y = element_text(hjust = 0.5), axis.ticks = element_blank())` | Ensures margins, text and tick marks are suitable for the UpSet layout |
#'
#' ## Panel Internals
#'
#' Internally, the supplied data.frame has the additional columns 'intersect',
#' 'degree' added, along with the optional 'highlight' column.
#' This object is used to directly create bars using `geom_bar()` and as such,
#' any of the additional columns can be passed to `geom_bar()` as mapping
#' aesthetics, along with all original columns.
#'
#' For both the sets and intersection totals (i.e. labels), separate tables are
#' created specifically for printing totals at the top (or left) of each bar,
#' and these tables are specifically passed to those layers.
#' Totals are included as 'size' and the proportion of all intersections is
#' also included as the column 'prop' for both the sets and intersections panel.
#' Whilst default labels are added using 'size', changing this to 'prop' and
#' using [scales::percent()] will work and is supported.
#'
#' @param fill Column to fill set bars by. Can be 'set' or another column within
#' the main data object
#' @param colour Primarily used for highlighting points and segments in the
#' intersections matrix
#' @param labels Choose either size or prop to label bars with totals or the
#' proportion of all intersections
#' @param f Function for labelling set or intersection sizes sizes
#' @param expand Multiplicative axis expansion passed to [expansion()]
#' @param name Main axis title
#' @param ... additional layers to include alongside default layers.
#' Will be added after the default layers
#' @param dry_run Set as TRUE to view the unevaluated layers which are defined
#' as the defaults. Additional layers passed through the ellipsis will not be
#' included as part of a dry_run
#' @param light,dark default colours for empty intersections (light) and for
#' both non-empty intersections and segments (dark)
#' @param shape,size Point shape/size passed to the intersections matrix
#' @param label_size Passed to labels for sets and intersections
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
    ..., fill = NULL, labels = "size", f = comma, expand = c(0.2, 0), hjust = 1.1,
    label_size = 3.5, name = "Set Size", dry_run = FALSE
){

  ## Currently, no NULL handler is setup for labels...
  if (!is.null(fill)) fill <- sym(fill)

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
      geom_bar(aes(fill = {{ fill }})),
      geom_text(
        aes(x = size, label = f(!!sym(labels))), hjust = hjust, size = label_size
      ),
      scale_x_reverse(expand = expansion(expand), name = name, labels = comma),
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
    ..., fill = NULL, labels = "size", f = comma, expand = c(0, 0.05),
    vjust = -0.5, label_size = 3.5, name = "Intersection Size",
    dry_run = FALSE
){

  ## Currently, no NULL handler is setup for labels...
  if (!is.null(fill)) fill <- sym(fill)

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
      geom_bar(aes(fill = {{ fill }})),
      geom_text(
        aes(y = size, label = f(!!sym(labels))), vjust = vjust, size = label_size
      ),
      scale_x_discrete(name = NULL, labels = NULL),
      scale_y_continuous(name = name, expand = expansion(expand), labels = comma),
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

  if (!is.null(fill)) fill <- sym(fill)
  if (!is.null(colour)) colour <- sym(colour)

  ## Set aesthetics manually before producing complete layers
  points_aes <- aes(colour = {{ colour }}, fill = {{ fill }})
  segment_aes <- aes(
    y = !!sym("y_min"), yend = !!sym("y_max"), colour = {{ colour }}
  )

  ## Check all dotargs for validity
  dot_list <- list(...)
  valid_gg <- vapply(dot_list, .check_gg_layers, logical(1))
  dot_list <- dot_list[valid_gg]

  layers <- expr(
    list(
      aes(x = intersect, y = set),
      if (!is.null(colour)) {
        geom_point(points_aes, size = size, shape = shape)
      } else {
        geom_point(points_aes, size = size, shape = shape, colour = dark)
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
