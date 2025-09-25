
test_that("default_sets_layer behaves as expected", {

  ## Default settings
  set_list <- default_set_layers()
  ## The overall structure
  expect_true(is(set_list, "default_layers"))
  ## Correct internal structure
  expect_equal(as_label(set_list[[1]]$y), "set")
  expect_true(is(set_list[[2]]$geom, "GeomBar"))
  expect_true(is(set_list[[3]]$geom, "GeomText"))
  expect_true(is(set_list[[4]], "ScaleContinuousPosition"))
  expect_true(is(set_list[[5]], "ScaleDiscretePosition"))
  expect_true(is(set_list[[6]], "theme"))
  ## Correctly passed variables
  expect_equal(unname(set_list[[2]]$aes_params), list())
  expect_equal(
    vapply(set_list[[3]]$mapping, as_label, character(1)),
    c(x = "size", label = "f(size)")
  )
  expect_equal(
    unlist(set_list[[3]]$aes_params), c(hjust = 1.1, size = 3.5)
  )
  expect_equal(set_list[[4]]$labels(1000), "1,000")
  expect_equal(set_list[[4]]$expand, c(0.2, 0, 0, 0))
  expect_equal(set_list[[4]]$name, "Set Size")
  expect_equal(set_list[[5]]$position, "right")
  expect_null(set_list[[5]]$name)
  expect_null(set_list[[5]]$labels)
  expect_equal(set_list[[6]]$margins, margin(5.5, 0, 5.5, 5.5))
  expect_true(is(set_list[[6]]$axis.ticks.y.right, "ggplot2::element_blank"))

  ## Now pass key arguments
  set_list <- default_set_layers(fill = "set", f = NULL)
  expect_equal(as_label(set_list[[2]]$mapping$fill), "set")
  expect_equal(set_list[[4]]$labels(1000), 1000)

  ## Dry run
  expect_error(eval(default_set_layers(dry_run = TRUE)), "object.+")

  ## Fail
  expect_error(default_set_layers(f = ""))

  ## Extra Layers
  expect_equal(
    default_set_layers()[-(1:3)], default_set_layers("bob")[-(1:3)]
  ) ## bob should be ignored as a layer
  expect_true(is_guides(default_set_layers(guides(fill = guide_none()))[[7]]))


})

test_that("default_intersect_layers behaves as expected", {

  ## Default settings
  intersect_list <- default_intersect_layers()
  ## The overall structure
  expect_true(is(intersect_list, "default_layers"))
  ## Correct internal structure
  expect_equal(as_label(intersect_list[[1]]$x), "intersect")
  expect_true(is(intersect_list[[2]]$geom, "GeomBar"))
  expect_true(is(intersect_list[[3]]$geom, "GeomText"))
  expect_true(is(intersect_list[[4]], "ScaleDiscretePosition"))
  expect_true(is(intersect_list[[5]], "ScaleContinuousPosition"))
  expect_true(is(intersect_list[[6]], "theme"))
  ## Correctly passed variables
  expect_equal(unname(intersect_list[[2]]$aes_params), list())
  expect_equal(
    vapply(intersect_list[[3]]$mapping, as_label, character(1)),
    c(y = "size", label = "f(size)")
  )
  expect_equal(
    unlist(intersect_list[[3]]$aes_params), c(vjust = -0.5, size = 3.5)
  )
  expect_null(intersect_list[[4]]$labels)
  expect_null(intersect_list[[4]]$name)
  expect_equal(intersect_list[[5]]$expand, c(0, 0, 0.05, 0))
  expect_equal(intersect_list[[5]]$labels(1000), "1,000")
  expect_equal(intersect_list[[5]]$name, "Intersection Size")
  expect_equal(intersect_list[[6]]$margins, margin(5.5, 5.5, 0, 0))
  expect_true(is(intersect_list[[6]]$axis.ticks.x.bottom, "ggplot2::element_blank"))

  ## Now pass key arguments
  intersect_list <- default_intersect_layers(fill = "Decade", f = NULL)
  expect_equal(as_label(intersect_list[[2]]$mapping$fill), "Decade")
  expect_equal(intersect_list[[5]]$labels(1000), 1000)

  ## Dry run
  expect_error(eval(default_intersect_layers(dry_run = TRUE)), "object.+")

  ## Fail
  expect_error(default_intersect_layers(f = ""))

  ## Extra Layers
  expect_equal(
    default_intersect_layers()[-(1:3)], default_intersect_layers("bob")[-(1:3)]
  ) ## bob should be ignored as a layer
  expect_true(
    is_guides(default_intersect_layers(guides(fill = guide_none()))[[7]])
  )

})

test_that("default_grid_layers behaves as expected", {

  ## Default settings
  grid_list <- default_grid_layers()
  ## The overall structure
  expect_true(is(grid_list, "default_layers"))
  ## Correct internal structure
  expect_equal(as_label(grid_list[[1]]$x), "intersect")
  expect_equal(as_label(grid_list[[1]]$y), "set")
  expect_true(is(grid_list[[2]]$geom, "GeomPoint"))
  expect_true(is(grid_list[[3]]$geom, "GeomPoint"))
  expect_true(is(grid_list[[4]]$geom, "GeomSegment"))
  expect_true(is(grid_list[[5]], "ScaleDiscretePosition"))
  expect_true(is(grid_list[[6]], "ScaleDiscretePosition"))
  expect_true(is_guides(grid_list[[7]]))
  expect_true(is(grid_list[[8]], "theme"))
  ## Correctly passed variables
  ## Main points
  expect_true(is_mapping(grid_list[[2]]$mapping))
  expect_equal(length(grid_list[[2]]$mapping), 0)
  expect_equal(
    grid_list[[2]]$aes_params, list(size = 4, shape = 19, colour = "grey23")
  )
  ## Empty points
  expect_null(grid_list[[3]]$mapping)
  expect_equal(
    grid_list[[3]]$aes_params, list(size = 4, shape = 19, colour = "grey80")
  )
  ## Segments
  expect_equal(
    vapply(grid_list[[4]]$mapping, as_label, character(1)),
    c(y = "y_min", yend = "y_max")
  )
  expect_equal(grid_list[[4]]$aes_params$colour, "grey23")
  ## Axes
  expect_null(grid_list[[5]]$name)
  expect_null(grid_list[[6]]$name)
  expect_null(grid_list[[6]]$labels)
  ## Guide
  expect_true(is_guides(grid_list[[7]]))
  ## Theme
  expect_equal(grid_list[[8]]$margins, margin(5.5, 5.5, 5.5, 0))
  expect_true(is(grid_list[[8]]$axis.ticks, "ggplot2::element_blank"))

  ## Now pass key arguments
  grid_list <- default_grid_layers(
    colour = "highlight", fill = "highlight", name = "Intersection"
  )
  ## Points
  expect_equal(
    vapply(grid_list[[2]]$mapping, as_label, character(1)),
    c(colour = "highlight", fill = "highlight")
  )
  expect_equal(grid_list[[2]]$aes_params, list(size = 4, shape = 19))
  ## Make sure the empty points are as before
  expect_null(grid_list[[3]]$mapping)
  expect_equal(
    grid_list[[3]]$aes_params, list(size = 4, shape = 19, colour = "grey80")
  )
  ## Segments
  expect_equal(as_label(grid_list[[4]]$mapping$colour), "highlight")
  expect_equal(length(grid_list[[4]]$aes_params), 0)

  ## Dry run
  expect_error(eval(default_grid_layers(dry_run = TRUE)), "object.+")

  ## Extra Layers
  expect_equal(
    default_grid_layers()[-(1:4)], default_grid_layers("bob")[-(1:4)]
  ) ## bob should be ignored as a layer
  expect_true(
    is_guides(default_grid_layers(guides(fill = guide_legend()))[[9]])
  )

})
