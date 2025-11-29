test_that("Basic plotting works", {
  p <- simpleUpSet(movies, sets)
  ## Basic structure
  expect_true(is(p, "patchwork"))
  expect_equal(length(p), 4)
  ## Simple failures
  expect_error(simpleUpSet(movies, sets, width = 10))
  ## Expected data objects
  expect_true(is_waiver(prop(p[[1]], 'data')))
  expect_true("intersect" %in% colnames(prop(p[[2]], 'data')))
  expect_true("set" %in% colnames(prop(p[[3]], 'data')))
  expect_true(all(c("set","intersect") %in% colnames(prop(p[[4]], 'data'))))
  ## Expected layers
  expect_equal(names(prop(p[[2]], 'layers')), c("geom_bar", "geom_text"))
  expect_equal(
    names(prop(p[[3]], 'layers')), c("geom_rect", "geom_bar", "geom_text")
  )
  expect_equal(
    unname(vapply(prop(p[[4]], 'layers'), \(x) is(x$geom), character(1))),
    c("GeomRect", "GeomPoint", "GeomPoint", "GeomSegment")
  )
  ## Expected scales
  expect_equal(
    vapply(prop(p[[2]], 'scales')$scales, is, character(1)),
    c("ScaleDiscretePosition", "ScaleContinuousPosition")
  )
  expect_equal(prop(p[[3]], 'scales')$scales[[1]]$trans$name, "reverse")
  expect_equal(
    vapply(prop(p[[3]], 'scales')$scales, is, character(1)),
    c("ScaleDiscretePosition", "ScaleContinuousPosition")[c(2, 1)]
  )
  expect_equal(
    vapply(prop(p[[4]], 'scales')$scales, is, character(1)),
    rep("ScaleDiscretePosition", 2)
  )

})

test_that("Adding upper panels works as expected", {
  p <- simpleUpSet(
    movies, sets, n_intersect = 10,
    annotations = list(geom_boxplot(aes(y = AvgRating)))
  )
  expect_true(is(p, "patchwork"))
  expect_true(is(p[[2]], "patchwork"))
  expect_true(is(prop(p[[2]][[1]], 'layers')[[1]]$geom, "GeomBoxplot"))
  expect_equal(as_label(prop(p[[2]][[1]], 'layers')[[1]]$mapping$y), "AvgRating")
  expect_equal(prop(p[[2]][[1]], 'theme')$margins, margin(0, 5.5, 5.5, 0))

  expect_error(simpleUpSet(movies, sets, annotations = "bob"))
})

test_that("Manually contructed lists are handled correctly", {

  ## Test correct geoms
  test_list <- list(aes(), geom_text())
  expect_error(simpleUpSet(movies, sets, set_layers = test_list))
  expect_error(simpleUpSet(movies, sets, intersect_layers = test_list))
  expect_error(simpleUpSet(movies, sets, grid_layers = test_list))
  ## Test for missing geoms
  test_list <- list(aes())
  expect_error(simpleUpSet(movies, sets, set_layers = test_list))
  expect_error(simpleUpSet(movies, sets, intersect_layers = test_list))
  expect_error(simpleUpSet(movies, sets, grid_layers = test_list))
  test_list <- list(aes(), geom_segment())
  expect_error(simpleUpSet(movies, sets, grid_layers = test_list))

})

test_that("Upper Left works as expected", {
  p <- simpleUpSet(
    movies, sets, top_left = ggplot(cars, aes(speed, dist)) + geom_point()
  )
  expect_true(is_ggplot(p[[1]]))
  expect_equal(
    vapply(prop(p[[1]], 'mapping'), as_label, character(1)), c(x = "speed", y = "dist")
  )

})
test_that("highlights work as expected", {
  p <- simpleUpSet(movies, sets, highlight = case_when(Action ~TRUE))
  expect_true("highlight" %in% colnames(prop(p[[2]], 'data')))
  expect_error(simpleUpSet(movies, sets, highlight = TRUE), "highlight can only.+")
})

test_that("Empty points are shown for sets without an intersection", {
  a <- sample(letters, 20)
  b <- sample(letters, 20)
  c <- "dog"
  vals <- unique(c(a, b, c))
  df <- as.data.frame(
    lapply(list(a = a, b = b, c = c) , \(x) vals %in% x)
  )
  p <- simpleUpSet(df, min_size = 2)
  expect_true("c" %in% p[[4]]@layers[[3]]$data$set)


})
