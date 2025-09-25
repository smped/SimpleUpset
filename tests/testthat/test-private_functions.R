
test_that(".add_intersections adds & transforms columns", {
  hl <- NULL
  sort_intersect <- substitute(list(desc(size)))
  tbl <- .add_intersections(movies, sets, sort_intersect, TRUE, enquo(hl))
  expect_true(all(vapply(tbl[sets], is.logical, logical(1))))
  expect_true(all(colnames(tbl) %in% c(colnames(movies), "intersect", "degree")))
  expect_null(tbl[["highlight"]])
  hl <- ""
  expect_error(.add_intersections(movies, sets, sort_intersect, TRUE, hl))

})

test_that("Sorting works as expected", {
  hl <- NULL

  sort_intersect <- substitute(list(desc(size)))
  tbl <- .add_intersections(movies, sets, sort_intersect, TRUE, enquo(hl))
  expect_true(which.max(table(tbl$intersect)) == 1)

  sort_intersect <- substitute(list(size))
  tbl <- .add_intersections(movies, sets, sort_intersect, TRUE, enquo(hl))
  expect_true(which.max(table(tbl$intersect)) == 27)

  sort_intersect <- substitute(list(desc(degree), desc(size)))
  tbl <- .add_intersections(movies, sets, sort_intersect, TRUE, enquo(hl))
  expect_true(which.max(table(tbl$intersect)) == 23)

  sort_intersect <- substitute(list(degree, desc(size)))
  tbl <- .add_intersections(movies, sets, sort_intersect, TRUE, enquo(hl))
  expect_true(table(tbl$intersect)[[5]] == 65)

  sort_intersect <- substitute(desc(size))
  expect_error(
    .add_intersections(movies, sets, sort_intersect, TRUE, enquo(hl)),
    "Sorting.+"
  )

})

test_that(".bg_stripes produces GeomRect", {
  col <- c("grey", "white")
  gg <- .bg_stripes(sets, col)
  expect_true(is(gg$geom, "GeomRect"))
  expect_true(gg$aes_params$height == 1)
  expect_equal(gg$aes_params$fill, rep_len(col, length(sets)))
  expect_equal(
    vapply(gg$mapping, as_label, character(1)),
    c(y = "set", xmin = "xmin", xmax = "xmax")
  )
  expect_null(.bg_stripes(sets, NULL))
})

test_that(".check_sets behaves as expected", {
  expect_error(.check_sets(movies, "ReleaseDate", TRUE), ".+ReleaseDate.+")
  expect_error(.check_sets(movies, NULL, TRUE))
  expect_error(.check_sets(movies, "x", TRUE))
  expect_equal(.check_sets(movies, sets, TRUE), sets)
})
