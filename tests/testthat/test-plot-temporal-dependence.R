test_that("plot_temporal_dependence returns correlation and pair plots", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  plot <- plot_temporal_dependence(fixture$result)

  expect_true(is.list(plot))
  expect_true(all(c("correlation", "pairs") %in% names(plot)))
  expect_true(all(c("original", "clustered", "delta") %in% names(plot$correlation)))
  expect_s3_class(plot$correlation$original, "ggplot")
  expect_s3_class(plot$correlation$clustered, "ggplot")
  expect_s3_class(plot$correlation$delta, "ggplot")
  expect_true(length(plot$pairs) >= 1L)
  expect_true(all(vapply(plot$pairs, inherits, logical(1), what = "ggplot")))
})

test_that("plot_temporal_dependence validates bad pair overrides", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  bad_pairs <- list(
    list(
      name = "bad",
      x_var = "demand",
      x_node = "missing-node",
      y_var = "wind"
    )
  )

  expect_error(
    plot_temporal_dependence(fixture$result, pairs = bad_pairs),
    "pair|y_node|missing-node|x_node"
  )
})

test_that("default dependence pairs exist and node scoping narrows correlation data", {
  fixture <- make_plot_result_fixture()
  on.exit(unlink(fixture$fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  pairs <- mick:::default_dependence_pairs(fixture$result)
  all_nodes <- mick:::prepare_dependence_correlation_data(fixture$result)
  first_node <- fixture$result$spatial$membership$cluster[[1L]]
  scoped <- mick:::prepare_dependence_correlation_data(fixture$result, node = first_node)

  expect_true(length(pairs) >= 1L)
  expect_true(all(vapply(pairs, is.list, logical(1))))
  expect_true(nrow(scoped$original) < nrow(all_nodes$original))
  expect_setequal(unique(scoped$original$node), first_node)
})
