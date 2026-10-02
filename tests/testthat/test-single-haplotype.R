test_that("malaria.em handles a single possible haplotype", {
  mat <- matrix(
    c("A", "A", "A",
      "C", "C", "C"),
    ncol = 2,
    dimnames = list(c("s1", "s2", "s3"), c("locus1", "locus2"))
  )

  fit <- expect_no_error(
    malaria.em(mat, sizes = 1:2, locus.label = colnames(mat))
  )

  expect_equal(length(fit$haplo.prob), 1)
  expect_equal(fit$haplo.prob, 1)
  expect_equal(fit$haplo.prob.std, 0)
  expect_equal(unname(fit$haplotype[1, ]), c("A", "C"))
})
