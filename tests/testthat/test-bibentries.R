test_that("bibentries are valid", {
  expect_s3_class(bibentries, "bibentry")
  expect_gte(length(bibentries), 5)

  print_bib("ewald_2024", "konig_2021") |>
    expect_equal_to_reference(
      "Ewald, Katharina F, Bothmann, Ludwig, Wright, N. M, Bischl, Bernd, Casalicchio, Giuseppe, König, Gunnar (2024).\n\\dQuote{A Guide to Feature Importance Methods for Scientific Inference.}\nIn Longo, Luca, Lapuschkin, Sebastian, Seifert, Christin (eds.), \\emph{Explainable Artificial Intelligence}, 440--464.\nISBN 978-3-031-63797-1, \\doi{10.1007/978-3-031-63797-1_22}.\n\nKönig, Gunnar, Molnar, Christoph, Bischl, Bernd, Grosse-Wentrup, Moritz (2021).\n\\dQuote{Relative Feature Importance.}\nIn \\emph{2020 25th International Conference on Pattern Recognition (ICPR)}, 9318--9325.\n\\doi{10.1109/ICPR48806.2021.9413090}."
    )
})
