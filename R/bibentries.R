#' Print an Rd-formatted bib entry
#'
#' @keywords internal
#' @param ... (`character`) One or more quoted names of `bibentries` to print.
#' @importFrom tools toRd
#' @importFrom utils bibentry
print_bib = function(...) {
  str = sapply(list(...), function(entry) tools::toRd(bibentries[[entry]]))
  paste0(str, collapse = "\n\n")
}

bibentries = c(
  ewald_2024 = bibentry(
    "inproceedings",
    title = "A Guide to Feature Importance Methods for Scientific Inference",
    booktitle = "Explainable Artificial Intelligence",
    author = "Ewald, Fiona Katharina and Bothmann, Ludwig and Wright, Marvin N. and Bischl, Bernd and Casalicchio, Giuseppe and K\u00f6nig, Gunnar",
    editor = "Longo, Luca and Lapuschkin, Sebastian and Seifert, Christin",
    year = "2024",
    pages = "440--464",
    publisher = "Springer Nature Switzerland",
    location = "Cham",
    doi = "10.1007/978-3-031-63797-1_22",
    isbn = "978-3-031-63797-1"
  ),

  konig_2021 = bibentry(
    "inproceedings",
    title = "Relative Feature Importance",
    author = "K\u00f6nig, Gunnar and Molnar, Christoph and Bischl, Bernd and Grosse-Wentrup, Moritz",
    year = "2021",
    booktitle = "2020 25th International Conference on Pattern Recognition (ICPR)",
    pages = "9318--9325",
    doi = "10.1109/ICPR48806.2021.9413090"
  ),

  blesch_2025 = bibentry(
    "article",
    title = "Conditional Feature Importance with Generative Modeling Using Adversarial Random Forests",
    author = "Blesch, Kristin and Koenen, Niklas and Kapar, Jan and Golchian, Pegah and Burk, Lukas and Loecher, Markus and Wright, Marvin N.",
    year = "2025",
    journal = "Proceedings of the AAAI Conference on Artificial Intelligence",
    volume = "39",
    number = "15",
    pages = "15596--15604",
    doi = "10.1609/aaai.v39i15.33712"
  ),

  watson_2023 = bibentry(
    "inproceedings",
    title = "Adversarial Random Forests for Density Estimation and Generative Modeling",
    booktitle = "Proceedings of The 26th International Conference on Artificial Intelligence and Statistics",
    author = "Watson, David S. and Blesch, Kristin and Kapar, Jan and Wright, Marvin N.",
    year = "2023",
    pages = "5357--5375",
    publisher = "PMLR",
    url = "https://proceedings.mlr.press/v206/watson23a.html"
  )
)
