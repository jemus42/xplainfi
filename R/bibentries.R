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

#' @importFrom utils person
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
	),

	breiman_2001 = bibentry(
		"article",
		title = "Random Forests",
		author = "Breiman, Leo",
		year = "2001",
		journal = "Machine Learning",
		volume = "45",
		number = "1",
		pages = "5--32",
		doi = "10.1023/A:1010933404324"
	),

	fisher_2019 = bibentry(
		"article",
		title = "All Models Are Wrong, but Many Are Useful: Learning a Variable's Importance by Studying an Entire Class of Prediction Models Simultaneously",
		author = "Fisher, Aaron and Rudin, Cynthia and Dominici, Francesca",
		year = "2019",
		journal = "Journal of Machine Learning Research",
		volume = "20",
		pages = "177",
		url = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8323609/"
	),

	lundberg_2020 = bibentry(
		"inproceedings",
		title = "Understanding Global Feature Contributions With Additive Importance Measures",
		booktitle = "Advances in Neural Information Processing Systems",
		author = "Covert, Ian and Lundberg, Scott M and Lee, Su-In",
		year = "2020",
		volume = "33",
		pages = "17212--17223",
		publisher = "Curran Associates, Inc.",
		url = "https://proceedings.neurips.cc/paper/2020/hash/c7bf0b7c1a86d5eb3be2c722cf2cf746-Abstract.html",
	),

	watson_2021 = bibentry(
		"article",
		title = "Testing conditional independence in supervised learning algorithms",
		author = c(
			person("David S.", "Watson"),
			person("Marvin N.", "Wright")
		),
		journal = "Machine Learning",
		volume = "110",
		number = "8",
		pages = "2107-2129",
		year = "2021",
		doi = "10.1007/s10994-021-06030-6"
	),

	blesch_2023 = bibentry(
		"article",
		title = "Conditional feature importance for mixed data",
		author = c(
			person("Kristin", "Blesch"),
			person("David S.", "Watson"),
			person("Marvin N.", "Wright")
		),
		journal = "AStA Advances in Statistical Analysis",
		volume = "108",
		number = "2",
		pages = "259-278",
		year = "2023",
		doi = "10.1007/s10182-023-00477-9"
	),

	lei_2018 = bibentry(
		"article",
		title = "Distribution-Free Predictive Inference for Regression",
		author = "Lei, Jing and , Max, G'Sell and , Alessandro, Rinaldo and , Ryan J., Tibshirani and Wasserman, Larry",
		year = "2018",
		month = "jul",
		journal = "Journal of the American Statistical Association",
		volume = "113",
		number = "523",
		pages = "1094--1111",
		publisher = "ASA Website",
		issn = "0162-1459",
		doi = "10.1080/01621459.2017.1307116",
	),

	strobl_2008 = bibentry(
		"article",
		title = "Conditional Variable Importance for Random Forests",
		author = "Strobl, Carolin and Boulesteix, Anne-Laure and Kneib, Thomas and Augustin, Thomas and Zeileis, Achim",
		year = "2008",
		month = "jul",
		journal = "BMC Bioinformatics",
		volume = "9",
		number = "1",
		pages = "307",
		issn = "1471-2105",
		doi = "10.1186/1471-2105-9-307"
	),

	molnar_2023 = bibentry(
		"inproceedings",
		title = "Relating the Partial Dependence Plot and Permutation Feature Importance to the Data Generating Process",
		booktitle = "Explainable Artificial Intelligence",
		author = "Molnar, Christoph and Freiesleben, Timo and K\u00f6nig, Gunnar and Herbinger, Julia and Reisinger, Tim and Casalicchio, Giuseppe and Wright, Marvin N. and Bischl, Bernd",
		editor = "Longo, Luca",
		year = "2023",
		pages = "456--479",
		publisher = "Springer Nature Switzerland",
		doi = "10.1007/978-3-031-44064-9_24",
		isbn = "978-3-031-44064-9"
	),

	nadaeu_2003 = bibentry(
		"article",
		title = "Inference for the Generalization Error",
		author = "Nadeau, Claude and Bengio, Yoshua",
		year = "2003",
		journal = "Machine Learning",
		volume = "52",
		number = "3",
		pages = "239--281",
		issn = "1573-0565",
		doi = "10.1023/A:1024068626366"
	),

	little_2019 = bibentry(
		"book",
		title = "Statistical Analysis with Missing Data",
		author = c(
			person("Roderick J. A.", "Little"),
			person("Donald B.", "Rubin")
		),
		year = "2019",
		edition = "3rd",
		publisher = "John Wiley & Sons",
		address = "Hoboken, NJ",
		isbn = "9780470526798"
	),

	troyanskaya_2001 = bibentry(
		"article",
		title = "Missing Value Estimation Methods for DNA Microarrays",
		author = c(
			person("Olga", "Troyanskaya"),
			person("Michael", "Cantor"),
			person("Gavin", "Sherlock"),
			person("Pat", "Brown"),
			person("Trevor", "Hastie"),
			person("Robert", "Tibshirani"),
			person("David", "Botstein"),
			person("Russ B.", "Altman")
		),
		year = "2001",
		journal = "Bioinformatics",
		volume = "17",
		number = "6",
		pages = "520--525",
		doi = "10.1093/bioinformatics/17.6.520"
	),

	anderson_2003 = bibentry(
		"book",
		title = "An Introduction to Multivariate Statistical Analysis",
		author = person("Theodore W.", "Anderson"),
		year = "2003",
		edition = "3rd",
		publisher = "Wiley-Interscience",
		address = "Hoboken, NJ",
		isbn = "9780471360919"
	),

	mardia_1979 = bibentry(
		"book",
		title = "Multivariate Analysis",
		author = c(
			person("K. V.", "Mardia"),
			person("J. T.", "Kent"),
			person("J. M.", "Bibby")
		),
		year = "1979",
		publisher = "Academic Press",
		address = "London",
		isbn = "9780124712522"
	),

	hothorn_2006 = bibentry(
		"article",
		title = "Unbiased Recursive Partitioning: A Conditional Inference Framework",
		author = c(
			person("Torsten", "Hothorn"),
			person("Kurt", "Hornik"),
			person("Achim", "Zeileis")
		),
		year = "2006",
		journal = "Journal of Computational and Graphical Statistics",
		volume = "15",
		number = "3",
		pages = "651--674",
		doi = "10.1198/106186006X133933"
	),

	aas_2021 = bibentry(
		"article",
		title = "Explaining Individual Predictions When Features Are Dependent: More Accurate Approximations to Shapley Values",
		author = c(
			person("Kjersti", "Aas"),
			person("Martin", "Jullum"),
			person("Anders", "Løland")
		),
		year = "2021",
		journal = "Artificial Intelligence",
		volume = "298",
		pages = "103502",
		doi = "10.1016/j.artint.2021.103502"
	)
)
