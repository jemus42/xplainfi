#' Fisher test wrapper extracted from cpi pkg
#' Only performs one sided test
#' @noRd
fisher_one_sided = function(
	x,
	B = 1999,
	conf.level = 0.95,
	...
) {
	orig_mean = mean(x)
	# B permutations
	perm_means = replicate(B, {
		signs = sample(c(-1, 1), length(x), replace = TRUE)
		mean(signs * x)
	})
	list(
		p.value = (sum(perm_means >= orig_mean) + 1) / (B + 1),
		conf.int = orig_mean - quantile(perm_means, conf.level)
	)
}

#' Binomial test wrapper extracted from cpi pkg
#' Only performs one sided test
#' @noRd
binom_one_sided = function(x, conf.level = 0.95, ...) {
	binom.test(sum(x > 0), length(x), alternative = "greater", conf.level = conf.level)
}
