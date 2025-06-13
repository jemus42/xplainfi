PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`

all: format doc README.md install check

.PHONY: format
format:
	air format .

.PHONY: doc
doc:
	Rscript -e "usethis::use_tidy_description()"
	Rscript -e "devtools::document()"

.PHONY: build
build:
	Rscript -e "devtools::build()"

.PHONY: install
install:
	Rscript -e "pak::local_install()"

.PHONY: deps 
deps:
	Rscript -e "pak::local_install_dev_deps()"

# devtools::check() automatically redocuments, so no need to add doc here
.PHONY: check
check:
	Rscript -e "devtools::check()"

.PHONY: test
test:
	Rscript -e "devtools::test()"

.PHONY: check-remote
check-remote:
	Rscript -e "devtools::check(remote = TRUE)"

.PHONY: site
site:
	Rscript -e "pkgdown::build_site()"

README.md: README.Rmd
	Rscript -e "rmarkdown::render('README.Rmd')"
	rm README.html

clean:
	rm vignettes/*html
	rm -r docs
