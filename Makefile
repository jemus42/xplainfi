PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`

all: format doc README.md install check

.PHONY: format
format:
	air format .

.PHONY: doc
doc: README.md
	Rscript -e "usethis::use_tidy_description()"
	Rscript -e "devtools::document()"

.PHONY: build
build:
	Rscript -e "devtools::build()"

.PHONY: vignettes
vignettes:
	Rscript -e "devtools::build_vignettes()"

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

coverage:
	Rscript -e "covr::report(covr::package_coverage(\".\"), file = \"coverage.html\")"

.PHONY: check-remote
check-remote:
	Rscript -e "devtools::check(remote = TRUE)"

.PHONY: site
site:
	Rscript -e "pkgdown::build_site()"


FIPPY_RESULT=vignettes/articles/fippy-comparison/fippy_results.json
FIPPY_SCRIPT=vignettes/articles/fippy-comparison/calculate_fippy.py

XPLAINFI_RESULT=vignettes/articles/fippy-comparison/xplainfi_results.json
XPLAINFI_SCRIPT=vignettes/articles/fippy-comparison/calculate_xplainfi.R

.PHONY: fippy-comparison fippy-comparison-fippy fippy-comparison-xplainfi
fippy-comparison: fippy-comparison-fippy fippy-comparison-xplainfi

fippy-comparison-fippy: $(FIPPY_RESULT)

fippy-comparison-xplainfi: $(XPLAINFI_RESULT)

$(FIPPY_RESULT): $(FIPPY_SCRIPT)
	@echo "Running fippy comparison script..."
	cd vignettes/articles/fippy-comparison && \
	./calculate_fippy.py

$(XPLAINFI_RESULT): $(XPLAINFI_SCRIPT)
	@echo "Running xplainfi comparison script..."
	cd vignettes/articles/fippy-comparison && \
	Rscript calculate_xplainfi.R

README.md: README.Rmd
	Rscript -e "rmarkdown::render('README.Rmd')"
	rm README.html

clean:
	rm vignettes/*html
	rm -r docs
	rm -rf lib
	rm coverage.html
