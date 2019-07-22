PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: check clean

man-docs:
	R --silent -e 'devtools::document()'

html-docs:
	R --silent -e 'rmarkdown::render("vignettes/guide.Rmd", "html_document", output_dir="/tmp")'

pdf-docs:
	R --silent -e 'rmarkdown::render("vignettes/guide.Rmd", "pdf_document", output_dir="/tmp")'

build: man-docs
	cd ..;\
	R CMD build $(PKGSRC)

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	$(RM) -rf inst/doc
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/
  
test:
	R --silent -e 'if(!require("testthat")) install.packages("testthat"); devtools::test()'
