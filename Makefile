PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)
BIOCVER := RELEASE_3_15

all: rd check1 clean

rd:
	Rscript -e 'library(methods);devtools::document()'

readme:
	Rscript -e 'rmarkdown::render("README.Rmd", encoding="UTF-8")'

build1:
	Rscript -e 'devtools::build()'

build2:
	Rscript -e 'devtools::build(vignettes = FALSE)'

install:
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check1: rd build1
	cd ..;\
	Rscript -e 'rcmdcheck::rcmdcheck("$(PKGNAME)_$(PKGVERS).tar.gz")'

crancheck: rd build1
	cd ..;\
	Rscript -e 'rcmdcheck::rcmdcheck("$(PKGNAME)_$(PKGVERS).tar.gz", args="--as-cran")'

bioccheck:
	cd ..;\
	Rscript -e 'BiocCheck::BiocCheck("$(PKGNAME)_$(PKGVERS).tar.gz")'

debug: rd build1
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

vignettes:
	Rscript -e 'usethis::use_vignette("$(PKGNAME)")'
	
clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/

clean2:
	cd ..;\
	$(RM) $(PKGNAME)_$(PKGVERS).tar.gz

bignore:
	Rscript -e 'usethis::use_build_ignore(glob2rx("inst/extdata/*.png"), escape = FALSE)'
	Rscript -e 'usethis::use_build_ignore(c("Makefile", "README.md", "README.Rmd", "CONDUCT.md", ".Rproj.user", ".Rproj"))'

gignore:
	Rscript -e 'usethis::use_git_ignore(c(".DS_Store", ".RData", ".Rhistory", ".Rproj.user"))'

biocinit:
	git remote add upstream git@git.bioconductor.org:packages/$(PKGNAME).git;\
	git fetch --all

rmrelease:
	git branch -D $(BIOCVER)

release:
	git checkout $(BIOCVER);\
    git fetch --all

update:
	git fetch --all
	git merge upstream/master
	git merge origin/master

submit:
	git push upstream master
	git push origin master
