#Anthony M. Agelastos, 2016/08/24. Revised for CUG2018 by Ann Gentile 01/05/2018
# Revised for CUG2021 by Jim Brandt 06-16-2021
#amagela's Standard GNU Make Makefile, version 2.0
#This Makefile builds: LongAbstract
#
# THE FOLLOWING TARGETS ARE GENERALLY AVAILABLE
# all:                Performs all tasks to clean, build, and install the application and documentation
# spellcheckaspell:   This runs a spell checker on all TeX files within doc/
# spellcheckhunspell: This runs a spell checker on all TeX files within doc/
# clean:              Delete the files generated from building the examples and documentation
# purge:              This does clean and also removes the resultant document
# tar:                Create a stamped, Gzip-compressed tarball within directory tar

###### USER SETTINGS ######
# TITLE:      Name of resultant documentation
TITLE?=Darshan-LDMS

###### COMPILER SETTINGS (ONLY ADMINS BELOW) ######
#COMPILERS/LINKERS
# MAKPDF:=latexmk -lualatex
MAKPDF:=latexmk -pdf
CLEAN:=latexmk -c
PURGE:=latexmk -C

SPELLCHECKTARGET:=spellcheckaspell
ifeq ($(wildcard /usr/bin/hunspell),/usr/bin/hunspell)
	SPELLCHECKTARGET:=spellcheckhunspell
endif
ifeq ($(wildcard /opt/local/bin/hunspell),/opt/local/bin/hunspell)
	SPELLCHECKTARGET:=spellcheckhunspell
endif

#EXTRA VARS
LOGAPPEND:=$(shell date '+%Y%m%d_%H%M%S')

###### TARGETS ######
default: initpurge pdf clean
pdf: $(TITLE).pdf
$(TITLE).pdf: $(TITLE).tex
%.pdf: %.tex
#	$(MAKPDF) >$*.build.log 2>&1 $<
	$(MAKPDF) $<

.PHONY: clean
clean:
	$(CLEAN)
	-find . -type f -name "*.bbl" -exec rm -f '{}' \;
	-find . -type f -name "*~" -exec rm -f '{}' \;
	-find . -type f -name "*.swp" -exec rm -f '{}' \;
	-find . -type f -name ".*~" -exec rm -f '{}' \;
	-find . -type f -name ".*.swp" -exec rm -f '{}' \;

.PHONY: initpurge
initpurge:
	-rm -f $(TITLE).pdf

.PHONY: purge
purge: clean
	$(PURGE)

.PHONY: spellcheckaspell
spellcheckaspell:
	-find . -type f -name "[0-9]*.tex" -exec aspell -c '{}' --lang=en_US --mode=tex \;

.PHONY: spellcheckhunspell
spellcheckhunspell:
	-find . -type f -name "[0-9]*.tex" -exec hunspell -t -d en_US '{}' \;

.PHONY: spellcheck
spellcheck: $(SPELLCHECKTARGET)

.PHONY: tar
tar: clean
	mkdir -p tar
	tar -czhf tar/$(TITLE)_$(LOGAPPEND).tgz $(TITLE).pdf Makefile *.tex *.bib figs
