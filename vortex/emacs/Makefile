# Copyright (C) 1985-1992  Regents of the University of California
#
# In case of problems, send e-mail to vortex@cs.berkeley.edu
#
# Makefile for GNU Emacs TeX-mode and BibTeX-mode
#
# This makefile is designed for VorTeX Project internal use.  It is 
# unlikely you will need to use it.

SRCDIR = /home/yew/yew4/vortex/src/emacs

SRC = \
	BibTeX-abv.el \
	BibTeX-misc.el \
	BibTeX-mode.el \
	BibTeX-notes.el \
	BibTeX-ops.el \
	TeX-bib.el \
	TeX-cite.el \
	TeX-custom.el \
	TeX-index.el \
	TeX-insp.el \
	TeX-match.el \
	TeX-misc.el \
	TeX-mode.el \
	TeX-spell.el \
	VorTeX-common.el

OTHER = README.EMACS

checkout:
	co $(SRC) $(OTHER)

copy-docs:
	cp -rp 	$(SRCDIR)/doc $(SRCDIR)/paper .

dist-checkout: checkout copy-docs

compile:
	emacs -batch -f batch-byte-compile $(SRC)
