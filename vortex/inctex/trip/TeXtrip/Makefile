#
#   Makefile for the =trip subdirectory of the UNIX TeX82 distribution
#
#   This makefile runs the nefarious trip test.
#

FONTDIR= ./fonts

test: triptex trip.tex $(FONTDIR)/trip.tfm
#	-triptex trip
#	-triptex -b trip
	-triptex -b < tripin.1
	mv trip.log tripin.log
	-diff tripin.log SAVEtripin.log > tripin.diff
#	-triptex \&trip trip            > trip.fot
#	-triptex -b \&trip trip            > trip.fot
	-triptex -b < tripin.2            > trip.fot
	-diff trip.fot   SAVEtrip.fot   > tripfot.diff
	-diff trip.log   SAVEtrip.log   > trip.log.diff
	-diff tripos.tex SAVEtripos.tex > tripos.diff
	dvitype trip.dvi                < SAVEdvitype.in > /dev/null
	mv dvitype.out trip.typ
	-diff trip.typ   SAVEtrip.typ   > trip.typ.diff

trip.tex: SAVEtrip.tex
	cp SAVEtrip.tex trip.tex

triptex:
	mv ../triptex triptex

$(FONTDIR)/trip.tfm: SAVEtrip.tfm
	cp SAVEtrip.tfm $(FONTDIR)/trip.tfm

clean:
	rm -f trip.tex trip.log trip.fot tripin.log
	rm -f trip.dvi trip.typ trip.fmt tripos.tex
	rm -f *.diff
