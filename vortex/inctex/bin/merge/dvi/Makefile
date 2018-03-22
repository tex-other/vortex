#
# Copyright (c) 1987 University of Maryland Department of Computer Science.
# All rights reserved.  Permission to copy for any purpose is hereby granted
# so long as this copyright notice remains intact.
#
# $Header: /home/yew/yew4/vortex/newdist/inctex/bin/merge/dvi/RCS/Makefile,v 1.3 1992/04/15 21:34:28 pan Exp $
#
# Makefile for ctex/dvi (DVI-to-DVI conversion programs)
#
DESTDIR=../../..
#DESTDIR=~pan/
#CFLAGS=	-I../h -DPAN -E -C    #preprocessor pass, saves comments
#CFLAGS=	-I../h -DPAN
#CFLAGS=	-I../h -DPAN -g
#CFLAGS=	-O -R -I../h -DPAN
CFLAGS=	-O -I../h -DPAN

# these are intended to be overridden by the definition in ../Makefile
# but just in case, it's included here too
BINDIR=	${DESTDIR}/bin
MANDIR=	${DESTDIR}/man

SRCS=	dviselect.c mergedvi.c

usual: patchdvi

all: dviselect mergedvi

install: inst-dviselect inst-mergedvi

clean:
	rm -f core *.o dviselect *.bak

dist-checkout: RCS/patchdvi.c,v
	co patchdvi.c

inst-patchdvi: patchdvi
	install -s patchdvi ${BINDIR}/patchdvi

inst-dviselect: dviselect
	install -s dviselect ${BINDIR}/dviselect
	install -c -m 444 ../man/dviselect.1 ${MANDIR}/man1/dviselect.1

depend: ${SRCS}
	${CC} -M ${CFLAGS} ${SRCS} |\
	    awk '{ if ($$1 != prev) { if (rec != "" ) print rec;\
		rec = $$0; prev = $$1; }\
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; }\
		else rec = rec " " $$2 } }\
		END { print rec }' >makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile <eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >>Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >>Makefile
	echo '# see make depend above' >>Makefile

patchdvi: patchdvi.o ../lib/lib.a
	${CC} ${CFLAGS} -o patchdvi patchdvi.o ../lib/lib.a
	@echo done

mergedvi: mergedvi.o ../lib/lib.a
	${CC} ${CFLAGS} -o mergedvi mergedvi.o ../lib/lib.a
	@echo done

dviselect: dviselect.o ../lib/lib.a
	${CC} ${CFLAGS} -o dviselect dviselect.o ../lib/lib.a
	@echo done

lint: ${SRCS}
	lint -I../h -hbxuL ../lib/llib-lib dviselect.c

# DO NOT DELETE THIS LINE -- make depend uses it

dviselect.c:
dviselect.o: dviselect.c ../h/types.h ../h/dvi.h ../h/dviclass.h
dviselect.o: ../h/dvicodes.h ../h/fio.h ../h/search.h /usr/include/stdio.h
dviselect.o: /usr/include/ctype.h
interleavedvi.c:
patchdvi.o: patchdvi.c ../h/types.h ../h/dvi.h ../h/dviclass.h
patchdvi.o: ../h/dvicodes.h ../h/fio.h ../h/search.h /usr/include/stdio.h
patchdvi.o: /usr/include/ctype.h
mergedvi.c:
mergedvi.o: mergedvi.c ../h/types.h ../h/dvi.h ../h/dviclass.h ../h/dvicodes.h
mergedvi.o: ../h/fio.h ../h/search.h /usr/include/stdio.h /usr/include/ctype.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
