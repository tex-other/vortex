#
# Copyright (c) 1987 University of Maryland Department of Computer Science.
# All rights reserved.  Permission to copy for any purpose is hereby granted
# so long as this copyright notice remains intact.
#
# Makefile for ctex/lib (C-TeX library routines)
#
# $Header: Makefile,v 1.2 87/06/16 18:27:29 chris Exp $
#
DESTDIR=/usr4/texers/pan/inctex/stuff
# Alas, -R makes ALL initialised variables read-only, and we cannot
# use it on font files; hence, FCFLAGS.
CFLAGS=	-O -R -I../h
FCFLAGS=-O -I../h

# This will be overridden by ../Makefile, but just in case...
CONFFILE=/usr/local/lib/tex/fontdesc

# 4.1BSD Vax:
#ASSRC=	bcopy.s bzero.s
#ASOBJ=	bcopy.o bzero.o
#MISCC=	getopt.c
#MISCO=	getopt.o

# 4.2BSD Vax:
#ASSRC=
#ASOBJ=
#MISCC=	getopt.c
#MISCO=	getopt.o

# 4.2BSD Sun:
#ASSRC=
#ASOBJ=
#MISCC=
#MISCO=

# 4.2BSD Pyramid:
#ASSRC=
#ASOBJ=
#MISCC=	getopt.c
#MISCO=	getopt.o

# 4.3BSD Vax:
ASSRC=
ASOBJ=
MISCC=
MISCO=

# all
CSRC=	conv.c dviclass.c error.c findpost.c fio.c font.c font_subr.c \
	gfclass.c gripes.c magfactor.c rotate.c scaletfm.c scanpost.c \
	search.c seek.c split.c strsave.c tfm.c ${MISCC}
COBJ=	conv.o dviclass.o error.o findpost.o fio.o font.o font_subr.o \
	gfclass.o gripes.o magfactor.o rotate.o scaletfm.o scanpost.o \
	search.o seek.o split.o strsave.o tfm.o ${MISCO}
FSRC=	gffont.c pkfont.c pxlfont.c tfmfont.c
FOBJ=	gffont.o pkfont.o pxlfont.o tfmfont.o

OBJS=	${COBJ} ${ASOBJ} ${FOBJ}

all: lib.a

lib.a: ${OBJS}
	ar cr lib.a ${OBJS}
	ranlib lib.a

# no installation is necessary; this entry is just for standardisation
install:

clean:
	rm -f *.o lib.a

depend: ${CSRC} ${FSRC}
	${CC} -M ${CFLAGS} ${CSRC} ${FSRC} | \
	    awk '{ if ($$1 != prev) { if (rec != "") print rec; \
		rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
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

#pxl.o:
#	${CC} ${CFLAGS} -c -DPXLPATH=\"${PXLPATH}\" pxl.c

# font.o needs to know where to find the font description file
font.o:
	${CC} ${CFLAGS} -c -DCONFFILE=\"${CONFFILE}\" font.c

# special rules for font objects
${FOBJ}:
	${CC} ${FCFLAGS} -c $*.c

# DO NOT DELETE THIS LINE -- make depend uses it

conv.c:
conv.o: conv.c ../h/types.h ../h/conv.h
dviclass.c:
dviclass.o: dviclass.c ../h/dviclass.h
error.c:
error.o: error.c /usr/include/stdio.h /usr/include/varargs.h
findpost.c:
findpost.o: findpost.c /usr/include/stdio.h ../h/types.h ../h/dvicodes.h
findpost.o: ../h/fio.h
fio.c:
fio.o: fio.c /usr/include/stdio.h ../h/types.h ../h/fio.h
font.c:
font.o: font.c /usr/include/stdio.h /usr/include/errno.h
font.o: /usr/include/sys/errno.h ../h/types.h ../h/conv.h ../h/font.h
font_subr.c:
font_subr.o: font_subr.c ../h/font.h ../h/types.h
gfclass.c:
gfclass.o: gfclass.c ../h/gfclass.h
gripes.c:
gripes.o: gripes.c /usr/include/stdio.h ../h/types.h
magfactor.c:
magfactor.o: magfactor.c
rotate.c:
rotate.o: rotate.c ../h/font.h ../h/types.h
scaletfm.c:
scaletfm.o: scaletfm.c ../h/types.h ../h/font.h
scanpost.c:
scanpost.o: scanpost.c /usr/include/stdio.h ../h/types.h ../h/dvicodes.h
scanpost.o: ../h/fio.h ../h/postamble.h
search.c:
search.o: search.c ../h/types.h ../h/search.h
seek.c:
seek.o: seek.c /usr/include/stdio.h /usr/include/sys/param.h
seek.o: /usr/include/machine/param.h /usr/include/signal.h
seek.o: /usr/include/sys/types.h /usr/include/sys/sysmacros.h
seek.o: /usr/include/sys/file.h /usr/include/sys/fcntl.h
seek.o: /usr/include/sys/stat.h
split.c:
split.o: split.c /usr/include/ctype.h
strsave.c:
strsave.o: strsave.c
tfm.c:
tfm.o: tfm.c /usr/include/stdio.h ../h/types.h ../h/fio.h ../h/tfm.h
gffont.c:
gffont.o: gffont.c /usr/include/stdio.h /usr/include/sys/types.h
gffont.o: /usr/include/sys/sysmacros.h /usr/include/sys/stat.h ../h/types.h
gffont.o: ../h/font.h ../h/gfcodes.h ../h/gfclass.h ../h/num.h
pkfont.c:
pkfont.o: pkfont.c /usr/include/stdio.h /usr/include/sys/types.h
pkfont.o: /usr/include/sys/sysmacros.h /usr/include/sys/stat.h ../h/types.h
pkfont.o: ../h/font.h ../h/num.h
pxlfont.c:
pxlfont.o: pxlfont.c /usr/include/stdio.h /usr/include/sys/types.h
pxlfont.o: /usr/include/sys/sysmacros.h /usr/include/sys/stat.h
pxlfont.o: /usr/include/errno.h /usr/include/sys/errno.h ../h/types.h
pxlfont.o: ../h/font.h ../h/fio.h
tfmfont.c:
tfmfont.o: tfmfont.c /usr/include/stdio.h /usr/include/sys/types.h
tfmfont.o: /usr/include/sys/sysmacros.h /usr/include/sys/stat.h ../h/types.h
tfmfont.o: ../h/conv.h ../h/font.h ../h/tfm.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
# DO NOT DELETE THIS LINE -- make depend depends on it.
