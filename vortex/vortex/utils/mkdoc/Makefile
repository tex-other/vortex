# Copyright (c) 1986 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
# 
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#
#  VorTeX -- Visually ORiented TeX
#
#  Peehong Chen, John Coker, Jeff McCarrell, Steve Procter
#  for Prof. Michael Harrison of the Computer Science Division
#  University of California, Berkeley
#
#  Makefile for documentation string preparer
#

DEST	      = ../../bin

LIBDIR	      = ../../lib

PROGRAM	      = mkdoc

MACROS	      = docmac.tex

EXTHDRS	      = /usr/include/ctype.h \
		/usr/include/stdio.h

HDRS	      = docstr.h \
		mkdoc.h

CFLAGS	      = -O

IFLAGS	      = -g tex -m ug+rw -c

LDFLAGS	      = $(CFLAGS)

LIBS	      =

LINKER	      = cc

MAKEFILE      = Makefile

OBJS	      = ascii.o \
		ccom.o \
		docstr.o \
		format.o \
		lcom.o \
		main.o \
		pscom.o \
		tex.o \
		troff.o \
		util.o

PRINT	      = tgrind -Pgp

SRCS	      = ascii.c \
		ccom.c \
		docstr.c \
		format.c \
		lcom.c \
		main.c \
		pscom.c \
		tex.c \
		troff.c \
		util.c

$(PROGRAM):     $(OBJS)
		@rm -f $(PROGRAM) core
		@echo "Linking \`$(PROGRAM)' ..."
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@size $(PROGRAM)

install:	$(DEST)/$(PROGRAM) $(LIBDIR)/$(MACROS)

clean:;		@rm -f $(OBJS) $(PROGRAM) core *.out .#* $(SHAR) $(TAR)

depend:;	@rm -f .#*
		@mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

index:;		@ctags -wx $(HDRS) $(SRCS)

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:;          @ctags $(HDRS) $(SRCS)

$(DEST)/$(PROGRAM): $(PROGRAM)
		install $(IFLAGS) -s $(PROGRAM) $(DEST)
		@ls -lgs $(DEST)/$(PROGRAM)

$(LIBDIR)/$(MACROS): $(MACROS)
		install $(IFLAGS) $(MACROS) $(LIBDIR)
		@ls -lgs $(LIBDIR)/$(MACROS)

.DEFAULT:;	co $@

.SUFFIXES:;
.SUFFIXES:	.o .c
###
ascii.o: mkdoc.h /usr/include/stdio.h /usr/include/ctype.h docstr.h
ccom.o: mkdoc.h /usr/include/stdio.h /usr/include/ctype.h docstr.h
docstr.o: mkdoc.h /usr/include/stdio.h /usr/include/ctype.h docstr.h
format.o: mkdoc.h /usr/include/stdio.h /usr/include/ctype.h docstr.h
lcom.o: mkdoc.h /usr/include/stdio.h /usr/include/ctype.h docstr.h
main.o: mkdoc.h /usr/include/stdio.h /usr/include/ctype.h docstr.h
pscom.o: mkdoc.h /usr/include/stdio.h /usr/include/ctype.h docstr.h
tex.o: mkdoc.h /usr/include/stdio.h /usr/include/ctype.h docstr.h
troff.o: mkdoc.h /usr/include/stdio.h /usr/include/ctype.h docstr.h
util.o: /usr/include/stdio.h
