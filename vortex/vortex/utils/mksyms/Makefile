# Copyright (c) 1987 The Regents of the University of California.
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
#  RCS Info: $Header$
#
#  VorTeX - Visually Oriented TeX
#  A source-based WYSIWYG editor for beautiful documents
#
#  This file is part of the utility program mksyms which
#  builds the standard symbols for the source editor of VorTeX,
#  written by John Coker under Prof. Michael A. Harrison of
#  the University of California at Berkeley.
#
#  Makefile - makefile for mksyms, exsyms and dlsyms
#
 
DEST	      = ../../bin

ALL	      = exsyms mksyms dlsyms

EXTHDRS	      = ../../source/symtab.h \
		../../source/value.h \
		/usr/include/ctype.h \
		/usr/include/ndbm.h \
		/usr/include/stdio.h \
		/usr/include/sys/file.h

HDRS	      = syms.h

#CFLAGS	      = -I../../source -DFILENAMES -O
CFLAGS	      = -I../../source -O

IFLAGS	      = -g tex -m ug+rw -c -s

LDFLAGS	      = $(CFLAGS)

LIBS	      =

LINKER	      = cc

MAKEFILE      = Makefile

OBJS	      = common.o \
		dlsyms.o \
		exsyms.o \
		mksyms.o

PRINT	      = tgrind -Pgp

SRCS	      = common.c \
		dlsyms.c \
		exsyms.c \
		mksyms.c

all::		$(ALL)

exsyms:		exsyms.o common.o
		@rm -f $@ core
		@echo "Linking \`$@' ..."
		@$(LINKER) $(LDFLAGS) exsyms.o common.o $(LIBS) -o $@
		@size $@

mksyms:		mksyms.o common.o
		@rm -f $@ core
		@echo "Linking \`$@' ..."
		@$(LINKER) $(LDFLAGS) mksyms.o common.o $(LIBS) -o $@
		@size $@

dlsyms:		dlsyms.o common.o
		@rm -f $@ core
		@echo "Linking \`$@' ..."
		@$(LINKER) $(LDFLAGS) dlsyms.o common.o $(LIBS) -o $@
		@size $@

install::	$(DEST)/exsyms $(DEST)/mksyms $(DEST)/dlsyms

clean:;		@rm -f $(OBJS) $(ALL) core *.out .#*

index:;		@ctags -wx $(HDRS) $(SRCS)

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:;          @ctags $(HDRS) $(SRCS)

$(DEST)/exsyms: exsyms
		install $(IFLAGS) exsyms $(DEST)
		@ls -lgs $(DEST)/exsyms

$(DEST)/mksyms: mksyms
		install $(IFLAGS) mksyms $(DEST)
		@ls -lgs $(DEST)/mksyms

$(DEST)/dlsyms: dlsyms
		install $(IFLAGS) dlsyms $(DEST)
		@ls -lgs $(DEST)/dlsyms

.DEFAULT:;	co $@

.SUFFIXES:;
.SUFFIXES:	.o .c
###
common.o: syms.h /usr/include/stdio.h /usr/include/ctype.h \
	/usr/include/ndbm.h /usr/include/sys/file.h ../../source/value.h \
	../../source/symtab.h
exsyms.o: syms.h /usr/include/stdio.h /usr/include/ctype.h \
	/usr/include/ndbm.h /usr/include/sys/file.h ../../source/value.h \
	../../source/symtab.h
mksyms.o: syms.h /usr/include/stdio.h /usr/include/ctype.h \
	/usr/include/ndbm.h /usr/include/sys/file.h ../../source/value.h \
	../../source/symtab.h
dlsyms.o: syms.h /usr/include/stdio.h /usr/include/ctype.h \
	/usr/include/ndbm.h /usr/include/sys/file.h ../../source/value.h \
	../../source/symtab.h
