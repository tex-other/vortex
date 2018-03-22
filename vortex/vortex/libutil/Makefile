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
#  Makefile for general utility library
#  April 18, 1986
#

DEST	      = ../lib

LIBRARY	      = libutil.a

MANDIR	      =

MANUAL	      =

EXTHDRS	      =

HDRS	      =

CFLAGS	      = -O -I. -I../h

IFLAGS	      = -m ug+w -g tex -c

LDFLAGS	      = $(CFLAGS)

LIBS	      =

LINKER	      = cc

AR	      = ar

MAKEFILE      = Makefile

OBJS	      = compl.o

PRINT	      = tgrind -Pgp

SRCS	      = compl.c

$(LIBRARY):     $(OBJS)
		@rm -f $(LIBRARY)
		@echo "Creating \`$(LIBRARY)' ..."
		@$(AR) cr $(LIBRARY) $(OBJS)
		@ranlib $(LIBRARY)
		@ls -lgs $(LIBRARY)

install::	$(DEST)/$(LIBRARY)

clean:;		@rm -f $(OBJS) $(LIBRARY) core *.out .#* $(SHAR) $(TAR)

depend:;	@rm -f .#*
		@mkmf -l -f $(MAKEFILE) LIBRARY=$(LIBRARY) DEST=$(DEST)

index:;		@ctags -wx $(HDRS) $(SRCS)

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:;          @ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(LIBRARY)

$(DEST)/$(LIBRARY): $(LIBRARY)
		install $(IFLAGS) $(LIBRARY) $(DEST)
		@ranlib $(DEST)/$(LIBRARY)
		@ls -lgs $(DEST)/$(LIBRARY)

.DEFAULT:;	co $@
###
