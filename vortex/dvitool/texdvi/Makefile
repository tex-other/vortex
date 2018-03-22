# Copyright (c) 1986-1991 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
# 
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#
#  Jeff Mc Carrell.  Adopted from John Coker.
#  University of California, Berkeley
#

DEST	      = /usr/sww/bin

PROGRAM	      = texdvi

LPROGRAM      = latexdvi

SPROGRAM      = slitexdvi

MANDIR	      = /usr/sww/man/man1

MANUAL	      = texdvi.1

HDRS	      = texdvi.h

CFLAGS	      = -O -DTEXDVI 

IFLAGS	      =

LDFLAGS	      = $(CFLAGS)

LIBS	      =

LINKER	      = cc

MAKEFILE      = Makefile

OBJS	      = texdvi.o

SRCS	      = texdvi.c

DISTRIB	      = $(MAKEFILE) $(SRCS) $(HDRS) $(MANUAL)

$(PROGRAM):     $(OBJS)
		@rm -f $(PROGRAM) core
		@echo "Linking \`$(PROGRAM)' ..."
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)

install:	$(DEST)/$(PROGRAM) $(MANDIR)/$(MANUAL)
		@echo "Installation completed."

clean:;		@rm -f $(OBJS) $(PROGRAM) core *.out .#* *~

update:		$(DEST)/$(PROGRAM)

manual:		$(MANDIR)/$(MANUAL)

$(DEST)/$(PROGRAM): $(PROGRAM)
		install $(IFLAGS) -c -s -m 0755 $(PROGRAM) $(DEST)
		@rm -f $(DEST)/$(LPROGRAM) $(DEST)/$(SPROGRAM)
		@ln -s $(DEST)/$(PROGRAM) $(DEST)/$(LPROGRAM)
		@ln -s $(DEST)/$(PROGRAM) $(DEST)/$(SPROGRAM)
		@ls -lgs $(DEST)/$(PROGRAM) $(DEST)/$(LPROGRAM) $(DEST)/$(SPROGRAM)

$(MANDIR)/$(MANUAL): $(MANUAL)
		install $(IFLAGS) -c -m 0644 $(MANUAL) $(MANDIR)
		@ls -lgs $(MANDIR)/$(MANUAL)

checkout:
		co $(SRCS) $(HDRS) $(MANUAL)

dist-checkout: checkout

###
texdvi.o: texdvi.c texdvi.h
