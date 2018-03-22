#   Makefile for initex, as part of
#   IncTeX - An Editor Independent Incremental TeX Formatter
# 
# Copyright (c) 1992 The Regents of the University of California.
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

#If your make system doesn't take the VPATH variable, you can do
#ln -s ../*.o .
#and remove the VPATH definition
VPATH         = ..

DEST	      = /usr/local/bin

MANDIR	      = /usr/man/manl

MANUAL	      =

HDRS	      = Imain.h \
                Icmp.h \
                Icmpdef.h \
                Idebug.h \
                Inewdef.h \
		align.h \
		arith.h \
		box.h \
		boxlists.h \
		char.h \
		cmds.h \
		cond.h \
		def.h \
		dvi.h \
		eq.h \
		eqstack.h \
		error.h \
		eval.h \
		evalstack.h \
		expand.h \
		file.h \
		fmt.h \
		hash.h \
		heap.h \
		hyph.h \
		io.h \
		math.h \
		mathlists.h \
		mlst-hlst.h \
		pack.h \
		page.h \
		par.h \
		print.h \
		scan.h \
		str.h \
		tex.h \
		texext.h \
		tfm.h \
		token.h \
		tokenlists.h \
		tokenstack.h

CFLAGS	      = -DINCTEX -DINIT
#CFLAGS	      = -O -DINCTEX -DINIT

LDFLAGS	      = $(CFLAGS)

LIBS	      =

LINKER	      = cc

MAKEFILE      = Makefile

OBJS	      = Iglobal.o \
		Imain.o \
		Istate.o \
                Icmp.o \
                Iparse.o \
                Idebug.o \
		align.o \
		arith.o \
		box.o \
		boxlists.o \
		char.o \
		cmds.o \
		cond.o \
		def.o \
		dvi.o \
		eq.o \
		eqstack.o \
		error.o \
		eval.o \
		evalstack.o \
		expand.o \
		file.o \
		fmt.o \
		hash.o \
		heap.o \
		hyph.o \
		io.o \
		math.o \
		mathlists.o \
		mlst-hlst.o \
		pack.o \
		page.o \
		par.o \
		print.o \
		scan.o \
		str.o \
		tex.o \
		texext.o \
		tfm.o \
		token.o \
		tokenlists.o \
		tokenstack.o

PRINT	      = psgrind -h

PROGRAM	      = initex

UNDUMP	      = undump

FORMAT	      = -b \&lplain

FORMATTER     = tex

CORE	      = core

SRCS	      = Iglobal.c \
		Imain.c \
		Istate.c \
                Icmp.c \
                Iparse.c \
                Idebug.c \
		align.c \
		arith.c \
		box.c \
		boxlists.c \
		char.c \
		cmds.c \
		cond.c \
		def.c \
		dvi.c \
		eq.c \
		eqstack.c \
		error.c \
		eval.c \
		evalstack.c \
		expand.c \
		file.c \
		fmt.c \
		hash.c \
		heap.c \
		hyph.c \
		io.c \
		math.c \
		mathlists.c \
		mlst-hlst.c \
		pack.c \
		page.c \
		par.c \
		print.c \
		scan.c \
		str.c \
		tex.c \
		texext.c \
		tfm.c \
		token.c \
		tokenlists.c \
		tokenstack.c

DIFF	      = cmds.c, \
		def.c, \
		eq.c, \
		fmt.c, \
		hash.c, \
		heap.c, \
		hyph.c, \
		scan.c, \
		tex.c, \
		token.c \
		Imain.c

#.IGNORE:;

$(PROGRAM):     $(OBJS)
		@rm -f $(PROGRAM)
		$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@size $(PROGRAM)

dist-checkout:
		co README empty
		empty

#For some reason, this doesn't work from Make
touch:
		touch $(VPATH)/{$(DIFF)}

#flagfirst is just used to tell if we've ever done a make before
#if not, we need to compile DIFF files from scratch
flagfirst:
		echo '' > flagfirst
Imain.o:	flagfirst
cmds.o:		flagfirst
def.o:		flagfirst
eq.o:		flagfirst
fmt.o:		flagfirst
hash.o:		flagfirst
heap.o:		flagfirst
hyph.o:		flagfirst
scan.o:		flagfirst
tex.o:		flagfirst
token.o:	flagfirst

dump:		$(PROGRAM)
		$(PROGRAM) $(FORMAT)

$(FORMATTER):	$(PROGRAM) $(CORE)
		$(UNDUMP) $(FORMATTER) $(PROGRAM) $(CORE)

install:	$(FORMATTER)
		install -c -s -m 0755 $(FORMATTER) $(DEST)
		@ls -lgs $(DEST)/$(FORMATTER)

clean:;		@rm -f $(OBJS) core $(PROGRAM) $(FORMATTER) *.out
		@empty

depend:;	@rm -f .#*.[chly]
		mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

index:;		@ctags -wx $(HDRS) $(SRCS)

print:;		@$(PRINT) $(HDRS) $(SRCS)

program:        $(PROGRAM)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM): $(SRCS) $(HDRS)
		@make -f $(MAKEFILE) DEST=$(DEST) install

###
Iparse.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h texext.h tfm.h math.h box.h token.h tokenlists.h Imain.h
Iglobal.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h \
	hash.h arith.h Imain.h texext.h scan.h evalstack.h eqstack.h \
	tokenstack.h token.h box.h pack.h math.h cond.h file.h tfm.h hyph.h \
	dvi.h fmt.h page.h Imain.h
Imain.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h Imain.h file.h scan.h dvi.h tokenstack.h texext.h eqstack.h \
	cond.h Imain.h
Istate.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h Imain.h file.h dvi.h eqstack.h cond.h box.h texext.h tfm.h \
	tokenstack.h token.h evalstack.h Idebug.h Imain.h Icmp.h
Icmp.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h Imain.h file.h dvi.h eqstack.h cond.h box.h texext.h tfm.h \
	tokenstack.h token.h evalstack.h Idebug.h Imain.h Inewdef.h Icmpdef.h \
	Icmp.h
Idebug.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h Imain.h file.h dvi.h eqstack.h cond.h box.h texext.h tfm.h \
	tokenstack.h token.h evalstack.h Idebug.h Imain.h Inewdef.h
align.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h eqstack.h token.h tokenstack.h scan.h evalstack.h box.h \
	pack.h math.h mlst-hlst.h align.h
arith.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h
box.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h texext.h tfm.h math.h box.h token.h tokenlists.h Imain.h
boxlists.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h \
	hash.h arith.h token.h tokenstack.h tokenlists.h eqstack.h \
	evalstack.h box.h scan.h def.h tfm.h pack.h page.h math.h boxlists.h \
	Imain.h
char.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h
cmds.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h texext.h eqstack.h token.h tokenlists.h tokenstack.h scan.h \
	evalstack.h def.h cond.h expand.h box.h boxlists.h tfm.h math.h \
	mathlists.h align.h
cond.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h box.h eqstack.h token.h tokenlists.h scan.h tokenstack.h \
	evalstack.h file.h cond.h
def.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h eqstack.h evalstack.h token.h scan.h tokenstack.h expand.h \
	box.h boxlists.h tokenlists.h file.h tfm.h dvi.h page.h def.h Imain.h
dvi.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h texext.h box.h token.h scan.h tfm.h file.h pack.h dvi.h \
	Imain.h
eq.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h token.h box.h tfm.h
eqstack.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h \
	hash.h arith.h token.h tokenstack.h tokenlists.h eqstack.h Imain.h
error.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h tokenstack.h token.h file.h Imain.h
eval.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h tfm.h eqstack.h token.h scan.h tokenstack.h evalstack.h box.h \
	boxlists.h math.h mathlists.h cond.h def.h dvi.h pack.h page.h par.h \
	eval.h Imain.h
evalstack.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h \
	hash.h arith.h box.h page.h tokenstack.h evalstack.h
expand.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h box.h tokenstack.h scan.h token.h tokenlists.h cond.h file.h \
	expand.h
file.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h token.h scan.h tokenstack.h fmt.h file.h Imain.h
fmt.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h texext.h token.h box.h eqstack.h file.h tfm.h hyph.h fmt.h \
	Imain.h
hash.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h box.h scan.h math.h boxlists.h Imain.h
heap.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h box.h evalstack.h par.h page.h
hyph.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h token.h tfm.h box.h scan.h tokenstack.h par.h hyph.h
io.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h tokenstack.h file.h scan.h Imain.h
math.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h scan.h evalstack.h box.h tfm.h pack.h math.h
mathlists.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h \
	hash.h arith.h eqstack.h evalstack.h token.h tokenstack.h scan.h \
	tfm.h box.h pack.h math.h mlst-hlst.h par.h page.h mathlists.h
mlst-hlst.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h \
	hash.h arith.h scan.h box.h math.h pack.h tfm.h mlst-hlst.h
pack.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h scan.h tokenstack.h eqstack.h evalstack.h box.h tfm.h dvi.h \
	pack.h
page.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h token.h tokenstack.h tokenlists.h eqstack.h evalstack.h \
	scan.h expand.h box.h pack.h par.h math.h dvi.h page.h Imain.h
par.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h tfm.h tokenstack.h evalstack.h box.h pack.h hyph.h par.h
print.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h texext.h
scan.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h token.h tokenstack.h tokenlists.h evalstack.h box.h tfm.h \
	dvi.h page.h scan.h
str.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h
tex.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h texext.h evalstack.h eqstack.h tokenstack.h token.h box.h \
	pack.h cond.h file.h tfm.h hyph.h dvi.h fmt.h page.h Imain.h
texext.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h token.h box.h scan.h def.h tokenstack.h tokenlists.h \
	evalstack.h file.h dvi.h texext.h Imain.h
tfm.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h box.h scan.h token.h tokenstack.h file.h tfm.h Imain.h
token.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h hash.h \
	arith.h scan.h box.h cond.h expand.h align.h tokenstack.h token.h
tokenlists.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h \
	hash.h arith.h box.h token.h tokenstack.h scan.h expand.h def.h \
	file.h tfm.h tokenlists.h
tokenstack.o: tex.h cmds.h char.h str.h io.h print.h error.h heap.h eq.h \
	hash.h arith.h box.h def.h token.h tokenlists.h Imain.h tokenstack.h
