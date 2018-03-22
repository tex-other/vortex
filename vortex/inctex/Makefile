#   Makefile for virtex, as part of
#
#   IncTeX
#   An Editor-Independent, Adaptive, Incremental TeX Formatter
# 
#   Copyright (C) 1992 by Regents of the University of California
#   Authors:
# 	Derluen Pan
# 	Ikuo Minata
# 	Pehong Chen
#   in association with
# 	Prof. Michael A. Harrison
#	Univ. of California, Berkeley
# 
#   All rights reserved by the copyright holders.  See the copyright
#   notices distributed with this software for a complete description of
#   the conditions under which it is made available.

#FLAGS	      = -DINCTEX -DDEBUG
#FLAGS	      = -DINCTEX -DDEBUG -DCHECKSTATE
FLAGS	      = -DINCTEX -DCHECKSTATE
#FLAGS	      = -DINCTEX

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

#CFLAGS	      = -g $(FLAGS)
CFLAGS	      = -O $(FLAGS)
#CFLAGS	      = -O -temp=./tmp $(FLAGS)

LDFLAGS	      = $(CFLAGS)

LINTFLAGS     = $(FLAGS)

LIBS	      =

CC	      = cc
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

PROGRAM	      = virtex

UNDUMP	      = undump

TFORMAT	      = -b \&plain
LFORMAT	      = -b \&lplain

TFORMATTER    = tex
LFORMATTER    = latex

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

ALLSRCS	      = $(SRCS)

DIFF	      = Iglobal.c \
		Imain.c \
		Istate.c \
		Icmp.c \
		Iparse.c \
		Idebug.c \
		boxlists.c \
		def.c \
		dvi.c \
		eqstack.c \
		error.c \
		eval.c \
		file.c \
		fmt.c \
		io.c \
		hash.c \
		page.c \
		print.c \
		tex.c \
		tfm.c \
		tokenstack.c \
		Imain.h \
		eq.h \
		tex.h

DEP	      = Iglobal.c \
		Imain.c \
		Istate.c \
		Icmp.c \
		Iparse.c \
		Idebug.c \
		boxlists.c \
		def.c \
		dvi.c \
		eqstack.c \
		error.c \
		eval.c \
		file.c \
		fmt.c \
		io.c \
		hash.c \
		page.c \
		print.c \
		tex.c \
		texext.c \
		tfm.c \
		tokenstack.c

#.IGNORE:;

$(PROGRAM):     $(OBJS)
		@rm -f $(PROGRAM)
		$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@size $(PROGRAM)

dist-checkout:
		co $(HDRS) $(SRCS) README.INCTEX INFO COPYING COPYRIGHT \
			build install lplain.tex plain.tex
		@echo 'cd INIT' > temp
		@echo 'co Makefile' >> temp
		@echo 'make dist-checkout' >> temp
		@echo 'cd ../bin' >> temp
		@echo 'co dvicompare itex ilatex README vline' >> temp
		@echo 'cd merge/dvi' >> temp
		@echo 'co Makefile' >> temp
		@echo 'make dist-checkout' >> temp
		@echo 'cd ../../..' >> temp
		csh -f temp
		@rm -f temp
# assumes files in INIT and merge/{lib,h} don't change

saber:
		#load -G $(OBJS) $(LIBS)

touch:
		touch $(DIFF)

dep:
		touch INIT/*.o
		touch *.o
		touch $(DEP)

shar:
		rm -rf A.shar B.shar C.shar D.shar
		shar A.shar Imain.c Imain.h Iglobal.c Istate.c
		shar B.shar def.c dvi.c error.c
		shar C.shar eval.c file.c io.c
		shar D.shar tex.c tex.h texext.c tokenstack.c tokenstack.h Makefile

#Note: the following undumped versions are no longer used.  With a fast CPU
#it is more efficient just to run through the initialization process instead
#of saving an initialized memory image.

#core for undumping tex
tdump:		$(PROGRAM)
		-./$(PROGRAM) $(TFORMAT)

$(TFORMATTER):	$(PROGRAM) $(CORE)
		$(UNDUMP) $(TFORMATTER) $(PROGRAM) $(CORE)
		\rm -f $(CORE)

#core for undumping latex
ldump:		$(PROGRAM)
		-./$(PROGRAM) $(LFORMAT)

$(LFORMATTER):	$(PROGRAM) $(CORE)
		$(UNDUMP) $(LFORMATTER) $(PROGRAM) $(CORE)
		\rm -f $(CORE)

install:	$(TFORMATTER) $(LFORMATTER)
		install -c -s -m 0755 $(TFORMATTER) $(DEST)
		install -c -s -m 0755 $(LFORMATTER) $(DEST)
		@ls -lgs $(DEST)/$(TFORMATTER) $(DEST)/$(LFORMATTER)

clean:;		@rm -f $(OBJS) core $(PROGRAM) $(TFORMATTER) $(LFORMATTER) *.out

depend:;	@rm -f .#*.[chly]
		mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

index:;		@ctags -wx $(HDRS) $(ALLSRCS)

print:;		@$(PRINT) $(HDRS) $(ALLSRCS)

program:        $(PROGRAM)

tags:           $(HDRS) $(ALLSRCS); @ctags -t $(HDRS) $(ALLSRCS)

lint:           $(HDRS) $(SRCS); lint $(LINTFLAGS) $(SRCS) > lint.out

update:		$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM): $(ALLSRCS) $(HDRS)
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
