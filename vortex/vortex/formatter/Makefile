# Copyright (c) 1986-1987 The Regents of the University of California.
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
# VorTeX Formatter Makefile
#
# Copyright (C) 1987	by  Pehong Chen (phc@berkeley.edu)
# Computer Science Division
# University of California, Berkeley
#

DEST	      = ../bin

MANDIR	      = /usr/man/manl

MANUAL	      =

HDRS	      = align.h \
		arith.h \
		bits.h \
		box.h \
		boxlists.h \
		char.h \
		cmds.h \
		comm.h \
		cond.h \
		def.h \
		dvi.h \
		eq.h \
		eqstack.h \
		error.h \
		eval.h \
		evalstack.h \
		expand.h \
		failcodes.h \
		file.h \
		fmt.h \
		hash.h \
		heap.h \
		hyph.h \
		io.h \
		irv.h \
		macro.h \
		main.h \
		math.h \
		mathlists.h \
		mdep.h \
		mlist-hlist.h \
		msg.h \
		pack.h \
		page.h \
		par.h \
		print.h \
		protocol.h \
		scan.h \
		state.h \
		str.h \
		tex.h \
		texext.h \
		tfm.h \
		token.h \
		tokenlists.h \
		tokenstack.h \
		tpfdecls.h \
		tsfdecls.h \
		var.h

68010CFLAGS   = -m68010 -L/usr.MC68010/lib -Qpath /usr.MC68010/cstart

CFLAGS	      = -I../h  -DVORTEX -DTIME
#CFLAGS	      = -I../h -O $(68010CFLAGS)

LDFLAGS	      = $(CFLAGS)

LIBS	      =

LINKER	      = cc

MAKEFILE      = Makefile

OBJS	      = align.o \
		arith.o \
		box.o \
		boxlists.o \
		char.o \
		cmds.o \
		cond.o \
		conn.o \
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
		global_sv.o \
		global_un.o \
		hash.o \
		heap.o \
		hyph.o \
		inc.o \
		io.o \
		irf.o \
		iri.o \
		irl.o \
		irp.o \
		irs.o \
		irt.o \
		irv.o \
		main.o \
		math.o \
		mathlists.o \
		mkconn.o \
		mlist-hlist.o \
		msg.o \
		pack.o \
		page.o \
		par.o \
		print.o \
		scan.o \
		send.o \
		state.o \
		str.o \
		test.o \
		tex.o \
		texext.o \
		tfm.o \
		token.o \
		tokenlists.o \
		tokenstack.o \
		tpcmds.o \
		tpconn.o \
		tscmds.o \
		tsconn.o

PRINT	      = psgrind -h

PROGRAM	      = virtex

UNDUMP	      = undump

FORMAT	      = -t \&plain

FORMATTER     = tex

CORE	      = core

SRCS	      = align.c \
		arith.c \
		box.c \
		boxlists.c \
		char.c \
		cmds.c \
		cond.c \
		conn.c \
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
		global_sv.c \
		global_un.c \
		hash.c \
		heap.c \
		hyph.c \
		inc.c \
		io.c \
		irf.c \
		iri.c \
		irl.c \
		irp.c \
		irs.c \
		irt.c \
		irv.c \
		main.c \
		math.c \
		mathlists.c \
		mkconn.c \
		mlist-hlist.c \
		msg.c \
		pack.c \
		page.c \
		par.c \
		print.c \
		scan.c \
		send.c \
		state.c \
		str.c \
		test.c \
		tex.c \
		texext.c \
		texincl.c \
		tfm.c \
		token.c \
		tokenlists.c \
		tokenstack.c \
		tpcmds.c \
		tpconn.c \
		tscmds.c \
		tsconn.c

#.IGNORE:;

$(PROGRAM):     $(OBJS)
		@rm -f $(PROGRAM)
		$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@size $(PROGRAM)

dump:		$(PROGRAM)
		$(PROGRAM) $(FORMAT)

$(FORMATTER):	$(PROGRAM) $(CORE)
		$(UNDUMP) $(FORMATTER) $(PROGRAM) $(CORE)

install:	$(FORMATTER)
		install -c -s -m 0755 $(FORMATTER) $(DEST)
		@ls -lgs $(DEST)/$(FORMATTER)

clean:;		@rm -f $(OBJS) core $(PROGRAM) $(FORMATTER) *.out

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
align.o: tex.h cmds.h heap.h arith.h eq.h eqstack.h hash.h token.h \
	tokenstack.h scan.h evalstack.h box.h pack.h math.h mlist-hlist.h \
	error.h align.h
arith.o: tex.h print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h \
	arith.h
box.o: tex.h texext.h arith.h heap.h char.h str.h eq.h hash.h tfm.h print.h \
	../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h math.h box.h \
	token.h tokenlists.h
boxlists.o: tex.h cmds.h heap.h eq.h eqstack.h def.h box.h tokenstack.h \
	token.h scan.h tokenlists.h evalstack.h tfm.h pack.h page.h math.h \
	io.h print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h \
	error.h boxlists.h
char.o: tex.h char.h
cmds.o: tex.h texext.h heap.h eq.h eqstack.h str.h hash.h token.h \
	tokenlists.h tokenstack.h scan.h evalstack.h def.h cond.h expand.h \
	box.h boxlists.h tfm.h math.h mathlists.h align.h error.h print.h \
	../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h cmds.h
cond.o: tex.h cmds.h heap.h box.h eq.h eqstack.h hash.h token.h \
	tokenlists.h scan.h tokenstack.h evalstack.h file.h print.h \
	../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h error.h cond.h \
	var.h
conn.o: main.h failcodes.h msg.h mdep.h comm.h ../h/gl_comm.h \
	../h/ts_comm.h ../h/tp_comm.h bits.h
def.o: tex.h cmds.h heap.h io.h eq.h hash.h eqstack.h evalstack.h token.h \
	scan.h tokenstack.h expand.h arith.h str.h box.h boxlists.h \
	tokenlists.h file.h tfm.h dvi.h page.h print.h ../h/gl_comm.h \
	../h/ts_comm.h ../h/allir.h main.h error.h def.h macro.h var.h
dvi.o: tex.h texext.h heap.h str.h io.h eq.h box.h scan.h tfm.h file.h \
	pack.h print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h \
	error.h dvi.h msg.h state.h
eq.o: tex.h cmds.h heap.h char.h token.h hash.h box.h print.h \
	../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h error.h eq.h
eqstack.o: tex.h cmds.h heap.h eq.h token.h tokenstack.h tokenlists.h \
	print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h error.h \
	eqstack.h
error.o: tex.h str.h tokenstack.h token.h eq.h io.h file.h print.h \
	../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h error.h
eval.o: tex.h cmds.h heap.h arith.h char.h tfm.h eq.h eqstack.h hash.h \
	token.h scan.h tokenstack.h evalstack.h box.h boxlists.h math.h \
	mathlists.h cond.h def.h dvi.h pack.h page.h par.h print.h \
	../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h error.h eval.h \
	msg.h macro.h texext.h var.h
evalstack.o: tex.h cmds.h heap.h str.h arith.h tokenstack.h eq.h box.h \
	page.h print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h \
	error.h evalstack.h
expand.o: tex.h cmds.h heap.h io.h eq.h hash.h box.h tokenstack.h scan.h \
	token.h tokenlists.h cond.h file.h print.h ../h/gl_comm.h \
	../h/ts_comm.h ../h/allir.h main.h error.h expand.h macro.h var.h \
	msg.h
file.o: tex.h cmds.h heap.h char.h eq.h token.h scan.h tokenstack.h str.h \
	fmt.h io.h print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h \
	error.h file.h state.h macro.h msg.h var.h
fmt.o: tex.h texext.h heap.h token.h eq.h box.h eqstack.h hash.h file.h \
	tfm.h str.h hyph.h io.h print.h ../h/gl_comm.h ../h/ts_comm.h \
	../h/allir.h main.h error.h fmt.h
global_sv.o: tex.h texext.h cmds.h heap.h char.h str.h eq.h hash.h scan.h \
	evalstack.h eqstack.h tokenstack.h token.h box.h pack.h math.h cond.h \
	io.h file.h tfm.h hyph.h dvi.h fmt.h error.h print.h ../h/gl_comm.h \
	../h/ts_comm.h ../h/allir.h main.h page.h macro.h
global_un.o: tex.h texext.h cmds.h heap.h char.h str.h eq.h hash.h scan.h \
	evalstack.h eqstack.h tokenstack.h token.h box.h pack.h math.h cond.h \
	io.h file.h tfm.h hyph.h dvi.h fmt.h error.h print.h ../h/gl_comm.h \
	../h/ts_comm.h ../h/allir.h main.h page.h macro.h state.h
hash.o: tex.h cmds.h heap.h box.h scan.h eq.h io.h math.h boxlists.h str.h \
	error.h hash.h
heap.o: tex.h eq.h arith.h box.h evalstack.h par.h page.h print.h \
	../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h error.h heap.h
hyph.o: tex.h cmds.h heap.h token.h eq.h str.h tfm.h box.h scan.h \
	tokenstack.h par.h print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h \
	main.h error.h hyph.h
inc.o: tex.h str.h dvi.h scan.h ../h/allir.h msg.h main.h
io.o: tex.h char.h str.h tokenstack.h print.h ../h/gl_comm.h ../h/ts_comm.h \
	../h/allir.h main.h file.h io.h
irf.o: tex.h ../h/allir.h main.h msg.h ../h/ts_comm.h
iri.o: tex.h cmds.h heap.h arith.h char.h tfm.h eq.h eqstack.h hash.h \
	token.h scan.h tokenstack.h evalstack.h box.h boxlists.h math.h \
	mathlists.h cond.h def.h dvi.h pack.h page.h par.h print.h \
	../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h error.h eval.h \
	msg.h macro.h texext.h var.h
irl.o: tex.h scan.h box.h heap.h tokenstack.h dvi.h tfm.h eq.h ../h/allir.h \
	main.h msg.h
irp.o: ../h/allir.h main.h msg.h comm.h ../h/gl_comm.h ../h/ts_comm.h \
	../h/tp_comm.h
irs.o: tex.h cmds.h heap.h eq.h hash.h scan.h io.h char.h box.h cond.h \
	print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h error.h \
	expand.h align.h tokenstack.h token.h macro.h var.h msg.h
irt.o: tex.h scan.h box.h heap.h tokenstack.h dvi.h tfm.h eq.h str.h \
	texext.h ../h/allir.h main.h msg.h
irv.o: protocol.h tex.h texext.h cmds.h heap.h char.h str.h eq.h hash.h \
	evalstack.h eqstack.h tokenstack.h token.h box.h pack.h cond.h io.h \
	file.h tfm.h hyph.h dvi.h fmt.h error.h print.h ../h/gl_comm.h \
	../h/ts_comm.h ../h/allir.h main.h page.h macro.h irv.h texincl.c
main.o: ../h/gl_comm.h ../h/ts_comm.h tex.h scan.h main.h failcodes.h msg.h
math.o: tex.h heap.h eq.h scan.h evalstack.h arith.h str.h box.h tfm.h \
	print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h pack.h \
	math.h token.h tokenstack.h
mathlists.o: tex.h cmds.h heap.h arith.h eq.h eqstack.h evalstack.h token.h \
	tokenstack.h scan.h tfm.h box.h pack.h math.h mlist-hlist.h par.h \
	page.h print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h \
	error.h mathlists.h macro.h var.h msg.h
mkconn.o: comm.h ../h/gl_comm.h ../h/ts_comm.h ../h/tp_comm.h mdep.h \
	failcodes.h msg.h
mlist-hlist.o: tex.h heap.h arith.h scan.h eq.h box.h math.h pack.h tfm.h \
	print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h error.h \
	str.h mlist-hlist.h
msg.o: msg.h failcodes.h
pack.o: tex.h heap.h arith.h scan.h tokenstack.h eq.h eqstack.h evalstack.h \
	box.h tfm.h dvi.h print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h \
	main.h error.h pack.h
page.o: tex.h heap.h arith.h str.h token.h tokenstack.h tokenlists.h eq.h \
	eqstack.h evalstack.h scan.h expand.h box.h pack.h par.h math.h dvi.h \
	print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h error.h \
	page.h
par.o: tex.h heap.h arith.h eq.h tfm.h str.h tokenstack.h evalstack.h box.h \
	pack.h hyph.h print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h \
	main.h error.h par.h
print.o: tex.h texext.h eq.h char.h str.h io.h print.h ../h/gl_comm.h \
	../h/ts_comm.h ../h/allir.h main.h
scan.o: tex.h cmds.h heap.h arith.h eq.h token.h tokenstack.h tokenlists.h \
	evalstack.h char.h str.h box.h tfm.h dvi.h print.h ../h/gl_comm.h \
	../h/ts_comm.h ../h/allir.h main.h error.h page.h scan.h var.h
send.o: tex.h dvi.h tfm.h str.h eq.h ../h/allir.h ../h/tp_comm.h comm.h \
	../h/gl_comm.h ../h/ts_comm.h main.h msg.h
state.o: tex.h file.h msg.h io.h dvi.h heap.h eq.h eqstack.h cond.h box.h \
	texext.h tfm.h tokenstack.h token.h evalstack.h var.h hash.h main.h \
	macro.h state.h
str.o: tex.h io.h file.h error.h str.h
tex.o: tex.h texext.h cmds.h heap.h char.h str.h eq.h hash.h evalstack.h \
	eqstack.h tokenstack.h token.h box.h pack.h cond.h io.h file.h tfm.h \
	hyph.h dvi.h fmt.h error.h print.h ../h/gl_comm.h ../h/ts_comm.h \
	../h/allir.h main.h page.h scan.h macro.h var.h
texext.o: tex.h cmds.h heap.h eq.h hash.h token.h box.h scan.h def.h \
	tokenstack.h tokenlists.h evalstack.h io.h str.h file.h dvi.h print.h \
	../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h error.h texext.h
tfm.o: tex.h cmds.h heap.h arith.h eq.h hash.h box.h scan.h token.h \
	tokenstack.h str.h io.h file.h print.h ../h/gl_comm.h ../h/ts_comm.h \
	../h/allir.h main.h error.h tfm.h
token.o: tex.h cmds.h heap.h eq.h hash.h scan.h io.h char.h box.h cond.h \
	print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h main.h error.h \
	expand.h align.h tokenstack.h token.h macro.h var.h msg.h
tokenlists.o: tex.h cmds.h heap.h eq.h hash.h str.h box.h token.h expand.h \
	tokenstack.h io.h scan.h def.h file.h tfm.h print.h ../h/gl_comm.h \
	../h/ts_comm.h ../h/allir.h main.h error.h tokenlists.h macro.h var.h
tokenstack.o: tex.h cmds.h heap.h eq.h def.h char.h io.h token.h \
	tokenlists.h box.h print.h ../h/gl_comm.h ../h/ts_comm.h ../h/allir.h \
	main.h error.h tokenstack.h macro.h var.h
tpcmds.o: msg.h failcodes.h comm.h ../h/gl_comm.h ../h/ts_comm.h \
	../h/tp_comm.h tpfdecls.h
tpconn.o: mdep.h main.h comm.h ../h/gl_comm.h ../h/ts_comm.h ../h/tp_comm.h \
	../h/allir.h msg.h state.h tex.h file.h io.h str.h
tscmds.o: msg.h failcodes.h comm.h ../h/gl_comm.h ../h/ts_comm.h \
	../h/tp_comm.h tsfdecls.h
tsconn.o: tex.h scan.h dvi.h comm.h ../h/gl_comm.h ../h/ts_comm.h \
	../h/tp_comm.h msg.h ../h/allir.h main.h mdep.h bits.h
