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
# Generic Makefile
#

DEST	      = /yew2/vortex/bin

EXTHDRS	      = /usr/include/X/X.h \
		/usr/include/X/Xlib.h \
		/usr/include/X/mit-copyright.h \
		/usr/include/net/if.h \
		/usr/include/netdb.h \
		/usr/include/netinet/in.h \
		/usr/include/stdio.h \
		/usr/include/sys/fcntl.h \
		/usr/include/sys/file.h \
		/usr/include/sys/ioctl.h \
		/usr/include/sys/signal.h \
		/usr/include/sys/socket.h \
		/usr/include/sys/sysmacros.h \
		/usr/include/sys/time.h \
		/usr/include/sys/ttychars.h \
		/usr/include/sys/ttydev.h \
		/usr/include/sys/types.h \
		/usr/include/time.h

HOSTS	      = renoir arpa ji eros

HDRS	      = debug.h \
		document.h \
		font.h \
		macros.h \
		pkdefs.h \
		tex_macros.h \
		texfont.h \
		tfm.h \
		vpeselect.h \
		window.h
#socket.h
INCLUDES      = -I. -I../h

CFLAGS	      = $(INCLUDES) -g -DSUNBITS

LIBS	      = -lX -lm

CC	      = cc

MAKEFILE      = Makefile

OBJS	      = command.o \
		debug.o \
		document.o \
		font.o \
		main.o \
		pkfont.o \
		socket.o \
		tex_command.o \
		tex_ir.o \
		vpe_send.o \
		vpe_window.o \
		vse_command.o \
		vse_document.o \
		vse_window.o \
		searchon.o \
		vpeselect.o \
		scrolltoid.o \
		hlt.o \
		window.o

PRINT	      = pr

PROGRAM	      = vpe

SRCS	      = command.c \
		debug.c \
		document.c \
		font.c \
		main.c \
		pkfont.c \
		socket.c \
		tex_command.c \
		tex_ir.c \
		vpe_send.c \
		vpe_window.c \
		vse_command.c \
		vse_document.c \
		vse_window.c \
		searchon.c \
		vpeselect.c \
		scrolltoid.c \
		hlt.c \
		window.c

SOURCES	      =	$(SRCS) $(HDRS)

END           = @echo done

$(PROGRAM):     $(SOURCES) $(OBJS)
		$(CC) $(CFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		$(END)

depend:;	rm -rf \#* .[\#]*
		mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

index:;		ctags -wx $(HDRS) $(SRCS)

install:	
		install -s -m 0755 $(PROGRAM) $(DEST)

print:;		@$(PRINT) $(HDRS) $(SRCS)

program:        $(PROGRAM)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM): $(SRCS) $(LIBS) $(HDRS) $(EXTHDRS)
		@make -f $(MAKEFILE) DEST=$(DEST) install

lint:;		lint -hbxca $(SRCS)

igrind:;	ctags -v $(SRCS) $(HDRS) | sort -f > index
		igrind -x index $(SRCS) $(HDRS)
		rm -f index

lpr:;		$(PRINT) $(HDRS) $(SRCS) | lpr -s -m

clean:;		rm -f $(OBJS) core .#* #*

sources:        $(SOURCES)

checkin:;	echo "Checking in a revision." | ci -l $(SOURCES)

tar:
		csh -c "( rm -f ~/lib/tar/$(PROGRAM).tar ;\
		      tar cf ~/lib/tar/$(PROGRAM).tar ~/src/$(PROGRAM) )"

rdist:;		echo FILES = '($(SOURCES))' > distfile
		echo HOSTS = '($(HOSTS))' >> distfile
		echo '$${FILES} -> $${HOSTS}' >> distfile
		echo '	install' \; >> distfile
		rdist -f distfile
		rm distfile
###
command.o: /usr/include/sys/file.h /usr/include/sys/fcntl.h \
	/usr/include/X/Xlib.h /usr/include/X/mit-copyright.h \
	/usr/include/X/X.h /usr/include/stdio.h
debug.o: /usr/include/stdio.h debug.h
document.o: /usr/include/sys/types.h /usr/include/sys/sysmacros.h \
	/usr/include/X/Xlib.h /usr/include/X/mit-copyright.h \
	/usr/include/X/X.h /usr/include/stdio.h document.h window.h
font.o: /usr/include/sys/types.h /usr/include/sys/sysmacros.h \
	/usr/include/sys/file.h /usr/include/sys/fcntl.h /usr/include/stdio.h \
	font.h
main.o: /usr/include/sys/types.h /usr/include/sys/sysmacros.h \
	/usr/include/X/Xlib.h /usr/include/X/mit-copyright.h \
	/usr/include/X/X.h /usr/include/stdio.h
pkfont.o: /usr/include/sys/types.h /usr/include/sys/sysmacros.h \
	/usr/include/sys/file.h /usr/include/sys/fcntl.h /usr/include/stdio.h \
	pkdefs.h font.h texfont.h tfm.h
socket.o: /usr/include/sys/types.h /usr/include/sys/sysmacros.h \
	/usr/include/sys/file.h /usr/include/sys/fcntl.h \
	/usr/include/sys/socket.h /usr/include/sys/signal.h \
	/usr/include/sys/time.h /usr/include/time.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/netinet/in.h /usr/include/net/if.h /usr/include/netdb.h \
	/usr/include/stdio.h
tex_command.o: /usr/include/sys/types.h /usr/include/sys/sysmacros.h \
	/usr/include/X/Xlib.h /usr/include/X/mit-copyright.h \
	/usr/include/X/X.h /usr/include/stdio.h
tex_ir.o: /usr/include/sys/types.h /usr/include/sys/sysmacros.h \
	/usr/include/stdio.h macros.h tex_macros.h
vpe_send.o: /usr/include/stdio.h
vpe_window.o: /usr/include/X/Xlib.h /usr/include/X/mit-copyright.h \
	/usr/include/X/X.h /usr/include/stdio.h
vse_command.o: /usr/include/sys/types.h /usr/include/sys/sysmacros.h \
	/usr/include/X/Xlib.h /usr/include/X/mit-copyright.h \
	/usr/include/X/X.h /usr/include/stdio.h macros.h
vse_document.o: /usr/include/sys/types.h /usr/include/sys/sysmacros.h \
	/usr/include/X/Xlib.h /usr/include/X/mit-copyright.h \
	/usr/include/X/X.h /usr/include/stdio.h document.h window.h
vse_window.o: /usr/include/sys/types.h /usr/include/sys/sysmacros.h \
	/usr/include/X/Xlib.h /usr/include/X/mit-copyright.h \
	/usr/include/X/X.h /usr/include/stdio.h window.h macros.h
window.o: /usr/include/sys/types.h /usr/include/sys/sysmacros.h \
	/usr/include/X/Xlib.h /usr/include/X/mit-copyright.h \
	/usr/include/X/X.h /usr/include/stdio.h window.h
