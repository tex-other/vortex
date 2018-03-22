#
#  Jeff Mc Carrell
#  University of California, Berkeley
#

DEST	      = /usr/sww/bin

PROGRAM	      = dvitool

MANDIR	      = /usr/sww/man/man1

HELPDIR	      = /usr/sww/lib/dvitool

FONTPATH      = /usr/sww/lib/tex/fonts/previewPK

MANUAL	      = dvitool.1

CONTACT	      = your-name-here

HELPDOC	      = $(OHELPSRC) $(FHELPSRC) $(VHELPSRC) ohelpsed

DOCUMENTATION = $(HELPDOC) docmac.tex $(MANUAL) sample.rc $(HELPIDXS) \
		README.DVITOOL COPYRIGHT

VHELPFILE     = vhelp.dvi

VHELPSRC      = vhelp.tex

FHELPFILE     = fhelp.dvi

FHELPSRC      = fhelp.tex

OHELPFILE     = ohelp.dvi

OHELPSRC      = ohelp.tex

HELPIDXS      = ohelpidx fhelpidx vhelpidx

MKDOC	      = ./mkdoc

EXTHDRS	      =

HDRS	      = bits.h \
		circle.h \
		commands.h \
		constants.h \
		cursor.defs.h \
		cursor.h \
		fdecls.h \
		font.h \
		funcdefs.h \
		hourglass.h \
		icon.h \
		keys.h \
		mag.h \
		mouse.h \
		pkcommands.h \
		readmacs.h \
		rectangs.h \
		structs.h \
		sun.h \
		texdvi.h \
		vars.h

#
# you may substitute -O for -g if you prefer.
#
CFLAGS	      = -g -DV_DEF_FONT_PATH='"$(FONTPATH)"'

IFLAGS	      =

LDFLAGS	      = $(CFLAGS)

LIBS	      = -lsuntool -lsunwindow -lpixrect 

LINKER	      = cc

MAKEFILE      = Makefile

OBJS	      = compl.o \
		cursor.o \
		file.o \
		font.o \
		help.o \
		input.o \
		keymaps.o \
		mag.o \
		main.o \
		mouse.o \
		msg.o \
		page.o \
		postamble.o \
		read.o \
		reread.o \
		scroll.o \
		search.o \
		select.o \
		sun.o \
		typeout.o \
		utils.o \
		vars.o \
		version.o

PRINT	      = tgrind -Pgp

SRCS	      = compl.c \
		cursor.c \
		file.c \
		font.c \
		help.c \
		input.c \
		keymaps.defs \
		mag.c \
		main.c \
		mapkeys.c \
		mouse.c \
		msg.c \
		page.c \
		postamble.c \
		read.c \
		reread.c \
		scroll.c \
		search.c \
		select.c \
		sun.c \
		typeout.c \
		utils.c \
		vars.c \
		version.c

$(PROGRAM):     $(OBJS)
		@rm -f $(PROGRAM) core
		@echo "Linking \`$(PROGRAM)' ..."
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)

mapkeys:	mapkeys.c funcdefs.h
		cc $(CFLAGS) -o mapkeys mapkeys.c

keymaps.o:	mapkeys keymaps.defs
		@rm -f keymaps.c
		./mapkeys < keymaps.defs > keymaps.c
		cc $(CFLAGS) -c keymaps.c

help.o:		help.c
		cc $(CFLAGS) \
		   -DVHELPFILE=\"$(HELPDIR)/$(VHELPFILE)\" \
		   -DFHELPFILE=\"$(HELPDIR)/$(FHELPFILE)\" \
		   -DOHELPFILE=\"$(HELPDIR)/$(OHELPFILE)\" -c help.c 

helpsrcs:	$(VHELPSRC) $(FHELPSRC)

dvihelp:	$(VHELPFILE) $(FHELPFILE) $(OHELPFILE)

$(VHELPSRC):	vars.c vars.h
		@echo -n making variable tex file...
		@$(MKDOC) -f tex vars.c > $(VHELPSRC)
		@echo "\\bye" >> $(VHELPSRC)
		@echo done.

$(VHELPFILE):	$(VHELPSRC) docmac.tex
		@echo $(FONTPATH) > fontpath.tex
		tex $(VHELPSRC)
		@rm -f *.log

vhelpidx:	$(VHELPSRC)
		@echo -n writing variable index \`vhelpidx\'...
		@sed -n -e "s/^\\\\docstr{\(.*\)}$$/\"\1\",/w vhelpidx" $(VHELPSRC)
		@echo done.

$(FHELPSRC):	$(SRCS)
		@echo -n making command tex file...
		@$(MKDOC) -f tex `echo $(SRCS) | sed 's/vars.c//'` > $(FHELPSRC)
		@echo "\\bye" >> $(FHELPSRC)
		@echo done.

$(FHELPFILE):	$(FHELPSRC) docmac.tex
		tex $(FHELPSRC)
		@rm -f *.log

fhelpidx:	$(FHELPSRC)
		@echo -n writing command index \`fhelpidx\'...
		@sed -n -e "s/^\\\\docstr{\(.*\)}$$/\"\1\",/w fhelpidx" $(FHELPSRC)
		@echo done.

$(OHELPFILE):	$(OHELPSRC)
		@echo '$(CONTACT)' > contact.file
		tex $(OHELPSRC)
		@rm -f ohelp.log

ohelpidx:	$(OHELPSRC)
		@echo -n writing overview index \`ohelpidx\'...
		@sed -n -f ohelpsed $(OHELPSRC) | egrep '^".*",$$' | sort -o ohelpidx
		@-uniq -d ohelpidx /tmp/xxohlp
		@if test -s /tmp/xxohlp; then \
			echo indices not unique\; see /tmp/xxohlp.; \
			exit 1; \
		else \
			echo done.; \
			exit 0; \
		fi;
		@rm -f /tmp/xxohlp

#
# for the distribution, make MKDOC a noop.
#
$(MKDOC):
		echo foobar > $(MKDOC)

install:	$(DEST)/$(PROGRAM) $(MANDIR)/$(MANUAL) $(HELPDIR)/$(OHELPFILE) \
		$(HELPDIR)/$(FHELPFILE) $(HELPDIR)/$(VHELPFILE)
		@echo "Installation completed."

clean:;		@rm -f $(OBJS) mapkeys keymaps.c $(PROGRAM) core *.out .#* \
		$(VHELPFILE) $(FHELPFILE) \
		*.log $(PROGRAM).bindings $(PROGRAM).commands *~

update:		$(DEST)/$(PROGRAM)

manual:		$(MANDIR)/$(MANUAL)

$(DEST)/$(PROGRAM): $(PROGRAM)
		install $(IFLAGS) -c -m 0755 $(PROGRAM) $(DEST)
		@ls -lgs $(DEST)/$(PROGRAM)

$(MANDIR)/$(MANUAL): $(MANUAL)
		install $(IFLAGS) -c -m 0644 $(MANUAL) $(MANDIR)
		@ls -lgs $(MANDIR)/$(MANUAL)

$(HELPDIR)/$(VHELPFILE): $(VHELPFILE)
		install $(IFLAGS) -c -m 0644 $(VHELPFILE) $(HELPDIR)
		@ls -lgs $(HELPDIR)/$(VHELPFILE)

$(HELPDIR)/$(FHELPFILE): $(FHELPFILE)
		install $(IFLAGS) -c -m 0644 $(FHELPFILE) $(HELPDIR)
		@ls -lgs $(HELPDIR)/$(FHELPFILE)

$(HELPDIR)/$(OHELPFILE): $(OHELPFILE)
		install $(IFLAGS) -c -m 0644 $(OHELPFILE) $(HELPDIR)
		@ls -lgs $(HELPDIR)/$(OHELPFILE)

#
# The checkout target is only useful for VorTeX developers
#
SRCDIR = /home/yew/yew4/vortex/src/dvitool

DISTRIB	      = $(SRCS) $(HDRS) $(DOCUMENTATION)

checkout:
		co $(DISTRIB)
		-mkdir texdvi
		cd texdvi; rm -f RCS; ln -s $(SRCDIR)/texdvi/RCS
		cd texdvi; co Makefile ; $(MAKE) checkout

dist-checkout: checkout

###
cursor.o: structs.h constants.h sun.h cursor.defs.h circle.h hourglass.h \
	mouse.h icon.h
file.o: structs.h constants.h mag.h fdecls.h
font.o: structs.h constants.h commands.h font.h fdecls.h readmacs.h \
	pkcommands.h bits.h
help.o: structs.h constants.h
input.o: structs.h constants.h fdecls.h sun.h vars.h keys.h funcdefs.h
keymaps.o: structs.h
mag.o: structs.h constants.h mag.h
main.o: structs.h constants.h fdecls.h texdvi.h
mapkeys.o: structs.h constants.h funcdefs.h keys.h
mouse.o: structs.h mag.h sun.h
msg.o: structs.h constants.h
page.o: structs.h constants.h mag.h fdecls.h vars.h commands.h
postamble.o: structs.h constants.h commands.h fdecls.h
read.o: structs.h constants.h commands.h font.h readmacs.h sun.h
reread.o: structs.h constants.h fdecls.h texdvi.h
scroll.o: structs.h constants.h rectangs.h sun.h fdecls.h
search.o: structs.h constants.h fdecls.h
select.o: structs.h constants.h rectangs.h sun.h fdecls.h
sun.o: structs.h constants.h sun.h fdecls.h rectangs.h icon.h
typeout.o: structs.h constants.h
utils.o: structs.h fdecls.h
vars.o: structs.h constants.h vars.h fdecls.h bits.h
version.o: structs.h constants.h
