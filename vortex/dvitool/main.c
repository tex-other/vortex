/* 
 * Copyright (c) 1986-1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

static char copyright_notice[] = "Copyright (c) 1986 - 1991 Regents of the University of California.\nAll rights reserved.";

#ifndef lint
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/main.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

#include "structs.h"
#include "constants.h"
#include "fdecls.h"
#include "texdvi.h"

int	arg_val = 0;

char	prog_name[40],
	cwd[MAXPATHLEN],
	home[MAXPATHLEN];

dvi_h		main_dvifile,
		*dvi = &main_dvifile;
pgs		main_pages;
switches	main_switches;
tl_data		main_tl_data,
		*tl = &main_tl_data;
int		interrupted = 0;
		erase_ch,
		werase_ch,
		kill_ch,
		intr_ch,
		lit_next_ch,
		reprint_ch;
rectang		rectang_null = { 0, 0, 0, 0 };

main(argc, argv)
	int	argc;
	char	*argv[]; 
{
	pg	*first;
	char	*cp;

	if ((cp = rindex(*argv, '/')) == (char *) 0) {
		strcpy(prog_name, *argv);
	} else {
		strcpy(prog_name, ++cp);
	}

	init_data();
	process_args(argc, argv);
	init(&argc, argv);

	/*
	 * we don't set up the created flag here, because the window
	 * isn't actually painted yet.  We'll set that in the repaint
	 * proc since only then are we sure that the window is acutally
	 * up on the screen.
	 */

	if ((dvi->fname = next_file_name()) != (char *) 0) {
		(void) load_dvi_file((pg *) 0, tl->sw->init_page,
		  0, 0);
	}

	main_loop();
	abort_run(0);
}

#include <pwd.h>
#include <sgtty.h>
extern char	*getenv();

init_data()
{
	extern tl_data	*tl;
	char		*home_dir;
	struct passwd	pwentry;
	func_arg	f_arg;
	struct ltchars	special_chars;
	struct sgttyb	regular_chars;
	struct tchars	abort_chars;

	tl->prog_name = prog_name;
	tl->dvi = dvi;
	tl->pid = getpid();

	dvi->cwd = cwd;
	if (getwd(dvi->cwd) == 0) {
		msg(FATAL, "couldn't get cwd: %s", dvi->cwd);
	} else {
		f_arg.str = dvi->cwd;
		set_wd(&f_arg);
	}

	dvi->pages = &main_pages;

	if ((home_dir = getenv("HOME")) != (char *) 0){
		strcpy(home, home_dir);
		tl->home = home;
	} else if (getpwent(pwentry) != (struct passwd *) 0) {
		strcpy(home, pwentry.pw_dir);
		tl->home = home;
	} else {
		msg(FATAL, "couldn't find home directory");
	}

	/*
	 * get the user's erase, word-erase, kill, abort, reprint and
	 * literal-next characters.
	 */
	ioctl(0, TIOCGETP, &regular_chars);
	erase_ch = (int) regular_chars.sg_erase;
	kill_ch = (int) regular_chars.sg_kill;
	ioctl(0, TIOCGLTC, &special_chars);
	werase_ch = (int) special_chars.t_werasc;
	lit_next_ch = (int) special_chars.t_lnextc;
	reprint_ch = (int) special_chars.t_rprntc;
	ioctl(0, TIOCGETC, &abort_chars);
	intr_ch = (int) abort_chars.t_intrc;
	
	
	tl->created = 0;
	tl->in_rc = 0;
	tl->sw = &main_switches;
}


init(argc, argv)
	int	*argc;
	char	*argv[];
{
	extern int	max_fonts;
	int		j;
	char		init_fn[256],
			*tl_name;
	int		reread_handler();

	install_pid();
	/* install the reread signal handler */
	(void) signal(SIGREREAD, reread_handler);

	/*
	 * when dvitool is fork'd from some other program (like suntools) it
	 * inherits all of the parents open file descriptors.  since dvitool
	 * needs all of the file descriptors it can get for font files, here
	 * we close all files except stdin, stderr, and stdout.
	 */
	tl->max_fds = getdtablesize();
	for (j = 3; j < tl->max_fds; j++) {
		close(j);
	}

	/*
	 * set up the space for the font files.  15 is a magic cookie
	 * discovered through trial and error.
	 */
	max_fonts = tl->max_fds - 15;

 	if (tl->home != (char *) 0) {
		strcpy(init_fn, home);
		strcat(init_fn, "/.");
		strcat(init_fn, tl->prog_name);
		strcat(init_fn, "rc");
		(void) read_init_file(init_fn);
	}
	if (strcmp(tl->home, dvi->cwd) != 0) {
		strcpy(init_fn, "./.");
		strcat(init_fn, tl->prog_name);
		strcat(init_fn, "rc");
		(void) read_init_file(init_fn);
	}

	/* expand the default font. */
	(void) expand_path();

	if (make_window(argc, argv) == 1)
		abort_run(1);
}

process_args(argc, argv)
	char	**argv;
{
	char		*arg,
			*tmp;
	switches	*sw = tl->sw;
	int		dummy;

	while (--argc) {
		arg = *++argv;
		if (*arg == '-') {
			arg++;
			while (*arg != '\0') {
				switch(*arg++) {
				/* ignore Sun window switches */
				case 'W':
					goto next_arg;
				case 'E':
					sw->use_existing = 1;
					break;
				default:
					fprintf(stderr, "%c is not a legal flag\n", *(arg-1));
					fflush(stderr);
					break;
				}
			}
		} else if (*arg == '+') {
			sscanf(&arg[1], "%d", &(sw->init_page));
 		} else if ((tmp = parse_dvi_name(arg, 1, &dummy)) != 0) {
			add_file_name(str_save(tmp));
		}
next_arg:	;
	}

	if (sw->use_existing && use_existing_tool() >= 0) {
		exit(0);
	}
}

/*
 * DOCUMENTATION
 *
 * Name: exit
 * Desc: This is the command to use when you wish to end your session
 *	with \lit{dvitool}.  It is usually bound to \lit{e}, 
 *	\lit{<C-C>}, and \lit{<C-X><C-C>}.
 */
bye_bye()
{
	abort_run(0);
}

abort_run(code)
{
	destroy_window();
	remove_pid(tl->pid);
	exit(code);
}

char	rc_buf[512],
	*rc_linep;

read_init_file(name)
	char	*name;
{
	FILE		*fp;

	extern tl_data	*tl;

	if ((fp = fopen(name, "r")) == (FILE *) 0) {
		return;
	}
	tl->in_rc = 1;
	while (fgets(rc_buf, sizeof(rc_buf), fp) != (char *) 0) {
		rc_linep = rc_buf;
		(void) do_init_line();
	}
	tl->in_rc = 0;
	fclose(fp);
	return;
}

extern char	arg0buf[],
		arg1buf[];

do_init_line()
{
	char		*line = rc_linep,
			c,
			cmd_name[50],
			*cmd = cmd_name;
	func		*fp;
	func_arg	argv[2],
			*ap;

	extern func	*cfp;

	while ((c = *line) == ' ' || c == '\t') {
		line++;
	}
	/* skip comments */
	if (*line == '#')
		return;
	while ((c = *line++) != ' ' && c != '\t' && c != '\n' && c != '\0') {
		*cmd++ = c;
	}
	*cmd = '\0';
	if (cmd == cmd_name) {
		return;
	}
	if ((fp = lookup_func_by_name(cmd_name)) == (func *) 0) {
		msg(PLAIN, "unknown function %s", cmd_name);
		return;
	}
	/* set the current function */
	cfp = fp;
	if (cfp->flags & ODVI && dvi->file == (FILE *) 0) {
		msg(PLAIN, "%s needs an open dvi file.", cfp->name);
		return;
	}
	if (cfp->flags & NORC) {
		msg(PLAIN, "%s cannot be done in the init file.", cfp->name);
		return;
	}
	if (cfp->arg0 != 0) {
		if (rc_get_arg(&line, arg0buf) != 0) {
			msg(PLAIN, "%s needs an argument!", cfp->name);
			return(-1);
		}
		if ((*cfp->arg0)(arg0buf, '\n', &cmd, &ap) != 0) {
			return(-1);
		}
		argv[0] = *ap;
	}
	if (cfp->arg1 != 0) {
		if (rc_get_arg(&line, arg1buf) != 0) {
			msg(PLAIN, "%s needs two arguments!", cfp->name);
			return(-1);
		}
		if ((*cfp->arg1)(arg1buf, '\n', &cmd, &ap) != 0) {
			return;
		}
		argv[1] = *ap;
	}
	/*
	 * point the global variable rc_linep to the first character not
	 * read by the argument routines so individual functions can read
	 * the args if necessary.  (It is necessary for bind-to-key which
	 * treats it's second arg differently depending on whether we're
	 * reading from the rc file or running interactively).
	 */
	rc_linep = line;
	(void) (cfp->funcp)(argv);
	return(0);
}

/*
 * Read the arguments for functions out of a line buffer.
 */
rc_get_arg(src, dest)
	char	**src,
		*dest;
{
	int	in_quotes = 0,
		found_an_arg = 0;
	char	*line = *src,
		c;

	while (*line == ' ' || *line == '\t') {
		line++;
	}
	while ((c = *line++) != '\0' && c != '\n') {
		if (!in_quotes && (c == ' ' || c == '\t')) {
			break;
		}
		if (c == '\\') {
			switch (c = *line++) {
			case '\\':
				break;
			case '"':
				break;
			case 'n':
				c = '\15';
				break;
			default:
				*dest++ = '\\';
				break;
			}
			found_an_arg++;
			*dest++ = c;
			continue;
		}
		if (c == '"') {
			in_quotes = !in_quotes;
			continue;
		}
		found_an_arg++;
		*dest++ = c;
	}
	*dest = '\0';
	*src = line;
	return((found_an_arg) ? 0 : -1);
}

char *
get_rc_line()
{
	return(&rc_buf[0]);
}

