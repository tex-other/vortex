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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/input.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

/* Routines to handle input from the user. */

#include <sgtty.h>
#include "structs.h"
#include "constants.h"
#include "fdecls.h"
#include "sun.h"
#include "vars.h"
#include "keys.h"

extern char
	cwd[],
	pattern_buf[];

/*
 * these are the buffers for the last values of some of the arguments.
 */
char	last_cmd[ARGBUF_SIZE],
	last_fhelp[ARGBUF_SIZE],
	last_fname[ARGBUF_SIZE],
	last_font[ARGBUF_SIZE],
	last_ohelp[ARGBUF_SIZE],
	last_printed[ARGBUF_SIZE],
	last_set[ARGBUF_SIZE],
	last_vhelp[ARGBUF_SIZE];
	
/*
 * this routine takes a window event and returns an integer suitable for
 * mapping onto the function maps.  We set the meta flag and translate
 * the event into an ascii char if it comes in with the meta bit (bit 7)
 * on.
 */
Event	last_event;
static	is_mouse_button = 0,
	getascii = 0,
	meta_set = 0;

translate_event(event, meta_flag)
	Event	*event;
	int	*meta_flag;
{
	int	virtual_key = 0,
		code;

	is_mouse_button = *meta_flag = 0;
	last_event = *event;
	code = event_id(event);

	if (event_is_meta(event)) {
		meta_set = *meta_flag = 1;
		return(code - 128);
	}
	if (event_is_ascii(event)) {
		return(code);
	}
	/*
	 * ignore all other events if we just want ascii events.
	 */
	if (getascii) {
		return(-1);
	}

	/*
	 * we throw away all of the up button transitions here, because
	 * otherwise I see two events each time the user presses a button.
	 */
	if (event_is_up(event)) {
		return(-1);
	}

	/*
	 * eventually, after I write my own scrollbar code, we'll check
	 * here to see if the button went down in a scrollbar and return
	 * a different code for scrollbar events.  currently, scrollbar
	 * events are caught much earlier in the input stream, so we
	 * never see them here.
	 */

	if (event_is_button(event)) {
		is_mouse_button = 1;
		if (event_shift_is_down(event))
			virtual_key++;
		if (event_ctrl_is_down(event))
			virtual_key += 2;
		if (code == MS_LEFT)
			return(virtual_key + 128);
		if (code == MS_MIDDLE)
			return(virtual_key + 132);
		return(virtual_key + 136);
	}
	/* ignore other events such as window resize or repaint */
	return(-1);
}

static int	input_c;

/* return only ascii events. */

win_getascii()
{
	int	wc;

	getascii++;
	wc = win_getc();
	getascii = 0;
	return(wc);
}

win_getc()
{
	Event		ev;
	int		wc,
			meta;

	extern tl_data	*tl;
	extern sun_d	*sunp;
	for (;;) {
		if (window_read_event(sunp->image, &ev) < 0) {
			perror(tl->prog_name);
			exit(1);
		}

		if ((wc = translate_event(&ev, &meta)) != -1) {
			input_c = wc;
			return(wc);
		}
	}
}

/*
 * wait for the user to type c.  accept anything if c == '\0'.  return
 * the character (or mouse button) typed.
 */
wait_for(c)
	char	c;
{
	int	gc;

	for(;;) {
		if ((gc = win_getc()) == -1)
			continue;
		if (c == '\0' || gc == c)
			return(gc);
	}
}

extern func	*main_map[],
		*pref1_map[],
		*pref2_map[],
		*ansi_map[];
static char	abort_msg[] = "[aborted]",
		null_msg[] =  "[use string \"\\^@\" for `\\0']";

input_handler(win, event)
	Canvas	win;
	Event	*event;
{
	int		c,
			meta;
	/*
	 * we throw away all up events.  The only time they are
	 * significant is in the menu package, and that package reads its
	 * own events anyway.
	 */

	if (event_is_up(event)) {
		return;
	}

	if ((c = translate_event(event, &meta)) == -1)
		return;

	input_c = c;
	if (meta) {
		(void) exec_key_command(c, pref1_map);
	} else {
		(void) exec_key_command(c, main_map);
	}
}

/*
 * use prefixes to change function tables
 */
/*
 * DOCUMENTATION
 *
 * Name: Prefix-1
 * Desc: \em{Prefix-1} is a command to access an auxiliary keymap in the
 *	Emacs tradition.  There are 3 such keymaps: the default keymap,
 *	the \em{Prefix-1} keymap and the \em{Prefix-2} keymap.
 *	\em{Prefix-1} is always bound to \lit{<ESC>}.
 *	Normally, the user need not be aware of this command; it simply
 *	exists more commands can be run from the keyboard.  You can
 *	think of it as a different kind of shift key.
 * SeeA: Prefix-2
 */
pre_1()
{
	push_cursor(MOUSE_CUR);
	exec_key_command(win_getc(), pref1_map);
	pop_cursor();
}
/*
 * DOCUMENTATION
 *
 * Name: Prefix-2
 * Desc: \em{Prefix-2} is the command to access the second auxiliary
 *	keymap.  \em{Prefix-2} is always bound to \lit{<C-X>}.
 * SeeA: Prefix-1
 */

pre_2()
{
	push_cursor(MOUSE_CUR);
	exec_key_command(win_getc(), pref2_map);
	pop_cursor();
}

exec_key_command(ch, map)
	func	*map[];
{
	if (ch == -1)
		return;
	if (ch == abort_ch) {
		msg(PLAIN, abort_msg);
		return;
	}
	exec_command(map[ch]);
}

#include "funcdefs.h"

int	arg_val;
func	*cfp = 0;	/* the current function pointer. */
char	arg0buf[ARGBUF_SIZE],
	arg1buf[ARGBUF_SIZE];

exec_command(fp)
	func	*fp;
{
	func_arg	argv[2],
			*ap,
			*get_arg();

	char		*a0 = arg0buf,
			*a1 = arg1buf;
	int		run_named_command();

	if ((cfp = fp) == (func *) 0) {
		msg(PLAIN, "[unbound]");
		return;
	}

	if (cfp->flags & ISBUT && !is_mouse_button) {
		msg(PLAIN, "use the mouse for %s.", cfp->name);
		return;
	}
	if (cfp->flags & ODVI && dvi->file == (FILE *) 0) {
		msg(PLAIN, "need an open dvi file for %s.", cfp->name);
		return;
	}
	if (cfp->flags & SEL && dvi->sel_start == (s_char *) 0) {
		msg(PLAIN, "make a selection for %s.", cfp->name);
		return;
	}
	if (cfp->arg0 != 0 || cfp->arg1 != 0) {
		push_cursor(MOUSE_CUR);
		if (cfp->funcp == run_named_command) {
			msg(PLAIN, ": ");
		} else {
			msg(PLAIN, "%s", cfp->name);
		}
		if (arg_val != 0 && cfp->arg0 == a_int) {
			argv[0].integer = arg_val;
		} else if (cfp->arg0 != 0) {
			msg_cursor_from_arg(cfp->arg0);
			*a0 = '\0';
			if ((ap = get_arg(arg0buf, cfp->arg0, cfp->ctrl0,
			   cfp->arg0_def)) == (func_arg *) 0) {
				goto fail;
			}
			argv[0] = *ap;
		}
		if (cfp->arg1 != 0) {
			msg_cursor_from_arg(cfp->arg1);
			*a1 = '\0';
			if ((ap = get_arg(arg1buf, cfp->arg1, cfp->ctrl1,
			  cfp->arg1_def)) == (func_arg *) 0) {
			  	goto fail;
			}
			argv[1] = *ap;
		}
		pop_cursor();
		msg_cursor_from_arg((int (*)()) 0);

	}
	(void) (cfp->funcp)(argv);
	/*
	 * we don't clear the message when doing Sun window functions
	 * because the interaction does funny things.
	 */
	if (cfp != (func *) 0 &&
	    cfp->funcp != redraw_tool &&
	    cfp->funcp != hide_tool &&
	    cfp->funcp != expose_tool &&
	    cfp->funcp != toggle_tool) {
		clear_msg();
	}
	return;

fail:
	pop_cursor();
	msg_cursor_from_arg((int (*)()) 0);
}

/*
 * DOCUMENTATION
 *
 * Name: exec
 * Call: command name
 * Desc: \em{Exec} is the most general way to run a \lit{dvitool}
 *	command.  \em{Exec} takes a (possibly completed) string
 *	and looks in it's table of command names.  If some command
 *	name exactly matches the string, then that command is 
 *	executed.  Since the command name argument to \em{exec}
 *	is subject to completion, a quick way to see all of
 *	\lit{dvitool}'s commands is to type \lit{<ESC>x?}.
 */
/*
 * execute a command by its name.
 */
run_named_command(argv)
	func_arg	*argv;
{
	char	*func_name = (*argv).str;
	func	*fp;

	if (*func_name == '\0') {
		return;
	}
	if ((fp = lookup_func_by_name(func_name)) == (func *) 0) {
		msg(APPEND, " %s: no such command", func_name);
		return;
	}
	strcpy(last_cmd, func_name);
	(void) exec_command(fp);
}

func *
lookup_func_by_name(name)
	char	*name;
{
	func	*fp;
	int	stat;

	for (fp = commands; fp->name != (char *) 0; fp++) {
		if ((stat = strcmp(name, fp->name)) == 0) {
			return(fp);
		} else if (stat < 0) {
			break;
		}
	}
	return((func *) 0);
}

func_arg *
get_arg(arg_buf, complete, ctrl, previous)
	char		*arg_buf;
	int		(*complete)(),
			(*ctrl)();
	char		*previous;
{
	func_arg	*arg = (func_arg *) 0;
	char		*bufp = arg_buf,
			*msg_p,
			*ctrl_p;
	int		wc,
			k,
			count,
			dirty,
			msg_len = 0;

	msg_c(' ');
	*bufp = '\0';
	for(;;) {
		wc = win_getascii();
do_char:
		if (bufp - arg_buf > ARGBUF_SIZE) {
			msg(PLAIN, "character buffer overflow!");
			return((func_arg *) 0);
		}
		if (msg_len) {
			msg_del_c(msg_len);
			msg_len = 0;
		}
		if (wc == lit_next_ch) {
			wc = win_getascii();
			*bufp++ = wc;
			*bufp = '\0';
			msg_c(wc);
			continue;
		}
		if (wc == abort_ch || wc == intr_ch) {
			msg(PLAIN, abort_msg);
			return((func_arg *) 0);
		}
		if (wc == erase_ch) {
			if (bufp == arg_buf)
				continue;
			msg_del_c(1);
			*--bufp = '\0';
			continue;
		}
		/* kill character, erase the entire line. */
		if (wc == kill_ch) {
			msg_del_c(strlen(arg_buf));
			bufp = arg_buf;
			*bufp = '\0';
			continue;
		}
		/* word erase, erase the last word. */
		if (wc == werase_ch) {
			count = 0;
			dirty = 0;
			for (;;) {
				if (arg_buf == bufp) {
					break;
				}
				if (dirty && (*bufp == ' ' ||
					      *bufp == '\t' ||
					      *bufp == '-')) {
					break;
				}
			  	dirty = (!dirty) ? isalnum(*bufp) : dirty;
			  	count++;
				bufp--;
			}
			if (bufp != arg_buf) {
				count--; bufp++;
			}
			msg_del_c(count);
			*bufp = '\0';
			continue;
		}

		/* reprint; insert the default value if it exists. */
		if (wc == reprint_ch) {
			if (previous == (char *) 0) {
				msg_p = "  [no default]";
				msg_len = 14;
				msg(APPEND | LITERAL, msg_p);
			} else {
				msg_p = previous;
				msg(APPEND | LITERAL, msg_p);
				while (*msg_p != '\0') {
					*bufp++ = *msg_p++;
				}
				*bufp = '\0';
			}
			continue;
		}
		if (wc == ' ' || wc == '\r' || wc == '\n' || wc == '?') {
			/*
			 * the completion functions return:
			 *	< 0	keep getting characters.
			 *	  0	complete argument. return it.
			 *	> 0	a character to act on.
			 */
			msg_p = (char *) 0;
			k = (*complete)(arg_buf, wc, &msg_p, &arg, previous);
			if (*bufp != '\0') {
				/* show the completion. */
				msg(APPEND | LITERAL, bufp);
				/* and advance our local buffer pointer. */
				while (*bufp != '\0') {
					bufp++;
				}
			}
			*bufp = '\0';
			if (msg_p != (char *) 0) {
				msg_len = strlen(msg_p);
				msg(APPEND | LITERAL, msg_p);
			}
			if (k == 0) {
				break;
			}
			if (k > 0) {
				wc = k;
				goto do_char;
			}
			continue;
		}
		if (wc == 0) {
			msg_len = sizeof(null_msg) - 1;
			msg(APPEND | LITERAL, null_msg);
			continue;
		}
		if (ctrl != (int (*)()) 0 && iscntrl(wc)) {
			/*
			 * if the control character routine processes
			 * this character, then report those results.
			 */
			if ((k = (*ctrl)(wc, &ctrl_p, &msg_p)) >= 0) {
				if (k == APPEND && ctrl_p != (char *) 0) {
					msg(APPEND | LITERAL, ctrl_p);
					while (*ctrl_p != '\0') {
						*bufp++ = *ctrl_p++;
					}
					*bufp = '\0';
				}
				if (k == OVERWRITE && ctrl_p != (char *) 0) {
					msg_del_c(strlen(arg_buf));
					bufp = arg_buf;
					msg(APPEND | LITERAL, ctrl_p);
					while (*ctrl_p != '\0') {
						*bufp++ = *ctrl_p++;
					}
					*bufp = '\0';
				}
				if (msg_p != (char *) 0) {
					msg_len = strlen(msg_p);
					msg(APPEND | LITERAL, msg_p);
				}
				continue;
			}
		}
		*bufp++ = wc;
		*bufp = '\0';
		msg_c(wc);
	}
	return(arg);
}

func_arg	rtn;

/*
 * the completion routines need to point the func_arg pointer to some
 * valid func_arg if they succeed or return null in the func_arg
 * pointer.  They are guaranteed that the func_arg pointer is null when
 * they get it.
 */
/*
 * the completion functions return:
 *	< 0	keep getting characters
 *	  0	complete argument. return it.
 *	> 0	a character to act on
 */

a_int(buf, wc, msg, arg)
	char		*buf,
			**msg;
	func_arg	**arg;
{
	int		val;

	if (*buf == '\0') {
		*msg = "  [enter some digits!]";
		return(-1);
	} else if (sscanf(buf, "%d", &val) == 0) {
		*msg = "  [couldn't convert integer!]";
		return(-1);
	}
	rtn.integer = val;
	*arg = &rtn;
	return(0);
}

a_str(buf, wc, msg, arg)
	char		*buf,
			**msg;
	func_arg	**arg;
{
	char	lbuf[2];

	if (wc == ' ' || wc == '?') {
		lbuf[0] = wc;
		lbuf[1] = '\0';
		strcat(buf, lbuf);
		return(-1);
	}
	rtn.str = buf;
	*arg = &rtn;
	return(0);
}

/*
 * a_lstr is exactly like a_str, except that the string is parsed for
 * control characters after is completely read in, and it is terminated
 * with 3 '\0' chars.
 */
static char	lstr_buf[ARGBUF_SIZE];

a_lstr(buf, wc, msg, arg)
	char		*buf,
			**msg;
	func_arg	**arg;
{
	char		lbuf[2],
			*cp,
			*emsg;
	int		*rawkeys,
			len;
	extern int	srch_pattern_len;

	if (wc == ' ' || wc == '?') {
		lbuf[0] = wc;
		lbuf[1] = '\0';
		strcat(buf, lbuf);
		return(-1);
	}

	if ((len = parse_keys(buf, &rawkeys, &emsg)) < 0) {
		sprintf(lstr_buf, "  [%s]", emsg);
		*msg = lstr_buf;
		return(-1);
	}
	/*
	 * this is a complete hack to allow us to search for characters
	 * that have '\0' in them.
	 */
	srch_pattern_len = len;

	for (cp = buf; len--; ) {
		*cp++ = (u_char) *rawkeys++;
	}
	*cp++ = '\0';

	rtn.str = buf;
	*arg = &rtn;
	return(0);
}


static char	*bad_ext[] = {
	".aux",
	".bbl",
	".bib",
	".c",
	".h",
	".ind",
	".log",
	".o",
	".tex",
	".toc",
	"~",
	(char *) 0
};

#include <sys/stat.h>

static char
	no_match[] =   "  [no match]",
	not_unique[] = "  [complete but not unique/substring]",
	incomplete[] = "  [incomplete match]",
	need_more[] =  "  [ambiguous]";
static int		expanding_filenames = 0;
static struct stat	st_buf;
static char		wd[1024],
			append[128];


a_fname(buf, wc, rtnmsg, arg)
	char		*buf,
			**rtnmsg;
	func_arg	**arg;
{
	static char	fname[1024];
	char		*cp,
			*stripped_fn,	/* the fn w/o directories */
			*rindex();
	int		k;
	static struct fncmpl {
		char	cwd[1024];
		time_t	st_mtime,
			st_ctime;
		char	**cwd_list;
	} last_wd;

	/*
	 * expand any meta-characters in the filename passed in into a
	 * separate buffer.
	 */
	if (expand(buf, fname) < 0) {
		sprintf(wd, "  [%s]", fname);
		*rtnmsg = wd;
		return(-1);
	}

	/* <compute the effective directory>
		<effective directory is everything up to and including
			the first '/'
			or nothing if no '/' in file name.>
		<if first char of passed in filename is not '/',
		prepend cwd>
	 */
	wd[0] = '\0';
	if (fname[0] != '/') {
		strcpy(wd, dvi->cwd);
		strcat(wd, "/");
	}

	/* set up the (possibly expanded) working directory */
	if ((stripped_fn = rindex(fname, '/')) != (char *) 0) {
		stripped_fn++;
		strncat(wd, fname, (stripped_fn - fname));
	} else {
		stripped_fn = fname;
	}

	/* see if we've aleady read the effective directory. */
	if (strcmp(wd, last_wd.cwd) == 0) {
		if (stat(wd, &st_buf) < 0) {
			msg(FATAL | PERROR, "stat failed: %s?", wd);
		}
		if (st_buf.st_mtime == last_wd.st_mtime &&
		    st_buf.st_ctime == last_wd.st_ctime)
			goto unchanged;
		last_wd.st_mtime = st_buf.st_mtime;
		last_wd.st_ctime = st_buf.st_ctime;
	}
	strcpy(last_wd.cwd, wd);
	
	if (last_wd.cwd_list != (char **) 0) {
		(void) compl_free(last_wd.cwd_list);
	}

	if ((last_wd.cwd_list = compl_path(wd, bad_ext)) == (char **) 0) {
		sprintf(fname, "  [couldn't read filenames from dir %s]", wd);
		*rtnmsg = &fname[0];
		return(-1);
	}

unchanged:
	if (wc == '?') {
		expanding_filenames++;
		/*
		 * generic_match returns either 0 or an ascii character
		 * to append.
		 */
		k = generic_match(stripped_fn, rtnmsg, last_wd.cwd_list);
		expanding_filenames = 0;
		/* we never complete from here. */
		return((k == 0) ? -1 : k);
	}
	if (wc == ' ') {
		expanding_filenames++;
		k = generic_expand(stripped_fn, rtnmsg, last_wd.cwd_list);
		expanding_filenames = 0;
		strcat(buf, append);
		return(-1);
	}
	/* a '\r' or '\n' or success in the expand routine. */
	rtn.str = buf;
	*arg = &rtn;
	return(0);
}

/*
 * expand the match leaving the result in the global append and set up
 * the diagnostic message (if any).
 */

generic_expand(buf, rtnmsg, possibles)
	char	*buf,
		**rtnmsg,
		**possibles;
{

	if (possibles == (char **) 0) {
		msg(FATAL, "null possibles list in generic_expand!");
	}
		
	append[0] = '\0';
	switch(compl_expand(buf, possibles, append)) {
	case -1:
		msg(FATAL, "compl_expand on %s", dvi->cwd);
		/* no return */
	case 0:
		*rtnmsg = no_match;
		break;
	case 1:
		*rtnmsg = (char *) 0;
		if (expanding_filenames) {
			/* check for directories */
			strcat(wd, buf);
			strcat(wd, append);
			if (stat(wd, &st_buf) >= 0 &&
			  st_buf.st_mode & S_IFDIR) {
				strcat(append, "/");
				break;
			}
		}
		return(1);
	case 2:
		*rtnmsg = not_unique;
		break;
	case 3:
		*rtnmsg = incomplete;
		break;
	case 4:
		*rtnmsg = need_more;
		break;
	}
	return(0);
}

/* show all of the possible matches */
generic_match(partial, rtnmsg, possibles)
	char	*partial,
		**rtnmsg,
		**possibles;
{
	int	how_many,
		k;
	char	**match_list;

	if ((how_many = compl_match(partial, possibles, &match_list)) > 0) {
		k = typeout(match_list, how_many, T_COL_BY_ROW, rtnmsg);
		free(match_list);
	} else {
		*rtnmsg = no_match;
		k = 0;
	}
	return(k);
}

static char	**font_names = 0,
		**func_names = 0,
		**var_names = 0;
char		*vhelp_names[] = {
#			include "vhelpidx"
			(char *) 0
		},
		*fhelp_names[] = {
#			include "fhelpidx"
			(char *) 0
		},
		*ohelp_names[] = {
#			include "ohelpidx"
			(char *) 0
		};
static func	*functionp;
static variable	*varp;
extern variable	vars[];

fv_compl(buf, wc, rtnmsg, possibles, arg, require_ret)
	char		*buf,
			**rtnmsg,
			**possibles;
	func_arg	**arg;
{
	int		k;

	if (wc == '?') {
		/*
		 * generic_match returns either 0 or an ascii character
		 * to append.
		 */
		k = generic_match(buf, rtnmsg, possibles);
		return((k == 0) ? -1 : k);
	} 
	if (wc == ' ') {
		k = generic_expand(buf, rtnmsg, possibles);
		strcat(buf, append);
		/*
		 * we will not signal a finished completion if require_ret
		 * is true.
		 */
		if (k <= 0 || require_ret) {
			return(-1);
		} /* else fall out and return the completed string */
	}
	/* a '\r' or '\n' */
	rtn.str = buf;
	*arg = &rtn;
	return(0);
}

char *
next_func()
{
	char	*name;

	if (functionp != (func *) 0) {
		name = functionp->name;
		functionp++;
		return(name);
	}
	return((char *) 0);
}

char *
next_var()
{
	char	*name;

	if (varp != (variable *) 0) {
		name = varp->name;
		varp++;
		return(name);
	}
	return((char *) 0);
}

a_vhelp(buf, wc, msg, arg)
	char		*buf,
			**msg;
	func_arg	**arg;
{
	/*
	 * we never have to build the completion list -- it is static.
	 */
	return(fv_compl(buf, wc, msg, vhelp_names, arg, 1));
}

a_fhelp(buf, wc, msg, arg)
	char		*buf,
			**msg;
	func_arg	**arg;
{
	return(fv_compl(buf, wc, msg, fhelp_names, arg, 1));
}

a_ohelp(buf, wc, msg, arg)
	char		*buf,
			**msg;
	func_arg	**arg;
{
	return(fv_compl(buf, wc, msg, ohelp_names, arg, 1));
}

a_func(buf, wc, msg, arg)
	char		*buf,
			**msg;
	func_arg	**arg;
{
	/*
	 * build the completion list for the first time if necessary.
	 */
	if (func_names == (char **) 0) {
		functionp = commands;
		func_names = compl_list(next_func);
	}
	return(fv_compl(buf, wc, msg, func_names, arg, 0));
}

a_font(buf, wc, msg, arg)
	char		*buf,
			**msg;
	func_arg	**arg;
{
	extern int	font_names_dirty;
	extern char	*next_font_name();

	if (font_names_dirty) {
		free(font_names);
		font_names = (char **) 0;
		font_names_dirty = 0;
	}

	if (font_names == (char **) 0) {
		init_next_font();
		font_names = compl_list(next_font_name);
	}
	return(fv_compl(buf, wc, msg, font_names, arg, 1));
}

a_var(buf, wc, msg, arg)
	char		*buf,
			**msg;
	func_arg	**arg;
{
	if (var_names == (char **) 0) {
		varp = vars;
		var_names = compl_list(next_var);
	}
	return(fv_compl(buf, wc, msg, var_names, arg, 0));
}

msg_cursor_from_arg(fp)
	int	(*fp)();
{
	int	val;

	if (fp == (int (*)()) 0) {
		val = NO_MCUR;
	} else if (fp == a_int) {
		val = INT_MCUR;
	} else if (fp == a_str) {
		val = STR_MCUR;
	} else if (fp == a_lstr) {
		val = LSTR_MCUR;
	} else if (fp == a_fname) {
		val = FNAME_MCUR;
	} else if (fp == a_func) {
		val = FUNC_MCUR;
	} else if (fp == a_font) {
		val = FONT_MCUR;
	} else if (fp == a_var) {
		val = VAR_MCUR;
	} else if (fp == a_vhelp) {
		val = VHELP_MCUR;
	} else if (fp == a_fhelp) {
		val = FHELP_MCUR;
	} else if (fp == a_ohelp) {
		val = OHELP_MCUR;
	} else {
		return;
	}
	set_msg_cur(val);
}

/*
 * DOCUMENTATION
 *
 * Name: numeric-argument
 * Desc: This command is used to enter a ``count'' which can be interpreted
 *	by functions that take an integer as their first argument.
 *	It is a prefix argument that is zeroed after each command.
 *	The command interpreter checks each time a command is run for a
 *	non-zero value of the numeric argument.  If it is non-zero and
 *	the first argument of the command being executed is of type 
 *	integer, the command interpreter substitutes the numeric argument
 *	for the command argument.
 *	For
 *	example, \em{goto-physical-page} takes a single integer argument
 *	and is usually bound to \lit{G}.  So the character sequence
 *	\lit{1G} will position \lit{dvitool} on the first page of the
 *	\lit{DVI} file.
 *	Other commands use the presence or absence of a numeric argument
 *	as a switch; for example, the edge commands act differently when
 *	a numeric argument is present.
 */
numeric_arg()
{
	do_numeric_arg(input_c);
}

do_numeric_arg(ch)
	int	ch;
{
	int	sign = 1;
	u_char	c = ch;

	if (c == '-') {
		sign = -1;
		arg_val = 0;
	} else {
		arg_val = (int) c - '0';
	}
	msg(PLAIN, "arg: %c", c);
	for(;;) {
		c = win_getascii();
		if (!isdigit(c))
			break;
		arg_val = 10 * arg_val + (int) c - '0';
		msg_c(c);
	}
	arg_val *= sign;
	exec_key_command(c, main_map);
	arg_val = 0;
}

/*
 * this routine translates an input string of ascii printable characters to
 * the various characters that dvitool deals with including control
 * characters and the character defined to be mouse inputs.
 */
/*
 * here is a list of the translations which are done:
 *	\e	escape
 *	\^X	ctrl-X for all control characters
 *	\^?	delete (\0177)
 *	\m	mouse prefix
 *	\v	vertical scrollbar prefix
 *	\h	horizontal scrollbar prefix
 *		l,r,m	left, right, middle
 *		L,R,M	shift left, shift right, shift middle
 *		\^	ctrl-{l,r,m,L,R,M}
 *	\\	\
 *	\000	char in octal.  limited to 3 characters.
 */
static char	pkeys_ebuf[ARGBUF_SIZE];

parse_keys(str, retp, msg)
	char	*str,
		**msg;
	int	**retp;
{
	char		c;
	static int	buf[ARGBUF_SIZE];
	int		digit = 0,
			mouse_c,
			j,
			*bp = buf;
	extern int	srch_pattern_len;

	*msg = (char *) 0;
	while ((c = *str++) != '\0') {
		if (c != '\\') {
			*bp++ = (int) c;
			continue;
		}
		if ((c = *str++) == 'e') {
			*bp++ = (int) '\033';
			continue;
		} else if (c == '\\') {
			*bp++ = (int) '\\';
			continue;
		} else if (c == '^') {
			if ((c = *str++) == '?') {
				*bp++ = (int) '\177';
				continue;
			} else {
				*bp++ = (int) c & 037;
				continue;
			}
		} else if (c == 'm' || c == 'h' || c == 'v') {
			switch(c) {
			case 'm': mouse_c = FIRST_MOUSE; break;
			case 'h': mouse_c = FIRST_HSCROLL; break;
			case 'v': mouse_c = FIRST_VSCROLL; break;
			default:
				sprintf(pkeys_ebuf, 
				  "illegal key escape %c in %s.  (valid: \m, \h and \v).",
				  c, str);
				*msg = pkeys_ebuf;
				return(-1);
			}
			c = *str++;
			if (c == '\\') {
				if ((c = *str++) == '^') {
					mouse_c += CONTROL_MOUSE;
					c = *str++;
				} else {
					sprintf(pkeys_ebuf,
					  "illegal \\ inside a key escape %s",
					   str);
					*msg = pkeys_ebuf;
					return(-1);
				}
			}
			if (isupper(c)) {
				mouse_c++;
				c = tolower(c);
			}
			switch(c) {
			case 'l': mouse_c += LEFT_MOUSE; break;
			case 'm': mouse_c += MIDDLE_MOUSE; break;
			case 'r': mouse_c += RIGHT_MOUSE; break;
			default:
				sprintf(pkeys_ebuf, 
				  "illegal button char %c in key escape %s. (legal: l, m, r).",
				  c, str);
				*msg = pkeys_ebuf;
				return(-1);
			}
			*bp++ = mouse_c;
			continue;
		} else if (isdigit(c)) {
			digit = 0;
			for (j = 0; j != 3; j++) {
				if (c < '8') {
					digit = digit * 8 + c - '0';
				}
				c = *str++;
				if (!isdigit(c)) {
					str--;
					break;
				}
			}
			*bp++ = digit;
		} else {
			*bp++ = (int) c;
		}
	}
	*bp = 0;

	*retp = buf;
	return(bp - buf);
}

/*
 * DOCUMENTATION
 *
 * Name: bind-to-key
 * Call: function-name key-strokes
 * Desc: This command ``binds'' a key (or keys)
 *	to a function that will be executed when that
 *	key is pressed.  The binding's life is the life of the 
 *	\lit{dvitool} that executed it; to get bindings which take effect
 *	on every instance of \lit{dvitool}, put the \em{bind-to-key}
 *	command in your startup file.  Control and mouse characters
 *	have ASCII representations which must be used in the startup
 *	file.  \em{Describe-key} displays what command a particular
 *	key sequence will invoke; as a side effect, it also displays
 *	the ASCII representation of the sequence, so it can be 
 *	used to easily determine the correct ASCII representation
 *	for any valid input.
 * SeeA: describe-key dump-bindings Startup-File
 */
bind_key(argv)
	func_arg	*argv;
{
	func		**fp = main_map,
			*bind_fp;
	char		*funcn = (*argv).str,
			*keystr,
			*emsg;
	int		done = 0,
			ch,
			meta_char,
			*rawkeys,
			len;
	extern char	*rc_linep;

	bind_fp = lookup_func_by_name(funcn);
	if (tl->in_rc) {
		/*
		 * the global rc_linep points to the first character
		 * after the last one read by the rc routines (in other
		 * words, it's pointing to the start of the string which
		 * names the key to bind to).
		 */
		if (rc_get_arg(&rc_linep, arg1buf) != 0) {
			msg(PLAIN, "%s needs two arguments!", cfp->name);
			return(-1);
		}
		if ((len = parse_keys(arg1buf, &rawkeys, &emsg)) < 0) {
			msg(PLAIN, "%s", emsg);
			return(-1);
		}
	} else {
		set_msg_cur(REG_MCUR);
		msg(APPEND|LITERAL, " ");
	}

	while (!done) {
		if (tl->in_rc) {
			if (len == 0) {
				msg(PLAIN, "%s: not enough characters!", arg1buf);
				return(-1);
			}
			ch = *rawkeys++;
			len--;
		} else {
			if (!meta_set) {
				ch = win_getc();
			} else {
				ch = meta_char;
				meta_set = 0;
			}
			if (meta_set) {
				meta_char = ch;
				ch = '\033';
			}

			if (ch == abort_ch) {
				msg(PLAIN, abort_msg);
				goto clean_up;
			}
			msg(APPEND, "%s ", p_char(ch));
		}

		if (fp[ch] == (func *) 0) {
			done = 1;
		} else if (fp[ch]->funcp == pre_1) {
			fp = pref1_map;
		} else if (fp[ch]->funcp == pre_2) {
			fp = pref2_map;
		} else {
			done = 1;
		}
	}
	fp[ch] = bind_fp;

clean_up:
	if (!tl->in_rc) {
		set_msg_cur(NO_MCUR);
		meta_set = 0;
	}
	return(0);
}

/*
 * DOCUMENTATION
 *
 * Name: describe-key
 * Call: key-strokes
 * Desc: This command identifies what command a sequence of keystrokes
 *	will invoke.  In the process it also echos the ASCII
 *	character sequences \lit{dvitool} uses to represent all of 
 *	the different input combinations.
 *	This means that you don't have to remember that
 *	control--shift--middle--mouse button is represented as
 *	\lit{\m\^M}; \em{describe-key} will tell you that.
 * SeeA: bind-to-key dump-bindings
 */
describe_key()
{
	func	**map = main_map,
		*fp;
	int	c,
		meta_char;

	set_msg_cur(REG_MCUR);
	msg_c(' ');
	for (;;) {
		if (!meta_set) {
			c = win_getc();
		} else {
			c = meta_char;
			meta_set = 0;
		}
		if (meta_set) {
			meta_char = c;
			c = '\033';
		}
		msg(APPEND, "%s ", p_char(c));
		fp = map[c];
		if (fp == (func *) 0) {
			msg(APPEND, "is unbound");
			break;
		} else if (fp->funcp == pre_1) {
			map = pref1_map;
			continue;
		} else if (fp->funcp == pre_2) {
			map = pref2_map;
			continue;
		}
		msg(APPEND, "is bound to %s", fp->name);
		break;
	}
	set_msg_cur(NO_MCUR);
	return(0);
}

/*
 * DOCUMENTATION
 *
 * Name: dump-bindings
 * Desc: This command creates a file in the current directory
 *	called \lit{dvitool.bindings} that 
 *	describes which command each of the possible keyboard and mouse 
 *	combinations will invoke.
 * SeeA: bind-to-key dump-commands
 */

char	dfname[] = "%s/%s.bindings";
dump_bindings()
{
	FILE		*dfd;
	char		bfname[128];
	extern tl_data	*tl;
	
	sprintf(bfname, dfname, dvi->cwd, tl->prog_name);
	if ((dfd = fopen(bfname, "w")) == (FILE *) 0) {
		msg(PERROR, "couldn't open bindings file: %s", bfname);
	}
	msg(PLAIN, "dumping bindings to %s...", bfname);
	dump_b_tbl(dfd, main_map, "");
	fclose(dfd);
	msg(APPEND, "done.");
}

char *
a_type(fp)
	int	(*fp)();
{
	if (fp == (int (*)()) 0) {
		return("");
	} else if (fp == a_func) {
		return("function name");
	} else if (fp == a_str) {
		return("string");
	} else if (fp == a_int) {
		return("integer");
	} else if (fp == a_fname) {
		return("file name");
	} else if (fp == a_var) {
		return("variable name");
	} else if (fp == a_font) {
		return("font name");
	} else if (fp == a_vhelp) {
		return("variable help name");
	} else if (fp == a_fhelp) {
		return("command help name");
	} else if (fp == a_ohelp) {
		return("overview help word");
	} else if (fp == a_lstr) {
		return("literal string");
	}
	return("bad function pointer type!");
}
	
static char	template[] = "%-26s%-16s%-16s%s\n";

dump_cmds_vars(fp)
	FILE	*fp;
{
	func		*funcp,
			**map = main_map;
	char		*(*writer)(),
			*type,
			null[1],
			key_buf[128];
	int		count;
	variable	*vp;
	extern char	*wv_int(),
			*wv_str(),
			*wv_bool(),
			*wv_dim();


	fprintf(fp, "dvitool commands\n\n");
	fprintf(fp, template, "name", "first arg", "second arg", "bound to");
	for (funcp = commands; funcp->name != (char *) 0; funcp++) {
		count = 0; null[0] = key_buf[0] = '\0';
		fprintf(fp, template, funcp->name, 
		  a_type(funcp->arg0), a_type(funcp->arg1),
		  (recur_one_binding(map, funcp->name, key_buf, &null[0],
		  0, &count) == 0) ? "" : key_buf);
	}

	fprintf(fp, "dvitool variables\n\n");
	fprintf(fp, template, "name", "type", "current val", "");
	for (vp = vars; vp->name != (char *) 0; vp++) {
		switch(vp->flags & V_ALL_TYPES) {
		case V_INT:
			writer = wv_int; type = "int";
			break;
		case V_STR:
			writer = wv_str; type = "string";
			break;
		case V_BOOL:
			writer = wv_bool; type = "boolean";
			break;
		case V_DIM:
			writer = wv_dim; type = "dimension";
			break;
		default:
			fprintf(fp, "type error at %s\n", vp->name);
			return;
		}
		fprintf(fp, template, vp->name, type, (*writer)(vp), "");
	}
}


/*
 * DOCUMENTATION
 *
 * Name: dump-commands
 * Desc: This command creates a file in the current directory
 *	called \lit{dvitool.commands} that describes each of the 
 *	\lit{dvitool} commands, the type of arguments that it takes,
 *	and the key strokes it is bound to separated by commas.
 *	In addition, all of the \lit{dvitool} variables and the their types
 *	are described.
 * SeeA: dump-bindings
 */
char	dcfname[] = "%s/%s.commands";
dump_commands()
{
	FILE		*dfd;
	char		exfname[128];
	extern tl_data	*tl;

	sprintf(exfname, dcfname, dvi->cwd, tl->prog_name);
	if ((dfd = fopen(exfname, "w")) == (FILE *) 0) {
		msg(PERROR, "couldn't open commands file: %s", exfname);
	}
	msg(PLAIN, "dumping commands/vars to %s...", exfname);
	dump_cmds_vars(dfd);
	fclose(dfd);
	msg(APPEND, "done.");
}

char	ctrl[] = "\\^ ",
	buf[7],
	unknown_key[] = "unknown key";

char *
p_char(ch)
	int	ch;
{
	if (ch == '\177') {
		return("^?");
	}
	if (ch == '\033') {
		return("\\e");
	}
	if (ch < ' ') {
		ctrl[2] = ch + '\100';
		return(ctrl);
	}
	if (ch < FIRST_MOUSE) {
		buf[0] = ch;
		buf[1] = '\0';
		return(buf);
	}
	if (ch >= FIRST_MOUSE && ch <= LAST_VSCROLL) {
		if (ch < FIRST_HSCROLL) {
			strcpy(buf, "\\m");
			ch -= FIRST_MOUSE;
		} else if (ch < FIRST_VSCROLL) {
			strcpy(buf, "\\h");
			ch -= FIRST_HSCROLL;
		} else if (ch <= LAST_VSCROLL) {
			strcpy(buf, "\\v");
			ch -= FIRST_VSCROLL;
		} else {
			return(unknown_key);
		}
		switch(ch) {
		case LEFT_MOUSE:
			strcat(buf, "l");
			break;
		case LEFT_MOUSE + SHIFT_MOUSE:
			strcat(buf, "L");
			break;
		case LEFT_MOUSE + CONTROL_MOUSE:
			strcat(buf, "\\^l");
			break;
		case LEFT_MOUSE + SHIFT_MOUSE + CONTROL_MOUSE:
			strcat(buf, "\\^L");
			break;
		case MIDDLE_MOUSE:
			strcat(buf, "m");
			break;
		case MIDDLE_MOUSE + SHIFT_MOUSE:
			strcat(buf, "M");
			break;
		case MIDDLE_MOUSE + CONTROL_MOUSE:
			strcat(buf, "\\^m");
			break;
		case MIDDLE_MOUSE + SHIFT_MOUSE + CONTROL_MOUSE:
			strcat(buf, "\\^M");
			break;
		case RIGHT_MOUSE:
			strcat(buf, "r");
			break;
		case RIGHT_MOUSE + SHIFT_MOUSE:
			strcat(buf, "R");
			break;
		case RIGHT_MOUSE + CONTROL_MOUSE:
			strcat(buf, "\\^r");
			break;
		case RIGHT_MOUSE + SHIFT_MOUSE + CONTROL_MOUSE:
			strcat(buf, "\\^R");
			break;
		default:
			return(unknown_key);
			break;
		}
	}
	return(buf);
}

char	*un = "unbound";

extern	int	pre_1(),
		pre_2();

dump_b_tbl(fd, tbl, prefix)
	FILE		*fd;
	func		*tbl[];
	char		*prefix;
{
	func		*prev = 0,
			*p = 0;
	int		item;
	char		*func_name,
			sv_prefix[5];
	struct map_tbl	*maps;

	for (item = 0; ; item++) {
		p = tbl[item];
		if (p == (func *) -1) {
			return;
		}
		if (p == 0) {
			func_name = un;
		} else {
			func_name = p->name;
		}
		fprintf(fd, "%s\t%s\t%s\n", prefix, p_char(item), func_name);
		if (p == 0)
			continue;
		if (p->funcp == pre_1) {
			dump_b_tbl(fd, pref1_map, strcpy(sv_prefix,
			  p_char(item)));
		}
		if (p->funcp == pre_2) {
			dump_b_tbl(fd, pref2_map, strcpy(sv_prefix,
			  p_char(item)));
		}
	}
}

/*
 * DOCUMENTATION
 *
 * Name: bound-to
 * Call: command name
 * Desc: This command describes all of the key strokes that will invoke
 *	\sym{command name} separated by commas.
 * SeeA: dump-bindings
 */
bound_to(argv)
	func_arg	*argv;
{
	char	*name = (*argv).str,
		null[1],
		key_buf[128];
	func	**map = main_map;
	int	count = 0;

	null[0] = '\0';
	if (recur_one_binding(map, name, key_buf, &null[0], 0, &count) == 0) {
		msg(APPEND, " is not bound to anything.");
	} else {
		msg(APPEND, " %s", key_buf);
	}
	return(0);
}

/*
 * return the first string of keystrokes (lexicographically) that will
 * invoke the named command.  If the command is unbound, return the
 * command name and -1.
 */
one_binding(name, keys)
	char	*name,
		*keys;
{
	func	**map;
	int	count = 0;
	char	null[1];

	null[0] = '\0';
	map = main_map;
	return(recur_one_binding(map, name, keys, &null[0], 1, &count));
}

recur_one_binding(map, name, keys, so_far, just_one, count)
	func	**map;
	char	*name,
		*keys,
		*so_far;
	int	*count;
{
	int	stat,
		j;
	func	*scan,
		**next_map;
	char	*cp,
		*str,
		one_prefix[4];

	cp = keys;
	for (j = 0; ; j++) {
		scan = map[j];
		if (scan == (func *) -1) {
			if (*count == 0 && map == main_map) {
				/*
				 * we've reached the end of this table
				 * without finding a match.
				 */
				strcpy(keys, name);
			}
			return(*count);
		}
		if (scan == (func *) 0) {
			continue;
		}
		if (*scan->name == *name) {
			/* first character matchs, try the rest. */
			if (!strcmp(scan->name, name)) {
				/* we've found a match. */
				if (*count != 0) {
					*cp++ = ',';
					*cp++ = ' ';
				}
				(*count)++;
				str = so_far;
				if (*str != '\0') {
					for (; *str != '\0';) {
						*cp++ = *str++;
					}
					*cp++ = ' ';
				}
				str = p_char(j);
				for (; *str != '\0';) {
					*cp++ = *str++;
				}
				*cp = '\0';
				if (just_one) {
					return(1);
				}
			}
		}
		if (scan->funcp == pre_1) {
			next_map = pref1_map;
		} else if (scan->funcp == pre_2) {
			next_map = pref2_map;
		} else {
			continue;
		}

		strcpy(one_prefix, p_char(j));
		
 		recur_one_binding(next_map, name, cp, &one_prefix[0], just_one, count);
		if (just_one && *count == 1) {
			return(*count);
		}
		/* scan past any bindings our recursive call found. */
		for (; *cp != '\0'; cp++)
			;
		continue;
	}
}

/*
 * this routine simply shows the user all of the names of the commands
 * available to her.
 */
static char	list_msg[] = "hit any space to page, any key to erase...";

/*
 * DOCUMENTATION
 *
 * Name: list-all-commands
 * Desc: This command simply displays the names of all of the commands.
 * SeeA: dump-commands list-all-variables
 */
list_cmds()
{
	char	**scan;
	int	count;

	/*
	 * build the completion list for the first time if necessary.
	 */
	if (func_names == (char **) 0) {
		functionp = commands;
		func_names = compl_list(next_func);
	}
	/* count how many function names we have. */
	for (count = 0, scan = func_names; *scan != (char *) 0;
	  count++, scan++) {
	  	;
	}
	msg(PLAIN, list_msg);
	(void) typeout(func_names, count, T_COL_BY_ROW, scan);
	clear_msg_text();
	return(0);
}

/*
 * DOCUMENTATION
 *
 * Name: list-all-variables
 * Desc: This command simply displays the names of all of the variables.
 * SeeA: dump-commands list-all-commands
 */
list_vars()
{
	char	**scan;
	int	count;

	/*
	 * build the completion list for the first time if necessary.
	 */
	if (var_names == (char **) 0) {
		varp = vars;
		var_names = compl_list(next_var);
	}
	/* count how many variable names we have. */
	for (count = 0, scan = var_names; *scan != (char *) 0;
	  count++, scan++) {
	  	;
	}
	msg(PLAIN, list_msg);
	(void) typeout(var_names, count, T_COL_BY_ROW, scan);
	clear_msg_text();
	return(0);
}

/*
 * DOCUMENTATION
 *
 * Name: ansi-keys
 * Desc: This function interprets the codes sent by the Sun function keys
 *	and attempts to do something intelligent with them.  The best 
 *	solution would be to be able to map each of them to a function
 *	just like any other key.  But since the function keys generate
 *	strings of characters, it isn't easy to incorporate them into 
 *	\lit{dvitool}'s key binding scheme.  So instead
 *	\lit{dvitool} interprets each
 *	sequence and runs a command from a private key map, much like
 *	the key map the menu uses.  The person who installed \lit{dvitool}
 *	can change the default bindings for the function keys; the
 *	normal defaults are ``unbound'' for every key except the arrow
 *	keys on the right keypad which scroll, L5 which runs 
 *	\em{toggle-tool}, and L7 which runs \em{close-window}.
 * 	These binding are site-wide; once compiled in by the installer,
 *	they cannot be changed by individual users.
 *	This command is something of a hack; it was never intended to be
 *	run intentionally by the user.  It's ability to interpret the
 *	ANSI sequences depends on it being bound to \lit{\e[} and you
 *	will after you type some additional input, you will get innocuous
 *	error messages if you run \em{ansi-keys} manually.  For all its
 *	short comings though, it does suppress the unwanted characters 
 *	that result when your fingers slip onto one of the function keys.
 * SeeA: enable-ansi-keys
 */

/*
 * The sun implementation of the function key handler has bugs in it; in
 * particular, one cannot ask for a single event from all of the function,
 * left keys and right keys.  Some of them will generate a single event,
 * while others, notably the arrow keys in the numeric pad, will always
 * generate their ANSI sequence.  This behavior is in SunView 3.0.  So it
 * seems that the best way to attack this problem is just to interpret
 * the ANSI sequences and do something with them.  The range of strings
 * we get are:
 *
 * \e[A to \e[D	are the numeric pad arrow keys.
 * the rest of the keys generate \e[193z to \e[232z.
 */
static char	disa[] = "[disabled]";

ansi_keys()
{
	char		strbuf[4];
	int		nc,
			read = 0;
	extern int	enable_ansi;

	/* we we get called we've read the \e[. */
	for(;;) {
		nc = win_getascii();
		if (read == 0 && nc >= 'A' && nc <= 'D') {
			if (enable_ansi) {
				(void) exec_command(ansi_map[nc - 'A']);
			} else {
				msg(LITERAL, disa);
			}
			return(0);
		}
		if (nc >= '0' && nc <= '9' && read < 3) {
			strbuf[read++] = nc;
			continue;
		}
		if (nc == 'z' && read == 3) {
			/* we've found the end of the string. */
			if (str2int(strbuf, 10, 193, 232, &nc) == (char *) 0) {
				return(0);
			}
			if (enable_ansi) {
				(void) exec_command(ansi_map[nc - 193 + 4]);
			} else {
				msg(LITERAL, disa);
			}
			return(0);
		}
		msg(LITERAL, (enable_ansi) ? "unknown ANSI sequence." : disa);
		break;
	}
	return(0);
}
