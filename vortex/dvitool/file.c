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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/file.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

/* file.c.  Handle file name expansion and multiple file operations. */

#include "structs.h"
#include "constants.h"
#include "mag.h"
#include "fdecls.h"
#include <errno.h>
#include <pwd.h>

extern int	errno;

/*
 * DOCUMENTATION
 *
 * Name: find-file
 * Call: filename
 * Desc: This command is the main entry point for viewing a \lit{DVI} file.
 *	\em{Find-file} takes a filename, expands \lit{~} and \lit{$}
 *	characters into home directories and environment variables 
 *	respectively, appends \lit{.dvi} if necessary and attempts to read
 *	the file and display it.  Various error messages are issued if
 *	the named file is not a \lit{DVI} file or if it malformed etc.
 *	The argument filename is subject to completion, so at any time
 *	the user may enter a space character to attempt completion or
 *	a question mark to see the list of choices \lit{dvitool} will
 *	select from.  If a question mark or space character is needed
 *	in the filename itself, precede it with a backslash.
 *	
 *	In addition to completion, there is ``next file'' and 
 *	``previous file'' selection, used in conjuction
 *	with the \lit{DVI} files named on the command line.  Control-N
 *	gets the next file name off of the list and control-P gets the
 *	previous file name.  So to preview all of the chapters of
 *	your upcoming book, invoke \lit{dvitool} like this:
 *	\lit{% dvitool ch*.dvi} and then use control-N and control-P
 *	to select which file you'd like to view.  Just as with
 *	space and question mark, if you really want a control-N or
 *	a control-P in your filename, precede it with a backslash.
 */
find_file(argv)
	func_arg	*argv;
{
	char		*raw_fname,
			*cooked_fn;
	int		appended_ext;
	extern char	last_fname[];

	raw_fname = (*argv).str;
	if (*raw_fname == '\0') {
		msg(PLAIN, "null filename ignored.");
		return(-1);
	}
	strcpy(last_fname, raw_fname);

	if ((cooked_fn = str_save(parse_dvi_name(raw_fname, 1, &appended_ext))) == (char *) 0) {
		return(-1);
	}

	errno = 0;
	dvi->fname = cooked_fn;

	if (load_dvi_file((pg *) 0, 0, 0, 0) < 0 && errno == ENOENT) {
		if (!appended_ext) {
			msg(PLAIN, "can't open \"%s\".", cooked_fn);
			free(cooked_fn);
			dvi->fname = (char *) 0;
			return(-1);
		}
		if ((dvi->fname = str_save(parse_dvi_name(raw_fname, 0, &appended_ext))) ==
		  (char *) 0) {
			return(-1);
		}
		errno = 0;
		if (load_dvi_file((pg *) 0, 0, 0, 0) < 0 && errno == ENOENT) {
			msg(PLAIN, "can't open \"%s\" or \"%s\".",
			  cooked_fn, dvi->fname);
			free(cooked_fn); free(dvi->fname);
			dvi->fname = (char *) 0;
			return(-1);
		}
	}
	return(0);
}

/*
 * all of these parameters specify how things are to be loaded and
 * painted.  if a physical page number is specified, we look up the page
 * by that number, else if a pg * is passed, we use it, else we use the
 * first page.
 */
load_dvi_file(page, num, view_x, view_y)
	pg	*page;
{
	extern tl_data	*tl;

	push_cursor(HOUR_CUR);
	clear_msg();

	if (open_dvi_file() < 0) {
		display_no_file();
		pop_cursor();
		return(-1);
	}

	if (num != 0) {
		/*
		 * we will only fail to find the physical page number if
		 * the document has been shortened, so leave them at the
		 * last page of the file.
		 */
		if ((page = lookup_page_by_ppage(num)) == (pg *) 0)
			page = dvi->pages->last;
	}
	if (page == (pg *) 0) {
		page = dvi->pages->first;
	}

	(void) show_page(page, view_x, view_y, 1);
	pop_cursor();
	return(0);
}

/*
 * reread the dvi file because some display parameters (like
 * magnification) have changed.
 */
reload_this_file()
{
	/*
	 * we can't pass dvi->cur_pg here because the page list is freed
	 * when the file is closed.
	 */
	return(load_dvi_file((pg *) 0, dvi->cur_pg->ppage,
	  h_view_start(), v_view_start()));
}


open_dvi_file()
{
	if (dvi_open() != 0)
		return(-1);
	if (read_post_amble() != 0)
		return(-1);
	if (fill_in_page_ptrs() != 0)
		return(-1);
	return(size_page_image());
}


/*
 * open the dvi file.  fill in the appropriate data structures.  return 0
 * for success, -1 for failure.
 */

int
dvi_open()
{
	dvi_close();

	if (dvi->fname == (char *) 0) {
		msg(PLAIN, "filename is nil in open?");
		return(-1);
	} else 	if ((dvi->file = fopen(dvi->fname, "r")) == (FILE *) 0) {
		return(-1);
	}
	return(0);
}

/*
 * come here to close a dvi file.
 */
dvi_close()
{
	extern int	rereading;

	if (dvi->file != (FILE *) 0) {
		fclose(dvi->file);
		(void) free_all_pages();
		(void) unnumber_fonts();
	}

	if (!rereading) {
		dvi->user_mag = DEFAULT_MAG;
	}

	dvi->file = (FILE *) 0;
}

/*
 * add file names to the data structure to be examined at a later time.
 */
typedef struct dvi_files {
	char			*dvi_name;
	struct dvi_files	*previous,
				*next;
} fnames;
static fnames head_dvi_files = {
		(char *) 0,
		(fnames *) 0,
		(fnames *) 0
};
static fnames *cur_fname = &head_dvi_files,
		*tail = &head_dvi_files;

add_file_name(dvi_name)
	char *dvi_name;
{
	register fnames *tmp;
	
	tmp = (fnames *) alloc(sizeof (fnames));
	tmp->previous = tail;
	tmp->next = tail->next;
	tmp->dvi_name = dvi_name;
	tail->next = tmp;
	tail = tmp;
}

/*
 * This routine is called by the argument fetching routine to handle any
 * control characters.
 */
find_ctrl(ch, str, rtn_msg)
	char	**str,
		**rtn_msg;
{
	char	*fn;

	*rtn_msg = (char *) 0;
	if (ch == '\016') {
		/* control-n */
		if ((fn = next_file_name()) == (char *) 0) {
			*rtn_msg = "  [end of fname list]";
			return(0);
		}
		*str = fn;
		return(OVERWRITE);
	} else if (ch == '\020') {
		/* control-p */
		if ((fn = prev_file_name()) == (char *) 0) {
			*rtn_msg = "  [beginning of name list]";
			return(0);
		}
		*str = fn;
		return(OVERWRITE);
	}
	return(-1);
}

/*
 * return the next dvi file to be previewed, 0 on error.
 */

char *
next_file_name()
{
	if (cur_fname->next == (fnames *) 0)
		return((char *) 0);
	cur_fname = cur_fname->next;
	return(cur_fname->dvi_name);
}

char *
prev_file_name()
{
	if (cur_fname == &head_dvi_files ||
	  cur_fname->previous == &head_dvi_files)
		return((char *) 0);
	cur_fname = cur_fname->previous;
	return(cur_fname->dvi_name);
}


/*
 * do whatever is necessary to transform a single file name into a full path
 * name ready to be fopen()'d.  expansions done here include '~', '$', and
 * "../".  returns a pointer to a static buf, so this file name will go away
 * on the next call or 0 for an error.  we print our own error messages.
 */

#define DVIEXTENSION ".dvi"

char *
parse_dvi_name(name, append_ext, appended_ext)
	char	*name;
	int	*appended_ext;
{
	char	fullpath[MAXPATHLEN],
		fname[MAXPATHLEN],
		*extension;

	*appended_ext = 0;
	if (expand(name, fname) < 0) {
		msg(PLAIN, "%s", fname);
		return((char *) 0);
	}
	if ((name = do_dot_dot(fname, dvi->cwd)) == (char *) 0) {
		return((char *) 0);
	}
	if (*name == '/') {
		strcpy(fullpath, name);
	} else {
		strcpy(fullpath, dvi->cwd);
		strcat(fullpath, '/');
		strcat(fullpath, name);
	}
	if (!append_ext) {
		return(fullpath);
	}

	if ((extension = rindex(fullpath, '.')) != (char *) 0 &&
	  (strncmp(extension, DVIEXTENSION, sizeof (DVIEXTENSION))) == 0) {
	  	;
	} else if ((extension) && *(extension + 1) == '\0') {
		strcat(fullpath, DVIEXTENSION + 1);
		*appended_ext = 1;
	} else {
		strcat(fullpath, DVIEXTENSION);
		*appended_ext = 1;
	}
	return(fullpath);
}

/*
 * handle file names with ".."'s in them. handles bizarre cases like
 * "../../down/../up/newdir".  Also handles "./etc".  it returns 0 on error
 * and prints an error message
 */
char *
do_dot_dot(dot_dir, cwd)
	char	*dot_dir,
		*cwd;
{
	char		*cp = dot_dir,
			*upone,
			*endpath,
			*rindex();
	static char	fullpath[MAXPATHLEN];


	if (*dot_dir == '/')
		return(dot_dir);
	strcpy(fullpath, cwd);
	while(*cp) {
		if (*cp == '.' && *(cp+1) == '.' && *(cp+2) == '/') {
			if ((upone = rindex(fullpath, '/')) == (char *) 0) {
				msg(PLAIN,
				  "not enough parent directories for %s", 
				  dot_dir);
				return((char *) 0);
			} else {
				*upone = '\0';
				cp += 3;
			}
		} else if (*cp == '.' && *(cp+1) == '/') {
			cp += 2;
		} else {
			for (endpath = fullpath; *endpath; endpath++)
				;
			*endpath++ = '/';
			while (*cp && *cp != '/') {
				*endpath++ = *cp++;
			}
			if (*cp == '/')
				cp++;
			*endpath = '\0';
		}
	}
	return(fullpath);
}

/*
 * do whatever is necessary to halt processing of the dvi file after some
 * error and leave the tool in a reasonable state.
 */
abort_this_file()
{
	dvi_close();
	display_no_file();
}

display_no_file()
{
	clear_users_window();
	clear_page_image();
	restore_cursor();
	/* clear the title line of the filename etc. */
	msg(TITLE, " ");
}

/*
 * DOCUMENTATION
 *
 * Name: cd
 * Call: filename
 * Desc: This command is similiar to the \em{sh(1)} or \em{csh(1)}
 *	commands of the same name; it changes the directory.
 *	When it is given a null argument, it changes to the user's
 *	home directory.
 *	The working directory can be viewed with the \em{print}
 *	command; it can also be altered with the \em{set} command,
 *	but since \em{cd} takes a file name which can be completed
 *	on, and cwd is considered a string when changed with \em{set},
 *	it is generally easier to use \em{cd}.
 * SeeA: cwd print set
 */
set_wd(argv)
	func_arg	*argv;
{
	char		buf[MAXPATHLEN],
			*path;

	path = (*argv).str;

	if (*path == '\0') {
		strcpy(buf, tl->home);
	} else if (expand(path, buf) < 0) {
		msg(PLAIN, "cd: %s", buf);
		return;
	}
	if (chdir(buf) != 0) {
		msg(PERROR, "couldn't cd to %s", buf);
		return;
	}
	if (getwd(dvi->cwd) == 0) {
		msg(PERROR, "couldn't read cwd %s", dvi->cwd);
		return;
	}
	if (tl->created) {
		/* redisplay the file name since the cwd has changed */
		if (dvi->file != (FILE *) 0) {
			show_page_number(dvi->cur_pg);
		}
		msg(PLAIN, "working directory is now %s", dvi->cwd);
	}
}

