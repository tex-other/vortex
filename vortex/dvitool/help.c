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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/help.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

#include "structs.h"
#include "constants.h"

/*
 * these routines do the help stuff.  all we do is save the state of the
 * current document and read in our help file to be displayed.
 */

extern char	*vhelp_names[],
		*fhelp_names[],
		*ohelp_names[],
		last_fhelp[],
		last_vhelp[],
		last_ohelp[];

#define		STACK_DEPTH	(10)

static char	over_file[] = OHELPFILE,
		cmd_file[] = FHELPFILE,
		var_file[] = VHELPFILE,
		over_template[] = "%s",
		cmd_template[] = "Name: %s",
		var_template[] = "Name: %s",
		search_str[512],
		**words;

/*
 * DOCUMENTATION
 *
 * Name: help-overview
 * Call: help overview string
 * Desc: This command displays the overview file which describes
 *	the general mechanisms of \lit{dvitool}.  The optional string
 *	argument describes a search string to search for in the
 *	overview file.  If completion is used to generate the string,
 *	a match is guaranteed; however, the user may search for arbitrary
 *	strings in the overview file.
 * SeeA: help-commands help-variables
 */
help_over(argv)
	func_arg	*argv;
{
	char	*str = (*argv).str;

	/* check to see if the user got the string from a completion. */
	words = ohelp_names;
	if (compl_expand(str, words, search_str) == 1) {
		sprintf(search_str, over_template, str);
	} else {
		strcpy(search_str, str);
	}
	strcpy(last_ohelp, str);
	return(do_help(over_file, search_str));
}
/*
 * DOCUMENTATION
 *
 * Name: help-commands
 * Call: help command name
 * Desc: This command displays the \lit{dvitool} command help
 *	file and then searchs for the optional argument in that
 *	file.  The argument is subject to completion.  If completion
 *	is used to generate the search string, a match is guaranteed.
 *	The user may type in an arbitrary string as an argument.
 * SeeA: help-overview help-variables
 */

help_cmds(argv)
	func_arg	*argv;
{
	char	*str = (*argv).str;

	/* check to see if the user got the string from a completion. */
	words = fhelp_names;
	if (compl_expand(str, words, search_str) == 1) {
		sprintf(search_str, cmd_template, str);
	} else {
		strcpy(search_str, str);
	}
	strcpy(last_fhelp, str);
	return(do_help(cmd_file, search_str));
}
/*
 * DOCUMENTATION
 *
 * Name: help-variables
 * Call: help variable name
 * Desc: Like \em{help-commands}, this command displays the \lit{dvitool}
 *	variable help file and then searchs for an optional string in
 *	that file.  If completion is used to generate the search string,
 *	then a match is guaranteed; the user may supply arbitrary strings
 *	though.
 * SeeA: help-commands help-overview
 */
help_vars(argv)
	func_arg	*argv;
{
	char	*str = (*argv).str;

	/* check to see if the user got the string from a completion. */
	words = vhelp_names;
	if (compl_expand(str, words, search_str) == 1) {
		sprintf(search_str, var_template, str);
	} else {
		strcpy(search_str, str);
	}
	strcpy(last_vhelp, str);
	return(do_help(var_file, search_str));
}

typedef struct help_stack {
	char	*fname;
	int	ppage,
		x_view,
		y_view;
} h_stack;

static h_stack	stack[STACK_DEPTH];
static int	sp = 0;

do_help(fname, s_str)
	char	*fname,
		*s_str;
{
	char		str[10];
	extern int	srch_pattern_len;

	if (sp >= STACK_DEPTH) {
		one_binding("exit-help", str);
		msg(PLAIN,  "help nesting too deep!  use %s to exit help,",
		  str);
		one_binding("exit", str);
		msg(APPEND, " %s to exit dvitool.", str);
		return(0);
	}

	if (dvi->file == (FILE *) 0) {
		stack[sp].fname = (char *) 0;
	} else {
		stack[sp].fname = dvi->fname;
		stack[sp].ppage = dvi->cur_pg->ppage;
		stack[sp].x_view = h_view_start();
		stack[sp].y_view = v_view_start();
		dvi_close();
	}
	sp++;

	dvi->fname = fname;
	if (load_dvi_file((pg *) 0, 1, 0, 0) < 0) {
		msg(PLAIN, "can't open \"%s\".", fname);
		sp--;
		return(-1);
	}
	if (s_str != (char *) 0 && *s_str != '\0') {
		/* signal that we wish to search without fonts. */
		setup_search_font((char *) 0);
		/* more hackery. */
		srch_pattern_len = strlen(s_str);
		str_search(s_str, 1);
	}
	one_binding("exit-help", str);
	msg(PLAIN,  "use %s to exit help,", str);
	one_binding("exit", str);
	msg(APPEND, " %s to exit dvitool.", str);
	return(0);
}

/*
 * DOCUMENTATION
 *
 * Name: exit-help
 * Desc: This command exits from the last instance of help to the
 *	document you were viewing before you invoked help.  If the
 *	stack of help entries is empty, an error message is printed.
 * SeeA: help-commands help-overview help-variables
 */
exit_help()
{
	if (sp == 0) {
		msg(PLAIN, "help stack empty!  run one of the helps first.");
		return(0);
	}
	dvi_close();
	--sp;
	if (stack[sp].fname == (char *) 0) {
		display_no_file();
		return(0);
	} else {
		dvi->fname = stack[sp].fname;
		return(load_dvi_file((pg *) 0, stack[sp].ppage,
		  stack[sp].x_view, stack[sp].y_view));
	}
}

/*
 * The documentation was removed from the variable file where these
 * functions are defined because the variable file is parsed by mkdoc for
 * variable names and they were included in the variable help lists
 * instead of the command help lists.
 */
/*
 * DOCUMENTATION
 *
 * Name: print
 * Call: variable name
 * Desc: This function shows the value of \lit{dvitool}'s internal
 *	variables.
 * SeeA: set
 */

/*
 * DOCUMENTATION
 *
 * Name: set
 * Call: variable name  value string
 * Desc: This command is used to change the value of \lit{dvitool}'s 
 *	variables.  It can be run interactively, but is used most
 *	often in the user's \lit{.dvitoolrc} file to customize 
 *	\lit{dvitool}'s behavior.  This first argument is the name
 *	of the variable to change and the second argument is the
 *	value to change it to.  The second argument will of course
 *	vary depending on the type of the variable to be changed;
 *	to get the previous contents of the variable, type your
 *	\lit{rprnt} character (usually control-R, see \em{stty(1)}).
 * SeeA: print
 */

