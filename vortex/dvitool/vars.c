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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/vars.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

/* all of the declarations for variables are here.  */

#include "structs.h"
#include "constants.h"
#include "vars.h"
#include "fdecls.h"
#include "bits.h"
#include <errno.h>

extern int	errno;
extern char	cwd[];

/*
 * the first thing we need to define are all of the procedures to read
 * and write variables of the different types.  read and write here mean
 * from/to ascii.
 */
/*
 * all of these readers return non-zero if they changed the value they
 * are reading.
 */
rv_str(vp, src)
	variable	*vp;
	char		*src;
{
	char	*dest = (char *) vp->val;

	if (strcmp(src, dest) == 0) {
		return(0);
	}
	strcpy(dest, (strlen(src) > strlen(dest)) ? str_save(src)
	   : src);
	return(1);
}

rv_int(vp, src)
	variable	*vp;
	char		*src;
{
	int	val,
		*dest = (int *) vp->val,
		state,
		base,
		rtn_val;
	char	c,
		*scan,
		*stat;

	/*
	 * we allow the TeX input form here for hex and octal integers
	 * and also the C forms for those diehards among us.
	 */
	for (scan = src; *scan == ' ' || *scan == '\t'; scan++) {
		;
	}
	/*
	 * we must recognize the forms:
	 *	"num		TeX hex number
	 *	0xnum		C hex number
	 *	'num		TeX octal number
	 *	0num		C octal number
	 *	[1-9]*		decimal number
	 *	-num		negative decimal
	 *	default		error
	 */
	c = *scan;
	if (c >= '1' && c <= '9') {
		state = 0;
		base = 10;
	} else if (c == '"') {
		state = V_T_HEX;
		base = 16;
		scan++;
	} else if (c == '\'') {
		state = V_T_OCTAL;
		base = 8;
		scan++;
	} else if (c == '-') {
		state = 0;
		base = 10;
	} else if (c == '0') {
		scan++;
		if (*scan == '\0') {
			/* "0" alone -- decimal */
			--scan;
			state = 0;
			base = 10;
		} else if (*scan == 'x' || *scan == 'X') {
			state = V_C_HEX;
			base = 16;
			scan++;
		} else {
			state = V_C_OCTAL;
			base = 8;
		}
	} else {
		msg(PLAIN, "%s is not an integer!", src);
		return(0);
	}

	/*
	 * convert the number according to it's base and check for error
	 * conditions.
	 */
	if (str2int(scan, base, vp->un_def1, vp->un_def2, &val)
	  == (char *) 0) {
		if (errno == EDOM) {
			msg(PLAIN, "nothing changed.");
		} else if (errno == ERANGE) {
			msg(PLAIN,
			  "%s: %s out of range. must be >= %d and <= %d",
			  vp->name, src, vp->un_def1, vp->un_def2);
		}
		return(0);
	}

	/*
	 * set up the flags that tell how the variable is to be printed.
	 * use base as a scratch variable.
	 */
	base = vp->flags;
	/* clear the octal/hex flags */
	CLEAR_BITS(base, HEX_OCTAL_MASK);
	SET_BITS(base, state);
	vp->flags = base;

	rtn_val = (*dest != val);
	*dest = val;
	return(rtn_val);
}

rv_dim(vp, src)
	variable	*vp;
	char		*src;
{
	int	*dest = (int *) vp->val;
	char	*cp = src;
	float	val;

	if (sscanf(cp, "%f", &val) == 0) {
		msg(PLAIN, "couldn't read numeric part of dimen %s, %s not changed",
		  cp, vp->name);
		return(0);
	}
	while (isspace(*cp))
		cp++;
	/* skip the numeric part of the dimen */
	while (!isalpha(*cp) && *cp != '\0')
		cp++;
	if (*cp == '\0') {
		goto error;
	}
	/*
	 * interpret the dimension.  According to the TeXbook, pg 58,
	 * ``the maximum legal dimension is slightly less than 16384 pt.''
	 * we calculate limits based on that definition of a dimen.
	 */
	if (strcmp("pt", cp) == 0) {
		if (val >= 16383 || val < -16383) {
			msg(PLAIN, "point val %s out of range, %s not changed",
			  src, vp->name);
			return(0);
		}
		val *= 65536;
		vp->un_def1 = DIM_PT;
	} else if (strncmp("in", cp, 2) == 0) {
		/* 32767 / (2 * 72.27) = 226.698 */
		if (val > 226.698 || val < -226.5) {
			msg(PLAIN, "inch val %s out of range, %s not changed",
			  src, vp->name);
			return(0);
		}
		/* 72.27 * 2^16 = 4736286.7 */
		val *= 4736287;
		vp->un_def1 = DIM_IN;
	} else if (strncmp("sp", cp, 2) == 0) {
		/* 2^30 = 1073741824 */
		if (val > 1073741823 || val < -1073741824) {
			msg(PLAIN, "scaled pt val %s out of range, %s not changed",
			  src, vp->name);
			return(0);
		}
		vp->un_def1 = DIM_SP;
	} else if (strncmp("cm", cp, 2) == 0) {
		/* 32767 / (2 * 72.27) * 2.54 = 575.814 */
		if (val > 575.814 || val < -575) {
			msg(PLAIN, "cm val %s out of range, %s not changed",
			  src, vp->name);
			return(0);
		}
		/* 72.27 * 2^16 / 2.54 == 1864679.811023 */
		val *= 1864679.811023;
		vp->un_def1 = DIM_CM;
	} else {
		goto error;
	}

	*dest = (int) val;
	return(1);

error:
	msg(PLAIN, "must have one of \"in\", \"cm\", \"sp\", or \"pt\" in dimen %s; %s not changed",
	  src, vp->name);
	return(0);
}


static char	on[]  = "on",
		off[] = "off";

rv_bool(vp, src)
	variable	*vp;
	char		*src;
{
	int	val,
		*boolp = (int *) vp->val;

	if (*src == '0' ||
	    (strncmp(src, off, sizeof(off) - 1) == 0)) {
		val = 0;
	} else if (*src == '1' ||
	    (strncmp(src, on, sizeof(on) - 1) == 0)) {
		val = 1;
	} else {
		msg(PLAIN, "need one of: \"on\", \"off\", \"0\" or \"1\"");
		return(0);
	}
	if (val == *boolp) {
		return(0);
	}
	*boolp = val;
	return(1);
}

char *
wv_str(vp)
	variable	*vp;
{
	return((char *) vp->val);
}

static char	write_int_buf[20];

/*
 * we look at the flag bits to see if the value should be printed in hex
 * or octal.  the default is decimal.
 */
char *
wv_int(vp)
	variable	*vp;
{
	int	*p = (int *) vp->val;
	char	*format;

	if (vp->flags & V_C_HEX) {
		format = "0x%x";
	} else if (vp->flags & V_T_HEX) {
		format = "\"%x";
	} else if (vp->flags & V_C_OCTAL) {
		format = "0%o";
	} else if (vp->flags & V_T_OCTAL) {
		format = "'%o";
	} else {
		format = "%d";
	}
	sprintf(write_int_buf, format, *p);
	return(write_int_buf);
}

char *
wv_dim(vp)
	variable	*vp;
{
	int	*p = (int *) vp->val;
	float	dim;
	char	*fmt;
	
	switch(vp->un_def1) {
	case DIM_IN:
		dim = (float) *p / (65536 * 72.27);
		fmt = "%gin";
		break;
	case DIM_SP:
		dim = (float) *p;
		fmt = "%gsp";
		break;
	case DIM_PT:
		dim = (float) *p / 65536;
		fmt = "%gpt";
		break;
	case DIM_CM:
		dim = (float) (*p / (65536 * 72.27)) * 2.54;
		fmt = "%gcm";
		break;
	}
	sprintf(write_int_buf, fmt, dim);
	return(write_int_buf);
}

char *
wv_bool(vp)
	variable	*vp;
{
	int	*p = (int *) vp->val;

	if (*p == 0)
		return(off);
	else
		return(on);
}

/*
 * next we define the procedures that actually print and set variables.
 * these are the routines that are in the function key tables, i.e. these
 * are the main entry points to variable handling.
 */
print_var(argv)
	func_arg	*argv;
{
	char		*name = (*argv).str;
	variable	*vp,
			*lookup_var();
	char		*(*fp)();
	extern char	last_printed[];

	if ((vp = lookup_var(name)) == (variable *) 0) {
		msg(APPEND, " is not a variable", name);
		return(-1);
	}
	strcpy(last_printed, name);

	switch(vp->flags & V_ALL_TYPES) {
	case V_INT:
		fp = wv_int;
		break;
	case V_STR:
		fp = wv_str;
		break;
	case V_BOOL:
		fp = wv_bool;
		break;
	case V_DIM:
		fp = wv_dim;
		break;
	default:
		msg(PLAIN, "unknown type of var %s", vp->name);
		return(-1);
	}
	msg(APPEND, " => %s", (*fp)(vp));
	return(0);
}

static char	var_value[ARGBUF_SIZE];

set_var(argv)
	func_arg	*argv;
{
	char		*name = argv->str,
			*val_str;
	variable	*vp,
			*lookup_var();
	func_arg	*f,
			*get_arg();
	int		(*fp)(),
			a_str();
	char		*(*fpp)();
	extern tl_data	*tl;
	extern char	last_set[],
			arg1buf[],
			*rc_linep;
	extern func	*cfp;

	if ((vp = lookup_var(name)) == (variable *) 0) {
		msg(PLAIN, "%s is not a variable", name);
		return(1);
	}
	if (!tl->in_rc && strncmp("init-", name, 5) == 0) {
		msg(PLAIN, "%s is an initial value variable.  it cannot be changed.",
		  name);
		return(1);
	}
	strcpy(last_set, name);

	switch(vp->flags & V_ALL_TYPES) {
	case V_INT:
		fp = rv_int;
		fpp = wv_int;
		break;
	case V_STR:
		fp = rv_str;
		fpp = wv_str;
		break;
	case V_BOOL:
		fp = rv_bool;
		fpp = wv_bool;
		break;
	case V_DIM:
		fp = rv_dim;
		fpp = wv_dim;
		break;
	default:
		msg(PLAIN, "unknown type of var %s", vp->name);
		return(-1);
	}

	/*
	 * actually get the value to be interpreted from the user's
	 * keyboard or wherever.  We have to do this differently when
	 * we're reading the rc file.
	 */
	if (tl->in_rc) {
		if (rc_get_arg(&rc_linep, arg1buf) != 0) {
			msg(PLAIN, "%s needs two arguments!", cfp->name);
			return(-1);
		} 
		val_str = arg1buf;
	} else {
		push_cursor(MOUSE_CUR);
		msg_cursor_from_arg(a_str);
		/* get the default value of this variable. */
		strcpy(var_value, (*fpp)(vp));
		f = get_arg(arg1buf, a_str, (int (*)()) 0, var_value);
		pop_cursor();
		msg_cursor_from_arg((int (*)()) 0);
		if (f == (func_arg *) 0) {
			return(-1);
		}
		val_str = f->str;
	}

	/*
	 * read the new value and decide whether or not to call the
	 * action routine.
	 */
	if ((fp)(vp, val_str) != 0 && 
	     vp->changed != (int (*)()) 0 &&
	     (!tl->in_rc | !(vp->flags & V_NORC)) &&
	     (!(vp->flags & V_ODVI) || dvi->file != (FILE *) 0)) {
		return((vp->changed)(vp));
	}
	return(0);
}

/*
 * this routine gets called when the page cache changes size
 */
do_shrink_cache(vp)
	variable	*vp;
{
	return(shrink_cache(vp->val));
}

/*
 * one of the values we use to compute where lines break and whether
 * horizontal movements should be considered kerns or spaces has changed,
 * so throw away all of the pages and rebuild the current page image.
 */
s_char_val_changed(vp)
	variable	*vp;
{
	push_cursor(HOUR_CUR);
	free_all_load_images();
	if (load_page(dvi->cur_pg) < 0)
		return(-1);
	pop_cursor();
	return(0);
}

/*
 * open (or close) the message log file.
 */
FILE	*log_fp;

open_lfile(vp)
	variable	*vp;
{
	char 	*fname = vp->val;

	if (*fname == '\0') {
		if (log_fp != (FILE *) 0) {
			fclose(log_fp);
			log_fp = (FILE *) 0;
		}
		return(0);
	}
	if (log_fp != (FILE *) 0) {
		/* they want a new log file. */
		fclose(log_fp);
		log_fp = (FILE *) 0;
	}
	if ((log_fp = fopen(fname, "w")) == (FILE *) 0) {
		msg(PERROR, "couldn't open %s", fname);
		return(-1);
	}
	return(0);
}


int	abort_ch	= V_DEF_ABORT_CH,
	border_width	= V_DEF_BORDER_W,
	draw_borders	= V_DEF_DRAW_B,
	enable_ansi	= V_DEF_ANSI_KEYS,
	i_cursor_xhot	= V_DEF_CURSOR_XHOT,
	i_cursor_yhot	= V_DEF_CURSOR_YHOT,
	i_icon_x	= V_DEF_ICON_X,
	i_icon_y	= V_DEF_ICON_Y,
	i_iconic	= V_DEF_ICONIC,
	i_win_width	= V_DEF_WIN_W,
	i_win_height	= V_DEF_WIN_H,
	i_win_x		= V_DEF_WIN_X,
	i_win_y		= V_DEF_WIN_Y,
	kern_threshold	= V_DEF_KERN_THRESHOLD,
	left_mar	= V_DEF_LEFT_MAR,
	lbrk_threshold	= V_DEF_LBRK_THRESHOLD,
	page_limit	= V_DEF_PAGES_CACHED,
	pg_image_h	= V_DEF_PAGE_HEIGHT,
	pg_image_w	= V_DEF_PAGE_WIDTH,
	scrollbars	= V_DEF_SCROLLBARS,
	show_ld_image	= V_DEF_SHOW_LD_IMAGE,
	top_mar		= V_DEF_TOP_MAR;

char	font_path[MAXPATHLEN] = V_DEF_FONT_PATH,
	/*
	 * e_font_path is the font path after it has been expanded for
	 * '~' and '$' characters.
	 */
	e_font_path[2*MAXPATHLEN],
	i_icon_file[MAXPATHLEN] = V_DEF_ICON_FILE,
	i_cursor_file[MAXPATHLEN] = V_DEF_CURSOR_FILE,
	log_fname[MAXPATHLEN] = V_DEF_LOG_FILE;

/*
 * provide a stub to interface with the real cd command.
 */
do_cwd()
{
	func_arg	a;

	a.str = cwd;
	return(set_wd(&a));
}


/*
 * expand tilde's and dollar signs (home directories and environment
 * variables) in the fontpath.
 */
expand_path()
{
	char		*cp,
			*ecp,
			*next,
			buf[MAXPATHLEN];
	int		one_more = 1;

	ecp = e_font_path;
	for (cp = font_path; one_more != 0;) {
		next = index(cp, ':');
		if (next != (char *) 0) {
			/*
			 * turn the ':' into a '\0' and hand this part
			 * of the path to the expand routine.
			 */
			*next = '\0';
			one_more = 1;
		} else {
			one_more = 0;
		}
		if (expand(cp, buf) < 0) {
			strcpy(e_font_path, buf);
			msg(LITERAL, e_font_path);
			return(-1);
		}
		if (ecp != e_font_path) {
			*ecp++ = ':';
			*ecp = '\0';
		}
		for (cp = buf; *cp != '\0';) {
			*ecp++ = *cp++;
		}
		*ecp = '\0';

		if (ecp > e_font_path + sizeof(e_font_path)) {
			msg(FATAL, "font_path %s too long!", font_path);
		}

		if (one_more) {
			*next = ':';
			cp = next + 1;
		}
	}
	return(0);
}
		

/*
 * well, here's a good example of where C bites the dust.  I'd like to be able
 * to declare a union that would hold both ints and char *'s and then
 * initialize them in this table.  But C doesn't allow static initialization
 * of unions.
 */

extern int	reshow_pg(),
		reload_pg();

/*
 * note that the un_def fields are only used for non-initial-value
 * integers and dimens.
 */
#define BIGINT (2147483647)	/* 2^31 - 1 */

variable	vars[] = {
/*
 * DOCUMENTATION
 *
 * Name: abort-character
 * Type: integer
 * Defa: 0x7
 * Desc: This is the character that is recognized as the ``interrupt''
 *	character.  The default is control-G.
 *	Each character typed is compared to this value, and
 *	if it matches, the current action is aborted.
 *	While this value is
 *	interpreted as a character, it is stored as an integer.
 *	Thus to change your \em{abort-character} to the character `A',
 *	you should set \em{abort-character} to 65.
 */
{ "abort-character", 	(var) &abort_ch,	V_INT, 0, 0, 127 },

/*
 * DOCUMENTATION
 *
 * Name: border-width
 * Type: integer
 * Defa: 3
 * Desc: This is the width in pixels of the box that will be drawn around
 *	the page image.  This box is only drawn if the boolean
 *	\em{draw-borders} is true.
 * SeeA: draw-borders
 */

{ "border-width", 	(var) &border_width,	V_INT|V_NORC|V_ODVI, reshow_pg, 0, 100 },
/*
 * DOCUMENTATION
 *
 * Name: cached-pages
 * Type: integer
 * Defa: 3
 * Desc: This is the number of \lit{DVI} pages that \lit{dvitool} will
 *	cache.  A cached page does not have to be reread from the file 
 *	if it is viewed again, so redisplaying it is fast.  However,
 *	there is a
 *	substantial performance penalty due to swapping if \lit{dvitool}
 *	attempts to cache too many pages, so it behooves you to keep
 *	this variable small.  You can see the effects of various settings
 *	by searching for some non-existant string in a long \lit{DVI}
 *	file while watching various performance indicators (see 
 *	\em{perfmeter(1)}).  An empirical test
 *	(searching for a non-existant string in a 200 page document)
 *	showed an average reduction in total search time of 49 percent
 *	when a 2 page cache was used compared with a 10 page cache.
 *	A cache size of 2--4 pages seems optimal for most circumstances.
 */
{ "cached-pages",	(var) &page_limit,	V_INT|V_NORC, do_shrink_cache, 2, 30 },
/*
 * DOCUMENTATION
 *
 * Name: cwd
 * Type: string
 * Desc: This string is the exact analog to the shell's current working
 *	directory.  Though it can be altered via the \em{set} command,
 *	a more appropriate means of changing it is the \em{cd} command
 *	because \em{cd} expects an argument of type \em{filename} which
 *	can be completed on.  To \em{set}, \em{cwd} is just a string.
 * SeeA: cd set
 */

{ "cwd",		(var) cwd,		V_STR, do_cwd },
/*
 * DOCUMENTATION
 *
 * Name: draw-borders
 * Type: boolean
 * Defa: on
 * Desc: This variable controls whether a border of \em{border-width}
 *	pixels is drawn around the page image.
 * Side: Makes the page image larger, and thus costs some memory (about
 *	4000 bytes for a 3 pixel border).  You may wish to turn this
 *	off in a memory--starved environment.
 * SeeA: border-width
 */
{ "draw-borders",	(var) &draw_borders,	V_BOOL|V_NORC|V_ODVI, reshow_pg },

/*
 * DOCUMENTATION
 *
 * Name: enable-ansi-keys
 * Type: boolean
 * Default: on
 * Desc: This variable controls the way the function keys are interpreted.
 *	When true, the ANSI sequences generated by the function (and left
 *	and right) keys are interpreted and functions are run based on
 *	bindings that are changeable only at compile time.  When false,
 *	all the function keys simply print out an innocuous message.
 *	N.B. This information is only valid when the function 
 *	\em{ansi-keys} is bound to the string ``\lit{\e[}''.
 * SeeA: ansi-keys
 */
{ "enable-ansi-keys",	(var) &enable_ansi,	V_BOOL,0, 0, 0 },

/*
 * DOCUMENTATION
 *
 * Name: font-path
 * Type: string
 * Defa: \pass{\input fontpath }
 * Desc: This is a list of places to look for the font files (more correctly,
 *	for pk or pxl files) that \lit{dvitool} uses.  It is in the
 *	same format as the PATH of \em{csh(1)}, i.e. \lit{path:path...} where
 *	each path element may contain the \lit{~} character to reference
 *	a user's home directory or $NAME to reference any environment
 * 	variable.  It is an error to reference an undefined environment
 *	variable.
 *
 *	\lit{Dvitool} looks for both types (PK and PXL) of 
 *	font files when it goes searching for a font unless the directory
 *	of the pathname (one of the \em{path}'s above) contains the
 *	substring ``\lit{pk}'' or the substring ``\lit{pixel}.''
 *	If one of those two substrings are found, then \lit{dvitool} 
 *	will only look for that type of font file in that directory.
 *	So if you have some PXL and some PK files, it makes
 *	good sense to use a \em{font-path} something like this:
 *	\pass{\break\indent\indent\tt /usr/local/lib/fonts/pk:/usr/local/lib/fonts/pixel}
 */
{ "font-path",		(var) font_path,	V_STR, expand_path },
/*
 * DOCUMENTATION
 *
 * Name: init-cursor-file
 * Type: string
 * Defa: null
 * Desc: If set, this string points to a file that contains an image
 *	to be used as the default cursor for \lit{dvitool}.  The
 *	named file may contain a \lit{~} reference and it is presumed
 *	to be the output of \em{iconedit(1)}.  As with all the 
 *	\em{init-} variables, this variable can only be set in
 *	the user's \lit{.dvitoolrc} file.
 * SeeA: dvitoolrc init-cursor-xhot init-cursor-yhot init-icon-file
 */
{ "init-cursor-file",	(var) i_cursor_file,	V_STR, 0 },
/*
 * DOCUMENTATION
 *
 * Name: init-cursor-xhot
 * Type: integer
 * Defa: 9
 * Desc: This is the horizontal offset in pixels from the upper left 
 *	corner of the cursor to consider the ``hot spot'' of the
 *	cursor.  The hot spot of the cursor is the point considered
 *	to be the focal point of the cursor; it is a means of describing
 *	which of the \pass{$16^2$} pixels in the cursor image is the pixel 
 *	actually being pointed to.  The default is for the circle cursor;
 *	it should be changed if you load your own cursor with 
 *	\em{init-cursor-file}.
 * SeeA: init-cursor-file init-cursor-yhot
 */
{ "init-cursor-xhot",	(var) &i_cursor_xhot,	V_INT, 0, 0, 15 },
/*
 * DOCUMENTATION
 *
 * Name: init-cursor-yhot
 * Type: integer
 * Defa: 9
 * Desc: The vertical analog to \em{init-cursor-xhot}.
 * SeeA: init-cursor-file init-cursor-xhot
 */
{ "init-cursor-yhot",	(var) &i_cursor_yhot,	V_INT, 0, 0, 15 },
/*
 * DOCUMENTATION
 *
 * Name: init-icon-file
 * Type: string
 * Defa: null
 * Desc: This string names a file to be used to create the icon image
 *	for \lit{dvitool}.  It is assumed to be the output of
 *	\em{iconedit(1)}.
 * SeeA: init-cursor-file init-icon-x init-icon-y
 */
{ "init-icon-file",	(var) i_icon_file,	V_STR, 0 },
/*
 * DOCUMENTATION
 *
 * Name: init-icon-x
 * Type: integer
 * Defa: 1000
 * Desc: This is the horizontal position of the
 *	pixel at which the upper left hand corner of the
 *	\lit{dvitool} icon will be painted.
 * SeeA: close-window init-icon-y init-icon-file
 */
{ "init-icon-x",	(var) &i_icon_x,	V_INT, 0, 0, SCR_WIDTH },
/*
 * DOCUMENTATION
 *
 * Name: init-icon-y
 * Type: integer
 * Defa: 0
 * Desc: This is the vertical position of the pixel at which the upper
 *	left hand corner of the \lit{dvitool} icon will be painted.
 * SeeA: close-window init-icon-x init-icon-file
 */

{ "init-icon-y",	(var) &i_icon_y,	V_INT, 0, 0, SCR_HEIGHT },
/*
 * DOCUMENTATION
 *
 * Name: init-iconic
 * Type: boolean
 * Defa: off
 * Desc: When this variable is turned on, \lit{dvitool} appears in iconic
 * form when it is first painted.  This variable provides exactly the
 * same functionality as the \em{suntools(1)} flag \lit{-Wi}.  The most
 * common use of \em{init-iconic} is when \lit{dvitool} is invoked from a
 * \em{suntools(1)} menu (which doesn't allow the \lit{-Wi} flag).  When
 * \lit{dvitool} is started from a \lit{.suntools} file it is better to
 * specify the \lit{-Wi} flag on the command line so that when
 * \lit{dvitool} is invoked some other way, it will come up non-iconic.
 * SeeA: close-window init-icon-file init-icon-x init-icon-y
 */
{ "init-iconic",	(var) &i_iconic,	V_BOOL, 0, 0, 0 },

/*
 * DOCUMENTATION
 *
 * Name: init-scrollbars-on
 * Type: boolean
 * Defa: on
 * Desc: This variable controls whether or not \lit{dvitool} will be created
 *	with scrollbars.
 */
{ "init-scrollbars-on",	(var) &scrollbars,	V_BOOL,0 },
/*
 * DOCUMENTATION
 *
 * Name: init-win-height
 * Type: integer
 * Defa: 400
 * Desc: This is the height in pixels that \lit{dvitool} will have 
 *	when it is first created.
 * SeeA: full-screen init-win-width zoom-tool
 */

{ "init-win-height",	(var) &i_win_height,	V_INT, 0, 50, SCR_HEIGHT },
/*
 * DOCUMENTATION
 *
 * Name: init-win-width
 * Type: integer
 * Defa: 1064
 * Desc: This is the width in pixels that \lit{dvitool} will have when it is
 *	first created.  The default value creates a window that is wide
 *	enough to see the entire width of an 8.5 X 11 inch page with a 
 *	3 pixel wide border and 1 inch margins.
 * SeeA: full-screen init-win-height zoom-horizontal
 */
{ "init-win-width",	(var) &i_win_width,	V_INT, 0, 100, SCR_WIDTH },
/*
 * DOCUMENTATION
 *
 * Name: init-win-x
 * Type: integer
 * Defa: 88
 * Desc: This is the horizontal position of the pixel that the upper
 *	left hand corner of \lit{dvitool} will be initially drawn at.
 * SeeA: init-win-y
 */
{ "init-win-x",		(var) &i_win_x,		V_INT, 0, 0, SCR_WIDTH },
/*
 * DOCUMENTATION
 *
 * Name: init-win-y
 * Type: integer
 * Defa: 200
 * Desc: This is the vertical position of the pixel that the upper
 *	left hand corner of \lit{dvitool} will be initially drawn at.
 * SeeA: init-win-x
 */
{ "init-win-y",		(var) &i_win_y,		V_INT, 0, 0, SCR_HEIGHT },
/*
 * DOCUMENTATION
 *
 * Name: kern-threshold
 * Type: integer
 * Defa: 150
 * Desc: This variable controls how \lit{dvitool} interprets the stream
 *	of \lit{DVI} commands that it sees as ASCII characters.  
 *	Recall that a \lit{DVI} file consists of an arbitrary combination
 *	of ``set character'' commands interspersed with horizontal and
 *	vertical movement commands.  \lit{Dvitool} must have some way of
 *	deciding whether a horizontal movement is an interword movement,
 *	or a horizontal kern.  The prior command should be interpreted as
 *	an ASCII space, while the latter should not.  \em{Kern-threshold}
 *	is the break--point for that decision: horizontal movements greater
 *	than \em{kern-threshold} are considered to be ASCII spaces.
 *	The value compared with the horizontal movements is actually the
 *	width of the widest page mulitplied by the \em{kern-threshold}
 *	and a constant.
 *	Thus for large point sizes or small page widths, 
 *	\em{kern-threshold} may need to be increased.
 *	The default value works well for 10 point text.
 * SeeA: ascii-of-selection extend-selection line-break-threshold select-char
 */
{ "kern-threshold",	(var) &kern_threshold,	V_INT, s_char_val_changed, 0, 10000 },
/*
 * DOCUMENTATION
 *
 * Name: left-margin
 * Type: dimension
 * Defa: 1in
 * Desc: This is the width of the left margin.
 *	Characters may be typeset in the margins, but it is an error 
 *	to attempt to set a character to the left of the area allocated
 *	by \em{left-margin} which is ``off the page.'' 
 *	\lit{Dvitool} will automatically make the load image bigger when
 *	asked to set any characters in the margins (left, right, top or
 * 	bottom), but will complain if asked to set a character outside them.
 *	If you really want to see those characters, you must change
 *	\em{page-height} and \em{page-width}.
 *	As with all variables
 *	of type \em{dimension}, this value may be specified in either
 *	inches (in), scaled points (sp), points (pt), or centimeters
 *	(cm).
 * SeeA: page-height page-width show-load-image top-margin
 */
{ "left-margin",	(var) &left_mar,	V_DIM|V_NORC|V_ODVI, reload_pg, DIM_IN },
/*
 * DOCUMENTATION
 *
 * Name: line-break-threshold
 * Type: integer
 * Defa: 110
 * Desc: This value is a threshold over which vertical movements will be 
 *	considered line breaks.  A line break is considered a space in
 *	the search algorithm.  See \em{kern-threshold}.
 * SeeA: ascii-of-selection extend-selection kern-threshold select-char
 */
{ "line-break-threshold",(var) &lbrk_threshold,	V_INT, s_char_val_changed, 0, 10000 },
/*
 * DOCUMENTATION
 *
 * Name: log-filename
 * Type: string
 * Defa: null
 * Desc: When this string is non-null, it is the name of a log file in
 * 	which a copy of all of the messages \lit{dvitool} produces in it's
 * 	message window will be placed.  When the file is first openned, it is
 * 	truncated.  It is an error for the user not to be able to open the
 * 	file.  To turn message logging off, set \em{log-file} to the 
 *	null string by entering \lit{<return>} as the first character.
 */
{ "log-filename",	(var) log_fname,	V_STR, open_lfile },

/*
 * DOCUMENTATION
 *
 * Name: page-height
 * Type: dimension
 * Defa: 11in
 * Desc: The minimum height of the page image.  See \em{left-margin}.
 * SeeA: left-margin page-width top-margin
 */
{ "page-height",	(var) &pg_image_h,	V_DIM|V_NORC|V_ODVI, reshow_pg, DIM_IN },
/*
 * DOCUMENTATION
 *
 * Name: page-width
 * Type: dimension
 * Defa: 8.5in
 * Desc: The minimum width of the page image.  See \em{left-margin}.
 * SeeA: left-margin page-height top-margin
 */
{ "page-width",		(var) &pg_image_w,	V_DIM|V_NORC|V_ODVI, reshow_pg, DIM_IN },
/*
 * DOCUMENTATION
 *
 * Name: show-load-image
 * Type: boolean
 * Defa: off
 * Desc: When true, \lit{dvitool} will draw a 1 pixel wide box around
 *	the load image.  Each page has its own load image,
 *	while there is only 1 global page image.  The box reveals exactly
 *	which pixels are kept, or cached, for each page.
 */
{ "show-load-image",	(var) &show_ld_image,	V_BOOL|V_ODVI, reshow_pg },
/*
 * DOCUMENTATION
 *
 * Name: top-margin
 * Type: dimension
 * Defa: 1in
 * Desc: The height of the top margin.  See \em{left-margin}.
 * SeeA: left-margin show-load-image
 */
{ "top-margin",		(var) &top_mar,		V_DIM|V_NORC|V_ODVI, reload_pg, DIM_IN },
{ (char *) 0 }
};

variable *
lookup_var(name)
	char	*name;
{
	variable	*vp = vars;

	for(;;) {
		if (vp->name == (char *) 0 ||  vp->name[0] > *name) {
			return((variable *) 0);
		}
		if (strcmp(vp->name, name) == 0) {
			return(vp);
		}
		vp++;
	}
}

