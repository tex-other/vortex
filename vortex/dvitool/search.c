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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/search.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

/*
 * a string search facility
 */

#include "structs.h"
#include "constants.h"
#include "fdecls.h"

char	pattern_buf[ARGBUF_SIZE];
int	patt_buf_len = 0;

/*
 * font numbers in TeX files (and hence in our search pages) are always
 * positive.
 */
static int	search_font = -1;
int		prev_search_f = -1,	/* the font from a previous search */
		/*
		 * this is a complete hack so we can search for strings
		 * with '\0' in them.
		 */
		srch_pattern_len = 0;

/*
 * DOCUMENTATION
 *
 * Name: search-forward
 * Call: literal-string
 * Desc: This command searches forward sequentially from a point on the 
 *	current	page for a string of characters in the \lit{DVI} file.
 *	The string is first parsed for any control sequences (any sequence
 *	of characters beginning with the backslash (\lit{`\'}) character.)
 *	Then the search begins, either at the first character on the page
 *	if there is no selection on the current page or at the first 
 *	character after the selection.  Each character in the search
 *	string is compared with a \lit{SET_CHAR} operand in the \lit{DVI}
 *	file or with a dvitool-created logical space character that
 *	matches an ASCII space character.
 *	The character matches if the integer parameter to the
 *	\lit{SET_CHAR} matches the integer value of the ASCII character.
 *	If the end of the search string is reached, a match is reported.
 *	Note that a match which spans a page boundary will not be found.
 *	The variables \em{kern-threshold} and \em{line-break-threshold}
 *	control how dvitool decides whether movements are interword
 *	spaces or kerns.
 *
 *	Since a match is based on the position of the character in the
 *	font, ligatures, in particular require some special handling.
 *	The ``fi'' ligature for example is at position 12 decimal
 *	in the roman font family.  This corresponds to the ASCII
 *	character control-L.  So to search for the word ``file'',
 *	you must use the string \lit{\^Lle}.  The command 
 *	\em{ascii-of-selection} greatly eases the task of computing
 *	the proper search string for a selection.
 *
 *	Searches can can be aborted prematurely by hitting the L1
 *	key (usually the key at the upper-left most position on the
 *	keyboard).  This is the same key that is used to reboot the
 *	machine, but you needn't hold it down like a control key
 *	to make it work; just depress it and release it.  
 *	Every so often \lit{dvitool} will check to see if the key
 *	has been depressed and will terminate the search if it has.
 *	This key is effective only in the search functions.
 * SeeA: ascii-of-selection kern-threshold line-break-threshold 
 *	search-backward search-forward-by-font
 */
forwd_str_search(argv)
	func_arg	*argv;
{
	char	*pattern = (*argv).str;

	search_font = -1;
	return(str_search(pattern, 1));
}

/*
 * DOCUMENTATION
 *
 * Name: search-backward
 * Call: literal-string
 * Desc: This command is exactly like \em{search-forward} except that 
 *	it searches backwards through the document instead of forwards,
 *	and the search is begun either from the last character of the
 *	page, or the character before the first character of the selection.
 *	
 * SeeA: search-backward-by-font search-forward
 */
backwd_str_search(argv)
	func_arg	*argv;
{
	char	*pattern = (*argv).str;

	search_font = -1;
	return(str_search(pattern, 0));
}

/*
 * DOCUMENTATION
 *
 * Name: search-forward-by-font
 * Call: font-name literal-string
 * Desc: This command is exactly like \em{search-forward} except that
 *	a \lit{SET_CHAR} must be in the named font to be a match.
 *
 *	\lit{Dvitool} keeps a cache of all the fonts you've used,
 *	so the fonts presented as choices for the \em{font-name}
 *	argument may or may not actually be in your DVI file.
 *	This isn't a major concern, however, since \lit{dvitool} will
 *	just issue an innocuous error message if you ask to restrict
 *	searching to a font that doesn't exist in your current DVI file.
 * SeeA: search-forward
 */
forwd_by_font(argv)
	func_arg	*argv;
{
	char	*font_name = (*argv).str,
		*pattern = argv[1].str;

	if (setup_search_font(font_name) < 0)
		return(-1);
	return(str_search(pattern, 1));
}

/*
 * DOCUMENTATION
 *
 * Name: search-backward-by-font
 * Call: font-name  literal-string
 * Desc: This command is exactly like \em{search-backward} except that
 *	a \lit{SET_CHAR} must be in the named font to be a match.
 * SeeA: search-backward search-forward
 */
backwd_by_font(argv)
	func_arg	*argv;
{
	char	*font_name = (*argv).str,
		*pattern = argv[1].str;

	if (setup_search_font(font_name) < 0)
		return(-1);
	return(str_search(pattern, 0));
}

/*
 * lookup the name of the font to convert it to its number and store that
 * in the global search_font.  return 0 on success, -1 otherwise.
 */
setup_search_font(font)
	char	*font;
{
	int	font_num;

	if (font == (char *) 0) {
		/* set up for a plain (no font) search. */
		search_font = -1;
		return(0);
	}
	if (*font == '\0') {
		if (prev_search_f == -1) {
			msg(PLAIN, "no previous font name or font cache has changed");
			return(-1);
		} else {
			search_font = prev_search_f;
			return(0);
		}
	}


	if ((font_num = font_name_to_num(font)) < 0) {
		msg(PLAIN, "font %s is not in this document", font);
		return(-1);
	}
	search_font = prev_search_f = font_num;
	return(0);
}

str_search(pattern, forwd)
	char		*pattern;
{
	register s_char	*scan;
	register char	*cp,
			*dp;
	register pg	*scan_pg = dvi->cur_pg;
	register int	stat;
	int		rtn_val = 0;
	extern int	interrupted;

	/* pattern in pointing to a string terminated by 3 '\0' bytes. */
	if (srch_pattern_len == 0) {
		if (patt_buf_len == 0) {
			msg(PLAIN, "0 length search string?");
			return(0);
		} else {
			pattern = pattern_buf;
		}
	} else {
		/*
		 * save off the current string in case they type the null
		 * string next time.
		 */
		cp = pattern;
		dp = pattern_buf;
		patt_buf_len = srch_pattern_len;
		while (srch_pattern_len--) {
			*dp++ = *cp++;
		}
	}
	/*
	 * show the user that this operation may take some time.
	 */
	push_cursor(HOUR_CUR);
	/*
	 * this page may have been thrown out of the cache by some
	 * previous search, so make sure we've got a page to search on.
	 */
	if (scan_pg->s_buf == (s_char *) 0) {
		if (load_page(scan_pg) < 0) {
			rtn_val = -1;
			goto clean_up;
		}
	}
	/*
	 * if there already is a selection on this page, start after the
	 * character after the beginning of that match, else pick the
	 * first character on this page and begin from there for forward
	 * searches.  Do the obvious corresponding thing for reverse
	 * searches.
	 */
	if (dvi->sel_start == (s_char *) 0 || dvi->cur_pg != dvi->sel_pg) {
		/*
		 * we should find the visible first character on the page
		 * here, but for now just grab the first char... GROT.
		 */
		scan = (forwd) ? scan_pg->s_buf + 1
		  : scan_pg->s_buf_end - 1;
	} else {
		scan = (forwd) ? dvi->sel_end + 1 : dvi->sel_start - 1;
		(void) null_selection();
	}

	interrupted = 0;
	while (scan_pg != (pg *) 0) {
		if ((stat = search_page(scan, pattern, forwd)) < 0) {
			rtn_val = -1;
			goto clean_up;
		} else if (stat == 1) {
			show_sel(scan_pg, SHOW_SEL, 0);
			dvi->sel_pg = scan_pg;
			rtn_val = 0;
			goto clean_up;
		}
		scan_pg = (forwd) ? scan_pg->next : scan_pg->prev;
		if (scan_pg != (pg *) 0) {
			if (interrupted) {
				msg(APPEND|OVERWRITE, "(interrupted)");
				interrupted = 0;
				rtn_val = 0;
				goto clean_up;
			}
			msg(APPEND|OVERWRITE, "...%d",
			  (int) scan_pg->count[0]);

			if (load_page(scan_pg) < 0) {
				rtn_val = -1;
				goto clean_up;
			}
			scan_pg->time_stamp = time(0);

			scan = (forwd)  ? scan_pg->s_buf + 1
					: scan_pg->s_buf_end - 1;
		}
	}
	msg(PLAIN, "search failed.");
	dvi->sel_start = dvi->sel_end = (s_char *) 0;
	dvi->sel_pg = (pg *) 0;
	rtn_val = 0;
clean_up:
	pop_cursor();
	return(rtn_val);
}

/*
 * starting from scan, search for the pattern specified in match.  This
 * search is done in a very simple-minded manner.  Matching is done on a
 * character by character basis, with the ASCII value of a character
 * compared to the value of the set_char cmd in the DVI file.  If the
 * global search_font is positive, only characters set from that font are
 * compared.  If we do find a match, we put the first char of the match
 * at sel_start and the last char at sel_end.
 */
search_page(scan, match, forwd)
	register s_char	*scan;
	char		*match;
	register int	forwd;
{
	register s_char	*sp = scan;
	register char	*cp;
	register int	len;

	for (; sp->c_type != S_END; (forwd) ? sp++ : sp--) {
		if (sp->c_type == S_SEARCHABLE && sp->c == *match) {
			if (search_font != -1 && search_font != (int) sp->f)
				continue;
			/*
			 * the first char matches, so check the rest.
			 * Note that we don't allow matching a space on
			 * the first character.
			 */
			/* save the start of the match... */
			scan = sp++;
			cp = match + 1;
			len = patt_buf_len - 1;
			for (;;) {
				if (len == 0) {
					/* we've matched */
					dvi->sel_start = scan;
					dvi->sel_end = --sp;
					return(1);
				}
				if (sp->c_type == S_END) {
					/*
					 * we've run off the page in a
					 * partial match, and that's not
					 * a match.
					 */
					return(0);
				}
				if (sp->c != *cp++) {
					/* we've just mismatched a char */
					break;
				}
				if (search_font != -1 && 
				  search_font != (int) sp->f) {
					/* we've just mismatched a font */
					break;
				}
				sp++;
				len--;
			}
			sp = (forwd) ? scan++ : --scan;
		}
	}
	return(0);
}
