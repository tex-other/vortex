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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/page.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

/*
 * control page movement.
 */

#include "structs.h"
#include "constants.h"
#include "mag.h"
#include "fdecls.h"
#include "vars.h"
#include <sunwindow/rect.h>

extern rectang	rectang_null;

/*
 * these data structures describe the various aspects of a ``page'' in
 * dvitool, including the page itself and the user's view onto the page.
 */
/*
 * page image: this is the logical page image, which is usually the largest
 *	image.  It includes the margins and the borders.  It is the
 *	top-most description of the page, and thus it has no parent
 *	rectangle.  If no dvi file has been opened for viewing yet,
 *	this image and its children the physical page image and
 *	the load images do not exist unless they are created by the
 *	typeout routines.
 * physical page image: this is the same as the page, image, but 
 *	without the borders.  It is the representation of the physical
 *	sheet of paper.  Its parent is the page image, so its x and
 *	y are relative to the page image.  If the variable that controls
 *	the border width is set to 0, this is identical to the page image.
 * load image: there is one of these for each page of the dvi file.
 *	It specifies the width and height of the minimum rect in which
 *	which characters have been painted and the x and y of the
 *	physical page image at which they should be painted.
 * the win_r rectang describes the physical window which is up on
 *	the user's screen.  It changes when the window is resized, but
 *	no other time.  It's left and top variables vary with the
 *	placement of the scrollbars -- if the scrollbars are placed at
 *	the NORTH and EAST edges, for example, win_r.r_left will be 0
 *	and win_r.r_top will be the width of the scrollbar.  The width
 *	and height fields describe the width and height of the window
 *	including the scrollbars.
 * view_r describes the part of the total dvi page that is
 *	currently visible in the user's window.  It's width and height
 *	are win_r's width and height minus the width of the
 *	scrollbars.  It's left and top are the offsets from the top
 *	left corner of pg_image where this view was painted.  In other
 *	words, as the user scrolls down the page, view_r.r_top
 *	increases.
 */
/*
 * Note that neither the physical page image nor the the view image
 * describe a real image, but rather describe a sub-portion of another
 * image.  The actual images (pixrects and pixwins in SunView) are
 * private to the particular implementation.  This is to ease the job of
 * porting to different environments (like X).  These rects are public to
 * all implementations of dvitool.  In fact much of this information is
 * duplicated in the private image descriptions; the redundancy is
 * intentional.
 */
rectang
	pg_image_r,
	phys_image_r,
	/* the load image rects are in the pgs list */
	win_r,
	view_r;


h_view_start()
{
	return(view_r.r_left);
}

v_view_start()
{
	return(view_r.r_top);
}

draw_width()
{
	return(pg_image_r.r_width < view_r.r_width ? pg_image_r.r_width :
	  view_r.r_width);
}

draw_height()
{
	return(pg_image_r.r_height < view_r.r_height ? pg_image_r.r_height :
	  view_r.r_height);
}


/*
 * Beware here that there are 2 levels of repainting being done by this
 * routine: the first paints the lowest level notion of the page (the one
 * which is only as big as the maximum page dimensions in the postamble
 * and hence does not contain any room for the standard 1 inch margins,
 * borders, etc.), the load_image, onto the window systems notion of the
 * whole page.  There is one load_image for each cached page of the DVI
 * file, and this call is executed once for each page.  The image painted
 * by this lower level call is not (under suntools) displayed at that
 * time.  The second level routine paints the portion of the whole page
 * image including margins and borders, the page_image, onto the visible
 * portion of the user's window.  There is only one page_image per
 * invocation of the tool.  Scroll requests are handled by painting some
 * portion of the page_image onto the user's window.
 */
/*
 * the main entry point for painting a page.  The intent is to be able to
 * call this routine with any combination of page and view point and have
 * it do the minimal amount of work, unless the refresh flag is set, in
 * which case everything is repainted, though the page will not be reread
 * from the DVI file.  The only time the page should be re-read from the
 * DVI file is when the DVI file has been changed, in which case we need
 * to look at the postamble etc. and the reread mechanism should be used.
 */

show_page(page, view_x, view_y, refresh)
	pg	*page;
{
	int	rtn_val = 0,
		new_page = page != dvi->cur_pg,
		new_x,
		new_y;
	long	time();

	push_cursor(HOUR_CUR);
	show_page_number(page);

	if (new_page && load_page(page) != 0) {
		rtn_val = -1;
		goto leave;
	}

	if (refresh && build_page_image(page) != 0) {
		rtn_val = -1;
		goto leave;
	}

	rtn_val = do_abs_scroll(view_x, view_y, refresh);

	page->time_stamp = time(0);
	dvi->cur_pg = page;

leave:	pop_cursor();
	return(rtn_val);
}

/*
 * redisplay the current page because some of the display parameters have
 * changed and we want to show the page with the new parameter.
 */
reshow_pg(vp)
	variable	*vp;
{
	if (size_page_image() < 0)
		return(-1);
	return(show_page(dvi->cur_pg, view_r.r_left, view_r.r_top, 1));
}

/*
 * this routine is very similiar to the previous one; the difference is
 * that this routine reloads the load image and then reconstructs the
 * page image.  that one just rebuilds the page image.  
 */
reload_pg(vp)
	variable	*vp;
{
	if (dvi->file != (FILE *) 0) {
		reload_this_file();
	}
}

/*
 * this routine is called when we want to load a new page from the dvi
 * file.  The page may or may not be cached, so we may or may not have to
 * go read it from the DVI file.
 */
load_page(page)
	pg	*page;
{
	long		ftell();


	if ((page->load_i != (char *) 0) && (page->s_buf != (s_char *) 0)) {
		/* the page is completely cached, so we need do nothing. */
		goto successful;
	}

	/* the page is not cached, so we need to create the buffers. */
	alloc_one_page(page);

	clear_load_image(page);

	if (page->offset != ftell(dvi->file)) {
		if (seek_to_page(page) != 0)
			return(-1);
	}

	if (read_page(page) != 0) {
		abort_this_file();
		return(-1);
	}
successful:
	return(0);
}

/*
 * this is the routine which fills in the page structure with pointers
 * (fseek offsets) to each page and the logical page numbers TeX outputs
 * as well as the physical page number within the document.
 */

#include "commands.h"

fill_in_page_ptrs()
{
	register pg	*tmp_page,
			*scan;
	register long	seek_pos = dvi->last_page,
			prev_pos = dvi->postamble;
	register int	index;
	pgs		*pages = dvi->pages;
	char		*prev_s_buf;

	/*
	 * scan backwards through the dvi file filling in all
	 * of the parameters of the page structure except the
	 * physical page number.
	 */
	do {
		fseek(dvi->file, seek_pos, 0);
		if ((index = getc(dvi->file)) != BOP) {
			msg(FATAL, "couldn't scan through the pages: bad dvi file.");
			abort_this_file();
			return(-1);
		}
		/*
		 * read the 10 \count parameters.  
		 */
		tmp_page = (pg *) alloc(sizeof (pg));
		/* mark the last page. */
		if (seek_pos == dvi->last_page) {
			pages->last = tmp_page;
		} else {
			scan->prev = tmp_page;
			tmp_page->next = scan;
		}
		tmp_page->offset = seek_pos;
		tmp_page->size = prev_pos - seek_pos;
		for (index = 0; index < 10;) {
			tmp_page->count[index++] =
			  (short) sign_extend(dvi->file, 4);
		}
		scan = tmp_page;
		prev_pos = seek_pos;
	} while ((seek_pos = no_sign_extend(dvi->file, 4)) != -1);
	pages->first = scan;
	
	/*
	 * scan forward through the page structure filling in the physical
	 * page numbers.  phys pages are numbered starting from 1.
	 */
	index = 1;
	while (scan != (pg *) 0) {
		scan->ppage = index++;
		scan = scan->next;
	}
	return(0);
}

seek_to_page(page)
	pg	*page;
{
	if (fseek(dvi->file, page->offset, 0) == -1) {
		msg(PLAIN, "couldn't get to page %s in %s",
			fmt_page_number(page, 0), dvi->fname);
		return(-1);
	}
	return(0);
}


show_page_number(page)
	pg	*page;
{
	char		fname[512];
	register char	*fn;
	register int	len;
	extern tl_data	*tl;

	if (dvi->fname == (char *) 0) {
		return(0);
	}

	len = strlen(dvi->cwd);
	if ((strncmp(dvi->cwd, dvi->fname, len) == 0) &&
	  (!index(&dvi->fname[len + 1], '/'))) {
		fn = &dvi->fname[len + 1];
	} else {
		len = strlen(tl->home);
		if (strncmp(tl->home, dvi->fname, len) == 0) {
			strcpy(fname, "~");
			strcat(fname, &dvi->fname[len]);
			fn = fname;
		} else {
			fn = dvi->fname;
		}
	}
	msg(TITLE, "Displaying \"%s\", page %s of %d%s.", fn, 
	  fmt_page_number(page, 0), dvi->pages->last->ppage, fmt_mag());

	return(0);
}

/*
 * DOCUMENTATION
 *
 * Name: beginning-of-file
 * Desc: This command positions \lit{dvitool} on the first page
 *	of the \lit{DVI} file.
 * SeeA: end-of-file
 */

bof()
{
	(void) show_page(dvi->pages->first, 0, 0, 1);
}
/*
 * DOCUMENTATION
 *
 * Name: end-of-file
 * Desc: This command positions \lit{dvitool} on the last page
 *	of the \lit{DVI} file.
 * SeeA: beginning-of-file
 */
eof()
{
	(void) show_page(dvi->pages->last, 0, 0, 1);
}

/*
 * format a string which tells the user how the current magnification has
 * been computed.  magnification is a combination of the TeX mag read
 * from the postamble and the global user magnification.
 */
char *
fmt_mag()
{
	static char	buf[60];

	if (dvi->user_mag == DEFAULT_MAG &&
	    dvi->TeX_mag == DEFAULT_MAG) {
		buf[0] = '\0';
	} else if (dvi->user_mag == DEFAULT_MAG && 
	  dvi->TeX_mag != DEFAULT_MAG) {
	  	sprintf(buf, " at TeXmag %d", dvi->TeX_mag);
	} else {
		sprintf(buf, " at global mag %d [TeX mag = %d]",
		  dvi->mag, dvi->TeX_mag);
	}
	return(buf);
}

/*
 * here we create a page number from the 10 count vars TeX ships out.  We
 * emulate the TeX convention of suppressing fields in which the count is 0
 * except for count0 which is always displayed.
 */

char *
fmt_page_number(page, wildcards)
	register pg	*page;
	unsigned int	wildcards;
{
	static char	buf[80],
			tbuf[20];
	register char	*bp = buf;
	register int	upper,
			lower;
	
	/*
	 * we first scan the count vars to find out which ones
	 * are non-zero since if our count vars look like
	 * [3][0][0][6][...], "[3.6]" is clearly not the right thing,
	 * "[3.0.0.6]" is.
	 */
	*bp++ = '[';
	*bp = '\0';
	for (upper = 9; upper > 0; upper--) {
		if (page->count[upper] != 0 || 1 & (wildcards >> upper))
			break;
	}
	for (lower = 0; lower <= upper; lower++) {
		/*
		 * check for wildcards
		 */
		if (1 & (wildcards >> lower)) {
			strcat(buf, "*.");
		} else {
			sprintf(tbuf, "%d.", page->count[lower]);
			strcat(buf, tbuf);
		}
	}
	while (*bp != '\0')
		bp++;
	/*
	 * write the ']' over the last '.'
	 */
	*(bp - 1) = ']';
	return(buf);
}

/*
 * convert the roman numeral of the form "ixvcdlm" at p into an int.
 * store it in val.
 * return -1 for failure & print an error, the converted int otherwise.
 */
int
from_roman(buf)
	char *buf;
{
	register int j = 0;
	char *p = buf;
	
	while (*p) {
		if (isupper(*p))
			*p = tolower(*p);
		p++;
	}
	p = buf;
	while (*p) {
		switch(*p) {
		case 'i':
			switch(*(p + 1)) {
			case 'v':
				j += 4;
				p++;
				break;
			case 'x':
				j += 9;
				p++;
				break;
			default:
				j++;
				break;
			}
			break;
		case 'v':
			j += 5;
			break;
		case 'x':
			switch(*(p + 1)) {
			case 'l':
				j += 40;
				p++;
				break;
			case 'c':
				j += 90;
				p++;
				break;
			default:
				j += 10;
				break;
			}
			break;
		case 'l':
			j += 50;
			break;
		case 'c':
			switch(*(p + 1)) {
			case 'd':
				j += 400;
				p++;
				break;
			case 'm':
				j += 900;
				p++;
				break;
			default:
				j += 1000;
				break;
			}
			break;
		default:
			msg(PLAIN,
			  "couldn't understand roman number %s, try any of \"ivxlcdm\".",
			  p);
			return(-1);
		}
		p++;
	}
	return(j);
}

/*
 * convert a positive integer to a roman numeral.  returns the null
 * string when val == 0;
 */
char *
to_roman(val)
	register int val;
{
	static char buf[25];
	register char *bp = buf;
	register int i;	
	struct r {
		int	val;
		char	representation[3];
	};
	register struct r *rp;
	static struct r roman[] = {
		{ 1000,  "m" },
		{  900, "cm" },
		{  500,  "d" },
		{  400, "cd" },
		{  100,  "c" },
		{   90, "xc" },
		{   50,  "l" },
		{   40, "xl" },
		{   10,  "x" },
		{    9, "ix" },
		{    5,  "v" },
		{    4, "iv" },
		{    1,  "i" },
		{    0,   "" }
	};

	/*
	 * do this in the most obvious way...
	 */
	for (rp = roman, *bp = '\0'; rp->val != 0; rp++) {
		for (i = val / rp->val; i--;) {
			val -= rp->val;
			strcat(bp, rp->representation);
		}
	}
	return(buf);
}

/*
 * this routine handles going to pages specified by the user.
 * it expects an argument in one of several forms:
 * 1.	"n[.n]" where [.n] may be repeated up to 9 times and
 *  	n is a page number or the char '*' which means "match
 *  	any page" or blank which also means "match any page".
 * 2.	"romannumeral".  See from_roman() for valid romannumerals.
 *
 * it returns zero for a successful goto (which implies that the new page
 * must be read in and displayed) or less than zero for an error (which
 * implies that no page need be read).
 */

pg *
lookup_page_by_lpage(arg)
	char	*arg;
{
	register pg	*newpg;
	register int	stat;
	pg		searchpg;
	unsigned int	wildcards = 0;

	for (stat = 0; stat < 10; stat++) {
		searchpg.count[stat] = (short) 0;
	}
	if ((stat = parse_goto_buf(arg, &searchpg, &wildcards)) < 0) {
		return((pg *) 0);
	} else if (stat == 0) {
		return(dvi->pages->last);
	}
	return(find_page(stat, &searchpg, wildcards));
}


	
/*
 * this routine parses the buffer filling in 2 structures to be used by
 * the lookup routine:
 *
 * 1. the count array of a pg * is filled in with
 *    the counts of the page to match.
 * 2. the wildcard int sets 1 bit for any count field which has
 *    an explicit or implicit '*' in it.
 *
 * the buffer is expected to be in one of two formats:
 *
 * 1.  n[.n] where there may be up to 9 [.n]'s.
 *     n can be '*' or blank which implies '*'.
 * 2.  romannumeral where romannumeral is parsed by From_roman.
 *     see that routine for details.  a bit in wildcard is set
 *     to indicate that we're searching for a roman numeral.
 *
 * returns < 0 for error (& prints its own error message)
 *    0 for nothing in the buffer to parse
 *    or the highest count[n] + 1 that it filled in.
 */

#define ROMAN (30)

parse_goto_buf(buf, page, wildcard)
	char buf[];
	register pg *page;
	unsigned int *wildcard;
{
	register char	*cp = buf;
	register int	count = 0;

	if (any_of(buf, "ivxlcdmIVXLCDM")) {
		if ((count = from_roman(buf)) < 0) {
			return(count);
		} else {
			/*
			 * assume roman pages are negative.
			 */
			page->count[0] = -count;
			*wildcard |= 1 << ROMAN;
			return(0 + 1);
		}
	}
	/*
	 * check for an empty buffer
	 */	
	if (*cp == '\0')
		return(0);
	while (*cp != '\0' && count < 10) {
		/*
		 * look for wildcard chars.  an empty field ("..")
		 * implies a wildcard.
		 */
		if (*cp == '*' || *cp == '.') {
			*wildcard |= 1 << count++;
			cp++;
			/*
			 * when the field looks like "*.",
			 * inc past the '.'
			 */
			if (*(cp - 1) == '*' && *cp == '.') {
				cp++;
			}
			continue;
		}
		/*
		 * no '*' or '.' so look for a number.
		 */
		if (sscanf(cp, "%hd", &page->count[count]) == 0) {
			msg(PLAIN,
			  "couldn't understand %s in your page number",
			  cp);
			return(-1);
		} else {
			count++;
			if (*cp == '-') {
				cp++;
			}
			while (isdigit(*cp)) {
				cp++;
			}
			if (*cp == '.') {
				cp++;
			}
		}
	}
	return(count);
}

/*
 * this routine searches the page structure using how many of the count
 * vars in pg to determine matches.  wildcards is a mask of count vars
 * which always match with bit 0 corresponding to pg->count[0] etc.  we
 * return 0 for failure and print our own error message, otherwise we
 * return a pg pointer.
 */
pg *
find_page(how_many, searchpg, wildcards)
	int			how_many;
	register pg		*searchpg;
	register unsigned int	wildcards;
{
	register pg	*page;

	/*
	 * we search from the current page + 1 to the end of
	 * the list and wrap around to search from the beginning
	 * of the list to our current page.
	 */
	page = dvi->cur_pg->next;
	for (; page != (pg *) 0; page = page->next) {
		if (match_pages(searchpg, page, how_many, wildcards) == 0) {
			return(page);
		}
	}
	for (page = dvi->pages->first; page != dvi->cur_pg->next; page = page->next) {
		if (match_pages(searchpg, page, how_many, wildcards) == 0) {
			return(page);
		}
	}
	/*
	 * if we're looking for a roman numbered page,
	 * search for both page n & page -n.
	 */
	if (1 & (wildcards >> ROMAN)) {
		searchpg->count[0] = -searchpg->count[0];
		return(find_page(how_many, searchpg,
		  wildcards & ~(1 << ROMAN)));
	}
		
	msg(PLAIN, "couldn't find page %s in %s",	  
	  fmt_page_number(searchpg, wildcards), dvi->fname);
	return((pg *) 0);
}

int
match_pages(spg, page, how_many, wildcards)
	register pg	*spg,
			*page;
	register int	how_many;
	register unsigned int	wildcards;
{
	register int	count;

	for (count = 0; how_many-- ;count ++) {
		if (spg->count[count] == page->count[count] ||
		  1 & (wildcards >> count)) {
			continue;
		} else {
			return(1);
		}
	}
	return(0);
}


/*
 * DOCUMENTATION
 *
 * Name: goto-manuscript-page
 * Call: string
 * Desc: This command searches for a specific page by examining
 *	between 1 and 10 of \pass{\TeX's} \lit{\count} variables.
 *	Each page of a \lit{DVI} file contains the value of these
 *	10 variables when it was shipped out; with the proper macro
 *	definitions, any number of section, chapter, or heading
 *	configurations can be described with them.  The string that
 *	\em{goto-manuscript-page} takes is parsed into up to 10 fields;
 *	then each of the \lit{\count} fields of each page is compared
 *	with the parsed string until either a match is found or until
 *	all the pages have been examined.  The format for the search
 *	string is \pass{$n[.n]\ldots$} where \pass{$n$}
 *	is either a decimal number
 *	or the asterisk character \lit{*}.  The dot character is
 *	a field separator.  Thus, assuming the the appropriate macros
 *	have put the chapter number into \lit{\count1}, the string
 *	\lit{*.4} will find the first page of chapter 4 beyond the
 *	current page.
 *
 *	\em{Goto-manuscript-page} also accepts strings of roman numerals
 *	as input.
 * SeeA: goto-physical-page
 */
goto_logical(argv)
	func_arg	*argv;
{
	pg	*page;

	if ((page = lookup_page_by_lpage((*argv).str)) == (pg *) 0) {
		return(-1);
	}
	return(show_page(page, 0, 0, 1));
}

/*
 * DOCUMENTATION
 *
 * Name: goto-physical-page
 * Call: integer
 * Desc: This command provides an alternative way to
 *	 \em{goto-manuscript-page} to seek to a \lit{DVI} page.  
 *	Instead of the complexity of \lit{\count} variables,
 *	\em{goto-physical-page} simply displays the nth page
 *	of the file.  Thus, \em{goto-physical-page}
 *	with an argument of 1 is semantically equivalent to
 *	\em{beginning-of-file}.
 * SeeA: goto-manuscript-page
 */

goto_ppage(argv)
	func_arg	*argv;
{
	pg	*page;

	if ((page = lookup_page_by_ppage((*argv).integer)) == (pg *) 0) {
		return(-1);
	}
	return(show_page(page, 0, 0, 1));
}

pg *
lookup_page_by_ppage(ppage)
{
	pg	*scan = dvi->pages->first;

	while (scan != (pg *) 0) {
		if (scan->ppage == ppage)
			return(scan);
		scan = scan->next;
	}
	msg(PLAIN, "physical page %d not in %s", ppage, dvi->fname);
	return((pg *) 0);
}

/*
 * DOCUMENTATION
 *
 * Name: next-page-positioned
 * Desc: This command displays the next page of the \lit{DVI} file
 *	at the same position on the page as the current page.  Thus,
 *	if you are viewing the bottom of one page and want to see the 
 *	bottom of the next page, this command suffices.
 * SeeA: next-page previous-page-positioned
 */
next_positioned()
{
	step_page(0, 1);
}
/*
 * DOCUMENTATION
 *
 * Name: previous-page-positioned
 * Desc: This command is the previous analog to \em{next-page-positioned}.
 * SeeA: next-page-positioned previous-page
 */
prev_positioned()
{
	step_page(1, 1);
}

/*
 * DOCUMENTATION
 *
 * Name: next-page
 * Desc: This is the normal command to page through the \lit{DVI} file.
 *	It displays the next page of the document with the upper left
 *	hand corner of the page visible, easing the normal reading
 *	flow from the bottom of one page to the top of the next.
 * SeeA: goto-manuscript-page next-page-positioned previous-page
 */
next_page()
{
	step_page(0, 0);
}

/*
 * DOCUMENTATION
 *
 * Name: previous-page
 * Desc: This is the previous analog to \em{next-page}.
 * SeeA: next-page previous-page-positioned
 */
prev_page()
{
	step_page(1, 0);
}

step_page(backwards, positioned)
{
	extern int	arg_val;

	int		repeat = (arg_val == 0) ? 1 : arg_val;
	pg		*scan = dvi->cur_pg;

	if (repeat == 1) {
		if (backwards == 0 && dvi->cur_pg == dvi->pages->last) {
			msg(PLAIN, "at the last page of %s", dvi->fname);
			return;
		}
		if (backwards != 0 && dvi->cur_pg == dvi->pages->first) {
			msg(PLAIN, "at the first page of %s", dvi->fname);
			return;
		}
	}
	while (repeat--) {
		if (backwards && scan->prev == (pg *) 0)
			break;
		else if (!backwards && scan->next == (pg *) 0)
			break;
		scan = (backwards) ? scan->prev : scan->next;
	}
	(void) show_page(scan, 
	  (positioned) ? view_r.r_left : 0,
	  (positioned) ? view_r.r_top  : 0, 1);
}

/* here are the routines to allocate space for individual pages. */

/*
 * the next couple of routines deallocate space used by the pages.
 * free_all_pages is the highest level.  it is meant to be called when
 * going to a new DVI file.  free_all_load_images is meant to be called
 * when some of the parameters affecting how the page is constructed
 * (either the load_image or the search buffer) have changed.
 * free_oldest_load_image is even lower level, to be called directly by
 * routines that allocate space who are panicking because there is no
 * more space and some needs to be recycled.  free_oldest_load_image is
 * not in this file because it must free up window-system specific data.
 */

free_all_pages()
{
	pg	*scan;

	for (scan = dvi->pages->first; scan != (pg *) 0; scan = scan->next) {
		free_one_page(scan);
		free(scan);
	}

	dvi->cur_pg = dvi->pages->first = dvi->pages->last = (pg *) 0;
	dvi->sel_start = dvi->sel_end = (s_char *) 0;
	return(0);
}


free_all_load_images()
{
	pg	*scan;

	for (scan = dvi->pages->first; scan != (pg *) 0; scan = scan->next) {
		free_one_page(scan);
	}
	dvi->sel_start = dvi->sel_end = (s_char *) 0;
	return(0);
}

/*
 * this routine expects new_limit to be in the range 1 <= new_l <= big
 * integer.
 */
shrink_cache(new_limit)
{
	int	now = dvi->pages->num_cached;

	if (new_limit > 1 && new_limit < now) {
		for ( ; new_limit < now; now--) {
			(void) free_oldest_load_image();
		}
	}
	return(0);
}

free_oldest_load_image()
{
	pg	*scan = dvi->pages->first,
		*oldest = 0;
	long	time;

	/* find the first one. */
	for (; scan != (pg *) 0; scan = scan->next) {
		if (scan->load_i != (char *) 0 && 
		    scan->s_buf != (s_char *) 0 &&
		    scan->time_stamp != KEEP_PG_CACHED) {
			oldest = scan;
			time = scan->time_stamp;
		    	scan = scan->next;
			break;
		}
	}

	if (scan == (pg *) 0) {
		msg(PLAIN, "couldn't free up a page!");
		return(-1);
	}

	for (; scan != (pg *) 0; scan = scan->next) {
		if (scan->load_i == (char *) 0 ||
		    scan->s_buf == (s_char *) 0) {
			continue;
		}
		/* skip any pages marked ``don't deallocate''. */
		if (scan->time_stamp == KEEP_PG_CACHED) {
			continue;
		}
		if (scan->time_stamp < time) {
			time = scan->time_stamp;
			oldest = scan;
		}
	}

	free_one_page(oldest);
	return(0);
}

/*
 * free up the space for one page pointed to by page. This routine is
 * slightly misnamed since it doesn't free all of the dynamically
 * allocated space for a page, just the heavyweight objects on the page.
 * The only routine that really frees everything is free_all_pages().
 * Side effect: decrements the cached page count.
 */
free_one_page(page)
	pg	*page;
{
	if (page->load_i != (char *) 0) {
		dvi->pages->num_cached--;
		free_one_load_image(page);
	}
	page->load_i = (char *) 0;

	if (page->s_buf != (s_char *) 0) {
		free(page->s_buf);
	}
	page->s_buf = (s_char *) 0;

	if (page->lines != (s_char **) 0) {
		free(page->lines);
	}
	page->lines = (s_char **) 0;
}


/*
 * the default size and default number of lines to add to the lines
 * structure for each page.
 */
#define DEF_LINES	(100)
#define DEF_ADD_LINES	(50)

/*
 * do everything to fill in the heavy weight structures for one page.
 * recall that all of the lightweight stuff has been allocated before,
 * when we scanned backwards through the document, reading each BOP.
 * Side effects: changes the count of the cached pages.
 */

alloc_one_page(page)
	pg	*page;
{
	int	w = dvi->w_in_pixels,
		h = dvi->h_in_pixels;
	
	/* first fill in the rectang structure. */
	page->load_r.r_width = w;
	page->load_r.r_height = h;
	page->load_r.r_left = dvi->l_mar_in_pixels;
	page->load_r.r_top = dvi->t_mar_in_pixels;

	if (alloc_load_image(&page->load_i, w, h) == -1) {
		msg(FATAL, "out of memory, can't allocate space for page %s",
		  fmt_page_number(page, 0));
	}

	page->s_buf = (s_char *) alloc(sizeof(s_char) * page->size);
	page->lines = (s_char **) alloc(sizeof(s_char *) * DEF_LINES);
	page->max_lines = DEF_LINES;

	/*
	 * we don't time stamp the page now, because unless it gets
	 * stamped by the paint routine we want it to be the oldest.
	 */

	dvi->pages->num_cached++;
	return(0);
}

/*
 * come here to add more space to the lines structure.  There cannot be
 * any pointers into the lines storage, as it is freed and reallocated.
 */
add_to_lines(page)
	pg	*page;
{
	page->max_lines += DEF_ADD_LINES;
	free((char *) page->lines);
	page->lines = (s_char **) 
	  alloc(page->max_lines * sizeof(s_char **));
	return(0);
}

/*
 * compute the new width of the page image in pixels, taking borders and
 * margins into account.  Store this information into the page image and
 * phys page image structures.
 */

/*
 * pg_image_h and pg_image_w are the height and width (in true sp) that
 * the user wants the displayed page image to have.  That's the size he
 * gets, unless the margins plus the maximum width & maximum height of a
 * DVI page as recorded in the postamble are bigger.
 */

size_page_image()
{
	int		w,
			left_margin,
			horiz_conv,
			h,
			top_margin,
			vert_conv;
	extern int	left_mar,
			top_mar,
			draw_borders,
			border_width,
			show_ld_image,
			pg_image_w,
			pg_image_h,
			true_hconv,
			true_vconv;

	/*
	 * if the dvi file is open, then we are about to read in a new
	 * DVI file and we need to compute the size of the page based on
	 * the user's magnification.  if not, however, we're just
	 * creating a blank page for typeout messages, so we use a
	 * pre-computed value for the conv_factor.  the default value is
	 * the number returned by do_conv with num and den the standard
	 * TeX values (they are constant in DVI files), mag == 1000, and
	 * HCONV or VCONV as required.
	 */
#	define TRUE_HOR_CONV	(39469)
#	define TRUE_VER_CONV	TRUE_HOR_CONV

	if (dvi->file == (FILE *) 0) {
		horiz_conv = TRUE_HOR_CONV;
		vert_conv = TRUE_VER_CONV;
	} else {
		horiz_conv = true_hconv;
		vert_conv = true_vconv;
	}

	w = pix_round(pg_image_w, horiz_conv);
	left_margin = pix_round(left_mar, horiz_conv);

	if (dvi->w_in_pixels + 2 * left_margin > w) {
		w = dvi->w_in_pixels + 2 * left_margin;
	}

	h = pix_round(pg_image_h, vert_conv);
	top_margin = pix_round(top_mar, vert_conv);

	if (dvi->h_in_pixels + 2 * top_margin > h) {
		h = dvi->h_in_pixels + 2 * top_margin;
	}

	/*
	 * if the user has no borders and no margins and they want to
	 * show the load image, we have to add 1 to the width and
	 * height.
	 */
	if (show_ld_image && border_width == 0) {
		if (left_margin == 0) {
			w++;
		}
		if (top_margin  == 0) {
			h++;
		}
	}

	/* set up the physical image rect. */
	rect_construct(&pg_image_r, 0, 0, w, h);
	phys_image_r = pg_image_r;

	if (draw_borders != 0) {
		pg_image_r.r_width  += 3 * border_width + 3 * border_width;
		pg_image_r.r_height += 3 * border_width + 3 * border_width;
		phys_image_r.r_left = phys_image_r.r_top = 3 * border_width;
	}

	dvi->l_mar_in_pixels = left_margin;
	dvi->t_mar_in_pixels = top_margin;

	if (set_page_image_size(pg_image_r.r_width, pg_image_r.r_height) < 0) {
		msg(PLAIN, "couldn't allocate memory for %d by %d page image",
		  pg_image_r.r_width, pg_image_r.r_height);
		pg_image_r = phys_image_r = rectang_null;
		return(-1);
	}
	return(0);
}
