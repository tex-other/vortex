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

/*
 * These routines implement a general purpose escape completion mechanism.
 * See the man pages (in compl.3) for a detailed description of their
 * operation.  The entry points are:
 *
 *	char **
 *	compl_path(pathname, bad_ext)
 *	 	char	*pathname,
 *			*bad_ext[];
 *	builds and returns a list of file names in the directory pathname
 *	minus any files with extensions in the bad_ext list.
 *	returns (char **) 0 for error or the list.
 *
 *	char **
 *	compl_list(nextf)
 *		char	*(*nextf)();
 *	also builds a list to be completed on.  calls nextf for strings to
 *	be put into the list until nextf returns (char *) 0.
 *	returns (char **) 0 for error or the list.
 *	
 *	compl_expand(partial, possibles, completed)
 *		char	*partial,
 *			**possibles,
 *			completed[];
 *	completes the partial from possibles into completed.
 *	completed is the (possibly null) part that can be added to
 *	partial.
 *	returns one of:
 *		0	no matches.  no strings have this prefix.
 *		1	exactly 1 unique complete match
 *		2	1 complete match, but not unique.  match is
 *			a substring of another match.
 *		3	some completion was done, but no complete match
 *			was found.
 *		4	no completion is possible.  we need more char's
 *			in partial to do a completion.
 *
 *	compl_match(partial, possibles, matched_list)
 *		char	*partial,
 *			**possibles,
 *			***matched_list;
 *	returns in matched_list an array of all the strings that match
 *	partial from possibles.
 *	returns the number of matches.
 *
 *	compl_free(possibles)
 *		char	**possibles;
 *	deallocates (with free(3)) the space allocated to the possibles list.
 *	always returns 0.
 *
 */
/*
 * Authors: Jeff Mc Carrell and John Coker
 * University of California at Berkeley
 *
 * RCS Info:
 * $Header: /home/yew/yew4/vortex/newdist/dvitool/RCS/compl.c,v 2.15 1993/09/16 02:29:01 munson Exp $
 */

#include <sys/types.h>
#include <sys/dir.h>

/* here are the definitions of the return codes which correspond to the
 * comments above */
#define NOMATCH		(0)
#define EXACTMATCH	(1)
#define SUBMATCH	(2)
#define PARTIALMATCH	(3)
#define NOCOMPLETION	(4)

compl_expand(partial, possibles, completed)
	char	*partial,
		**possibles,
		completed[];
{
	register char	*scan,
			**first_m,
			**last_m,
			match_char;
	register int	match_pos,
			match_state;
	int		first_m_match_state,
			more_to_do;
	char		*cp;

	/*
	 * consider the special case where there is only one element in the
	 * possibles array and the partial string is the empty string.  in
	 * that case, we always return an exact match.
	 */
	if (*partial == '\0' && *(possibles + 1) == (char *) 0) {
		strcpy(completed, *possibles);
		return(EXACTMATCH);
	}
	/*
	 * consider the special case where the partial string is the empty
	 * string and all of the elements in the array have a common
	 * prefix.  Then we should like to return that common prefix.
	 */
	if (*partial == '\0') {
		/*
		 * we reuse some variables here to scan through the
		 * possibles list.
		 */
		cp = *possibles;
		/*
		 * cp is now pointing at the first string in the possibles
		 * list.
		 */
		for (last_m = possibles; (scan = *last_m) != (char *) 0;) {
			last_m++;
		}
		--last_m;
		scan = *last_m;
		/*
		 * scan is now pointing at the last string in the
		 * possibles list.
		 */
		if (*scan == *cp) {
			first_m = possibles;
			match_pos = 0;
			match_state = NOCOMPLETION;
			goto fillout;
		}
		return(NOMATCH);
	}

	match_state = NOMATCH;
	for (first_m = last_m = (char **) 0;
	     (scan = *possibles++) != (char *) 0;) {
		if (*scan < *partial)
			continue;
		if (*scan > *partial)
			break;
		if ((match_state = one_cmp(partial, scan)) != NOMATCH) {
			first_m = possibles - 1;
			break;
		}
	}
	if (first_m == (char **) 0 || match_state == NOMATCH)
		return(NOMATCH);

	/* else scan is pointing to the first item in our list that matches
	 * the partial string. */
	first_m_match_state = match_state;
	match_state = NOMATCH;
	for (; (scan = *possibles++) != (char *) 0; ) {
		if (*scan > *partial)
			break;
		if ((match_state = one_cmp(partial, scan)) != NOMATCH) {
			last_m = (possibles - 1);
		}
	}
	if (match_state == NOMATCH && last_m == (char **) 0) {
		strcpy(completed, *first_m + strlen(partial));
		return(EXACTMATCH);
	}
	/* else we've got more than 1 match.  the cases are:
	 * 1) first_m points at a proper substring of the other matches
	 * in which case match_state of first_m == EXACTMATCH.
	 * 2) we can either complete some or we can't depending on
	 * the strings.  we must scan them to find out what we can
	 * or cannot complete. */

	if (first_m_match_state == EXACTMATCH) {
		strcpy(completed, *first_m + strlen(partial));		
		return(SUBMATCH);
	}

	match_pos = strlen(partial);
fillout:
	match_state = NOCOMPLETION;
	cp = completed;
	for (more_to_do = 1; more_to_do; match_pos++) {
		if ((match_char = *((*first_m) + match_pos)) == '\0')
			break;
		for (possibles = first_m + 1; possibles <= last_m; possibles++) {
			if (match_char != *((*possibles) + match_pos)) {
				more_to_do = 0;
				break;
			}
		}
		if (possibles > last_m) {
			*cp++ = match_char;
			match_state = PARTIALMATCH;
		}
	}
	*cp = '\0';
	return(match_state);
}

static char		*null_ptr = 0;

compl_match(partial, possibles, matches)
	char	*partial,
		**possibles,
		***matches;
{
	register char	*scan,
			**first_m,
			**last_m,
			**match_space;
	register int	how_many = 0;

	if (*partial == '\0') {
		/*
		 * consider the special case where there is only one element
		 * in the possibles array and the partial string is the empty
		 * string.  in that case, we always return an exact match.
		 */
		first_m = possibles;
		if (*(possibles + 1) == (char *) 0) {
			last_m = possibles;
		} else {
		/*
		 * consider the case where the user gives us the null string
		 * ("").  then everything is a match.
		 */
			while (*possibles != (char *) 0) {
				possibles++;
			}
			last_m = possibles - 1;
		}
	} else {
		for (first_m = last_m = (char **) 0;
		     (scan = *possibles++) != (char *) 0;) {
			if (*scan < *partial)
				continue;
			if (*scan > *partial) {
				*matches = &null_ptr;
		     		return(0);
			}
			if (one_cmp(partial, scan) != NOMATCH) {
				first_m = possibles - 1;
				break;
			}
		}
		if (first_m == (char **) 0) {
			*matches = &null_ptr;
			return(0);
		}
		for (; (scan = *possibles++) != (char *) 0; ) {
			if (*scan > *partial)
				break;
			if (one_cmp(partial, scan) != NOMATCH) {
				last_m = (possibles - 1);
			}
		}
	}
	/*
	 * check to see if we fell off the end of the list without
	 * finding a last match.
	 */
	if (last_m == (char **) 0) {
		last_m = first_m;
	}
	how_many = last_m - first_m + 1;
	if ((match_space = (char **) malloc((how_many + 1)
	     * sizeof (char *))) == (char **) 0) {
		return(-1);
	}
	*matches = match_space;
	for (possibles = first_m; possibles <= last_m;) {
		*match_space++ = *possibles++;
	}
	*match_space = (char *) 0;
	return(how_many);
}

compl_free(list)
	register char	**list;
{
	register char	**scan = list;

	while (*scan != (char *) 0) {
		free(*scan++);
	}
	free(list);
	return(0);
}

/* must be called as one_cmp(partial, match_string).
 * returns one of EXACTMATCH, NOMATCH, or PARTIALMATCH */
static
one_cmp(s1, s2)
	register char	*s1,
			*s2;
{
	while (*s1 != '\0') {
		if (*s1++ != *s2++) {
			return(NOMATCH);
		}
	}
	return((*s2 == '\0') ? EXACTMATCH : PARTIALMATCH);
}


#define		ALLOC_ITEMS	(128)
#define		ALLOC_BLK_SIZE	((ALLOC_ITEMS) * (sizeof(char *)))

static unsigned	block_size;
static char	**bad_extens;
static int	*extens_len = 0;
static DIR	*dirp;

char **
compl_path(dirname, bad_list)
	char	*dirname,
		*bad_list[];
{
	char	**list,
		*next_dir_name(),
		**compl_list();

	if ((dirp = opendir(dirname)) == (DIR *) 0) {
		return((char **) 0);
	}
	bad_extens = bad_list;
	if ((list = compl_list(next_dir_name)) == (char **) 0) {
		return((char **) 0);
	}
	free(extens_len);
	extens_len = (int *) 0;
	closedir(dirp);
	return(list);
}

/*
 * this routine is called once each time another file name from the
 * previously openned directory is needed.  It does not pass the files "."
 * or "..".  If the global var bad_extens is not null, then it compares
 * the end of the file name with each string on that list.  If any of them
 * form an exact match, that file name is not passed.
 */
static char *
next_dir_name()
{
	register struct direct	*dp;
	register char		**bad;
	register int		*badl,
				skip_it;

	while ((dp = readdir(dirp)) != (struct direct *) 0) {
		if (dp->d_namlen <= 2) {
			if (strcmp(dp->d_name, ".") == 0 ||
			    strcmp(dp->d_name, "..") == 0)
			    	continue;
		}
		if (bad_extens != (char **) 0) {
			if (extens_len == (int *) 0) {
				if (build_ext_lens() < 0)
					return((char *) 0);
			}
			skip_it = 0;
			bad = bad_extens;
			badl = extens_len;
			for (; *bad != (char *) 0; bad++, badl++) {
				if (strncmp(*bad,
				  dp->d_name + (dp->d_namlen - *badl),
				  *badl) == 0) {
					skip_it++;
					break;
				}
			}
			if (skip_it)
				continue;
		}
		return(dp->d_name);
	}
	return((char *) 0);
}

/*
 * here we take the list in the global variable bad_extens which is
 * guaranteed to be non-nil and we build a corresponding array of ints
 * which contain the lengths of each of those strings.  We leave this
 * array in the global variable extens_len.
 */
static
build_ext_lens()
{
	register char	**scan;
	register int	count = 0,
			*p;

	/* find out how many there are so we can malloc the right size. */
	for (scan = bad_extens; *scan != (char *) 0; scan++) {
		count++;
	}
	if ((p = (int *) malloc(count * sizeof(int))) == (int *) 0) {
		return(-1);
	}
	extens_len = p;
	for (scan = bad_extens; *scan != (char *) 0; scan++) {
		*p++ = strlen(*scan);
	}
	return(0);
}


char **
compl_list(nextf)
	char	*(*nextf)();
{
	char	**blk_base,
		**max,
		*add_this,
		**blk_p;
	int	offset,
		len,
		list_cmp();

	/*
	 * the get_ routines leave max pointing to the last available slot in
	 * the added on space.
	 */
	get_first_block(&blk_base, &max);
	blk_p = blk_base;

	while ((add_this = (*nextf)()) != (char *) 0) {
		/* we must leave room for the null marker */
		if (blk_p  == max - 1) {
			/* we need to keep around offset because realloc may
			 * move blk_base somewhere else. */
			offset = (blk_p - blk_base);
			if (get_more(&blk_base, &max) < 0) {
				return((char **) 0);
			}
			blk_p = blk_base + offset;
		}
		len = strlen(add_this);
		if ((*blk_p = (char *) malloc(len)) == (char *) 0) {
			return((char **) 0);
		}
		strcpy(*blk_p++, add_this);
	}
	*blk_p = (char *) 0;
	(void) qsort(blk_base, blk_p - blk_base, sizeof(char **), list_cmp);
	return(blk_base);
}


static
get_first_block(base, end)
	char	***base,
		***end;
{
	char	*b;

	block_size = ALLOC_BLK_SIZE;

	if ((b = (char *) malloc(block_size)) == (char *) 0) {
		return(-1);
	}
	*base = (char **) b;
	*end = (char **) b + (block_size - 1) / sizeof(char **);
	return(0);
}

static
get_more(base, end)
	char	***base,
		***end;
{
	char	*b;

	block_size += ALLOC_BLK_SIZE;

	if ((b = (char *) realloc((char *) *base, block_size)) == (char *) 0) {
		return(-1);
	}
	*base = (char **) b;
	*end = (char **) b + (block_size - 1) / sizeof(char **);
	return(0);
}

static
list_cmp(p1, p2)
	char	**p1,
		**p2;
{
	return(strcmp(*p1, *p2));
}
