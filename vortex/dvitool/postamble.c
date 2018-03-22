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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/postamble.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

#include "structs.h"
#include "constants.h"
#include "commands.h"
#include "fdecls.h"

int	hconv,
	vconv,
	true_hconv,
	true_vconv;

#include <sys/stat.h>

find_post_amble()
{
	int	i;
	long	scan;
	struct stat	buf;

	(void) fseek(dvi->file, (long) 0, 2);   /* goto end of file */
	scan = ftell(dvi->file) - 4;

	for(;;) {
		(void) fseek (dvi->file, --scan, 0);
		i = getc(dvi->file);
		if (i == EOF && scan > 0) {
			/*
			 * if the file shrunk (during a re-read), find
			 * the end of it.  This is a gross hack but
			 * something in SunView 3.0 (probably NFS) always
			 * hands me the old (larger) size of the file,
			 * even if I fclose it and re-fopen it!
			 */
			continue;
		}
		if (i != 223 && i != DVIFORMAT) {
			msg(PLAIN, "Bad byte (%d) at the end of %s.",
			  i, dvi->fname);
			abort_this_file();
			return(-1);
		}
		if (i == DVIFORMAT)
			break;
	}
	(void) fseek (dvi->file, scan - 4, 0);
	dvi->postamble = no_sign_extend(dvi->file, 4);
	(void) fseek (dvi->file, dvi->postamble, 0);

	return(0);
}

/*
 * This routine is used to read in the postamble values. It initializes the
 * magnification and checks the stack height prior to starting printing the
 * document.  Leaves the file pointer at the end of the font definitions in
 * the postamble.
 */
read_post_amble()
{
	int		tex_hconv,
			tex_vconv;
	extern int	kern_threshold,
			lbrk_threshold;

	if (find_post_amble() != 0)
		return(-1);
	if (getc(dvi->file) != POST) {
		msg(PLAIN, "POST missing at head of postamble -- bad DVI file");
		abort_this_file();
		return(-1);
	}
	dvi->last_page = no_sign_extend(dvi->file, 4);
	dvi->num = no_sign_extend(dvi->file, 4);
	dvi->den = no_sign_extend(dvi->file, 4);
	dvi->TeX_mag = no_sign_extend(dvi->file, 4);

	/*
	 * the magnification we will use is computed from the TeX
	 * magnification and the user's magnification.
	 */
	dvi->mag = (int) ((actual_factor(dvi->TeX_mag) * 
			  actual_factor(dvi->user_mag) *
			  (float) 1000.0) + 0.5);

	hconv = do_conv(dvi->num, dvi->den, HCONVRESOLUTION, dvi->mag);
	vconv = do_conv(dvi->num, dvi->den, VCONVRESOLUTION, dvi->mag);

	/*
	 * The conversion factors for ``true'' dimensions (page widths
	 * and the like) are subject only to the user's global
	 * magnification, not to TeX's magnification.
	 */
	true_hconv = do_conv(dvi->num, dvi->den, HCONVRESOLUTION, 
	  dvi->user_mag);
	true_vconv = do_conv(dvi->num, dvi->den, VCONVRESOLUTION, 
	  dvi->user_mag);
	/*
	 * It turns out that we also need the conversion factor for just
	 * the TeX mag, so compute that.
	 */
	tex_hconv = do_conv(dvi->num, dvi->den, HCONVRESOLUTION, 
	  dvi->TeX_mag);
	tex_vconv = do_conv(dvi->num, dvi->den, VCONVRESOLUTION, 
	  dvi->TeX_mag);

	dvi->max_height = no_sign_extend(dvi->file, 4);
	dvi->max_width  = no_sign_extend(dvi->file, 4);
	dvi->h_in_pixels = pix_round(dvi->max_height, tex_vconv) + 1;
	dvi->w_in_pixels = pix_round(dvi->max_width, tex_hconv) + 1;

	/* compute the kern_threshold for this file. */
	if (kern_threshold == 0) {
		dvi->kern_limit = 0;
	} else if (kern_threshold == 100000) {
		dvi->kern_limit = 0x7fffffff;
	} else {
		dvi->kern_limit = (int) ((float) dvi->max_width * (float) 
		  kern_threshold / 100000);
	}

	/*
	 * compute the linebreak threshold for this file, even though we
	 * don't use it yet.
	 */
	if (lbrk_threshold == 0) {
		dvi->baseln_limit = 0;
	} else if (lbrk_threshold == 100000) {
		dvi->baseln_limit = 0x7fffffff;
	} else {
		dvi->baseln_limit = (int) ((float) dvi->max_height * (float)
		  lbrk_threshold / 100000);
	}

	dvi->stack_depth = no_sign_extend(dvi->file, 2);
	if (dvi->stack_depth >= STACKSIZE) {
		msg(PLAIN,
		  "%s: dvi interpreter stack size is too small.",
		  dvi->fname);
		abort_this_file();
		return(-1);
	}
	dvi->num_pages = no_sign_extend(dvi->file, 2);
	get_font_def();

	return(0);
}

skip_font_def(file, k)
	FILE	*file;
{
	int	a,
		l;
	/*
	 * skip over the parameters until the directory (area)
	 * & file name lengths for this font are specified.
	 */
	fseek(file, (long) 3 * 4, 1);
	a = getc(file);
	l = getc(file);
	fseek(file, (long) a + l, 1);
}

/*
 * return the actual size factor given the approximation.
 */

float
actual_factor(s)
	int	s;
{
	float	realsize;

	realsize = (float) s / 1000.0;

	if (s == 1000 || s == 1200 || s == 1440 || s == 1728)
		return(realsize);

	/* handle the cases where we must supply more precision */
	if (s == 2074)
		realsize = 2.0736;		/* \magstep4 */
	else if (s == 2488)
		realsize = 2.48832;		/* \magstep5 */
	else if (s == 1095)
		realsize = 1.095445;		/* \magstephalf */
	else if (s == 833)
		realsize = 0.8333333;		/* magstep -1 */
	else if (s == 694)
		realsize = 0.6944444;		/* magstep -2 */
	else if (s == 579)
		realsize = 0.5787037;		/* magstep -3 */
	else if (s == 482)
		realsize = 0.4822531;		/* magstep -4 */
	else if (s == 402)
		realsize = 0.4018776;		/* magstep -5 */

	return(realsize);
}

do_conv(num, den, conv_resolution, mag)
{
	register float	conv;

	conv = ((float) num / (float) den) * 
		actual_factor(mag) * 
		((float) conv_resolution/254000.0);
	return((int) (1.0 / conv + 0.5));
}

pix_round(dvi_units, conv_factor)
	register int	dvi_units,
			conv_factor;
{
    return((int)((dvi_units + (conv_factor >> 1)) / conv_factor));
}


