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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/font.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

/* Routines to load font definitions. */
#include "structs.h"
#include "constants.h"
#include "commands.h"
#include "font.h"
#include "fdecls.h"
#include "readmacs.h"
#include "pkcommands.h"
#include "bits.h"
#include <pixrect/pixrect_hs.h>

extern int		errno;

pixr			*no_font_char;
fontp			*cur_font;

static  fontp		*font_list = 0;

static FILE		*pxlfp;

/*
 * max_fonts is used to determine how many simultaneously open font files
 * we can have.
 */
int			max_fonts,
			font_names_dirty = 0;
static int		num_open_fonts = 0,
			/*
			 * this buffer is used when reading the bits of
			 * the pixel images.
			 */
			pxl_buffer[PXL_BUF_SIZE];

/*
 * Read the font definitions as they are in the postamble of the DVI file.
 */

get_font_def()
{
	u_char	byte;
	int	i;

	while (((byte = getc(dvi->file)) >= FNT_DEF1) &&
		(byte <= FNT_DEF4)) {
		switch (byte) {
		case FNT_DEF1:
			read_font_def(no_sign_extend(dvi->file, 1));
			break;
		case FNT_DEF2:
			read_font_def(no_sign_extend(dvi->file, 2));
			break;
		case FNT_DEF3:
			read_font_def(no_sign_extend(dvi->file, 3));
			break;
		case FNT_DEF4:
			read_font_def(no_sign_extend(dvi->file, 4));
			break;
		}
	}
	if (byte != POST_POST)
		msg(FATAL, "POST_POST missing after fontdefs -- bad dvi file.");
}

#include <sys/file.h>	/* for use with access(2) call below */

read_font_def(font_num)
{
	register fontp		*fp;
	int			j,
				area_len,
				fname_len,
				pk_mag,
				pxl_mag,
				found;
	char			*dir,
				*cp,
				*cq,
				curarea[MAXPATHLEN],
				fontname[512];
	char_entry		*chp;
	fontp			*lookup,
				*font_name_to_entry();
	double			dbl_f;
	extern char		font_path[],
				e_font_path[];
	extern int		prev_search_f;

	/*
	 * mark the font name cache dirty so it will be recomputed the
	 * next time the user wants to complete on a font-name.
	 */
	font_names_dirty = 1;
	prev_search_f = -1;

	fp = (fontp *) alloc(sizeof(fontp));

	fp->k = font_num;
	/* throw away the checksum. */
	fseek(dvi->file, 4, 1);
	/* space size. */
	fp->s = no_sign_extend(dvi->file, 4);
	/* design size. */
	fp->d = no_sign_extend(dvi->file, 4);

	/*
	 * ``area'' and ``device'' here are TeX's analogs for directory
	 * and filename.  Since we use a font_path here to find the font
	 * file, this is something of a no-op.  If an ``area'' is
	 * specified, we will only look there for the font file.
	 */
	UNSIGNED_1(area_len, dvi->file);
	UNSIGNED_1(fname_len, dvi->file);
	cp = fontname;
	/*
	 * We don't need to make an explicit check here to see if we are
	 * overrunning the fontname array because area_len and fname_len
	 * are 1 byte quantities.
	 */
	for (j = area_len + fname_len; j--;) {
		*cp++ = getc(dvi->file);
	}
	*cp = '\0';

	/* compute the magnification of this font. */
	dbl_f = (actual_factor(
	  (int)(((double) fp->s / (double) fp->d) * 1000.0 + 0.5)) * 
	  actual_factor(dvi->mag));

	pk_mag = (dbl_f * (double) HCONVRESOLUTION) + 0.5;
	pxl_mag = (dbl_f * (double) HCONVRESOLUTION * 5.0) + 0.5;
	
	if (area_len != 0) {
		found = find_font_file(fp, (char *) 0, fontname,
		  TRY_PXL | TRY_PK, pxl_mag, pk_mag);
	} else {
		dir = e_font_path;
		found = 0;
		do {
			cp = index(dir, ':');
			if (cp == (char *) 0)
				cp = strlen(dir) + dir;
			for (cq = curarea; dir < cp; ) {
				*cq++ = *dir++;
			}
			*cq++ = '/';
			*cq++ = '\0';
			if (str_index(curarea, "pk") != -1) {
				found = find_font_file(fp, curarea, fontname, 
				  TRY_PK, 0, pk_mag);
			} else if (str_index(curarea, "pixel") != -1) {
				found = find_font_file(fp, curarea, fontname, 
				  TRY_PXL, pxl_mag, 0);
			} else {
				found = find_font_file(fp, curarea, fontname, 
				  TRY_PXL | TRY_PK, pxl_mag, pk_mag);
			}
			dir = (*cp == '\0') ? cp : cp + 1;
		} while ( !found && *dir != '\0');
	}

	if (!found) {
		/* save the font name. */
		sprintf(curarea, "%s.%dpk", fontname, pk_mag);
		fp->name = str_save(curarea);
	}

	/*
	 * check to see if we've already read in this font on some
	 * previous dvifile.
	 */
	if ((lookup = font_name_to_entry(fp->name)) != (fontp *) 0) {
		/*
		 * we've already got this font, so ignore it, except that
		 * we must copy the font number, since this varies from
		 * TeX job to TeX job depending on how the fonts are
		 * loaded in.
		 */
		lookup->k = fp->k;
		free(fp);
		return(0);
	} else {
		 /* we haven't seen this file, so put it in the list. */
		fp->next = font_list;
		font_list = fp;
		/* and fall out to open it etc. */
	}

	if (!found) {
		msg(WAIT, "couldn't find \"%s.%dpk\" or \"%s.%dpxl\" in path \"%s\"",
		  fontname, pk_mag, fontname, pxl_mag, font_path);

		if (fp->ch == (char_entry *) 0) {
			chp = (char_entry *) alloc(sizeof(char_entry));
			chp->width = no_font_char->pr_width;
			chp->height = no_font_char->pr_height;
			chp->x_offset = 0;
			chp->y_offset = 3;
			chp->flags |= NOT_FOUND;
			chp->address.pr = no_font_char;
			/*
			 * we'll use the design size and the at size in
			 * the DVI file to compute the tfm width for all
			 * of these characters.
			 */
			chp->tfmw = (int) 
			  ((double) fp->s * (double) fp->d / (1 << 20));
			fp->ch = chp;
		}

		fp->flags |= NOT_FOUND;

		return(0);
	}

	set_font_num(fp->k);
	if (fp->flags & C_PK) {
		return(setup_pk_chars(fp));
	} else {
		return(setup_pxl_chars(fp));
	}
}


find_font_file(fp, dir, fn, order, pxl_mag, pk_mag)
	fontp	*fp;
	char	*dir,
		*fn;
{
	int	found;
	char	name_buf[MAXPATHLEN];

	if (order & TRY_PK) {
		sprintf(name_buf, "%s%s.%dpk", dir, fn, pk_mag);
		if ((found = access(name_buf, R_OK)) == 0) {
			fp->flags |= C_PK;
			fp->font_mag = pk_mag;
			fp->name = str_save(name_buf);
			return(1);
		}
	}
	if (order & TRY_PXL) {
		sprintf(name_buf, "%s%s.%dpxl", dir, fn, pxl_mag);
		if ((found = access(name_buf, R_OK)) == 0) {
			fp->flags &= ~(C_PK);
			fp->font_mag = pxl_mag;
			fp->name = str_save(name_buf);
			return(1);
		}
	}
	return(0);
}

/*
 * Make sure that the font num font_num is open.  Since there is an upper
 * limit on the number of font files we can have open at one time, some
 * shuffling of the available file descriptors must be done.  This
 * routine does not position the file descriptor on any particular byte.
 *
 * Identification of the least used font is based on the counts of the
 * number of times each font has been "opened" by this routine.
 */

fontp *
open_font_file(num)
{
	register fontp	*scan,
			*wanted,
			*least_used;

	for (scan = font_list; scan != (fontp *) 0; scan = scan->next) {
		if (scan->k == num) {
			break;
		}
	}
	if (scan == (fontp *) 0) {
		/* we couldn't find the font numbered num. */
		return((fontp *) 0);
	}

	if (scan->fp != (FILE *) 0 || scan->flags & NOT_FOUND) {
		scan->used++;
		return(scan);
	}

	wanted = scan;
	if (num_open_fonts >= max_fonts) {
		/*
		 * we need to find the least used file, close it, and
		 * then open the file pointed to by wanted.
		 */
		least_used = (fontp *) 0;
		scan = font_list;
		for (; scan != (fontp *) 0; scan = scan->next) {
			if (scan->fp == (FILE *) 0) {
				continue;
			}
			if (least_used == (fontp *) 0) {
				least_used = scan;
				continue;
			}
			least_used = (scan->used < least_used->used) ?
			  scan : least_used;
		}
		fclose(least_used->fp);
		least_used->fp = (FILE *) 0;
		num_open_fonts--;
	}
	if ((wanted->fp = fopen(wanted->name, "r")) == (FILE *) 0) {
		msg(FATAL | PERROR, "couldn't open font file \"%s\"",
		  wanted->name);
	}
	wanted->used++;
	num_open_fonts++;
	return(wanted);
}


set_font_num(font_num)
{
	cur_font = open_font_file(font_num);

	if (cur_font == (fontp *) 0) {
		msg(FATAL, "font %d undefined", font_num);
	}
	pxlfp = cur_font->fp;
}


/*
 * initialize the data structures for the pixel (PXL) characters, but
 * don't read the raster data yet.
 */
setup_pxl_chars(fp)
	fontp	*fp;
{
	register int	j,
			k;
	char_entry	*chp,
			*chars;

	fseek(pxlfp, 0L, 0);
	if ((j = no_sign_extend(pxlfp, 4)) != PXLID)
		msg(FATAL,
		  "%s: version = %d, can only process version %d PXL files",
		  fp->name, j, PXLID);
	fseek(pxlfp, -8L, 2);
	fseek(pxlfp, no_sign_extend(pxlfp, 4) * 4, 0);

	chars = (char_entry *) alloc(sizeof(char_entry) * 128);

	for (j = 0; j < PXL_CHARS; j++) {
		chp = &(chars[j]);
		UNSIGNED_2(k, pxlfp);
		chp->width = (u_short) k;
		UNSIGNED_2(k, pxlfp);
		chp->height = (u_short) k;
		SIGNED_2(k, pxlfp);
		chp->x_offset= (short) k;
		SIGNED_2(k, pxlfp);
		chp->y_offset = (short) k;
		chp->flags &= ~(C_LOADED);
		chp->address.file_offset = no_sign_extend(pxlfp, 4) * 4;
		UNSIGNED_4(k, pxlfp);
		chp->tfmw = ((double) k * (double) fp->s) / (double) (1 << 20);
	}
	fp->ch = chars;
	fp->max_ch = PXL_CHARS;

	return(0);
}

/*
 * Likewise, set up the data structures for PK characters, but don't read
 * the pixel data yet.
 */
setup_pk_chars(fp)
	fontp	*fp;
{
	int		j,
			pl,
			pl_bits,
			cc,
			type,
			sflag;
	char_entry	*chp,
			*chars;
	/*
	 * the basic configuration of a PK file looks like:
	 *
	 * PRE i[1] k[1] x[k] ds[4] cs[4] hppp[4] vppp[4]
	 * 	i == id byte
	 *	k == len of comment x
	 *	ds == design size
	 *	cs == checksum
	 *	hppp == horizontal pixels per point
	 *	vppp == vertical pixels per point
	 * 0 or more character definitions
	 * POST
	 * 0 or more PK_NO_OP's
	 */
	/*
	 * note that unlike the PXL format, there can be any number of
	 * characters in a PK file, although TeX82 will never reference a
	 * character with an index greater than 127.
	 */
	/*
	 * read the ID byte of the font to make sure it is a PK file.
	 */
	fseek(pxlfp, 0L, 0);
	if ((j = getc(pxlfp)) != PK_PRE) {
		msg(FATAL,
		  "%s: no PK_PREamble; are you sure this is a pk file?",
		  fp->name);
	}
	if ((j = getc(pxlfp)) != PK_ID) {
		msg(FATAL,
		  "%s: version = %d, can only process version %d PK files",
		  fp->name, j, PK_ID);
	}
	/*
	 * set up the space for the font's characters.
	 */
	chars = (char_entry *) alloc(sizeof(char_entry) * PXL_CHARS);
	
	j = getc(pxlfp);
	/*
	 * skip the comment and the parameters to the char defs.
	 */
	fseek(pxlfp, (long) j + 16, 1);
	while ((j = getc(pxlfp)) != PK_POST) {
		if (j >= PK_XXX1) {
			if (j <= PK_XXX4) {
				switch(j) {
				case PK_XXX1:
					j = getc(pxlfp);
					break;
				case PK_XXX2:
					UNSIGNED_2(j, pxlfp);
					break;
				case PK_XXX3:
					UNSIGNED_3(j, pxlfp);
					break;
				case PK_XXX4:
					UNSIGNED_4(j, pxlfp);
					break;
				}
				/* skip the special command. */
				fseek(pxlfp, (long) j, 1);
				continue;
			}
			if (j == PK_YYY) {
				/* skip the numeric parameter. */
				fseek(pxlfp, 4L, 1);
				continue;
			}
			if (j == PK_NOOP) {
				continue;
			}
			msg(FATAL, "unknown byte %d at byte %d of %s!\n",
			  j, ftell(pxlfp), fp->name);
		}
		/* 
		 * otherwise we have a character definition. There are 3
		 * types of PK characters with different lengths and
		 * different word sizes: short (1 byte sizes), extended
		 * (2 byte sizes) and long (4 byte sizes).
		 */
		/*
		 * WARNING:  The short format section of this code has
		 * been tested; the extended format has not since none of
		 * the dvitool characters require it.
		 */
		/*
		 * the flag byte is split up logically into the following
		 * chunks:
		 *
		 *  -----------------------------------
		 *  |   dyn_f (0-14)  | b | s | p | l |       
		 *  -----------------------------------
		 *
		 * if dyn_f == 14, this character is stored as the bitmap
		 * and not run-encoded because the bitmap form is
		 * actually shorter than the run-encoding.
		 *
		 * b tells us whether the first run-length should set
		 * white or black pixels.
		 *
		 * s tells us whether certain size parameters are 1 or 2
		 * bytes long.  It's actually used to compute the type,
		 * see below.
		 *
		 * p and l should be concatenated to the packet length
		 * (the number of bytes this char def occupies in the
		 * file, but if the 3 bit quantity of s, p, and l == 7,
		 * then we have a ``long'' form char def.  4-6 is
		 * ``extended'' short form and 0-3 is short form.
		 */
		type = j & PK_MODE_BITS;
		if (type == PK_LONG_FORM) {
			msg(FATAL, "can't process type LONG chars in PK file %s",
			  fp->name);
		} else if (type > PK_SHORT_FORM) {
			pl_bits = j & PK_PL_BITS;
			type = PK_EXTENDED_FORM;
			UNSIGNED_2(pl, pxlfp);
			cc = getc(pxlfp);
			sflag = C_PK;
		} else {
			type = PK_SHORT_FORM;
			pl_bits = j & PK_PL_BITS;
			pl = getc(pxlfp);
			cc = getc(pxlfp);
			sflag = C_PK | C_SHORT;
		}
		if (type == PK_SHORT_FORM) {
			pl += pl_bits << 8;
		} else {
			pl += pl_bits << 16;
		}
		chp = &chars[cc];
		/*
		 * construct our internal char_entry flag.
		 */
		/*
		 * it turns out that the dyn_f is in bits 4-7 of the flag
		 * byte and that we store the dyn_f in bits 4-7 of our
		 * internal char_entry flag byte.
		 */
		COPY_BITS(j, sflag, C_DYN_F_MASK);
		if (j & PK_BLACK_BIT) {
			sflag |= C_START_BLACK;
		}
		chp->flags = (short) sflag;
		/*
		 * the packet length (pl) is relative to the tfm byte in
		 * the packet which is the next byte we would read.  So
		 * ftell gives us the fseek offset we need to read this
		 * char's info when and if we ever load it; plus fseeking
		 * pl bytes from where we are now will put us the byte
		 * after this character def.
		 */
		chp->address.file_offset = (long) ftell (pxlfp);
		fseek(pxlfp, (long) pl, 1);
	}
	fp->ch = chars;
	fp->max_ch = PXL_CHARS;

	return(0);
}


/* 
 * starting in SunOS 4.0, bitmaps wider than 16 bits have their rows
 * padded out to a 32-bit boundary, while bitmaps with a width less
 * than or equal to 16 have their rows padded out to 16 bits.
 * wider than 16 bits are padded out to a 32-bit boundary, instead of a
 *16-bit boundary.
 */
#define MAXPIXELSINSHORTLINE	16
#define SHORTLINEPADDING	16
#define LONGLINEPADDING		32
#define BITSPERBYTE		8

load_pxl_char(cp)
	register char_entry	*cp;
{
	register pixr	*pr;
	register int	nshorts,
			i,
			col,
			nints;
	register short	*dp,
			*sp;

	cp->flags |= C_LOADED;
	if (cp->width == 0 || cp->height == 0) {
		cp->address.pr = (pixr *) 0;
		return;
	}

	fseek(pxlfp, cp->address.file_offset, 0);
	pr = mem_create(cp->width, cp->height, 1);

	if (cp->width <= MAXPIXELSINSHORTLINE) {
		/* rows of bitmap padded to 16 bit boundary */
		nshorts = (cp->width + 15) >> 4;
		nints = (nshorts + 1) >> 1;
		dp = ((struct mpr_data *)pr->pr_data)->md_image;
		for (col = 0; col < cp->height; col++) {
			fread(pxl_buffer, 4, nints, pxlfp);
			sp = (short *) &pxl_buffer[0];
			for (i = nshorts; i > 0; i--)
				*dp++ = *sp++;
		}
	} else {
		/* rows padded to 32 bit boundary, just like a pxl file */
		nints = (cp->width + LONGLINEPADDING - 1) / LONGLINEPADDING;
		fread((char *) ((struct mpr_data *)pr->pr_data)->md_image,
		      sizeof(int), nints * cp->height, pxlfp);
	}
	cp->address.pr = pr;
}

/*
 * the macros that read pk values need these variables.
 */
static int	_nyb_buf,
		_nyb_flag,
		_pk_i,
		_pk_j,
		_pk_k,
		_pk_repeat;
/*
 * masks for loading up the pixel values.
 */
static short	masks[] = {
	0x8000,
	0x4000,
	0x2000,
	0x1000,
	0x0800,
	0x0400,
	0x0200,
	0x0100,
	0x0080,
	0x0040,
	0x0020,
	0x0010,
	0x0008,
	0x0004,
	0x0002,
	0x0001
};
	

/*
 * load a PK character into memory.  None of the width, height or tfm
 * info has been read yet.
 */
load_pk_char(cp)
	register char_entry	*cp;
{
	register pixr	*pr;
	int		j,
			k,
			cw,	/* character width */
			ch,
			pw,	/* present width */
			dst_w,
			dyn_f,	/* dynamic factor, part of a PK word. */
			rc,	/* repeat count */
			black,	/* start with white or black pixels ? */
			at_word,/* true if we're at a short boundary. */
			bits,
			extra_padding;
	u_short		*dp,
			*sp,
			*lim;

	/*
	 * we need the tfm width, the bounding box width and height, and
	 * the horiz. and vert. offsets from the reference point.
	 */
	fseek(pxlfp, cp->address.file_offset, 0);
	UNSIGNED_3(j, pxlfp);
	cp->tfmw = ((double) j * (double) cur_font->s) / (double) (1<<20);
	  
	if (cp->flags & C_SHORT) {
		/* ignore the horizontal escapement. */
		UNSIGNED_1(j, pxlfp);
		UNSIGNED_1(cw, pxlfp);
		UNSIGNED_1(ch, pxlfp);
		cp->width = (short) cw;
		cp->height = (short) ch;
		SIGNED_1(cp->x_offset, pxlfp);
		SIGNED_1(cp->y_offset, pxlfp);
	} else {
		/* ignore the horizontal escapement. */
		UNSIGNED_2(j, pxlfp);
		UNSIGNED_2(cw, pxlfp);
		UNSIGNED_2(ch, pxlfp);
		cp->width = (short) cw;
		cp->height = (short) ch;
		SIGNED_2(cp->x_offset, pxlfp);
		SIGNED_2(cp->y_offset, pxlfp);
	}

	cp->flags |= C_LOADED;
	if (cp->width == 0 || cp->height == 0) {
		cp->address.pr = (pixr *) 0;
	}

	/*
	 * what remains is the data for the image.  It can be packaged
	 * either as a run-encoding where successive values are the
	 * number of adjacent pixels to paint in the opposite color of
	 * the previous painting, or simply as a bitmap with no padding
	 * except (possibly) for the very last nybble to round up to a
	 * byte value.
	 */
	/*
	 * if the character width (cw) is 16 pixels or less, then
	 * the data for the pixrect is stored in successive bits with
	 * each new horizontal row at a short boundary.  If the 
	 * character width is greater than 16 pixels, then the data 
	 * for the pixrect is stored in successive bits with each 
	 * new horizontal row at an 32bit boundary.  See the Pixrect
	 * Reference Manual for details.
	 */
	cp->address.pr = pr = mem_create(cw, ch, 1);
	if (pr == (pixr *) 0) {
		msg(FATAL, "couldn't aquire space for character image");
	}
	
	sp = dp = (u_short *) ((struct mpr_data *)pr->pr_data)->md_image;

	/*
	 * grab the dyn_f out of the flag for this character.
	 */
	COPY_BITS((int) cp->flags, dyn_f, C_DYN_F_MASK);
	dyn_f = cp->flags & C_DYN_F_MASK;
	dyn_f >>= 4;
	/*
	 * the data returned by mem_create is zeroed; we depend on that
	 * because we only turn on characters that are supposed to be
	 * black.
	 */
	bits = cw * ch;
	extra_padding = ((cw > MAXPIXELSINSHORTLINE) &&
			 ((cw % LONGLINEPADDING) > 0) &&
			 ((cw % LONGLINEPADDING) <= SHORTLINEPADDING));
	if (dyn_f == 14) {
		/*
		 * we have a bitmap rather than a run-encoding.
		 *
		 * k is the (index for the) mask for the src byte, i.e.
		 * the result of the getc's.
		 *
		 * dst_w is the (index for the) mask for the dest short,
		 * i.e.  *dp.
		 */

		j = getc(pxlfp);
		for (k = 8, pw = 1, dst_w = 0; bits > 0; bits--) {
			if (j & masks[k]) {
				*dp |= masks[dst_w];
			}
			dst_w++;

			if (pw == cw || dst_w == 16) {
				dp++;
				dst_w = 0;
				if (extra_padding) dp++;
			}
			pw = (pw == cw) ? 1 : pw + 1;
			k = (k == 15) ? (j = getc(pxlfp), 8) : k + 1;
		}
		CLEAR_NYB;
		return(0);
	}
	dst_w = pw = _pk_repeat = rc = 0;
	black = cp->flags & C_START_BLACK;
	while (bits > 0) {
		GET_PACKED(j, pxlfp);

		if (_pk_repeat != 0) {
			rc = _pk_repeat;
			_pk_repeat = 0;
			continue;
		}

		/*
		 * we have a run count in j.
		 */
		for (; j-- > 0;) {

			if (black) {
				*dp |= masks[dst_w];
			}
			dst_w++; pw++;
			if (pw == cw || (pw % 16 == 0)) {
				/*
				 * we are at the end of a row, or the end
				 * of a short.
				 */
				dp++;
				dst_w = 0;
				if (pw != cw) {
					continue;
				}
				if (extra_padding) dp++; 
				/*
				 * copy the bits in this row to the next
				 * row if we have a repeat count.
				 */
				bits -= (rc + 1) * cw;
				for (k = 0; k < rc; k++) {
					for (lim = dp; sp < lim; ) {
						*dp++ = *sp++;
					}
				}
				sp = dp;
				pw = rc = 0;
			}
		}
		black = !black;
	}
	CLEAR_NYB;
	return(0);
}


static char	unknown_f[] = "unknown font";

char *
font_num_to_name(font_num)
{
	fontp	*scan = font_list;

	for (; scan != (fontp *) 0; scan = scan->next) {
		if (scan->k == font_num) {
			return(scan->name);
		}
	}
	return(unknown_f);
}

fontp *
font_name_to_entry(name)
	char	*name;
{
	fontp	*scan = font_list;

	for (; scan != (fontp *) 0; scan = scan->next) {
		if (*scan->name == *name && !strcmp(scan->name, name)) {
			return(scan);
		}
	}
	return((fontp *) 0);
}

font_name_to_num(font_name)
	char	*font_name;
{
	fontp	*f;

	if ((f = font_name_to_entry(font_name)) == (fontp *) 0) {
		return(-1);
	}
	return(f->k);
}

static fontp	*name_scan;

init_next_font()
{
	name_scan = font_list;
}


char *
next_font_name()
{
	char	*name;

	if (name_scan == (fontp *) 0) {
		return((char *) 0);
	}

	name = name_scan->name;
	name_scan = name_scan->next;

	return(name);
}
	
/*
 * DOCUMENTATION
 * Name: which-font
 * Desc: This command reports the font
 * name of the characters in the selection.  If there are characters from
 * more that one font in the selection, the selection is first trimmed to
 * include only characters from the same font as the first character in
 * the selection.
 * SeeA: ascii-of-selection which-char
 */
/*
 * We first search to see if there is more than one font represented in
 * the selection.  If there is, we limit the selection to one font and
 * report that one.
 */
which_font(argv)
	func_arg	*argv;
{
	s_char	*scan;
	int	f,
		dirty;

	scan = dvi->sel_start;
	f = (int) scan->f;
	dirty = 0;

	for (scan++; scan != dvi->sel_end + 1; scan++) {
		if (scan->c_type != S_SEARCHABLE) {
			continue;
		}
		if (scan->f != f) {
			dirty++;
			break;
		}
	}
	
	if (dirty) {
		show_sel(dvi->cur_pg, UNINVERT_SEL, 0);
		/*
		 * scan is pointing at the first character that is in a
		 * different font.  scan backwards until we hit one of
		 * our characters.
		 */
		for (--scan; scan->f != f; --scan)
			;
		dvi->sel_end = scan;
		show_sel(dvi->cur_pg, SHOW_SEL, 0);
	}
	
	msg(PLAIN, "the selection is in font %s", 
	  font_num_to_name((int) dvi->sel_start->f));
}
	

/*
 * DOCUMENTATION
 *
 * Name: which-char
 * Desc: This command describes some of the properties of the first
 *	character of the selection.  It reports the font name 
 *	and the position of the character in the font in decimal,
 *	octal, and hexadecimal.  If the selection contains more
 *	than one character, it is trimmed down to the first charcter
 *	in the selection and the information is reported on that
 *	character.
 * SeeA: ascii-of-selection which-font
 */
char_info(argv)
	func_arg	*argv;
{
	s_char	*cp;
	
	/* make sure that there is only one character in the selection. */
	if (dvi->sel_start != dvi->sel_end) {
		show_sel(dvi->cur_pg, UNINVERT_SEL, 0);
		dvi->sel_end = dvi->sel_start;
		show_sel(dvi->cur_pg, SHOW_SEL, 0);
	}
	
	cp = dvi->sel_start;
	
	msg(PLAIN, "character %d ('%03o \"%02x %s) from font %s",
	  cp->c, cp->c, cp->c, p_char(cp->c), font_num_to_name((int) cp->f));
}

/*
 * Remove all of the font numbers in the fonts in the cache because the
 * number assigned to a given font in our cache varies between DVI files.
 */
unnumber_fonts()
{
	fontp	*scan = font_list;

	for (; scan != (fontp *) 0; scan = scan->next) {
		scan->k = -1;
	}
}
	
/*
 * flush the entire font cache.  Presumably, the user wants to see a
 * recent modification to a font character that is cached.
 */
flush_font_cache()
{
	fontp	*scan = font_list,
		*next;
	
	for (; scan != (fontp *) 0; scan = next) {
		next = scan->next;
		if (flush_one(scan) < 0)
			return(-1);
		free(scan);
	}
	font_list = cur_font = (fontp *) 0;
	return(0);
}
	
flush_one(f)
	fontp	*f;
{
	int		j;
	char_entry	*ch;

	for (j = f->max_ch; j > 0; j--) {
		ch = &(f->ch[j - 1]);
		if (ch->flags & C_LOADED && !(ch->flags & NOT_FOUND) &&
		  ch->address.pr != (pixr *) 0) {
			free(ch->address.pr);
		}
	}

	if (f->fp != (FILE *) 0) {
		fclose(f->fp);
		num_open_fonts--;
	}

	free(f->name);
	return(0);
}

