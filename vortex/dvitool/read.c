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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/read.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

#include "structs.h"
#include "constants.h"
#include "commands.h"
#include "font.h"
#include "readmacs.h"
#include "sun.h"

extern fontp		*cur_font;
extern sun_d		*sunp;
extern int		hconv,
			vconv;

typedef struct Dvi_Stack {
	int	h,
		v,
		w,
		x,
		y,
		z;
} dvi_stack;

dvi_stack		page_stack[STACKSIZE];
static int		stack,
			h,	/* horizontal position			*/
			hh,	/* horizontal position in pixels	*/
			v,	/* vertical position			*/
			vv,	/* vertical position in pixels		*/
			w,	/* horizontal spacing			*/
			x,	/* current horizontal spacing		*/
			y,	/* current vertical spacing		*/
			z;	/* current vertical spacing		*/

static pixr		*ppr;

/*
 * this routine has undergone a fair amount of optimization since it is
 * the ``inner loop'' of dvitool.  I use macros extensively to reduce
 * function call overhead.  The sequence of the if (cmd == DVICMD)
 * statements has been coded from statistics gathered about the relative
 * frequencies of the various DVI commands.  I used two sources for my
 * statistics gathering: the first was tex.dvi -- the WEB source for TeX
 * in DVI form and the second was 40 pages of a thesis.  the results are
 * below:
 */
/*
 * tex.dvi			the thesis
 * dvi cmd   %total		dvi cmd   %total
 * ================		================
 * set_char  56.236		set_char  63.141
 * right3    9.516		w0        8.904
 * font_num  8.291		font_num  6.120
 * pop       6.506		push      4.095
 * push      6.506		pop       4.095
 * w0        5.573		right3    4.042
 * put_rule  1.468		x0        2.343
 * x0        1.421		down3     2.142
 * w3        1.316		w3        1.456
 * y0        0.926		right2    1.129
 * right2    0.845		y0        0.657
 * x3        0.350		x2        0.588
 * x2        0.323		w2        0.277
 * down3     0.214		x3        0.274
 * w2        0.123		right4    0.147
 * down4     0.121		put_rule  0.141
 * right4    0.098		z0        0.109
 * z0        0.052		down4     0.072
 * y3        0.039		set_rule  0.063
 * eop       0.030		y3        0.062
 * bop       0.030		z3        0.039
 * z3        0.014		bop       0.035
 * fnt_def1  0.002		fnt_def1  0.035
 * set_rule  0.001		eop       0.035
 */

/*
 * this macro determines whether the given amount of horizontal space
 * should be considered a logical space, or ignored because it's a kern.
 */
#define	SPACE(k) if ((k) > k_lim && (sp - 1)->c_type == S_SEARCHABLE) {	\
			sp->c_type = (u_char) S_SPACE;			\
			sp->c = ' ';					\
			sp++;						\
		}
/*
 * this macro marks a linebreak by storing the address of this byte if the
 * vertical movement is over the break threshold.
 */
#define LINEBREAK(k)	if ((k) > b_lim) { down_addr = cmd_offset; }
/*
 * this macro is also used in evaluating line breaks.
 */
#define MARK_RULE	rule_addr = cmd_offset

/*
 * these variables are used to determine when we see a POP/DOWN/PUSH
 * sequence which is interpreted as a line break.
 */
static int	pop_addr,
		rule_addr,
		down_addr;

static int	val,
		f,
		line_c,
		at_line_break;
/*
 * the min_hh and min_vv variables are used to cover the assumption made
 * by TeX that position 0, 0 on the DVI page is exactly 1 inch from the
 * left and top edges of the page.  TeX will gladly issue setchar's at a
 * negative h or v.  Thus we must keep track of the minimum hh (h in
 * pixels) that we have seen so far so that if we see a new minimum hh
 * (which addresses pixels at a negative coordinate that do not exist) we
 * can salvage the situation by making the underlying image bigger.  We
 * also check to see if characters have been set off of the right or
 * bottom edges.
 */
static int	min_hh,
		max_hh,
		min_vv,
		max_vv,
		p_flag;

#define CHECK_HH(k, w)						\
	if ((k) < min_hh) {					\
		if ((k) == -1) {				\
			(k) = 0;				\
		} else if (pg_adjust(page, MV_LEFT, min_hh, (k),\
		   &p_flag) == 0) {				\
			min_hh = (k);				\
			ppr = (pixr *) page->load_i;		\
		}						\
	}							\
	if ((k) + (w) > max_hh) {				\
		if (pg_adjust(page, MV_RIGHT, max_hh, ((k) + (w)),\
		  &p_flag) == 0) {				\
			max_hh = (k) + (w);			\
			ppr = (pixr *) page->load_i;		\
		}						\
	}

#define CHECK_VV(k, h)						\
	if ((k) < min_vv) {					\
		if (pg_adjust(page, MV_UP, min_vv, (k),		\
		  &p_flag) == 0) {				\
			min_vv = (k);				\
			ppr = (pixr *) page->load_i;		\
		}						\
	}							\
	if ((k) + (h) > max_vv) {				\
		if (pg_adjust(page, MV_DOWN, max_vv, ((k) + (h)),\
		  &p_flag) == 0) { 				\
			max_vv = (k) + (h);			\
			ppr = (pixr *) page->load_i;		\
		}						\
	}

read_page(page)
	pg	*page;
{
	register int		cmd,
				k_lim = dvi->kern_limit,
				b_lim = dvi->baseln_limit;
	register s_char		*sp = page->s_buf;
	register FILE		*fp = dvi->file;
	register char_entry	*cp;
	register int		cmd_offset = 0;
	unsigned int		unsigned_4();
	int			ch_x,
				ch_y,
				ch_w,
				ch_h;

	min_vv = min_hh = p_flag = line_c = at_line_break = rule_addr = 0;
	max_hh = page->load_r.r_width;
	max_vv = page->load_r.r_height;

	ppr = (pixr *) page->load_i;
	for(;; cmd_offset++)	{
		UNSIGNED_1(cmd, fp);

		if (cmd >= SETC_000 && cmd <= SETC_127) {
			hh = (int) (h + (hconv >> 1)) / hconv;
			vv = (int) (v + (vconv >> 1)) / vconv;

			if (cur_font->flags & NOT_FOUND) {
				cp = cur_font->ch;
			} else {
				cp = &(cur_font->ch[cmd]);
				if (!(cp->flags & C_LOADED)) {
					if (cp->flags & C_PK) {
						load_pk_char(cp);
					} else {
						load_pxl_char(cp);
					}
				}
			}

			ch_x = hh - cp->x_offset;
			ch_y = vv - cp->y_offset;
			ch_w = cp->width;
			ch_h = cp->height;

			/*
			 * don't enlarge the page for missing characters.
			 */
			if ((cur_font->flags & NOT_FOUND) == 0) {
				CHECK_HH(ch_x, ch_w);
				CHECK_VV(ch_y, ch_h);
			}

			ch_x -= min_hh;
			ch_y -= min_vv;

			if (ch_w != 0 && ch_h != 0) {
				pr_rop(ppr,
				  ch_x, ch_y,
				  ch_w, ch_h,
				  sunp->or_op,
				  cp->address.pr,
				  0, 0);
			}

			h += cp->tfmw;

			/*
			 * if this is the first character after a line
			 * break, we need to put the address of this char
			 * into the lines array.
			 */
			if (at_line_break) {
				/* always leave space for the ending null */
				if (line_c == page->max_lines) {
					add_to_lines(page);
				}
				page->lines[line_c++] = sp;
				at_line_break = 0;
				/* and mark the end of the line. */
				if ((sp - 1)->c_type == S_SEARCHABLE) {
					sp->c = ' ';
					sp->c_type = (u_char) S_EOL;
					sp++;
				}
			}
			sp->c = (u_char) cmd;
			sp->f = (u_char) f;
			sp->c_type = (u_char) S_SEARCHABLE;
			sp->x = (short) ch_x;
			sp->y = (short) ch_y;
			sp->f_info = cp;
			sp++;
			continue;
		}
		if (cmd == W0) {
			h += w;
			SPACE(w);
			continue;
		}
		if (cmd >= FONT_00 && cmd <= FONT_63) {
			set_font_num(cmd - FONT_00);
			f = cmd - FONT_00;
			continue;
		}
		if (cmd == RIGHT3) {
			SIGNED_3(cmd, fp);
			h += cmd;
			SPACE(cmd);
			continue;
		}
		if (cmd == PUSH) {
			if (stack >= STACKSIZE)
				msg(FATAL, "stack overflow");
			page_stack[stack].h = h;
			page_stack[stack].v = v;
			page_stack[stack].w = w;
			page_stack[stack].x = x;
			page_stack[stack].y = y;
			page_stack[stack].z = z;
			stack++;
			/* check for a line break */
			val = cmd_offset;
			/*
			 * we consider a hrule in between the pop and the
			 * push to be a linebreak as well as the usual
			 * POP-DOWN-PUSH sequence.
			 */
			if ((pop_addr < rule_addr && rule_addr < down_addr) ||
			  (down_addr == --val && pop_addr == --val)) {
				at_line_break++;
			}
			continue;
		}
		if (cmd == POP) {
			--stack;
			if (stack < 0)
				msg(FATAL, "stack underflow");
			h = page_stack[stack].h;
			v = page_stack[stack].v;
			w = page_stack[stack].w;
			x = page_stack[stack].x;
			y = page_stack[stack].y;
			z = page_stack[stack].z;
			pop_addr = cmd_offset;
			continue;
		}
		if (cmd == X0) {
			h += x;
			SPACE(x)
			continue;
		}
		if (cmd == DOWN3) {
			SIGNED_3(cmd, fp);
			v += cmd;
			LINEBREAK(cmd)
			continue;
		}
		if (cmd == W3) {
			SIGNED_3(w, fp);
			h += w;
			SPACE(w)
			continue;
		}
		if (cmd == RIGHT2) {
			SIGNED_2(cmd, fp);
			h += cmd;
			SPACE(cmd)
			continue;
		}
		if (cmd == Y0) {
			v += y;
			LINEBREAK(y)
			continue;
		}
		if (cmd == X2) {
			x = signed_2(fp);
			h += x;
			SPACE(x)
			continue;
		}
		if (cmd == W2) {
			w = signed_2(fp);
			h += w;
			SPACE(w)
			continue;
		}
		if (cmd == X3) {
			x = signed_3(fp);
			h += x;
			SPACE(x);
			continue;
		}
		if (cmd == RIGHT4) {
			cmd = signed_4(fp);
			h += cmd;
			SPACE(cmd);
			continue;
		}
		if (cmd == PUT_RULE) {
			MARK_RULE;
			cmd = (unsigned) unsigned_4(fp);
			val = (unsigned) unsigned_4(fp);
			set_rule(page, cmd, val);
			continue;
		}
		if (cmd == Z0) {
			v += z;
			LINEBREAK(z)
			continue;
		}
		if (cmd == DOWN4) {
			cmd = signed_4(fp);
			v += cmd;
			LINEBREAK(cmd)
			continue;
		}
		if (cmd == SET_RULE) {
			MARK_RULE;
			cmd = (unsigned) unsigned_4(fp);
			val = (unsigned) unsigned_4(fp);
			set_rule(page, cmd, val);
			h += val;
			SPACE(val)
			continue;
		}
		if (cmd == Y3) {
			y = signed_3(fp);
			v += y;
			LINEBREAK(y)
			continue;
		}
		if (cmd == Z3) {
			z = signed_3(fp);
			v += z;
			LINEBREAK(z);
			continue;
		}
		if (cmd == BOP) {
			/*
			 * skip over the parameters of the BOP.
			 * c0[4]...c9[4], p[4].
			 */
			fseek(fp, (long) 44, 1);
			h = v = w = x = y = z = stack = 0;
			cur_font = (fontp *) 0;
			sp->c_type = (u_char) S_END;
			sp++;
			continue;
		}
		if (cmd == FNT_DEF1) {
			cmd = unsigned_1(fp);
			skip_font_def(fp, cmd);
			continue;
		}
		if (cmd == EOP) {
			sp->c_type = S_END;
			page->line_c = line_c;
			page->s_buf_end = sp;
			return(0);
		}
		/*
		 * the rest of these commands weren't even in any of the
		 * files I looked at...
		 */
		/*
		 * but I expect that the special commands will show up
		 * someday.
		 */
		if (cmd == XXX1) {
			cmd = unsigned_1(fp);
			fseek(fp, cmd, 1);
			continue;
		}
		if (cmd == XXX2) {
			cmd = unsigned_2(fp);
			fseek(fp, cmd, 1);
			continue;
		}
		if (cmd == XXX3) {
			cmd = unsigned_3(fp);
			fseek(fp, cmd, 1);
			continue;
		}
		if (cmd == XXX4) {
			cmd = (unsigned) unsigned_4(fp);
			fseek(fp, cmd, 1);
			continue;
		}
		
		if (cmd == SET1 || cmd == SET2 || cmd == SET3 || cmd == SET4) {
			val = no_sign_extend(fp, cmd - SET1 + 1);
			set_char(page, val, 1);
			h += cp->tfmw;
			continue;
		}
		if (cmd == PUT1 || cmd == PUT2 || cmd == PUT3 || cmd == PUT4) {
			val = no_sign_extend(fp, cmd - PUT1 + 1);
			set_char(page, val, 0);
			continue;
		}
		if (cmd == RIGHT1) {
			cmd = signed_1(fp);
			h += cmd;
			SPACE(cmd)
			continue;
		}
		if (cmd == W1) {
			w = signed_1(fp);
			h += w;
			SPACE(w)
			continue;
		}
		if (cmd == W4) {
			w = signed_4(fp);
			h += w;
			SPACE(w);
			continue;
		}
		if (cmd == X1) {
			x = signed_1(fp);
			h += x;
			SPACE(x);
			continue;
		}
		if (cmd == X4) {
			x = signed_4(fp);
			h += x;
			SPACE(x)
			continue;
		}
		if (cmd == DOWN1) {
			cmd = signed_1(fp);
			v += cmd;
			LINEBREAK(cmd)
			continue;
		}
		if (cmd == DOWN2) {
			cmd = signed_2(fp);
			v += cmd;
			LINEBREAK(cmd)
			continue;
		}
		if (cmd == Y1) {
			y = signed_1(fp);
			v += y;
			LINEBREAK(y)
			continue;
		}
		if (cmd == Y2) {
			y = signed_2(fp);
			v += y;
			LINEBREAK(y)
			continue;
		}
		if (cmd == Y4) {
			y = signed_4(fp);
			v += y;
			LINEBREAK(y)
			continue;
		}
		if (cmd == Z1) {
			z = signed_1(fp);
			v += z;
			LINEBREAK(z)
			continue;
		}
		if (cmd == Z2) {
			z = signed_2(fp);
			v += z;
			LINEBREAK(z)
			continue;
		}
		if (cmd == Z4) {
			z = signed_4(fp);
			v += z;
			LINEBREAK(z)
			continue;
		}
		if (cmd >= FNT_DEF2 && cmd <= FNT_DEF4) {
			switch(cmd) {
			case FNT_DEF2:
				cmd = unsigned_2(fp);
				break;
			case FNT_DEF3:
				cmd = unsigned_3(fp);
				break;
			case FNT_DEF4:
				cmd = signed_4(fp);
				break;
			}
			skip_font_def(dvi->file, cmd);
			continue;
		}
		if (cmd == FNT1 || cmd == FNT2 || cmd == FNT3 || cmd == FNT4) {
			val = no_sign_extend(fp, cmd - FNT1 + 1);
			set_font_num(val);
			f = val;
			continue;
		}

		msg(PLAIN, 
		  "%s: Unknown DVI command %d in page -- bad DVI file\n",
		  dvi->fname, cmd);
		return(-1);
	}
}

signed_1(fp)
	register FILE	*fp;
{
	register int	val;
	SIGNED_1(val, fp);
	return(val);
}

signed_2(fp)
	register FILE	*fp;
{
	register int	val;
	SIGNED_2(val, fp);
	return(val);
}

signed_3(fp)
	register FILE	*fp;
{
	register int	val;
	SIGNED_3(val, fp);
	return(val);
}

signed_4(fp)
	register FILE	*fp;
{
	register int	val;
	SIGNED_4(val, fp);
	return(val);
}

unsigned_1(fp)
	register FILE	*fp;
{
	register int	val;
	UNSIGNED_1(val, fp);
	return(val);
}

unsigned_2(fp)
	register FILE	*fp;
{
	register int	val;
	UNSIGNED_2(val, fp);
	return(val);
}

unsigned_3(fp)
	register FILE	*fp;
{
	register int	val;
	UNSIGNED_3(val, fp);
	return(val);
}

unsigned
unsigned_4(fp)
	register FILE		*fp;
{
	register unsigned int	val;
	UNSIGNED_4(val, fp);
	return(val);
}

/* to read n bytes we need to read n times and shift n - 1 times */

no_sign_extend(fp, n)
	register FILE	*fp;
	register int	n;
{
	register int x;

	x = 0;
	while (--n)  {
		x |= getc(fp);
		x <<= 8;
	}
	x |= getc(fp);
	return(x);
}


/* return n byte quantity from file fp */

sign_extend(fp, n)
	register FILE	*fp;
	register int	n;

{
	register int	x,
			n1;
	/* get first (high-order) byte */
	x = getc(fp);
	n1 = n--;
	while (n--) {
		x <<= 8;
		x |= getc(fp);
	}

	/*
	 * NOTE: This code assumes that the right-shift is an arithmetic,
	 * rather than logical, shift which will propagate the sign bit
	 * right.  According to Kernighan and Ritchie, this is compiler
	 * dependent!
	 */
	x <<= 32 - 8 * n1;
	/* sign extend */
	x >>= 32 - 8 * n1;  
	return(x);
}

set_char(page, c, move_over)
	pg	*page;
{
	register char_entry	*cp;
	register int		x, y, h, w;
	

	if (cur_font->flags & NOT_FOUND) {
		cp = cur_font->ch;
	} else {
		cp = &(cur_font->ch[c]);
		if (!(cp->flags & C_LOADED)) {
			if (cp->flags & C_PK) {
				load_pk_char(cp);
			} else {
				load_pxl_char(cp);
			}
		}
	}

	hh = pix_round(h, hconv);
	vv = pix_round(v, vconv);

	x = hh - cp->x_offset;
	y = vv - cp->y_offset;
	w = cp->width;
	h = cp->height;

	CHECK_HH(x, w);
	CHECK_VV(y, h);
	x -= min_hh;
	y -= min_vv;

	if (w != 0 && h != 0) {
		pr_rop(ppr,
		  x, y,
		  w, h,
		  sunp->or_op,
		  cp->address.pr,
		  0, 0);
	}

	if (move_over)
		h += cp->tfmw;
}



set_rule(page, a, b)
	pg	*page;
{
	extern int	hconv,
			vconv;
	int		ehh,
			evv;
	
	hh  = pix_round(h, hconv);
	vv  = pix_round(v - a, vconv);
	ehh = pix_round(h + b, hconv);
	evv = pix_round(v, vconv);
	if (hh == ehh)
		ehh++;
	if (vv == evv)
		vv--;

	CHECK_HH(hh, ehh-hh);
	CHECK_VV(vv, evv-vv);
	hh -= min_hh;
	vv -= min_vv;
	ehh -= min_hh;
	evv -= min_vv;

	if ((a > 0) && (b > 0))
		pr_rop(ppr,
		  hh, vv,
		  ehh - hh, evv - vv,
		  sunp->set_op,
		  NULL, 0, 0);
}
