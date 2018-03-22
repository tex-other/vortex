/* 
 * Copyright (c) 1986-1992 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/*  Iparse.c
 *  This file is part of IncTeX 1.0
 *
 *  Copyright (C) 1992 by Regents of the University of California
 *
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
 *
 * Technique: scan thru nodes 1st pass, set up info in parse_number1[],
 * copy mem[] to copy_of_mem[] (for efficiency, later, could run thru
 * used list and copy just those sections, or even hack up ALL the macro's 
 * to use a pointer telling which copy of mem[] to access, but this would
 * be major hand work.  We'll figure on token[] being same, will also have
 * to check string & font arrays.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#ifdef INCTEX
#include	"tex.h"
#include	"texext.h"
#include	"align.h"
#include	"tfm.h"
#include	"math.h"
#include	"box.h"
#include	"token.h"
#include	"tokenlists.h"
#include	"evalstack.h"
#include	"page.h"
#include	"cond.h"
#include	"hyph.h"
#include	"eqstack.h"
#include	"Imain.h"
#include	"Icmp.h"

extern	val     depth_threshold;
extern	val     breadth_max;
extern	fnt     font_in_short_display;
extern	list    cur_list;
extern	list    nest[NEST_SIZE];
extern	ptr     nest_ptr;
extern	int     max_nest_stack;

int	old_selector;
int	check_error = FALSE;
int	cur_parse_count;
int	parse_number1[MEM_MAX-MEM_MIN+1];
int	parse_number2[MEM_MAX-MEM_MIN+1];
int	*cur_p_number;
mword	copy_of_mem[MEM_MAX-MEM_MIN+1];

FILE	*parse_check_file;

#define CHECK_FILE            15 
#define NODE_BODY             ((short) -1)

#define nested_list_parse(N) \
    {append_char('.'); parse_node_list(N); flush_char();}

/*!! #define parse_count(P) cur_p_number[P]*/
#define parse_count(P) *(cur_p_number+P)

/* Don't print mode line number,
 * Convert show_ligature so it does show_node_list instead of short_display.
 * Make a copy of print_spec so we can parse glue spec's, this is the only
 *	one that allows repeated visits to a node for now.
 */

parse_mem()
{
    int	    old_selector,i,ok;

    old_selector = selector;
    selector = CHECK_FILE;

    ok = TRUE;
    cur_parse_count = 1;
    parse_stat_glue();
    parse_activities();
    parse_hyphens();
    parse_eqtb();
    parse_savestack();
    parse_font_glue();
    parse_condstack();
    ok = parse_alignstack();
    print_nl("Special in-mem locations");
    for (i=HI_MEM_STAT_MIN;i<=MEM_TOP;i++) {		/* special ptrs */
    	if (parse_count(i) == 0)
    		parse_count(i) = cur_parse_count++;
	print_nl("@");print_int(i);print_char('-');
    	if ((link(i)<=MEM_BOT) || (link(i)>MEM_TOP))
		print_int(0);
	else {
		/*print_char('<');print_int(link(i));print_char('>');*/
		print_int(parse_count(link(i)));
	}
	/* !! now should print parse_number[link(i)] */
    }
    parse_count(lo_mem_max) = cur_parse_count++;	/* unused, really */
    /*print_char('<');print_int(lo_mem_max);print_char('>');*/
    print_nl("Parse count = "); print_int(cur_parse_count);print_ln();

    selector = old_selector;
    return(ok);
}

parse_activities ()
{
    val     a;
    int     p;
    int     m;
    ptr     q;
    ptr     r;
    val     t;

    nest[nest_ptr] = cur_list;
    print_ln();
    for (p = nest_ptr; p >= 0; decr(p)) {
        m = nest[p].mode_field;
        a = nest[p].aux_field;
        print_nl("### ");
        print_mode(m);
        /*print(" entered at line ");*/
        print(" entered ");
        /*print_val(abs(nest[p].ml_field));*/
        if (nest[p].ml_field < 0)
            print(" (\\output routine)");
        if (p == 0) {
            if (page_head != page_tail) {
                print_nl("### current page:");
                if (output_active)
                    print(" (held over for next output)");
    		if (parse_count(page_head) == 0) {		/*!*/
    		    parse_count(page_head) = cur_parse_count++;	/*!*/
		    /*print_char('<');print_int(page_head);print_char('>');*/
		}
                parse_box(link(page_head));
                if (page_contents > EMPTY) {
                    print_nl("total height ");
                    print_totals();
                    print_nl(" goal height ");
                    print_scaled(page_goal);
                    r = link(page_ins_head);
                    while (r != page_ins_head) {
                        print_ln();
                        print_esc("insert");
                        t = qo(subtype(r));
                        print_int((int) t);
                        print(" adds ");
                        t = x_over_n(height(r), 1000L) * count(t);
                        print_scaled((scal) t);
    			if (parse_count(p) == 0) {/*!*/
    			  parse_count(p) = cur_parse_count++;/*!*/
    			  parse_count(p+1) = parse_count(p+2) = parse_count(p+3)
   	 			= NODE_BODY; /*!*/
			} else if (TALK) {/*!*/
    			  selector = old_selector;
			  print_nl("!!Page insertion already parsed, node ");/*!*/
			  print_int(r);/*!*/
    			  selector = CHECK_FILE;
			}/*!*/
                        if (type(r) == SPLIT_UP) {
                            q = page_head;
                            t = 0;
                            do  {
                                q = link(q);
                                if (type(q) == INS_NODE && 
                                    subtype(q) == subtype(r))
                                    incr(t);
                            } while (q != broken_ins(r));
                            print(", #");
                            print_int((int) t);
                            print(" might split");
                        }
                        r = link(r);
                    }
                }
            }
	    if (contrib_head != nest[p].head_field) {
		print_nl("!Error, contrib_head != nest[0].head_field!");
		check_error = TRUE;
	    }
            if (link(contrib_head) != NULL)
                print_nl("### recent contributions:");
        }
    	if (parse_count(nest[p].head_field) == 0) {		/*!*/
    	    parse_count(nest[p].head_field) = cur_parse_count++;/*!*/
	    /*print_char('<');print_int(nest[p].head_field);print_char('>');*/
	}
        parse_box(link(nest[p].head_field)); 
        switch (abs(m) / (MAX_COMMAND + 1))
        {
        case 0:
            print_nl("prevdepth ");
            if (a <= IGNORE_DEPTH)
                print("ignored");
            else print_scaled((scal) a);
            if (nest[p].pg_field != 0) {
                print(", prevgraf ");
                print_int(nest[p].pg_field);
                print(" line");
                if (nest[p].pg_field != 1)
                    print_char('s');
            }
            break;

        case 1:
            print_nl("spacefactor ");
            print_int((int) a);
            break;

        case 2:
            if (a != NULL) {
                print_nl("this will be denominator of:");
                parse_box((ptr) a);
            }
            break;
        }
    }
    print_nl("Parse count = "); print_int(cur_parse_count);
}

#define parse_error(P) {print("!!parse error hyphen node ");\
	print_int(P);print(" ");}

parse_hyphens()
{	/* print hyphen exception entry, then info() fields of linked list */
	int     h;
	ptr	p;

	print_nl("hyphen_list:");print_ln();
        for (h=0;h<=HYPH_SIZE;h++) {
            if (hyph_list[h] != 0) {
	      print_char('@'); print_int(h);
	      p = hyph_list[h];
	      do {	/* chase linked list */
		if (p<hi_mem_min) parse_error(p);	/*!*/
    		if (parse_count(p) != 0) parse_error(p);/*!*/
    		parse_count(p) = cur_parse_count++;	/*!*/
		print_char('-');
		/*print_char('<');print_int(p);print_char('>');*/
		print_int(info(p));
		p = link(p);
              } while (p != 0);
	      print_ln();
            }
        }
    print_nl("Parse count = "); print_int(cur_parse_count);
}

parse_eqtb()
{
	int     n;
	ptr	p;

	print_nl("eqtb3:");print_ln();
	/* reg 3 */
	for (n=GLUE_BASE;n<LOCAL_BASE;n++) {
	   p = equiv(n);
	   if (p != NULL) {
		print_char('@'); print_int(n);print_char('-');
		parse_spec(p,"");
		print_ln();
	   }
	}
	print_nl("eqtb4:");print_ln();
	/* reg 4 */
	print_char('@'); print_int(PAR_SHAPE_LOC);print_char('-');
	if (equiv(PAR_SHAPE_LOC)==NULL) print_int(0);
	else {
	    print("par shape size = 1 + 2*");print_int(info(PAR_SHAPE_LOC));
    	    if (parse_count(par_shape_ptr) != 0)
		print_nl("!!parse error par_shape_ptr");
    	    parse_count(par_shape_ptr) = cur_parse_count++;/*!*/
	    /*print_char('<');print_int(par_shape_ptr);print_char('>');*/
	    for (n=par_shape_ptr+2;n<=par_shape_ptr+1+2*info(par_shape_ptr);n++)
    	    	if (parse_count(n) == 0)
	  		parse_count(n) = NODE_BODY;
		else
			{print_nl("!!parse error at ");print_int(n);break;}
	}
	print_ln();
	for (n=BOX_BASE;n<CUR_FONT_LOC;n++) {
	   p = equiv(n);
	   if (p != NULL) {
		print_char('@'); print_int(n);print_char('-');
		parse_node_list(p);
		print_ln();
	   }
	}
    	print_nl("Parse count = "); print_int(cur_parse_count);
	print_nl("GLUE_BASE:");print_int(GLUE_BASE);
	print_nl("LOCAL_BASE:");print_int(LOCAL_BASE);
	print_nl("PAR_SHAPE_LOC:");print_int(PAR_SHAPE_LOC);
	print_nl("BOX_BASE:");print_int(BOX_BASE);
	print_nl("CUR_FONT_LOC:");print_int(CUR_FONT_LOC);
}

parse_savestack()
{
	int n,flag;
	ptr i,e,j;

	print_nl("save stack:");
	if (save_ptr>0)
	   for (n=save_ptr-1;n>=0;n--)
	      if (save_type(n) == RESTORE_OLD_VALUE) {
		i=save_index(n);
		if (n>0)
			n--;	/* Pick up value off next item */
		else {	print_nl("!!Restore dropped off end of save stack!");
			printf("!!Restore dropped off end of save stack!\n");
			break;	/* ? means save stack messed up ? */
		}
		if ((i>=GLUE_BASE && i<LOCAL_BASE)   ||
		    (i>=BOX_BASE  && i<CUR_FONT_LOC) ||
		    (i==PAR_SHAPE_LOC)) {  /* shape_loc < box_base actually */
			e = equiv_field(save_stack[n]);
			print_ln();print_char('@');print_int(i);print_char('-');
			if (i<LOCAL_BASE)
			   parse_spec(e,"");
			else if (i>=BOX_BASE) {
			   if (parse_count(e)==0)
			   	parse_node_list(e);
			   else
				{print_char('#');print_int(parse_count(e));}
			} else if (e==NULL) print_char('0');/* null PAR_SHAPE */
			else {	/* print left margin & line lens of PAR_SHAPE */
	    		     print("par shape size = 1+2*");print_int(info(e));
    	    		     if (flag = (parse_count(e) == 0)) {
    	    			parse_count(e) = cur_parse_count++;
				/*print_char('<');print_int(e);print_char('>');*/
			     }
	    		     for (j=e+1;j<=e+2*info(e);j++)
				if (!flag) {
				   print_char('=');print_int(mem[j].i);
    	    			} else if (parse_count(j) == 0) {
	  			   parse_count(j) = NODE_BODY;
				   print_char(' ');print_int(mem[j].i);
				} else {
				   print_nl("!!parse error at ");
				   print_int(j);break;
				} /* if stmt */
			} /* show par shape */
		} /* if >= <= */
	    } /* if RESTORE_OLD_VALUE */
    	print_nl("Parse count = "); print_int(cur_parse_count);
	print_ln();
}

#undef parse_error
#define parse_error(P) {print("!!parse error static glue def ");\
	print_int(P);print(" ");}

parse_stat_glue()
{
	int     q;

	print_nl("static glue def:");print_ln();
	for (q=0;q<LO_MEM_STAT_MAX;q=q+GLUE_SPEC_SIZE) {
	    print_char('@'); print_int(q);print_char('-');
	    parse_spec(q,"");
	    print_ln();
	}
    	print_nl("Parse count = "); print_int(cur_parse_count);
}

#undef parse_error
#define parse_error(P) {print("!!parse error font_glue node ");\
	print_int(P);print(" ");}

parse_font_glue()
{
	int     q;

	print_nl("font_glue:");print_ln();
	for (q=0;q<font_ptr;q++) {
	  if (font_glue[q] != NULL) {
	    print_char('@'); print_int(q);print_char('-');
	    parse_spec(font_glue[q],"");
	    print_ln();
	  }
	}
    	print_nl("Parse count = "); print_int(cur_parse_count);
}

/* ignore if_line (line number field) */
parse_condstack()
{
	int     p;

	print_nl("condstack:");print_ln();
	p = cond_ptr;
	while (p != NULL) {
	   if (p>lo_mem_max) parse_error(p);/*!*/
    	   if (parse_count(p) != 0) parse_error(p);/*!*/
    	   parse_count(p) = cur_parse_count++;/*!*/
	   /*print_char('<');print_int(p);print_char('>');*/
    	   parse_count(p+1) = NODE_BODY;
	   print_char('c');print_int(subtype(p));;
	   print_char('l');print_int(type(p));;
	   p = link(p);
	}
    	print_nl("Parse count = "); print_int(cur_parse_count);
	print_ln();
}

int
parse_alignstack()
{
    if (align_ptr != NULL) {
        if (TALK) {print("!!align stack not null");print_ln();}
	return(FALSE);
    } else {
    	/* if (explain) {print_nl("alignstack:");print_ln();} */
    	return(TRUE);
    }
}

#undef parse_error
#define parse_error(P) {print("!!parse error node ");print_int(P);print(" ");}

parse_box (p)
    ptr     p;
{
    depth_threshold = 99999;
    breadth_max = 99999;
    parse_node_list(p);
}

parse_box1 (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = parse_count(p+2) = parse_count(p+3) = /*!*/
   	 parse_count(p+4) = parse_count(p+5) = parse_count(p+6) = NODE_BODY;
    if (type(p) == HLIST_NODE)
        print_esc("h");
    else if (type(p) == VLIST_NODE)
        print_esc("v");
    else print_esc("unset");
    print("box(");
    print_scaled(height(p));
    print_char('+') ;
    print_scaled(depth(p));
    print(")x") ;
    print_scaled(width(p));
    if (type(p) == UNSET_NODE) {
        if (span_count(p) != MIN_QUARTERWORD) {
            print(" (");
            print_int(qo(span_count(p))+1);
            print(" columns)");
        }
        if (glue_stretch(p) != 0) {
            print(", stretch ");
            print_glue(glue_stretch(p), glue_order(p), "");
        }
        if (glue_shrink(p) != 0) {
            print(", shrink ");
            print_glue(glue_shrink(p), glue_sign(p), "");
        }
    } else {
        show_glue_set(p);
        if (shift_amount(p) != 0) {
            print(", shifted ");
            print_scaled(shift_amount(p));
        }
    }
    nested_list_parse(list_ptr(p));
}

parse_rule (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = parse_count(p+2) = parse_count(p+3) = NODE_BODY;/*!*/
    print_esc("rule(");
    print_rule_dimen(height(p));
    print_char('+');
    print_rule_dimen(depth(p));
    print(")x");
    print_rule_dimen(width(p));
}

parse_insertion (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = parse_count(p+2) = parse_count(p+3) = parse_count(p+4)
                     = NODE_BODY;/*!*/
    print_esc("insert");
    print_int(qo(subtype(p)));
    print(", natural size ");
    print_scaled(height(p));
    print("; split(");
    parse_spec(split_top_ptr(p), "");
    print_char(',');
    print_scaled(depth(p));
    print("); float cost ");
    print_val(float_cost(p));
    nested_list_parse(ins_ptr(p));
}

parse_spec(p, s)
    ptr     p;
    chrs    s;
{
    if (p < MEM_MIN || p >= hi_mem_min) {
        print_char('*');
    } else {
	if (parse_count(p) == 0) { /*! Okay for duplicate references to spec's*/
	  parse_count(p) = cur_parse_count++;/*!*/
    	  /*print_char('<');print_int(p);print_char('>');*/
	  parse_count(p+1) = parse_count(p+2) = parse_count(p+3) = NODE_BODY;
	}
        print_scaled(width(p));
        print(s);
        if (stretch(p) != 0) {
            print(" plus ");
            print_glue(stretch(p), stretch_order(p), s);
        }
        if (shrink(p) != 0) {
            print(" minus ");
            print_glue(shrink(p), shrink_order(p), s);
        }
    }
}

parse_leaders (p)
    ptr     p;
{
    print_esc("");
    if (subtype(p) == C_LEADERS)
        print_char('c');
    else if (subtype(p) == X_LEADERS)
        print_char('x');
    print("leaders ");
    parse_spec(glue_ptr(p), "");
    nested_list_parse(leader_ptr(p));
}

parse_glue (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = NODE_BODY;/*!*/
    if (subtype(p) >= A_LEADERS) {
        parse_leaders(p);
    } else {
        print_esc("glue");
        if (subtype(p) != NORMAL) {
            print_char('(');
            if (subtype(p) < COND_MATH_GLUE)
                print_skip_param(subtype(p) - 1);
            else if (subtype(p) == COND_MATH_GLUE)
                print_esc("nonscript");
            else print_esc("mskip");
            print_char(')');
        }
        if (subtype(p) != COND_MATH_GLUE) {
            print_char(' ');
            if (subtype(p) < COND_MATH_GLUE)
                parse_spec(glue_ptr(p), "");
            else parse_spec(glue_ptr(p), "mu");
        }
    }
}

parse_kern (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = NODE_BODY;/*!*/
    if (subtype(p) != MU_GLUE) {
        print_esc("kern");
        if (subtype(p) != NORMAL)
            print_char(' ');
        print_scaled(width(p));
        if (subtype(p) == ACC_KERN)
            print(" (for accent)");
    } else {
        print_esc("mkern");
        print_scaled(width(p));
        print("mu");
    }
}

parse_math (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = NODE_BODY;/*!*/
    print_esc("math");
    if (subtype(p) == BEFORE)
        print("on");
    else print("off");
    if (width(p) != 0) {
        print(", surrounded ");
        print_scaled(width(p));
    }
}
    
parse_ligature (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = NODE_BODY;/*!*/
    parse_font_and_char(lig_char(p));
    print(" (ligature ");
    font_in_short_display = font(lig_char(p));
    /*short_display(lig_ptr(p));*/
    parse_node_list(lig_ptr(p));
    print_char(')');
}

parse_discretionary (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = NODE_BODY;/*!*/
    print_esc("discretionary");
    if (replace_count(p) > 0) {
        print(" replacing ");
        print_int(replace_count(p));
    }
    nested_list_parse(pre_break(p));
    append_char('|');
    parse_node_list(post_break(p));
    flush_char();
}

parse_penalty (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = NODE_BODY;/*!*/
    print_esc("penalty ");
    print_val(penalty(p));
}

parse_mark (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = NODE_BODY;/*!*/
    print_esc("mark");
    print_mark(mark_ptr(p));/*!This shows token in token memory*/
}

parse_adjust (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = NODE_BODY;/*!*/
    print_esc("vadjust");
    nested_list_parse(adjust_ptr(p));
}

parse_font_and_char (p)
    ptr     p;
{
    if (p > mem_end) {
        print_esc("CLOBBERED.");
    } else {
        if (font(p) < FONT_BASE || font(p) > FONT_MAX) {
            print_char('*');
        } else {
            print_esc("");
            print_str(font_id_text(font(p)));
            print_char(' ');
            print_ASCII(qo(character(p)));
        }
    }
}

parse_node_list (p)
    ptr     p;
{
    int     n;

    if (cur_length() > depth_threshold) {
        if (p > NULL)
            print(" []");
        return;
    }
    n = 0;
    while (p > NULL) {
        print_ln();
        print_cur_str();
        if (p > mem_end) {
            print("Bad link, display aborted.");
            return;
        }
        incr(n);
        if (n > breadth_max) {
            print("etc.");
            return;
        }
        if (is_char_node(p)) {
	    if (p<hi_mem_min) parse_error(p);/*!*/
	    if (parse_count(p) != 0) parse_error(p);/*!*/
	    parse_count(p) = cur_parse_count++;/*!*/
    	    /*print_char('<');print_int(p);print_char('>');*/
            parse_font_and_char(p);
        } else {
            switch (type(p))
            {
            case HLIST_NODE:
            case VLIST_NODE:
            case UNSET_NODE:
                parse_box1(p);
                break;

            case RULE_NODE:
                parse_rule(p);
                break;
            
            case INS_NODE:
                parse_insertion(p);
                break;
            
            case WHATSIT_NODE:
                parse_whatsit(p);
                break;
            
            case GLUE_NODE:
                parse_glue(p);
                break;
            
            case KERN_NODE:
                parse_kern(p);
                break;
            
            case MATH_NODE:
                parse_math(p);
                break;
            
            case LIGATURE_NODE:
                parse_ligature(p);
                break;

            case PENALTY_NODE:
                parse_penalty(p);
                break;
        
            case DISC_NODE:
                parse_discretionary(p);
                break;
            
            case MARK_NODE:
                parse_mark(p);
                break;
            
            case ADJUST_NODE:
                parse_adjust(p);
                break;
            
            case STYLE_NODE:
                print_style(subtype(p));
                break;
            
            case CHOICE_NODE:
                parse_choice_node(p);
                break;

            case INNER_NOAD:
            case ORD_NOAD:
            case OP_NOAD:
            case BIN_NOAD:
            case REL_NOAD:
            case OPEN_NOAD:
            case CLOSE_NOAD:
            case PUNCT_NOAD:
            case RADICAL_NOAD:
            case OVER_NOAD:
            case UNDER_NOAD:
            case VCENTER_NOAD:
            case ACCENT_NOAD:
            case LEFT_NOAD:
            case RIGHT_NOAD:
                parse_normal_noad(p);
                break;
            
            case FRACTION_NOAD:
                parse_fraction_noad(p);
                break;

            default:
                print("Unknown node type!");
                break;
            }
        }
        p = link(p);
    }
}

parse_whatsit (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = NODE_BODY;/*!*/
    switch (subtype(p)) 
    { 
    case OPEN_NODE: 
	parse_count(p+1) = parse_count(p+2) = NODE_BODY;/*!*/
        print_write("openout", p);
        print_char('='); 
        print_file_name(open_name(p), open_area(p), open_ext(p)); 
        break; 
     
    case WRITE_NODE: 
        print_write("write", p); 
        print_mark(write_tokens(p)); 
        break; 
     
    case CLOSE_NODE: 
        print_write("closeout", p); 
        break; 
    
    case SPECIAL_NODE: 
        print_esc("special"); 
        print_mark(write_tokens(p)); 
        break; 
     
    default: 
        print("whatsit?"); 
        break;
    } 
}

parse_choice_node (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = parse_count(p+2) = NODE_BODY;/*!*/
    print_esc("mathchoice");
    append_char('D');
    parse_node_list(display_mlist(p));
    flush_char();
    append_char('T');
    parse_node_list(text_mlist(p));
    flush_char();
    append_char('S');
    parse_node_list(script_mlist(p));
    flush_char();
    append_char('s');
    parse_node_list(script_script_mlist(p));
    flush_char();
}

parse_normal_noad (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = parse_count(p+2) = parse_count(p+3) = NODE_BODY;/*!*/
    switch (type(p)) 
    {
    case ORD_NOAD:
        print_esc("mathord");
        break;

    case OP_NOAD:
        print_esc("mathop");
        break;

    case BIN_NOAD:
        print_esc("mathbin");
        break;

    case REL_NOAD:
        print_esc("mathrel");
        break;

    case OPEN_NOAD:
        print_esc("mathopen");
        break;

    case CLOSE_NOAD:    
        print_esc("mathclose");
        break;

    case PUNCT_NOAD:
        print_esc("mathpunct");
        break;

    case INNER_NOAD:
        print_esc("mathinner");
        break;

    case OVER_NOAD:
        print_esc("overline");
        break;

    case UNDER_NOAD:
        print_esc("underline");
        break;

    case VCENTER_NOAD:
        print_esc("vcenter");
        break;

    case RADICAL_NOAD:
	parse_count(p+4) = NODE_BODY;/*!*/
        print_esc("radical");
        print_delimiter(left_delimiter(p));
        break;

    case ACCENT_NOAD:
	parse_count(p+4) = NODE_BODY;/*!*/
        print_esc("accent");
        print_fam_and_char(accent_chr(p));
        break;

    case LEFT_NOAD:
        print_esc("left");
        print_delimiter(nucleus(p));
        break;

    case RIGHT_NOAD:
        print_esc("right");
        print_delimiter(nucleus(p));
        break;

    }
    if (subtype(p) != NORMAL)
        if (subtype(p) == LIMITS)
            print_esc("limits");
        else print_esc("nolimits");
    if (type(p) < LEFT_NOAD)
        print_subsidiary_data(nucleus(p), '.');
    print_subsidiary_data(supscr(p), '^');
    print_subsidiary_data(subscr(p), '_');
}

parse_fraction_noad (p)
    ptr     p;
{
    if (p>lo_mem_max) parse_error(p);/*!*/
    if (parse_count(p) != 0) parse_error(p);/*!*/
    parse_count(p) = cur_parse_count++;/*!*/
    /*print_char('<');print_int(p);print_char('>');*/
    parse_count(p+1) = parse_count(p+2) = parse_count(p+3) = 
	parse_count(p+4) = parse_count(p+5) = NODE_BODY;/*!*/
    print_esc("fraction, thickness ");
    if (thickness(p) == DEFAULT_CODE)
        print("= default");
    else print_scaled(thickness(p));
    if (small_fam(left_delimiter(p)) != 0 ||
        small_char(left_delimiter(p)) != MIN_QUARTERWORD ||
        large_fam(left_delimiter(p)) != 0 ||
        large_char(left_delimiter(p)) != MIN_QUARTERWORD) {
        print(", left-delimiter ");
        print_delimiter(left_delimiter(p));
    }
    if (small_fam(right_delimiter(p)) != 0 ||
        small_char(right_delimiter(p)) != MIN_QUARTERWORD ||
        large_fam(right_delimiter(p)) != 0 ||
        large_char(right_delimiter(p)) != MIN_QUARTERWORD) {
        print(", right-delimiter ");
        print_delimiter(right_delimiter(p));
    }
    print_subsidiary_data(numerator(p), '\\');
    print_subsidiary_data(denominator(p), '/');
}
#endif
