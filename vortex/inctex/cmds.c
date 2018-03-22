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

/*  This file is part of IncTeX 1.0
 *
 *  Copyright (C) 1992 by Regents of the University of California
 *
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
 */
/*
 * @(#)cmds.c 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include "tex.h"
#include "texext.h"
#include "eqstack.h"
#include "token.h"
#include "tokenlists.h"
#include "tokenstack.h"
#include "scan.h"
#include "evalstack.h"
#include "def.h"
#include "cond.h"
#include "expand.h"
#include "box.h"
#include "boxlists.h"
#include "tfm.h"
#include "math.h"
#include "mathlists.h"
#include "align.h"

#define chr_cmd(S) \
    {print(S);  print_ASCII(chr_code);}

show_cur_cmd_chr()
{
    begin_diagnostic();
    print_nl("{");
    if (mode != shown_mode) {
        print_mode(mode);
        print(": ");
        shown_mode = mode;
    }
    print_cmd_chr(cur_cmd, cur_chr);
    print_char('}'); 
    end_diagnostic(FALSE);
}

print_cmd_chr (cmd, chr_code)
    qword   cmd;
    hword   chr_code;
{
    switch (cmd)
    {
    case RELAX:
        print_esc("relax"); 
        break;

    case LEFT_BRACE:
        chr_cmd("begin-group character "); 
        break;

    case RIGHT_BRACE:
        chr_cmd("end-group character "); 
        break;

    case MATH_SHIFT:
        chr_cmd("math shift character "); 
        break;

    case TAB_MARK:
         if (chr_code == SPAN_CODE)
            print_esc("span");
        else chr_cmd("alignment tab character "); 
        break;

    case CAR_RET:
         if (chr_code == CR_CODE)
            print_esc("cr");
        else print_esc("crcr"); 
        break;

    case MAC_PARAM:
        chr_cmd("macro parameter character "); 
        break;

    case SUP_MARK:
        chr_cmd("superscript character "); 
        break;

    case SUB_MARK:
        chr_cmd("subscript character "); 
        break;

    case ENDV:
        print("end of alignment template"); 
        break;

    case SPACER:
        chr_cmd("blank space "); 
        break;

    case LETTER:
        chr_cmd("the letter "); 
        break;

    case OTHER_CHAR:
        chr_cmd("the character "); 
        break;

    case ASSIGN_GLUE:
    case ASSIGN_MU_GLUE:
        if (chr_code < SKIP_BASE)
            print_skip_param(chr_code - GLUE_BASE);
        else if (chr_code < MU_SKIP_BASE) {
            print_esc("skip"); 
            print_int(chr_code - SKIP_BASE);
        } else {
            print_esc("muskip");
            print_int(chr_code - MU_SKIP_BASE);
        } 
        break;

    case ASSIGN_TOKS:
        if (chr_code >= TOKS_BASE) {
            print_esc("toks");
            print_int(chr_code - TOKS_BASE);
        } else {
            switch (chr_code)
            {
            case OUTPUT_ROUTINE_LOC:
                print_esc("output"); 
                break;

            case EVERY_PAR_LOC:
                print_esc("everypar"); 
                break;

            case EVERY_MATH_LOC:
                print_esc("everymath"); 
                break;

            case EVERY_DISPLAY_LOC:
                print_esc("everydisplay"); 
                break;

            case EVERY_HBOX_LOC:
                print_esc("everyhbox"); 
                break;

            case EVERY_VBOX_LOC:
                print_esc("everyvbox"); 
                break;

            case EVERY_JOB_LOC:
                print_esc("everyjob"); 
                break;

            case EVERY_CR_LOC:
                print_esc("everycr"); 
                break;

            default:
                print_esc("errhelp"); 
                break;
            }
        }
        break;

    case ASSIGN_INT:
        if (chr_code < COUNT_BASE)
            print_param(chr_code - INT_BASE);
        else {
            print_esc("count");
            print_int(chr_code - COUNT_BASE);
        } 
        break;

    case ASSIGN_DIMEN:
        if (chr_code < SCALED_BASE)
            print_length_param(chr_code - DIMEN_BASE);
        else {
            print_esc("dimen");
            print_int(chr_code - SCALED_BASE);
        } 
        break;

    case ACCENT:
        print_esc("accent"); 
        break;

    case ADVANCE:
        print_esc("advance"); 
        break;

    case AFTER_ASSIGNMENT:
        print_esc("afterassignment"); 
        break;

    case AFTER_GROUP:
        print_esc("aftergroup"); 
        break;

    case ASSIGN_FONT_DIMEN:
        print_esc("fontdimen"); 
        break;

    case BEGIN_GROUP:
        print_esc("begingroup"); 
        break;

    case BREAK_PENALTY:
        print_esc("penalty"); 
        break;

    case CHAR_NUM:
        print_esc("char"); 
        break;

    case CS_NAME:
        print_esc("csname"); 
        break;

    case DEF_FONT:
        print_esc("font"); 
        break;

    case DELIM_NUM:
        print_esc("delimiter"); 
        break;

    case DIVIDE:
        print_esc("divide"); 
        break;

    case END_CS_NAME:
        print_esc("endcsname"); 
        break;

    case END_GROUP:
        print_esc("endgroup"); 
        break;

    case EX_SPACE:
        print_esc(" "); 
        break;

    case EXPAND_AFTER:
        print_esc("expandafter"); 
        break;

    case INPUT:
         if (chr_code == 0)
            print_esc("input");
        else print_esc("endinput"); 
        break;

    case HALIGN:
        print_esc("halign"); 
        break;

    case HRULE:
        print_esc("hrule"); 
        break;

    case IGNORE_SPACES:
        print_esc("ignorespaces"); 
        break;

    case INSERT:
        print_esc("insert"); 
        break;

    case ITAL_CORR:
        print_esc("/"); 
        break;

    case MARK:
        print_esc("mark"); 
        break;

    case TOP_BOT_MARK:
        switch (chr_code)
        {
        case TOP_MARK_CODE:
            print_esc("topmark"); 
            break;

        case FIRST_MARK_CODE:
            print_esc("firstmark"); 
            break;

        case BOT_MARK_CODE:
            print_esc("botmark"); 
            break;

        case SPLIT_FIRST_MARK_CODE:
            print_esc("splitfirstmark"); 
            break;

        case SPLIT_BOT_MARK_CODE:
            print_esc("splitbotmark"); 
            break;
        }
        break;

    case MATH_ACCENT:
        print_esc("mathaccent"); 
        break;

    case MATH_CHAR_NUM:
        print_esc("mathchar"); 
        break;

    case MATH_CHOICE:
        print_esc("mathchoice"); 
        break;

    case MULTIPLY:
        print_esc("multiply"); 
        break;

    case NO_ALIGN:
        print_esc("noalign"); 
        break;

    case NO_EXPAND:
        print_esc("noexpand"); 
        break;

    case NON_SCRIPT:
        print_esc("nonscript"); 
        break;

    case OMIT:
        print_esc("omit"); 
        break;

    case RADICAL:
        print_esc("radical"); 
        break;

    case READ_TO_CS:
        print_esc("read"); 
        break;

    case SET_BOX:
        print_esc("setbox"); 
        break;

    case SET_PREV_GRAF:
        print_esc("prevgraf"); 
        break;

    case SET_SHAPE:
        print_esc("parshape"); 
        break;

    case THE:
        print_esc("the"); 
        break;

    case TOKS_REGISTER:
        print_esc("toks"); 
        break;

    case VADJUST:
        print_esc("vadjust"); 
        break;

    case VALIGN:
        print_esc("valign"); 
        break;

    case VCENTER:
        print_esc("vcenter"); 
        break;

    case VRULE:
        print_esc("vrule"); 
        break;

    case PAR_END:
        print_esc("par"); 
        break;

    case SET_AUX:
         if (chr_code == VMODE)
            print_esc("prevdepth");
        else print_esc("spacefactor"); 
        break;

    case SET_PAGE_INT:
         if (chr_code == 0)
            print_esc("deadcycles");
        else print_esc("insertpenalties"); 
        break;

    case SET_BOX_DIMEN:
        if (chr_code == WIDTH_OFFSET)
            print_esc("wd");
        else if (chr_code == HEIGHT_OFFSET)
            print_esc("ht");
        else print_esc("dp"); 
        break;

    case SET_PAGE_DIMEN:
        switch (chr_code)
        {
        case 0:
            print_esc("pagegoal"); 
            break;

        case 1:
            print_esc("pagetotal"); 
            break;

        case 2:
            print_esc("pagestretch"); 
            break;

        case 3:
            print_esc("pagefilstretch"); 
            break;

        case 4:
            print_esc("pagefillstretch"); 
            break;

        case 5:
            print_esc("pagefilllstretch"); 
            break;

        case 6:
            print_esc("pageshrink"); 
            break;
        }
        break;

    case LAST_ITEM:
         if (chr_code == INT_VAL)
            print_esc("lastpenalty");
        else if (chr_code == DIMEN_VAL)
            print_esc("lastkern");
        else print_esc("lastskip"); 
        break;

    case REGISTER:
         if (chr_code == INT_VAL)
            print_esc("count");
        else if (chr_code == DIMEN_VAL)
            print_esc("dimen");
        else if (chr_code == GLUE_VAL)
            print_esc("skip");
        else print_esc("muskip"); 
        break;

    case CONVERT:
        switch (chr_code)
        {
        case NUMBER_CODE:
            print_esc("number"); 
            break;

        case ROMAN_NUMERAL_CODE:
            print_esc("romannumeral"); 
            break;

        case STRING_CODE:
            print_esc("string"); 
            break;

        case MEANING_CODE:
            print_esc("meaning"); 
            break;

        case FONT_NAME_CODE:
            print_esc("fontname"); 
            break;

        default:
            print_esc("jobname"); 
            break;
        }
        break;

    case IF_TEST:
        switch (chr_code)
        {
        case IF_CHAR_CODE:
            print_esc("if"); 
            break;

        case IF_CAT_CODE:
            print_esc("ifcat"); 
            break;

        case IF_INT_CODE:
            print_esc("ifnum"); 
            break;

        case IF_DIM_CODE:
            print_esc("ifdim"); 
            break;

        case IF_ODD_CODE:
            print_esc("ifodd"); 
            break;

        case IF_VMODE_CODE:
            print_esc("ifvmode"); 
            break;

        case IF_HMODE_CODE:
            print_esc("ifhmode"); 
            break;

        case IF_MMODE_CODE:
            print_esc("ifmmode"); 
            break;

        case IF_INNER_CODE:
            print_esc("ifinner"); 
            break;

        case IF_VOID_CODE:
            print_esc("ifvoid"); 
            break;

        case IF_HBOX_CODE:
            print_esc("ifhbox"); 
            break;

        case IF_VBOX_CODE:
            print_esc("ifvbox"); 
            break;

        case IFX_CODE:
            print_esc("ifx"); 
            break;

        case IF_EOF_CODE:
            print_esc("ifeof"); 
            break;

        case IF_TRUE_CODE:
            print_esc("iftrue"); 
            break;

        case IF_FALSE_CODE:
            print_esc("iffalse"); 
            break;

        case IF_CASE_CODE:
            print_esc("ifcase"); 
            break;
        }
        break;

    case FI_OR_ELSE:
        if (chr_code == FI_CODE)
            print_esc("fi");
        else if (chr_code == OR_CODE)
            print_esc("or");
        else print_esc("else"); 
        break;

    case PREFIX:
        if (chr_code == 1)
            print_esc("long");
        else if (chr_code == 2)
            print_esc("outer");
        else print_esc("global"); 
        break;

    case DEF:
        if (chr_code == 0)
            print_esc("def");
        else if (chr_code == 1)
            print_esc("gdef");
        else if (chr_code == 2)
            print_esc("edef");
        else print_esc("xdef");
        break;

    case LET:
        if (chr_code != NORMAL)
            print_esc("futurelet");
        else print_esc("let"); 
        break;

    case SHORTHAND_DEF:
        switch (chr_code)
        {
        case CHAR_DEF_CODE:
            print_esc("chardef"); 
            break;

        case MATH_CHAR_DEF_CODE:
            print_esc("mathchardef"); 
            break;

        case COUNT_DEF_CODE:
            print_esc("countdef"); 
            break;

        case DIMEN_DEF_CODE:
            print_esc("dimendef"); 
            break;

        case SKIP_DEF_CODE:
            print_esc("skipdef"); 
            break;

        case MU_SKIP_DEF_CODE:
            print_esc("muskipdef"); 
            break;

        default:
            print_esc("toksdef"); 
            break;
        }
        break;

    case CHAR_GIVEN:
        print_esc("char");
        print_hex((val) chr_code); 
        break;

    case MATH_GIVEN:
        print_esc("mathchar");
        print_hex((val) chr_code); 
        break;

    case DEF_CODE:
        if (chr_code == CAT_CODE_BASE)
            print_esc("catcode");
        else if (chr_code == MATH_CODE_BASE)
            print_esc("mathcode");
        else if (chr_code == LC_CODE_BASE)
            print_esc("lccode");
        else if (chr_code == UC_CODE_BASE)
            print_esc("uccode");
        else if (chr_code == SF_CODE_BASE)
            print_esc("sfcode");
        else print_esc("delcode"); 
        break;

    case DEF_FAMILY:
        print_size(chr_code - MATH_FONT_BASE); 
        break;

    case SET_FONT:
        print("select font ");
        print_str(font_name[chr_code]);
        if (font_size[chr_code] != font_dsize[chr_code]) {
            print(" at ");
            print_scaled(font_size[chr_code]);
            print("pt");
        } 
        break;

    case ASSIGN_FONT_INT:
        if (chr_code == 1)
            print_esc("skewchar");
        else print_esc("hyphenchar"); 
        break;

    case HYPH_DATA:
        if (chr_code == 1)
            print_esc("patterns");
        else print_esc("hyphenation"); 
        break;

    case SET_INTERACTION:
        switch (chr_code)
        {
        case BATCH_MODE:
            print_esc("batchmode"); 
            break;

        case NONSTOP_MODE:
            print_esc("nonstop"); 
            break;

        case SCROLL_MODE:
            print_esc("scrollmode"); 
            break;

        default:
            print_esc("errorstopmode"); 
            break;
        }
        break;

    case IN_STREAM:
        if (chr_code == 0)
            print_esc("closein");
        else print_esc("openin"); 
        break;

    case MESSAGE:
        if (chr_code == 0)
            print_esc("message");
        else print_esc("errmessage"); 
        break;

    case CASE_SHIFT:
        if (chr_code == LC_CODE_BASE)   
            print_esc("lowercase");
        else print_esc("uppercase"); 
        break;

    case XRAY:
        switch (chr_code)
        {
        case SHOW_BOX_CODE:
            print_esc("showbox"); 
            break;

        case SHOW_THE_CODE:
            print_esc("showthe"); 
            break;

        case SHOW_LISTS:
            print_esc("showlists"); 
            break;

        default:
            print_esc("show"); 
            break;
        }
        break;

    case UNDEFINED_CS:
        print("undefined"); 
        break;

    case CALL:
        print("macro"); 
        break;

    case LONG_CALL:
        print_esc("long macro"); 
        break;

    case OUTER_CALL:
        print_esc("outer macro"); 
        break;

    case LONG_OUTER_CALL:
        print_esc("long");
        print_esc("outer macro"); 
        break;

    case END_TEMPLATE:
        print_esc("outer endtemplate"); 
        break;

    case STOP:
        if (chr_code == 1)
            print_esc("dump"); 
        else print_esc("end"); 
        break;

    case HSKIP:
        switch (chr_code)
        {
        case SKIP_CODE:
            print_esc("hskip"); 
            break;

        case FIL_CODE:
            print_esc("hfil"); 
            break;

        case FILL_CODE:
            print_esc("hfill"); 
            break;

        case SS_CODE:
            print_esc("hss"); 
            break;

        default:
            print_esc("hfilneg"); 
            break;
        }
        break;

    case VSKIP:
        switch (chr_code)
        {
        case SKIP_CODE:
            print_esc("vskip"); 
            break;

        case FIL_CODE:
            print_esc("vfil"); 
            break;

        case FILL_CODE:
            print_esc("vfill"); 
            break;

        case SS_CODE:
            print_esc("vss"); 
            break;

        default:
            print_esc("vfilneg"); 
            break;
        }
        break;

    case MSKIP:
        print_esc("mskip"); 
        break;

    case KERN:
        print_esc("kern"); 
        break;

    case MKERN:
        print_esc("mkern"); 
        break;

    case HMOVE:
        if (chr_code == 1)
            print_esc("moveleft");
        else print_esc("moveright"); 
        break;

    case VMOVE:
        if (chr_code == 1)
            print_esc("raise");
        else print_esc("lower"); 
        break;

    case MAKE_BOX:
        switch (chr_code)
        {
        case BOX_CODE:
            print_esc("box"); 
            break;

        case COPY_CODE:
            print_esc("copy"); 
            break;

        case LAST_BOX_CODE:
            print_esc("lastbox"); 
            break;

        case VSPLIT_CODE:
            print_esc("vsplit"); 
            break;

        case VTOP_CODE:
            print_esc("vtop"); 
            break;

        case VTOP_CODE + VMODE:
            print_esc("vbox"); 
            break;

        default:
            print_esc("hbox");
            break;
        } 
        break;

    case LEADER_SHIP:
        if (chr_code == A_LEADERS)
            print_esc("leaders");
        else if (chr_code == C_LEADERS)
            print_esc("cleaders");
        else if (chr_code == X_LEADERS)
            print_esc("xleaders");
        else print_esc("shipout"); 
        break;

    case START_PAR:
        if (chr_code == 0)
            print_esc("noindent");
        else print_esc("indent"); 
        break;

    case REMOVE_ITEM:
        if (chr_code == GLUE_NODE)
            print_esc("unskip");
        else if (chr_code == KERN_NODE)
            print_esc("unkern");
        else print_esc("unpenalty"); 
        break;

    case UN_HBOX:
        if (chr_code == COPY_CODE)
            print_esc("unhcopy");
        else print_esc("unhbox"); 
        break;

    case UN_VBOX:
        if (chr_code == COPY_CODE)
            print_esc("unvcopy");
        else print_esc("unvbox"); 
        break;

    case DISCRETIONARY:
        if (chr_code == 1)
            print_esc("-");  
        else print_esc("discretionary"); 
        break;

    case EQ_NO:
        if (chr_code == 1)
            print_esc("leqno");
        else print_esc("eqno"); 
        break;

    case MATH_COMP:
        switch (chr_code)
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

        case UNDER_NOAD:
            print_esc("underline"); 
            break;

        default:
            print_esc("overline"); 
            break;
        }
        break;

    case LIMIT_SWITCH:
         if (chr_code == LIMITS)
            print_esc("limits");
        else if (chr_code == NO_LIMITS)
            print_esc("nolimits");
        else print_esc("displaylimits"); 
        break;

    case MATH_STYLE:
        print_style(chr_code); 
        break;

    case ABOVE:
        switch (chr_code)
        {
        case OVER_CODE:
            print_esc("over"); 
            break;

        case ATOP_CODE:
            print_esc("atop"); 
            break;

        case DELIMITED_CODE + ABOVE_CODE:
            print_esc("abovewithdelims"); 
            break;

        case DELIMITED_CODE + OVER_CODE:
            print_esc("overwithdelims"); 
            break;

        case DELIMITED_CODE + ATOP_CODE:
            print_esc("atopwithdelims"); 
            break;

        default:
            print_esc("above"); 
            break;
        }
        break;

    case LEFT_RIGHT:
         if (chr_code == LEFT_NOAD)
            print_esc("left");
        else print_esc("right"); 
        break;

    case EXTENSION:
        switch (chr_code) {
        case OPEN_NODE:
            print_esc("openout"); 
            break;

        case WRITE_NODE:
            print_esc("write"); 
            break;

        case CLOSE_NODE:
            print_esc("closeout"); 
            break;

        case SPECIAL_NODE:
            print_esc("special"); 
            break;

        case IMMEDIATE_CODE:
            print_esc("immediate"); 
            break;
        }
        break;
    }
 }

init_cmds ()
{
#ifdef INIT
no_new_control_sequence = FALSE;
primitive("lineskip", ASSIGN_GLUE, GLUE_BASE + LINE_SKIP_CODE); 
primitive("baselineskip", ASSIGN_GLUE, GLUE_BASE + BASELINE_SKIP_CODE); 
primitive("parskip", ASSIGN_GLUE, GLUE_BASE + PAR_SKIP_CODE); 
primitive("abovedisplayskip", ASSIGN_GLUE,
GLUE_BASE + ABOVE_DISPLAY_SKIP_CODE); 
primitive("abovedisplayshortskip", ASSIGN_GLUE,
GLUE_BASE + ABOVE_DISPLAY_SHORT_SKIP_CODE); 
primitive("belowdisplayskip", ASSIGN_GLUE,
GLUE_BASE + BELOW_DISPLAY_SKIP_CODE); 
primitive("belowdisplayshortskip", ASSIGN_GLUE,
GLUE_BASE + BELOW_DISPLAY_SHORT_SKIP_CODE); 
primitive("leftskip", ASSIGN_GLUE, GLUE_BASE + LEFT_SKIP_CODE); 
primitive("rightskip", ASSIGN_GLUE, GLUE_BASE + RIGHT_SKIP_CODE); 
primitive("topskip", ASSIGN_GLUE, GLUE_BASE + TOP_SKIP_CODE); 
primitive("splittopskip", ASSIGN_GLUE, GLUE_BASE + SPLIT_TOP_SKIP_CODE); 
primitive("tabskip", ASSIGN_GLUE, GLUE_BASE + TAB_SKIP_CODE); 
primitive("spaceskip", ASSIGN_GLUE, GLUE_BASE + SPACE_SKIP_CODE); 
primitive("xspaceskip", ASSIGN_GLUE, GLUE_BASE + XSPACE_SKIP_CODE); 
primitive("parfillskip", ASSIGN_GLUE, GLUE_BASE + PAR_FILL_SKIP_CODE); 
primitive("thinmuskip", ASSIGN_MU_GLUE, GLUE_BASE + THIN_MU_SKIP_CODE); 
primitive("medmuskip", ASSIGN_MU_GLUE, GLUE_BASE + MED_MU_SKIP_CODE); 
primitive("thickmuskip", ASSIGN_MU_GLUE, GLUE_BASE + THICK_MU_SKIP_CODE); 
primitive("output", ASSIGN_TOKS, OUTPUT_ROUTINE_LOC);
primitive("everycr", ASSIGN_TOKS, EVERY_CR_LOC);
primitive("everypar", ASSIGN_TOKS, EVERY_PAR_LOC);
primitive("everymath", ASSIGN_TOKS, EVERY_MATH_LOC);
primitive("everydisplay", ASSIGN_TOKS, EVERY_DISPLAY_LOC);
primitive("everyhbox", ASSIGN_TOKS, EVERY_HBOX_LOC);
primitive("everyvbox", ASSIGN_TOKS, EVERY_VBOX_LOC);
primitive("everyjob", ASSIGN_TOKS, EVERY_JOB_LOC);
primitive("errhelp", ASSIGN_TOKS, ERR_HELP_LOC);
primitive("pretolerance", ASSIGN_INT, INT_BASE + PRETOLERANCE_CODE);
primitive("tolerance", ASSIGN_INT, INT_BASE + TOLERANCE_CODE);
primitive("linepenalty", ASSIGN_INT, INT_BASE + LINE_PENALTY_CODE);
primitive("hyphenpenalty", ASSIGN_INT, INT_BASE + HYPHEN_PENALTY_CODE);
primitive("exhyphenpenalty", ASSIGN_INT, INT_BASE + EX_HYPHEN_PENALTY_CODE);
primitive("clubpenalty", ASSIGN_INT, INT_BASE + CLUB_PENALTY_CODE);
primitive("widowpenalty", ASSIGN_INT, INT_BASE + WIDOW_PENALTY_CODE);
primitive("displaywidowpenalty", ASSIGN_INT,
INT_BASE + DISPLAY_WIDOW_PENALTY_CODE);
primitive("brokenpenalty", ASSIGN_INT, INT_BASE + BROKEN_PENALTY_CODE);
primitive("binoppenalty", ASSIGN_INT, INT_BASE + BIN_OP_PENALTY_CODE);
primitive("relpenalty", ASSIGN_INT, INT_BASE + REL_PENALTY_CODE);
primitive("predisplaypenalty", ASSIGN_INT,
INT_BASE + PRE_DISPLAY_PENALTY_CODE);
primitive("postdisplaypenalty", ASSIGN_INT,
INT_BASE + POST_DISPLAY_PENALTY_CODE);
primitive("interlinepenalty", ASSIGN_INT,
INT_BASE + INTER_LINE_PENALTY_CODE);
primitive("doublehyphendemerits", ASSIGN_INT,
INT_BASE + DOUBLE_HYPHEN_DEMERITS_CODE);
primitive("finalhyphendemerits", ASSIGN_INT,
INT_BASE + FINAL_HYPHEN_DEMERITS_CODE);
primitive("adjdemerits", ASSIGN_INT, INT_BASE + ADJ_DEMERITS_CODE);
primitive("mag", ASSIGN_INT, INT_BASE + MAG_CODE);
primitive("delimiterfactor", ASSIGN_INT, INT_BASE + DELIMITER_FACTOR_CODE);
primitive("looseness", ASSIGN_INT, INT_BASE + LOOSENESS_CODE);
primitive("time", ASSIGN_INT, INT_BASE + TIME_CODE);
primitive("day", ASSIGN_INT, INT_BASE + DAY_CODE);
primitive("month", ASSIGN_INT, INT_BASE + MONTH_CODE);
primitive("year", ASSIGN_INT, INT_BASE + YEAR_CODE);
primitive("showboxbreadth", ASSIGN_INT, INT_BASE + SHOW_BOX_BREADTH_CODE);
primitive("showboxdepth", ASSIGN_INT, INT_BASE + SHOW_BOX_DEPTH_CODE);
primitive("hbadness", ASSIGN_INT, INT_BASE + HBADNESS_CODE);
primitive("vbadness", ASSIGN_INT, INT_BASE + VBADNESS_CODE);
primitive("pausing", ASSIGN_INT, INT_BASE + PAUSING_CODE);
primitive("tracingonline", ASSIGN_INT, INT_BASE + TRACING_ONLINE_CODE);
primitive("tracingmacros", ASSIGN_INT, INT_BASE + TRACING_MACROS_CODE);
primitive("tracingstats", ASSIGN_INT, INT_BASE + TRACING_STATS_CODE);
primitive("tracingoutput", ASSIGN_INT, INT_BASE + TRACING_OUTPUT_CODE);
primitive("tracingparagraphs", ASSIGN_INT, INT_BASE + TRACING_PARAGRAPHS_CODE);
primitive("tracingpages", ASSIGN_INT, INT_BASE + TRACING_PAGES_CODE);
primitive("tracinglostchars", ASSIGN_INT, INT_BASE + TRACING_LOST_CHARS_CODE);
primitive("tracingcommands", ASSIGN_INT, INT_BASE + TRACING_COMMANDS_CODE);
primitive("tracingrestores", ASSIGN_INT, INT_BASE + TRACING_RESTORES_CODE);
primitive("uchyph", ASSIGN_INT, INT_BASE + UC_HYPH_CODE);
primitive("outputpenalty", ASSIGN_INT, INT_BASE + OUTPUT_PENALTY_CODE);
primitive("maxdeadcycles", ASSIGN_INT, INT_BASE + MAX_DEAD_CYCLES_CODE);
primitive("floatingpenalty", ASSIGN_INT, INT_BASE + FLOATING_PENALTY_CODE);
primitive("globaldefs", ASSIGN_INT, INT_BASE + GLOBAL_DEFS_CODE);
primitive("fam", ASSIGN_INT, INT_BASE + CUR_FAM_CODE);
primitive("escapechar", ASSIGN_INT, INT_BASE + ESCAPE_CHAR_CODE);
primitive("defaulthyphenchar", ASSIGN_INT, INT_BASE + DEFAULT_HYPHEN_CHAR_CODE);
primitive("defaultskewchar", ASSIGN_INT, INT_BASE + DEFAULT_SKEW_CHAR_CODE);
primitive("endlinechar", ASSIGN_INT, INT_BASE + END_LINE_CHAR_CODE);
primitive("newlinechar", ASSIGN_INT, INT_BASE + NEW_LINE_CHAR_CODE);
primitive("parindent", ASSIGN_DIMEN, DIMEN_BASE + PAR_INDENT_CODE);
primitive("mathsurround", ASSIGN_DIMEN, DIMEN_BASE + MATH_SURROUND_CODE);
primitive("lineskiplimit", ASSIGN_DIMEN, DIMEN_BASE + LINE_SKIP_LIMIT_CODE);
primitive("hsize", ASSIGN_DIMEN, DIMEN_BASE + HSIZE_CODE);
primitive("vsize", ASSIGN_DIMEN, DIMEN_BASE + VSIZE_CODE);
primitive("maxdepth", ASSIGN_DIMEN, DIMEN_BASE + MAX_DEPTH_CODE);
primitive("splitmaxdepth", ASSIGN_DIMEN, DIMEN_BASE + SPLIT_MAX_DEPTH_CODE);
primitive("boxmaxdepth", ASSIGN_DIMEN, DIMEN_BASE + BOX_MAX_DEPTH_CODE);
primitive("hfuzz", ASSIGN_DIMEN, DIMEN_BASE + HFUZZ_CODE);
primitive("vfuzz", ASSIGN_DIMEN, DIMEN_BASE + VFUZZ_CODE);
primitive("delimitershortfall", ASSIGN_DIMEN,
DIMEN_BASE + DELIMITER_SHORTFALL_CODE);
primitive("nulldelimiterspace", ASSIGN_DIMEN,
DIMEN_BASE + NULL_DELIMITER_SPACE_CODE);
primitive("scriptspace", ASSIGN_DIMEN, DIMEN_BASE + SCRIPT_SPACE_CODE);
primitive("predisplaysize", ASSIGN_DIMEN, DIMEN_BASE + PRE_DISPLAY_SIZE_CODE);
primitive("displaywidth", ASSIGN_DIMEN, DIMEN_BASE + DISPLAY_WIDTH_CODE);
primitive("displayindent", ASSIGN_DIMEN, DIMEN_BASE + DISPLAY_INDENT_CODE);
primitive("overfullrule", ASSIGN_DIMEN, DIMEN_BASE + OVERFULL_RULE_CODE);
primitive("hangafter", ASSIGN_INT, INT_BASE + HANG_AFTER_CODE);
primitive("hangindent", ASSIGN_DIMEN, DIMEN_BASE + HANG_INDENT_CODE);
primitive("hoffset", ASSIGN_DIMEN, DIMEN_BASE + H_OFFSET_CODE);
primitive("voffset", ASSIGN_DIMEN, DIMEN_BASE + V_OFFSET_CODE);
primitive(" ", EX_SPACE, 0);
primitive("/", ITAL_CORR, 0);
primitive("accent", ACCENT, 0);
primitive("advance", ADVANCE, 0);
primitive("afterassignment", AFTER_ASSIGNMENT, 0);
primitive("aftergroup", AFTER_GROUP, 0);
primitive("begingroup", BEGIN_GROUP, 0);
primitive("char", CHAR_NUM, 0);
primitive("csname", CS_NAME, 0);
primitive("font", DEF_FONT, 0);
primitive("fontdimen", ASSIGN_FONT_DIMEN, 0);
primitive("nullfont", SET_FONT, null_font);
eqtb[FROZEN_NULL_FONT] = eqtb[cur_val];
font_name[null_font] =
text(FROZEN_NULL_FONT) = text(cur_val);
font_area[null_font] = null_str;
primitive("delimiter", DELIM_NUM, 0);
primitive("divide", DIVIDE, 0);
primitive("endcsname", END_CS_NAME, 0);
primitive("endgroup", END_GROUP, 0);
text(FROZEN_END_GROUP) = make_str_given("endgroup");
eqtb[FROZEN_END_GROUP] = eqtb[cur_val];
primitive("expandafter", EXPAND_AFTER, 0);
primitive("halign", HALIGN, 0);
primitive("hrule", HRULE, 0);
primitive("ignorespaces", IGNORE_SPACES, 0);
primitive("insert", INSERT, 0);
primitive("mark", MARK, 0);
primitive("topmark", TOP_BOT_MARK, TOP_MARK_CODE);
primitive("firstmark", TOP_BOT_MARK, FIRST_MARK_CODE);
primitive("botmark", TOP_BOT_MARK, BOT_MARK_CODE);
primitive("splitfirstmark", TOP_BOT_MARK, SPLIT_FIRST_MARK_CODE);
primitive("splitbotmark", TOP_BOT_MARK, SPLIT_BOT_MARK_CODE);
primitive("mathaccent", MATH_ACCENT, 0);
primitive("mathchar", MATH_CHAR_NUM, 0);
primitive("mathchoice", MATH_CHOICE, 0);
primitive("multiply", MULTIPLY, 0);
primitive("noalign", NO_ALIGN, 0);
primitive("noexpand", NO_EXPAND, 0);
eq_type(FROZEN_DONT_EXPAND) = DONT_EXPAND;
text(FROZEN_DONT_EXPAND) = make_str_given("notexpanded:");
primitive("nonscript", NON_SCRIPT, 0);
primitive("omit", OMIT, 0);
primitive("parshape", SET_SHAPE, 0);
primitive("penalty", BREAK_PENALTY, 0);
primitive("prevgraf", SET_PREV_GRAF, 0);
primitive("radical", RADICAL, 0);
primitive("read", READ_TO_CS, 0);
primitive("relax", RELAX, 256);
text(FROZEN_RELAX) = make_str_given("relax");
eqtb[FROZEN_RELAX] = eqtb[cur_val];
primitive("setbox", SET_BOX, 0);
primitive("the", THE, 0);
primitive("toks", TOKS_REGISTER, 0);
primitive("vadjust", VADJUST, 0);
primitive("valign", VALIGN, 0);
primitive("vcenter", VCENTER, 0);
primitive("vrule", VRULE, 0);
primitive("par", PAR_END, 0);
par_loc = cur_val; par_token = CS_TOKEN_FLAG + par_loc;
primitive("count", REGISTER, INT_VAL);
primitive("dimen", REGISTER, DIMEN_VAL);
primitive("skip", REGISTER, GLUE_VAL);
primitive("muskip", REGISTER, MU_VAL);
primitive("spacefactor", SET_AUX, HMODE);
primitive("prevdepth", SET_AUX, VMODE);
primitive("deadcycles", SET_PAGE_INT, 0);
primitive("insertpenalties", SET_PAGE_INT, 1);
primitive("wd", SET_BOX_DIMEN, WIDTH_OFFSET);
primitive("ht", SET_BOX_DIMEN, HEIGHT_OFFSET);
primitive("dp", SET_BOX_DIMEN, DEPTH_OFFSET);
primitive("pagegoal", SET_PAGE_DIMEN, 0);
primitive("pagetotal", SET_PAGE_DIMEN, 1);
primitive("pagestretch", SET_PAGE_DIMEN, 2);
primitive("pagefilstretch", SET_PAGE_DIMEN, 3);
primitive("pagefillstretch", SET_PAGE_DIMEN, 4);
primitive("pagefilllstretch", SET_PAGE_DIMEN, 5);
primitive("pageshrink", SET_PAGE_DIMEN, 6);
primitive("pagedepth", SET_PAGE_DIMEN, 7);
primitive("lastpenalty", LAST_ITEM, INT_VAL);
primitive("lastkern", LAST_ITEM, DIMEN_VAL);
primitive("lastskip", LAST_ITEM, GLUE_VAL);
primitive("input", INPUT, 0);
primitive("endinput", INPUT, 1);
primitive("number", CONVERT, NUMBER_CODE);
primitive("romannumeral", CONVERT, ROMAN_NUMERAL_CODE);
primitive("string", CONVERT, STRING_CODE);
primitive("meaning", CONVERT, MEANING_CODE);
primitive("fontname", CONVERT, FONT_NAME_CODE);
primitive("jobname", CONVERT, JOB_NAME_CODE);
primitive("if", IF_TEST, IF_CHAR_CODE);
primitive("ifcat", IF_TEST, IF_CAT_CODE);
primitive("ifnum", IF_TEST, IF_INT_CODE);
primitive("ifdim", IF_TEST, IF_DIM_CODE);
primitive("ifodd", IF_TEST, IF_ODD_CODE);
primitive("ifvmode", IF_TEST, IF_VMODE_CODE);
primitive("ifhmode", IF_TEST, IF_HMODE_CODE);
primitive("ifmmode", IF_TEST, IF_MMODE_CODE);
primitive("ifinner", IF_TEST, IF_INNER_CODE);
primitive("ifvoid", IF_TEST, IF_VOID_CODE);
primitive("ifhbox", IF_TEST, IF_HBOX_CODE);
primitive("ifvbox", IF_TEST, IF_VBOX_CODE);
primitive("ifx", IF_TEST, IFX_CODE);
primitive("ifeof", IF_TEST, IF_EOF_CODE);
primitive("iftrue", IF_TEST, IF_TRUE_CODE);
primitive("iffalse", IF_TEST, IF_FALSE_CODE);
primitive("ifcase", IF_TEST, IF_CASE_CODE);
primitive("fi", FI_OR_ELSE, FI_CODE); 
text(FROZEN_FI) = make_str_given("fi");
eqtb[FROZEN_FI] = eqtb[cur_val];
primitive("or", FI_OR_ELSE, OR_CODE);
primitive("else", FI_OR_ELSE, ELSE_CODE);
primitive("hskip", HSKIP, SKIP_CODE);
primitive("hfil", HSKIP, FIL_CODE);
primitive("hfill", HSKIP, FILL_CODE);
primitive("hss", HSKIP, SS_CODE);
primitive("hfilneg",  HSKIP, FIL_NEG_CODE);
primitive("vskip", VSKIP, SKIP_CODE);
primitive("vfil", VSKIP, FIL_CODE);
primitive("vfill", VSKIP, FILL_CODE);
primitive("vss", VSKIP, SS_CODE);
primitive("vfilneg", VSKIP, FIL_NEG_CODE);
primitive("mskip", MSKIP, MSKIP_CODE);
primitive("kern", KERN, EXPLICIT);
primitive("mkern", MKERN, MU_GLUE);
primitive("moveleft", HMOVE, 1);
primitive("moveright", HMOVE, 0);
primitive("raise", VMOVE, 1);
primitive("lower", VMOVE, 0);
primitive("box", MAKE_BOX, BOX_CODE);
primitive("copy", MAKE_BOX, COPY_CODE);
primitive("lastbox", MAKE_BOX, LAST_BOX_CODE);
primitive("vsplit", MAKE_BOX, VSPLIT_CODE);
primitive("vtop", MAKE_BOX, VTOP_CODE);
primitive("vbox", MAKE_BOX, VTOP_CODE + VMODE);
primitive("hbox", MAKE_BOX, VTOP_CODE + HMODE);
primitive("indent", START_PAR, 1);
primitive("noindent", START_PAR, 0);
primitive("shipout", LEADER_SHIP, A_LEADERS - 1);
primitive("leaders", LEADER_SHIP, A_LEADERS);
primitive("cleaders", LEADER_SHIP, C_LEADERS);
primitive("xleaders", LEADER_SHIP, X_LEADERS);
primitive("unpenalty", REMOVE_ITEM, PENALTY_NODE);
primitive("unkern", REMOVE_ITEM, KERN_NODE);
primitive("unskip", REMOVE_ITEM, GLUE_NODE);
primitive("unhbox", UN_HBOX, BOX_CODE);
primitive("unhcopy", UN_HBOX, COPY_CODE);
primitive("unvbox", UN_VBOX, BOX_CODE);
primitive("unvcopy", UN_VBOX, COPY_CODE);
primitive("discretionary", DISCRETIONARY, 0);
primitive("-", DISCRETIONARY, 1);
primitive("eqno", EQ_NO, 0);
primitive("leqno", EQ_NO, 1);
primitive("mathord", MATH_COMP, ORD_NOAD);
primitive("mathop", MATH_COMP, OP_NOAD);
primitive("mathbin", MATH_COMP, BIN_NOAD);
primitive("mathrel", MATH_COMP, REL_NOAD);
primitive("mathopen", MATH_COMP, OPEN_NOAD);
primitive("mathclose", MATH_COMP, CLOSE_NOAD);
primitive("mathpunct", MATH_COMP, PUNCT_NOAD);
primitive("mathinner", MATH_COMP, INNER_NOAD);
primitive("underline", MATH_COMP, UNDER_NOAD);
primitive("overline", MATH_COMP, OVER_NOAD);
primitive("displaylimits", LIMIT_SWITCH, NORMAL);
primitive("limits", LIMIT_SWITCH, LIMITS);
primitive("nolimits", LIMIT_SWITCH, NO_LIMITS);
primitive("displaystyle", MATH_STYLE, DISPLAY_STYLE);
primitive("textstyle", MATH_STYLE, TEXT_STYLE);
primitive("scriptstyle", MATH_STYLE, SCRIPT_STYLE);
primitive("scriptscriptstyle", MATH_STYLE, SCRIPT_SCRIPT_STYLE);
primitive("above", ABOVE, ABOVE_CODE);
primitive("over", ABOVE, OVER_CODE);
primitive("atop", ABOVE, ATOP_CODE);
primitive("abovewithdelims", ABOVE, DELIMITED_CODE + ABOVE_CODE);
primitive("overwithdelims", ABOVE, DELIMITED_CODE + OVER_CODE);
primitive("atopwithdelims", ABOVE, DELIMITED_CODE + ATOP_CODE);
primitive("left", LEFT_RIGHT, LEFT_NOAD);
primitive("right", LEFT_RIGHT, RIGHT_NOAD);
text(FROZEN_RIGHT) = make_str_given("right");
eqtb[FROZEN_RIGHT] = eqtb[cur_val];
primitive("span", TAB_MARK, SPAN_CODE);
primitive("cr", CAR_RET, CR_CODE);
text(FROZEN_CR) = text(cur_val);
eqtb[FROZEN_CR] = eqtb[cur_val];
primitive("crcr", CAR_RET, CR_CR_CODE);
text(FROZEN_END_TEMPLATE) = make_str_given("endtemplate");
text(FROZEN_ENDV) = text(FROZEN_END_TEMPLATE);
eq_type(FROZEN_ENDV) = ENDV;
equiv(FROZEN_ENDV) = null_list;
eq_level(FROZEN_ENDV) = LEVEL_ONE;
eqtb[FROZEN_END_TEMPLATE] = eqtb[FROZEN_ENDV];
eq_type(FROZEN_END_TEMPLATE) = END_TEMPLATE;
primitive("long", PREFIX, 1);
primitive("outer", PREFIX, 2);
primitive("global", PREFIX, 4);
primitive("def", DEF, 0);
primitive("gdef", DEF, 1);
primitive("edef", DEF, 2);
primitive("xdef", DEF, 3);
primitive("let", LET, NORMAL);
primitive("futurelet", LET, NORMAL + 1);
primitive("chardef", SHORTHAND_DEF, CHAR_DEF_CODE);
primitive("mathchardef", SHORTHAND_DEF, MATH_CHAR_DEF_CODE);
primitive("countdef", SHORTHAND_DEF, COUNT_DEF_CODE);
primitive("dimendef", SHORTHAND_DEF, DIMEN_DEF_CODE);
primitive("skipdef", SHORTHAND_DEF, SKIP_DEF_CODE);
primitive("muskipdef", SHORTHAND_DEF, MU_SKIP_DEF_CODE);
primitive("toksdef", SHORTHAND_DEF, TOKS_DEF_CODE);
primitive("catcode", DEF_CODE, CAT_CODE_BASE);
primitive("mathcode", DEF_CODE, MATH_CODE_BASE);
primitive("lccode", DEF_CODE, LC_CODE_BASE);
primitive("uccode", DEF_CODE, UC_CODE_BASE);
primitive("sfcode", DEF_CODE, SF_CODE_BASE);
primitive("delcode", DEF_CODE, DEL_CODE_BASE);
primitive("textfont", DEF_FAMILY, MATH_FONT_BASE); 
primitive("scriptfont", DEF_FAMILY, MATH_FONT_BASE + SCRIPT_SIZE);
primitive("scriptscriptfont", DEF_FAMILY, MATH_FONT_BASE + SCRIPT_SCRIPT_SIZE);
primitive("hyphenation", HYPH_DATA, 0);
primitive("patterns", HYPH_DATA, 1);
primitive("hyphenchar", ASSIGN_FONT_INT, 0);
primitive("skewchar", ASSIGN_FONT_INT, 1);
primitive("batchmode", SET_INTERACTION,  BATCH_MODE);
primitive("nonstopmode", SET_INTERACTION, NONSTOP_MODE);
primitive("scrollmode", SET_INTERACTION, SCROLL_MODE);
primitive("errorstopmode", SET_INTERACTION, ERROR_STOP_MODE);
primitive("closein", IN_STREAM, 0);
primitive("openin", IN_STREAM, 1);
primitive("message", MESSAGE, 0);
primitive("errmessage", MESSAGE, 1);
primitive("lowercase", CASE_SHIFT, LC_CODE_BASE);
primitive("uppercase", CASE_SHIFT, UC_CODE_BASE);
primitive("show", XRAY, SHOW_CODE);
primitive("showbox", XRAY, SHOW_BOX_CODE);
primitive("showthe", XRAY, SHOW_THE_CODE);
primitive("showlists", XRAY, SHOW_LISTS);
primitive("openout", EXTENSION, OPEN_NODE);
primitive("write", EXTENSION, WRITE_NODE);
write_loc = cur_val;
text(END_WRITE) = make_str_given("endwrite");
eq_level(END_WRITE) = LEVEL_ONE;
eq_type(END_WRITE) = OUTER_CALL;
equiv(END_WRITE) = NULL;
primitive("closeout", EXTENSION, CLOSE_NODE);
primitive("special", EXTENSION, SPECIAL_NODE);
primitive("immediate", EXTENSION, IMMEDIATE_CODE);
primitive("end", STOP, 0);
primitive("dump", STOP, 1);
text(FROZEN_PROTECTION) = make_str_given("inaccessible");
no_new_control_sequence = TRUE;
#endif
}
