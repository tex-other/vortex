/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
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
 *
 * The above licensing information supersedes all licensing information
 * below.
 */

#ifndef CMDS_
#define CMDS_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		cmds.h
 */

#define	ESCAPE				0
#define	RELAX				0
#define	LEFT_BRACE			1
#define	RIGHT_BRACE			2
#define	MATH_SHIFT			3
#define	TAB_MARK			4
#define	CAR_RET				5
#define	OUT_PARAM			5
#define	MAC_PARAM			6
#define	SUP_MARK			7
#define	SUB_MARK			8
#define	IGNORE				9
#define	ENDV				9
#define	SPACER				10
#define	LETTER				11
#define	OTHER_CHAR			12
#define	ACTIVE_CHAR			13
#define	PAR_END				13
#define	MATCH				13
#define	COMMENT				14
#define	END_MATCH			14
#define	STOP				14
#define	INVALID_CHAR			15
#define	DELIM_NUM			15
#define	MAX_CHAR_CODE			15
#define	CHAR_NUM			16
#define	MATH_CHAR_NUM			17
#define	MARK				18
#define	XRAY				19
#define	MAKE_BOX			20
#define	HMOVE				21
#define	VMOVE				22
#define	UN_HBOX				23
#define	UN_VBOX				24
#define	REMOVE_ITEM			25
#define	HSKIP				26
#define	VSKIP				27
#define	MSKIP				28
#define	KERN				29
#define	MKERN				30
#define	LEADER_SHIP			31
#define	HALIGN				32
#define	VALIGN				33
#define	NO_ALIGN			34
#define	VRULE				35
#define	HRULE				36
#define	INSERT				37
#define	VADJUST				38
#define	IGNORE_SPACES			39
#define	AFTER_ASSIGNMENT		40
#define	AFTER_GROUP			41
#define	BREAK_PENALTY			42
#define	START_PAR			43
#define	ITAL_CORR			44
#define	ACCENT				45
#define	MATH_ACCENT			46
#define	DISCRETIONARY			47
#define	EQ_NO				48
#define	LEFT_RIGHT			49
#define	MATH_COMP			50
#define	LIMIT_SWITCH			51
#define	ABOVE				52
#define	MATH_STYLE			53
#define	MATH_CHOICE			54
#define	NON_SCRIPT			55
#define	VCENTER				56
#define	CASE_SHIFT			57
#define	MESSAGE				58
#define	EXTENSION			59
#define	IN_STREAM			60
#define	BEGIN_GROUP			61
#define	END_GROUP			62
#define	OMIT				63
#define	EX_SPACE			64
#define	RADICAL				65
#define	END_CS_NAME			66
#define	MIN_INTERNAL			67
#define	CHAR_GIVEN			67
#define	MATH_GIVEN			68
#define	LAST_ITEM			69

#define	MAX_NON_PREFIXED_COMMAND	69

#define	TOKS_REGISTER			70
#define	ASSIGN_TOKS			71
#define	ASSIGN_INT			72
#define	ASSIGN_DIMEN			73
#define	ASSIGN_GLUE			74
#define	ASSIGN_MU_GLUE			75
#define	ASSIGN_FONT_DIMEN		76
#define	ASSIGN_FONT_INT			77
#define	SET_AUX				78
#define	SET_PREV_GRAF			79
#define	SET_PAGE_DIMEN			80
#define	SET_PAGE_INT			81
#define	SET_BOX_DIMEN			82
#define	SET_SHAPE			83
#define	DEF_CODE			84
#define	DEF_FAMILY			85
#define	SET_FONT			86
#define	DEF_FONT			87
#define	REGISTER			88
#define	MAX_INTERNAL			88
#define	ADVANCE				89
#define	MULTIPLY			90
#define	DIVIDE				91
#define	PREFIX				92
#define	LET				93
#define	SHORTHAND_DEF			94
#define	READ_TO_CS			95
#define	DEF				96
#define	SET_BOX				97
#define	HYPH_DATA			98
#define	SET_INTERACTION			99
#define	MAX_COMMAND			99

#define	UNDEFINED_CS			(MAX_COMMAND + 1)
#define	EXPAND_AFTER			(MAX_COMMAND + 2)
#define	NO_EXPAND			(MAX_COMMAND + 3)
#define	INPUT				(MAX_COMMAND + 4)
#define	IF_TEST				(MAX_COMMAND + 5)
#define	FI_OR_ELSE			(MAX_COMMAND + 6)
#define	CS_NAME				(MAX_COMMAND + 7)
#define	CONVERT				(MAX_COMMAND + 8)
#define	THE				(MAX_COMMAND + 9)
#define	TOP_BOT_MARK			(MAX_COMMAND + 10)
#define	CALL				(MAX_COMMAND + 11)
#define	LONG_CALL			(MAX_COMMAND + 12)
#define	OUTER_CALL			(MAX_COMMAND + 13)
#define	LONG_OUTER_CALL			(MAX_COMMAND + 14)
#define	END_TEMPLATE			(MAX_COMMAND + 15)
#define	DONT_EXPAND			(MAX_COMMAND + 16)
#define	GLUE_REF			(MAX_COMMAND + 17)
#define	SHAPE_REF			(MAX_COMMAND + 18)
#define	BOX_REF				(MAX_COMMAND + 19)
#define	DATA				(MAX_COMMAND + 20)

int		print_cmd_chr();
int		show_cur_cmd_chr();

#endif
