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

#ifndef TFM_
#define TFM_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		tfm.h
 */

global	byte_file			tfm_file;
	
global	mword				font_info[];
global	ptr				fmem_ptr;
	
global	fourq				null_character;
			
global	fourq				font_check[];
global	scal				font_size[];
global	scal				font_dsize[];
global	hword				font_params[];
global	str				font_name[];
global	str				font_area[];
global	byte				font_bc[];
global	byte				font_ec[];
global	ptr				font_glue[];
global	bool				font_used[];
global	int				hyphen_char[];
global	int				skew_char[];
		
global	int				char_base[];
global	int				width_base[];
global	int				height_base[];
global	int				depth_base[];
global	int				italic_base[];
global	int				lig_kern_base[];
global	int				kern_base[];
global	int				exten_base[];
global	int				param_base[];
		
global	fnt				font_ptr;

#define	char_exists(CI) \
	((CI).b0 > MIN_QUARTERWORD)

#define	char_info(F, C)	\
	font_info[char_base[F] + (C)].qqqq

#define	char_width(F, CI) \
	font_info[width_base[F] + CI.b0].sc

#define	char_height(F, CB) \
	font_info[height_base[F] + (CB) / 16].sc

#define	char_depth(F, CB) \
	font_info[depth_base[F] + (CB) % 16].sc

#define	char_italic(F, CI) \
	font_info[italic_base[F] + qo(CI.b2) / 4].sc

#define	NO_TAG				0
#define	LIG_TAG				1
#define	LIST_TAG			2
#define	EXT_TAG				3

#define	stop_bit(M)			M.b0
#define	next_char(M)			M.b1
#define	op_bit(M)			M.b2
#define	rem_byte(M)			M.b3
#define	STOP_FLAG			128
#define	KERN_FLAG			128

#define	ext_top(M)			M.b0
#define	ext_mid(M)			M.b1
#define	ext_bot(M)			M.b2
#define	ext_rep(W)			W.b3

#define	char_tag(CI) \
	qo(CI.b2) % 4

#define	height_depth(CI) \
	qo(CI.b1)

#define	lig_kern_start(F, CI) \
	lig_kern_base[F] + rem_byte(CI)

#define	char_kern(F, CI) \
	font_info[kern_base[F] + rem_byte(CI)].sc

#define	SLANT_CODE			1
#define	SPACE_CODE			2
#define	SPACE_STRETCH_CODE		3
#define	SPACE_SHRINK_CODE		4
#define	X_HEIGHT_CODE			5
#define	QUAD_CODE			6
#define	EXTRA_SPACE_CODE		7

#define	param(F, CODE)	\
	font_info[param_base[F] + CODE].sc

#define	slant(F) \
	param(F, SLANT_CODE)

#define	space(F) \
	param(F, SPACE_CODE)

#define	space_stretch(F) \
	param(F, SPACE_STRETCH_CODE)

#define	space_shrink(F) \
	param(F, SPACE_SHRINK_CODE)

#define	x_height(F) \
	param(F, X_HEIGHT_CODE)

#define	quad(F) \
	param(F, QUAD_CODE)

#define	extra_space(F) \
	param(F, EXTRA_SPACE_CODE)

fnt		read_font_info();
int		find_font_dimen();
int		scan_font_ident();
int		char_warning();
ptr		new_character();

#endif
