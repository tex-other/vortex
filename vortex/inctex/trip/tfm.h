
/*
 * @(#)tfm.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

global  byte_file       tfm_file;

global  mword   font_info[];
global  ptr     fmem_ptr;

global  qqqq    null_character;

global  qqqq    font_check[];
global  scal    font_size[];
global  scal    font_dsize[];
global  hword   font_params[];
global  str     font_name[];
global  str     font_area[];
global  byte    font_bc[];
global  byte    font_ec[];
global  ptr     font_glue[];
global  bool    font_used[];
global  int     hyphen_char[];
global  int     skew_char[];

global  ptr     char_base[];
global  ptr     width_base[];
global  ptr     height_base[];
global  ptr     depth_base[];
global  ptr     italic_base[];
global  ptr     lig_kern_base[];
global  ptr     kern_base[];
global  ptr     exten_base[];
global  ptr     param_base[];


global  fnt     font_ptr;

#define null_font   FONT_BASE

#define char_exists(CI) \
    ((CI).b0 > MIN_QUARTERWORD)

#define char_info(F, C) \
    font_info[char_base[F] + (C)].qqqq

#define char_width(F, CI) \
    font_info[width_base[F] + CI.b0].sc

#define char_height(F, CB) \
    font_info[height_base[F] + (CB) / 16].sc

#define char_depth(F, CB) \
    font_info[depth_base[F] + (CB) % 16].sc

#define char_italic(F, CI) \
    font_info[italic_base[F] + qo(CI.b2) / 4].sc

#define NO_TAG              0
#define LIG_TAG             1
#define LIST_TAG            2
#define EXT_TAG             3

#define stop_bit(M)         M.b0
#define next_char(M)        M.b1
#define op_bit(M)           M.b2
#define rem_byte(M)         M.b3
#define STOP_FLAG           128
#define KERN_FLAG           128

#define ext_top(M)          M.b0
#define ext_mid(M)          M.b1
#define ext_bot(M)          M.b2
#define ext_rep(W)          W.b3

#define char_tag(CI) \
    qo(CI.b2) % 4

#define height_depth(CI) \
    qo(CI.b1)

#define lig_kern_start(F, CI) \
    lig_kern_base[F] + rem_byte(CI)

#define char_kern(F, CI) \
    font_info[kern_base[F] + rem_byte(CI)].sc

#define SLANT_CODE          1
#define SPACE_CODE          2
#define SPACE_STRETCH_CODE  3
#define SPACE_SHRINK_CODE   4
#define X_HEIGHT_CODE       5
#define QUAD_CODE           6
#define EXTRA_SPACE_CODE    7

#define param(F, CODE)  \
    font_info[param_base[F] + CODE].sc

#define slant(F) \
    param(F, SLANT_CODE)

#define space(F) \
    param(F, SPACE_CODE)

#define space_stretch(F) \
    param(F, SPACE_STRETCH_CODE)

#define space_shrink(F) \
    param(F, SPACE_SHRINK_CODE)

#define x_height(F) \
    param(F, X_HEIGHT_CODE)

#define quad(F) \
    param(F, QUAD_CODE)

#define extra_space(F) \
    param(F, EXTRA_SPACE_CODE)

fnt     read_font_info();
int     find_font_dimen();
int     scan_font_ident();
int     char_warning();
ptr     new_character();
