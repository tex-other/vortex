
/*
 * @(#)scan.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

int     scan_left_brace();
int     scan_optional_equals();
bool    scan_keyword();

global  val     cur_val;
global  int     cur_val_level;

int     scan_something_internal();

#define INT_VAL         0
#define DIMEN_VAL       1
#define GLUE_VAL        2
#define MU_VAL          3
#define IDENT_VAL       4
#define TOK_VAL         5

int     scan_eight_bit_int();
int     scan_seven_bit_int();
int     scan_four_bit_int();
int     scan_char_num();
int     scan_fifteen_bit_int();
int     scan_twenty_seven_bit_int();

int     scan_int();
#define INFINITY    017777777777

global  int     radix;

#define PLUS_TOKEN          (OTHER_TOKEN + '+')
#define MINUS_TOKEN         (OTHER_TOKEN + '-')
#define ZERO_TOKEN          (OTHER_TOKEN + '0')
#define A_TOKEN             (LETTER_TOKEN + 'A')
#define OTHER_A_TOKEN       (OTHER_TOKEN + 'A')
#define OCTAL_TOKEN         (OTHER_TOKEN + '\'')
#define HEX_TOKEN           (OTHER_TOKEN + '"')
#define ALPHA_TOKEN         (OTHER_TOKEN + '`')
#define POINT_TOKEN         (OTHER_TOKEN + '.')
#define EURO_POINT_TOKEN    (OTHER_TOKEN + ',')

int     scan_dimen();
#define MAX_DIMEN   07777777777

#define scan_normal_dimen()     scan_dimen(FALSE, FALSE, FALSE)

global  gord    cur_order;
int     scan_glue();

#define scan_optional_space() \
    {get_x_token(); if (cur_cmd != SPACER) back_input();}

#define get_nbx_token() \
    {do get_x_token(); while (cur_cmd == SPACER);}

#define get_nbrx_token() \
    {do get_x_token(); while (cur_cmd == SPACER || cur_cmd == RELAX);}

ptr     scan_rule_spec();
