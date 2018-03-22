
/*
 * @(#)mathlists.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#define ABOVE_CODE      0
#define OVER_CODE       1
#define ATOP_CODE       2
#define DELIMITED_CODE  3

int     init_math();
int     start_eq_no();
int     scan_math();
int     set_math_char();
int     math_limit_switch();
int     scan_delimiter();
int     math_radical();
int     math_ac();
int     append_choices();
int     build_choices();
int     sub_sup();
int     math_fraction();
ptr     fin_mlist();
int     math_left_right();
int     after_math();
int     resume_after_display();
