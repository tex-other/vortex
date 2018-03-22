
/*
 * @(#)arith.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#define UNITY           0200000
#define TWO             0400000

val     half();
scal    round_decimals();
int     print_scaled();

global  bool    arith_error;
global  scal    remainder;

scal    nx_plus_y();
scal    x_over_n();
scal    xn_over_d();

#define INF_BAD         10000 

hword   badness();
