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

/* Copyright (c) 1992 Regents of the University of California
 * All rights reserved.
 */
/*
 * @(#)io.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

global  int     last;
global  ascii   buffer[];
global  int     first;
global  int     max_buf_stack;

FILE    *a_open_in();
FILE    *a_open_out();

FILE    *b_open_in();
FILE    *b_open_out();

FILE    *w_open_in();
FILE    *w_open_out();

#define a_close(FD)             (fclose(FD))
#define b_close(FD)             (fclose(FD))
#define w_close(FD)             (fclose(FD))

#define prompt_input(S)         {print(S); term_input();}

bool    init_terminal();
int     term_input();
bool    input_ln();

#define term_in                 stdin
#define term_out                stdout
#define t_open_in()
#define t_open_out()
#define update_terminal()       fflush(stdout)
#define clear_terminal()
