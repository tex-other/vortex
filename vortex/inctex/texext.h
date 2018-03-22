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
 * @(#)texext.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */


#define OPEN_NODE           0
#define open_name(M)        link(M + 1)
#define open_area(M)        info(M + 2)
#define open_ext(M)         link(M + 2)
#define OPEN_NODE_SIZE      3

#define WRITE_NODE          1
#define write_tokens(M)     link(M + 1)
#define write_stream(M)     info(M + 1)
#define WRITE_NODE_SIZE     2

#define CLOSE_NODE          2
#define SPECIAL_NODE        3

#define IMMEDIATE_CODE      4
#define END_WRITE_TOKEN     CS_TOKEN_FLAG + END_WRITE

global  alpha_file  write_file[];

global  bool    write_open[];
global  ptr     write_loc;

int     do_extension();

int     new_whatsit();
int     show_whatsit();
int     free_whatsit();
ptr     copy_whatsit();
int     out_whatsit();
int     new_write();
int     out_write();
int     print_write();
int     out_special();
