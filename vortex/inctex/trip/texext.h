
/*
 * @(#)texext.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
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
