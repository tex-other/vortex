
/*
 * @(#)io.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
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
