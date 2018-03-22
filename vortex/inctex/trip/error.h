
/*
 * @(#)error.h 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#define BATCH_MODE              0
#define NONSTOP_MODE            1
#define SCROLL_MODE             2
#define ERROR_STOP_MODE         3

#define wake_up_terminal()

global  int     old_setting;

int     begin_diagnostic();
int     end_diagnostic();

int     print_err();

global  int     interaction;
global  bool    deletions_allowed;
global  int     history;
global  int     error_count;

#ifndef NOHELP 
#define help0() \
    {help_ptr = 0;}

#define help1(h0) \
    {help_ptr = 1; help_line[0] = h0;}

#define help2(h0, h1) \
    {help_ptr = 2; \
    help_line[0] = h0; help_line[1] = h1;}

#define help3(h0, h1, h2) \
    {help_ptr = 3; help_line[0] = h0; \
    help_line[1] = h1; help_line[2] = h2;}

#define help4(h0, h1, h2, h3) \
    {help_ptr = 4; \
    help_line[0] = h0; help_line[1] = h1; \
    help_line[2] = h2; help_line[3] = h3;}

#define help5(h0, h1, h2, h3, h4) \
    {help_ptr = 5; help_line[0] = h0; \
    help_line[1] = h1; help_line[2] = h2; \
    help_line[3] = h3; help_line[4] = h4;}

#define help6(h0, h1, h2, h3, h4, h5) \
    {help_ptr = 6; \
    help_line[0] = h0; help_line[1] = h1; \
    help_line[2] = h2; help_line[3] = h3; \
    help_line[4] = h4; help_line[5] = h5;}
#else
#define help0()
#define help1(h0)
#define help2(h0, h1)
#define help3(h0, h1, h2)
#define help4(h0, h1, h2, h3)
#define help5(h0, h1, h2, h3, h4) 
#define help6(h0, h1, h2, h3, h4, h5)
#endif

global  chrs        help_line[];
global  ptr         help_ptr;
global  bool        use_err_help;

int     jump_out();
int     error();
int     int_error();
int     normalize_selector();

#define succumb() \
    {if (interaction == ERROR_STOP_MODE) \
        interaction = SCROLL_MODE; \
    error(); history = FATAL_ERROR_STOP; jump_out();}

#define SPOTLESS                0
#define WARNING_ISSUED          1
#define ERROR_MESSAGE_ISSUED    2
#define FATAL_ERROR_STOP        3

int     fatal_error();
int     overflow();
int     confusion();

global  int     interrupt;
global  bool    OK_to_interrupt;

#define check_interrupt() \
    {if (interrupt != 0) pause_for_instructions();}

int     pause_for_instructions();
