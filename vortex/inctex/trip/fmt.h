
/*
 * @(#)fmt.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

global  word_file   fmt_file;

global  str format_ident;

bool    load_fmt_file();

#ifdef INIT
int     store_fmt_file();
#endif
