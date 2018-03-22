
/*
 * @(#)hash.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#define next(H)             hash[H].hh1.lh
#define text(H)             hash[H].hh1.rh
#define font_id_text(H)     text(FONT_ID_BASE + H)

#define hash_is_full        (hash_used == HASH_BASE)

global  hh      hash[];
global  ptr     hash_used;
global  bool    no_new_control_sequence;
global  int     cs_count;

ptr     id_lookup();
int     print_cs();
int     sprint_cs();

#ifdef  INIT
int     primitive();
#endif
