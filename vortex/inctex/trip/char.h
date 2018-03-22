
/*
 * @(#)char.h 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

global  ascii   xord[];

#define FIRST_ASCII_CODE        0
#define LAST_ASCII_CODE         255

global  byte    xchr[];

#define FIRST_TEXT_CHAR         0
#define LAST_TEXT_CHAR          255

#define TAB                     011
#define FORM_FEED               014
#define CARRIAGE_RETURN         015
#define NULL_CODE               000
#define INVALID_CODE            0177

int init_char();
