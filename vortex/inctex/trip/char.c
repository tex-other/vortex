
/*
 * @(#)char.c 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#include    "tex.h"

ascii   xord[256];
byte    xchr[256];

init_char ()
{
    int     i;

    xchr[040] = ' ';
    xchr[041] = '!';
    xchr[042] = '"';
    xchr[043] = '#';
    xchr[044] = '$';
    xchr[045] = '%';
    xchr[046] = '&';
    xchr[047] = '\'';
    xchr[050] = '(';
    xchr[051] = ')';
    xchr[052] = '*';
    xchr[053] = '+';
    xchr[054] = ',';
    xchr[055] = '-';
    xchr[056] = '.';
    xchr[057] = '/';
    xchr[060] = '0';
    xchr[061] = '1';
    xchr[062] = '2';
    xchr[063] = '3';
    xchr[064] = '4';
    xchr[065] = '5';
    xchr[066] = '6';
    xchr[067] = '7';
    xchr[070] = '8';
    xchr[071] = '9';
    xchr[072] = ':';
    xchr[073] = ';';
    xchr[074] = '<';
    xchr[075] = '=';
    xchr[076] = '>';
    xchr[077] = '?';
    xchr[0100] = '@';
    xchr[0101] = 'A';
    xchr[0102] = 'B';
    xchr[0103] = 'C';
    xchr[0104] = 'D';
    xchr[0105] = 'E';
    xchr[0106] = 'F';
    xchr[0107] = 'G';
    xchr[0110] = 'H';
    xchr[0111] = 'I';
    xchr[0112] = 'J';
    xchr[0113] = 'K';
    xchr[0114] = 'L';
    xchr[0115] = 'M';
    xchr[0116] = 'N';
    xchr[0117] = 'O';
    xchr[0120] = 'P';
    xchr[0121] = 'Q';
    xchr[0122] = 'R';
    xchr[0123] = 'S';
    xchr[0124] = 'T';
    xchr[0125] = 'U';
    xchr[0126] = 'V';
    xchr[0127] = 'W';
    xchr[0130] = 'X';
    xchr[0131] = 'Y';
    xchr[0132] = 'Z';
    xchr[0133] = '[';
    xchr[0134] = '\\';
    xchr[0135] = ']';
    xchr[0136] = '^';
    xchr[0137] = '_';
    xchr[0140] = '`';
    xchr[0141] = 'a';
    xchr[0142] = 'b';
    xchr[0143] = 'c';
    xchr[0144] = 'd';
    xchr[0145] = 'e';
    xchr[0146] = 'f';
    xchr[0147] = 'g';
    xchr[0150] = 'h';
    xchr[0151] = 'i';
    xchr[0152] = 'j';
    xchr[0153] = 'k';
    xchr[0154] = 'l';
    xchr[0155] = 'm';
    xchr[0156] = 'n';
    xchr[0157] = 'o';
    xchr[0160] = 'p';
    xchr[0161] = 'q';
    xchr[0162] = 'r';
    xchr[0163] = 's';
    xchr[0164] = 't';
    xchr[0165] = 'u';
    xchr[0166] = 'v';
    xchr[0167] = 'w';
    xchr[0170] = 'x';
    xchr[0171] = 'y';
    xchr[0172] = 'z';
    xchr[0173] = '{';
    xchr[0174] = '|';
    xchr[0175] = '}';
    xchr[0176] = '~';
    xchr[0177] = xchr[000] = ' ';
    for (i = 1; i <= 037; incr(i))
        xchr[i] = ' ';
    for (i = 0200; i <= 0377; incr(i))
        xchr[i] = i;
    xchr[FORM_FEED] =  '\l';
    xchr[TAB] = '\t';
    xchr[NUL] = '\0';
    for (i = FIRST_TEXT_CHAR; i <= LAST_TEXT_CHAR; incr(i))
        xord[xchr[i]] = INVALID_CODE;
    for (i = 1; i <= 0176; incr(i))
        xord[xchr[i]] = i;
    for (i = 0200; i <= 0377; incr(i))
        xord[xchr[i]] = i;
}
