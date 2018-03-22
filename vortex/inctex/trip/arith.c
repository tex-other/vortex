
/*
 * @(#)arith.c 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#include	"tex.h"

#ifdef INCTEX
extern	bool    arith_error;
extern	scal    remainder;
#else
bool    	arith_error;
scal	    	remainder;
#endif

val 
half (x)
    val     x;
{
    return (odd(x) ? (x + 1) / 2 : x / 2);
}


scal
round_decimals (k)
    int     k;
{
    val     a;

    a = 0;
    while (k > 0) {
        decr(k);
        a = (a + dig[k] * TWO) / 10;
    }
    return ((a + 1) / 2);
}

print_scaled (s)
    scal    s;
{
    scal    delta;

    if (s < 0) {
        print_char('-');
        negate(s);
    }
    print_val(s / UNITY);
    print_char('.');
    s = 10 * (s % UNITY) + 5;
    delta = 10;
    do {
        if (delta > UNITY)
            s += 0100000 - (delta / 2);
        print_char('0' + s / UNITY);
        s = 10 * (s % UNITY);
        delta *= 10;
    } while (s > delta);
}

scal
nx_plus_y (n, x, y)
    val     n;
    scal    x;
    scal    y;
{
    if (n < 0) {
        negate(x);
        negate(n);
    }
    if (n == 0)
        return y;
    else if (x <= (07777777777 - y) / n &&
            -x <= (07777777777 + y) / n)
        return (n * x + y);
    else {
        arith_error = TRUE;
        return 0;
    }
}

scal
x_over_n (x, n)
    scal    x;
    val     n;
{
    bool    negative;
    scal    quotient;

    negative = FALSE;
    if (n == 0) {
        arith_error = TRUE;
        remainder = x;
        return 0;
    }
    if (n < 0) {
        negate(x);
        negate(n);
        negative = TRUE;
    }
    if (x >= 0) {
        quotient = x / n;
        remainder = x % n;
    } else {
        quotient = -(-x / n);
        remainder = -(-x % n);
    }
    if (negative)
        negate(remainder);
    return quotient;
}

scal
xn_over_d (x, n, d)
    scal    x;
    val     n;
    val     d;
{
    val     t;
    val     u;
    val     v;
    bool    positive;

    if (x >= 0)
        positive = TRUE;
    else {
        negate(x);
        positive = FALSE;
    }
    t = (x % 0100000) * n;
    u = (x / 0100000) * n + (t / 0100000);
    v = (u % d) * 0100000 + (t % 0100000);
    if (u / d >= 0100000)
        arith_error = TRUE;
    else u = 0100000 * (u / d) + (v / d);
    if (positive) {
        remainder = v % d;
        return u;
    } else {
        remainder = - (v % d);
        return -u;
    }
}

hword
badness (t, s)
    scal    t;
    scal    s;
{
    val     r;

    if (t == 0)
        return 0;
    else if (s <= 0)
        return INF_BAD;
    else {
        if (t <= 7230584)
            r = (t * 297) / s;
        else if (s >= 1663497)
            r = t / (s / 297);
        else r = t;
        if (r > 1290)
            return INF_BAD;
        else return ((r * r * r + 0400000) / 01000000);
    }
}
