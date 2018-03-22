/* 
 * Copyright (c) 1986-1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/*
 * some macros to read quantities from DVI files.  these macros make a number
 * of assumptions: that the file being read is in 2's complement notation
 * stored sequentially from high byte to low byte, i.e. the first getc reads
 * what is to be the high byte etc; that the target machine stores it's words
 * in two's complement notation.  This assumption is not as dangerous as the
 * first since both vaxen and 68K's use that scheme.  In this context, neither
 * of these assumptions are dangerous:  DVI files are guaranteed to be in 2's
 * complement notation, and most modern architectures use 2's complement
 * notation.
 */
/*
 * note that the 1 2 and 3 byte macro's can be safely called with signed
 * ints.  UNSIGNED_4 must be called with an unsigned int (or cast) for
 * it's result to be interpreted correctly.  Alternatively, (maybe) int =
 * (unsigned) UN_4() might work.
 */
/*
 * First a brief review of 2's complement notation.  Assume a 4 bit machine
 * for all of these examples.  There are 2 required steps to negate a number
 * using 2's complement form (positive numbers are stored in normal binary
 * form) :  1) complement all of the bits in the representation and 2) add 1
 * to that bit pattern.  For example, 2 is represented as
 *							0 0 1 0
 * so to get -2 we complement all of the bits		1 1 0 1
 * and add 1						1 1 1 0
 */
/*
 * the important property of 2's complement notation for reading an n byte
 * quantity into a larger quantity is sign extension.  To put our 4 bit 2 into
 * an 8 bit register requires no twiddling.
 */
/* 2 still ==					0 0 0 0 0 0 1 0
 * lets negate that.  first complement		1 1 1 1 1 1 0 1
 * then add 1					1 1 1 1 1 1 1 0
 */
/*
 * we have extended the sign bit across all of the intervening bits.
 * There are 2 ways I know how to sign extend and I'll describe them
 * both. The first involves shifting and the second addition and xor'ing
 * with magic constants.
 */
/* Shifting method
 */
/* To sign extend, first logical shift left (which fills the least
 * significant bit with 0) and then arithmetic shift right (which
 * propogates the most significant (sign) bit).  Note that C does *not*
 * define whether the >> operator is a logical or an arithmetic right
 * shift!
 */
/* to sign extend 4 bit -2,			0 0 0 0 1 1 1 0
 * first logical shift left by 4 bits		1 1 1 0 0 0 0 0
 * then arithmetic shift right 4 bits		1 1 1 1 1 1 1 0
 */
/* add/xor method
 */
/* The magic constant is all of the bits from the sign bit upward set,
 * the rest reset.  In our case the constant is 0xf8 (1 1 1 1 1 0 0 0) To
 * sign extend, first add the magic constant and then xor with it.  So,
 */
/* to sign extend 4 bit -2			0 0 0 0 1 1 1 0
 * add the constant				1 1 1 1 1 0 0 0
 *						===============
 * result (note the arithmetic overflow)      1 0 0 0 0 0 1 1 0
 * now xor the constant				1 1 1 1 1 0 0 0
 *						===============
 * same result as shifting method.		1 1 1 1 1 1 1 0
 */
/*
 * it works with positive numbers too.  This method assumes that
 * arithmetic overflow does *not* cause an error condition.
*/
/*
 * empirical tests on a SUN 68020 and on a vax 750 (using the -O flag to
 * cc) show the following results (s.e. == sign extended):
 *
 *				SUN 68020		Vax 750
 * 1 byte s.e. to 4 bytes	add/xor 42% faster	add/xor 35% faster
 * 2 bytes s.e. to 4 bytes	add/xor 23% faster	add/xor 30% faster
 * 3 bytes s.e. to 4 bytes	shifts  25% faster	add/xor 28% faster
 * code size			add/xor 4 less instr.	add/xor 1 less instr.
 * registers used		shifts: 3, add/xor 1	shifts: 2, add/xor 1
 */
/*
 * The 68020 results makes sense if one recalls that in the 1 byte case,
 * the shift method must shift by 24 bits, while in the 3 byte case only
 * by 8 bits.  The add/xor method always adds/xors 32 bit ints, so
 * execution time will remain constant across sizes.
 *
 * Clearly the and/xor method is better in all cases except for the 68020
 * 3 byte.  Since speed is the primary concern, the SUN 68020 3 byte
 * macro uses the shift method.
 */

#define	_SHIFT_READ(x, fp)	(x) = ((x) << 8) | getc(fp)
#define _READ(x, fp)		(x) = getc(fp)
/* the magic constants */
#define _MC_1			(~0x7f)
#define _MC_2			(~0x7fff)
#define _MC_3			(~0x7fffff)

#define UNSIGNED_1(x, fp)	_READ((x), (fp))
#define SIGNED_1(x, fp)		_READ((x), (fp)); (x) += (_MC_1); (x) ^= (_MC_1)

#define UNSIGNED_2(x, fp)	_READ((x), (fp)); _SHIFT_READ((x), (fp))
#define SIGNED_2(x, fp)		UNSIGNED_2((x), (fp)); (x) += (_MC_2); (x) ^= (_MC_2)

#define UNSIGNED_3(x, fp)	_READ((x), (fp)); _SHIFT_READ((x), (fp));\
				_SHIFT_READ((x), (fp))
#ifdef sun
# define SIGNED_3(x, fp)	UNSIGNED_3((x), (fp)); (x) = (((x) << 8) >> 8)
#else !sun
# define SIGNED_3(x, fp)	UNSIGNED_3((x), (fp)); (x) += (_MC_3); (x) ^= (_MC_3)
#endif sun

/* since we have 4 byte int's, no sign extension is required... */
#define UNSIGNED_4(x, fp)	_READ((x), (fp)); _SHIFT_READ((x), (fp));\
				_SHIFT_READ((x), (fp)); _SHIFT_READ((x), (fp))
#define SIGNED_4(x, fp)		UNSIGNED_4(x, fp)


/*
 * some macros to get nybbles out of bytes for PK files.
 */
/*
 * put the high nybble (bits 4-7) of src into the low nybble (bits 0-3)
 * of dest.
 */
#define HIGH_NYB(src, dest)	(dest) = (src) >> 4;

/*
 * get the next nybble of the file into the value.  This macro requires
 * that 2 integers named _nyb_buf and _nyb_flag be allocated elsewhere in
 * the program.  In addition, _nyb_flag must be initialized to 0.
 */
#define GET_NYB(x, fp)			\
	if (_nyb_flag == 0) {		\
		_nyb_buf = getc(fp);	\
		(x) = _nyb_buf;		\
		(x) >>= 4;		\
		_nyb_flag = 1;		\
	} else {			\
		(x) = _nyb_buf & 0xf;	\
		_nyb_flag = 0;		\
	}

/*
 * The quantity to be packed into nybbles may require an odd number of
 * nybbles which will cause the nybble fetching macro to get out of
 * sync.  The following macro ``clears'' the state of the nybble fetching
 * routine and should be executed whenever transitioning from nybble to
 * byte (or other) quantities.
 */
#define CLEAR_NYB	_nyb_flag = 0;

/*
 * this macro gets a PK ``packed number'' from fp and puts it into x.  It
 * requires integer parameters _pk_i, _pk_j, and _pk_k and _pk_repeat.
 * It is an adaption of an algorithm presented in Tugboat V. 6, No. 3.
 * Because that algorithm is recursive to one level, this macro expands
 * into RGET_PACKED.
 */

#define GET_PACKED(x, fp)						\
	GET_NYB(_pk_i, (fp));						\
	if (_pk_i == 0) {						\
		/*							\
		 * we have an arbitrarily long number.  scan to		\
		 * find the first non-zero nybble which is the		\
		 * count of the nybbles in this value.			\
		 */							\
		do {							\
			GET_NYB(_pk_j, (fp));				\
			_pk_i++;					\
		} while (_pk_j == 0);					\
		while (_pk_i > 0) {					\
			GET_NYB(_pk_k, fp);				\
			_pk_i--;					\
			_pk_j = (_pk_j << 4) + _pk_k;			\
		}							\
		(x) = _pk_j - 15 + (13 - dyn_f) * 16 + dyn_f;		\
	} else if (_pk_i <= dyn_f) {					\
		/* this nybble is the number we want. */		\
		(x) = _pk_i;						\
	} else if (_pk_i < 14) {					\
		GET_NYB(_pk_j, fp);					\
		(x) = (_pk_i - dyn_f - 1) * 16 + _pk_j + dyn_f + 1;	\
	} else if (_pk_i == 14) {					\
		RGET_PACKED(_pk_repeat, (fp));				\
	} else {							\
		_pk_repeat = 1;						\
	}

#define RGET_PACKED(x, fp)						\
	GET_NYB(_pk_i, (fp));						\
	if (_pk_i == 0) {						\
		/*							\
		 * we have an arbitrarily long number.  scan to		\
		 * find the first non-zero nybble which is the		\
		 * count of the nybbles in this value.			\
		 */							\
		do {							\
			GET_NYB(_pk_j, (fp));				\
			_pk_i++;					\
		} while (_pk_j == 0);					\
		while (_pk_i > 0) {					\
			GET_NYB(_pk_k, fp);				\
			_pk_i--;					\
			_pk_j = (_pk_j << 4) + _pk_k;			\
		}							\
		(x) = _pk_j - 15 + (13 - dyn_f) * 16 + dyn_f;		\
	} else if (_pk_i <= dyn_f) {					\
		/* this nybble is the number we want. */		\
		(x) = _pk_i;						\
	} else if (_pk_i < 14) {					\
		GET_NYB(_pk_j, fp);					\
		(x) = (_pk_i - dyn_f - 1) * 16 + _pk_j + dyn_f + 1;	\
	} else {							\
		/*							\
		 * this guards against an too many ``recursive''	\
		 * calls.						\
		 */							\
		msg(FATAL, "too many repeat counts!");			\
		exit(19);						\
	}
