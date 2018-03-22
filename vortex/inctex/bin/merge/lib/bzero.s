#
# Copyright (c) 1987 University of Maryland Department of Computer Science.
# All rights reserved.  Permission to copy for any purpose is hereby granted
# so long as this copyright notice remains intact.
#

rcsid:	.asciz	"$Header: bzero.s,v 1.2 87/06/16 18:27:36 chris Exp $"

# bzero (addr, count) char *addr; int count;
#
# Zero "count" bytes at address "addr"

	.align	2
	.globl	_bzero
_bzero:
	.word	0
	movl	4(ap),r3		# r3 = addr
	brb	2f
1:
	subl2	r0,8(ap)		# count-=65535 (bytes zeroed this time)
	movc5	$0,(sp),$0,r0,(r3)	# r3 magically points to next 65K
2:
	movzwl	$65535,r0
	cmpl	8(ap),r0		# <= 65535 bytes to zero?
	jgtr	1b			# brif not, zero 65535 and try again
	movc5	$0,(sp),$0,8(ap),(r3)	# zero up to 65535 bytes
	ret
