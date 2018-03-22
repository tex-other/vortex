#
# Copyright (c) 1987 University of Maryland Department of Computer Science.
# All rights reserved.  Permission to copy for any purpose is hereby granted
# so long as this copyright notice remains intact.
#

rcsid:	.asciz	"$Header: bcopy.s,v 1.2 87/06/16 18:27:33 chris Exp $"

# bcopy (from, to, count) char *from, *to; int count;
#
# Copy "count" bytes from "from" to "to"; not guaranteed to
# work if "from" and "to" overlap.

	.align	2
	.globl	_bcopy
_bcopy:
	.word	0
	movl	4(ap),r1		# r1 = from
	movl	8(ap),r3		# r3 = to
	brb	2f
1:
	subl2	r0,12(ap)		# count-=65535 (bytes moved this time)
	movc3	r0,(r1),(r3)		# r1, r3 magically point to next 65K
2:
	movzwl	$65535,r0
	cmpl	12(ap),r0		# <= 65535 bytes to move?
	jgtr	1b			# brif not, move 65535 and try again
	movc3	12(ap),(r1),(r3)	# move up to 65535 bytes
	ret
