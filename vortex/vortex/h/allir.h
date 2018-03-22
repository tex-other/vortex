/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
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
 *
 * The above licensing information supersedes all licensing information
 * below.
 */

#ifndef _ALLIR_
#define _ALLIR_

/*
 *  IRf is organized as an ordinary binary tree.
 *  IRs is organized as a doubly-linked list.  Empty file is one that has
 *  a single node of struct _char containing EOF
 */

typedef struct _file {
	char		*fn;		/* filename */
	unsigned long	id;		/* unique file identifier */
	struct _file	*lt;		/* left sibling */
	struct _file	*rt;		/* right sibling */
	struct _char	*hd;		/* head of IRs */
	struct _char	*pt;		/* current update point within IRs */
} _File;


/*
 *  Node types, these all have their own structures above, among
 *  the interior nodes of the IRi or the text nodes for character
 *  leaves.  These numbers must all be distinct, of course, as they
 *  are the only way of telling what (size) structure makes up a
 *  particular node.
 */

#define NODE_NONE	0		/* unknown node */

#define NODE_CHAR	10		/* a character leaf */

#define NODE_LIG	20		/* a ligature node */
#define NODE_WORD	21		/* an word delimiting node */
#define NODE_PAR	22		/* an paragraph, as with \par */

#define NODE_DEF	30		/* a macro def node */
#define NODE_FDEF	31		/* a font def  node */
#define NODE_CSEQ	32		/* a macro invocation */
#define NODE_FONT	33		/* a font invocation */
#define NODE_RULE	34		/* a rule node */
#define NODE_SYMBOL	35		/* a TeX symbol */
#define NODE_SPECIAL	36		/* \special node */

#define NODE_MATH	40		/* a math shift node (group) */
#define NODE_DISPLAY	41		/* a display math node */

#define NODE_GROUP	50		/* group node, as with {} */

#define NODE_INPUT	60		/* an \input node */

#define NODE_SPACE	70		/* a space/comment node */

/*
 *  One of these structure is used for all nodes in the IRi that
 *  are not leaf nodes.  Leaf nodes (letters) are really text
 *  structures in the IRs (not the same as the interior nodes).
 *  All these structures have a first byte which tells their
 *  type so that one may guarantee being able to tell the type
 *  by examining the first byte.  Note that each different node
 *  type has a different (sized!) structure, but all references
 *  are through the (pointer) type, so this is somewhat less
 *  of a problem than it might otherwise be.
 *
 *  The tree is stored as a sort of directed graph.  Since
 *  nodes may have arbitrary numbers of children, we keep
 *  a pointer to the linked list of children, although each
 *  child has a direct ``up'' pointer to the parent.  The
 *  list of children is doubly-linked so that one may reach
 *  any sibling quickly.  Some nodes, however, don't use
 *  this scheme.  Nodes that sit directly on the IRs (don't
 *  have interior nodes for children) have begin and end
 *  pointers to the IRs directly.  An example of this is the
 *  (struct _word) for the word node type.
 *
 *  The (struct _node) is the basis for all interior tree
 *  nodes, they are all the same in the fields that appear
 *  in the (struct _node); type dependent information
 *  appears later on.  All the other node types contain a
 *  (struct _node) so that changes will be less likely to
 *  mess something up, but to avoid extra work, all the
 *  fields are defined so as to make them appear to be
 *  in the top level structure.
 */

typedef struct _node {
	char		nd_type;		/* type of node in tree */
	char		nd_char;		/* char in ACII */
	struct _node	*nd_up;       		/* parent of this node */
	struct _node	*nd_lt;			/* left (prevous) child */
	struct _node	*nd_rt;			/* right (next) child */
} _Node;

#define	_ty		_com.nd_type
#define	_ch		_com.nd_char
#define	_up		_com.nd_up
#define	_lt		_com.nd_lt
#define	_rt		_com.nd_rt
#define	_lc		_com.nd_up
#define	_pb		nd_up->nd_up->nd_up
#define	_qb		nd_up->nd_up
#define	_wb		nd_up
#define	_rb		nd_rt

typedef struct _char {
	struct _node	_com;			/* common node info. */
	struct _node	*_re;			/* box reference in IRt */
	unsigned long	_id;			/* unique identifier */
} _Char;

typedef struct _unode {
	struct _node	_com;			/* common node info. */
	struct _node	*_re;			/* box reference in IRt */
	struct _node   	*_dn;       		/* beginning in IRs */ 
} _Unode;

typedef struct _group {
	struct _node	_com;			/* common node info. */
	struct _node	*_dn;			/* list of children */
	short		_level;			/* nesting level */
} _Group;

typedef struct _cseq {
	struct _node	_com;			/* common node info. */
	struct _node	*_re;			/* crodd reference to IRt */
	struct _node	*_bon;			/* beginning of cs name */
	struct _node	*_eon;			/* end of cs name */
	struct _node	*_boc;			/* beginning of char params */
	struct _node	*_eoc;			/* end of char params */
	struct _node	*_def;			/* defined IRi */
} _Cseq;
	
typedef struct _math {
	struct _node	_com;			/* common node info. */
	struct _node	*_re;			/* box reference in IRt */
	struct _node	*_dn;			/* list of children */
} _Math;
	
typedef struct _input {
	struct _node	_com;			/* common node info. */
	struct _node	*_dn;			/* list of children */
	struct _node	*_bon;			/* name of file input from */
	struct _file	*_fp;			/* pointer to IRf */
} _Input;
	
typedef struct _space {
	struct _node	_com;			/* common node info. */
	struct _node	*_boc;			/* beginning in IRs */
	struct _node	*_eoc;			/* end in IRs */
} _Space;


/*
 * Box structure
 */

#define BOX_NONE	0	/* NULL */
#define BOX_CHAR	67	/* C: box of a letter matching a IRs node */
#define BOX_LIG		76	/* L: box of a ligature matching a IRi node */
#define BOX_HYPH	72	/* H: box of `-' introduced by hyphenation */
#define BOX_MACRO	77	/* M: expanded text by invocation of a macro */
#define BOX_EXP		69	/* E: expanded text, no direct incocation */
#define BOX_RULE	82	/* R: box of rule */
#define BOX_SPECIAL	83	/* S: box of special */
#define BOX_WORD	87	/* W: box of single word in IRt */
#define BOX_PAR		81	/* Q: box of a paragraph in IRt */
#define BOX_PAGE	80	/* P: box of whole page in IRt */

typedef struct _cbox {			/* terminal char box */
	struct _node	_com;		/* common node info */
	struct _node	*_re;		/* pointer to IRs+IRi */
	unsigned long	_id;		/* box id */
	long		_xb;		/* baseline x coordinate of box */
	long		_yb;		/* baseline y coordinate of box */
	unsigned short	_ft;		/* current font number */
} _Cbox;

typedef struct _sbox {			/* terminal char box */
	struct _node	_com;		/* common node info */
	struct _node	*_re;		/* pointer to IRs+IRi */
	unsigned long	_id;		/* box id */
	long		_xb;		/* baseline x coordinate of box */
	long		_yb;		/* baseline y coordinate of box */
	unsigned short	_ta;		/* total no of chars in arg */
	char		*_ap;		/* point to argument list */
} _Sbox;

typedef struct _rbox {			/* nonterminal intermediate box */
	struct _node	_com;		/* common node info */
	struct _node	*_re;		/* pointer to IRs+IRi */
	unsigned long	_id;		/* box id */
	long		_xc;		/* upper-left cornor x coordinate */
	long		_yc;		/* upper-left cornor y coordinate */
	long		_wd;		/* horizontal width of word box */
	long		_ht;		/* vertical height+depth of word box */
} _Rbox;

typedef struct _ubox {			/* nonterminal intermediate box */
	struct _node	_com;		/* common node info */
	struct _node	*_re;		/* pointer to IRs+IRi */
	struct _node	*_dn;		/* down pointer */
	unsigned long	_id;		/* box id */
	long		_xc;		/* upper-left cornor x coordinate */
	long		_yc;		/* upper-left cornor y coordinate */
	long		_wd;		/* horizontal width of word box */
	long		_ht;		/* vertical height+depth of word box */
} _Ubox;

#define FT_MAX		64

typedef struct _pbox {			/* nonterminal intermediate box */
	struct _node	_com;		/* common node info */
	struct _node	*_ec;		/* pointer to extended context */
	struct _node	*_dn;		/* down pointer */
	unsigned long	_id;		/* context id */
	long		_xc;		/* upper-left cornor x coordinate */
	long		_yc;		/* upper-left cornor y coordinate */
	long		_wd;		/* horizontal width of page box */
	long		_ht;		/* vertical height+depth of page box */
	long		_ok;		/* TRUE if already sent */
	unsigned short	_no;		/* physical page number */
	unsigned short	_tb;		/* total number of boxes */
	unsigned short	_tf;		/* total number of fonts used */
	unsigned short	_tn;		/* total no. of letters in font names*/
	unsigned short	_ts;		/* total no. of \special's*/
	unsigned short	_ta;		/* total no. of chars in \special arg*/
	long		_ct[10];	/* TeX's 10 count registers */
	unsigned short	_ft[FT_MAX];	/* used fonts */
} _Pbox;

#endif !_ALLIR_

