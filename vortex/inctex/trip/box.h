
/*
 * @(#)box.h 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#define type(M)             mem[M].hh.hh2.b0
#define subtype(M)          mem[M].hh.hh2.b1

#define font                type
#define character           subtype
#define is_char_node(M)     (M >= hi_mem_min)

ptr     new_null_box();

#define HLIST_NODE          0
#define VLIST_NODE          1

#define BOX_NODE_SIZE       7
#define WIDTH_OFFSET        1
#define DEPTH_OFFSET        2
#define HEIGHT_OFFSET       3
#define SHIFT_OFFSET        4
#define LIST_OFFSET         5
#define GLUE_OFFSET         6
#define width(B)            mem[B + WIDTH_OFFSET].sc
#define depth(B)            mem[B + DEPTH_OFFSET].sc
#define height(B)           mem[B + HEIGHT_OFFSET].sc
#define shift_amount(B)     mem[B + SHIFT_OFFSET].sc
#define list_ptr(B)         link(B + LIST_OFFSET)
#define glue_order(B)       subtype(B + LIST_OFFSET)
#define glue_sign(B)        type(B + LIST_OFFSET)
#define glue_set(B)         mem[B + GLUE_OFFSET].gr

#define NORMAL              0
#define STRETCHING          1
#define SHRINKING           2
    
ptr     new_rule();

#define RULE_NODE           2
#define RULE_NODE_SIZE      4
#define NULL_FLAG           -010000000000
#define is_running(R)       (R == NULL_FLAG)

#define INS_NODE            3
#define INS_NODE_SIZE       5
#define float_cost(I)       mem[I + 1].i
#define ins_ptr(I)          info(I + 4)
#define split_top_ptr(I)    link(I + 4)
    
#define MARK_NODE           4
#define SMALL_NODE_SIZE     2
#define mark_ptr(M)         link(M + 1)
    
#define ADJUST_NODE         5
#define adjust_ptr          mark_ptr

ptr     new_ligature();

#define LIGATURE_NODE       6
#define lig_char(L)         L + 1
#define lig_ptr(L)          link(lig_char(L))

ptr     new_disc();

#define DISC_NODE           7
#define replace_count       subtype
#define pre_break           llink
#define post_break          rlink

#define WHATSIT_NODE        8

ptr     new_math();

#define MATH_NODE           9
#define BEFORE              0
#define AFTER               1

#define precedes_break(M)   (type(M) < MATH_NODE)
#define non_discardable(M)  (type(M) < MATH_NODE)

ptr     new_spec();
ptr     new_param_glue();
ptr     new_glue();
ptr     new_skip_param();

#define GLUE_NODE           10
#define COND_MATH_GLUE      98
#define MU_GLUE             99
#define A_LEADERS           100
#define C_LEADERS           101
#define X_LEADERS           102
#define glue_ptr            llink
#define leader_ptr          rlink

#define GLUE_SPEC_SIZE      4
#define glue_ref_count(G)   link(G)
#define stretch(G)          mem[G + 2].sc
#define shrink(G)           mem[G + 3].sc
#define stretch_order       type
#define shrink_order        subtype
#define FIL                 1
#define FILL                2
#define FILLL               3

ptr     new_kern();

#define KERN_NODE           11
#define EXPLICIT            1
#define ACC_KERN            2

ptr     new_penalty();

#define PENALTY_NODE        12
#define INF_PENALTY         10000L
#define EJECT_PENALTY       -INF_PENALTY
#define penalty(P)          mem[P + 1].i

#define UNSET_NODE          13
#define span_count          subtype
#define glue_stretch(U)     mem[U + GLUE_OFFSET].sc
#define glue_shrink         shift_amount

int     print_short_display();
int     print_font_and_char();
int     print_mark();
int     print_rule_dimen();
int     print_glue();
int     print_spec(); 

#define node_list_display(N) \
    {append_char('.'); show_node_list(N); flush_char();}

global  fnt     font_in_short_display;
global  val     depth_threshold;
global  val     breadth_max;

int     show_node_list();
int     show_box();
int     show_info();
int     short_display();

ptr     copy_node_list();
int     flush_node_list();

int     delete_glue_ref();
#define fast_delete_glue_ref(G) \
    {if (glue_ref_count(G) == NULL) \
        free_node(G, GLUE_SPEC_SIZE); \
    else decr(glue_ref_count(G));}
#define add_glue_ref(G) \
    incr(glue_ref_count(G))
