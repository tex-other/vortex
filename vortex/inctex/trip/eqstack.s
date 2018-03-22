LL0:
	.data
	.text
|#PROC# 04
	.globl	_new_save_level
_new_save_level:
|#PROLOGUE# 0
	link	a6,#0
	addl	#-LF269,sp
	moveml	#LS269,sp@
|#PROLOGUE# 1
	movw	_save_ptr,d0
	cmpw	_max_save_stack,d0
	jls	L271
	movw	_save_ptr,_max_save_stack
	cmpw	#0x3e2,_max_save_stack
	jls	L272
	.data1
L273:
	.ascii	"save size\0"
	.text
	pea	0x3e8
	pea	L273
	jbsr	_overflow
	addqw	#0x8,sp
L272:
L271:
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack+0x2,a0
	movb	#0x3,a0@(0,d0:l:4)
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack+0x3,a0
	lea	a0@(0,d0:l:4),a0
	movb	_cur_group+0x3,a0@
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack,a0
	movw	_cur_boundary,a0@(0,d0:l:4)
	cmpb	#0xff,_cur_level
	jne	L274
	.data1
L275:
	.ascii	"grouping levels\0"
	.text
	pea	0xff
	pea	L275
	jbsr	_overflow
	addqw	#0x8,sp
L274:
	movw	_save_ptr,_cur_boundary
	movl	a6@(0x8),_cur_group
	addqb	#0x1,_cur_level
	addqw	#0x1,_save_ptr
LE269:
	unlk	a6
	rts
	LF269 = 0
	LS269 = 0x0
	LFF269 = 0
	LSS269 = 0x0
	LP269 =	0x10
	.data
	.text
|#PROC# 04
	.globl	_eq_destroy
_eq_destroy:
|#PROLOGUE# 0
	link	a6,#0
	addl	#-LF276,sp
	moveml	#LS276,sp@
|#PROLOGUE# 1
	moveq	#0,d0
	movb	a6@(0xa),d0
	jra	L279
L280:
L281:
L282:
	moveq	#0,d0
	movw	a6@(0x8),d0
	lea	_tok_mem,a0
	tstw	a0@(0,d0:l:2)
	jne	L283
	movw	a6@(0x8),d0
	cmpw	_premac_lo,d0
	jcs	L2000000
	movw	a6@(0x8),d0
	cmpw	_premac_hi,d0
	jls	L284
L2000000:	moveq	#0,d0
	movw	a6@(0x8),d0
	movl	d0,sp@-
	jbsr	_flush_list
	addqw	#0x4,sp
L284:
	jra	L285
L283:
	moveq	#0,d0
	movw	a6@(0x8),d0
	lea	_tok_mem,a0
	subqw	#0x1,a0@(0,d0:l:2)
L285:
	jra	L278
L286:
	moveq	#0,d0
	movw	a6@(0x8),d0
	movl	d0,sp@-
	jbsr	_delete_glue_ref
	addqw	#0x4,sp
	jra	L278
L288:
	movw	a6@(0x8),a6@(-0x2)
	tstw	a6@(-0x2)
	jeq	L289
	moveq	#0,d0
	movw	a6@(-0x2),d0
	lea	_mem,a0
	movw	a0@(0,d0:l:4),d0
	andl	#0xffff,d0
	moveq	#0,d1
	movw	a6@(-0x2),d1
	lea	_mem,a0
	movw	a0@(0,d1:l:4),d1
	andl	#0xffff,d1
	addl	d1,d0
	addql	#0x1,d0
	movl	d0,sp@-
	moveq	#0,d0
	movw	a6@(-0x2),d0
	movl	d0,sp@-
	jbsr	_free_node
	addqw	#0x8,sp
L289:
	jra	L278
L290:
	moveq	#0,d0
	movw	a6@(0x8),d0
	movl	d0,sp@-
	jbsr	_flush_node_list
	addqw	#0x4,sp
	jra	L278
L279:
	subl	#0x6e,d0
	cmpl	#8,d0
	jhi	L292
	movw	pc@(6,d0:l:2),d0
	jmp	pc@(2,d0:w)
L293:  
	.word	L280-L293
	.word	L281-L293
	.word	L292-L293
	.word	L282-L293
	.word	L292-L293
	.word	L292-L293
	.word	L286-L293
	.word	L288-L293
	.word	L290-L293
L292:
L278:
LE276:
	unlk	a6
	rts
	LF276 = 4
	LS276 = 0x0
	LFF276 = 4
	LSS276 = 0x0
	LP276 =	0x10
	.data
	.text
|#PROC# 04
	.globl	_eq_save
_eq_save:
|#PROLOGUE# 0
	link	a6,#0
	addl	#-LF294,sp
	moveml	#LS294,sp@
|#PROLOGUE# 1
	movw	_save_ptr,d0
	cmpw	_max_save_stack,d0
	jls	L296
	movw	_save_ptr,_max_save_stack
	cmpw	#0x3e2,_max_save_stack
	jls	L297
	.data1
L298:
	.ascii	"save size\0"
	.text
	pea	0x3e8
	pea	L298
	jbsr	_overflow
	addqw	#0x8,sp
L297:
L296:
	tstb	a6@(0xf)
	jne	L299
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack+0x2,a0
	movb	#0x1,a0@(0,d0:l:4)
	jra	L300
L299:
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_eqtb,a0
	lea	a0@(0,d0:l:4),a0
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack,a1
	lea	a1@(0,d0:l:4),a1
	movl	a0@,a1@
	addqw	#0x1,_save_ptr
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack+0x2,a0
	clrb	a0@(0,d0:l:4)
L300:
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack,a0
	movw	a6@(0xa),a0@(0,d0:l:4)
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack+0x3,a0
	movb	a6@(0xf),a0@(0,d0:l:4)
	addqw	#0x1,_save_ptr
LE294:
	unlk	a6
	rts
	LF294 = 0
	LS294 = 0x0
	LFF294 = 0
	LSS294 = 0x0
	LP294 =	0x10
	.data
	.text
|#PROC# 04
	.globl	_eq_define
_eq_define:
|#PROLOGUE# 0
	link	a6,#0
	addl	#-LF301,sp
	moveml	#LS301,sp@
|#PROLOGUE# 1
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_eqtb+0x3,a0
	movb	a0@(0,d0:l:4),d0
	cmpb	_cur_level,d0
	jne	L303
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_eqtb,a0
	movl	a0@(0,d0:l:4),sp@-
	jbsr	_eq_destroy
	addqw	#0x4,sp
	jra	L304
L303:
	cmpb	#0x1,_cur_level
	jls	L305
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_eqtb+0x3,a0
	movb	a0@(0,d0:l:4),d0
	andl	#0xff,d0
	movl	d0,sp@-
	moveq	#0,d0
	movw	a6@(0xa),d0
	movl	d0,sp@-
	jbsr	_eq_save
	addqw	#0x8,sp
L305:
L304:
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_eqtb+0x3,a0
	movb	_cur_level,a0@(0,d0:l:4)
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_eqtb+0x2,a0
	movb	a6@(0xf),a0@(0,d0:l:4)
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_eqtb,a0
	movw	a6@(0x12),a0@(0,d0:l:4)
LE301:
	unlk	a6
	rts
	LF301 = 0
	LS301 = 0x0
	LFF301 = 0
	LSS301 = 0x0
	LP301 =	0x10
	.data
	.text
|#PROC# 04
	.globl	_eq_word_define
_eq_word_define:
|#PROLOGUE# 0
	link	a6,#0
	addl	#-LF306,sp
	moveml	#LS306,sp@
|#PROLOGUE# 1
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_xeq_level+-0x1813,a0
	movb	a0@(0,d0:l),d0
	cmpb	_cur_level,d0
	jeq	L308
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_xeq_level+-0x1813,a0
	movb	a0@(0,d0:l),d0
	andl	#0xff,d0
	movl	d0,sp@-
	moveq	#0,d0
	movw	a6@(0xa),d0
	movl	d0,sp@-
	jbsr	_eq_save
	addqw	#0x8,sp
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_xeq_level+-0x1813,a0
	movb	_cur_level,a0@(0,d0:l)
L308:
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_eqtb,a0
	movl	a6@(0xc),a0@(0,d0:l:4)
LE306:
	unlk	a6
	rts
	LF306 = 0
	LS306 = 0x0
	LFF306 = 0
	LSS306 = 0x0
	LP306 =	0x10
	.data
	.text
|#PROC# 04
	.globl	_geq_define
_geq_define:
|#PROLOGUE# 0
	link	a6,#0
	addl	#-LF309,sp
	moveml	#LS309,sp@
|#PROLOGUE# 1
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_eqtb,a0
	movl	a0@(0,d0:l:4),sp@-
	jbsr	_eq_destroy
	addqw	#0x4,sp
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_eqtb+0x3,a0
	movb	#0x1,a0@(0,d0:l:4)
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_eqtb+0x2,a0
	movb	a6@(0xf),a0@(0,d0:l:4)
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_eqtb,a0
	movw	a6@(0x12),a0@(0,d0:l:4)
LE309:
	unlk	a6
	rts
	LF309 = 0
	LS309 = 0x0
	LFF309 = 0
	LSS309 = 0x0
	LP309 =	0xc
	.data
	.text
|#PROC# 04
	.globl	_geq_word_define
_geq_word_define:
|#PROLOGUE# 0
	link	a6,#0
	addl	#-LF311,sp
	moveml	#LS311,sp@
|#PROLOGUE# 1
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_eqtb,a0
	movl	a6@(0xc),a0@(0,d0:l:4)
	moveq	#0,d0
	movw	a6@(0xa),d0
	lea	_xeq_level+-0x1813,a0
	movb	#0x1,a0@(0,d0:l)
LE311:
	unlk	a6
	rts
	LF311 = 0
	LS311 = 0x0
	LFF311 = 0
	LSS311 = 0x0
	LP311 =	0x8
	.data
	.text
|#PROC# 04
	.globl	_save_for_after
_save_for_after:
|#PROLOGUE# 0
	link	a6,#0
	addl	#-LF314,sp
	moveml	#LS314,sp@
|#PROLOGUE# 1
	movw	_save_ptr,d0
	cmpw	_max_save_stack,d0
	jls	L316
	movw	_save_ptr,_max_save_stack
	cmpw	#0x3e2,_max_save_stack
	jls	L317
	.data1
L318:
	.ascii	"save size\0"
	.text
	pea	0x3e8
	pea	L318
	jbsr	_overflow
	addqw	#0x8,sp
L317:
L316:
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack+0x2,a0
	movb	#0x2,a0@(0,d0:l:4)
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack+0x3,a0
	clrb	a0@(0,d0:l:4)
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack,a0
	movw	a6@(0xa),a0@(0,d0:l:4)
	addqw	#0x1,_save_ptr
LE314:
	unlk	a6
	rts
	LF314 = 0
	LS314 = 0x0
	LFF314 = 0
	LSS314 = 0x0
	LP314 =	0x10
	.data
	.text
|#PROC# 04
	.globl	_unsave
_unsave:
|#PROLOGUE# 0
	link	a6,#0
	addl	#-LF319,sp
	moveml	#LS319,sp@
|#PROLOGUE# 1
	cmpb	#0x1,_cur_level
	jls	L321
	subqb	#0x1,_cur_level
L322:
	subqw	#0x1,_save_ptr
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack+0x2,a0
	cmpb	#0x3,a0@(0,d0:l:4)
	jne	L324
	jra	L323
L324:
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack,a0
	movw	a0@(0,d0:l:4),a6@(-0x4)
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack+0x2,a0
	cmpb	#0x2,a0@(0,d0:l:4)
	jne	L325
	movw	_cur_tok,a6@(-0x6)
	movw	a6@(-0x4),_cur_tok
	jbsr	_back_input
	movw	a6@(-0x6),_cur_tok
	jra	L326
L325:
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack+0x2,a0
	tstb	a0@(0,d0:l:4)
	jne	L327
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack+0x3,a0
	movb	a0@(0,d0:l:4),a6@(-0x1)
	subqw	#0x1,_save_ptr
	jra	L328
L327:
	lea	_eqtb+0x3b14,a0
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack,a1
	lea	a1@(0,d0:l:4),a1
	movl	a0@,a1@
L328:
	cmpw	#0x1813,a6@(-0x4)
	jcc	L329
	moveq	#0,d0
	movw	a6@(-0x4),d0
	lea	_eqtb+0x3,a0
	cmpb	#0x1,a0@(0,d0:l:4)
	jne	L330
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack,a0
	movl	a0@(0,d0:l:4),sp@-
	jbsr	_eq_destroy
	addqw	#0x4,sp
	tstl	_eqtb+0x60e0
	jle	L331
	.data1
L332:
	.ascii	"retaining\0"
	.text
	pea	L332
	moveq	#0,d0
	movw	a6@(-0x4),d0
	movl	d0,sp@-
	jbsr	_restore_trace
	addqw	#0x8,sp
L331:
	jra	L333
L330:
	moveq	#0,d0
	movw	a6@(-0x4),d0
	lea	_eqtb,a0
	movl	a0@(0,d0:l:4),sp@-
	jbsr	_eq_destroy
	addqw	#0x4,sp
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack,a0
	lea	a0@(0,d0:l:4),a0
	moveq	#0,d0
	movw	a6@(-0x4),d0
	lea	_eqtb,a1
	lea	a1@(0,d0:l:4),a1
	movl	a0@,a1@
	tstl	_eqtb+0x60e0
	jle	L334
	.data1
L335:
	.ascii	"restoring\0"
	.text
	pea	L335
	moveq	#0,d0
	movw	a6@(-0x4),d0
	movl	d0,sp@-
	jbsr	_restore_trace
	addqw	#0x8,sp
L334:
L333:
	jra	L336
L329:
	moveq	#0,d0
	movw	a6@(-0x4),d0
	lea	_xeq_level+-0x1813,a0
	cmpb	#0x1,a0@(0,d0:l)
	jeq	L337
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack,a0
	lea	a0@(0,d0:l:4),a0
	moveq	#0,d0
	movw	a6@(-0x4),d0
	lea	_eqtb,a1
	lea	a1@(0,d0:l:4),a1
	movl	a0@,a1@
	moveq	#0,d0
	movw	a6@(-0x4),d0
	lea	_xeq_level+-0x1813,a0
	movb	a6@(-0x1),a0@(0,d0:l)
	tstl	_eqtb+0x60e0
	jle	L338
	.data1
L339:
	.ascii	"restoring\0"
	.text
	pea	L339
	moveq	#0,d0
	movw	a6@(-0x4),d0
	movl	d0,sp@-
	jbsr	_restore_trace
	addqw	#0x8,sp
L338:
	jra	L340
L337:
	tstl	_eqtb+0x60e0
	jle	L341
	.data1
L342:
	.ascii	"retaining\0"
	.text
	pea	L342
	moveq	#0,d0
	movw	a6@(-0x4),d0
	movl	d0,sp@-
	jbsr	_restore_trace
	addqw	#0x8,sp
L341:
L340:
L336:
L326:
	jra	L322
L323:
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack+0x3,a0
	movb	a0@(0,d0:l:4),d0
	andl	#0xff,d0
	movl	d0,_cur_group
	moveq	#0,d0
	movw	_save_ptr,d0
	lea	_save_stack,a0
	movw	a0@(0,d0:l:4),_cur_boundary
	jra	L343
L321:
	.data1
L344:
	.ascii	"curlevel\0"
	.text
	pea	L344
	jbsr	_confusion
	addqw	#0x4,sp
L343:
LE319:
	unlk	a6
	rts
	LF319 = 8
	LS319 = 0x0
	LFF319 = 8
	LSS319 = 0x0
	LP319 =	0x10
	.data
	.text
|#PROC# 04
	.globl	_off_save
_off_save:
|#PROLOGUE# 0
	link	a6,#0
	addl	#-LF346,sp
	moveml	#LS346,sp@
|#PROLOGUE# 1
	tstl	_cur_group
	jne	L348
	.data1
L349:
	.ascii	"Extra \0"
	.text
	pea	L349
	jbsr	_print_err
	addqw	#0x4,sp
	moveq	#0,d0
	movw	_cur_chr,d0
	movl	d0,sp@-
	moveq	#0,d0
	movw	_cur_cmd,d0
	movl	d0,sp@-
	jbsr	_print_cmd_chr
	addqw	#0x8,sp
	jbsr	_help_offsave_xtra
	jbsr	_error
	jra	L351
L348:
	jbsr	_back_input
	jbsr	_new_token
	movw	d0,a6@(-0x2)
	movw	a6@(-0x2),_tok_link+0xea60
	.data1
L352:
	.ascii	"Missing \0"
	.text
	pea	L352
	jbsr	_print_err
	addqw	#0x4,sp
	movl	_cur_group,d0
	jra	L354
L355:
	moveq	#0,d0
	movw	a6@(-0x2),d0
	lea	_tok_mem,a0
	movw	#0x1dbc,a0@(0,d0:l:2)
	.data1
L356:
	.ascii	"groupend\0"
	.text
	pea	L356
	jbsr	_print_esc
	addqw	#0x4,sp
	jra	L353
L357:
	moveq	#0,d0
	movw	a6@(-0x2),d0
	lea	_tok_mem,a0
	movw	#0x324,a0@(0,d0:l:2)
	pea	0x24
	jbsr	_print_char
	addqw	#0x4,sp
	jra	L353
L358:
	moveq	#0,d0
	movw	a6@(-0x2),d0
	lea	_tok_mem,a0
	movw	#0x1dbd,a0@(0,d0:l:2)
	jbsr	_new_token
	moveq	#0,d1
	movw	a6@(-0x2),d1
	lea	_tok_link,a0
	movw	d0,a0@(0,d1:l:2)
	moveq	#0,d0
	movw	a6@(-0x2),d0
	lea	_tok_link,a0
	movw	a0@(0,d0:l:2),a6@(-0x2)
	moveq	#0,d0
	movw	a6@(-0x2),d0
	lea	_tok_mem,a0
	movw	#0xc2e,a0@(0,d0:l:2)
	.data1
L359:
	.ascii	"right.\0"
	.text
	pea	L359
	jbsr	_print_esc
	addqw	#0x4,sp
	jra	L353
L360:
	moveq	#0,d0
	movw	a6@(-0x2),d0
	lea	_tok_mem,a0
	movw	#0x27d,a0@(0,d0:l:2)
	pea	0x7d
	jbsr	_print_char
	addqw	#0x4,sp
	jra	L353
L354:
	cmpl	#0xe,d0
	jeq	L355
	cmpl	#0xf,d0
	jeq	L357
	cmpl	#0x10,d0
	jeq	L358
	jra	L360
L353:
	.data1
L361:
	.ascii	" inserted\0"
	.text
	pea	L361
	jbsr	_print
	addqw	#0x4,sp
	pea	0x4
	moveq	#0,d0
	movw	_tok_link+0xea60,d0
	movl	d0,sp@-
	jbsr	_begin_token_list
	addqw	#0x8,sp
	jbsr	_help_offsave_missing
	jbsr	_error
L351:
LE346:
	unlk	a6
	rts
	LF346 = 4
	LS346 = 0x0
	LFF346 = 4
	LSS346 = 0x0
	LP346 =	0x10
	.data
	.text
|#PROC# 04
	.globl	_restore_trace
_restore_trace:
|#PROLOGUE# 0
	link	a6,#0
	addl	#-LF363,sp
	moveml	#LS363,sp@
|#PROLOGUE# 1
	jbsr	_begin_diagnostic
	pea	0x7b
	jbsr	_print_char
	addqw	#0x4,sp
	movl	a6@(0xc),sp@-
	jbsr	_print
	addqw	#0x4,sp
	pea	0x20
	jbsr	_print_char
	addqw	#0x4,sp
	moveq	#0,d0
	movw	a6@(0xa),d0
	movl	d0,sp@-
	jbsr	_show_eqtb
	addqw	#0x4,sp
	pea	0x7d
	jbsr	_print_char
	addqw	#0x4,sp
	pea	0
	jbsr	_end_diagnostic
	addqw	#0x4,sp
LE363:
	unlk	a6
	rts
	LF363 = 0
	LS363 = 0x0
	LFF363 = 0
	LSS363 = 0x0
	LP363 =	0xc
	.data
	.text
|#PROC# 04
	.globl	_help_offsave_xtra
_help_offsave_xtra:
|#PROLOGUE# 0
	link	a6,#0
	addl	#-LF365,sp
	moveml	#LS365,sp@
|#PROLOGUE# 1
	movw	#0x1,_help_ptr
	.data1
L367:
	.ascii	"Things are pretty mixed up, but I think the worst is over.\0"
	.text
	movl	#L367,_help_line
LE365:
	unlk	a6
	rts
	LF365 = 0
	LS365 = 0x0
	LFF365 = 0
	LSS365 = 0x0
	LP365 =	0x8
	.data
	.text
|#PROC# 04
	.globl	_help_offsave_missing
_help_offsave_missing:
|#PROLOGUE# 0
	link	a6,#0
	addl	#-LF368,sp
	moveml	#LS368,sp@
|#PROLOGUE# 1
	movw	#0x5,_help_ptr
	.data1
L370:
	.ascii	"I've inserted something that you may have forgotten.\0"
	.text
	movl	#L370,_help_line
	.data1
L371:
	.ascii	"(See the <inserted text> above.)\0"
	.text
	movl	#L371,_help_line+0x4
	.data1
L372:
	.ascii	"With luck, this will get me unwedged. But if you\0"
	.text
	movl	#L372,_help_line+0x8
	.data1
L373:
	.ascii	"really didn't forget anything, try typing `2' now\73 then\0"
	.text
	movl	#L373,_help_line+0xc
	.data1
L374:
	.ascii	"my insertion and my current dilemma will both disappear.\0"
	.text
	movl	#L374,_help_line+0x10
LE368:
	unlk	a6
	rts
	LF368 = 0
	LS368 = 0x0
	LFF368 = 0
	LSS368 = 0x0
	LP368 =	0x8
	.data
