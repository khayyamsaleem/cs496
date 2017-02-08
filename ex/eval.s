	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 12
	.globl	_f
	.align	4, 0x90
_f:                                     ## @f
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp0:
	.cfi_def_cfa_offset 16
Ltmp1:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp2:
	.cfi_def_cfa_register %rbp
	movl	$3, %eax
	movq	_g@GOTPCREL(%rip), %rcx
	movl	%edi, -4(%rbp)
	movl	(%rcx), %edi
	addl	$1, %edi
	movl	%edi, (%rcx)
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp3:
	.cfi_def_cfa_offset 16
Ltmp4:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp5:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movl	$2, %edi
	movq	_g@GOTPCREL(%rip), %rax
	movl	$0, -4(%rbp)
	movl	$6, (%rax)
	movl	(%rax), %ecx
	movl	%ecx, -8(%rbp)          ## 4-byte Spill
	callq	_f
	movl	$2, %edi
	movl	%eax, -12(%rbp)         ## 4-byte Spill
	callq	_f
	movl	-12(%rbp), %ecx         ## 4-byte Reload
	addl	%eax, %ecx
	movl	-8(%rbp), %eax          ## 4-byte Reload
	cmpl	%ecx, %eax
	jne	LBB1_2
## BB#1:
	leaq	L_.str(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -16(%rbp)         ## 4-byte Spill
	jmp	LBB1_3
LBB1_2:
	leaq	L_.str.1(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -20(%rbp)         ## 4-byte Spill
LBB1_3:
	movl	-4(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.comm	_g,4,2                  ## @g
	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"Eject!"

L_.str.1:                               ## @.str.1
	.asciz	"Maintain current course"


.subsections_via_symbols
