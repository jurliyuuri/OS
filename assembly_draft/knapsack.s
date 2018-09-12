

.globl	_knapsack 
_knapsack:
  pushq %rbp
  decl %edi
  shll $2, %esi
  pushq %rbx
  movl $101, %eax
  subq $24, %rsp
  movq $0, 16(%rsp)
  leaq -808(%rsp), %r10
  leaq -404(%rsp), %rbp
  movq %rbp, (%rsp)
.L:
  movl $0, (%r10)
  addq $4, %r10
  decq %rax
  jnz .L
  leaq -808(%rsp), %r10
  movq %r10, 8(%rsp)
/*

infos:
	rdi : n - 1
	rsi : W * 4
	rdx : weight
	rcx : value

buffers:
	(%rsp)
	8(%rsp)

tmp:
	rbp;ebp
	r10;r10d
	rax;eax

*/

.L7:
  movl $0, %eax
.L2:
  movl %eax, %r10d
  shll $2, %r10d
  cmpl %r10d, %esi
  jl .L12
  movq %rax, %rbp
  shlq $2, %rbp
  addq 8(%rsp), %rbp
  movl (%rbp), %ebp
  movq 16(%rsp), %r10
  addq %rdx, %r10
  movl (%r10), %r10d
  cmpl %eax, %r10d
  jg .L3

// %r10d = %eax - %r10d
  negl %r10d
  addl %eax, %r10d

  movslq %r10d, %r10
  shlq $2, %r10
  addq 8(%rsp), %r10
  movq (%r10), %r10

  addq 16(%rsp),%rcx
  addl (%rcx), %r10d
  subq 16(%rsp),%rcx

  shlq $2, %rax
  addq (%rsp), %rax
  movl %r10d, (%rax)
  movq %rax, %r10
  subq (%rsp), %rax
  shrq $2, %rax

  cmpl %ebp, (%r10)
  jge .L4
  movl %ebp, (%r10)
  jmp .L4
.L3:
  movq %rax, %r10
  shlq $2, %r10
  addq (%rsp), %r10
  movl %ebp, (%r10)
.L4:
  incq %rax
  jmp .L2
.L12:
  movl %edi, %r10d
  shll $2, %r10d
  cmpl 16(%rsp), %r10d
  je .L9

/* cannot xchg two locations in x86 */
  movq 8(%rsp), %rbx
  xchg %rbx, (%rsp)
  movq %rbx, 8(%rsp)

  addq $4, 16(%rsp)
  jmp .L7
.L9:
  movslq %esi, %rsi
  movq (%rsp), %rax
  addq %rsi, %rax
  movl (%rax), %eax
  addq $24, %rsp
  popq %rbx
  popq %rbp
  ret