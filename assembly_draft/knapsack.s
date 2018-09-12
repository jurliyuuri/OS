

.globl	_knapsack 
_knapsack:
  decl %edi
  shll $2, %esi
  pushq %rbx
  movl $101, %r9d
  subq $24, %rsp
  movq $0, 16(%rsp)
  leaq -808(%rsp), %r10
  leaq -404(%rsp), %r11
  movq %r11, (%rsp)
.L:
  movl $0, (%r10)
  addq $4, %r10
  decq %r9
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
	r11;r11d
	r10;r10d
	r8

loop counters:
	r9;r9d
	16(%rsp)
*/

.L7:
  movl $0, %r9d
.L2:
  movl %r9d, %r10d
  shll $2, %r10d
  cmpl %r10d, %esi
  jl .L12
  movq %r9, %r11
  shlq $2, %r11
  addq 8(%rsp), %r11
  movl (%r11), %r11d
  movq 16(%rsp), %r10
  addq %rdx, %r10
  movl (%r10), %r10d
  cmpl %r9d, %r10d
  jg .L3

  movl %r9d, %r8d
  subl %r10d, %r8d
  movl %r8d, %r10d

  movslq %r10d, %r10
  shlq $2, %r10
  addq 8(%rsp), %r10
  movq (%r10), %r10

  movq 16(%rsp), %r8
  addq %rcx, %r8
  addl (%r8), %r10d

  movq %r9, %r8
  shlq $2, %r8
  addq (%rsp), %r8
  movl %r10d, (%r8)
  movq %r8, %r10

  cmpl %r11d, (%r10)
  jge .L4
  movl %r11d, (%r10)
  jmp .L4
.L3:
  movq %r9, %r10
  shlq $2, %r10
  addq (%rsp), %r10
  movl %r11d, (%r10)
.L4:
  addq $1, %r9
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
  ret