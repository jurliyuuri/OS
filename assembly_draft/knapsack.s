

.globl	_knapsack 
_knapsack:
  decl %edi
  shll $2, %esi
  pushq %rbx
  movl $201, %r9d
  movl %r9d, %r10d
  shll $2, %r10d # 804
  subq $24, %rsp
  movq $0, 16(%rsp)
  movq %rsp, (%rsp)
  subq %r10, (%rsp)
  subq %r10, %rsp
  subq %r10, %rsp
.L:
  movl $0, (%rsp)
  addq $4, %rsp
  decq %r9
  jnz .L
  addq %r10, %rsp
  movq %rsp, 8(%rsp)
  subq %r10, 8(%rsp)
  subq %r10, 8(%rsp)
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
  cmpl %r10d, %esi
  jl .L12
  movq %r9, %r11
  addq 8(%rsp), %r11
  movl (%r11), %r11d
  movq 16(%rsp), %r10
  addq %rdx, %r10
  movl (%r10), %r10d
  movl %r10d, %r8d
  shll $2, %r8d
  cmpl %r9d, %r8d
  jg .L3

  movl %r9d, %r8d
  shll $2, %r10d
  subl %r10d, %r8d
  movslq %r8d, %r10
  addq 8(%rsp), %r10
  movq (%r10), %r10

  movq 16(%rsp), %r8
  addq %rcx, %r8
  addl (%r8), %r10d

  movq %r9, %r8
  addq (%rsp), %r8
  movl %r10d, (%r8)
  cmpl %r11d, (%r8)
  jge .L4
  movl %r11d, (%r8)
  jmp .L4
.L3:
  movq %r9, %r10
  addq (%rsp), %r10
  movl %r11d, (%r10)
.L4:
  addq $4, %r9
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