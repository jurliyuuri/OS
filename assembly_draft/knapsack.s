

.globl	_knapsack 
_knapsack:
  pushq %rbp
  decl %edi
  shll $2, %esi
  movq %rcx, %r11
  pushq %rbx
  movl $101, %eax
  movl $0, %r9d
  subq $16, %rsp
  leaq -808(%rsp), %r10
  leaq -404(%rsp), %r8
.L:
  movl $0, (%r10)
  addq $4, %r10
  decq %rax
  jnz .L
  leaq -808(%rsp), %rbx
/*

infos:
	rdi : n - 1
	rsi : W * 4
	rdx : weight
	r11 : value

buffers:
	rbx
	r8

rcx;ecx
rbp;ebp
r10;r10d
rax;eax
r9 ;r9d

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
  movl (%rbx,%rbp), %ebp
  cmpl %eax, (%r9,%rdx)
  jg .L3
  movl %eax, %r10d
  subl (%r9,%rdx), %r10d
  movslq %r10d, %r10
  shlq $2, %r10
  movl (%rbx,%r10), %ecx

  movq %rax, %r10
  shlq $2, %r10

  addl (%r9,%r11), %ecx
  movl %ecx, (%r8,%r10)
/* if ( (%r8,%r10) < ebp) { (%r8,%r10) = ebp; } */
  cmpl %ebp, (%r8,%r10)
  jge .L4
  movl %ebp, (%r8,%r10)
  jmp .L4
.L3:
  movq %rax, %r10
  shlq $2, %r10
  movl %ebp, (%r8,%r10)
.L4:
  incq %rax
  jmp .L2
.L12:
  movl %edi, %r10d
  shll $2, %r10d
  cmpl %r9d, %r10d
  je .L9
  xchg %rbx, %r8
  addq $4, %r9
  jmp .L7
.L9:
  movslq %esi, %rsi
  movl (%rsi,%r8), %eax
  addq $16, %rsp
  popq %rbx
  popq %rbp
  ret