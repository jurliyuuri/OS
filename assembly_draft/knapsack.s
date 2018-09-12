/*

rdi : n
rsi : W
rdx : weight
rcx : value

*/

	.globl	_knapsack 
_knapsack:
  pushq %rbp
  movl %edi, %r10d
  movq %rcx, %r11
  xorl %eax, %eax
  pushq %rbx
  movl $101, %ecx
  xorl %r9d, %r9d
  subq $696, %rsp
  leaq -112(%rsp), %rdi
  leaq 292(%rsp), %r8
  rep stosl
  leaq -112(%rsp), %rbx
  leal -1(%r10), %edi
.L7:
  xorl %eax, %eax
.L2:
  movl %eax, %r10d
  cmpl %eax, %esi
  jl .L12
  movl (%rdx,%r9,4), %ecx
  movl (%rbx,%rax,4), %ebp
  cmpl %eax, %ecx
  jg .L3
  subl %ecx, %r10d
  movl (%r11,%r9,4), %ecx
  movslq %r10d, %r10
  addl (%rbx,%r10,4), %ecx
  cmpl %ebp, %ecx
  cmovl %ebp, %ecx
  movl %ecx, (%r8,%rax,4)
  jmp .L4
.L3:
  movl %ebp, (%r8,%rax,4)
.L4:
  incq %rax
  jmp .L2
.L12:
  cmpl %r9d, %edi
  je .L9
  movq %rbx, %rax
  incq %r9
  movq %r8, %rbx
  movq %rax, %r8
  jmp .L7
.L9:
  movslq %esi, %rsi
  movl (%r8,%rsi,4), %eax
  addq $696, %rsp
  popq %rbx
  popq %rbp
  ret