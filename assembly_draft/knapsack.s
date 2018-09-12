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
  movl $0, %eax
  pushq %rbx
  movl $101, %ecx
  movl $0, %r9d
  subq $696, %rsp
  leaq -112(%rsp), %rdi
  leaq 292(%rsp), %r8
  rep stosl
  leaq -112(%rsp), %rbx
  leal -1(%r10), %edi
.L7:
  movl $0, %eax
.L2:
  movl %eax, %r10d
  cmpl %eax, %esi
  jl .L12
  movq %r9, %rcx
  shlq $2, %rcx
  movl (%rcx,%rdx), %ecx
  movq %rax, %rbp
  shlq $2, %rbp
  movl (%rbx,%rbp), %ebp
  cmpl %eax, %ecx
  jg .L3
  subl %ecx, %r10d
  movq %r9, %rcx
  shlq $2, %rcx
  movl (%rcx,%r11), %ecx
  movslq %r10d, %r10
  shlq $2, %r10
  addl (%rbx,%r10), %ecx
  cmpl %ebp, %ecx
  cmovl %ebp, %ecx
  movq %rax, %r10
  shlq $2, %r10
  movl %ecx, (%r8,%r10)
  jmp .L4
.L3:
  movq %rax, %r10
  shlq $2, %r10
  movl %ebp, (%r8,%r10)
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
  movq %rsi, %rax
  shlq $2, %rax
  movl (%rax,%r8), %eax
  addq $696, %rsp
  popq %rbx
  popq %rbp
  ret