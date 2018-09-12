/* 
	rdi: int *A
	esi: int n
	edx: int T

eax
ecx
r8
esi
*/

.globl _binary_search
_binary_search:
  subl $1, %esi
  js .L6
  movl $0, %ecx
  jmp .L5
.L10:
  movl %eax, %ecx
  incl %ecx
  cmpl %ecx, %esi
  jl .L6
.L5:
  movl %esi, %eax
  addl %ecx, %eax
  sarl %eax
  movslq %eax, %r8
  salq $2, %r8
  addq %rdi, %r8
  movl (%r8), %r8d
  cmpl %edx, %r8d
  jl .L10
  cmpl %edx, %r8d
  je .L7
  movl %eax, %esi
  decl %esi
  cmpl %ecx, %esi
  jge .L5
.L6:
  movl $0, %eax
  ret
.L7:
  movl $1, %eax
  ret
