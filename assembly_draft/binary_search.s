.globl _binary_search
_binary_search:
  subl $1, %esi
  js .L6
  xorl %ecx, %ecx
  jmp .L5
.L10:
  leal 1(%rax), %ecx
  cmpl %ecx, %esi
  jl .L6
.L5:
  leal (%rsi,%rcx), %eax
  sarl %eax
  movslq %eax, %r8
  cmpl %edx, (%rdi,%r8,4)
  jl .L10
  jle .L7
  leal -1(%rax), %esi
  cmpl %ecx, %esi
  jge .L5
.L6:
  xorl %eax, %eax
  ret
.L7:
  movl $1, %eax
  ret
