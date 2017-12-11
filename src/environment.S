.text

    .comm buff, 2
    .comm vars, 32768


.global bfgetchar
.type bfgetchar, %function
bfgetchar:
    push %rdi

    mov $0, %eax
    mov $0, %edi
    mov $buff, %rsi
    mov $1, %edx
    syscall

    pop %rdi
    mov buff, %al
    ret

.global bfputchar
.type bfputchar, %function
bfputchar:
    push %rdi
    mov %al, buff
    movb $0, buff+1

    mov $1, %eax
    mov $1, %edi
    mov $buff, %rsi
    mov $1, %edx
    syscall

    pop %rdi
    ret

.global bfputs
.type bfputs, %function
bfputs:
    push %rdi

    mov %rax, %rsi
    mov %rcx, %rdx
    mov $1, %eax
    mov $1, %edi
    syscall

    pop %rdi
    ret

.global _start
.type _start, %function
_start:
    /* TODO: zero vars*/
    mov $vars, %rdi
    call bfmain

    mov $60, %rax
    mov $0, %rdi
    syscall
