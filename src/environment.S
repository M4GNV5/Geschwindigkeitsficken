.data

writepos:
    .quad 0

.text

    .comm vars, 32768
    .comm writebuff, 1024
    .comm readbuff, 2

.global bfgetchar
.type bfgetchar, %function
bfgetchar:
    call bfflush

    push %rdi

    mov $0, %eax
    mov $0, %edi
    mov $readbuff, %rsi
    mov $1, %edx
    syscall

    pop %rdi
    mov readbuff, %al
    ret

.global bfputchar
.type bfputchar, %function
bfputchar:
    mov writepos, %rdx
    mov %al, writebuff(%rdx)
    inc %rdx

    cmp $'\n', %al
    je flushWritebuff
    cmp $1024, %rdx
    jae flushWritebuff

    mov %rdx, writepos
    ret

flushWritebuff:
    push %rdi

    mov $1, %eax
    mov $1, %edi
    mov $writebuff, %rsi
    syscall

    movq $0, writepos
    pop %rdi
    ret

.type bfputchar, %function
bfflush:
    mov writepos, %rdx
    cmp $0, %rdx
    je emptyBuff

    call flushWritebuff

emptyBuff:
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
    add $16384, %rdi
    call bfmain

    call bfflush

exit:
    mov $60, %rax
    mov $0, %rdi
    syscall
