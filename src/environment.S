.data

writepos:
    .quad 0
vars:
    .zero 32768

.text
    .comm writebuff, 1024
    .comm readbuff, 2

.global bfgetchar
.type bfgetchar, %function
bfgetchar:
    call bfflush

    mov $0, %eax
    mov $0, %edi
    mov $readbuff, %rsi
    mov $1, %edx
    syscall

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
    mov $1, %eax
    mov $1, %edi
    /* size is put in %rdx by caller */
    mov $writebuff, %rsi
    syscall

    movq $0, writepos
    ret

.type bfputchar, %function
bfflush:
    mov writepos, %rdx
    cmp $0, %rdx
    jne flushWritebuff
    ret


.global bfputs
.type bfputs, %function
bfputs:
    mov %rax, %rsi
    mov %rcx, %rdx
    mov $1, %edi
    mov $1, %eax
    syscall

    ret

.global _start
.type _start, %function
_start:
    mov $vars, %rbx
    add $16384, %rbx
    call bfmain

    call bfflush

    mov $60, %rax
    mov $0, %rbx
    syscall
