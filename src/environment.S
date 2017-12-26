.data

writepos:
    .quad 0

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

.global bfhandlesegv
.type bfhandlesegv, %function
bfhandlesegv:
    /*mmap*/
    mov $9, %eax
    mov 0x10(%rsi), %rdi
    and $~4095, %rdi
    mov $4096, %esi
    mov $3, %edx /*PROT_READ | PROT_WRITE*/
    mov $34, %r10d /*MAP_ANONYMOUS | MAP_PRIVATE*/
    mov $-1, %r8d
    mov $0, %r9d
    syscall

    add $8, %rsp

    /*sigreturn*/
    mov $15, %eax
    mov $0, %edx
    syscall

.global _start
.type _start, %function
_start:
    sub $32, %rsp
    xorps %xmm0, %xmm0 /*zero the sigaction struct*/
    movups %xmm0, (%rsp)
    movups %xmm0, 0x10(%rsp)
    movq $bfhandlesegv, (%rsp) /*sa_sigaction = bfhandlesegv*/
    movl $0x4000004, 0x8(%rsp) /*sa_flags = SA_SIGINFO | SA_RESTORER*/

    /*sigaction*/
    mov $13, %eax
    mov $11, %edi
    mov %rsp, %rsi
    mov $0, %edx
    mov $8, %r10d
    syscall

    add $32, %rsp

    mov $0x100000800, %rbx
    call bfmain

    call bfflush

    /*exit*/
    mov $60, %eax
    mov $0, %edi
    syscall
