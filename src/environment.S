/*
Speedfuck custom ABI:
argument1 in %rax
argument2 in %rcx
return value in %rax

scratch registers: %rax, %rcx, %r11
%rbx contains the brainfuck pointer
callee saved: all others
*/

.data

writepos:
    .quad 0

.text
    .comm writebuff, 1024
    .comm readbuff, 2

.macro store_regs
    sub $8, %rsp
    mov %dx, (%rsp)
    mov %dil, 2(%rsp)
    mov %sil, 3(%rsp)
.endm

.macro restore_regs
    mov (%rsp), %dx
    mov 2(%rsp), %dil
    mov 3(%rsp), %sil
    add $8, %rsp
.endm

.global bfgetchar
.type bfgetchar, %function
bfgetchar:
    store_regs

    call bfflush

    mov $0, %eax
    mov $0, %edi
    mov $readbuff, %rsi
    mov $1, %edx
    syscall

    mov readbuff, %al

    restore_regs
    ret

.global bfputchar
.type bfputchar, %function
bfputchar:
    mov writepos, %rcx
    mov %al, writebuff(%rcx)
    inc %rcx

    cmp $'\n', %al
    je storeAndFlush
    cmp $1024, %rcx
    jae storeAndFlush

    mov %rcx, writepos
    ret

storeAndFlush:
    store_regs
    mov %rcx, %rdx
    call flushWritebuff
    restore_regs
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
    store_regs

    call bfflush

    mov %rax, %rsi
    mov %rcx, %rdx
    mov $1, %edi
    mov $1, %eax
    syscall

    restore_regs
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
