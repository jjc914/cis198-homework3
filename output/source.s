.global _main
.align 16
_main:
str x29, [sp]
mov x29, sp
mov x1, #3
sub sp, sp, #16
str x1, [sp]
mov x1, #5
sub sp, sp, #16
str x1, [sp]
ldr x1, [x29, #-16]
sub sp, sp, #16
str x1, [sp]
ldr x1, [x29, #-32]
sub sp, sp, #16
str x1, [sp]
ldr x1, [x29, #-16]
sub sp, sp, #16
str x1, [sp]
ldr x1, [sp]
add sp, sp, #16
ldr x2, [sp]
mul x1, x1, x2
str x1, [sp]
ldr x1, [sp]
add sp, sp, #16
ldr x2, [sp]
mul x1, x1, x2
str x1, [sp]
ldr x0, [sp]
add sp, sp, #16
mov x16, #1
svc 0
ldr x29, [sp]
mov sp, x29
ret