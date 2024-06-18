segment .text
global _start
_start:
call main
mov rax, 60
mov rdi, 0
syscall
write:
push rbp
mov rbp, rsp
mov rdx, [rbp + 16]
mov rsi, [rbp + 24]
mov rdi, [rbp + 32]
mov rax, 1
syscall
pop rbp
ret
exit:
push rbp
mov rbp, rsp
mov rdi, [rbp + 16]
mov rax, 60
syscall
print:
push rbp
mov rbp, rsp
mov rax, 1
sub rsp, 8
mov [rsp], rax
sub rsp, 16
mov rax, [rbp - -16]
mov [rbp - 24], rax
mov rax, [rbp - -24]
mov [rbp - 16], rax
call write
add rsp, 24
add rsp, 0
pop rbp
ret
println:
push rbp
mov rbp, rsp
sub rsp, 16
mov rax, [rbp - -16]
mov [rbp - 16], rax
mov rax, [rbp - -24]
mov [rbp - 8], rax
call print
add rsp, 16
sub rsp, 16
mov QWORD [rsp + 8], .C0
mov QWORD [rsp], 1
call print
add rsp, 16
add rsp, 0
pop rbp
ret
.C0: db "", 10, ""
die:
push rbp
mov rbp, rsp
sub rsp, 16
mov QWORD [rsp + 8], .C0
mov QWORD [rsp], 7
call print
add rsp, 16
sub rsp, 16
mov rax, [rbp - -16]
mov [rbp - 16], rax
mov rax, [rbp - -24]
mov [rbp - 8], rax
call println
add rsp, 16
mov rax, 1
sub rsp, 8
mov [rsp], rax
call exit
add rsp, 8
add rsp, 0
pop rbp
ret
.C0: db "error: "
todo:
push rbp
mov rbp, rsp
sub rsp, 16
mov QWORD [rsp + 8], .C0
mov QWORD [rsp], 6
call print
add rsp, 16
sub rsp, 16
mov rax, [rbp - -16]
mov [rbp - 16], rax
mov rax, [rbp - -24]
mov [rbp - 8], rax
call println
add rsp, 16
mov rax, 1
sub rsp, 8
mov [rsp], rax
call exit
add rsp, 8
add rsp, 0
pop rbp
ret
.C0: db "todo: "
main:
push rbp
mov rbp, rsp
mov rax, [rbp - -16]
sub rsp, 8
mov [rsp], rax
mov rax, 1
mov rdx, rax
mov rax, [rsp]
add rsp, 8
cmp rax, rdx
sete al
movzx rax, al
cmp rax, 0
je .B0
sub rsp, 16
mov QWORD [rsp + 8], .C0
mov QWORD [rsp], 53
call die
add rsp, 16
.B0:
sub rsp, 16
mov QWORD [rsp + 8], .C1
mov QWORD [rsp], 4
call todo
add rsp, 16
add rsp, 0
pop rbp
ret
.C0: db "incorrect argument count; usage: bpl path/to/file.bpl"
.C1: db "main"
