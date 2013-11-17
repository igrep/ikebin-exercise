! Assembly for MINIX
! write(1, hello, 6);

mov ax, #1 ! 出力先を標準出力に設定
int 7
.data1 4 ; sys write
.data2 hello, 6 ; データセクション helloから6バイト分

! exit(0);
mov ax, #0
int 7
.data1 1

.sect .data
hello: .ascii "hello\n"
