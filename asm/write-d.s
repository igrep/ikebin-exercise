0000: b80100        mov ax, 0001
0003: cd07          int 7
0005: 04            ; sys write
0006: 1000          ; arg ; ヘッダを除いて 0x10バイト目（データセクションの「hello」から
0008: 0600          ; arg ; 0x6バイト分
000a: b80000        mov ax, 0000
000d: cd07          int 7
000f: 01            ; sys exit
