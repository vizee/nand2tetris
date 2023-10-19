// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen
// by writing 'black' in every pixel;
// the screen should remain fully black as long as the key is pressed.
// When no key is pressed, the program clears the screen by writing
// 'white' in every pixel;
// the screen should remain fully clear as long as no key is pressed.

(FILL)
    // row = 0
    @row
    M=0
(FILLROW)
    // if row >= 8192; goto ENDFILL
    @row
    D=M
    @8192
    D=D-A
    @ENDFILL
    D;JGE
    // col = 0
    @col
    M=0
    // R1 = KBD ? -1 : 0
    @R1
    M=0
    @KBD
    D=M
    @FILLCOL
    D;JEQ
    @R1
    M=-1
(FILLCOL)
    // if col >= 32; goto ENDFILLCOL
    @col
    D=M
    @32
    D=D-A
    @ENDFILLCOL
    D;JGE

    // R0 = &SCREEN[row+col]
    @SCREEN
    D=A
    @row
    D=D+M
    @col
    D=D+M
    @R0
    M=D
    // *R0 = R1
    @R1
    D=M
    @R0
    A=M
    M=D

    // col += 1
    @col
    M=M+1
    @FILLCOL
    0;JMP
(ENDFILLCOL)
    // row += 32
    @32
    D=A
    @row
    M=M+D
    @FILLROW
    0;JMP
(ENDFILL)
    @FILL
    0;JMP
