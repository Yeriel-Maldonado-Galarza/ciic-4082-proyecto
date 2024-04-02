.segment "HEADER"
.byte $4e, $45, $53, $1a ; Magic string that always begins an iNES header
.byte $02        ; Number of 16KB PRG-ROM banks
.byte $01        ; Number of 8KB CHR-ROM banks
.byte %00000000  ; Horizontal mirroring, no save RAM, no mapper
.byte %00000000  ; No special-case flags set, no mapper
.byte $00        ; No PRG-RAM present
.byte $00        ; NTSC format


.segment "ZEROPAGE"
player_x: .res 1
player_y: .res 1
player_dir: .res 1
player_look: .res 1
player_state: .res 1
pad1: .res 1  ; task 3 specific

; constants
PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
PPUADDR   = $2006
PPUDATA   = $2007
OAMADDR   = $2003
OAMDMA    = $4014

;  registro del control
CONTROLLER1 = $4016

; bits para cada boton que planeo usar 
BTN_RIGHT   = %00000001
BTN_LEFT    = %00000010
BTN_DOWN    = %00000100
BTN_UP      = %00001000



.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA


  JSR read_controller1
  ;updates tiles AFTER dma transfer
  JSR update_player
  JSR draw_player1



	LDA #$00
	STA $2005
	STA $2005
  RTI
.endproc

.export reset_handler
.proc reset_handler
  SEI
  CLD
  LDX #$40
  STX $4017
  LDX #$FF
  TXS
  INX
  STX $2000
  STX $2001
  STX $4010
  BIT $2002
vblankwait:
  BIT $2002
  BPL vblankwait

	LDX #$00
	LDA #$FF
clear_oam:
	STA $0200,X ; set sprite y-positions off the screen
	INX
	INX
	INX
	INX
	BNE clear_oam

vblankwait2:
  BIT $2002
  BPL vblankwait2
  JMP main
.endproc


.export main
.proc main
  ; write a palette
  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR
load_palettes:
  LDA palettes,X
  STA PPUDATA
  INX
  CPX #$20
  BNE load_palettes



vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

forever:
  JMP forever
.endproc

.proc draw_player1
;save resgisters
	PHP
	PHA
	TXA
	PHA
	TYA
	PHA

;store player model tile numbers
;i want to change what version of this code runs dependig on the value currently in player_state

LDA player_dir ; variable que determina el estado del sprite, aumenta cada 10 frames

cmp #$02
BEQ state2a 
CMP #$01
BEQ state1a
jmp state3a
    state1a: 
    ;p1 walk right'
    ; task 3 specific ---------------------------------------------------------------------
        lda player_look
        cmp #$02
        beq walk_up1
        cmp #$03
        beq walk_down1
        cmp #$04
        beq walk_left1

        walk_right1:
          LDA #$27 ;load top left 
          STA $0201
          LDA #$28  ; top right
          STA $0205
          LDA #$37 ; bottom left
          STA $0209
          LDA #$38 ; bottom right
          STA $020d
          jmp exit1

        walk_up1:
          LDA #$0b ;load top left
          STA $0201
          LDA #$0c  ; top right
          STA $0205
          LDA #$1b ; bottom left
          STA $0209
          LDA #$1c ; bottom right
          STA $020d
          jmp exit1
        walk_down1:
          LDA #$07 ;load top left
          STA $0201
          LDA #$08  ; top right
          STA $0205
          LDA #$17 ; bottom left
          STA $0209
          LDA #$18 ; bottom right
          STA $020d
          jmp exit1
        walk_left1:
          LDA #$47 ;load top left
          STA $0201
          LDA #$48  ; top right
          STA $0205
          LDA #$57 ; bottom left
          STA $0209
          LDA #$58 ; bottom right
          STA $020d
          jmp exit1

        exit1:
        JMP positiona
    state2a:

        lda player_look
        cmp #$02
        beq walk_up2
        cmp #$03
        beq walk_down2
        cmp #$04
        beq walk_left2


        walk_right2:
          LDA #$25 ;load top left 
          STA $0201
          LDA #$26  ; top right
          STA $0205
          LDA #$35 ; bottom left
          STA $0209
          LDA #$36 ; bottom right
          STA $020d
          jmp exit2

        walk_up2:
          LDA #$0d ;load top left
          STA $0201
          LDA #$0e  ; top right
          STA $0205
          LDA #$1d ; bottom left
          STA $0209
          LDA #$1e ; bottom right
          STA $020d
          jmp exit2
        walk_down2:
          LDA #$05 ;load top left
          STA $0201
          LDA #$06  ; top right
          STA $0205
          LDA #$15 ; bottom left
          STA $0209
          LDA #$16 ; bottom right
          STA $020d
          jmp exit2
        walk_left2:
          LDA #$45 ;load top left
          STA $0201 
          LDA #$46  ; top right
          STA $0205
          LDA #$55 ; bottom left
          STA $0209
          LDA #$56 ; bottom right
          STA $020d 
          jmp exit2

        exit2:

        JMP positiona
    state3a:
        lda player_look
        cmp #$02
        beq walk_up3
        cmp #$03
        beq walk_down3
        cmp #$04
        beq walk_left3


        walk_right3:
          LDA #$29 ;load top left 
          STA $0201
          LDA #$2a  ; top right
          STA $0205
          LDA #$39 ; bottom left
          STA $0209
          LDA #$3a ; bottom right
          STA $020d
          jmp exit3

        walk_up3:
          LDA #$2d ;load top left
          STA $0201
          LDA #$2e  ; top right
          STA $0205
          LDA #$3d ; bottom left
          STA $0209
          LDA #$3e ; bottom right
          STA $020d
          jmp exit3
        walk_down3:
          LDA #$09 ;load top left
          STA $0201 
          LDA #$0a  ; top right
          STA $0205
          LDA #$19 ; bottom left
          STA $0209
          LDA #$1a ; bottom right
          STA $020d
          jmp exit3
        walk_left3:
          LDA #$49 ;load top left
          STA $0201
          LDA #$4a  ; top right
          STA $0205
          LDA #$59 ; bottom left
          STA $0209
          LDA #$5a ; bottom right
          STA $020d 
          jmp exit3

        exit3:



positiona:
    ;store model tile attributes 
        LDA #$00
        STA $0202
        STA $0206
        STA $020a
        STA $020e

    ;store tile locations
        ; top left tile:
        LDA player_y
        STA $0200
        LDA player_x
        STA $0203

        ; top right tile (x + 8): for an addition we do  clc then adc
        LDA player_y
        STA $0204
        LDA player_x
        CLC
        ADC #$08
        STA $0207

        ; bottom left tile (y + 8):
        LDA player_y
        CLC
        ADC #$08
        STA $0208
        LDA player_x
        STA $020b

        ; bottom right tile (x + 8, y + 8)
        LDA player_y
        CLC
        ADC #$08
        STA $020c
        LDA player_x
        CLC
        ADC #$08
        STA $020f

    

exit_subroutine:
;restore registers and return
	PLA
	TAY
	PLA
	TAX
	PLA
	PLP
	RTS ;rts ends subroutine

	;grab byte 2 of the first 4 sprites
.endproc





.proc update_player
    PHP
    PHA
    TXA
    PHA
    TYA
    PHA

; task 3 specific ---------------------------------------------------------------------
  LDA pad1        ; carga registro con toda la informacion sobre los botones
  AND #BTN_LEFT   ; lee el bit responsable por el movimiento a la izquierda
  BEQ check_right ; si el boton no esta siendo apretado se revisa el siguiente boton
  DEC player_x  ; si el boton esta siendo apretado, manipula la posicion x
  LDA #$04
  STA player_look   ; se ajusta la variable que controla cual sprite se carga
  check_right:
    LDA pad1
    AND #BTN_RIGHT
    BEQ check_up
    INC player_x
    LDA #$01
    STA player_look
  check_up:
    LDA pad1
    AND #BTN_UP
    BEQ check_down
    DEC player_y
    LDA #$02
    STA player_look
  check_down:
    LDA pad1
    AND #BTN_DOWN
    BEQ done_checking
    INC player_y
    LDA #$03
    STA player_look

  done_checking:


    lda player_dir
    cmp #$04
    BEQ resetdir

    LDA player_state ; load player state
    CMP #$14
    BEQ third_state   ; branch if at 3 state

    state_even:
        INC player_state
        JMP exit_subroutine
    
    resetdir:
        dec player_dir
        dec player_dir
        dec player_dir
        jmp exit_subroutine

    third_state:   ; once im here i just want to take the number down to 0
        DEC player_state
        DEC player_state
        DEC player_state
        DEC player_state
        DEC player_state
        DEC player_state
        DEC player_state
        DEC player_state
        DEC player_state
        inc player_dir
        JMP exit_subroutine

    ; all done, clean up and return

    exit_subroutine:
        ; all done, clean up and return
        PLA
        TAY
        PLA
        TAX
        PLA
        PLP
        RTS
    .endproc


; task 3 specific ---------------------------------------------------------------------
.proc read_controller1
  PHA
  TXA
  PHA
  PHP

  ; write a 1, then a 0, to CONTROLLER1
  ; to latch button states
  LDA #$01
  STA CONTROLLER1
  LDA #$00
  STA CONTROLLER1

  LDA #%00000001 ;  se inicializa el bus como 1, el valor especifico es importante.
  STA pad1

get_buttons:
  LDA CONTROLLER1 ; carga el valor del boton en el acumulador
  LSR A           ; este valor en el acumulador se mueve al carry
  ROL pad1        ; del carry se mueve al lado derecho
                  ; valor al final izquierdo se mueve al carry
  BCC get_buttons ; si el carry es nuestro valor al final del lado derecho ya se ha movido hasta el carry. por lo que termina la subrutina

  PLP
  PLA
  TAX
  PLA
  RTS
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"
palettes:
.byte $0f, $0C, $21, $32 ; backgroun probBLY
.byte $0f, $2b, $3c, $39
.byte $0f, $0c, $07, $13
.byte $0f, $19, $09, $29

.byte $0f, $0C, $21, $32 ;
.byte $0f, $19, $09, $29
.byte $0f, $19, $09, $29
.byte $0f, $19, $09, $29

sprites:
;suouth movement sprites
	;#1
.byte $20, $05, $01, $70 ; position y, sprite, palette, position x
.byte $20, $06, $01, $78
.byte $28, $15, $01, $70
.byte $28, $16, $01, $78

	;# neutral



.segment "CHR"
.incbin "pixelart.chr"
