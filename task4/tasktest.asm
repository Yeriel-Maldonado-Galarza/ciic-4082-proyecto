.segment "HEADER"
.byte $4e, $45, $53, $1a ; Magic string that always begins an iNES header
.byte $02        ; Number of 16KB PRG-ROM banks
.byte $01        ; Number of 8KB CHR-ROM banks
.byte $01, $00  ; Horizontal mirroring, no save RAM, no mapper
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
data: .res 1; tile data
backx: .res 1  ;  not currently in use the idea was to use as ppuadrress
backy: .res 1
back: .res 1 ; byte
dummy: .res 1  ; copy of byte
megatilecount: .res 2
index: .res 1
yb: .res 1
xb: .res 1
relative: .res 1
storex: .res 1
leveloffset: .res 1
scrollvalue: .res 1


; constants
PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
PPUSCROLL = $2005
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
btn_switch =   %00010000
btn_stage =   %00100000

floor = %00000000
wall1 = %00000001
wall2 = %00000010
seethru = %00000011


.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA

 ; 

  JSR read_back
  JSR read_controller1
  ;updates tiles AFTER dma transfer
  JSR update_player
  JSR draw_player1
  ;JSR load_backgroundfirst


  LDA scrollvalue
  STA PPUSCROLL ; set X scroll
  LDA #$00
  STA PPUSCROLL ; set Y scroll

	; LDA #$00
	; STA $2005
	; STA $2005
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
  ;lda leveloffset
  ;cmp #$00
  ;beq lvl1
  ;JMP main2
  lvl1:
  JMP main
.endproc

; just need to add an offset for the next level
.export main

.proc main
  ; write a palette
  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR
load_palettes:
  LDA palettes,x
  STA PPUDATA
  INX
  CPX #$10
  BNE load_palettes


; THIS MIGHT BE RESONSIBLE FOR it only doing like the first loop


load_address:

  LDA PPUSTATUS 
  LDA #$00
  STA backx

  LDA PPUSTATUS 
  LDA #$20
  STA backy

  LDA PPUSTATUS 
  LDA #$00
  STA yb

  LDA PPUSTATUS 
  LDA #$00
  STA xb

  
  LDA PPUSTATUS 
  LDA #$00
  STA relative

  LDA #$00
  STA megatilecount


  lda leveloffset
  cmp #$00
  beq stage1
   stage2:
     ldx #$78
     jmp load2

   stage1:
     ldx #$00

LoadBackgroundLoop1:
    
    lda megatilecount
    sta yb
    lda yb
    lsr yb
    lsr yb
    asl yb
    asl yb
    asl yb
    asl yb
    asl yb
    asl yb

;00000001
;00001000

    lda megatilecount
    sta xb
    lda xb
    and #$03
    sta xb
    asl xb
    asl xb
    asl xb
    clc
    lda #$00
    adc yb
    adc xb
    sta index
    ;so take index firt two bits go to backy and the last 2 go to backx
  ; start this at 2000  bc thats where my nametable 1 starts
  ; when i start writing the 2nd map of the stage i believe i start writing on 24
  ;index = yb shifter left 4 times + xb
  ;yb should go up on that third one
  ; xb shouuld go up
 

  ;LOAD A BYTE IN ME FALTA IR SUBIENDO EL MEGATILE POR UNO on each iteration

  ; backy increases one when backx is in the DE to FE loop 
  stx storex
  lda storex
  cmp #$78
  beq load2

  lda background, x
  STA dummy

  lda #$00
  sta relative
  JSR tileset1
  asl dummy
  asl dummy
  ; inc  megatilecount

  lda #$02
  sta relative


  JSR tileset1
  asl dummy
  asl dummy


  lda #$04
  sta relative


  JSR tileset1
  asl dummy
  asl dummy

  lda #$06
  sta relative
  
  JSR tileset1

  ; lda backx
  ; sbc #$08
  ; sta backx
  stx storex
  lda storex
  cmp #$3b
  beq BACKdone


  lda storex
  cmp #$77
  beq BACK2done

  lda index
  cmp #$D8
  bne continue

  inc backy
  lda #$00 
  sta megatilecount
  lda #$00
  sta index
  
  jumpa:
  inx
  jmp LoadBackgroundLoop1

  continue:
  inc megatilecount
  ;inx
  lda #$10
  cmp megatilecount

  bne jumpa
  
  BACKdone:

    inx
    inc backy
    lda #$00
    sta megatilecount
    lda #$00
    sta index
    jmp LoadBackgroundLoop1

  BACK2done:
    jmp vblankwait
    ;jmp load2
  
  load2:
;    inc backy
    lda #$00
    sta megatilecount
    lda #$00
    sta index
    jmp LoadBackgroundLoop2

  LoadBackgroundLoop2:
    lda megatilecount
    sta yb
    lda yb
    lsr yb
    lsr yb
    asl yb
    asl yb
    asl yb
    asl yb
    asl yb
    asl yb

;00000001
;00001000

    lda megatilecount
    sta xb
    lda xb
    and #$03
    sta xb
    asl xb
    asl xb
    asl xb
    clc
    lda #$00
    adc yb
    adc xb
    sta index
    ;so take ind
  lda background, x
  STA dummy

  lda #$00
  sta relative
  JSR tileset2
  asl dummy
  asl dummy
  ; inc  megatilecount

  lda #$02
  sta relative


  JSR tileset2
  asl dummy
  asl dummy


  lda #$04
  sta relative


  JSR tileset2
  asl dummy
  asl dummy

  lda #$06 
  sta relative
  
  JSR tileset2

  ; lda backx
  ; sbc #$08
  ; sta backx

  stx storex
  lda storex
  cmp #$b3
  beq BACK3done


  lda storex
  cmp #$ef
  beq BACK4done

  lda index
  cmp #$D8
  bne continue2

  inc backy
  lda #$00 
  sta megatilecount
  lda #$00
  sta index
  
  jumpas:
  inx
  jmp LoadBackgroundLoop2

  continue2:
  inc megatilecount
  ;inx
  lda #$10
  cmp megatilecount

  bne jumpas
  BACK3done:

    inx
    inc backy
    lda #$00
    sta megatilecount
    lda #$00
    sta index
    jmp LoadBackgroundLoop2

  BACK4done:
    jmp vblankwait

vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

forever:
  jmp forever
.endproc
    
.proc tileset1
  PHP
	PHA
	TXA
	PHA
	TYA
	PHA

  lda dummy
  AND #%11000000
  sta back

  check_seethru:
  lda back
  cmp #%11000000
  bne check_wall1
  jmp s1
  check_wall1:
  lda back
  CMP #%01000000
  bne check_wall2
  jmp s2
  check_wall2:
  lda back
  CMP #%10000000
  bne check_floor
  jmp s3
  check_floor:
  LDA PPUSTATUS 
  clc
  LDA #$60; for later thisll likely have to be this + stage offset
  ;adc leveloffset
  STA data
  jmp place_tile

;this part of the code should be the same i just add an offset for the second map
;OH FOR NAMETABLE 2 THIS IS LITERALLY THE SAME I JUST HAVE TO LOAD A DIFFERENT NAMETBLAE

  s1:
  LDA PPUSTATUS 
  clc
  LDA #$66
 ; adc leveloffset
  STA data
  jmp place_tile
  s2:
  LDA PPUSTATUS 
  clc
  LDA #$64
 ; adc leveloffset
  STA data
  jmp place_tile
  s3:
  LDA PPUSTATUS 
  clc
  LDA #$62
 ; adc leveloffset
  STA data
  jmp place_tile

  ;NAMETABLE
  ; add something here that will manipulate data based on read value
  ;lets say for now  00 =60, 01  = 61, 10  = 62, 11 = 63


place_tile:

; 1 
; index =   ;index = yb shifter left 4 times + xb

    ;so take index firt two bits go to backy and the last 2 go to backx


;i dont think i need this overflow thing
      ; check_overflow:
      ; lda index
      ; cmp #$ff
      ; beq overflowing
      ; lda backx 
      ; clc
      ; adc index
      ; jmp placement
      ; overflowing:
      ;   inc backy
      ;   lda #$00
      ;   sta backx
      
    ; find a way to chck if the value if over 255 , if it is add one to back y
    ; else i just add the raw value to backx

    placement:

      LDA PPUSTATUS
      ;;adress
      LDA backy
      STA PPUADDR
      clc
      LDA backx
      adc index
      adc relative
      STA PPUADDR
      CLC
      LDA data
    ;  adc leveloffset
      STA PPUDATA



  ;2
      clc
      LDA PPUSTATUS
      LDA backy

      STA PPUADDR
      clc
      LDA backx
      
      adc index
      adc relative
      adc #$01
      STA PPUADDR
      LDA data
      CLC
      ADC #$01
   ;   adc leveloffset
      STA PPUDATA
      ; inc backx
      ; inc backx
      ;inc data


  ;3

      CLC
      ; inc backx
      LDA PPUSTATUS     ;basic loading background form
      LDA backy
 
      STA PPUADDR
      clc
      LDA backx
      clc
      adc index
      adc relative
      ADC #$20
      STA PPUADDR
      LDA data        ;
      CLC
      ADC #$10
     ; adc leveloffset
      STA PPUDATA
      ; inc backx
      ; inc backx

  ;4

      CLC
      LDA PPUSTATUS     ;basic loading background form
      LDA backy
      ;ADC #$21
      STA PPUADDR
      clc
      LDA backx
      clc
      adc index
      adc relative
      ADC #$21
      STA PPUADDR
      LDA data        
      CLC
      ADC #$11
    ;  adc leveloffset
      STA PPUDATA
      ;inc backx
      ;inc backx
      ; inc backx
      ; inc backx





    ; second hhalf of the screen
    ; LDA PPUSTATUS 
    ; LDA #$00
    ; STA backx

    ; inc backy
    ; lda backy 
    ; cmp #$24
    ; BEQ vblankwait

    ; jmp nametable_gen

    ; finish:
    ; lda backx 
    ; cmp $bf
    ; beq vblankwait
    ; jmp nametable_gen 



	PLA
	TAY
	PLA
	TAX
	PLA
	PLP
	RTS ;rts ends subroutine
.endproc 
    
.proc tileset2
  PHP
	PHA
	TXA
	PHA
	TYA
	PHA

  lda dummy
  AND #%11000000
  sta back

  check_seethru:
  lda back
  cmp #%11000000
  bne check_wall1
  jmp s1
  check_wall1:
  lda back
  CMP #%01000000
  bne check_wall2
  jmp s2
  check_wall2:
  lda back
  CMP #%10000000
  bne check_floor
  jmp s3
  check_floor:
  LDA PPUSTATUS 
  clc
  LDA #$68; for later thisll likely have to be this + stage offset
  ;adc leveloffset
  STA data
  jmp place_tile

;this part of the code should be the same i just add an offset for the second map
;OH FOR NAMETABLE 2 THIS IS LITERALLY THE SAME I JUST HAVE TO LOAD A DIFFERENT NAMETBLAE

  s1:
  LDA PPUSTATUS 
  clc
  LDA #$6e
 ; adc leveloffset
  STA data
  jmp place_tile
  s2:
  LDA PPUSTATUS 
  clc
  LDA #$6a
 ; adc leveloffset
  STA data
  jmp place_tile
  s3:
  LDA PPUSTATUS 
  clc
  LDA #$6c
 ; adc leveloffset
  STA data
  jmp place_tile

  ;NAMETABLE
  ; add something here that will manipulate data based on read value
  ;lets say for now  00 =60, 01  = 61, 10  = 62, 11 = 63


place_tile:

; 1 
; index =   ;index = yb shifter left 4 times + xb

    ;so take index firt two bits go to backy and the last 2 go to backx


;i dont think i need this overflow thing
      ; check_overflow:
      ; lda index
      ; cmp #$ff
      ; beq overflowing
      ; lda backx 
      ; clc
      ; adc index
      ; jmp placement
      ; overflowing:
      ;   inc backy
      ;   lda #$00
      ;   sta backx
      
    ; find a way to chck if the value if over 255 , if it is add one to back y
    ; else i just add the raw value to backx

    placement:

      LDA PPUSTATUS
      ;;adress
      LDA backy
      STA PPUADDR
      clc
      LDA backx
      adc index
      adc relative
      STA PPUADDR
      CLC
      LDA data
    ;  adc leveloffset
      STA PPUDATA



  ;2
      clc
      LDA PPUSTATUS
      LDA backy

      STA PPUADDR
      clc
      LDA backx
      
      adc index
      adc relative
      adc #$01
      STA PPUADDR
      LDA data
      CLC
      ADC #$01
   ;   adc leveloffset
      STA PPUDATA
      ; inc backx
      ; inc backx
      ;inc data


  ;3

      CLC
      ; inc backx
      LDA PPUSTATUS     ;basic loading background form
      LDA backy
 
      STA PPUADDR
      clc
      LDA backx
      clc
      adc index
      adc relative
      ADC #$20
      STA PPUADDR
      LDA data        ;
      CLC
      ADC #$10
     ; adc leveloffset
      STA PPUDATA
      ; inc backx
      ; inc backx

  ;4

      CLC
      LDA PPUSTATUS     ;basic loading background form
      LDA backy
      ;ADC #$21
      STA PPUADDR
      clc
      LDA backx
      clc
      adc index
      adc relative
      ADC #$21
      STA PPUADDR
      LDA data        
      CLC
      ADC #$11
    ;  adc leveloffset
      STA PPUDATA
      ;inc backx
      ;inc backx
      ; inc backx
      ; inc backx





    ; second hhalf of the screen
    ; LDA PPUSTATUS 
    ; LDA #$00
    ; STA backx

    ; inc backy
    ; lda backy 
    ; cmp #$24
    ; BEQ vblankwait

    ; jmp nametable_gen

    ; finish:
    ; lda backx 
    ; cmp $bf
    ; beq vblankwait
    ; jmp nametable_gen 



	PLA
	TAY
	PLA
	TAX
	PLA
	PLP
	RTS ;rts ends subroutine
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
    BEQ check_scroll
    INC player_y
    LDA #$03
    STA player_look
  check_scroll:
    LDA pad1
    AND #btn_switch
    BEQ check_stage
    LDA scrollvalue
    ADC #$01
    STA scrollvalue
      ; vblankwait:       ; wait for another vblank before continuing
      ; BIT PPUSTATUS
      ; BPL vblankwait

      ; LDA #%10010001  ; turn on NMIs, sprites use first pattern table
      ; STA PPUCTRL
      ; LDA #%00011110  ; turn on screen
      ; STA PPUMASK
  check_stage:
    LDA pad1
    AND #btn_stage
    BEQ done_checking
      lda #$08
      sta leveloffset

      vblankwaitstage:       ; wait for another vblank before continuing
      BIT PPUSTATUS
      BPL vblankwaitstage
      LDA #%00  ; turn on NMIs, sprites use first pattern table
      STA PPUCTRL
      sta PPUMASK
      jsr main
      ; lda #$00
      ; sta PPUCTRL
      ; LDA #$00  ; turn on screen
      ; STA PPUMASK
      ; LDA #%10010000  ; turn on NMIs, sprites use first pattern table
      ; STA PPUCTRL
      LDA #%10010000  ; turn on screen
      STA PPUCTRL
      


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

.proc read_back
  PHA
  TXA
  PHA
  PHP
  

  ; manipulate this back variable 
; if its like controller then id get the raw info from somewhere and then store it in back
; once its in back i have a byte , in this byte i got 4 2 bit segments that i want to extract, it seems horribly inneficcient to  dedicate variables to each one. 
; 

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
.byte $0f, $0C, $21, $32
.byte $0f, $0C, $21, $32
.byte $0f, $0C, $21, $32

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
background:
; ROW 1
.byte %01010101
.byte %01010101
.byte %01010101
.byte %01010101
  
.byte %01001111
.byte %11110000
.byte %00000000
.byte %00000101

.byte %01001010
.byte %10101010
.byte %10101010
.byte %10001010

.byte %01001000
.byte %00000000
.byte %00100000
.byte %00100000
; ROW 5
.byte %01001000
.byte %10101010
.byte %00100010
.byte %00100000
; i am here
.byte %01001000
.byte %00000010
.byte %00000010
.byte %00000010

.byte %01001010
.byte %10001010
.byte %10101010
.byte %10101010

.byte %00001111
.byte %11110000
.byte %00000000
.byte %00000010
; i am here
; ROW 3
.byte %01001011
.byte %10101010
.byte %10100010
.byte %00101010

.byte %01001000
.byte %00000000
.byte %00100010
.byte %00100010

.byte %01001010
.byte %10100010
.byte %10100010
.byte %00100000

.byte %01001000
.byte %00000010
.byte %00000010
.byte %11111000
; ROW 4
.byte %01000010
.byte %10101010
.byte %00100010
.byte %10111010

.byte %01001111
.byte %11000000
.byte %00100000
.byte %00000010

.byte %01010101
.byte %01010101
.byte %01010101
.byte %01010101

;nametable 2
.byte %01010101
.byte %01010101 
.byte %01010101
.byte %01010101
.byte %10000000 
.byte %10100011
.byte %11111010
.byte %10100001
.byte %10001000
.byte %10100010
.byte %11101010
.byte %10100000
.byte %00001000
.byte %10000010
.byte %00000000
.byte %00001001
.byte %00001011
.byte %10001010
.byte %10101010
.byte %10100001
.byte %10001011
.byte %11001000
.byte %00000000
.byte %00100001
.byte %10101011
.byte %10100010
.byte %00101010
.byte %00101001
.byte %10001111
.byte %11000010
.byte %00000010
.byte %00000000
.byte %10001011
.byte %10100000
.byte %10100010
.byte %10100001
.byte %10000100
.byte %00000100
.byte %00010000
.byte %00010100
.byte %00000101
.byte %01000101
.byte %01010101
.byte %00000000
.byte %10000100
.byte %00000000
.byte %00000000
.byte %00010000
.byte %11000101
.byte %01010101
.byte %01010101
.byte %01010000
.byte %11000000
.byte %00000000
.byte %00000000
.byte %00000000
.byte %10101010
.byte %10101010
.byte %10101010
.byte %01010101

; nametable3
.byte %01010101
.byte %01010101
.byte %01010101
.byte %01010101
;Row 2
.byte %01000000
.byte %10000000
.byte %00000000
.byte %00000001
;row 3
.byte %01001000
.byte %10000101
.byte %01000110
.byte %11100001

.byte %01001011
.byte %10000000
.byte %00000110
.byte %01100001
;row 5
.byte %01001011
.byte %00000101
.byte %01010000
.byte %11100001
;row  6

.byte %01001011
.byte %10010100
.byte %00000010
.byte %11100001
;baron.chonk8066 — Yesterday at 8:01 PM
;row 7

.byte %01001011
.byte %00000000
.byte %01010100
.byte %11100101
;row 8

.byte %00001011
.byte %10010101
.byte %00010000
.byte %01100000
;row 9
.byte %01001011
.byte %10000000
.byte %00101110
.byte %01100001
;row 10
.byte %01001011
.byte %10001011
.byte %10101110
.byte %00000001
;row 11
.byte %01001011
.byte %00000011
.byte %10101110
.byte %00010101 
;row 12
.byte %01001011
.byte %10011011
.byte %10101110
.byte %00000001
;baron.chonk8066 — Yesterday at 8:09 PM
;row 13
.byte %01001011
.byte %10000000
.byte %00101110
.byte %01010001
;row 14
.byte %01000000
.byte %00000101
.byte %01000000
.byte %00000001
;row 15
.byte %01010101
.byte %01010101
.byte %01010101
.byte %01010101

; nametable 4

;ROW 1
.byte %01010101
.byte %01010101
.byte %01010101
.byte %01010101
;ROW 2
.byte %01100000
.byte %00001110
.byte %10110000
.byte %00010001
;ROW 3
.byte %01101110
.byte %00101110
.byte %10111001
.byte %00000001
;ROW 4
.byte %01101110
.byte %00101110
.byte %10111000
.byte %01010001
;ROW 5
.byte %01101110
.byte %01011110
.byte %10111000
.byte %00000001
;ROW 6
.byte %01101110
.byte %00101110
.byte %10111000
.byte %01010101
;ROW 7
.byte %01101110
.byte %00101110
.byte %10111000
.byte %00000001
;baron.chonk8066 — Yesterday at 8:28 PM
;ROW 8
.byte %00000000
.byte %00101110
.byte %10111010
.byte %10100000
;ROW 9
.byte %01101110
.byte %00101110
.byte %10111010
.byte %11100101
;ROW  10
.byte %01101110
.byte %00001110
.byte %10111011
.byte %11100001
;ROW 11
.byte %01101110
.byte %01010010
.byte %10111010
.byte %11110001
;ROW 12
.byte %01101110
.byte %01010101
.byte %10111010
.byte %11100001
;ROW 13
.byte %01101110
.byte %00000001
.byte %10111001
.byte %11010001
;ROW 14
.byte %01100000
.byte %00010000
.byte %00000000
.byte %00000001
;ROW 15
.byte %01010101
.byte %01010101
.byte %01010101
.byte %01010101
;wrtir down bytes for map
; look at palettes to see how i load them up
;then i work on the indexing

.segment "CHARS"
.incbin "pixelart.chr"