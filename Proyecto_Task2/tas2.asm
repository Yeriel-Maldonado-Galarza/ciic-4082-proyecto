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
player_state: .res 1

.exportzp player_x, player_y, player_dir, player_state

PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
PPUADDR   = $2006
PPUDATA   = $2007
OAMADDR   = $2003
OAMDMA    = $4014


.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA

  ;updates tiles AFTER dma transfer
  JSR update_player
  JSR draw_player1



	LDA #$00
	STA $2005
	STA $2005
  RTI
.endproc

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

  ; write sprite data
  LDX #$00
load_sprites:
  LDA sprites,X
  STA $0200,X
  INX
  CPX #$ff
  BNE load_sprites


	; finally, attribute table
	LDA PPUSTATUS
	LDA #$23
	STA PPUADDR
	LDA #$c2
	STA PPUADDR
	LDA #%01000000
	STA PPUDATA

	LDA PPUSTATUS
	LDA #$23
	STA PPUADDR
	LDA #$e0
	STA PPUADDR
	LDA #%00001100
	STA PPUDATA

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
LDA player_dir

cmp #$02
BEQ state2a 
CMP #$01
BEQ state1a
jmp state3a
    state1a: 
    ;p1 walk right
        LDA #$27 ;load top left 
        STA $0201
        LDA #$28  ; top right
        STA $0205
        LDA #$37 ; bottom left
        STA $0209
        LDA #$38 ; bottom right
        STA $020d

        ;p2 want down

        ;store tile locations
        LDA #$07 ;load top left
        STA $0211
        LDA #$08  ; top right
        STA $0215
        LDA #$17 ; bottom left
        STA $0219
        LDA #$18 ; bottom right
        STA $021d

        ;p3 walk up
        LDA #$0b ;load top left
        STA $0221
        LDA #$0c  ; top right
        STA $0225
        LDA #$1b ; bottom left
        STA $0229
        LDA #$1c ; bottom right
        STA $022d

        ;p4 walk west
        LDA #$47 ;load top left
        STA $0231
        LDA #$48  ; top right
        STA $0235
        LDA #$57 ; bottom left
        STA $0239
        LDA #$58 ; bottom right
        STA $023d        

        JMP positiona
    state2a:
        LDA #$25 ;load top left
        STA $0201
        LDA #$26  ; top right
        STA $0205
        LDA #$35 ; bottom left
        STA $0209
        LDA #$36 ; bottom right
        STA $020d

        ;p2
            ;store tile locations
        LDA #$05 ;load top left
        STA $0211
        LDA #$06  ; top right
        STA $0215
        LDA #$15 ; bottom left
        STA $0219
        LDA #$16 ; bottom right
        STA $021d

        ;p3 walk up
        LDA #$0d ;load top left
        STA $0221
        LDA #$0e  ; top right
        STA $0225
        LDA #$1d ; bottom left
        STA $0229
        LDA #$1e ; bottom right
        STA $022d

        ;p4 walk west
        LDA #$45 ;load top left
        STA $0231
        LDA #$46  ; top right
        STA $0235
        LDA #$55 ; bottom left
        STA $0239
        LDA #$56 ; bottom right
        STA $023d    

        JMP positiona
    state3a:
        LDA #$29 ;load top left
        STA $0201
        LDA #$2a  ; top right
        STA $0205
        LDA #$39 ; bottom left
        STA $0209
        LDA #$3a ; bottom right
        STA $020d

        ;p2
            ;store tile locations
        LDA #$09 ;load top left
        STA $0211
        LDA #$0a  ; top right
        STA $0215
        LDA #$19 ; bottom left
        STA $0219
        LDA #$1a ; bottom right
        STA $021d

        ;p3 walk up
        LDA #$2d ;load top left
        STA $0221
        LDA #$2e  ; top right
        STA $0225
        LDA #$3d ; bottom left
        STA $0229
        LDA #$3e ; bottom right
        STA $022d

        ;p4 walk west
        LDA #$49 ;load top left
        STA $0231
        LDA #$4a  ; top right
        STA $0235
        LDA #$59 ; bottom left
        STA $0239
        LDA #$5a ; bottom right
        STA $023d 

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




      ;store model tile attributes------------------------------p2
        LDA #$00
        STA $0212
        STA $0216
        STA $021a
        STA $021e


        ; top left tile:
        LDA player_y
        CLC
        ADC #$00
        STA $0210
        LDA player_x
        CLC
        ADC #$10
        STA $0213

        ; top right tile (x + 8): for an addition we do  clc then adc
        LDA player_y
        CLC
        ADC #$00
        STA $0214
        LDA player_x
        CLC
        ADC #$18
        STA $0217

        ; bottom left tile (y + 8):
        LDA player_y
        CLC
        ADC #$08
        STA $0218
        LDA player_x
        CLC
        ADC #$10
        STA $021b

        ; bottom right tile (x + 8, y + 8)
        LDA player_y
        CLC
        ADC #$08
        STA $021c
        LDA player_x
        CLC
        ADC #$18
        STA $021f

      ;store model tile attributes------------------------------p3
        LDA #$00
        STA $0222
        STA $0226
        STA $022a
        STA $022e


        ; top left tile:
        LDA player_y
        CLC
        ADC #$00
        STA $0220
        LDA player_x
        CLC
        ADC #$20
        STA $0223

        ; top right tile (x + 8): for an addition we do  clc then adc
        LDA player_y
        CLC
        ADC #$00
        STA $0224
        LDA player_x
        CLC
        ADC #$28
        STA $0227

        ; bottom left tile (y + 8):
        LDA player_y
        CLC
        ADC #$08
        STA $0228
        LDA player_x
        CLC
        ADC #$20
        STA $022b

        ; bottom right tile (x + 8, y + 8)
        LDA player_y
        CLC
        ADC #$08
        STA $022c
        LDA player_x
        CLC
        ADC #$28
        STA $022f

      ;store model tile attributes------------------------------p4
        LDA #$00
        STA $0232
        STA $0236
        STA $023a
        STA $023e


        ; top left tile:
        LDA player_y
        CLC
        ADC #$00
        STA $0230
        LDA player_x
        CLC
        ADC #$30
        STA $0233

        ; top right tile (x + 8): for an addition we do  clc then adc
        LDA player_y
        CLC
        ADC #$00
        STA $0234
        LDA player_x
        CLC
        ADC #$38
        STA $0237

        ; bottom left tile (y + 8):
        LDA player_y
        CLC
        ADC #$08
        STA $0238
        LDA player_x
        CLC
        ADC #$30
        STA $023b

        ; bottom right tile (x + 8, y + 8)
        LDA player_y
        CLC
        ADC #$08
        STA $023c
        LDA player_x
        CLC
        ADC #$38
        STA $023f
    

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
        DEC player_state
        DEC player_state
        DEC player_state
        DEC player_state
        DEC player_state
        DEC player_state
        DEC player_state
        DEC player_state
        DEC player_state
        lda player_dir
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
