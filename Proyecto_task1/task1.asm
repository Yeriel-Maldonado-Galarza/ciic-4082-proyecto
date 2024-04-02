PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
PPUADDR   = $2006
PPUDATA   = $2007
OAMADDR   = $2003
OAMDMA    = $4014

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
  STA $0210,X
  INX
  CPX #$ff
  BNE load_sprites

	; write nametables 
  ; place the  background tiles on the display

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$00
	STA PPUADDR
	LDX #$60
	STX PPUDATA

 LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$01
	STA PPUADDR
	LDX #$61
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$02
	STA PPUADDR
	LDX #$62
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$03
	STA PPUADDR
	LDX #$63
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$04
	STA PPUADDR
	LDX #$64
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$05
	STA PPUADDR
	LDX #$65
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$06
	STA PPUADDR
	LDX #$66
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$07
	STA PPUADDR
	LDX #$67
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$08
	STA PPUADDR
	LDX #$68
	STX PPUDATA

  LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$09
	STA PPUADDR
	LDX #$69
	STX PPUDATA


  LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$0a
	STA PPUADDR
	LDX #$6a
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$0b
	STA PPUADDR
	LDX #$6b
	STX PPUDATA

  LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$0C
	STA PPUADDR
	LDX #$6c
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$0d
	STA PPUADDR
	LDX #$6d
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$0e
	STA PPUADDR
	LDX #$6e
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$0f
	STA PPUADDR
	LDX #$6f
	STX PPUDATA



	; attribute table
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

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$20
	STA PPUADDR
	LDX #$70
	STX PPUDATA

 LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$21
	STA PPUADDR
	LDX #$71
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$22
	STA PPUADDR
	LDX #$72
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$23
	STA PPUADDR
	LDX #$73
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$24
	STA PPUADDR
	LDX #$74
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$25
	STA PPUADDR
	LDX #$75
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$26
	STA PPUADDR
	LDX #$76
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$27
	STA PPUADDR
	LDX #$77
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$28
	STA PPUADDR
	LDX #$78
	STX PPUDATA

  LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$29
	STA PPUADDR
	LDX #$79
	STX PPUDATA

  LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$2a
	STA PPUADDR
	LDX #$7a
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$2b
	STA PPUADDR
	LDX #$7b
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$2C
	STA PPUADDR
	LDX #$7c
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$2d
	STA PPUADDR
	LDX #$7d
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$2e
	STA PPUADDR
	LDX #$7e
	STX PPUDATA

	LDA PPUSTATUS
	LDA #$21
	STA PPUADDR
	LDA #$2f
	STA PPUADDR
	LDX #$7f
	STX PPUDATA



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

positiona:
  ;p1
        LDA #$27 ;load top left 
        STA $0201
        LDA #$28  ; top right
        STA $0205
        LDA #$37 ; bottom left
        STA $0209
        LDA #$38 ; bottom right
        STA $020d
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


;p2
        ;store tile locations
        LDA #$07 ;load top left
        STA $0211
        LDA #$08  ; top right
        STA $0215
        LDA #$17 ; bottom left
        STA $0219
        LDA #$18 ; bottom right
        STA $021d

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
;p3
        LDA #$0b ;load top left
        STA $0221
        LDA #$0c  ; top right
        STA $0225
        LDA #$1b ; bottom left
        STA $0229
        LDA #$1c ; bottom right
        STA $022d
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
;p4
        LDA #$47 ;load top left
        STA $0231
        LDA #$48  ; top right
        STA $0235
        LDA #$57 ; bottom left
        STA $0239
        LDA #$58 ; bottom right
        STA $023d   
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


;p5--------------------------------------------------------------------------

        LDA #$25 ;load top left
        STA $0241
        LDA #$26  ; top right
        STA $0245
        LDA #$35 ; bottom left
        STA $0249
        LDA #$36 ; bottom right
        STA $024d
    ;store model tile attributes
        LDA #$00
        STA $0242
        STA $0246
        STA $024a
        STA $024e

    ;store tile locations
        ; top left tile:
        LDA player_y
        CLC
        ADC #$10
        STA $0240
        LDA player_x
        STA $0243

        ; top right tile (x + 8): for an addition we do  clc then adc
        LDA player_y
        CLC
        ADC #$10
        STA $0244
        LDA player_x
        CLC
        ADC #$08
        STA $0247

        ; bottom left tile (y + 8):
        LDA player_y
        CLC
        ADC #$18
        STA $0248
        LDA player_x
        STA $024b

        ; bottom right tile (x + 8, y + 8)
        LDA player_y
        CLC
        ADC #$18
        STA $024c
        LDA player_x
        CLC
        ADC #$08
        STA $024f


;p6

        LDA #$05 ;load top left
        STA $0251
        LDA #$06  ; top right
        STA $0255
        LDA #$15 ; bottom left
        STA $0259
        LDA #$16 ; bottom right
        STA $025d
    ;store model tile attributes
        LDA #$00
        STA $0252
        STA $0256
        STA $025a
        STA $025e

    ;store tile locations
        ; top left tile:
        LDA player_y
        CLC
        ADC #$10
        STA $0250
        LDA player_x
        CLC
        ADC #$10
        STA $0253

        ; top right tile (x + 8): for an addition we do  clc then adc
        LDA player_y
        CLC
        ADC #$10
        STA $0254
        LDA player_x
        CLC
        ADC #$18
        STA $0257

        ; bottom left tile (y + 8):
        LDA player_y
        CLC
        ADC #$18
        STA $0258
        LDA player_x
        CLC
        ADC #$10
        STA $025b

        ; bottom right tile (x + 8, y + 8)
        LDA player_y
        CLC
        ADC #$18
        STA $025c
        LDA player_x
        CLC
        ADC #$18
        STA $025f
;p7
        LDA #$0d ;load top left
        STA $0261
        LDA #$0e  ; top right
        STA $0265
        LDA #$1d ; bottom left
        STA $0269
        LDA #$1e ; bottom right
        STA $026d
    ;store model tile attributes
        LDA #$00
        STA $0262
        STA $0266
        STA $026a
        STA $026e

    ;store tile locations
        ; top left tile:
        LDA player_y
        CLC
        ADC #$10
        STA $0260
        LDA player_x
        CLC
        ADC #$20
        STA $0263

        ; top right tile (x + 8): for an addition we do  clc then adc
        LDA player_y
        CLC
        ADC #$10
        STA $0264
        LDA player_x
        CLC
        ADC #$28
        STA $0267

        ; bottom left tile (y + 8):
        LDA player_y
        CLC
        ADC #$18
        STA $0268
        LDA player_x
        CLC
        ADC #$20
        STA $026b

        ; bottom right tile (x + 8, y + 8)
        LDA player_y
        CLC
        ADC #$18
        STA $026c
        LDA player_x
        CLC
        ADC #$28
        STA $026f
;p8
        LDA #$45 ;load top left
        STA $0271
        LDA #$46  ; top right
        STA $0275
        LDA #$55 ; bottom left
        STA $0279
        LDA #$56 ; bottom right
        STA $027d
    ;store model tile attributes
        LDA #$00
        STA $0272
        STA $0276
        STA $027a
        STA $027e

    ;store tile locations
        ; top left tile:
        LDA player_y
        CLC
        ADC #$10
        STA $0270
        LDA player_x
        CLC
        ADC #$30
        STA $0273

        ; top right tile (x + 8): for an addition we do  clc then adc
        LDA player_y
        CLC
        ADC #$10
        STA $0274
        LDA player_x
        CLC
        ADC #$38
        STA $0277

        ; bottom left tile (y + 8):
        LDA player_y
        CLC
        ADC #$18
        STA $0278
        LDA player_x
        CLC
        ADC #$30
        STA $027b

        ; bottom right tile (x + 8, y + 8)
        LDA player_y
        CLC
        ADC #$18
        STA $027c
        LDA player_x
        CLC
        ADC #$38
        STA $027f
;p9
        LDA #$29 ;load top left
        STA $0281
        LDA #$2a  ; top right
        STA $0285
        LDA #$39 ; bottom left
        STA $0289
        LDA #$3a ; bottom right
        STA $028d
    ;store model tile attributes
        LDA #$00
        STA $0282
        STA $0286
        STA $028a
        STA $028e

    ;store tile locations
        ; top left tile:
        LDA player_y
        CLC
        ADC #$20
        STA $0280
        LDA player_x
        CLC
        ADC #$0
        STA $0283

        ; top right tile (x + 8): for an addition we do  clc then adc
        LDA player_y
        CLC
        ADC #$20
        STA $0284
        LDA player_x
        CLC
        ADC #$08
        STA $0287

        ; bottom left tile (y + 8):
        LDA player_y
        CLC
        ADC #$28
        STA $0288
        LDA player_x
        CLC
        ADC #$00
        STA $028b

        ; bottom right tile (x + 8, y + 8)
        LDA player_y
        CLC
        ADC #$28
        STA $028c
        LDA player_x
        CLC
        ADC #$08
        STA $028f
;p10
        LDA #$09 ;load top left
        STA $0291
        LDA #$0a  ; top right
        STA $0295
        LDA #$19 ; bottom left
        STA $0299
        LDA #$1a ; bottom right
        STA $029d
    ;store model tile attributes
        LDA #$00
        STA $0292
        STA $0296
        STA $029a
        STA $029e

    ;store tile locations
        ; top left tile:
        LDA player_y
        CLC
        ADC #$20
        STA $0290
        LDA player_x
        CLC
        ADC #$10
        STA $0293

        ; top right tile (x + 8): for an addition we do  clc then adc
        LDA player_y
        CLC
        ADC #$20
        STA $0294
        LDA player_x
        CLC
        ADC #$18
        STA $0297

        ; bottom left tile (y + 8):
        LDA player_y
        CLC
        ADC #$28
        STA $0298
        LDA player_x
        CLC
        ADC #$10
        STA $029b

        ; bottom right tile (x + 8, y + 8)
        LDA player_y
        CLC
        ADC #$28
        STA $029c
        LDA player_x
        CLC
        ADC #$18
        STA $029f
;p11
        LDA #$2d ;load top left
        STA $02a1
        LDA #$2e  ; top right
        STA $02a5
        LDA #$3d ; bottom left
        STA $02a9
        LDA #$3e ; bottom right
        STA $02ad
    ;store model tile attributes
        LDA #$00
        STA $02a2
        STA $02a6
        STA $02aa
        STA $02ae

    ;store tile locations
        ; top left tile:
        LDA player_y
        CLC
        ADC #$20
        STA $02a0
        LDA player_x
        CLC
        ADC #$20
        STA $02a3

        ; top right tile (x + 8): for an addition we do  clc then adc
        LDA player_y
        CLC
        ADC #$20
        STA $02a4
        LDA player_x
        CLC
        ADC #$28
        STA $02a7

        ; bottom left tile (y + 8):
        LDA player_y
        CLC
        ADC #$28
        STA $02a8
        LDA player_x
        CLC
        ADC #$20
        STA $02ab

        ; bottom right tile (x + 8, y + 8)
        LDA player_y
        CLC
        ADC #$28
        STA $02ac
        LDA player_x
        CLC
        ADC #$28
        STA $02af

;p12
        LDA #$49 ;load top left
        STA $02b1
        LDA #$4a  ; top right
        STA $02b5
        LDA #$59 ; bottom left
        STA $02b9
        LDA #$5a ; bottom right
        STA $02bd
    ;store model tile attributes
        LDA #$00
        STA $02b2
        STA $02b6
        STA $02ba
        STA $02be

    ;store tile locations
        ; top left tile:
        LDA player_y
        CLC
        ADC #$20
        STA $02b0
        LDA player_x
        CLC
        ADC #$30
        STA $02b3

        ; top right tile (x + 8): for an addition we do  clc then adc
        LDA player_y
        CLC
        ADC #$20
        STA $02b4
        LDA player_x
        CLC
        ADC #$38
        STA $02b7

        ; bottom left tile (y + 8):
        LDA player_y
        CLC
        ADC #$28
        STA $02b8
        LDA player_x
        CLC
        ADC #$30
        STA $02bb

        ; bottom right tile (x + 8, y + 8)
        LDA player_y
        CLC
        ADC #$28
        STA $02bc
        LDA player_x
        CLC
        ADC #$38
        STA $02bf

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
