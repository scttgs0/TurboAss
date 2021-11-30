language        EQU 1                               ;0=German, 1=English
version         EQU $01071400                       ;The version number (Macro version >= $200000)
etv_exit        EQU $040C                           ;etv_exit()-vector
xbra_id         EQU 'BUG1'
                OPT X-,F-,O+,W+

********************************************************************************
* TurboAss Bugaboo                                                             *
* from Markus Fritze                                                           *
********************************************************************************
                PART 'Header'
                OUTPUT 'BUGABOO.PRG'
                IF ^^SYMTAB
                DEFAULT 1
                ELSE
                DEFAULT 2
                ENDC

                TEXT

anfang:         jsr     init_all

                DATA

                DC.L ^^RANDOM                       ;without function
                DC.L ^^RANDOM                       ;without function
                DC.L $110000                        ;Internal version number of the debugger
                DC.L ^^RANDOM

                DXSET 31,0
                DX.B 'Shareware-Basic version'
                DX.B 'Markus Fritze, Birkhahnkamp 38'
                DX.B '2000 Norderstedt 1'
                EVEN
                ENDPART

********************************************************************************

                PART 'start'
start:          move.l  SP,old_usp(A4)              ;USP notice
                pea     @_trap3(A4)
                move.l  #$050023,-(SP)
                trap    #13                         ;Trap #3 set (Supervisor-Mode an)
                addq.l  #8,SP
                move.l  D0,old_trap3
                movea.l D0,A0                       ;Get old trap # 3 vector
                cmpi.l  #'TASS',-8(A0)              ;Start through the assembler?
                seq     D0                          ;then d0 = -1
                trap    #3                          ;Supervisor mode an
                move.l  SP,old_stack(A4)            ;and remember the stackpointer
                move.b  D0,ass_load(A4)             ;Flag for loading through the assembler
                bne.s   start1                      ;then automatically resident

                st      le_allowed(A4)              ;LE is allowed
                move.l  #$0A000100,D0               ;appl_init()
                bsr     aes
                move.l  #$13000100,D0               ;appl_exit()
                bsr     aes
                move.w  spaced2+32(A4),D2           ;Remember AES version number
                sne     D2                          ;D2 = $FF, If the AES is available

                move.b  do_resident(A4),D1          ;resident-Flag

                movea.l kbshift_adr(A4),A0
                moveq   #4,D0
                and.b   (A0),D0                     ;Control Pressed?
                sne     D0                          ;D0=$FF, if pressed

                eor.b   D0,D1                       ;B = A xor B
                or.b    D2,D0                       ;A = A or C
                not.b   D0                          ;A = not A
                and.b   D2,D1                       ;B = B and C
                or.b    D1,D0                       ;A = not(A or C)or(C and (A xor B))
                move.b  D0,do_resident(A4)
                beq.s   start2                      ;non-resident
                pea     resident_text(PC)
                move.w  #9,-(SP)
                trap    #1                          ;Provide message for resident
                addq.l  #6,SP
start1:         st      do_resident(A4)             ;Automatically resident
start2:         jsr     @init(A4)                   ;initialize everything
                bra.s   start4

resident_text:  DC.B 13,10,'Bugaboo V'
                DC.B (version>>24)&$0F+'0'
                DC.B '.'
                IF (version>>20)&$0F<>0
                DC.B (version>>20)&$0F+'0'
                ENDC
                DC.B (version>>16)&$0F+'0'
                DC.B '.'
                IF (version>>12)&$0F<>0
                DC.B (version>>12)&$0F+'0'
                ENDC
                DC.B (version>>8)&$0F+'0'
                IF version&$FF
                DC.B version&$FF
                ENDC
                DC.B ' resident',13,10,0
                EVEN

;General debugging
newstart:       lea     varbase,A4

                move.l  A0,first_free(A4)           ;1st free address in the RAM
                move.l  A1,quit_stk(A4)             ;Return code
                move.l  A1,D0
                beq.s   newstart1                   ;No return address
                cmpi.l  #$DEADFACE,-(A1)            ;Magic?
                bne.s   newstart1                   ;No!
                move.l  -(A1),cmd_line_adr(A4)      ;Address of the Commandline
                move.l  A1,ass_vector(A4)           ;Remember pointer to jump table
newstart1:      st      le_allowed(A4)              ;LE is allowed
                clr.b   help_allow(A4)
                move.l  A2,prg_base(A4)             ;ADR OF AKT.PRGS
                beq.s   newstart2                   ;auto-load => LE forbidden
                sf      le_allowed(A4)              ;LE is not allowed
                move.b  (A2),help_allow(A4)
newstart2:      clr.l   merk_svar(A4)
                tst.b   help_allow(A4)              ;CTRL-HELP permitted?
                bpl.s   newstart3                   ;No!=>
                move.l  A3,D0
                subq.l  #8,D0
                bmi.s   newstart3                   ;Also in the RAM?
                move.l  A3,merk_svar(A4)
newstart3:      clr.l   end_of_mem(A4)
                trap    #3
                movea.l default_stk(A4),SP          ;is already initialized
                moveq   #-1,D0
                move.l  D0,line_back(A4)
                jsr     @init(A4)

start4:         clr.l   etv_exit.w                  ;etv_exit()-Clear vector
                movea.l act_pd(A4),A0
                movea.l (A0),A0                     ;Pointer to the BasePage of the Act.PRG
                move.l  A0,merk_act_pd(A4)          ;Remember the current program
                lea     128(A0),A0
                move.b  (A0),D0                     ;Is there still a commandline?
                beq.s   start5                      ;No =>
                clr.b   (A0)+                       ;Commandline
                bsr     do_cmdline                  ;Commandline in the input buffer
start5:
                sf      auto_sym(A4)                ;Symbol table through the assembler?
                st      autodo_flag(A4)             ;Ctrl + M command

                lea     gemdos_break(A4),A0
                lea     end_of_breaks(A4),A1
start6:         tst.b   (A0)                        ;Cancel at the trap?
                sne     (A0)+                       ;then put flag
                cmpa.l  A1,A0
                blo.s   start6

                suba.l  A1,A1
                movea.l $2C.w,A0                    ;Holen Vektor Chandor
                move.l  A0,D7
                btst    #0,D7                       ;English VOBIS TOS (85)
                bne.s   start8                      ;=> out
                lea     20(A0),A0
                cmpi.w  #$207C,(A0)+                ;moveL #,a0 ?
                bne.s   start8
                movea.l (A0),A1                     ;Get the base address of the table
                move.l  A1,linef_base(A4)
                subq.l  #4,A1
                moveq   #-4,D7
start7:         addq.l  #4,A1
                addq.w  #4,D7
                tst.b   (A1)
                beq.s   start7
                move.w  D7,max_linef(A4)            ;Maximum permitted line F-opcode
start8:

                movea.l #sekbuff,A0                 ;set internal ssp
                adda.l  A4,A0
                movea.l A0,SP
                move.l  A0,default_stk(A4)          ;and remember

                bsr     initreg                     ;Initialize Register
                jsr     set_reg                     ;tracebackBufferInitialisieren
                tst.b   ass_load(A4)                ;Loading through the assembler?
                bne     cmd_resident2               ;automatically resident
                jsr     @set_ins_flag(A4)           ;Insert/Overwrite Show
                jsr     @redraw_all(A4)             ;Rebuild screen
f_direct:       move.l  first_free(A4),default_adr(A4) ;1st free address in the RAM
                st      testwrd(A4)                 ;Rediren output according to A0
                moveq   #'1',D0
                add.b   prozessor(A4),D0            ;Insert processor
                move.b  D0,__star2
                sf      testwrd(A4)                 ;Output again on the screen
                jsr     @c_clrhome(A4)              ;Clear screen
                pea     __star(PC)                  ;Text of the start message
                jsr     @print_line(A4)             ;output
                tst.b   install_load(A4)
                beq.s   initvi2
                pea     __star4(PC)
                jsr     @print_line(A4)             ;Installation was loaded
initvi2:        sf      install_load(A4)
                jsr     @c_eol(A4)                  ;delete a line
                jsr     @crout(A4)                  ;Spend
                jsr     @c_eol(A4)                  ;delete a line
                move.l  trace_pos(A4),reg_pos(A4)
all_normal:     lea     main_loop(PC),A0
                move.l  A0,jmpdispa(A4)             ;Jump dispatcher on main loop
                jmp     (A4)

                SWITCH language
                CASE 0
__star:         DC.B " ∑-Soft's Bugaboo V"
                DC.B (version>>24)&$0F+'0'
                DC.B '.'
                IF (version>>20)&$0F<>0
                DC.B (version>>20)&$0F+'0'
                ENDC
                DC.B (version>>16)&$0F+'0'
                DC.B '.'
                IF (version>>12)&$0F<>0
                DC.B (version>>12)&$0F+'0'
                ENDC
                DC.B (version>>8)&$0F+'0'
                IF version&$FF
                DC.B version&$FF
                ENDC
                DC.B ' von Markus Fritze und Sӧren Hellwig',13
                DC.B ' Ein 680'
__star2:        DC.B '00 Prozessor ist aktiv.',13,0
__star4:        DC.B ' Parameter wurden geladen.',13,0

                CASE 1
__star:         DC.B ' Bugaboo V'
                DC.B (version>>24)&$0F+'0'
                DC.B '.'
                IF (version>>20)&$0F<>0
                DC.B (version>>20)&$0F+'0'
                ENDC
                DC.B (version>>16)&$0F+'0'
                DC.B '.'
                IF (version>>12)&$0F<>0
                DC.B (version>>12)&$0F+'0'
                ENDC
                DC.B (version>>8)&$0F+'0'
                IF version&$FF
                DC.B version&$FF
                ENDC
                DC.B ' by ∑-Soft',13
                DC.B ' from Markus Fritze and Sӧren Hellwig',13
                DC.B ' 680'
__star2:        DC.B '00 Processor is active',13,0
__star4:        DC.B ' parameter-file loaded.',13,0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* Jump distributor of the functions                                            *
********************************************************************************
                PART 'inp_loop'
ret_jump:       lea     varbase,A4
                movea.l default_stk(A4),SP          ;Take back stackpointer
                jsr     @my_driver(A4)              ;Own drivers pure (for CLR 8.4ff)
                jsr     set_reg
                moveq   #14,D0
                jsr     disable_irq                 ;Ring indicator
                move.b  #'$',hexbase
                clr.l   trace_count(A4)             ;Delete TraceCount
                moveq   #0,D0
                jsr     graf_mouse                  ;Turn on arrow
                andi.b  #$10,kbshift(A4)
                clr.b   direct(A4)
                lea     screen+1920(A4),A0
                jsr     @get_line(A4)               ;Evaluate the last line
                tst.b   D0                          ;Is something in the last line?
                beq.s   ret_jump1                   ;No! =>
                tst.b   ignore_autocrlf(A4)         ;CR/LF suppress?
                bne.s   ret_jump1                   ;Yes!=>
                jsr     @crout(A4)                  ;CR/LF
                jsr     @c_eol(A4)
ret_jump1:      andi    #$FB00,SR                   ;IRQs release
                move.l  jmpdispa(A4),-(SP)
                rts
                ENDPART

********************************************************************************
* The main loop                                                                *
********************************************************************************
                PART 'main_loop'
main_loop:      jsr     @page1(A4)                  ;debugger screen
                sf      untrace_flag(A4)            ;No more untrace
                clr.l   untrace_count(A4)           ;Delete untracecounter
                clr.b   device(A4)                  ;No printer edition
                sf      testwrd(A4)                 ;Output to the screen (not according to A0)
                clr.l   breakpnt+12*16(A4)          ;Break#16 Clear
                move.l  default_adr(A4),D1
                jsr     @anf_adr(A4)
                tst.b   assm_flag(A4)               ;Input by the line assembler?
                beq.s   main_loop2                  ;No!
                lea     _zeile2(A4),A0              ;UNDO-Buffer
                jsr     @get_line(A4)               ;Evaluate line
                cmp.b   #'|',D0
                beq.s   main_loop1                  ;Do command?
                cmp.b   #'!',D0
                bne.s   main_loop2                  ;Line-Assembler?
main_loop1:     jsr     @chrout(A4)                 ;Output automatically
                sf      assm_flag(A4)               ;End entry with the line assembler
main_loop2:     tst.b   illegal_flg(A4)             ;CTRL+Cursor Left for illegal
                beq.s   main_loop3
                sf      illegal_flg(A4)
                jsr     @cache_up(A4)               ;and a entry in the cache back
                jsr     @cursor_off(A4)             ;Switch off the cursor again
main_loop3:     move.l  $04BA.w,D0
                move.l  hz200_time(A4),D1
                cmp.l   D1,D0
                bhs.s   main_loop4                  ;Prevent timer underflow (hard disk!)
                move.l  D1,D0
                move.l  D0,$04BA.w
main_loop4:     move.l  D0,hz200_time(A4)
                bsr     rgout                       ;Spend the tab
                jsr     @desel_menu(A4)             ;Possibly selected menu item deselect
                tst.l   prg_base(A4)                ;Automatically load PRG
                bne     autoload
                tst.b   do_resident(A4)             ;Run 'Resident' automatically?
                bne     cmd_resident2               ;=> Auto folder version
                clr.b   akt_maust(A4)
                clr.b   maus_merk(A4)               ;Mouse buttons are not pressed!
                clr.b   maus_merk2(A4)
                move.w  #-1,maus_flag(A4)           ;Reset flag again
                st      mausprell(A4)
                st      mausprell2(A4)
                clr.b   maustast(A4)
main_loop5:     st      first_call(A4)
                tst.b   autodo_flag(A4)
                bne     autodo                      ;Ctrl + M command Automatically execute
                bclr    #0,help_allow(A4)           ;Bit 0: DIRECTION
                beq.s   main_loop6                  ;no =>
                jmp     cmd_go                      ;DIRECTION =>
main_loop6:     tst.b   fast_exit(A4)
                beq.s   main_loop7                  ;<>0 => Immediately with Ctrl + Help out
                jmp     do_help

main_loop7:     move.l  input_pnt(A4),D0            ;BREAKPT-Directive?
                beq.s   call_scr_edit               ;No! => Screen-Editor
                movea.l D0,A0
                moveq   #':',D1                     ;the line separator
main_loop71:    move.b  (A0)+,D0                    ;Anything in the buffer?
                beq.s   call_scr_edit               ;No!=> Screen editor
                cmp.b   #' ',D0                     ;Ignore leadership paces
                beq.s   main_loop71
                cmp.b   D1,D0                       ;Line separator at the beginning?
                beq.s   main_loop71                 ;Yes!=> Ignore
                lea     _zeile(A4),A1
                moveq   #0,D2                       ;Flag for quotation marks
                bra.s   main_loop83
main_loop8:     move.b  (A0)+,D0
                beq.s   main_loop9                  ;Stringendo =>
main_loop83:    cmp.b   #'"',D0                     ;Quotation marks?
                bne.s   main_loop81                 ;No!=>
                not.b   D2                          ;Flag toog
main_loop81:    tst.b   D2                          ;Within quotes?
                bne.s   main_loop82                 ;Yes!=> Not to ':' testing
                cmp.b   D1,D0                       ;Line separator?
                bne.s   main_loop82                 ;Yes!=>
                cmp.b   (A0),D1                     ;Another line separator?
                bne.s   main_loop10                 ;No => End of the input
                bra.s   main_loop8
main_loop82:    move.b  D0,(A1)+                    ;Copy into the input busher
                bra.s   main_loop8
main_loop9:     suba.l  A0,A0                       ;Delete flag for it
main_loop10:    clr.b   (A1)                        ;Complete entry bushing
                move.l  A0,input_pnt(A4)
                cmpi.b  #'-',(A0)                   ;Follow a "-"?
                bne.s   main_loop11                 ;No =>
                addq.l  #1,input_pnt(A4)
                bra.s   main_loop13                 ;Then spend nothing!
main_loop11:    pea     _zeile(A4)
                jsr     @print_line(A4)
                jsr     @crout(A4)                  ;Show command
                bra.s   main_loop13                 ;and evaluate

call_scr_edit:  clr.l   input_pnt(A4)               ;Reset batch pointer
                sf      batch_flag(A4)              ;Batch mode

                jsr     @scr_edit(A4)               ;Wait for input

main_loop13:    lea     _zeile(A4),A0
inp_loop1:      bsr     get
                tst.b   D0
                beq     ret_jump                    ;Empty input
                cmp.b   #'0',D0
                blo.s   inp_loop2                   ;nix
                cmp.b   #'9',D0                     ;Sign a number?
                bls.s   inp_loop3                   ;yes
inp_loop2:      bsr     numbas                      ;Evaluate number basis
                bmi.s   inp_loop4                   ;No, no number!
                bsr     get
inp_loop3:      bsr     get_zahl                    ;Read the number
                move.l  D1,default_adr(A4)          ;New Default address
inp_loop4:      cmp.b   #'>',D0
                beq.s   inp_loop5
                cmp.b   #'0',D0                     ;Ignore prompt!
                beq.s   inp_loop5
                cmp.b   #'',D0
                bne.s   inp_loop6
inp_loop5:      bsr     get                         ;PC marker
inp_loop6:      tst.b   D0
                beq     ret_jump
                subq.l  #1,A0
                movea.l A0,A6
                movea.l A0,A5
                lea     cmdtab(PC),A1
                lea     cmdadr-2(PC),A2
inp_loop7:      addq.l  #2,A2
                movea.l A6,A0
inp_loop8:      move.b  (A0),D0
                cmp.b   #' ',D0                     ;Space
                beq.s   inp_loop11
                cmpa.l  A0,A5                       ;1.Sign?
                beq.s   inp_loop9                   ;Yes! =>
                cmp.b   #'A',D0                     ;Point only from the 2nd place test
                blo.s   inp_loop11
                cmp.b   #'Z',D0
                bls.s   inp_loop9
                cmp.b   #'a',D0                     ;Special characters: e.g. M ^ A0
                blo.s   inp_loop11
                cmp.b   #'z',D0
                bhi.s   inp_loop11
inp_loop9:      tst.b   D0                          ;Line end
                beq.s   inp_loop11
                tst.b   (A1)                        ;Command end
                beq.s   inp_loop11
                bmi     no_bef
                bsr     get
                cmp.b   (A1)+,D0
                beq.s   inp_loop8
inp_loop10:     tst.b   (A1)+
                bne.s   inp_loop10
                bra.s   inp_loop7
inp_loop11:     moveq   #0,D1
                move.w  (A2),D1                     ;unsigned word
                adda.l  D1,A2                       ;Determine the address of the routine
                IFEQ ^^SYMTAB
                lea     intern_bus,A6
                move.l  A6,8.w                      ;Intercept internal bus errors
                ENDC
                clr.b   direct(A4)
                jmp     (A2)
                ENDPART

********************************************************************************

                PART 'cmdtab'
cmdtab:         DC.B ' ',0                          ;Dummy, will be descended
                DC.B '&',0
                DC.B '#',0
                DC.B '@',0
                DC.B '!',0
                DC.B '|',0
                DC.B '?',0
                DC.B '/',0
                DC.B ')',0
                DC.B ']',0
                DC.B '.',0
                DC.B ',',0
                DC.B $22,0
                DC.B 'PRN',0
                DC.B 'P',0
                DC.B 'BREAKPOINTS',0
                DC.B 'SAVE',0
                DC.B 'SYMBOLTABLE',0
                DC.B 'SYSINFO',0
                DC.B 'SYSTEM',0
                DC.B 'SET',0
                DC.B 'MEMORY',0
                DC.B 'LIST',0
                DC.B 'LL',0
                DC.B 'DISASSEMBLE',0
                DC.B 'DUMP',0
                DC.B 'LEXECUTE',0
                DC.B 'LOAD',0
                DC.B 'GO',0
                DC.B 'UNTRACE',0
                DC.B 'INFO',0
                DC.B 'TRACE',0
                DC.B 'CALL',0
                DC.B 'IF',0
                DC.B 'MOVE',0
                DC.B 'COMPARE',0
                DC.B 'COPY',0
                DC.B 'DIRECTORY',0
                DC.B 'HUNT',0
                DC.B 'FIND',0
                DC.B 'FILL',0
                DC.B 'CLS',0
                DC.B 'ASCII',0
                DC.B 'ASCFIND',0
                DC.B 'LET',0
                DC.B 'EXIT',0
                DC.B 'QUIT',0
                DC.B 'TYPE',0
                DC.B 'SHOWMEMORY',0
                DC.B 'MOUSEON',0
                DC.B 'MON',0
                DC.B 'SHOWMOUSE',0
                DC.B 'MOUSEOFF',0
                DC.B 'MOFF',0
                DC.B 'HIDEMOUSE',0
                DC.B 'READSEKTOR',0
                DC.B 'RSEKTOR',0
                DC.B 'WRITESEKTOR',0
                DC.B 'WSEKTOR',0
                DC.B 'READSECTOR',0
                DC.B 'RSECTOR',0
                DC.B 'WRITESECTOR',0
                DC.B 'WSECTOR',0
                DC.B 'READTRACK',0
                DC.B 'RTRACK',0
                DC.B 'ERASE',0
                DC.B 'KILL',0
                DC.B 'FREE',0
                DC.B 'MKDIRECTORY',0
                DC.B 'RMDIRECTORY',0
                DC.B 'NAME',0
                DC.B 'FORMAT',0
                DC.B 'GETREGISTER',0
                DC.B 'LINE',0
                DC.B 'CR',0
                DC.B 'FOPEN',0
                DC.B 'FCLOSE',0
                DC.B 'CLR',0
                DC.B 'CACHECLR',0
                DC.B 'CACHEGET',0
                DC.B 'RESET',0
                DC.B 'CHECKSUMME',0
                DC.B 'FILE',0
                DC.B 'SWITCH',0
                DC.B 'RESIDENT',0
                DC.B 'CURSOR',0
                DC.B 'INITREGISTER',0
                DC.B 'BSSCLEAR',0
                DC.B 'OBSERVE',0
                DC.B 'DO',0
                DC.B 'SYNC',0
                DC.B 'RWABS',0
                DC.B 'CONTINUE',0
                DC.B 'FATTRIBUT',0
                DC.B 'LABELBASE',0
                DC.B 'HELP',0
                DC.B 'READFDC',0
                DC.B 'COOKIE',0
                DC.B 'OVERSCAN',0
                DC.B 'B',0
                DC.B 'F',0
                DC.B '~',0
                DC.B -1
                EVEN
                OPT W-
                BASE DC.W,*
cmdadr:         DC.W ret_jump
                DC.W cmd_und                        ;&
                DC.W cmd_number                     ;#
                DC.W cmd_atsign                     ;@
                DC.W cmd_assem                      ;!
                DC.W cmd_dobef                      ;|
                DC.W cmd_calc                       ;?
                DC.W cmd_dchng                      ;/
                DC.W cmd_achng                      ;)
                DC.W cmd_schng                      ;]
                DC.W cmd_chng                       ;.
                DC.W cmd_mchng                      ;,
                DC.W cmd_send                       ;"
                DC.W cmd_prnt                       ;PRN
                DC.W cmd_prnt                       ;P
                DC.W cmd_bkpt                       ;BREAKPOINTS
                DC.W cmd_save                       ;SAVE
                DC.W cmd_symbol                     ;SYMBOLTABLE
                DC.W cmd_sysinfo                    ;SYSINFO
                DC.W cmd_exit                       ;SYSTEM
                DC.W cmd_set                        ;SET
                DC.W cmd_dump                       ;MEMORY
                DC.W cmd_list                       ;LIST
                DC.W cmd_listf                      ;LIST+
                DC.W cmd_disass                     ;DISASSEMBLE
                DC.W cmd_dump                       ;DUMP
                DC.W cmd_lexec                      ;LEXEC
                DC.W cmd_load                       ;LOAD
                DC.W cmd_go                         ;GO
                DC.W cmd_untrace                    ;UNTRACE
                DC.W cmd_info                       ;INFO
                DC.W cmd_trace                      ;TRACE
                DC.W cmd_call                       ;CALL
                DC.W cmd_if                         ;IF
                DC.W cmd_move                       ;MOVE

                DC.W cmd_compare                    ;COMPARE
                DC.W cmd_move                       ;COPY
                DC.W cmd_dir                        ;DIRECTORY
                DC.W cmd_hunt                       ;HUNT
                DC.W cmd_find                       ;FIND
                DC.W cmd_fill                       ;FILL
                DC.W cmd_cls                        ;CLS
                DC.W cmd_asc                        ;ASCII
                DC.W cmd_findasc                    ;ASCFIND
                DC.W cmd_set                        ;LET
                DC.W cmd_exit                       ;EXIT
                DC.W cmd_exit                       ;QUIT
                DC.W cmd_type                       ;TYPE
                DC.W cmd_showmem                    ;SHOWMEMORY
                DC.W cmd_mon                        ;MOUSEON
                DC.W cmd_mon                        ;MON
                DC.W cmd_mon                        ;SHOWM
                DC.W cmd_moff                       ;MOUSEOFF
                DC.W cmd_moff                       ;MOFF
                DC.W cmd_moff                       ;HIDEM
                DC.W cmd_dread                      ;READSEKTOR
                DC.W cmd_dread                      ;RSEKTOR
                DC.W cmd_dwrite                     ;WRITESEKTOR
                DC.W cmd_dwrite                     ;WSEKTOR
                DC.W cmd_dread                      ;READSECTOR
                DC.W cmd_dread                      ;RSECTOR
                DC.W cmd_dwrite                     ;WRITESECTOR
                DC.W cmd_dwrite                     ;WSECTOR
                DC.W cmd_rtrack                     ;READTRACK
                DC.W cmd_rtrack                     ;RTRACK
                DC.W cmd_erase                      ;KILL
                DC.W cmd_erase                      ;ERASE
                DC.W cmd_free                       ;FREE
                DC.W cmd_mkdir                      ;MKDIR
                DC.W cmd_rmdir                      ;RMDIR
                DC.W cmd_name                       ;NAME
                DC.W cmd_format                     ;FORMAT
                DC.W cmd_getreg                     ;GETREGISTER
                DC.W cmd_line                       ;LINE
                DC.W cmd_crout                      ;CR
                DC.W cmd_fopen                      ;FOPEN
                DC.W cmd_fclose                     ;FCLOSE
                DC.W cmd_clr                        ;CLR
                DC.W cmd_clrcach                    ;CACHECLR
                DC.W cmd_getcach                    ;CACHEGET
                DC.W cmd_reset                      ;RESET
                DC.W cmd_checksum                   ;CHECKSUMME
                DC.W cmd_file                       ;FILE
                DC.W cmd_switch                     ;SWITCH
                DC.W cmd_resident                   ;RESIDENT
                DC.W cmd_swchcur                    ;CURSOR
                DC.W cmd_ireg                       ;INITREGISTER
                DC.W cmd_bclr                       ;BSSCLEAR
                DC.W cmd_obser                      ;OBSERVE
                DC.W cmd_do                         ;DO
                DC.W cmd_sync                       ;SYNC
                DC.W cmd_rwabs                      ;RWABS
                DC.W cmd_cont                       ;CONTINUE
                DC.W cmd_fattrib                    ;FATTRIBUT
                DC.W cmd_labelbase                  ;LABELBASE
                DC.W cmd_help                       ;HELP
                DC.W cmd_fdc                        ;READFDC
                DC.W cmd_cookie                     ;COOKIE
                DC.W cmd_overscan                   ;OVERSCAN
                DC.W cmd_bkpt                       ;B
                DC.W cmd_file                       ;F
                DC.W cmd_set                        ; ~
                ENDPART

********************************************************************************
* Spring bar of the menu functions                                              *
********************************************************************************
                PART 'f_jumps'
                BASE DC.W,f_jumps
f_jumps:        DC.W f_trace                        ;F1    - Trace (Fast Traps)
                DC.W f_do_pc                        ;F2    - Do PC
                DC.W f_trarts                       ;F3    - Trace until RTS
                DC.W f_traall                       ;F4    - Trace all
                DC.W f_skip                         ;F5    - Skip
                DC.W f_dir                          ;F6    - Directory
                DC.W f_hexdump                      ;F7    - Hexdump
                DC.W f_disass                       ;F8    - Disassemble
                DC.W f_list                         ;F9    - List
                DC.W f_switch                       ;F10   - Switch Screen
                DC.W f_68020emu                     ;S+F1  - 68020 Emulator (for Trace)
                DC.W f_trasub                       ;S+F2  - Don't trace Subroutine
                DC.W f_trarte                       ;S+F3  - Trace until RTE/RTR
                DC.W go_pc                          ;S+F4  - Go
                DC.W f_togmode                      ;S+F9  - Overwrite/Insert
                DC.W f_marker                       ;S+F6  - Marker
                DC.W f_break                        ;S+F7  - Show Breakpoints
                DC.W f_info                         ;S+F8  - Info
                DC.W f_direct                       ;S+F5  - Direct
                DC.W f_quit                         ;S+F10 - Quit
                OPT W+
                ENDPART

********************************************************************************
* S+F6 - Show marker                                                           *
********************************************************************************
                PART 'f_marker'
f_marker:       movea.l #allg_buffer,A0
                adda.l  A4,A0
                lea     mark_va(PC),A1
                moveq   #'1',D1
                moveq   #9,D0
f_marker1:      move.l  A0,(A1)+                    ;RSC-Build texts in the buffer
                addq.l  #6,A1
                move.b  #'M',(A0)+
                move.b  D1,(A0)+
                move.b  #':',(A0)+
                move.b  #'$',(A0)+
                moveq   #15,D2
f_marker3:      move.b  #' ',(A0)+
                dbra    D2,f_marker3
                clr.b   (A0)+
                addq.w  #1,D1
                cmp.w   #':',D1
                bne.s   f_marker2
                moveq   #'0',D1
f_marker2:      dbra    D0,f_marker1

                st      testwrd(A4)
                movea.l basep(A4),A0                ;Major address
                move.l  8(A0),D2                    ;Initial ID of the TEXT segment
                move.l  $18(A0),D3                  ;Initial ID of the BSS segment
                add.l   $1C(A0),D3                  ;+ Long des BSS segments
                lea     simple_vars(A4),A5
                lea     mark_va(PC),A2
                lea     mark_vb(PC),A3
                moveq   #9,D7
f_mark1:        movea.l (A2)+,A0                    ;Get address of the string
                addq.l  #4,A0
                move.l  (A5)+,D1
                bsr     hexlout                     ;Use variable value
                addq.l  #1,A0
                moveq   #4,D0
f_mark2:        move.b  #'?',(A0)+                  ;Line number unknown
                dbra    D0,f_mark2
                lea     marker_25(PC),A6            ;Default-Symbol = " "
                cmp.l   D2,D1
                blo.s   f_mark3                     ;<TEXT-Segment
                cmp.l   D3,D1
                bhs.s   f_mark3                     ;>BSS-Segment
                tst.l   sym_size(A4)                ;Symbol table at all?
                beq.s   f_mark3                     ;No symbols there!
                andi.w  #$FFEF,4(A3)                ;Light!
                bsr     hunt_symbol
                bne.s   f_mark8
                ori.w   #$10,4(A3)                  ;light An
f_mark8:        movea.l (A1),A6                     ;Get symbol name address
f_mark3:        move.l  A6,(A3)+                    ;Insert address
                addq.l  #6,A3
                addq.l  #6,A2
                dbra    D7,f_mark1
                move.l  merk_svar(A4),D0
                beq.s   f_mark6                     ;No transfer by the assembler
                movea.l D0,A1
                moveq   #9,D7
                lea     mark_va(PC),A2
f_mark4:        movea.l (A2)+,A0                    ;Get address of the string
                lea     12(A0),A0                   ;Pointer to the line number
                moveq   #0,D1
                move.w  (A1)+,D1
                addq.w  #1,D1
                beq.s   f_mark5                     ;Line number -1 is illegal
                subq.w  #1,D1
                moveq   #5,D4
                bsr     dezw_out                    ;Insert line number
f_mark5:        addq.l  #6,A2
                addq.l  #4,A1
                dbra    D7,f_mark4
f_mark6:        sf      testwrd(A4)
                lea     marker_rsc(PC),A0
                jsr     @form_do(A4)
                subq.w  #2,D0
                bmi.s   f_mark7
                lsl.l   #2,D0
                lea     simple_vars(A4),A0
                move.l  0(A0,D0.w),D1               ;Get variable value
                jsr     do_dopp                     ;Run "Double click"
f_mark7:        rts

marker_rsc:     DC.W 0,0,49,15,1
                DC.W 5,4
mark_va:        DC.L 0
                DC.W 8
                DC.W 5,5
                DC.L 0
                DC.W 8
                DC.W 5,6
                DC.L 0
                DC.W 8
                DC.W 5,7
                DC.L 0
                DC.W 8
                DC.W 5,8
                DC.L 0
                DC.W 8
                DC.W 5,9
                DC.L 0
                DC.W 8
                DC.W 5,10
                DC.L 0
                DC.W 8
                DC.W 5,11
                DC.L 0
                DC.W 8
                DC.W 5,12
                DC.L 0
                DC.W 8
                DC.W 5,13
                DC.L 0
                DC.W 8

                DC.W 25,4
mark_vb:        DC.L 0                              ;Insert label addresses
                DC.W 8
                DC.W 25,5
                DC.L 0                              ;If none defines "marker_25"
                DC.W 8
                DC.W 25,6
                DC.L 0
                DC.W 8
                DC.W 25,7
                DC.L 0
                DC.W 8
                DC.W 25,8
                DC.L 0
                DC.W 8
                DC.W 25,9
                DC.L 0
                DC.W 8
                DC.W 25,10
                DC.L 0
                DC.W 8
                DC.W 25,11
                DC.L 0
                DC.W 8
                DC.W 25,12
                DC.L 0
                DC.W 8
                DC.W 25,13
                DC.L 0
                DC.W 8

                DC.W 40,1
                DC.L marker_13
                DC.W $26

                DC.W 1,4
                DC.L marker_25                      ;The buttons
                DC.W $24
                DC.W 3,5
                DC.L marker_25
                DC.W $24
                DC.W 1,6

                DC.L marker_25
                DC.W $24
                DC.W 3,7
                DC.L marker_25
                DC.W $24
                DC.W 1,8
                DC.L marker_25
                DC.W $24
                DC.W 3,9
                DC.L marker_25
                DC.W $24
                DC.W 1,10
                DC.L marker_25
                DC.W $24
                DC.W 3,11
                DC.L marker_25
                DC.W $24
                DC.W 1,12
                DC.L marker_25
                DC.W $24
                DC.W 3,13
                DC.L marker_25
                DC.W $24

                DC.W 9,3
                DC.L marker_10
                DC.W 8
                DC.W 18,3
                DC.L marker_11
                DC.W 8
                DC.W 32,3
                DC.L marker_12
                DC.W 8
                DC.W 15,1
                DC.L marker_24
                DC.W 8
                DC.W -1

marker_25:      DC.B ' ',0

                SWITCH language
                CASE 0
marker_10:      DC.B 'Adresse:',0
marker_11:      DC.B 'Zeile:',0
marker_12:      DC.B 'Labelname:',0
marker_24:      DC.B 'Markerliste:',0
marker_13:      DC.B '  OK  ',0

                CASE 1
marker_10:      DC.B 'Address:',0
marker_11:      DC.B 'Line:',0
marker_12:      DC.B 'Label name:',0
marker_24:      DC.B 'Marker',0
marker_13:      DC.B '  OK  ',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* S+F7 - Show Breakpoints                                                      *
********************************************************************************
                PART 'f_break'
f_break:        lea     breakpnt(A4),A2
                lea     cond_breaks(A4),A3
                lea     break_rsc_base(PC),A5
                movea.l #allg_buffer,A0
                adda.l  A4,A0
                st      testwrd(A4)
                moveq   #15,D7
f_brea1:        move.l  A0,(A5)+                    ;Insert address
                addq.l  #6,A5
                lea     42(A0),A1
                move.b  #'B',(A0)+
                move.b  #'0',(A0)+
                move.w  D7,D0
                neg.w   D0
                add.w   #15+'0',D0
                cmp.b   #'9',D0
                bls.s   f_brea10
                addq.w  #7,D0
f_brea10:       move.b  D0,(A0)+
                move.b  #'=',(A0)+
                move.b  #'$',(A0)+
                move.l  (A2)+,D1                    ;breakpoint addr
                bne.s   f_brea2
                moveq   #7,D0
f_brea4:        move.b  #'0',(A0)+                  ;Breakpoint not set
                dbra    D0,f_brea4
                addq.l  #8,A2
                bra.s   f_brea3
f_brea2:        bsr     hexlout
                move.w  (A2)+,D1                    ;breakpoint type
                move.l  (A2),D2                     ;Counter, ...
                addq.l  #6,A2                       ;Now shows the next breakpoint
                subq.w  #1,D1
                beq.s   f_brea5
                bcs.s   f_brea6
                bmi.s   f_brea7
                move.b  #',',(A0)+
                move.b  #'?',(A0)+
                move.l  A3,-(SP)
                moveq   #27,D0                      ;max. 28 characters output
f_brea9:        move.b  (A3)+,(A0)+
                dbeq    D0,f_brea9
                movea.l (SP)+,A3
                bra.s   f_brea3
f_brea7:        tst.l   D2
                bls.s   f_brea3
                move.b  #',',(A0)+
                bra.s   f_brea8
f_brea6:        move.b  #',',(A0)+
                move.b  #'=',(A0)+
f_brea8:        move.l  D2,D1
                bsr     dezout                      ;Output decimal number
                bra.s   f_brea3
f_brea5:        move.b  #',',(A0)+
                move.b  #'*',(A0)+
f_brea3:        clr.b   (A0)
                lea     80(A3),A3
                movea.l A1,A0
                dbra    D7,f_brea1
                sf      testwrd(A4)
                lea     break_rsc(PC),A0
                jmp     @form_do(A4)

break_rsc:      DC.W 0,0,44,20,1
                DC.W 18,18
                DC.L ok_button
                DC.W $26
                DC.W 1,1
break_rsc_base: DC.L 0
                DC.W 8
                DC.W 1,2
                DC.L 0
                DC.W 8
                DC.W 1,3
                DC.L 0
                DC.W 8
                DC.W 1,4
                DC.L 0
                DC.W 8
                DC.W 1,5
                DC.L 0
                DC.W 8
                DC.W 1,6
                DC.L 0
                DC.W 8
                DC.W 1,7
                DC.L 0
                DC.W 8
                DC.W 1,8
                DC.L 0
                DC.W 8
                DC.W 1,9
                DC.L 0
                DC.W 8
                DC.W 1,10
                DC.L 0
                DC.W 8
                DC.W 1,11
                DC.L 0
                DC.W 8
                DC.W 1,12
                DC.L 0
                DC.W 8
                DC.W 1,13
                DC.L 0
                DC.W 8
                DC.W 1,14
                DC.L 0
                DC.W 8
                DC.W 1,15
                DC.L 0
                DC.W 8
                DC.W 1,16
                DC.L 0
                DC.W 8
                DC.W -1
                ENDPART

********************************************************************************
* F1 - Trace                                                                   *
********************************************************************************
                PART 'f_trace'
f_trace:        bsr     init_trace
                bra     do_trace                    ;execute order
f_trac1:        bsr     exit_trace
f_trac2:        move.w  trace_delay(A4),D0
f_trac5:        move    #0,CCR
                dbra    D0,f_trac5                  ;Trace delay
                clr.l   merk_pc(A4)
                jsr     hunt_pc                     ;Screen built & PC on the screen?
                move.w  D7,-(SP)
                bpl.s   f_trac4                     ;then do not release
                clr.w   (SP)                        ;Cursor in line 0
                move.w  trace_flag(A4),D0           ;List or disassemble
                subq.w  #1,D0
                bmi.s   f_trac3                     ;=0 => List
                beq.s   f_trac6                     ;=1 => Disassemble

                tst.l   ass_vector(A4)              ;Assemble da?
                beq.s   f_trac3                     ;No!=> Then lists
                bsr     f_dir                       ;sourceList
                bra.s   f_trac4
f_trac6:        bsr     f_disass                    ;Disassemble from PC
                bra.s   f_trac4
f_trac3:        bsr     f_list                      ;abPcListen
f_trac4:        move.w  (SP)+,zeile(A4)
                clr.w   spalte(A4)
                move.l  _pc(A4),default_adr(A4)
                bra     all_normal                  ;That's it
                ENDPART

********************************************************************************
* S+F2 - Dont trace Subroutine                                                *
********************************************************************************
                PART 'f_trasub'
f_trasub:       movea.l _pc(A4),A6
                move.b  (A6),D0
                cmp.b   #$61,D0
                beq.s   f_do_pc                     ;Execute BSR
                move.w  (A6),D0                     ;to tracender oncode
                and.w   #$FFC0,D0
                cmp.w   #$4E80,D0
                beq.s   f_do_pc                     ;Execute
                bra.s   f_trace                     ;BEFEHL Trace
                ENDPART

********************************************************************************
* F2 - Do PC                                                                   *
********************************************************************************
                PART 'f_do_pc'
f_do_pc:        lea     f_trac2(PC),A0
                move.l  A0,jmpdispa(A4)             ;Return
                bsr     in_trace_buff               ;registerInDenTraceBuffer
                bra     cmd_call1                   ;Run next command
                ENDPART

********************************************************************************
* F3 - Trace until RTS   Shift+F3 - Trace until RTE/R                          *
********************************************************************************
                PART 'f_trarts/e'
f_trarte:       moveq   #2,D7                       ;Stackoffset for RTE / RTR
                bra.s   f_trara
f_trarts:       moveq   #0,D7                       ;No stack offset for RTS
f_trara:        lea     f_trac1(PC),A0
                move.l  A0,jmpdispa(A4)             ;Return
                bsr     in_trace_buff               ;register In Den Trace Buffer
                movea.l _ssp(A4),A0
                btst    #5,_sr(A4)                  ;userOder Supervisor Stack?
                bne.s   f_trar1
                movea.l _usp(A4),A0
f_trar1:        move.l  0(A0,D7.w),merk_stk(A4)
                lea     login_trace,A1
                move.l  A1,0(A0,D7.w)               ;Overwrite the return address
                bra     go_pc                       ;Here we go
                ENDPART

********************************************************************************
* F4 - Trace all                                                               *
********************************************************************************
                PART 'f_traall'
f_traall:       bsr     init_trace
                bsr     do_trace_all                ;execute order
                bra     f_trac1
                ENDPART

********************************************************************************
* F5 - Skip PC                                                                 *
********************************************************************************
                PART 'f_skip'
f_skip:         bsr     in_trace_buff               ;register In Den Trace Buffer
                movea.l _pc(A4),A6                  ;Determine command length on the PC
                jsr     get_dlen                    ;Determine command length
                move.l  A6,_pc(A4)                  ;Set new PC
                bsr     set_reg                     ;and reset
                bra     f_trac2
                ENDPART

********************************************************************************
* F6 - Hexdump                                                                 *
********************************************************************************
                PART 'f_hexdump'
f_hexdump:      movem.l D0-A6,-(SP)
                movea.l reg_pos(A4),A5
                move.l  64(A5),D1                   ;Get current PC
                bclr    #0,D1
                movea.l D1,A6
                move.w  #-1,zeile(A4)               ;Cursor home (s.u.)
                move.w  down_lines(A4),D7
                subq.w  #1,D7
f_hexd0:        move.w  D7,-(SP)
                addq.w  #1,zeile(A4)
                moveq   #0,D3
                bsr     cmd_dump7                   ;Hexdump output
                move.w  (SP)+,D7
                dbra    D7,f_hexd0
                clr.w   zeile(A4)
                move.w  #10,spalte(A4)              ;Cursor in the 1st row
                movem.l (SP)+,D0-A6
                rts
                ENDPART

********************************************************************************
* F7/F8 - List/Disassemble                                                     *
********************************************************************************
                PART 'f_list/disass'
f_list:         st      list_flg(A4)
                clr.w   trace_flag(A4)
                bra.s   f_list0
f_disass:       sf      list_flg(A4)
                move.w  #1,trace_flag(A4)
f_list0:        movem.l D0-A6,-(SP)
                movea.l reg_pos(A4),A5
                move.l  64(A5),D1                   ;Get current PC
                btst    #0,D1
                beq.s   f_disa1
                addq.l  #1,D1
f_disa1:        movea.l D1,A6
                move.w  #-1,zeile(A4)               ;Cursor home (s.u.)
                move.w  down_lines(A4),D7
                subq.w  #1,D7
f_disa0:        move.l  D7,-(SP)
                addq.w  #1,zeile(A4)
                bsr     do_disass
                move.l  (SP)+,D7
                dbra    D7,f_disa0
                clr.w   zeile(A4)
                move.w  #10,spalte(A4)              ;Cursor in the 1st row
                movem.l (SP)+,D0-A6
                sf      list_flg(A4)
                rts
                ENDPART

********************************************************************************
* F9 - Sourcecode-List                                                         *
********************************************************************************
                PART 'f_dir'
f_dir:          tst.l   ass_vector(A4)              ;Assemble?
                beq.s   f_list                      ;No!=> Then lists
                move.w  #2,trace_flag(A4)
                movem.l D0-A6,-(SP)
                movea.l reg_pos(A4),A5
                movea.l 64(A5),A6                   ;Get current PC
                move.l  A6,D0
                addq.l  #1,D0
                and.b   #-2,D0                      ;even
                movea.l D0,A6
                jsr     check_read                  ;Access granted?
                bne.s   src_list1                   ;End, if not
                movea.l basep(A4),A0                ;BasePage of the program to be debugged
                cmp.l   $18(A0),D0                  ;BSS segment ADR achieved?
                bhs.s   src_list2                   ;then end
                sub.l   8(A0),D0                    ;TEXT Segment Start
                bmi.s   src_list2                   ;smaller than text segment => end
                movea.l ass_vector(A4),A5
                jsr     -6(A5)                      ;Offset => line number
                move.w  D0,D6                       ;Remember Line Number
                clr.w   zeile(A4)                   ;cursorHome
                st      testwrd(A4)                 ;Output in the buffer A0
                move.w  down_lines(A4),D7
                subq.w  #1,D7                       ;Number of lines on the screen
src_list0:      move.w  D6,D0                       ;Set line number
                jsr     -18(A5)                     ;Line D0 according to A0
                addq.l  #1,D0
                bne.s   src_list3
                lea     src_list_null(PC),A0        ;Space
                bra.s   src_list4
src_list3:      move.l  A0,-(SP)
                lea     spaced2(A4),A0
                movem.l D0-D7/A1-A6,-(SP)
                move.l  A6,D1
                jsr     @anf_adr(A4)                ;Address at the beginning of the line
                movem.l (SP)+,D0-D7/A1-A6
                movea.l (SP)+,A1
                move.b  #'&',(A0)+                  ;Identification = SourceText Listing
                moveq   #4,D4                       ;5
                moveq   #0,D1
                move.w  D6,D1                       ;Line number
                bsr     dezw_out_b                  ;Spend number
                moveq   #65,D1                      ;Max number to character = 66
src_list1:      move.b  (A1)+,(A0)+                 ;buffer Umkopieren
                dbeq    D1,src_list1
                clr.b   (A0)                        ;Force line end
src_list4:      lea     spaced2(A4),A0
                move.w  zeile(A4),D0
                jsr     write_line                  ;Output the result of the disassembler
                addq.w  #1,D6                       ;Next line
                addq.w  #1,zeile(A4)                ;Line number + 1
                dbra    D7,src_list0                ;Already all lines?
src_list2:      clr.w   zeile(A4)
                move.w  #10,spalte(A4)              ;Cursor in the 1st row
                sf      testwrd(A4)                 ;Output again normal
                movem.l (SP)+,D0-A6
                rts
src_list_null:  DC.B '&',0
                ENDPART

********************************************************************************
* Output lines in D0                                                           *
********************************************************************************
                PART 'src_out'
src_out:        move.l  D0,D7                       ;Remember Line Number
                jsr     -12(A5)                     ;Line number => offset
                movea.l basep(A4),A6
                movea.l 8(A6),A6                    ;Text segment address
                tst.l   D0
                bmi.s   src_out0
                adda.l  D0,A6                       ;+ Text segment address
src_out0:       move.l  D7,D0                       ;Line number back
                jsr     -18(A5)                     ;Line D0 according to A0
                addq.l  #1,D0                       ;Sourcetexts?
                beq.s   src_out1                    ;Yes!=> out
                st      testwrd(A4)                 ;Output in the buffer A0
                move.l  A0,-(SP)
                lea     spaced2(A4),A0
                movem.l D0-D7/A1-A6,-(SP)
                move.l  A6,D1
                jsr     @anf_adr(A4)                ;Address at the beginning of the line
                movem.l (SP)+,D0-D7/A1-A6
                movea.l (SP)+,A1
                move.b  #'&',(A0)+                  ;Identification = SourceText Listing
                moveq   #4,D4                       ;5
                moveq   #0,D1
                move.w  D7,D1                       ;Line number
                bsr     dezw_out_b                  ;Spend number
                moveq   #65,D1                      ;Max number to character = 66
src_out2:       move.b  (A1)+,(A0)+                 ;bufferUmkopieren
                dbeq    D1,src_out2
                clr.b   (A0)                        ;Force line end
                lea     spaced2(A4),A0
                move.w  zeile(A4),D0
                jsr     write_line                  ;Output the result of the disassembler
                sf      testwrd(A4)                 ;Output in the buffer A0
                move    #$FF,CCR                    ;Set z-flag
                rts
src_out1:       move    #0,CCR                      ;Delete Z-Flag
                rts
                ENDPART

********************************************************************************
* F10 - Switch Screen                                                          *
********************************************************************************
                PART 'f_switch'
f_switch:       jsr     @desel_menu(A4)             ;Possibly selected menu item deselect
                lea     debugger_scr(A4),A0
                jsr     check_screen                ;the Debugger-Screen an?
                bne.s   f_switch1                   ;No!=>
                jmp     @page2(A4)                  ;Original Graphics
f_switch1:      jmp     @page1(A4)                  ;debugger screen
                ENDPART

********************************************************************************
* S+F1 - 68020 Emulator                                                        *
********************************************************************************
                PART 'f_68020emu'
f_68020emu:     lea     emu68020(PC),A0
                move.l  A0,$24.w                    ;68020-Trace-vector
                bsr     init_trace
                bsr     do_trace1                   ;Here we go
                bra     f_trac1                     ;and finished with trace

emu68020:       move    #$2700,SR                   ;Please do not disturb ... (IRQS off)
                movem.l D0/A0,-(SP)                 ;Register save
                movea.l 10(SP),A0                   ;Get the PC
                move.w  (A0),D0                     ;Get the command on the PC
                cmp.w   #$4E73,D0                   ;RTE
                beq.s   emu680203
                cmp.w   #$4E75,D0                   ;RTS
                beq.s   emu680203
                cmp.w   #$4E77,D0                   ;RTR
                beq.s   emu680203
                andi.w  #$F0F8,D0                   ;condition &RegisterAusmaskieren
                cmp.w   #$50C8,D0                   ;dBcc
                beq.s   emu680203
                andi.w  #$F000,D0                   ;Make condition & jump
                cmp.w   #$6000,D0                   ;bcc
                beq.s   emu680203
                move.w  (A0),D0                     ;Get the command on the PC again
                andi.w  #$FFF0,D0                   ;Make the trap number
                cmp.w   #$4E40,D0                   ;TRAP
                beq.s   emu680203
                andi.w  #$FFC0,D0                   ;EA first mask
                cmp.w   #$4EC0,D0                   ;JMP
                beq.s   emu680202
                cmp.w   #$4E80,D0                   ;JSR
                beq.s   emu680202
emu680201:      movem.l (SP)+,D0/A0                 ;Register back
                bset    #7,(SP)                     ;Trace again (do not forget)
                rte                                 ;and further
emu680202:      move.w  (A0),D0
                and.w   #%111111,D0                 ;Ea isolands
                cmp.w   #%111011,D0
                bhi.s   emu680201                   ;#, etc (68020) is not allowed
                cmp.w   #%101000,D0                 ;d (a), absolute, etc.=> Abbiech
                bhs.s   emu680203                   ;Here was a mistake => BLS.S !!!
                and.w   #%111000,D0                 ;Insulate mode
                cmp.w   #%10000,D0
                bne.s   emu680201                   ;If not (on) then continue
emu680203:      movem.l (SP)+,D0/A0                 ;Here should now be canceled
                bra     do_trace_excep              ;and finish
                ENDPART

********************************************************************************
* S+F9 - Info                                                                  *
********************************************************************************
                PART 'f_info'
f_info:         lea     info_rsc(PC),A1
                move.w  #10,6(A1)                   ;10 lines high
                move.w  #8,12(A1)                   ;Button in Line 8
                move.w  #-1,info_r1                 ;Baum kürzen
                st      testwrd(A4)
                lea     info_txt1+28(PC),A0
                move.l  basepage(A4),D1
                bsr     hexlout
                lea     info_txtx+28(PC),A0
                move.l  end_adr(A4),D1
                bsr     hexlout
                lea     info_txt2+28(PC),A0
                move.l  first_free(A4),D1
                bsr     hexlout
                lea     info_txta+28(PC),A0
                move.l  save_data+1070(A4),D1
                bsr     hexlout
                move.l  basep(A4),D0
                beq     f_info1
                movea.l D0,A2                       ;Program basepage notice
                move.w  #14,6(A1)                   ;14 lines high
                move.w  #12,12(A1)                  ;Button in Line 12
                move.w  #1,info_r1
                move.w  #1,info_r2                  ;Extend the tree to renew
                lea     info_txt5(PC),A0
                move.l  A0,info_r3
                lea     28(A0),A0
                move.l  8(A2),D1
                bsr     hexlout                     ;TEXT-Base insert
                lea     info_txt6(PC),A0
                move.l  A0,info_r4
                lea     28(A0),A0
                move.l  $10(A2),D1
                bsr     hexlout                     ;DATA-Base insert
                lea     info_txt7+28(PC),A0
                move.l  $18(A2),D1
                bsr     hexlout                     ;BSS-Base insert
                lea     info_txt8+28(PC),A0
                move.l  $18(A2),D1
                add.l   $1C(A2),D1
                bsr     hexlout                     ;Last used Adr
                move.w  #-1,info_r5
                move.l  sym_size(A4),D2
                beq     f_info2                     ;That was all for now
                move.w  #15,6(A1)                   ;15 lines high
                move.w  #13,12(A1)                  ;Button in Line 13
                move.w  #1,info_r5
                lea     info_txt9+27(PC),A0
                movea.l A0,A2
                moveq   #4,D0                       ;Max.5 digits
f_info3:        move.b  #' ',(A0)+                  ;Delete symbol value
                dbra    D0,f_info3
                movea.l A2,A0
                moveq   #14,D1
                bsr     ldiv                        ;An entry is 14 bytes long
                move.l  D2,D1
                moveq   #10,D2                      ;Decimal system
                bsr     numoutx
                move.l  #'    ',D0
                moveq   #' ',D1
                tst.b   gst_sym_flag(A4)
                beq.s   f_info4
                move.l  #'(GST',D0
                moveq   #')',D1
f_info4:        move.l  D0,info_txts
                move.b  D1,info_txts+4
                bra.s   f_info2
f_info1:        move.l  merk_anf(A4),D1
                beq.s   f_info2
                move.w  #12,6(A1)                   ;12 lines high
                move.w  #10,12(A1)                  ;Button in Line 10
                move.w  #1,info_r1
                move.w  #-1,info_r2                 ;Tree on half
                lea     info_txt3(PC),A0
                move.l  A0,info_r3
                lea     28(A0),A0
                bsr     hexlout                     ;Insert start address
                lea     info_txt4(PC),A0
                move.l  A0,info_r4
                lea     28(A0),A0
                move.l  merk_end(A4),D1
                subq.l  #1,D1
                bsr     hexlout                     ;Insert end address
f_info2:        clr.b   testwrd(A4)
                movea.l A1,A0
                jmp     @form_do(A4)

info_rsc:       DC.W 0,0,38,10,1
                DC.W 16,12
                DC.L ok_button
                DC.W $26
                DC.W 11,1
                DC.L info_txt0
                DC.W 8
                DC.W 1,3
                DC.L info_txt1
                DC.W 8
                DC.W 1,4
                DC.L info_txtx
                DC.W 8
                DC.W 1,5
                DC.L info_txt2
                DC.W 8
                DC.W 1,6
                DC.L info_txta
                DC.W 8
info_r1:        DC.W 1,7
info_r3:        DC.L info_txt5
                DC.W 8
                DC.W 1,8
info_r4:        DC.L info_txt6
                DC.W 8
info_r2:        DC.W 1,9
                DC.L info_txt7
                DC.W 8
                DC.W 1,10
                DC.L info_txt8
                DC.W 8
info_r5:        DC.W 1,11
                DC.L info_txt9
                DC.W 8
                DC.W -1

                SWITCH language
                CASE 0
info_txt9:      DC.B 'Symbolanzahl  '
info_txts:      DC.B '            :     ',0
info_txt0:      DC.B 'Speicherbelegung:',0
info_txt1:      DC.B 'Start des Debuggers       :$xxxxxxxx',0
info_txtx:      DC.B 'Ende des Debuggers        :$xxxxxxxx',0
info_txt2:      DC.B 'Start des freien Speichers:$xxxxxxxx',0
info_txta:      DC.B 'Ende des freien Speichers :$xxxxxxxx',0
info_txt3:      DC.B 'Start des Programms       :$xxxxxxxx',0
info_txt4:      DC.B 'Ende des Programms        :$xxxxxxxx',0
info_txt5:      DC.B 'Start des TEXT-Segments   :$xxxxxxxx',0
info_txt6:      DC.B 'Start des DATA-Segments   :$xxxxxxxx',0
info_txt7:      DC.B 'Start des BSS-Segments    :$xxxxxxxx',0
info_txt8:      DC.B 'Erste freie Adresse       :$xxxxxxxx',0
ok_button:      DC.B '  OK  ',0

                CASE 1
info_txt9:      DC.B 'Number of Symbols '           ;~
info_txts:      DC.B '            :     ',0
info_txt0:      DC.B 'Memory table:',0
info_txt1:      DC.B 'Start of the debugger     :$xxxxxxxx',0
info_txtx:      DC.B 'End of the debugger       :$xxxxxxxx',0
info_txt2:      DC.B 'Start of free memory      :$xxxxxxxx',0
info_txta:      DC.B 'End of free memory        :$xxxxxxxx',0
info_txt3:      DC.B 'Start of program          :$xxxxxxxx',0
info_txt4:      DC.B 'End of program            :$xxxxxxxx',0
info_txt5:      DC.B 'Start of TEXT-segment     :$xxxxxxxx',0
info_txt6:      DC.B 'Start of DATA-segment     :$xxxxxxxx',0
info_txt7:      DC.B 'Start of BSS-segment      :$xxxxxxxx',0
info_txt8:      DC.B 'first free address        :$xxxxxxxx',0
ok_button:      DC.B '  OK  ',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* S+F9 - Toggle Mode (Overwrite/Insert)                                        *
********************************************************************************
                PART 'f_togmode'
f_togmode:      not.b   ins_mode(A4)                ;Mode switch
                jmp     set_ins_flag
                ENDPART

********************************************************************************
* S+F10 - Quit?                                                                *
********************************************************************************
                PART 'f_quit'
f_quit:         lea     quit_rsc(PC),A0
                jsr     @form_do(A4)
                subq.w  #2,D0                       ;No end
                bne     cmd_exit1                   ;That's it
                rts

quit_rsc:       DC.W 0,0,27,6,1
                DC.W 1,1
                DC.L stop_icn
                DC.W $3303
                DC.W 7,1
                DC.L quit_txt1
                DC.W 8
                DC.W 7,2
                DC.L quit_txt2
                DC.W 8
                DC.W 7,4
                DC.L quit_txt3
                DC.W $26
                DC.W 17,4
                DC.L quit_txt4
                DC.W $24
                DC.W -1

                SWITCH language
                CASE 0
quit_txt1:      DC.B 'Möchten Sie den',0
quit_txt2:      DC.B 'Debugger verlassen?',0
quit_txt3:      DC.B '  JA  ',0
quit_txt4:      DC.B ' NEIN ',0

                CASE 1
quit_txt1:      DC.B 'Do you want to',0
quit_txt2:      DC.B 'leave the debugger?',0
quit_txt3:      DC.B ' YES ',0
quit_txt4:      DC.B ' NO  ',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* Output routines (system-independent)                                         *
********************************************************************************
********************************************************************************
* Hex output in D1                                                             *
********************************************************************************
                PART 'hex???out'
hexa2out:       moveq   #'$',D0
                jsr     @chrout(A4)
hexlout:        swap    D1                          ;Longword in D1 output
                bsr.s   hexwout
                swap    D1
hexwout:        rol.w   #8,D1                       ;Word in D1 output
                bsr.s   hexbout
                rol.w   #8,D1
hexbout:        movem.l D0-D2/A6,-(SP)              ;Byte in D1 output
                lea     hex2tab(PC),A6
                tst.w   small(A4)
                bne.s   hexbbut
                lea     hex_tab(PC),A6
hexbbut:        moveq   #0,D0
                moveq   #$0F,D2
                and.w   D1,D2
                rol.b   #4,D1
                and.w   #$0F,D1
                move.b  0(A6,D1.w),D0
                jsr     @chrout(A4)
                move.b  0(A6,D2.w),D0
                jsr     @chrout(A4)
                movem.l (SP)+,D0-D2/A6
                rts
hex_tab:        DC.B '0123456789ABCDEF'
hex2tab:        DC.B '0123456789abcdef'
                ENDPART

********************************************************************************
* Output number or label in D1                                                 *
********************************************************************************
                PART 'symbol_numout'
symbol_numout:  bsr.s   hunt_symbol
                beq     numout                      ;Z=1 => No label
                moveq   #'.',D0
                jsr     @chrout(A4)
                ENDPART

********************************************************************************
* Output label from A1                                                         *
********************************************************************************
                PART 'labelout'
labelout:       move.l  (A1),-(SP)
                jsr     @print_line(A4)
                rts
                ENDPART

********************************************************************************
* Test if a label has the value D1, then z = 0 and A1 = pointer to label       *
* (Binary search routine)                                                      *
********************************************************************************
                PART 'hunt_symbol'
hunt_symbol:    movem.l D0-D5,-(SP)
                tst.l   sym_size(A4)                ;Symbol table at all?
                beq.s   hunt_symbol4                ;No!=> No label possible
                movea.l sym_adr(A4),A1              ;Initial address of the symbol table
                moveq   #0,D5                       ;Left border = 0
                move.l  D1,D4
                move.l  sym_size(A4),D2             ;Right border
                moveq   #14,D1
                bsr     ldiv                        ;An entry is 14 bytes long
                move.l  D4,D1
hunt_symbol1:   move.w  D5,D4                       ;left border
                add.w   D2,D4                       ;+ right border
                lsr.w   #1,D4                       ;by 2
                moveq   #0,D0                       ;possibly the label> 64k
                move.w  D4,D0                       ;= New index
                mulu    #14,D0                      ;Width of an entry
                cmp.l   10(A1,D0.l),D1              ;Compare value
                bhi.s   hunt_symbol3                ;sought-after ADR > Tabellenadr
                blo.s   hunt_symbol2                ;sought-after ADR < Tabellenadr
                lea     0(A1,D0.l),A1               ;Found!
                move    #0,CCR
                movem.l (SP)+,D0-D5
                rts
hunt_symbol2:   move.w  D4,D2                       ;Right limit = index
                cmp.w   D5,D2
                beq.s   hunt_symbol4                ;Left = right limit => not found
                bra.s   hunt_symbol1                ;Next
hunt_symbol3:   move.w  D4,D5
                addq.w  #1,D5                       ;Increase left index boundary
                cmp.w   D5,D2                       ;Left = right limit => not found
                bne.s   hunt_symbol1                ;Next
hunt_symbol4:   lea     0(A1,D0.l),A1               ;last position
                move    #$FF,CCR                    ;Nothing found => normal number
                movem.l (SP)+,D0-D5
                rts
                ENDPART

********************************************************************************
* Output decimal number in D1                                                  *
* Number of jobs in D4                                                         *
********************************************************************************
                PART 'dezw_out'
dezw_out:       movem.l D0-D5/A3,-(SP)
                lea     dez_tab(PC),A3              ;Pointer to the table (S.U.)
                move.w  D4,D5                       ;Number of job 1
                add.w   D5,D5
                add.w   D5,D5                       ;4 (faster than LSL.W # 2, D5!)
                lea     4(A3,D5.w),A3               ;Table pointer to the number of digits
                moveq   #' ',D5                     ;Leading zeros as space
dezw_o1:        move.l  -(A3),D3                    ;Get value from the table
                moveq   #-'0',D2                    ;becomes too -'1 ', -' 2 ', -' 3 ', ...
dezw_o2:        sub.l   D3,D1                       ;Turn off the table
                dbmi    D2,dezw_o2                  ;Underflow? No! =>
                neg.b   D2                          ;z.B. -'1' => '1'
                move.b  D2,D0
                cmp.b   #'0',D0                     ;a zero?
                beq.s   dezw_o4                     ;Yes!=>
                moveq   #'0',D5                     ;From now on, zeros are issued as "0"
dezw_o3:        jsr     @chrout(A4)                 ;Output the character in D0
                add.l   D3,D1                       ;Take back the underflow (S.O.)
                dbra    D4,dezw_o1                  ;already spend all places?No!=>
                movem.l (SP)+,D0-D5/A3
                rts
dezw_o4:        move.w  D5,D0                       ;Bring characters for the zero
                tst.w   D4                          ;Last digit?
                bne.s   dezw_o3                     ;No!=> output
                moveq   #'0',D0                     ;If the value is 0, at least one '0'
                bra.s   dezw_o3                     ;output!

dez_tab:        DC.L 1,10,100,1000,10000,100000
                DC.L 1000000,10000000,100000000,1000000000
                ENDPART

********************************************************************************
* Output decimal number in D1 (with guide nulls!)                              *
* Number of jobs in D4                                                         *
********************************************************************************
                PART 'dezw_out_b'
dezw_out_b:     movem.l D0-D5/A3,-(SP)
                lea     dez_tab(PC),A3
                move.w  D4,D5
                lsl.w   #2,D5
                lea     4(A3,D5.w),A3
                moveq   #' ',D5
dezw_o1_b:      move.l  -(A3),D3
                moveq   #$D0,D2
dezw_o2_b:      sub.l   D3,D1
                dbmi    D2,dezw_o2_b
                neg.b   D2
                move.b  D2,D0
                moveq   #'0',D5
                jsr     @chrout(A4)
                add.l   D3,D1
                dbra    D4,dezw_o1_b
                movem.l (SP)+,D0-D5/A3
                rts
                ENDPART

********************************************************************************
* Output number D1 in the decimal system                                       *
********************************************************************************
                PART 'dezout'
dezout:         moveq   #10,D2                      ;Number system to decimal
                ENDPART

********************************************************************************
* Number (D1) with number base characters (base = D2) output                   *
********************************************************************************
                PART 'numout'
numout:         cmp.w   #$10,D2
                beq.s   hexout                      ;If hexadecimal => in your own output
                movem.l D0-D4/A6,-(SP)
                moveq   #10,D4
                cmp.w   D4,D2
                bne.s   numoutb                     ;Decimal number?
                cmp.l   D4,D1                       ;and the number of less than 10 is
                blo.s   numout0                     ;Do not issue a number base
numoutb:        bsr     basout                      ;Get number base sign according to D0
                jsr     @chrout(A4)                 ;and spend
                bra.s   numout0                     ;Spend number according to the base
                ENDPART

********************************************************************************
* Output hex number in D1                                                      *
********************************************************************************
                PART 'hexout'
hexout:         movem.l D0-D4,-(SP)
                moveq   #0,D4
                moveq   #-1,D2
                moveq   #7,D3                       ;max.8 digits
                cmp.l   #10,D1                      ;and the number of less than 10 is
                blo.s   hexouta                     ;Do not issue a number base
                move.b  hexbase(PC),D0
                jsr     @chrout(A4)
hexouta:        rol.l   #4,D1
                move.b  D1,D0
                andi.w  #$0F,D0
                tst.b   D2                          ;1st Ciffer <> "0" already output?
                beq.s   hexoutb
                tst.b   D0
                beq.s   hexoutd                     ;Suppress leaders
                moveq   #0,D2                       ;From now on all digits output
hexoutb:        addi.w  #$30,D0
                cmp.b   #'9',D0
                bls.s   hexoutc                     ;nibble In D0 by hex number
                addq.w  #7,D0
hexoutc:        jsr     @chrout(A4)                 ;and spend numbers
                moveq   #-1,D4
hexoutd:        dbra    D3,hexouta
                tst.w   D4
                bne.s   hexoute                     ;Do not spend nothing?
                moveq   #'0',D0
                jsr     @chrout(A4)                 ;At least a zero
hexoute:        movem.l (SP)+,D0-D4
                rts
                ENDPART

********************************************************************************
* Spend number (D1) for number base D2                                         *
********************************************************************************
                PART 'numoutx'
numoutx:        movem.l D0-D4/A6,-(SP)
numout0:        movea.l SP,A6                       ;Number base characters (e.g. $) preceded
numout1:        bsr     div                         ;divide
                move.w  D3,-(SP)                    ;BCD digit on stack
                tst.l   D1
                bne.s   numout1                     ;Number completely on the stack?
numout3:        move.w  (SP)+,D0                    ;Get BCD digit
                add.b   #'0',D0
                cmp.b   #$3A,D0
                blo.s   numout2
                addq.b  #7,D0                       ;convert to ASC digit or letters
numout2:        jsr     @chrout(A4)                 ;Spend characters
                cmpa.l  SP,A6
                bne.s   numout3                     ;already everything?
                movem.l (SP)+,D0-D4/A6
                rts
                ENDPART

********************************************************************************
* Character routines (system-independent)                                      *
********************************************************************************
********************************************************************************
* Reads a sign according to D0 (overlooking spaces, ...)                       *
********************************************************************************
                PART 'get'
get:            moveq   #0,D0
                move.b  (A0)+,D0                    ;Pick sign
                beq.s   get2
                cmp.b   #':',D0                     ;Line separator
                beq.s   get3
                cmp.b   #';',D0                     ;Also 'a final identification
                beq.s   get2
                cmp.b   #' ',D0                     ;Spaces are overlooked
                beq.s   get
                cmp.b   #'a',D0
                blo.s   get1                        ;No lower case
                cmp.b   #'z',D0
                bhi.s   get1                        ;No lower case
                and.b   #$DF,D0
get1:           tst.b   D0
                rts
get3:           move.l  A0,input_pnt(A4)            ;There it goes on ...
get2:           moveq   #0,D0
                subq.l  #1,A0                       ;Does not change flags
                rts
                ENDPART

********************************************************************************
* Is D0 lies in the number system D2?                                          *
********************************************************************************
                PART 'chkval'
chkval:         sub.b   #'0',D0                     ;Check D0 on Validity in the Number System D2
                cmp.b   #10,D0                      ;less than 10?
                blo.s   chkval0                     ;yes, okay
                subq.b  #7,D0                       ;No, 7 way
                cmp.b   #10,D0                      ;Now less than 10?
                blo.s   chkval1                     ;Yes, Error, Delete Carry
chkval0:        cmp.b   D2,D0                       ;Compare with number base
                bhs.s   chkval1
                rts
chkval1:        addi.b  #$37,D0                     ;restore because no number
                move    #0,CCR
                rts
                ENDPART

********************************************************************************
* Test if a comma or zero byte follows                                         *
********************************************************************************
                PART 'chkcom'
chkcom:         tst.b   D0
                beq.s   chkcom1
                cmp.b   #',',D0
                bne.s   syn_err                     ;Error, if not
                bsr.s   get                         ;Get next character
                move    #0,CCR                      ;Delete all bits, as there is a comma
chkcom1:        rts
syn_err:        bra     synerr
                ENDPART

********************************************************************************
* Number base according to the number base sign (in D0) to D3                  *
********************************************************************************
                PART 'numbar'
numbas:         moveq   #3,D3                       ;If the drawback in D0 a number base character
numbas1:        cmp.b   numtab(PC,D3.w),D0          ;is, return with the number base in D3
                dbeq    D3,numbas1                  ;otherwise negative = 1
                tst.w   D3
                bmi.s   numbas2
                move.b  numtab1(PC,D3.w),D3
numbas2:        rts
                DC.B '›'
numtab:         DC.B '%@.'
hexbase:        DC.B '$'
numtab1:        DC.B 2,8,10,16
                EVEN
                ENDPART

********************************************************************************
* Number base sign according to the number base (in D2) to D0                  *
********************************************************************************
                PART 'basout'
basout:         moveq   #3,D0                       ;Get sign for number basis ($, @, ...) in D0
basout1:        cmp.b   numtab1(PC,D0.w),D2         ;Space if no valid number basis
                dbeq    D0,basout1
                move.b  numtab(PC,D0.w),D0
                rts
                ENDPART

********************************************************************************
* Get parameters according to A2 and A3                                        *
* C=0, If 1 parameter exists                                                   *
* V=0, If 2 parameters available                                               *
********************************************************************************
                PART 'get_parameter'
get_parameter:  suba.l  A2,A2                       ;Bring two numerical values in A2 and A3
                suba.l  A3,A3                       ;If not specified, he is zero
                move.w  #3,-(SP)                    ;Flagbyte no parameter specified
                bsr     get                         ;Pick up
                beq.s   get_parameter2              ;Done, because no parameters
                cmp.b   #',',D0
                beq.s   get_parameter1              ;yes
                bsr     get_term
                movea.l D1,A2                       ;1st parameter according to A2
                andi.w  #$FE,(SP)                   ;C
                cmp.b   #',',D0                     ;Come?
                bne.s   get_parameter2              ;No, so no 2 parameter
get_parameter1: bsr     get                         ;Comma
                bsr     get_term
                movea.l D1,A3                       ;2nd parameter according to A3
                andi.w  #$FD,(SP)                   ;Remove
get_parameter2: move    (SP)+,CCR
                rts
                ENDPART

********************************************************************************
* Get parameters for disassemble / dump                                        *
* A2 - Start address                                                           *
* A3 - End address (valid if d2 = 0)                                           *
* D2 - Number of lines                                                         *
********************************************************************************
                PART 'get2(x)adr'
get2adr:        movea.l default_adr(A4),A2          ;Default start address
                suba.l  A3,A3                       ;default end address
get2xadr:       move.w  def_lines(A4),D2            ;Default line number
                subq.w  #1,D2
                bsr     get                         ;Pick up
                beq.s   get2ad0                     ;Done, because no parameters
                cmp.b   #'#',D0                     ;Number of lines?
                beq.s   get2ad2
                cmp.b   #'[',D0                     ;byte number?
                beq.s   get2ad6
                cmp.b   #',',D0                     ;Self address?
                beq.s   get2ad1
                bsr     get_term                    ;New start address
                movea.l D1,A2
                tst.b   D0
                beq.s   get2ad0
                cmp.b   #'[',D0                     ;byte number?
                beq.s   get2ad6
                cmp.b   #'#',D0
                beq.s   get2ad2
                cmp.b   #',',D0                     ;But now it has to be a comma!
                bne     syn_err
get2ad1:        bsr     get
                cmp.b   #'#',D0                     ;Number of lines as 2 parameters?
                beq.s   get2ad2
                cmp.b   #'[',D0                     ;byte number?
                beq.s   get2ad6
                bsr     get_term                    ;Get new end address
                movea.l D1,A3
get2ad01:       moveq   #0,D2                       ;Clear lines number
get2ad0:        move.l  A2,default_adr(A4)          ;Set new DefaultAdr
                tst.w   D2
                beq.s   get2ad4                     ;Do not list lines?
                suba.l  A3,A3                       ;End address invalid
get2ad4:        rts
get2ad2:        bsr     get
                beq.s   get2ad3                     ;If nothing follows => 1 line is default
                bsr     get_term                    ;Number of lines
                subq.l  #1,D1                       ;For DBRA
                move.l  D1,D2
                swap    D1
                tst.w   D1
                bne.s   get2ad5                     ;max.65535 Zeilen listen
                bra.s   get2ad0
get2ad3:        moveq   #0,D2                       ;Number of lines = 1
                move.l  A2,default_adr(A4)          ;Set new Default Address
                suba.l  A3,A3                       ;End address invalid
                rts
get2ad6:        bsr     get
                beq     synerr                      ;If nothing follows => error
                bsr     get_term                    ;Byte
                cmp.b   #']',D0
                bne.s   get2ad7
                bsr     get                         ;possibly "]"
get2ad7:        lea     0(A2,D1.l),A3               ;Calculate end address
                bra.s   get2ad01
get2ad5:        bra     illequa
                ENDPART

********************************************************************************
* Get parameters for Find and Fill                                             *
********************************************************************************
                PART 'get_such_para'
get_such_para:  cmp.b   #',',D0
                bne     syn_err
                moveq   #0,D3                       ;a byte entered
                move.b  #2,find_cont0(A4)
                lea     data_buff(A4),A1
get_such_para1: bsr.s   get_such_para4
                cmp.b   #',',D0
                beq.s   get_such_para1
                tst.b   D0
                bne     syn_err
get_such_para2: move.l  A1,D3
                lea     data_buff(A4),A1
                sub.l   A1,D3
                subq.w  #1,D3                       ;Long-1
                rts
get_such_para3: movem.l D1-D2/D4-D7/A2-A6,-(SP)
                moveq   #0,D3                       ;a byte entered
                lea     data_buff(A4),A1
                bsr.s   get_such_para4
                movem.l (SP)+,D1-D2/D4-D7/A2-A6
                bra.s   get_such_para2
get_such_para4: bsr     get                         ;1st sign according to D0
                cmp.b   #$22,D0                     ;Quotation marks?
                beq.s   get_such_para11             ;and, Ascii Hoen
                cmp.b   #$27,D0
                beq.s   get_such_para11             ;Yes, ASCII.
                cmp.b   #'!',D0
                beq.s   get_such_para10             ;is mnemonic
                bsr     get_term                    ;Get the term according to D1
                cmp.b   #'.',D0
                bne.s   get_such_para9              ;Determine size
                bsr     get                         ;get extension
                move.w  D0,D2                       ;and save
                bsr     get                         ;ever bring the epilial sign
                cmp.b   #'W',D2
                beq.s   get_such_para6
                cmp.b   #'L',D2
                beq.s   get_such_para5
                cmp.b   #'A',D2
                beq.s   get_such_para8
                cmp.b   #'B',D2
                beq.s   get_such_para7
                bra     syn_err
get_such_para5: swap    D1
                bsr.s   get_such_para6
                swap    D1
get_such_para6: ror.w   #8,D1                       ;Word
                move.b  D1,(A1)+
                ror.w   #8,D1
get_such_para7: move.b  D1,(A1)+                    ;byte number
                rts
get_such_para8: addi.w  #1,D3                       ;3-Byte-Address
                swap    D1
                move.b  D1,(A1)+
                swap    D1
                bra.s   get_such_para6

get_such_para9: move.l  D1,D2
                swap    D2
                tst.w   D2
                bne.s   get_such_para5              ;More than a Word => Long!
                swap    D2
                andi.w  #$FF00,D2
                bne.s   get_such_para6              ;Word recognized
                bra.s   get_such_para7              ;Only one byte

get_such_para10:movea.l A1,A6                       ;here should be fledged
                bsr     code_line                   ;and the command assemble
                movea.l A6,A1                       ;Next address
                rts

get_such_para11:moveq   #-1,D5                      ;No entry yet
                moveq   #63,D4                      ;Maximum 64 characters ASCII are allowed
                move.w  D0,D2                       ;"or 'remember (string must also end)
get_such_para12:move.b  (A0)+,D0                    ;Read ASCII sign
                beq.s   get_such_para13             ;Line end = demolition
                cmp.w   D2,D0                       ;End criteria reached?
                beq.s   get_such_para13             ;then abort
                moveq   #0,D5
                move.b  D0,(A1)+
                dbra    D4,get_such_para12
                bra     syn_err                     ;too long!
get_such_para13:tst.w   D5                          ;In general, what happened?
                bne     syn_err                     ;no!
                bra     get
                ENDPART

********************************************************************************
* Expression evaluation, result according to D1                                *
********************************************************************************
                PART 'get_term'
get_term:       moveq   #'-',D1
                cmp.b   D0,D1                       ;'--' is nothing
                bne.s   get_term2
                cmp.b   (A0),D1
                bne.s   get_term2
get_term1:      bsr     get                         ;'-' to the comma overlook (2.4 or 8)
                beq.s   get_term0
                cmp.b   #',',D0
                bne.s   get_term1
get_term0:      rts
get_term2:      tst.b   D0
                beq     syn_err
                movem.l D2-D7/A1-A6,-(SP)
                bsr.s   get_term4
                moveq   #-1,D2
get_term3:      addq.w  #1,D2
                move.b  get_term_tab(PC,D2.w),D3
                addq.b  #1,D3                       ;Table end = -1
                beq     synerr                      ;=> Wrong formula
                cmp.b   get_term_tab(PC,D2.w),D0    ;Formula signs found?
                bne.s   get_term3                   ;No, continue searching
                movem.l (SP)+,D2-D7/A1-A6
                rts
get_term_tab:   DC.B ',(.#=[]',$22,0,-1             ;Allowed signs as a formula
                EVEN

get_term4:      move.l  D2,-(SP)
                bsr     w_eausd
                move.l  D1,D2
get_term5:      cmp.b   #'+',D0                     ;Addition
                bne.s   get_term6
                bsr     get
                bsr.s   w_eausd
                add.l   D1,D2
                bvs.s   overflo
                bra.s   get_term5
overflo:        bra     overfl
get_term6:      cmp.b   #'-',D0                     ;subtraction
                bne.s   get_term7
                bsr     get
                bsr.s   w_eausd
                sub.l   D1,D2
                bvs.s   overflo
                bra.s   get_term5
get_term7:      cmp.b   #'|',D0                     ;OR
                bne.s   get_term8
                bsr     get
                bsr.s   w_eausd
                or.l    D1,D2
                bra.s   get_term5
get_term8:      cmp.b   #'^',D0                     ;EOR
                bne.s   get_term9
                bsr     get
                bsr.s   w_eausd
                eor.l   D1,D2
                bra.s   get_term5
get_term9:      cmp.b   #'<',D0                     ;SHL
                bne.s   get_term10
                cmpi.b  #'<',(A0)
                bne.s   get_term10
                addq.l  #1,A0
                bsr     get
                bsr.s   w_eausd
                lsl.l   D1,D2
                bra.s   get_term5
get_term10:     cmp.b   #'>',D0                     ;SHR
                bne.s   get_term11
                cmpi.b  #'>',(A0)
                bne.s   get_term11
                addq.l  #1,A0
                bsr     get
                bsr.s   w_eausd
                lsr.l   D1,D2
                bra.s   get_term5
get_term11:     move.l  D2,D1
                move.l  (SP)+,D2
                rts

w_eausd:        move.l  D2,-(SP)
                bsr.s   w_term
                move.l  D1,D2
w_eal:          cmp.b   #'*',D0                     ;multiplication
                bne.s   w_ea1
                bsr     get
                bsr.s   w_term
                bsr     lmult                       ;D2=D1*D2
                bra.s   w_eal
w_ea1:          cmp.b   #'/',D0                     ;Division
                bne.s   w_ea2
                bsr     get
                bsr.s   w_term
                bsr     ldiv                        ;D2.L = D2.L/D1.L
                bra.s   w_eal
w_ea2:          cmp.b   #'&',D0                     ;AND
                bne.s   w_ea3
                bsr     get
                bsr.s   w_term
                and.l   D1,D2
                bra.s   w_eal
w_ea3:          cmp.b   #'%',D0                     ;MODULO
                bne.s   w_eaend
                bsr     get
                bsr.s   w_term
                bsr     ldiv                        ;D1.L = D2 MOD D1
                move.l  D1,D2
                bra.s   w_eal
w_eaend:        move.l  D2,D1
                move.l  (SP)+,D2
                rts

w_term:         cmp.b   #'!',D0                     ;Logical NOT
                bne.s   w_term0
                bsr     get
                bsr.s   w_term0
                tst.l   D1
                beq.s   w_term4
                moveq   #0,D1
                rts
w_term4:        moveq   #1,D1
                rts
w_term0:        cmp.b   #'~',D0                     ;NOT
                bne.s   w_term1
                bsr     get
                bsr.s   w_term1
                not.l   D1
                rts
w_term1:        cmp.b   #'-',D0
                beq.s   w_term3
                cmp.b   #'+',D0
                bne.s   w_term2
                bsr     get                         ;Positive sign
w_term2:        bsr.s   w_fakt
                rts
w_term3:        bsr     get                         ;Negative sign
                bsr.s   w_fakt
                neg.l   D1
                rts

w_fakt:         move.l  D2,-(SP)
                cmp.b   #'(',D0
                beq.s   w_fakt1
                cmp.b   #'{',D0
                beq.s   w_fakt2
                bsr     get_zahl                    ;Get number according to D1
                move.l  (SP)+,D2
                rts
w_fakt1:        bsr     get                         ;Clip
                bsr     get_term4                   ;Evaluate expression in the bracket
                cmp.b   #')',D0
                bne.s   mistbra                     ;Brace must follow
                bsr     get
                move.l  (SP)+,D2
                rts
mistbra:        bra     misbrak
w_fakt2:        bsr     get
                bsr     get_term4
                cmp.b   #'}',D0                     ;indirect
                bne.s   mistbra
                bsr     get
                moveq   #0,D2                       ;Word is default
                cmp.b   #'.',D0                     ;Width indicated?
                bne.s   w_fakt4                     ;No!=> Word
                bsr     get
                move.b  D0,D3
                bsr     get
                moveq   #-1,D2                      ;Long
                cmp.b   #'L',D3
                beq.s   w_fakt4
                moveq   #0,D2                       ;Word
                cmp.b   #'W',D3
                beq.s   w_fakt4
                moveq   #1,D2                       ;Byte
                cmp.b   #'B',D3
                bne     synerr                      ;data was nothing!
w_fakt4:        movea.l $08.w,A1
                movea.l $0C.w,A2
                lea     w_fakt3(PC),A3
                move.l  A3,$08.w                    ;Change bus errors
                move.l  A3,$0C.w                    ;Intercept address error
                movea.l D1,A3
                moveq   #0,D1
                tst.b   D2
                bmi.s   w_fakt5                     ;Long
                beq.s   w_fakt7                     ;Word
                move.b  (A3),D1                     ;Byte
                bra.s   w_fakt6
w_fakt7:        move.w  (A3),D1                     ;Word get
                bra.s   w_fakt6
w_fakt5:        move.l  (A3),D1                     ;Long get
w_fakt6:        move.l  A1,8.w
                move.l  A2,$0C.w
                move.l  (SP)+,D2
                rts
w_fakt3:        move.l  A1,$08.w
                move.l  A2,$0C.w
                bra     illequa                     ;Bah, a mistake
                ENDPART

********************************************************************************
* Get number according to D1.L                                                 *
********************************************************************************
                PART 'get_zahl'
get_zahl:       movem.l D2-D7/A1-A6,-(SP)
                move.w  D0,D2                       ;News Make News 1st
                lea     vartab(PC),A1
                lea     w_legalc(PC),A3
                movea.l A0,A2                       ;Remember pointer to possibly variable or number
w_zahl0:        moveq   #-1,D1
                move.w  D2,D0                       ;Pick up 1st
                tst.b   (A1)                        ;End of the table reached?
                bmi     w_zahlh                     ;It must be a normal number
w_zahl1:        addq.w  #1,D1
                cmpi.b  #' ',0(A1,D1.w)             ;Entry found?
                beq.s   w_zahl3                     ;Yes!
                tst.b   0(A1,D1.w)
                beq.s   w_zahl3                     ;Entry also found
                tst.w   D1                          ;1st sign of the label
                beq.s   w_zah10                     ;Everything is still allowed
                ext.w   D0
                bmi.s   w_zahl1                     ;Signs> 127 are not allowed
                tst.b   0(A3,D0.w)                  ;Sign still allowed?
                bne.s   w_zah11                     ;No!=> Cancel, since unequal
w_zah10:        cmp.b   0(A1,D1.w),D0               ;Still the same?
w_zah11:        move    SR,D3
                bsr     get                         ;ever bring the next sign
                move.w  D0,D4                       ;Save if it was the last sign
                move    D3,CCR
                beq.s   w_zahl1                     ;If the same, test next character
                lea     16(A1),A1                   ;Pointer to the next variable
                movea.l A2,A0                       ;Pointer back
                bra.s   w_zahl0                     ;Next

w_zahl3:        moveq   #0,D1
                move.w  8(A1),D0                    ;artDerVariable
                move.w  10(A1),D1                   ;Handover parameter
                movea.l 12(A1),A1                   ;Pointer / value of the variables
                adda.l  A4,A1
                tst.w   D0
                beq.s   w_zahl6                     ;Direct value (even with direct values!)
                cmp.w   #2,D0
                blo.s   w_zahl5                     ;Pointer to the value
                beq.s   w_zahl7                     ;Pointer on pointer (+ offset)
                cmp.w   #4,D0
                beq.s   w_zahl8                     ;Pointer on Word
                suba.l  A4,A1                       ;-Varbase, absolutely address
                move.w  D4,D0                       ;Retrieve the last character
                jsr     (A1)                        ;Routine determines the variable value
                bra.s   w_zahla
w_zahl5:        move.l  0(A1,D1.w),D1               ;Pointer on long
                bra.s   w_zahl9
w_zahl6:        move.l  A1,D1                       ;Direct variable value
                bra.s   w_zahl9
w_zahl7:        move.w  D1,D2
                move.l  (A1),D1                     ;Get pointer
                beq.s   w_zahl9
                movea.l D1,A1
                move.l  0(A1,D2.w),D1               ;Get variable value
                bra.s   w_zahl9
w_zahl8:        move.w  0(A1,D1.w),D1               ;Pointer on Word
w_zahl9:        move.w  D4,D0                       ;Return last sign
w_zahla:        movem.l (SP)+,D2-D7/A1-A6
                move    #0,CCR                      ;All flags zero, as no empty input
                rts
w_zahlb:        lea     regs+32(A4),A1
                moveq   #8,D2
                bsr     chkval
                bcc     syn_err
                cmp.w   #7,D0
                bne.s   w_zahlg                     ;A7 = Get Stackpointer
                bsr     get                         ;Get next sign
w_zahlc:        btst    #5,_sr(A4)                  ;Supervisor-Mode?
                bne.s   w_zahld
                move.l  _usp(A4),D1
                rts
w_zahlbk:       bsr     get_term4                   ;Get Breakpoint number (recursive!)
                tst.l   D1
                bmi.s   ill_brk
                cmp.l   #15,D1
                bhi.s   ill_brk
                lea     breakpnt(A4),A1
                mulu    #12,D1                      ;Time 12 as an index in the table
                move.l  0(A1,D1.w),D1               ;Get address of the Breakpoint
                beq.s   ill_brk
                rts
ill_brk:        bra     illbkpt
w_zahld:        move.l  _ssp(A4),D1
                rts
w_zahle:        moveq   #0,D1
                move.w  _sr(A4),D1                  ;Get SR
                andi.w  #$FF,D1                     ;For the CCR only the lower 8 bits
                rts
w_zahlf:        lea     regs(A4),A1
                moveq   #8,D2
                bsr     chkval
                bcc     syn_err
w_zahlg:        cmp.w   #8,D0                       ;Register>7 ?
                bcc     syn_err
                lsl.w   #2,D0
                move.l  0(A1,D0.w),D1               ;Get Register
                bra     get                         ;Get next character & end

w_zahlme:       moveq   #10,D2
                bsr     chkval
                bcc     syn_err
                subq.w  #1,D0
                bpl.s   w_zahlmx
                moveq   #9,D0
w_zahlmx:       lea     simple_vars(A4),A1
                asl.w   #2,D0
                move.l  0(A1,D0.w),D1               ;Get Register
                bra     get                         ;Get next character & end

w_zahlsy:       moveq   #14,D1
                move.l  sym_size(A4),D2
                bra     ldiv                        ;An entry is 14 bytes long
w_zahlcache:    moveq   #0,D1
                tst.b   prozessor(A4)               ;68000 or 68010?
                ble.s   w_zahlcachee                ;then out here
                DC.W $4E7A,$1002                    ;CACR holen
w_zahlcachee:   bra     get

w_zahlh:        moveq   #0,D0
                move.b  D2,D0                       ;Pick up 1st
                movea.l A2,A0                       ;Pointer back to the number
                cmp.b   #$27,D0                     ;ASCII-String?
                beq.s   w_zahll
                cmp.b   #$22,D0                     ;ASCII-String?
                beq.s   w_zahll
                moveq   #$10,D2                     ;Hexadecimal is default
                bsr     numbas                      ;?Numerical base
                bmi.s   w_zahli                     ;no
                move.w  D3,D2                       ;yes, Set new number basis
                bsr     get                         ;and next sign
w_zahli:        bsr     chkval                      ;Lfd. Sign valid?
                bcc.s   w_zahlo                     ;No, error (possibly label?)
                moveq   #0,D1                       ;Preject of D1
w_zahlj:        move.l  D1,D3                       ;D1.L * D2.B = D1.L
                swap    D3
                mulu    D2,D3
                mulu    D2,D1
                swap    D3
                tst.w   D3
                bne     overfl
                add.l   D3,D1
                bcs     overfl
                add.l   D0,D1                       ;and add the place
                bcs     overfl
                bsr     get                         ;Next place
                bsr     chkval                      ;valid?
                bcs.s   w_zahlj                     ;Yes, continue
w_zahlk:        movem.l (SP)+,D2-D7/A1-A6
                move    #0,CCR                      ;All flags zero, as no empty input
                rts
w_zahll:        moveq   #0,D1
w_zahlm:        cmp.b   (A0)+,D0                    ;Sign like the initial sign 'or `?
                beq.s   w_zahln                     ;Yes finished
                rol.l   #8,D1                       ;Results register 8 bit to the left Shift
                tst.b   D1                          ;Were the highest 8bit already occupied?
                bne     illequa                     ;Yes, more than 4 bytes Ascii, Error
                move.b  -1(A0),D1
                beq.s   w_zahlz                     ;zero, end of the file
                cmp.b   #13,D1                      ;CR ends Ascii
                bne.s   w_zahlm
w_zahlz:        subq.l  #1,A0                       ;Remove one again, so get 0 or Cr
                lsr.l   #8,D1
w_zahln:        bsr     get
                bra.s   w_zahlk                     ;everything ok, end

w_zahlo:        cmp.w   #10,D2
                bne     illequa                     ;label =>Decimal system
;Symboltabelle des nachgeladenen Programms durchsuchen
                lea     w_legalc(PC),A5
                movea.l A0,A3
                subq.l  #1,A3                       ;Pointer on 1st sign of the label
                tst.l   sym_size(A4)
                beq.s   w__zahl                     ;No symbol table => Browse internal
                movea.l A3,A2
                movea.l sym_adr(A4),A1              ;Initial address of the symbol table
                moveq   #0,D7
                moveq   #0,D1
w_zahlp:        movea.l (A1),A6                     ;Pointer to the label
w_zahlq:        move.b  (A2)+,D1                    ;Bring the character of the entry
                bmi.s   w_zahlx                     ;Signs> 127 are always allowed
                tst.b   0(A5,D1.w)                  ;Is the character allowed in the label?
                bne.s   w_zahlr                     ;No => found
w_zahlx:        cmp.b   (A6)+,D1                    ;Does the cram still fit?
                beq.s   w_zahlq                     ;Continue if yes!
w_zahqq:        movea.l A3,A2                       ;Pointer back
                lea     14(A1),A1                   ;Next label
                cmpa.l  sym_end(A4),A1
                blo.s   w_zahlp                     ;End? No!
                bra.s   w__zahl                     ;Browse your own symbol table
w_zahlr:        tst.b   (A6)                        ;Label not enough
                bne.s   w_zahqq                     ;Test the next label
                lea     -1(A2),A0                   ;Pointer to the first episode
                bsr     get                         ;get the follow-up sign
                move.l  10(A1),D1                   ;Get value of the label
                bra.s   w_zahlk                     ;That's it

;Interne Symboltabelle durchsuchen
w__zahl:        movea.l A3,A2
                move.l  sym_buffer(A4),D0           ;Symbol table loaded?
                beq     illequa                     ;Error if no symbol table
                movea.l D0,A1
                moveq   #0,D7
                moveq   #0,D1
                move.w  sym_anzahl(A4),D0
                bra.s   w__zahll
w__zahlp:       lea     8(A1),A6                    ;Pointer to the label
w__zahlq:       move.b  (A2)+,D1                    ;Bring the character of the entry
                bmi.s   w__zahlx                    ;Signs> 127 are always allowed
                tst.b   0(A5,D1.w)                  ;Is the character allowed in the label?
                bne.s   w__zahlr                    ;No => found
w__zahlx:       cmp.b   (A6)+,D1                    ;Does the cram still fit?
                beq.s   w__zahlq                    ;Continue if yes!
w__zahqq:       movea.l A3,A2                       ;Pointer back
                lea     32(A1),A1                   ;Next symbol
w__zahll:       dbra    D0,w__zahlp                 ;All symbols through? No =>
                bra     illlabel                    ;Icon not found
w__zahlr:       tst.b   (A6)                        ;Icon not enough
                bne.s   w__zahqq                    ;Test the next icon
                lea     -1(A2),A0                   ;Pointer to the first episode
                bsr     get                         ;get the follow-up sign
                move.l  (A1),D1                     ;Get value of the symbol
                bra     w_zahlk                     ;That's it

w_legalc:       DC.B 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
                DC.B 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
                DC.B 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
                DC.B 0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1
                DC.B 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                DC.B 0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0
                DC.B 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                DC.B 0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1

                DXSET 8,' '
vartab:         DX.B 'SYMFLAG'
                DC.W 4,0
                DC.L bugaboo_sym
                DX.B 'USERSCR'
                DC.W 0,0
                DC.L user_scr
                DX.B 'INITSCR'
                DC.W 0,0
                DC.L user_scr
                DX.B 'RING'
                DC.W 4,0
                DC.L ring_flag
                DX.B 'SWITCH'
                DC.W 4,0
                DC.L smart_switch
                DX.B 'SYMTAB'
                DC.W 1,0
                DC.L sym_buffer
                DX.B 'TRACE'
                DC.W 4,0
                DC.L trace_flag
                DX.B 'TDELAY'
                DC.W 4,0
                DC.L trace_delay
                DX.B 'MIDI'
                DC.W 4,0
                DC.L midi_flag
                DX.B 'OVERSCAN'
                DC.W 4,0
                DC.L overscan
                DX.B 'CACHE'
                DC.W 3,0
                DC.L w_zahlcache
                DX.B 'SHIFT'
                DC.W 4,0
                DC.L shift_flag
                DX.B 'MEMCHECK'
                DC.W 4,0
                DC.L all_memory
                DX.B 'CONVERT'
                DC.W 0,0
                DC.L convert_tab
                DX.B 'ACT_PD'
                DC.W 2,0
                DC.L act_pd
                DX.B 'CLICK'
                DC.W 4,0
                DC.L format_flag
                DX.B 'KLICK'
                DC.W 4,0
                DC.L format_flag
                DX.B 'IKBD'
                DC.W 0,0
                DC.L ikbd_string
                DX.B 'SCROLLD'
                DC.W 4,0
                DC.L scroll_d
                DX.B 'UTRACE'
                DC.W 0,0
                DC.L user_trace_buf
                DX.B 'UT'
                DC.W 0,0
                DC.L user_trace_buf
                DX.B 'COL0'
                DC.W 4,0
                DC.L col0
                DX.B 'CONTERM'
                DC.W 4,0
                DC.L conterm
                DX.B 'AESFLAG'
                DC.W 4,0
                DC.L no_aes_check
                DX.B 'COL1'
                DC.W 4,0
                DC.L col1
                DX.B 'CHECKSUM'
                DC.W 4,0
                DC.L checksum
                DX.B 'SMALL'
                DC.W 4,0
                DC.L small
                DX.B 'SIZE'
                DC.W 4,0
                DC.L def_size
                DX.B 'LINES'
                DC.W 4,0
                DC.L def_lines
                DX.B 'TEXT'
                DC.W 2,8
                DC.L basep
                DX.B 'DATA'
                DC.W 2,16
                DC.L basep
                DX.B 'BSS'
                DC.W 2,24
                DC.L basep
                DX.B 'MEMBASE'
                DC.W 1,0
                DC.L first_free
                DX.B 'START'
                DC.W 1,0
                DC.L merk_anf
                DX.B 'SAVEAREA'
                DC.W 0,0
                DC.L default_stk
                DX.B 'END'
                DC.W 1,0
                DC.L merk_end
                DX.B 'BASEPAGE'
                DC.W 1,0
                DC.L basep
                DX.B 'BP'
                DC.W 1,0
                DC.L basep
                DX.B 'PC'
                DC.W 1,0
                DC.L _pc
                DX.B 'USP'
                DC.W 1,0
                DC.L _usp
                DX.B 'SP'
                DC.W 3,0
                DC.L w_zahlc
                DX.B 'SYMBOLS'
                DC.W 3,0
                DC.L w_zahlsy
                DX.B 'SSP'
                DC.W 1,0
                DC.L _ssp
                DX.B 'SR'
                DC.W 4,0
                DC.L _sr
                DX.B 'CCR'
                DC.W 3,0
                DC.L w_zahle
                DX.B '*'
                DC.W 1,0
                DC.L default_adr
                DX.B '^D'
                DC.W 3,0
                DC.L w_zahlf
                DX.B '^A'
                DC.W 3,0
                DC.L w_zahlb
                DX.B '^M'
                DC.W 3,0
                DC.L w_zahlme
                DX.B '^B'
                DC.W 3,0
                DC.L w_zahlbk
                DX.B 'DISBASE'
                DC.W 4,0
                DC.L disbase
                DX.B 'BUFFER'
                DC.W 1,0
                DC.L dsk_adr
                DX.B 'SEKBUFF'
                DC.W 0,0
                DC.L sekbuff
                DX.B 'TRKBUFF'
                DC.W 0,0
                DC.L first_free
                DX.B 'TRACK'
                DC.W 4,0
                DC.L dsk_track
                DX.B 'SEKTOR'
                DC.W 4,0
                DC.L dsk_sektor
                DX.B 'SECTOR'
                DC.W 4,0
                DC.L dsk_sektor
                DX.B 'SIDE'
                DC.W 4,0
                DC.L dsk_side
                DX.B 'DRIVE'
                DC.W 4,0
                DC.L dsk_drive
                DX.B 'S'
                DC.W 0,0
                DC.L default_stk
                DC.B -1
                EVEN
                ENDPART

********************************************************************************
* Filename after FNAME (set path & drive)                                     *
********************************************************************************
                PART 'getnam'
getnam:         bsr     get                         ;Bring sign according to D0
getnam_cont:    cmp.b   #'"',D0                     ;A valid path / filename?
                bne     synerr                      ;No!=>
                lea     fname(A4),A1                ;Place for the name
getnam1:        move.b  (A0)+,D0
                beq     synerr                      ;Quotes are missing!
                cmp.b   #'"',D0                     ;End of the filename / path?
                beq.s   getnam3                     ;Yes!=>
                cmp.b   #'a',D0
                blo.s   getnam2                     ;No lower case
                cmp.b   #'z',D0
                bhi.s   getnam2                     ;No lower case
                and.b   #$DF,D0                     ;in capital letters
getnam2:        move.b  D0,(A1)+
                bra.s   getnam1
getnam3:        movea.l A0,A3                       ;Pointer to the following sign
                clr.b   (A1)                        ;Close path / filename with zero bytes
                lea     fname(A4),A0
                cmpi.b  #':',1(A0)                  ;Drive identifier?
                bne.s   getnam5                     ;No!=>
                moveq   #0,D0
                move.b  (A0),D0                     ;Get drive letters
                cmp.w   #'P',D0
                bhi     illdrv
                sub.w   #'A',D0                     ;Disconnect drive offset
                bmi     illdrv
                move.l  A0,-(SP)
                move.w  D0,-(SP)
                move.w  #$0E,-(SP)                  ;Dsetdrv()
                tst.l   basep(A4)                   ;Other program loaded?
                beq.s   getnam4                     ;no
                trap    #1                          ;Path for the other program
getnam4:        jsr     do_trap_1                   ;Path for the debugger
                addq.l  #4,SP
                movea.l (SP)+,A0
                addq.l  #2,A0                       ;Pointer behind the drive identifier
getnam5:        movea.l A0,A2
                movea.l A0,A1
getnam6:        tst.b   (A0)
                beq.s   getnam7
                cmpi.b  #'\',(A0)+
                bne.s   getnam6                     ;path?
                movea.l A0,A1                       ;possibly remember the Filename
                bra.s   getnam6
getnam7:        cmpa.l  A0,A2
                beq.s   getnam9                     ;Only drive name indicated
                cmpi.b  #'.',(A1)                   ;Filename: "."?
                bne.s   getnam71
                addq.l  #1,A1
                cmpi.b  #'.',(A1)                   ;Filename: ".."?
                bne.s   getnam71
                addq.l  #1,A1
getnam71:       cmpa.l  A1,A2
                beq.s   getnam9                     ;No path specified
                move.b  (A1),D7                     ;Save the 1st sign of the filename
                clr.b   (A1)                        ;Terminate path with zero byte
                bsr.s   do_mediach                  ;Trigger Media-Change
                move.l  A2,-(SP)
                move.w  #$3B,-(SP)                  ;Dsetpath()
                tst.l   basep(A4)                   ;Other program loaded?
                beq.s   getnam8                     ;no
                trap    #1                          ;Path for the other program
getnam8:        jsr     do_trap_1                   ;Path for the debugger
                addq.l  #6,SP
                tst.w   D0
                bmi     toserr
                move.b  D7,(A1)
getnam9:        lea     fname(A4),A0
                movea.l A0,A2                       ;Return pointer to the filename
getnam10:       move.b  (A1)+,(A0)+                 ;Copy filenames to the front
                bne.s   getnam10
                clr.b   (A0)                        ;Another zero byte
                tst.b   (A2)                        ;No filename specified? Flag
                movea.l A3,A0
                rts
                ENDPART

********************************************************************************
* Media change required on the current drive? Then running                     *
********************************************************************************
                PART 'do_mediach'
do_mediach:     movem.l D0-D2/A0-A2,-(SP)
                move.w  #$19,-(SP)
                jsr     do_trap_1                   ;Dgetdrv()
                addq.l  #2,SP
                move.w  D0,a_mediach_drv
                move.w  D0,-(SP)                    ;Drive D0
                addq.w  #1,D0
                move.w  D0,-(SP)                    ;Drive D0 + 1
                pea     a_mediach_buf(PC)
                move.w  #$47,-(SP)
                jsr     do_trap_1                   ;Dgetpath()
                addq.l  #8,SP
                lea     a_mediach_buf(PC),A0
do_mediach00:   tst.b   (A0)+                       ;Find the end of the path
                bne.s   do_mediach00
                clr.b   (A0)
                move.b  #'\',-(A0)                  ;and complete the path
                clr.w   -(SP)                       ;Sector 0 Read
                move.w  #1,-(SP)                    ;a sector
                move.l  A4,-(SP)
                addi.l  #allg_buffer,(SP)           ;bufferadresse
                clr.w   -(SP)                       ;Read with media test
                move.w  #4,-(SP)                    ;Rwabs()
                trap    #13
                lea     14(SP),SP
                move.w  a_mediach_drv(PC),D1        ;retrieve the drive
                tst.l   D0                          ;A mistake?
                bmi.s   do_mediach1                 ;Yes!=> Immediately a media change
                movea.l #allg_buffer,A0
                adda.l  A4,A0
                move.l  8(A0),D0                    ;The serial number of the boot sector
                lsl.w   #2,D1                       ;Drive times 4
                movea.l #drv_table,A0
                adda.l  A4,A0
                cmp.l   0(A0,D1.w),D0               ;Serial number same?
                beq.s   do_mediach3                 ;Yes!=> No media change => out
                move.l  D0,0(A0,D1.w)               ;Remember new serial number
                lsr.w   #2,D1                       ;Restore drive
do_mediach1:    add.b   #'A',D1
                move.b  D1,do_mediach10
                move.l  $0472.w,a_mediach_bpb
                move.l  $047E.w,a_mediach_med
                move.l  $0476.w,a_mediach_rw
                move.l  #do_mediach4,$0472.w
                move.l  #do_mediach6,$047E.w
                move.l  #do_mediach8,$0476.w
                clr.w   -(SP)
                pea     do_mediach10(PC)
                move.w  #$3D,-(SP)
                trap    #1                          ;Fopen()
                addq.w  #8,SP
                tst.l   D0
                bmi.s   do_mediach2
                move.w  D0,-(SP)
                move.w  #$3E,-(SP)

                trap    #1                          ;Fclose()
                addq.w  #4,SP
do_mediach2:    cmpi.l  #do_mediach4,$0472.w
                bne.s   do_mediach3
                move.l  a_mediach_bpb(PC),$0472.w
                move.l  a_mediach_med(PC),$047E.w
                move.l  a_mediach_rw(PC),$0476.w
do_mediach3:    move.w  #$19,-(SP)
                jsr     do_trap_1                   ;Dgetdrv()
                addq.l  #2,SP
                move.w  D0,-(SP)                    ;Rescue drive
                move.w  a_mediach_drv(PC),-(SP)
                move.w  #$0E,-(SP)
                jsr     do_trap_1                   ;Dsetdrv(Changedrive)
                addq.l  #4,SP
                pea     a_mediach_buf(PC)
                move.w  #$3B,-(SP)
                jsr     do_trap_1                   ;Dsetpath(OldPath)
                addq.l  #6,SP
                move.w  #$0E,-(SP)
                jsr     do_trap_1                   ;Dsetdrv(OldAktDrive)
                addq.l  #4,SP
                movem.l (SP)+,D0-D2/A0-A2
                rts

do_mediach4:    move.w  a_mediach_drv(PC),D0
                cmp.w   4(SP),D0
                bne.s   do_mediach5
                move.l  a_mediach_bpb(PC),$0472.w
                move.l  a_mediach_med(PC),$047E.w
                move.l  a_mediach_rw(PC),$0476.w
do_mediach5:    movea.l a_mediach_bpb(PC),A0
                jmp     (A0)

do_mediach6:    move.w  a_mediach_drv(PC),D0
                cmp.w   4(SP),D0
                bne.s   do_mediach7
                moveq   #2,D0
                rts
do_mediach7:    movea.l a_mediach_med(PC),A0
                jmp     (A0)

do_mediach8:    move.w  a_mediach_drv(PC),D0
                cmp.w   14(SP),D0
                bne.s   do_mediach9
                moveq   #-14,D0
                rts
do_mediach9:    movea.l a_mediach_rw(PC),A0
                jmp     (A0)

do_mediach10:   DC.B 'x:\X',0
                EVEN
a_mediach_drv:  DS.W 1
a_mediach_bpb:  DS.L 1
a_mediach_med:  DS.L 1
a_mediach_rw:   DS.L 1
a_mediach_buf:  DS.B 128
                ENDPART

********************************************************************************
* Extension of a command (length indicator) to D3.                             *
********************************************************************************
                PART 'get_extension'
get_extension:  cmp.b   #'.',D0
                bne.s   get_ex2                     ;no length
                cmpi.b  #' ',-2(A0)                 ;If Space before the decimal point, then label
                beq.s   get_ex2                     ;Nothing good, no length
                movem.l D0/A0,-(SP)
                bsr     get                         ;Read command length
                moveq   #3,D3
get_ex1:        cmp.b   ext_tab(PC,D3.w),D0
                beq.s   get_ex3                     ;found
                dbra    D3,get_ex1
                movem.l (SP)+,D0/A0                 ;Was a decimal number, pointer back
get_ex2:        moveq   #0,D3                       ;Byte for mem.x as default
                move    #$FF,CCR                    ;alle flags like
                rts
get_ex3:        addq.l  #8,SP
                bsr     get                         ;Get next character
                move    #0,CCR                      ;CCR set to zero, there found
                rts

ext_tab:        DC.B 'BW L'                         ;Byte/Word/Long (Space is not allowed)
                ENDPART

********************************************************************************
* Arithmetic                                                                   *
********************************************************************************
********************************************************************************
* Div-Long D1.L/D2.B -> D1.L  Rest to D3.W                                     *
********************************************************************************
                PART 'div'
div:            move.l  D1,D3                       ;Div divided D1.L by D2.B according to D1.L
                ext.w   D2                          ;Remaining in D3, D2 unchanged
                clr.w   D3
                swap    D3
                divu    D2,D3
                move.l  D4,-(SP)
                move.w  D3,D4
                move.w  D1,D3
                divu    D2,D3
                swap    D4
                move.w  D3,D4
                swap    D3
                move.l  D4,D1
                move.l  (SP)+,D4
                rts
                ENDPART

*******************************************************************************
* LONG-Division      : D2=D2/D1  D1=D2 MOD D1                                 *
*******************************************************************************
                PART 'ldiv'
ldiv:           movem.l D0/D3-D4,-(SP)
                tst.l   D1
                beq     illequa
                exg     D1,D2
                clr.w   D4
                tst.l   D1
                bge.s   ldiv1
                addq.w  #3,D4
                neg.l   D1
ldiv1:          tst.l   D2
                bge.s   ldiv2
                addq.w  #1,D4
                neg.l   D2
ldiv2:          moveq   #1,D3
ldiv4:          cmp.l   D1,D2
                bhs.s   ldiv3
                add.l   D2,D2
                add.l   D3,D3
                bra.s   ldiv4
ldiv3:          moveq   #0,D0
ldiv6:          cmp.l   D1,D2
                bhi.s   ldiv5
                or.l    D3,D0
                sub.l   D2,D1
ldiv5:          lsr.l   #1,D2
                lsr.l   #1,D3
                bhs.s   ldiv6
                cmp.w   #3,D4
                blt.s   ldiv7
                neg.l   D1
ldiv7:          lsr.l   #1,D4
                bcc.s   ldiv8
                neg.l   D0
ldiv8:          move.l  D0,D2
                movem.l (SP)+,D0/D3-D4
                rts
                ENDPART


********************************************************************************
* Long-Mult D2.L*D1.L -> D2.L                                                  *
********************************************************************************
                PART 'lmult'
lmult:          movem.l D0-D1/D4-D5,-(SP)
                moveq   #0,D0
                tst.l   D1                          ;Multiplikatior stets positiv
                bpl.s   lmult1
                addq.b  #1,D0
                neg.l   D1
lmult1:         tst.l   D2                          ;Multiplikant stets positiv
                bpl.s   lmult2
                addq.b  #1,D0
                neg.l   D2
lmult2:         move.l  D2,D4                       ;1.Faktor merken
                mulu    D1,D2                       ;low-words multiplizieren
                move.l  D4,D5                       ;1st Factor again
                swap    D4                          ;High des second Factor
                mulu    D1,D4
                swap    D4                          ;Score
                tst.w   D4                          ;Test higher Word
                bne     overfl
                add.l   D4,D2                       ;and add
                bcs     overfl
                move.l  D5,D4                       ;1st Factor reproduce
                swap    D1
                mulu    D1,D4                       ;h-word d3 times l-word d1
                swap    D4                          ;Result of swapping (as above)
                tst.w   D4                          ;Test the higher Word again
                bne     overfl
                add.l   D4,D2                       ;re-add
                bcs     overfl
                swap    D5                          ;second Factor h-word downward
                mulu    D1,D5                       ;h-words multiply
                bne     overfl                      ;non-zero, erg. > $ffffffff
                btst    #0,D0
                beq.s   lmult3
                neg.l   D2
lmult3:         movem.l (SP)+,D0-D1/D4-D5
                rts
                ENDPART

********************************************************************************
* Output error message                                                         *
********************************************************************************
                PART 'Fehler ausgeben'
dskfull:        tst.l   D0                          ;General error message?
                bmi.s   toserr
                moveq   #-117,D0                    ;Disk full
                bra.s   toserr
illdrv:         moveq   #-46,D0                     ;Illegal drive
                bra.s   toserr
timeouterr:     moveq   #-11,D0                     ;Read-Fault
                bra.s   toserr
seekerr:        moveq   #-6,D0                      ;Seek-Error
                bra.s   toserr
ioerr:          move.w  D0,-(SP)
                cmpi.w  #-17,(SP)                   ;For hardware errors
                bhs.s   ioerr3                      ;Do not close the file
                tst.w   _fhdle(A4)
                blo.s   ioerr3
                move.w  _fhdle(A4),-(SP)
                move.w  #$3E,-(SP)
                jsr     do_trap_1                   ;Fclose()
                addq.l  #4,SP
                bsr     do_mediach                  ;Media-Change trigger
ioerr3:         move.w  (SP)+,D0
toserr:         ext.w   D0
                ext.l   D0
                move.l  D0,D1
                clr.w   $043E.w                     ;Floppy-VBL released
                lea     terrtxt(PC),A0
                bra.s   toserr1
toserr2:        tst.b   (A0)+                       ;Error text overlooked
                bne.s   toserr2
toserr1:        tst.b   (A0)
                beq.s   toserr3                     ;Error not found (end of the table)
                cmp.b   (A0),D0
                bne.s   toserr2                     ;to the next mistake
toserr3:        addq.l  #1,A0                       ;Pointer to the error text
                tst.w   spalte(A4)
                beq.s   toserr31
                jsr     crout
toserr31:       clr.b   device(A4)                  ;Printer output from
                move.l  A0,-(SP)                    ;and remember
                moveq   #'-',D0
                jsr     @chrout(A4)
                neg.l   D1
                moveq   #10,D2                      ;Decimal system
                bsr     numoutx                     ;Output error number
                jsr     @space(A4)
                moveq   #':',D0
                jsr     @chrout(A4)
                jsr     @space(A4)
                jsr     @print_line(A4)
                bra.s   err1                        ;File access error
batch_mode_err: lea     batch_errtxt(PC),A0
                bra.s   err
illbkpt:        lea     ill_bkpt(PC),A0
                bra.s   err
prn_err:        lea     prn_e(PC),A0
                bra.s   err
overfl:         lea     errtab(PC),A0
                bra.s   err
synerr:         lea     syntax(PC),A0
                bra.s   err
int_err:        lea     interr(PC),A0
                bra.s   err
misbrak:        lea     _misbra(PC),A0
                bra.s   err
no_syms:        lea     no_symt(PC),A0
                bra.s   err
file_er:        lea     file_e(PC),A0
                bra.s   err
fileer2:        lea     file_e2(PC),A0
                bra.s   err
illlabel:       lea     _illlab(PC),A0
                bra.s   err
no_prg:         lea     no_prg_(PC),A0
                bra.s   err
noallow:        lea     n_allow(PC),A0
                bra.s   err
no_bef:         lea     no_befx(PC),A0
                bra.s   err
illequa:        lea     illeqa(PC),A0
err:            tst.b   err_flag(A4)
                beq.s   err2
                movea.l err_stk(A4),SP
                jmp     mausc9z
err2:           clr.b   device(A4)                  ;Switch off printer edition
                move.w  zeile(A4),D0
                jsr     write_line
err1:           jsr     @crout(A4)
                jsr     clr_keybuff                 ;Keyboard buffer empty
                sf      do_resident(A4)             ;Delete auto-resident
                bra     all_normal

                SWITCH language
                CASE 0
batch_errtxt:   DC.B '?Im Batch-Mode nicht erlaubt',0
no_befx:        DC.B '?Unbekannter Befehl',0
interr:         DC.B '?Interner Fehler (Bitte Eingabe notieren!)',0
n_allow:        DC.B '?Nicht erlaubt',0
ill_bkpt:       DC.B '?Illegaler Breakpoint',0
errtab:         DC.B '?Überlauf',0
syntax:         DC.B '?Syntax-Fehler',0
illeqa:         DC.B '?Wert nicht erlaubt',0
_misbra:        DC.B '?Klammer fehlt',0
_illlab:        DC.B '?Label existiert nicht',0
no_symt:        DC.B '?Keine Symboltabelle',0
prn_e:          DC.B '?Welcher Drucker',0
file_e:         DC.B '?Datei nicht mit FOPEN geöffnet',0
file_e2:        DC.B '?Datei wurde bereits geöffnet',0
no_prg_:        DC.B '?Es ist kein Programm geladen',0
terrtxt:        DC.B -1,'Error',0
                DC.B -2,'Drive not ready',0
                DC.B -3,'Unknown command',0
                DC.B -4,'CRC-error',0
                DC.B -5,'Bad request',0
                DC.B -6,'Seek error',0
                DC.B -7,'Unknown media',0
                DC.B -8,'Sector not found',0
                DC.B -9,'No paper',0
                DC.B -10,'Write fault',0
                DC.B -11,'Read fault',0
                DC.B -12,'General mishap',0
                DC.B -13,'Write protect',0
                DC.B -14,'Media change',0
                DC.B -15,'Unknown device',0
                DC.B -16,'Bad sectors',0
                DC.B -17,'Insert disk',0
                DC.B -32,'EINVFN',0
                DC.B -33,'Datei nicht gefunden',0
                DC.B -34,'Pfad nicht gefunden',0
                DC.B -35,'ENHNDL',0
                DC.B -36,'Zugriff verwährt',0
                DC.B -37,'EIHNDL',0
                DC.B -39,'Speicher voll',0
                DC.B -40,'EIMBA',0
                DC.B -46,'Illegales Laufwerk',0
                DC.B -48,'ENSAME',0
                DC.B -49,'ENMFIL',0
                DC.B -64,'ERANGE',0
                DC.B -65,'EINTRN',0
                DC.B -66,'Illegales Programmformat',0
                DC.B -67,'EGSBF',0
                DC.B -117,'Disk voll',0
                DC.B -118,'Datei zu kurz',0
                DC.B 0,'Unbekannter TOS Fehler',0

                CASE 1
batch_errtxt:   DC.B '?Not allowed in batch-mode',0
no_befx:        DC.B '?Unknown Command',0
interr:         DC.B '?Internal Error (Write down your input!)',0
n_allow:        DC.B '?Not allowed',0
ill_bkpt:       DC.B '?Illegal Breakpoint',0
errtab:         DC.B '?Overflow',0
syntax:         DC.B '?Syntax error',0
illeqa:         DC.B '?Value not allowed',0
_misbra:        DC.B '?Braket missing',0
_illlab:        DC.B "?Label does not exist",0
no_symt:        DC.B '?No Symbol table',0
prn_e:          DC.B '?No Printer',0
file_e:         DC.B "?Can't open file with FOPEN",0
file_e2:        DC.B '?file already opened',0
no_prg_:        DC.B '?No program',0
terrtxt:        DC.B -1,'Error',0
                DC.B -2,'Drive not ready',0
                DC.B -3,'Unknown command',0
                DC.B -4,'CRC-error',0
                DC.B -5,'Bad request',0
                DC.B -6,'Seek error',0
                DC.B -7,'Unknown media',0
                DC.B -8,'Sector not found',0
                DC.B -9,'No paper',0
                DC.B -10,'Write fault',0
                DC.B -11,'Read fault',0
                DC.B -12,'General mishap',0
                DC.B -13,'Write protect',0
                DC.B -14,'Media change',0
                DC.B -15,'Unknown device',0
                DC.B -16,'Bad sectors',0
                DC.B -17,'Insert disk',0
                DC.B -32,'EINVFN',0
                DC.B -33,'File not found',0
                DC.B -34,'Path not found',0
                DC.B -35,'ENHNDL',0
                DC.B -36,'Access denied',0
                DC.B -37,'EIHNDL',0
                DC.B -39,'Memory full',0
                DC.B -40,'EIMBA',0
                DC.B -46,'Illegal Drive',0
                DC.B -48,'ENSAME',0
                DC.B -49,'ENMFIL',0
                DC.B -64,'ERANGE',0
                DC.B -65,'EINTRN',0
                DC.B -66,'Illegal program format',0
                DC.B -67,'EGSBF',0
                DC.B -117,'Disk full',0
                DC.B -118,'File too short',0
                DC.B 0,'Unknown TOS-Error',0
                ENDS
                EVEN
                ENDPART

********************************************************************************
* 68020/30/40 Clear Cache                                                      *
********************************************************************************
                PART 'clr_cache'
clr_cache:      movem.l D0/A0/A4-A6,-(SP)
                move    SR,-(SP)
                movea.l SP,A6                       ;SP right
                lea     $10.w,A5
                movea.l (A5),A4                     ;Illegal-vector save
                lea     clr_cache1(PC),A0
                move.l  A0,(A5)                     ;Insert new
                ori     #$0700,SR
                DC.L $4E7A0002                      ;MOVE CACR,D0
                or.w    #$0808,D0                   ;Clearing the Cache
                DC.L $4E7B0002                      ;MOVE D0,CACR
clr_cache1:     move.l  A4,(A5)                     ;Illegal vector back
                movea.l A6,SP                       ;Spew back
                move    (SP)+,SR
                movem.l (SP)+,D0/A0/A4-A6
                rts
                ENDPART

********************************************************************************
* The individual commands                                                      *
********************************************************************************
********************************************************************************
* OVERSCAN - OverScan-Switch up                                                *
********************************************************************************
                PART 'cmd_overscan'
cmd_overscan:   move.w  #4200,-(SP)
                trap    #14                         ;OverScan()
                addq.l  #2,SP
                cmp.w   #4200,D0                    ;OverScan software available?
                beq.s   cmd_overscan1               ;No!=> exit
                jsr     @page2(A4)
                move.l  #$106EFFFF,-(SP)
                trap    #14                         ;OverScanSwitch (-1: Query mode)
                addq.l  #4,SP
                bchg    #0,D0                       ;Toggle mode
                move.w  D0,-(SP)
                move.w  #$106E,-(SP)
                trap    #14                         ;OverScanSwitch (newMode)
                addq.l  #2,SP
                move.b  #$96,$FFFFFC00.w            ;OverScan immediately again
                jsr     @page1(A4)
                moveq   #1,D0
                and.w   (SP)+,D0
                move.w  D0,overscan(A4)             ;OverScan-Replace flag
cmd_overscan1:  jmp     (A4)
                ENDPART

********************************************************************************
* COOKIE - Cookie-Jar Show                                                     *
********************************************************************************
                PART 'cmd_cookie'
cmd_cookie:     move.l  $05A0.w,D0                  ;Get Cookie-Ptr
                bne.s   cmd_cookie1                 ;Everything ok =>
                pea     no_cookie(PC)
                jsr     @print_line(A4)             ;Cookie not available ...
                jmp     (A4)
cmd_cookie1:    movea.l D0,A3                       ;Cookie-Ptr notice
                pea     cookie_init(PC)
                jsr     @print_line(A4)
cmd_cookie2:    tst.l   (A3)                        ;End there list?
                beq.s   cmd_cookiex                 ;Yes!=>
                pea     cookie_1(PC)
                jsr     @print_line(A4)
                move.b  (A3)+,D0
                jsr     @chrout(A4)
                move.b  (A3)+,D0
                jsr     @chrout(A4)                 ;Spend name
                move.b  (A3)+,D0
                jsr     @chrout(A4)
                move.b  (A3)+,D0
                jsr     @chrout(A4)
                pea     cookie_2(PC)
                jsr     @print_line(A4)
                move.l  (A3)+,D1
                bsr     hexlout
                bra.s   cmd_cookie2
cmd_cookiex:    pea     cookie_end(PC)
                jsr     @print_line(A4)
                jmp     (A4)

                SWITCH language
                CASE 0
no_cookie:      DC.B '?kein Cookie-Jar vorhanden',13,0
cookie_init:    DC.B 'Cookie-Jar:',13
                DC.B 'ˇˇˇˇˇˇˇˇˇˇˇ',0
cookie_1:       DC.B 13,'Name : "',0
cookie_2:       DC.B '" = $',0
cookie_end:     DC.B 13,0

                CASE 1
no_cookie:      DC.B '?no cookie-jar',13,0
cookie_init:    DC.B 'cookie-jar:',13
                DC.B '-----------',0
cookie_1:       DC.B 13,'name : "',0
cookie_2:       DC.B '" = $',0
cookie_end:     DC.B 13,0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* #cmd - Batch-Subcommand                                                      *
********************************************************************************
                PART 'cmd_number'
cmd_number:     moveq   #3,D2                       ;Get 4 characters
cmd_numberloop: lsl.l   #8,D1
                move.b  (A0)+,D0                    ;Get big letters
                cmp.b   #$20,D0
                bhs.s   cmd_numberloop1             ;CR or LF? No =>
                moveq   #' ',D0                     ;space
                bra.s   cmd_numberloop2
cmd_numberloop1:and.w   #$DF,D0
cmd_numberloop2:or.b    D0,D1                       ;and to the others
                dbra    D2,cmd_numberloop
cmd_numberloop3:moveq   #$DF,D0
                and.b   (A0),D0                     ;Follow
                cmp.b   #'A',D0
                blo.s   cmd_number1                 ;Still a letter?
                cmp.b   #'Z',D0
                bhi.s   cmd_number1                 ;No!=>
                addq.l  #1,A0                       ;Ignore letters
                bra.s   cmd_numberloop3             ;and further
cmd_number1:    lea     cmd_num_table-2(PC),A1
cmd_number2:    addq.l  #2,A1                       ;Jump
                move.l  (A1)+,D0                    ;Command from the table
                bmi     synerr                      ;End of the table => Error
                cmp.l   D0,D1                       ;Command found?
                bne.s   cmd_number2                 ;No!=> Next
                adda.w  (A1),A1                     ;Calculate command address
                jsr     (A1)                        ;Command start
                st      batch_flag(A4)              ;batch Mode
                jmp     (A4)                        ;and back again

                BASE DC.W,*
cmd_num_table:  DC.L 'LOAD'                         ;Load batch file
                DC.W cmd_num_load
                DC.L 'END '                         ;End of the batch file
                DC.W cmd_num_end
                DC.B -1
                EVEN

cmd_num_end:    clr.l   input_pnt(A4)               ;Reset batch pointer
                sf      batch_flag(A4)              ;Flag for e.g. delete "you"
                jmp     (A4)                        ;and in the main loop

cmd_num_load:   bsr     getnam                      ;Get filenames
                movea.l #allg_buffer,A6
                adda.l  A4,A6                       ;Read in the 10K buffer
                movea.l #allg_buf_end,A5
                adda.l  A4,A5                       ;Pointer to the buffer end
                bsr     _clear                      ;Clear the buffer
                move.l  A5,-(SP)
                jsr     readimg                     ;Read the batch file
                movea.l (SP)+,A5
                move.l  A6,input_pnt(A4)            ;Set input line
                movea.l A6,A0
cmd_num_load1:  cmpi.b  #'%',(A0)                   ;Commenting line?
                bne.s   cmd_num_load3               ;No!=>
cmd_num_load2:  move.b  #':',(A0)+
                cmpi.b  #$20,(A0)                   ;cr/lf?
                bhs.s   cmd_num_load2               ;No!=>
                addq.l  #1,A0                       ;Pointer to the LF
                bra.s   cmd_num_load4
cmd_num_load3:  cmpi.b  #$20,(A0)+                  ;Search control characters in the buffer
                bhs.s   cmd_num_load1               ;No control character =>
cmd_num_load4:  move.b  -(A0),D0
                beq.s   cmd_num_load5               ;buffer  end =>
                move.b  #':',(A0)+                  ;Replace characters by separator
                bra.s   cmd_num_load1               ;and continue...
cmd_num_load5:  rts
                ENDPART

********************************************************************************
* HELP - Spend all commands                                                    *
********************************************************************************
                PART 'cmd_help'
cmd_help:       lea     cmdtab(PC),A6
cmd_help1:      move.b  (A6)+,D0
                bmi.s   cmd_help4
                beq.s   cmd_help2
cmd_help3:      jsr     @chrout(A4)
                bra.s   cmd_help1
cmd_help2:      moveq   #' ',D0
                bra.s   cmd_help3
cmd_help4:      jsr     @crout(A4)
                jmp     (A4)
                ENDPART

********************************************************************************
* & - noch nix tun                                                             *
********************************************************************************
                PART 'cmd_und'
cmd_und:        jmp     (A4)
                ENDPART

********************************************************************************
* LABELBASE P/S - Set the label base (program / segment)                       *
********************************************************************************
                PART 'cmd_labelbase'
cmd_labelbase:  bsr     get
                cmp.b   #'S',D0
                beq.s   cmd_labelbase1
                cmp.b   #'P',D0
                bne     syn_err
                moveq   #$18,D1                     ;Symbols also Data & BSS relative
                moveq   #$10,D2
                lea     cmd_labelbase3(PC),A0
                bra.s   cmd_labelbase2
cmd_labelbase1: moveq   #8,D1                       ;Symbols always TEXT relative
                moveq   #8,D2
                lea     cmd_labelbase4(PC),A0
cmd_labelbase2: move.b  D1,reloc_symbols12+1
                move.b  D2,reloc_symbols13+1

                SWITCH language
                CASE 0
                pea     cmd_labelbase5(PC)
                jsr     @print_line(A4)
                move.l  A0,-(SP)
                jsr     @print_line(A4)
                pea     cmd_labelbase6(PC)
                jsr     @print_line(A4)
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                jmp     (A4)
cmd_labelbase3: DC.B 'programm',0
cmd_labelbase4: DC.B 'segment',0
cmd_labelbase5: DC.B 'Symbolformat nun ',0
cmd_labelbase6: DC.B 'relativ.',0

                CASE 1
                pea     cmd_labelbase5(PC)
                jsr     @print_line(A4)
                move.l  A0,-(SP)
                jsr     @print_line(A4)
                pea     cmd_labelbase6(PC)
                jsr     @print_line(A4)
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                jmp     (A4)
cmd_labelbase3: DC.B 'program',0
cmd_labelbase4: DC.B 'segment',0
cmd_labelbase5: DC.B 'Symbol format now ',0
cmd_labelbase6: DC.B 'relative.',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* SHOWMEMORY formula[,[.number|[B|W|L][,][size]]]                               *
********************************************************************************
                PART 'cmd_showmem'
cmd_showmem:    movea.l A0,A1
                bsr     get
                cmp.b   #',',D0
                beq.s   cmd_showmem0
                movea.l A1,A0
                move.w  upper_line(A4),D1
                cmpi.w  #15,D1
                beq     ret_jump                    ;No further lines possible
                subq.w  #5,D1
                movea.l #spez_format,A1
                adda.l  A4,A1
                adda.w  D1,A1
                move.w  def_size(A4),D2
                beq.s   cmd_showmem00
                subq.b  #1,D2
cmd_showmem00:  lsl.b   #4,D2                       ;Set size
                move.b  D2,(A1)                     ;
                lsl.w   #8,D1                       ;* 256 Bytes (pro formula)
                movea.l #spez_buff,A1
                adda.l  A4,A1
                adda.w  D1,A1
                bsr     convert_formel
                tst.b   D0
                beq     cmd_showmem1
                cmp.b   #',',D0                     ;Follow parameters
                bne     syn_err
cmd_showmem0:   bsr     get
                cmp.b   #'.',D0                     ;Delete Entry?
                bne.s   cmd_showmem4
                bsr     get_term                    ;Get number
                moveq   #0,D0
                move.w  upper_line(A4),D0
                subq.w  #6,D0
                bmi     illequa
                cmp.l   D0,D1
                bhi     illequa                     ;Term> Max.Intrag

                movea.l #spez_format,A1
                adda.l  A4,A1
                lea     9(A1),A2
                adda.w  D1,A1
cmd_showmem21:  cmpa.l  A2,A1
                bhs.s   cmd_showmem22
                move.b  1(A1),(A1)+
                bra.s   cmd_showmem21
cmd_showmem22:  lsl.w   #8,D1
                movea.l #spez_buff,A1
                adda.l  A4,A1
                lea     $0900(A1),A2                ;Pointer to the last buffer element
                adda.w  D1,A1                       ;Address of the entry
                lea     $0100(A1),A3
cmd_showmem2:   cmpa.l  A2,A3
                bhs.s   cmd_showmem3
                move.l  (A3)+,(A1)+
                move.l  (A3)+,(A1)+
                move.l  (A3)+,(A1)+
                move.l  (A3)+,(A1)+
                bra.s   cmd_showmem2
cmd_showmem3:   subq.w  #1,upper_line(A4)
                addq.w  #1,down_lines(A4)
                subi.w  #80,upper_offset(A4)
                move.l  zeile(A4),-(SP)
                clr.l   zeile(A4)
                jsr     c_clrli
                move.l  (SP)+,zeile(A4)
                bsr     rgout
                jmp     (A4)

cmd_showmem4:   moveq   #0,D2                       ;Byte ist Default
                cmp.b   #'B',D0
                beq.s   cmd_showmem7
                cmp.b   #'W',D0
                bne.s   cmd_showmem6
                moveq   #1,D2                       ;Word
                bra.s   cmd_showmem7
cmd_showmem6:   cmp.b   #'L',D0
                bne.s   cmd_showmem5
                moveq   #3,D2                       ;Long
cmd_showmem7:   bsr     get
cmd_showmem5:   move.w  upper_line(A4),D1
                subq.w  #5,D1
                movea.l #spez_format,A1
                adda.l  A4,A1
                adda.w  D1,A1
                or.b    D2,(A1)                     ;Specify the output width
                tst.w   D0
                beq.s   cmd_showmem1
                cmp.b   #',',D0
                bne.s   cmd_showmem8                ;Comma
                bsr     get
cmd_showmem8:   bsr     get_term                    ;Get size
                moveq   #$10,D2
                cmp.l   D2,D1
                bhi     illequa                     ;1-16 is allowed!
                tst.l   D1
                beq     illequa
                subq.b  #1,D1
                lsl.b   #4,D1
                andi.b  #3,(A1)
                or.b    D1,(A1)                     ;Insert size value (bit 4-7)
cmd_showmem1:   jsr     scroll_dn                   ;Screen down screen
                addq.w  #1,upper_line(A4)
                subq.w  #1,down_lines(A4)
                addi.w  #80,upper_offset(A4)
                bsr     rgout                       ;Provide tab list
                jmp     (A4)
                ENDPART

********************************************************************************
* CLR [startadr,Endadr] -Delete memory area                                    *
********************************************************************************
                PART 'cmd_clr'
cmd_clr:        move.l  A0,-(SP)
                lea     clr_text(PC),A0
                jsr     ask_user                    ;Security query
                movea.l (SP)+,A0
                movea.l first_free(A4),A5           ;From here is deleted
                movea.l end_of_mem(A4),A6           ;Exactly until here
                bsr     get
                beq.s   cmd_clr2                    ;No parameters => Clear all
                cmp.b   #',',D0
                beq.s   cmd_clr0
                bsr     get_term                    ;Get initial address
                movea.l D1,A5
                cmp.b   #',',D0
                bne.s   cmd_clr1
cmd_clr0:       bsr     get
                bsr     get_term                    ;Get end address
                movea.l D1,A6
                bra.s   cmd_clr1
cmd_clr2:       bsr     kill_programm               ;To prevent Sören's crash
cmd_clr1:       move.l  A6,D1
                beq.s   cmd_clr3                    ;End address is missing!=>
                bsr.s   _clear                      ;and delete ...
cmd_clr3:       jmp     (A4)

                SWITCH language
                CASE 0
clr_text:       DC.B 'Wollen Sie löschen? (j/n) ',0
                CASE 1
clr_text:       DC.B 'Execute CLR? (y/n) ',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* clear(a5,a6) - fast-clear TOS                                                *
********************************************************************************
                PART '_clear'
_clear:         ori     #$0700,SR
                movem.l D0-D7/A2-A3,-(SP)
                cmpa.l  A5,A6
                blo.s   _clear4
                moveq   #0,D1
                moveq   #0,D2
                moveq   #0,D3
                moveq   #0,D4
                moveq   #0,D5
                moveq   #0,D6
                moveq   #0,D7
                movea.l D7,A3
                move.l  A5,D0
                btst    #0,D0
                beq.s   _clear1
                move.b  D1,(A5)+
_clear1:        move.l  A6,D0
                sub.l   A5,D0
                clr.b   D0
                tst.l   D0
                beq.s   _clear3
                lea     0(A5,D0.l),A5
                movea.l A5,A2
                lsr.l   #8,D0
_clear2:        movem.l D1-D7/A3,-(A2)              ;256 Byte Clear
                movem.l D1-D7/A3,-(A2)
                movem.l D1-D7/A3,-(A2)
                movem.l D1-D7/A3,-(A2)
                movem.l D1-D7/A3,-(A2)
                movem.l D1-D7/A3,-(A2)
                movem.l D1-D7/A3,-(A2)
                movem.l D1-D7/A3,-(A2)
                subq.l  #1,D0
                bne.s   _clear2
_clear3:        cmpa.l  A5,A6
                beq.s   _clear4
                move.b  D1,(A5)+
                bra.s   _clear3
_clear4:        movem.l (SP)+,D0-D7/A2-A3
                rts
                ENDPART

********************************************************************************
* @Befehl - Command in the car command buffer                                  *
********************************************************************************
                PART 'cmd_atsign'
cmd_atsign:     tst.b   (A0)                        ;Empty input
                beq.s   cmd_atsign2                 ;Then delete the batch command
                lea     _zeile3(A4),A1
                move.b  #'@',(A1)+
cmd_atsign1:    move.b  (A0)+,(A1)+                 ;Zeile in Buffer
                bne.s   cmd_atsign1
                jmp     (A4)
cmd_atsign2:    clr.l   _zeile3(A4)                 ;Delete batch command
                jmp     (A4)
                ENDPART

********************************************************************************
* RWABS - Read / write sectors                                                 *
********************************************************************************
                PART 'cmd_rwabs'
cmd_rwabs:      bsr     get
                beq     syn_err
                bsr     get_term                    ;Reading / write flag
                bsr     chkcom                      ;Does a comma also follow?
                moveq   #15,D2
                cmp.l   D2,D1
                bhi     illequa                     ;> 15 => Error
                move.w  D1,D7                       ;Flag
                bsr     get_term
                bsr     chkcom                      ;Does a comma also follow?
                movea.l D1,A6                       ;Remember buffer address
                bsr     get_term
                bsr     chkcom                      ;Does a comma also follow?
                swap    D1                          ;Number of sector> 65535?
                tst.w   D1
                bne     illequa
                swap    D1
                move.w  D1,D6                       ;Sector number
                bsr     get_term
                bsr     chkcom                      ;Does a comma also follow?
                swap    D1                          ;Start sector> 65535?
                tst.w   D1
                bne     illequa
                swap    D1
                move.w  D1,D5                       ;Remember start sector
                bsr     get_term                    ;Get drive
                moveq   #15,D2
                cmp.l   D2,D1
                bhi     illequa                     ;>15 => failure
                moveq   #1,D0
                jsr     graf_mouse                  ;Mouse pointer to the floppy disk
                move.w  D1,-(SP)                    ;driv
                move.w  D5,-(SP)                    ;recn
                move.w  D6,-(SP)                    ;secn
                move.l  A6,-(SP)                    ;buf
                move.w  D7,-(SP)                    ;rwfl
                move.w  #4,-(SP)                    ;rwabs()
                trap    #13
                lea     14(SP),SP
                tst.l   D0
                bmi     toserr                      ;That was probably nothing
                jmp     (A4)
                ENDPART

********************************************************************************
* SHOW Filename - ASCII-Show file                                              *
********************************************************************************
                PART 'cmd_type'
cmd_type:       bsr     get
                beq.s   cmd_type7                   ;Default File Show
                bsr     getnam_cont                 ;Get Filename

cmd_type7:      lea     fname(A4),A0
                tst.b   (A0)
                beq     synerr                      ;No file / orndername specified
                moveq   #1,D0
                jsr     graf_mouse                  ;Turn the disk on
                bsr     fopen                       ;open file
                move.b  ins_mode(A4),D5             ;Insert-Mode-Flag notice
                sf      ins_mode(A4)                ;INSERT MODE OF
cmd_type0:      movea.l #allg_buffer,A6             ;Buffer for 8K text
                adda.l  A4,A6
                move.l  #8192,D1                    ;Read 8192 bytes at once
                bsr     fread                       ;and 8K read
                cmp.l   D1,D0
                seq     D7                          ;D7<>0, If file ends are not yet reached
                move.w  D0,D1                       ;Number of bytes actually read
                beq.s   cmd_type5
                subq.w  #1,D1
cmd_type1:      move.b  (A6)+,D0                    ;Bring characters from the buffer
                cmp.b   #10,D0                      ;Ignore LF
                beq.s   cmd_type3
                cmp.b   #13,D0                      ;CR execute
                beq.s   cmd_type2
                cmp.b   #9,D0                       ;Tab
                beq.s   cmd_type6
                jsr     @chrout(A4)                 ;"normales" Spend characters
                moveq   #0,D6
                bra.s   cmd_type3
cmd_type6:      jsr     c_tab                       ;Tab carry out
                bra.s   cmd_type3
cmd_type2:      jsr     @c_eol(A4)
                jsr     @crout(A4)                  ;Delete line residue & delete CR
                moveq   #-1,D6
cmd_type3:      jsr     check_keyb                  ;Button pressed?
                bmi.s   cmd_type4                   ;yes!
                dbra    D1,cmd_type1                ;Buffer already empty?
                tst.b   D7
                bne.s   cmd_type0
cmd_type4:      tst.w   D6                          ;Text with CR completed?
                bne.s   cmd_type5                   ;Then no CR spend more
                jsr     @c_eol(A4)
                jsr     @crout(A4)                  ;again a CR as a conclusion
cmd_type5:      move.b  D5,ins_mode(A4)             ;Insert-Mode-Flag return
                bsr     fclose                      ;Close file again
                jmp     (A4)
                ENDPART

********************************************************************************
* SYNC - Switch synchronization between 50Hz and 60Hz                          *
********************************************************************************
                PART 'cmd_sync'
cmd_sync:       lea     user_scr(A4),A0
                bchg    #1,scr_sync(A0)             ;Synchronization frequency change
                jmp     (A4)
                ENDPART

********************************************************************************
* |Mnemonic - execute order                                                    *
********************************************************************************
                PART 'cmd_dobef'
cmd_dobef:      st      ignore_autocrlf(A4)         ;CR/LF to suppress the function
                movea.l A0,A1
                bsr     get
                movea.l A1,A0
                beq     ret_jump                    ;End, since empty input
                move.b  #2,find_cont0(A4)
                lea     data_buff(A4),A6            ;Intermediate buffer for the code
                move.l  default_adr(A4),(A6)+       ;Default-Adr notice
                move.l  _pc(A4),(A6)+               ;PC RIGHT
                bsr.s   code_line                   ;Lines of isgrades
                lea     data_buff+8(A4),A6          ;Intermediate buffer for the code
                move.l  A6,_pc(A4)                  ;Set temporary PC
                bsr     get_dlen                    ;Determine command length
                move.l  A6,breakpnt+12*16(A4)       ;Break #16 put
                move.w  #-1,breakpnt+12*16+4(A4)    ;stopBreakpoint
                clr.l   breakpnt+12*16+6(A4)        ;run only once
                st      dobef_flag(A4)              ;Flag for the exception handler
                bra     go_pc
                ENDPART

********************************************************************************
* !Mnemonic - Line-Assembler                                                   *
********************************************************************************
                PART 'cmd_assem'
cmd_assem:      st      ignore_autocrlf(A4)         ;CR/LF to suppress the function
                movea.l default_adr(A4),A6          ;after A6 is asleep
                movea.l A0,A1
                bsr     get
                movea.l A1,A0
                beq     ret_jump                    ;End, since empty input
                bsr.s   code_line                   ;Lines of isgrades
                subq.w  #1,zeile(A4)                ;and a line back
                st      list_flg(A4)                ;Issue symbolic
                movea.l default_adr(A4),A6
                bsr     do_disass                   ;Output line again
                move.l  A6,default_adr(A4)
                jsr     @crout(A4)                  ;and connect with a CR
                st      assm_flag(A4)               ;Enter with the line assembler
                jmp     (A4)
                ENDPART

********************************************************************************
* Mnemonic Assemble from A0 to A6                                              *
********************************************************************************
                PART 'code_line'
code_line:      movem.l D1-D7/A1-A5,-(SP)
                move.l  SP,D7                       ;Secure the return address
                bclr    #0,default_adr+3(A4)        ;So that it is certainly straight!
                moveq   #0,D5                       ;Number of the operant
                lea     op_buffer(A4),A3            ;A3 shows on line info
                lea     16(A3),A2                   ;A2 shows on operants of the opcodes
                moveq   #0,D0
                move.w  D0,(A3)                     ;Clear buffer
                move.l  D0,14(A3)
                move.l  D0,18(A3)
                move.l  D0,22(A3)
code_l3:        bsr     cut_space
                cmpi.b  #'?',(A0)                   ;'???'
                beq.s   code_l1
                bsr     search                      ;Find command in the table
                move.l  A1,D2
                bne.s   code_l5                     ;equal to zero, unknown command
code_l4:        move.b  (A0)+,D0
                beq.s   syntax_error                ;Line ends reach!
                cmp.b   #':',D0                     ;Label over
                bne.s   code_l4
                cmpi.b  #':',(A0)
                bne.s   code_l3                     ;2.Topplace for global?
                addq.l  #1,A0                       ;overlook
                bra.s   code_l3
code_l5:        bsr     cut_space
                moveq   #0,D2
                moveq   #0,D0
                move.b  (A0),D0                     ;Next sign
                move.w  14(A1),14(A3)               ;Write command bits in buffer
                move.w  #2,(A3)                     ;Put the length of the line to 2
                move.w  12(A1),D4                   ;Data for immediate operants
                movea.l 8(A1),A1                    ;Address the routine
                jsr     (A1)                        ;Jump to the operative evaluation
                bsr     get                         ;Follow
                move.w  (A3),D1                     ;Length of the command
                lea     14(A3),A3                   ;From here lies the command
                subq.w  #1,D1
code_l0:        move.b  (A3)+,(A6)+                 ;Copy command
                dbra    D1,code_l0
code_l2:        movem.l (SP)+,D1-D7/A1-A5
                rts
code_l1:        addq.l  #2,A6                       ;2 Byte - '???'-command
                bra.s   code_l2                     ;that's it

operant_err:    cmp.w   #-5,D0
                bne.s   unknow_error
                movea.l D7,SP
                movem.l (SP)+,D1-D7/A1-A6
                bra     overfl                      ;Overflow
unknow_error:
syntax_error:   movea.l D7,SP
                movem.l (SP)+,D1-D7/A1-A6
                bra     synerr                      ;false sign

;************************************************************************
;* Operant checks of the commands                                       *
;************************************************************************
t_abcd:         cmp.b   #'D',D0
                beq.s   t_abcd2
                cmp.b   #'d',D0
                beq.s   t_abcd2
                cmp.b   #'-',D0
                bne     op_error
                bsr     get_indirect
                bset    #3,D0
                or.b    D0,15(A3)
                bsr     chk_com
                cmpi.b  #'-',(A0)
                bne     op_error
                bsr     get_indirect
t_abcd3:        add.w   D0,D0
                or.b    D0,14(A3)
                rts
t_abcd2:        bsr     get_regnr
                or.b    D0,15(A3)
                bsr     chk_com
                bsr     get_datareg
                bmi     op_error
                bra.s   t_abcd3

t_add:          move.w  #$1C00,D1
                bsr     get_ea
                bsr     chk_com
                bsr     get_adrreg
                bpl.s   t_add4
                bsr     get_datareg
                bmi.s   t_add2
                move.w  14(A3),D1                   ;Get Opcode
                andi.w  #$3F,D1                     ;EA mask
                cmp.w   #$3C,D1                     ;Source operand immediate?
                beq.s   t_add6                      ;yes!
t_add3:         add.w   D0,D0
                or.b    D0,14(A3)
                rts
t_add6:         move.b  14(A3),D1                   ;ADDI #xx,xx
                rol.b   #3,D1                       ;ADD to ADDI and SUB to SUBI convert
                andi.b  #7,D1
                move.b  D1,14(A3)
                andi.b  #$C0,15(A3)
                or.b    D0,15(A3)                   ;Insert the data register
                rts
t_add2:         move.w  14(A3),D0                   ;Get Opcode
                andi.w  #$3F,D0                     ;EA mask
                cmp.w   #$3C,D0                     ;Source operand immediate?
                beq.s   t_add5                      ;yes!
                move.w  #$1F03,D1                   ;ADD Dx,xx
                move.w  14(A3),D0
                andi.w  #$FFC0,14(A3)
                bsr     get_ea
                andi.w  #$3F,D0
                cmp.w   #7,D0
                bgt.s   op_error
                add.w   D0,D0
                bset    #0,D0
                or.b    D0,14(A3)
                rts
t_add4:         move.w  14(A3),D1                   ;ADDA xx,Ax
                add.w   D1,D1
                andi.w  #$0100,D1
                ori.w   #$C0,D1
                or.w    D1,14(A3)
                bra.s   t_add3
t_add5:         move.b  14(A3),D0                   ;ADDI #xx,xx
                rol.b   #3,D0                       ;ADD to ADDI and SUB to SUBI convert
                andi.b  #7,D0
                move.b  D0,14(A3)
                move.w  #$1F02,D1
                bsr     get_ea
                rts

chk_com:        bsr     cut_space
                cmpi.b  #',',(A0)+
                bne.s   chk_com2
                moveq   #2,D5
                bsr     cut_space
                rts
chk_com2:       moveq   #-9,D0
                bra     operant_err

op_error:       moveq   #-7,D0
                bra     operant_err

t_adda:         move.w  #$1C00,D1
                bsr     get_ea
                bsr.s   chk_com
                bsr     get_adrreg
                bmi.s   op_error
                add.w   D0,D0
                or.b    D0,14(A3)
                rts

t_addi:         cmpi.b  #'#',(A0)+
                bne.s   op_error
                bsr     get_wert
                bsr     set_imidiate
                bsr.s   chk_com
                move.w  #$1F02,D1
                bsr     get_ea
                rts

t_addq:         cmpi.b  #'#',(A0)+
                bne.s   op_error
                bsr     get_wert
                tst.b   2(A3,D5.w)
                bne.s   t_addq3
                tst.l   D3
                beq     val_error
                cmp.l   #8,D3
                bhi     val_error
                andi.w  #7,D3
                add.b   D3,D3
                or.b    D3,14(A3)
t_addq2:        bsr.s   chk_com
                move.w  #$1F00,D1
                bsr     get_ea
                rts
t_addq3:        bsr     get_quick
                bra.s   t_addq2

t_and:          move.w  #$1C02,D1
                bsr     get_ea
                move.w  14(A3),D0                   ;Get EA
                andi.w  #$3F,D0
                cmp.w   #$3C,D0                     ;Source operator = immediate
                beq.s   t_and3                      ;Yes !
                bsr     chk_com
                bsr     get_datareg
                bmi     t_add2
                add.b   D0,D0
                or.b    D0,14(A3)
                rts
t_and3:         bsr     chk_com
                move.b  14(A3),D0
                ror.b   #5,D0
                move.b  D0,14(A3)
                andi.w  #$02C0,14(A3)               ;AND to ANDI and OR to ORI convert
                bra.s   t_andi2

t_andi:         cmpi.b  #'#',(A0)+
                bne     op_error
t_andi3:        bsr     get_wert
                bsr     set_imidiate
                bsr     chk_com
t_andi2:        move.w  #$1302,D1
                bsr     get_ea
                rts

t_asl:          cmp.b   #'#',D0
                beq.s   t_asl3
                bsr     get_datareg
                bmi.s   t_asl2
                andi.b  #$F1,14(A3)
                bsr     cut_space
                cmpi.b  #',',(A0)
                bne.s   t_asl6
                add.w   D0,D0
                or.b    D0,14(A3)
                bset    #5,15(A3)
t_asl4:         bsr     chk_com
                bsr     get_datareg
                bmi     op_error
                or.b    D0,15(A3)
                rts
t_asl6:         ori.b   #2,14(A3)                   ;#1 insert
                or.b    D0,15(A3)                   ;Register insert
                rts
t_asl3:         addq.l  #1,A0
                bsr     get_wert
                tst.w   2(A3)
                bne.s   t_asl5
                andi.w  #7,D3
                add.w   D3,D3
                andi.b  #$F1,14(A3)
                or.b    D3,14(A3)
                bra.s   t_asl4
t_asl2:         move.b  #$C0,15(A3)
                move.w  #$1F03,D1
                bsr     get_ea
                rts
t_asl5:         bsr     get_quick
                bra.s   t_asl4

t_bccs:         subq.l  #1,A0
                bsr     get_wert
                sub.l   default_adr(A4),D3
                subq.l  #2,D3
                move.l  D3,D1
                ext.w   D1
                ext.l   D1
                cmp.l   D1,D3
                bne     val_error
                move.b  D3,15(A3)
                rts

t_bcc:          subq.l  #1,A0
                bsr     get_wert
                sub.l   default_adr(A4),D3
                subq.l  #2,D3
                move.l  D3,D1
                ext.l   D1
                cmp.l   D1,D3
                bne     val_error
                move.w  D3,(A2)+
                addq.w  #2,(A3)
                rts

t_bchg:         cmp.b   #'D',D0
                beq.s   t_bchg2
                cmp.b   #'d',D0
                beq.s   t_bchg2
                cmpi.b  #'#',(A0)+
                bne     op_error
                bsr     get_wert
                cmp.w   #$1F,D3
                bhi     op_error
                move.b  #%1000,14(A3)
                move.w  D3,(A2)+
                addq.w  #2,(A3)
                bsr     chk_com
                move.w  #$1F02,D1
                bsr     get_ea
                rts
t_bchg2:        bsr     get_regnr
                add.b   D0,D0
                or.b    D0,14(A3)
                bsr     chk_com
                move.w  #$1F02,D1
                bsr     get_ea
                rts

t_btst:         cmp.b   #'D',D0
                beq.s   t_btst2
                cmp.b   #'d',D0
                beq.s   t_btst2
                cmpi.b  #'#',(A0)+
                bne     op_error
                bsr     get_wert
                cmp.w   #$1F,D3
                bhi     op_error
                move.b  #%1000,14(A3)
                move.w  D3,(A2)+
                addq.w  #2,(A3)
                bsr     chk_com
                move.w  #$1E02,D1
                bra     get_ea
t_btst2:        bsr     get_regnr
                add.b   D0,D0
                or.b    D0,14(A3)
                bsr     chk_com
                move.w  #$1E02,D1
                bra     get_ea

t_chk:          move.w  #$1C02,D1
                bsr     get_ea
                bsr     chk_com
                bsr     get_datareg
                bmi     op_error
                add.b   D0,D0
                or.b    D0,14(A3)
                rts

t_clr:          move.w  #$1F00,D1
                bsr     get_ea
                move.b  15(A3),D0                   ;Get EA
                andi.w  #$3F,D0
                cmp.w   #7,D0
                bls.s   t_clr2
                cmp.w   #$0F,D0
                bhi.s   t_clr2
                move.w  14(A3),D1                   ;Get command
                add.w   D1,D1                       ;Breitenbit at the right place
                andi.w  #$0100,D1                   ;and mask
                ori.w   #$90C0,D1                   ;convert to SUBA
                or.b    D0,D1                       ;Insert address registers
                move.w  D1,14(A3)
                add.w   D0,D0
                or.b    D0,14(A3)                   ;Insert address registers
t_clr2:         rts

t_cmp:          move.w  #$1C00,D1
                bsr     get_ea
                bsr     chk_com
                move.w  14(A3),D0                   ;Save command
                move.w  #$1F00,D1
                bsr     get_ea                      ;Get destination EA (CMPI/CMPA)
                move.b  15(A3),D1
                andi.w  #$3F,D1
                cmp.w   #7,D1
                bls.s   t_cmp3                      ;CMP x,Dx
                cmp.b   #$0F,D1
                bls.s   t_cmp2                      ;CMPA x,Ax
                move.b  D0,D3
                andi.w  #$3F,D3
                cmp.w   #$3C,D3                     ;Source operand immediate?
                bne     op_error                    ;No !
                andi.w  #$C0,D0
                ori.w   #$0C00,D0                   ;CMPI #x,x
                or.w    D1,D0
                move.w  D0,14(A3)
                rts
t_cmp2:         move.w  D0,D3
                add.w   D0,D0
                andi.w  #$0100,D0
                ori.w   #$C0,D0
                or.w    D3,D0
t_cmp3:         move.w  D0,14(A3)
                andi.w  #7,D1
                add.b   D1,D1
                or.b    D1,14(A3)
                rts

t_cmpm:         bsr     get_indirect2
                cmpi.b  #'+',(A0)+
                bne     op_error
                or.b    D0,15(A3)
                bsr     chk_com
                bsr     get_indirect2
                cmpi.b  #'+',(A0)+
                bne     op_error
                add.b   D0,D0
                or.b    D0,14(A3)
                rts

t_dbcc:         bsr     get_datareg
                bmi     op_error
                or.b    D0,15(A3)
                bsr     chk_com
                bsr     get_wert
                sub.l   default_adr(A4),D3
                subq.l  #2,D3
                move.l  D3,D1
                ext.l   D1
                cmp.l   D1,D3
                bne     val_error
                move.w  D3,(A2)+
                addq.w  #2,(A3)
                rts

t_eor:          cmpi.b  #'#',(A0)
                beq.s   t_eor2                      ;Source immediate
                bsr     get_datareg
                bmi     op_error
                add.b   D0,D0
                or.b    D0,14(A3)
                bsr     chk_com
                move.w  #$1F02,D1
                bsr     get_ea
                rts
t_eor2:         move.b  #%1010,14(A3)               ;EORI insert
                addq.w  #1,A0
                bra     t_andi3

t_exg:          cmp.b   #'D',D0
                beq.s   t_exg2
                cmp.b   #'d',D0
                beq.s   t_exg2
                bsr     get_adrreg
                bmi     op_error
                add.b   D0,D0
                or.b    D0,14(A3)
                move.b  #%1001000,15(A3)
                bsr     chk_com
                cmpi.b  #'D',(A0)
                beq.s   t_exg4
                cmpi.b  #'d',(A0)
                beq.s   t_exg4
                bsr     get_adrreg
                bmi     op_error
                or.b    D0,15(A3)
                rts
t_exg4:         lsr.b   #1,D0                       ;Return address register
                andi.w  #7,D0
                ori.w   #$C188,D0                   ;Set bits for data / address registers
                move.w  D0,14(A3)
                bsr     get_regnr
                add.b   D0,D0
                or.b    D0,14(A3)
                rts
t_exg2:         bsr     get_regnr
                add.b   D0,D0
                or.b    D0,14(A3)
                bsr     chk_com
                cmpi.b  #'D',(A0)
                beq.s   t_exg3
                cmpi.b  #'d',(A0)
                beq.s   t_exg3
                bsr     get_adrreg
                bmi     op_error
                move.b  #%10001000,15(A3)
                or.b    D0,15(A3)
                rts
t_exg3:         bsr     get_regnr
                move.b  #%1000000,15(A3)
                or.b    D0,15(A3)
                rts

t_ext:          bsr     get_datareg
                bmi     op_error
                or.b    D0,15(A3)
                rts

t_jmp:          move.w  #$1E1B,D1
                bsr     get_ea
                rts

t_lea:          move.w  #$1E1B,D1
                bsr     get_ea
                bsr     chk_com
                bsr     get_adrreg
                bmi     op_error
                add.b   D0,D0
                or.b    D0,14(A3)
                rts

t_link:         bsr     get_adrreg
                bmi     op_error
                or.b    D0,15(A3)
                bsr     chk_com
                cmpi.b  #'#',(A0)+
                bne     op_error
                bsr     get_wert
                tst.b   2(A3,D5.w)
                beq.s   t_link2
                move.b  #$12,3(A3,D5.w)
t_link2:        move.w  D3,(A2)
                addq.w  #2,(A3)
                rts

t_move:         move.w  #$0400,D1
                bsr     get_ea
                bsr     chk_com
                cmpi.b  #%1111100,15(A3)            ;SR
                beq.s   t_move2
                cmpi.b  #$3F,15(A3)                 ;USP
                beq     t_move5
                move.w  14(A3),D0
                move.w  #$0300,D1
                bsr     get_ea
                move.b  15(A3),D1                   ;Get EA
                cmp.b   #%1111100,D1                ;SR
                beq.s   t_move3
                cmp.b   #%111100,D1                 ;CCR
                beq.s   t_move3
                cmp.b   #$3F,D1                     ;USP
                beq     t_move6
                cmp.b   #7,D1
                bls.s   t_move11                    ;Dx
                cmp.b   #$0F,D1
                bls     t_move7                     ;Ax
t_move11:       move.b  15(A3),D1
                andi.w  #$3F,D1
                move.w  D0,14(A3)                   ;Rescribe source EA
                lsl.w   #3,D1
                move.w  D1,D0
                andi.w  #$01C0,D1
                or.w    D1,14(A3)                   ;Target EA (Mode)
                lsr.w   #2,D0
                andi.w  #$0E,D0
                or.b    D0,14(A3)                   ;Object EA (register)
                rts
t_move2:        move.w  #$40C0,14(A3)               ;MOVE from SR
                move.w  #$1302,D1
                bsr     get_ea
                rts
t_move3:        move.w  14(A3),D1
                rol.b   #3,D1
                andi.w  #2,D1
                andi.w  #$3F,D0                     ;MOVE to CCR
                cmp.w   #8,D0
                blt.s   t_move4
                cmp.w   #$10,D0
                blt     op_error
                cmp.w   #$3C,D0
                bgt     op_error
t_move4:        ori.w   #$44C0,D0
                move.w  D0,14(A3)
                or.b    D1,14(A3)
                rts
t_move5:        bsr     get_adrreg

                bmi     op_error
                ori.w   #$4E68,D0
                move.w  D0,14(A3)
                rts
t_move6:        andi.w  #7,D0
                ori.w   #$4E60,D0
                move.w  D0,14(A3)
                rts
t_move7:        ori.w   #$2040,D0
                move.w  D0,14(A3)
                andi.w  #7,D1
                add.w   D1,D1
                or.b    D1,14(A3)
                rts

t_movem:        moveq   #0,D3
                moveq   #0,D0
                lea     18(A3),A2
                bsr     get_datareg
                bpl.s   t_movem2
                bsr     get_adrreg
                bpl.s   t_movem21
                addq.w  #2,(A3)
                move.w  #$1E13,D1
                bsr     get_ea
                ori.w   #$4C80,14(A3)
                bsr     chk_com
                moveq   #0,D3
                bra.s   t_movem3
t_movem21:      addq.w  #8,D0
                moveq   #8,D4
t_movem2:       bsr.s   t_movem31
                addq.w  #2,(A3)
                bsr     chk_com
                move.w  #$1F0B,D1
                bsr     get_ea
                ori.w   #$4880,14(A3)
                move.w  14(A3),D0
                andi.w  #$38,D0
                cmp.w   #$20,D0
                bne.s   t_movem8
                move.w  16(A3),D3
                moveq   #15,D0
t_loop:         addx.w  D3,D3
                roxr.w  16(A3)
                dbra    D0,t_loop
t_movem8:       rts
t_movem3:       bsr.s   t_movem4
                moveq   #8,D4
                and.w   D0,D4
t_movem31:      bset    D0,D3
                move.w  D0,D1
                move.b  (A0)+,D0
                cmp.w   #'/',D0
                beq.s   t_movem3
                cmp.w   #'-',D0
                bne.s   t_movem5
                bsr.s   t_movem4
                moveq   #8,D4
                and.w   D0,D4
                cmp.w   D0,D1
                bge     op_error
t_movem6:       bset    D0,D3
                subq.w  #1,D0
                cmp.w   D1,D0
                bgt.s   t_movem6
                move.b  (A0)+,D0
                cmp.w   #'/',D0
                beq.s   t_movem3
t_movem5:       cmp.w   #'0'-1,D0
                bhi     op_error
                subq.l  #1,A0
                move.w  D3,16(A3)
                rts
t_movem4:       bsr     get_datareg
                bpl.s   t_movem7
                bsr     get_adrreg
                bmi.s   t_movem70
                ori.w   #8,D0
t_movem7:       rts
t_movem70:      bsr     get_datareg3
                or.w    D4,D0
                rts

t_movep:        bsr     get_datareg
                bmi.s   t_movep2
                add.w   D0,D0
                or.b    D0,14(A3)
                bset    #7,15(A3)
                bsr     cut_space
                cmpi.b  #',',(A0)+
                bne     ea_error
                bsr     cut_space
                moveq   #2,D5
                bsr.s   t_movep4
                or.b    D0,15(A3)
                move.w  D3,(A2)+
                addq.w  #2,(A3)
                rts
t_movep2:       move.w  #$1FEF,D1                   ;Storage to register
                bsr.s   t_movep4
                or.b    D0,15(A3)
                move.w  D3,(A2)+
                addq.w  #2,(A3)
                bsr     chk_com
                bsr     get_datareg
                bmi     op_error
                add.w   D0,D0
                or.b    D0,14(A3)
                rts
t_movep4:       moveq   #0,D3
                cmpi.b  #'(',(A0)
                beq     get_indirect2
                bsr     get_wert
                cmp.l   #$FFFF,D3
                bhi     val_error
                bra     get_indirect2

t_moveq:        cmpi.b  #'#',(A0)+
                bsr     get_wert
                tst.b   2(A3,D5.w)
                bne.s   t_moveq2
                move.b  D3,15(A3)
t_moveq3:       bsr     chk_com
                bsr     get_datareg
                bmi     op_error
                add.w   D0,D0
                or.b    D0,14(A3)
                rts
t_moveq2:       bsr.s   get_quick
                bra.s   t_moveq3

get_quick:      move.w  D3,2(A3,D5.w)
                ori.b   #$C0,2(A3,D5.w)
                rts

t_nop:          rts

t_pea:          move.w  #$1E1B,D1
                bsr     get_ea
                rts

t_stop:         cmpi.b  #'#',(A0)+
                bne     op_error
                bsr     get_wert
                move.w  D3,(A2)
                move.b  #4,1(A3)
                rts

t_linea:        cmpi.b  #'#',(A0)
                bne.s   t_line2
                addq.l  #1,A0
t_line2:        bsr     get_wert
                tst.b   2(A3,D5.w)
                bne     syntax_error
                cmp.l   #15,D3
                bhi     val_error
                or.b    D3,15(A3)
                bsr     cut_space
t_line3:        move.b  (A0)+,D0
                beq.s   t_line4
                cmp.b   #';',D0
                bne.s   t_line3
t_line4:        subq.w  #1,A0
                rts

t_trap:         cmpi.b  #'#',(A0)
                bne.s   t_trap2
                addq.l  #1,A0
t_trap2:        bsr     get_wert
                tst.b   2(A3,D5.w)
                bne.s   t_trap3
                cmp.l   #15,D3
                bhi     op_error
                or.b    D3,15(A3)
                rts
t_trap3:        bsr     get_quick
                rts

t_unlk:         bsr.s   get_adrreg
                bmi     op_error
                or.b    D0,15(A3)
                rts

;************************************************************************
;*  Retrieve an address register from A0 to D0                          *
;*  If error, D0 = -1 and A0 is reconstructed                           *
;************************************************************************
get_adrreg:     move.b  (A0)+,D0
                cmp.b   #'A',D0
                beq.s   get_adrreg4
                cmp.b   #'a',D0
                beq.s   get_adrreg4
                cmp.b   #'s',D0
                beq.s   get_adrreg2
                cmp.b   #'S',D0
                beq.s   get_adrreg2
                subq.w  #1,A0                       ;No address register -> -1
                moveq   #-1,D0
                rts
get_adrreg2:    move.b  (A0)+,D0
                cmp.b   #'p',D0
                beq.s   get_adrreg3
                cmp.b   #'P',D0
                beq.s   get_adrreg3
                subq.w  #2,A0
                moveq   #-1,D0
                rts
get_adrreg3:    moveq   #7,D0
                cmpi.b  #'0',(A0)
                bls.s   get_adrreg6                 ;The following sign < '0'->No label
get_adrreg5:    subq.w  #2,A0
                moveq   #-1,D0
                rts
get_adrreg4:    moveq   #0,D0
                move.b  (A0)+,D0
                subi.w  #'0',D0
                bmi.s   get_adrreg5                 ;No digit
                cmp.w   #7,D0
                bgt.s   get_adrreg5                 ;No digit
                cmpi.b  #'0',(A0)
                bhi.s   get_adrreg5
get_adrreg6:    tst.w   D0
                rts

;************************************************************************
;* Retrieves a data register from A0 to D0                              *
;************************************************************************
get_datareg:    cmpi.b  #'D',(A0)
                beq.s   get_datareg2
                cmpi.b  #'d',(A0)
                beq.s   get_datareg2
                moveq   #-1,D0                      ;No data register
                rts
get_datareg2:   addq.w  #1,A0
get_datareg3:   moveq   #0,D0
                move.b  (A0)+,D0
                subi.w  #'0',D0
                bmi.s   get_adrreg5
                cmp.w   #7,D0
                bgt.s   get_adrreg5
                cmpi.b  #'0',(A0)
                bhi.s   get_adrreg5
                tst.w   D0
                rts

get_regnr:      addq.l  #1,A0
                move.b  (A0)+,D0
                subi.b  #$30,D0
                bmi.s   regnr_err
                cmp.b   #7,D0
                bgt.s   regnr_err
                andi.w  #7,D0
                rts
regnr_err:      moveq   #-10,D0
                bra     operant_err
get_regnr2:     move.b  1(A0),D0
                sub.b   #$30,D0
                bmi.s   get_regnr3
                cmp.b   #7,D0
                bgt.s   get_regnr3
                cmpi.b  #'0',2(A0)                  ;Test the epil signs
                bhs.s   get_regnr3
                addq.l  #2,A0
                rts
get_regnr3:     addq.w  #4,SP
                bra     failed

;************************************************************************
;*  Tests immediate value on the width given by the opcode and          *
;*  Write value in buffer                                               *
;************************************************************************
set_imidiate:   move.b  D4,D0                       ;Data for immediate operants
                beq.s   set_imi1                    ;Command table
                bmi.s   set_imi3
                cmp.b   #3,D0
                bhi.s   set_imi3
                beq.s   tst_word
                cmp.w   #1,D0
                beq.s   tst_byte
                bra.s   set_imi2
set_imi3:       btst    #0,14(A3)
                beq.s   tst_word
                bra.s   set_imi2
set_imi1:       move.b  15(A3),D0
                rol.b   #2,D0
                andi.w  #3,D0
                beq.s   tst_byte
                cmp.b   #1,D0
                beq.s   tst_word
                cmp.b   #2,D0
                bne     syn_error
set_imi2:       tst.b   2(A3,D5.w)
                beq.s   kein_lab4
                move.b  1(A3),3(A3,D5.w)
                ori.w   #$20,2(A3,D5.w)
kein_lab4:      addq.w  #4,(A3)
                move.l  D3,(A2)+
                rts
tst_byte:       tst.b   2(A3,D5.w)
                beq.s   tst_by1
                move.b  1(A3),3(A3,D5.w)
                bra.s   tst_esc
tst_by1:        move.l  D3,D0
                clr.b   D0
                tst.l   D0
                beq.s   tst_esc
                move.l  D3,D0
                ext.w   D0
                ext.l   D0
                cmp.l   D0,D3
                bne.s   val_error
                and.w   #$FF,D3
                bra.s   tst_esc
tst_word:       tst.b   2(A3,D5.w)
                beq.s   tst_wo1
                move.b  1(A3),3(A3,D5.w)
                ori.w   #$10,2(A3,D5.w)
                bra.s   tst_esc
tst_wo1:        move.l  D3,D0
                swap    D0
                tst.w   D0
                beq.s   tst_esc
                move.l  D3,D0
                ext.l   D0
                cmp.l   D0,D3
                bne.s   val_error
tst_esc:        addq.w  #2,(A3)
                move.w  D3,(A2)+
                rts
val_error:      moveq   #-5,D0
                bra     operant_err

;************************************************************************
;* brings address registers in brackets                                 *
;************************************************************************
get_indirect:   addq.l  #1,A0
get_indirect2:  cmpi.b  #'(',(A0)+
                bne     op_error
                bsr     get_adrreg
                bmi     op_error
                cmpi.b  #')',(A0)+
                bne     op_error
                rts

;************************************************************************
;* Retrive <EA>, and ORT in the opcode                                  *
;* Bitplane in D1 indicates the permitted EAS                           *
;************************************************************************
get_ea:         moveq   #0,D3
                move.l  D0,-(SP)
                andi.w  #$FFC0,14(A3)               ;EA-Bits Clear
                move.b  (A0),D0
                cmp.b   #'#',D0
                beq     immidiate
                cmp.b   #'D',D0
                beq     data_reg
                cmp.b   #'d',D0
                beq     data_reg
                cmp.b   #'A',D0
                beq     adr_reg
                cmp.b   #'a',D0
                beq     adr_reg
                cmp.b   #'(',D0
                beq     indirect
                cmp.b   #'-',D0
                beq     predecrement
                cmp.b   #'C',D0
                beq     __ccr
                cmp.b   #'c',D0
                beq     __ccr
                cmp.b   #'S',D0
                beq     __sr
                cmp.b   #'s',D0
                beq     __sr
                cmp.b   #'U',D0
                beq     __usp
                cmp.b   #'u',D0
                beq     __usp
failed:         bsr     get_wert
                cmpi.b  #'(',(A0)
                beq     indirect2
                tst.b   D1
                bmi     ea_error
                cmpi.b  #'.',(A0)
                bne.s   adr_long
                addq.l  #1,A0
                move.b  (A0)+,D0
                cmp.b   #'L',D0
                beq.s   adr_long
                cmp.b   #'l',D0
                beq.s   adr_long
                cmp.b   #'W',D0
                beq.s   adr_short
                cmp.b   #'w',D0
                beq.s   adr_short
                cmp.b   #'s',D0
                beq.s   adr_short
                cmp.b   #'S',D0
                bne     syn_error
adr_short:      ori.b   #%111000,15(A3)
                tst.b   2(A3,D5.w)
                beq.s   kein_lab3b
                move.b  1(A3),3(A3,D5.w)
                ori.w   #$10,2(A3,D5.w)
kein_lab3b:     addq.w  #2,(A3)
                move.w  D3,(A2)+
                move.l  (SP)+,D0
                rts
adr_long:       ori.b   #%111001,15(A3)
                tst.b   2(A3,D5.w)
                beq.s   kein_lab3
                move.b  1(A3),3(A3,D5.w)
                ori.w   #$20,2(A3,D5.w)
kein_lab3:      addq.w  #4,(A3)
                move.l  D3,(A2)+
                move.l  (SP)+,D0
                rts
__usp:          cmpi.b  #'s',1(A0)
                beq.s   _usp2
                cmpi.b  #'S',1(A0)
                bne     failed
_usp2:          cmpi.b  #'p',2(A0)
                beq.s   _usp3
                cmpi.b  #'P',2(A0)
                bne     failed
_usp3:          cmpi.b  #'0'-1,3(A0)
                bhi     failed
                btst    #12,D1
                bne     ea_error
                move.b  #$3F,15(A3)
                addq.l  #3,A0
                move.l  (SP)+,D0
                rts
__ccr:          cmpi.b  #'c',1(A0)
                beq.s   _ccr2
                cmpi.b  #'C',1(A0)
                bne     failed
_ccr2:          cmpi.b  #'r',2(A0)
                beq.s   _ccr3
                cmpi.b  #'R',2(A0)
                bne     failed
_ccr3:          cmpi.b  #'0'-1,3(A0)
                bhi     failed
                btst    #10,D1
                bne     ea_error
                move.b  #%111100,15(A3)
                addq.l  #3,A0
                move.l  (SP)+,D0
                rts
__sr:           cmpi.b  #'P',1(A0)
                beq.s   _sp
                cmpi.b  #'p',1(A0)
                beq.s   _sp
                cmpi.b  #'r',1(A0)
                beq.s   _sr2
                cmpi.b  #'R',1(A0)
                bne     failed
_sr2:           cmpi.b  #'0'-1,2(A0)
                bhi     failed
                btst    #11,D1
                bne     ea_error
                move.b  #%1111100,15(A3)
                addq.l  #2,A0
                move.l  (SP)+,D0
                rts
_sp:            cmpi.b  #'0'-1,2(A0)
                bhi     failed
                moveq   #7,D0
                addq.l  #2,A0
                bra.s   adr_re2
data_reg:       bsr     get_regnr2
                btst    #0,D1
                bne     ea_error
                or.b    D0,15(A3)
                move.l  (SP)+,D0
                rts
adr_reg:        bsr     get_regnr2
adr_re2:        btst    #1,D1
                bne.s   ea_error
                ori.b   #%1000,D0
                or.b    D0,15(A3)
                move.l  (SP)+,D0
                rts
immidiate:      addq.l  #1,A0
                btst    #9,D1
                bne.s   ea_error
                bsr     get_wert
                bsr     set_imidiate
                ori.w   #%111100,14(A3)
                move.l  (SP)+,D0
                rts
_sp2:           addq.l  #1,A0
                move.b  (A0)+,D0
                cmp.b   #'p',D0
                beq.s   _sp3
                cmp.b   #'P',D0
                bne     syn_error
_sp3:           moveq   #7,D0
                bra.s   ind_sp
indirect:       addq.l  #1,A0
                cmpi.b  #'S',(A0)
                beq.s   _sp2
                cmpi.b  #'s',(A0)
                beq.s   _sp2
                cmpi.b  #'a',(A0)
                beq.s   adr_rel
                cmpi.b  #'A',(A0)
                bne.s   pc_rel
adr_rel:        bsr     get_regnr
ind_sp:         cmpi.b  #')',(A0)+
                bne     second_reg
                cmpi.b  #'+',(A0)
                beq.s   increment
                btst    #2,D1
                bne.s   ea_error
                ori.b   #%10000,D0
                or.b    D0,15(A3)
                move.l  (SP)+,D0
                rts
ea_error:       moveq   #-7,D0
                bra     operant_err
increment:      addq.l  #1,A0
                btst    #3,D1
                bne.s   ea_error
                ori.b   #%11000,D0
                or.b    D0,15(A3)
                move.l  (SP)+,D0
                rts
predecrement:   cmpi.b  #'(',1(A0)
                beq.s   pre2
                bra     failed
pre2:           btst    #4,D1
                bne.s   ea_error
                addq.w  #2,A0
                bsr     get_adrreg
                bmi     op_error
                cmpi.b  #')',(A0)+
                bne.s   syn_error
                ori.b   #%100000,D0
                or.b    D0,15(A3)
                move.l  (SP)+,D0
                rts
syn_error:      moveq   #-8,D0
                bra     operant_err
pc_rel:         cmpi.b  #'p',(A0)
                beq.s   pc_rel2
                cmpi.b  #'P',(A0)
                beq.s   pc_rel2
                subq.w  #1,A0
                bra     failed
pc_rel2:        btst    #8,D1
                bne.s   ea_error
                addq.w  #1,A0
                move.b  (A0)+,D0
                cmp.b   #'c',D0
                beq.s   pc_rel3
                cmp.b   #'C',D0
                bne.s   syn_error
pc_rel3:        cmpi.b  #')',(A0)+
                beq.s   no_second
                sub.l   default_adr(A4),D3
                moveq   #0,D0
                move.w  (A3),D0                     ;akt.-Länge holen
                sub.l   D0,D3
                move.b  D3,D0
                ext.w   D0
                ext.l   D0
                cmp.l   D3,D0
                bne     val_error
                bsr     get_second
                ori.b   #%111011,15(A3)
                addq.w  #2,(A3)                     ;xx(PC,Xn)
                move.l  (SP)+,D0
                rts
no_second:      ori.b   #%111010,15(A3)
                sub.l   default_adr(A4),D3
                moveq   #0,D0
                move.w  (A3),D0
                sub.l   D0,D3
                move.w  D3,D0
                ext.l   D0
                cmp.l   D3,D0
                bne     val_error
                move.w  D3,(A2)+
                addq.w  #2,(A3)                     ;xxxx(PC)
                move.l  (SP)+,D0
                rts
indirect2:      addq.l  #1,A0
                bsr.s   tst_sp
                cmp.w   #7,D0
                beq.s   indirect4
                cmpi.b  #'a',(A0)
                beq.s   indirect3
                cmpi.b  #'A',(A0)
                beq.s   indirect3
                bne     pc_rel
indirect3:      bsr     get_regnr
indirect4:      cmpi.b  #')',(A0)+
                beq.s   no_second2
second_reg:     btst    #6,D1
                bne     ea_error
                ori.b   #%110000,D0
                or.b    D0,15(A3)                   ;xx(Ax,Xn)
                bsr.s   get_second
                addq.w  #2,(A3)
                move.l  (SP)+,D0
                rts

no_second2:     btst    #5,D1
                bne     ea_error
                ori.b   #%101000,D0
                or.b    D0,15(A3)
                swap    D3
                tst.w   D3
                beq.s   no_sec3                     ;0 or -1 are allowed in the upper byte
                addq.w  #1,D3
                bne     val_error
no_sec3:        swap    D3
                move.w  D3,(A2)+                    ;xxxx(Ax)
                addq.w  #2,(A3)
                move.l  (SP)+,D0
                rts

tst_sp:         moveq   #0,D0
                cmpi.b  #'S',(A0)
                beq.s   _sp5
                cmpi.b  #'s',(A0)
                beq.s   _sp5
                rts
_sp5:           addq.w  #1,A0
                move.b  (A0)+,D0
                cmp.b   #'P',D0
                beq.s   _sp4
                cmp.b   #'p',D0
                bne     syn_error
_sp4:           moveq   #7,D0
                rts

get_second:     cmpi.b  #'D',(A0)
                beq.s   dat_reg
                cmpi.b  #'d',(A0)
                beq.s   dat_reg
                bset    #7,(A2)
                bsr.s   tst_sp
                cmp.w   #7,D0
                beq.s   sp_reg
                cmpi.b  #'a',(A0)
                beq.s   dat_reg
                cmpi.b  #'A',(A0)
                bne     syn_error
dat_reg:        bsr     get_regnr
sp_reg:         lsl.b   #4,D0
                or.b    D0,(A2)
                cmpi.b  #')',(A0)
                beq.s   end_reg
                cmpi.b  #'.',(A0)+
                bne     syn_error
                cmpi.b  #'W',(A0)
                beq.s   end_re2
                cmpi.b  #'w',(A0)
                beq.s   end_re2
                cmpi.b  #'l',(A0)
                beq.s   end_re3
                cmpi.b  #'L',(A0)
                bne     syn_error
end_re3:        bset    #3,(A2)
end_re2:        addq.l  #1,A0
                cmpi.b  #')',(A0)
                bne     syn_error
end_reg:        addq.l  #1,A0
                tst.b   2(A3,D5.w)
                bne.s   end_reg2
                cmp.l   #$FF,D3
                bls.s   end_reg2
                move.b  D3,D0
                ext.w   D0
                ext.l   D0
                cmp.l   D3,D0
                bne     val_error
end_reg2:       move.b  D3,1(A2)
                addq.l  #2,A2
                rts

;************************************************************************
;* Binary search routine                                                *
;************************************************************************
search:         movem.l D0-D7/A2-A6,-(SP)
                movea.l A0,A6                       ;Textpointer Brands
                lea     spaced(A4),A3
                move.l  #'    ',(A3)
                move.l  #'    ',4(A3)               ;Clear buffer
                lea     search_tab(PC),A5
                moveq   #7,D0                       ;max.7 Pick sign
                moveq   #0,D1
search1:        move.b  (A0)+,D1                    ;Pick sign
                move.b  0(A5,D1.w),D1               ;& convert
                beq.s   search4                     ;
                move.b  D1,(A3)+                    ;Ab in den Buffer
                dbra    D0,search1
                bra.s   search3
search4:        subq.l  #1,A0                       ;Pointer back (on the 1st sign behind it)
search3:        move.l  spaced(A4),D5               ;Bring the front 4 characters (=> Buffer)
                move.l  spaced+4(A4),D6             ;Get the rear 4 characters
                lea     code_tab(PC),A1
                moveq   #0,D1
                move.w  tablen(A4),D2
search2:        move.w  D1,D4
                add.w   D2,D4
                lsr.w   #1,D4
                move.w  D4,D0
                lsl.w   #4,D0                       ;Time 16 (length of an entry)
                cmp.l   0(A1,D0.w),D5
                bhi.s   search6
                bne.s   search5
                cmp.l   4(A1,D0.w),D6
                bhi.s   search6
                bne.s   search5
                lea     0(A1,D0.w),A1
                movem.l (SP)+,D0-D7/A2-A6
                rts
search5:        move.w  D4,D2
                cmp.w   D1,D2
                bne.s   search2
                bra.s   search7
search6:        move.w  D4,D1
                addq.w  #1,D1
                cmp.w   D1,D2
                bne.s   search2
search7:        suba.l  A1,A1
                movea.l A6,A0                       ;Pointer back
                movem.l (SP)+,D0-D7/A2-A6
                rts

search_tab:     DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,'.',0
                DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                DC.B 0,'ABCDEFGHIJKLMNO'
                DC.B 'PQRSTUVWXYZ',0,0,0,0,0
                DC.B 0,'ABCDEFGHIJKLMNO'
                DC.B 'PQRSTUVWXYZ',0,0,0,0,0
                DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

;************************************************************************
;* Bring a value (number or variable) according to D3                   *
;************************************************************************
get_wert:       movem.l D0-D2/D4-D7/A1-A6,-(SP)
                bsr     get
                bsr     get_term
                move.b  D0,-(A0)
                move.l  D1,D3
                movem.l (SP)+,D0-D2/D4-D7/A1-A6
                rts

;************************************************************************
;* Spaces from A0 overlooked                                            *
;************************************************************************
cut_space:      cmpi.b  #' ',(A0)+
                beq.s   cut_space
                subq.w  #1,A0
                rts

;************************************************************************
;* Here is the table of all commands & opcodes                          *
;************************************************************************
                DXSET 8,' '
code_tab:       DX.B 'ABCD'
                DC.L t_abcd
                DC.B 0,0,$C1,0
                DX.B 'ADD'
                DC.L t_add
                DC.B 0,0,$D0,$40
                DX.B 'ADD.B'
                DC.L t_add
                DC.B 0,0,$D0,0
                DX.B 'ADD.L'
                DC.L t_add
                DC.B 0,0,$D0,$80
                DX.B 'ADD.W'
                DC.L t_add
                DC.B 0,0,$D0,$40
                DX.B 'ADDA'
                DC.L t_adda
                DC.B 0,$80,$D0,$C0
                DX.B 'ADDA.L'
                DC.L t_adda
                DC.B 0,$80,$D1,$C0
                DX.B 'ADDA.W'
                DC.L t_adda
                DC.B 0,$80,$D0,$C0
                DX.B 'ADDI'
                DC.L t_addi
                DC.B 0,0,6,$40
                DX.B 'ADDI.B'
                DC.L t_addi
                DC.B 0,0,6,0
                DX.B 'ADDI.L'
                DC.L t_addi
                DC.B 0,0,6,$80
                DX.B 'ADDI.W'
                DC.L t_addi
                DC.B 0,0,6,$40
                DX.B 'ADDQ'
                DC.L t_addq
                DC.B 0,0,$50,$40
                DX.B 'ADDQ.B'
                DC.L t_addq
                DC.B 0,0,$50,0
                DX.B 'ADDQ.L'
                DC.L t_addq
                DC.B 0,0,$50,$80
                DX.B 'ADDQ.W'
                DC.L t_addq
                DC.B 0,0,$50,$40
                DX.B 'ADDX'
                DC.L t_abcd
                DC.B 0,0,$D1,$40
                DX.B 'ADDX.B'
                DC.L t_abcd
                DC.B 0,0,$D1,0
                DX.B 'ADDX.L'
                DC.L t_abcd
                DC.B 0,0,$D1,$80
                DX.B 'ADDX.W'
                DC.L t_abcd
                DC.B 0,0,$D1,$40
                DX.B 'AND'
                DC.L t_and
                DC.B 0,0,$C0,$40
                DX.B 'AND.B'
                DC.L t_and
                DC.B 0,0,$C0,0
                DX.B 'AND.L'
                DC.L t_and
                DC.B 0,0,$C0,$80
                DX.B 'AND.W'
                DC.L t_and
                DC.B 0,0,$C0,$40
                DX.B 'ANDI'
                DC.L t_andi
                DC.B 0,0,2,$40
                DX.B 'ANDI.B'
                DC.L t_andi
                DC.B 0,0,2,0
                DX.B 'ANDI.L'
                DC.L t_andi
                DC.B 0,0,2,$80
                DX.B 'ANDI.W'
                DC.L t_andi
                DC.B 0,0,2,$40
                DX.B 'ASL'
                DC.L t_asl
                DC.B 0,0,$E1,$40
                DX.B 'ASL.B'
                DC.L t_asl
                DC.B 0,0,$E1,0
                DX.B 'ASL.L'
                DC.L t_asl
                DC.B 0,0,$E1,$80
                DX.B 'ASL.W'
                DC.L t_asl
                DC.B 0,0,$E1,$40
                DX.B 'ASR'
                DC.L t_asl
                DC.B 0,0,$E0,$40
                DX.B 'ASR.B'
                DC.L t_asl
                DC.B 0,0,$E0,0
                DX.B 'ASR.L'
                DC.L t_asl
                DC.B 0,0,$E0,$80
                DX.B 'ASR.W'
                DC.L t_asl
                DC.B 0,0,$E0,$40
                DX.B 'BCC'
                DC.L t_bcc
                DC.B 0,0,$64,0
                DX.B 'BCC.S'
                DC.L t_bccs
                DC.B 0,0,$64,0
                DX.B 'BCC.W'
                DC.L t_bcc
                DC.B 0,0,$64,0
                DX.B 'BCHG'
                DC.L t_bchg
                DC.B 0,0,1,$40
                DX.B 'BCHG.B'
                DC.L t_bchg
                DC.B 0,0,1,$40
                DX.B 'BCHG.L'
                DC.L t_bchg
                DC.B 0,0,1,$40
                DX.B 'BCLR'
                DC.L t_bchg
                DC.B 0,0,1,$80
                DX.B 'BCLR.B'
                DC.L t_bchg
                DC.B 0,0,1,$80
                DX.B 'BCLR.L'
                DC.L t_bchg
                DC.B 0,0,1,$80
                DX.B 'BCS'
                DC.L t_bcc
                DC.B 0,0,$65,0
                DX.B 'BCS.S'
                DC.L t_bccs
                DC.B 0,0,$65,0
                DX.B 'BCS.W'
                DC.L t_bcc
                DC.B 0,0,$65,0
                DX.B 'BEQ'
                DC.L t_bcc
                DC.B 0,0,$67,0
                DX.B 'BEQ.S'
                DC.L t_bccs
                DC.B 0,0,$67,0
                DX.B 'BEQ.W'
                DC.L t_bcc
                DC.B 0,0,$67,0
                DX.B 'BGE'
                DC.L t_bcc
                DC.B 0,0,$6C,0
                DX.B 'BGE.S'
                DC.L t_bccs
                DC.B 0,0,$6C,0
                DX.B 'BGE.W'
                DC.L t_bcc
                DC.B 0,0,$6C,0
                DX.B 'BGT'
                DC.L t_bcc
                DC.B 0,0,$6E,0
                DX.B 'BGT.S'
                DC.L t_bccs
                DC.B 0,0,$6E,0
                DX.B 'BGT.W'
                DC.L t_bcc
                DC.B 0,0,$6E,0
                DX.B 'BHI'
                DC.L t_bcc
                DC.B 0,0,$62,0
                DX.B 'BHI.S'
                DC.L t_bccs
                DC.B 0,0,$62,0
                DX.B 'BHI.W'
                DC.L t_bcc
                DC.B 0,0,$62,0
                DX.B 'BHS'
                DC.L t_bcc
                DC.B 0,0,$64,0
                DX.B 'BHS.S'
                DC.L t_bccs
                DC.B 0,0,$64,0
                DX.B 'BHS.W'
                DC.L t_bcc
                DC.B 0,0,$64,0
                DX.B 'BLE'
                DC.L t_bcc
                DC.B 0,0,$6F,0
                DX.B 'BLE.S'
                DC.L t_bccs
                DC.B 0,0,$6F,0
                DX.B 'BLE.W'
                DC.L t_bcc
                DC.B 0,0,$6F,0
                DX.B 'BLO'
                DC.L t_bcc
                DC.B 0,0,$65,0
                DX.B 'BLO.S'
                DC.L t_bccs
                DC.B 0,0,$65,0
                DX.B 'BLO.W'
                DC.L t_bcc
                DC.B 0,0,$65,0
                DX.B 'BLS'
                DC.L t_bcc
                DC.B 0,0,$63,0
                DX.B 'BLS.S'
                DC.L t_bccs
                DC.B 0,0,$63,0
                DX.B 'BLS.W'
                DC.L t_bcc
                DC.B 0,0,$63,0
                DX.B 'BLT'
                DC.L t_bcc
                DC.B 0,0,$6D,0
                DX.B 'BLT.S'
                DC.L t_bccs
                DC.B 0,0,$6D,0
                DX.B 'BLT.W'
                DC.L t_bcc
                DC.B 0,0,$6D,0
                DX.B 'BMI'
                DC.L t_bcc
                DC.B 0,0,$6B,0
                DX.B 'BMI.S'
                DC.L t_bccs
                DC.B 0,0,$6B,0
                DX.B 'BMI.W'
                DC.L t_bcc
                DC.B 0,0,$6B,0
                DX.B 'BNE'
                DC.L t_bcc
                DC.B 0,0,$66,0
                DX.B 'BNE.S'
                DC.L t_bccs
                DC.B 0,0,$66,0
                DX.B 'BNE.W'
                DC.L t_bcc
                DC.B 0,0,$66,0
                DX.B 'BNZ'
                DC.L t_bcc
                DC.B 0,0,$66,0
                DX.B 'BNZ.S'
                DC.L t_bccs
                DC.B 0,0,$66,0
                DX.B 'BNZ.W'
                DC.L t_bcc
                DC.B 0,0,$66,0
                DX.B 'BPL'
                DC.L t_bcc
                DC.B 0,0,$6A,0
                DX.B 'BPL.S'
                DC.L t_bccs
                DC.B 0,0,$6A,0
                DX.B 'BPL.W'
                DC.L t_bcc
                DC.B 0,0,$6A,0
                DX.B 'BRA'
                DC.L t_bcc
                DC.B 0,0,$60,0
                DX.B 'BRA.S'
                DC.L t_bccs
                DC.B 0,0,$60,0
                DX.B 'BRA.W'
                DC.L t_bcc
                DC.B 0,0,$60,0
                DX.B 'BSET'
                DC.L t_bchg
                DC.B 0,0,1,$C0
                DX.B 'BSET.B'
                DC.L t_bchg
                DC.B 0,0,1,$C0
                DX.B 'BSET.L'
                DC.L t_bchg
                DC.B 0,0,1,$C0
                DX.B 'BSR'
                DC.L t_bcc
                DC.B 0,0,$61,0
                DX.B 'BSR.S'
                DC.L t_bccs
                DC.B 0,0,$61,0
                DX.B 'BSR.W'
                DC.L t_bcc
                DC.B 0,0,$61,0
                DX.B 'BTST'
                DC.L t_btst
                DC.B 0,0,1,0
                DX.B 'BTST.B'
                DC.L t_btst
                DC.B 0,0,1,0
                DX.B 'BTST.L'
                DC.L t_btst
                DC.B 0,0,1,0
                DX.B 'BVC'
                DC.L t_bcc
                DC.B 0,0,$68,0
                DX.B 'BVC.S'
                DC.L t_bccs
                DC.B 0,0,$68,0
                DX.B 'BVC.W'
                DC.L t_bcc
                DC.B 0,0,$68,0
                DX.B 'BVS'
                DC.L t_bcc
                DC.B 0,0,$69,0
                DX.B 'BVS.S'
                DC.L t_bccs
                DC.B 0,0,$69,0
                DX.B 'BVS.W'
                DC.L t_bcc
                DC.B 0,0,$69,0
                DX.B 'BZ'
                DC.L t_bcc
                DC.B 0,0,$67,0
                DX.B 'BZ.S'
                DC.L t_bccs
                DC.B 0,0,$67,0
                DX.B 'BZ.W'
                DC.L t_bcc
                DC.B 0,0,$67,0
                DX.B 'BZE'
                DC.L t_bcc
                DC.B 0,0,$67,0
                DX.B 'BZE.S'
                DC.L t_bccs
                DC.B 0,0,$67,0
                DX.B 'BZE.W'
                DC.L t_bcc
                DC.B 0,0,$67,0
                DX.B 'CHK'
                DC.L t_chk
                DC.B 0,3,$41,$80
                DX.B 'CLR'
                DC.L t_clr
                DC.B 0,0,$42,$40
                DX.B 'CLR.B'
                DC.L t_clr
                DC.B 0,0,$42,0
                DX.B 'CLR.L'
                DC.L t_clr
                DC.B 0,0,$42,$80
                DX.B 'CLR.W'
                DC.L t_clr
                DC.B 0,0,$42,$40
                DX.B 'CMP'
                DC.L t_cmp
                DC.B 0,0,$B0,$40
                DX.B 'CMP.B'
                DC.L t_cmp
                DC.B 0,0,$B0,0
                DX.B 'CMP.L'
                DC.L t_cmp
                DC.B 0,0,$B0,$80
                DX.B 'CMP.W'
                DC.L t_cmp
                DC.B 0,0,$B0,$40
                DX.B 'CMPA'
                DC.L t_adda
                DC.B 0,$80,$B0,$C0
                DX.B 'CMPA.L'
                DC.L t_adda
                DC.B 0,$80,$B1,$C0
                DX.B 'CMPA.W'
                DC.L t_adda
                DC.B 0,$80,$B0,$C0
                DX.B 'CMPI'
                DC.L t_addi
                DC.B 0,0,$0C,$40
                DX.B 'CMPI.B'
                DC.L t_addi
                DC.B 0,0,$0C,0
                DX.B 'CMPI.L'
                DC.L t_addi
                DC.B 0,0,$0C,$80
                DX.B 'CMPI.W'
                DC.L t_addi
                DC.B 0,0,$0C,$40
                DX.B 'CMPM'
                DC.L t_cmpm
                DC.B 0,0,$B1,$48
                DX.B 'CMPM.B'
                DC.L t_cmpm
                DC.B 0,0,$B1,8
                DX.B 'CMPM.L'
                DC.L t_cmpm
                DC.B 0,0,$B1,$88
                DX.B 'CMPM.W'
                DC.L t_cmpm
                DC.B 0,0,$B1,$48
                DX.B 'DBCC'
                DC.L t_dbcc
                DC.B 0,0,$54,$C8
                DX.B 'DBCS'
                DC.L t_dbcc
                DC.B 0,0,$55,$C8
                DX.B 'DBEQ'
                DC.L t_dbcc
                DC.B 0,0,$57,$C8
                DX.B 'DBF'
                DC.L t_dbcc
                DC.B 0,0,$51,$C8
                DX.B 'DBGE'
                DC.L t_dbcc
                DC.B 0,0,$5C,$C8
                DX.B 'DBGT'
                DC.L t_dbcc
                DC.B 0,0,$5E,$C8
                DX.B 'DBHI'
                DC.L t_dbcc
                DC.B 0,0,$52,$C8
                DX.B 'DBHS'
                DC.L t_dbcc
                DC.B 0,0,$54,$C8
                DX.B 'DBLE'
                DC.L t_dbcc
                DC.B 0,0,$5F,$C8
                DX.B 'DBLO'
                DC.L t_dbcc
                DC.B 0,0,$55,$C8
                DX.B 'DBLS'
                DC.L t_dbcc
                DC.B 0,0,$53,$C8
                DX.B 'DBLT'
                DC.L t_dbcc
                DC.B 0,0,$5D,$C8
                DX.B 'DBMI'
                DC.L t_dbcc
                DC.B 0,0,$5B,$C8
                DX.B 'DBNE'
                DC.L t_dbcc
                DC.B 0,0,$56,$C8
                DX.B 'DBNZ'
                DC.L t_dbcc
                DC.B 0,0,$56,$C8
                DX.B 'DBPL'
                DC.L t_dbcc
                DC.B 0,0,$5A,$C8
                DX.B 'DBRA'
                DC.L t_dbcc
                DC.B 0,0,$51,$C8
                DX.B 'DBT'
                DC.L t_dbcc
                DC.B 0,0,$50,$C8
                DX.B 'DBVC'
                DC.L t_dbcc
                DC.B 0,0,$58,$C8
                DX.B 'DBVS'
                DC.L t_dbcc
                DC.B 0,0,$59,$C8
                DX.B 'DBZE'
                DC.L t_dbcc
                DC.B 0,0,$57,$C8
                DX.B 'DIVS'
                DC.L t_chk
                DC.B 0,3,$81,$C0
                DX.B 'DIVU'
                DC.L t_chk
                DC.B 0,3,$80,$C0
                DX.B 'EOR'
                DC.L t_eor
                DC.B 0,0,$B1,$40
                DX.B 'EOR.B'
                DC.L t_eor
                DC.B 0,0,$B1,0
                DX.B 'EOR.L'
                DC.L t_eor
                DC.B 0,0,$B1,$80
                DX.B 'EOR.W'
                DC.L t_eor
                DC.B 0,0,$B1,$40
                DX.B 'EORI'
                DC.L t_andi
                DC.B 0,0,$0A,$40
                DX.B 'EORI.B'
                DC.L t_andi
                DC.B 0,0,$0A,0
                DX.B 'EORI.L'
                DC.L t_andi
                DC.B 0,0,$0A,$80
                DX.B 'EORI.W'
                DC.L t_andi
                DC.B 0,0,$0A,$40
                DX.B 'EXG'
                DC.L t_exg
                DC.B 0,0,$C1,0
                DX.B 'EXG.L'
                DC.L t_exg
                DC.B 0,0,$C1,0
                DX.B 'EXT'
                DC.L t_ext
                DC.B 0,0,$48,$80
                DX.B 'EXT.L'
                DC.L t_ext
                DC.B 0,0,$48,$C0
                DX.B 'EXT.W'
                DC.L t_ext
                DC.B 0,0,$48,$80
                DX.B 'ILLEGAL'
                DC.L t_nop
                DC.B 0,0,$4A,$FC
                DX.B 'JMP'
                DC.L t_jmp
                DC.B 0,0,$4E,$C0
                DX.B 'JSR'
                DC.L t_jmp
                DC.B 0,0,$4E,$80
                DX.B 'LEA'
                DC.L t_lea
                DC.B 0,0,$41,$C0
                DX.B 'LINEA'
                DC.L t_linea
                DC.B 0,0,$A0,0
                DX.B 'LINK'
                DC.L t_link
                DC.B 0,0,$4E,$50
                DX.B 'LSL'
                DC.L t_asl
                DC.B 0,0,$E3,$48
                DX.B 'LSL.B'
                DC.L t_asl
                DC.B 0,0,$E3,8
                DX.B 'LSL.L'
                DC.L t_asl
                DC.B 0,0,$E3,$88
                DX.B 'LSL.W'
                DC.L t_asl
                DC.B 0,0,$E3,$48
                DX.B 'LSR'
                DC.L t_asl
                DC.B 0,0,$E2,$48
                DX.B 'LSR.B'
                DC.L t_asl
                DC.B 0,0,$E2,8
                DX.B 'LSR.L'
                DC.L t_asl
                DC.B 0,0,$E2,$88
                DX.B 'LSR.W'
                DC.L t_asl
                DC.B 0,0,$E2,$48
                DX.B 'MOVE'
                DC.L t_move
                DC.B 0,3,$30,0
                DX.B 'MOVE.B'
                DC.L t_move
                DC.B 0,1,$10,0
                DX.B 'MOVE.L'
                DC.L t_move
                DC.B 0,2,$20,0
                DX.B 'MOVE.W'
                DC.L t_move
                DC.B 0,3,$30,0
                DX.B 'MOVEA'
                DC.L t_adda
                DC.B 0,3,$30,$40
                DX.B 'MOVEA.L'
                DC.L t_adda
                DC.B 0,2,$20,$40
                DX.B 'MOVEA.W'
                DC.L t_adda
                DC.B 0,3,$30,$40
                DX.B 'MOVEM'
                DC.L t_movem
                DC.B 0,0,$48,$80
                DX.B 'MOVEM.L'
                DC.L t_movem
                DC.B 0,0,$48,$C0
                DX.B 'MOVEM.W'
                DC.L t_movem
                DC.B 0,0,$48,$80
                DX.B 'MOVEP'
                DC.L t_movep
                DC.B 0,0,1,8
                DX.B 'MOVEP.L'
                DC.L t_movep
                DC.B 0,0,1,$48
                DX.B 'MOVEP.W'
                DC.L t_movep
                DC.B 0,0,1,8
                DX.B 'MOVEQ'
                DC.L t_moveq
                DC.B 0,0,$70,0
                DX.B 'MOVEQ.B'
                DC.L t_moveq
                DC.B 0,0,$70,0
                DX.B 'MULS'
                DC.L t_chk
                DC.B 0,3,$C1,$C0
                DX.B 'MULU'
                DC.L t_chk
                DC.B 0,3,$C0,$C0
                DX.B 'NBCD'
                DC.L t_clr
                DC.B 0,0,$48,0
                DX.B 'NEG'
                DC.L t_clr
                DC.B 0,0,$44,$40
                DX.B 'NEG.B'
                DC.L t_clr
                DC.B 0,0,$44,0
                DX.B 'NEG.L'
                DC.L t_clr
                DC.B 0,0,$44,$80
                DX.B 'NEG.W'
                DC.L t_clr
                DC.B 0,0,$44,$40
                DX.B 'NEGX'
                DC.L t_clr
                DC.B 0,0,$40,$40
                DX.B 'NEGX.B'
                DC.L t_clr
                DC.B 0,0,$40,0
                DX.B 'NEGX.L'
                DC.L t_clr
                DC.B 0,0,$40,$80
                DX.B 'NEGX.W'
                DC.L t_clr
                DC.B 0,0,$40,$40
                DX.B 'NOP'
                DC.L t_nop
                DC.B 0,0,$4E,$71
                DX.B 'NOT'
                DC.L t_clr
                DC.B 0,0,$46,$40
                DX.B 'NOT.B'
                DC.L t_clr
                DC.B 0,0,$46,0
                DX.B 'NOT.L'
                DC.L t_clr
                DC.B 0,0,$46,$80
                DX.B 'NOT.W'
                DC.L t_clr
                DC.B 0,0,$46,$40
                DX.B 'OR'
                DC.L t_and
                DC.B 0,0,$80,$40
                DX.B 'OR.B'
                DC.L t_and
                DC.B 0,0,$80,0
                DX.B 'OR.L'
                DC.L t_and
                DC.B 0,0,$80,$80
                DX.B 'OR.W'
                DC.L t_and
                DC.B 0,0,$80,$40
                DX.B 'ORI'
                DC.L t_andi
                DC.B 0,0,0,$40
                DX.B 'ORI.B'
                DC.L t_andi
                DC.B 0,0,0,0
                DX.B 'ORI.L'
                DC.L t_andi
                DC.B 0,0,0,$80
                DX.B 'ORI.W'
                DC.L t_andi
                DC.B 0,0,0,$40
                DX.B 'PEA'
                DC.L t_pea
                DC.B 0,0,$48,$40
                DX.B 'RESET'
                DC.L t_nop
                DC.B 0,0,$4E,$70
                DX.B 'ROL'
                DC.L t_asl
                DC.B 0,0,$E7,$58
                DX.B 'ROL.B'
                DC.L t_asl
                DC.B 0,0,$E7,$18
                DX.B 'ROL.L'
                DC.L t_asl
                DC.B 0,0,$E7,$98
                DX.B 'ROL.W'
                DC.L t_asl
                DC.B 0,0,$E7,$58
                DX.B 'ROR'
                DC.L t_asl
                DC.B 0,0,$E6,$58
                DX.B 'ROR.B'
                DC.L t_asl
                DC.B 0,0,$E6,$18
                DX.B 'ROR.L'
                DC.L t_asl
                DC.B 0,0,$E6,$98
                DX.B 'ROR.W'
                DC.L t_asl
                DC.B 0,0,$E6,$58
                DX.B 'ROXL'
                DC.L t_asl
                DC.B 0,0,$E5,$50
                DX.B 'ROXL.B'
                DC.L t_asl
                DC.B 0,0,$E5,$10
                DX.B 'ROXL.L'
                DC.L t_asl
                DC.B 0,0,$E5,$90
                DX.B 'ROXL.W'
                DC.L t_asl
                DC.B 0,0,$E5,$50
                DX.B 'ROXR'
                DC.L t_asl
                DC.B 0,0,$E4,$50
                DX.B 'ROXR.B'
                DC.L t_asl
                DC.B 0,0,$E4,$10
                DX.B 'ROXR.L'
                DC.L t_asl
                DC.B 0,0,$E4,$90
                DX.B 'ROXR.W'
                DC.L t_asl
                DC.B 0,0,$E4,$50
                DX.B 'RTE'
                DC.L t_nop
                DC.B 0,0,$4E,$73
                DX.B 'RTR'
                DC.L t_nop
                DC.B 0,0,$4E,$77
                DX.B 'RTS'
                DC.L t_nop
                DC.B 0,0,$4E,$75
                DX.B 'SBCD'
                DC.L t_abcd
                DC.B 0,0,$81,0
                DX.B 'SCC'
                DC.L t_clr
                DC.B 0,0,$54,$C0
                DX.B 'SCS'
                DC.L t_clr
                DC.B 0,0,$55,$C0
                DX.B 'SEQ'
                DC.L t_clr
                DC.B 0,0,$57,$C0
                DX.B 'SF'
                DC.L t_clr
                DC.B 0,0,$51,$C0
                DX.B 'SF.B'
                DC.L t_clr
                DC.B 0,0,$51,$C0
                DX.B 'SGE'
                DC.L t_clr
                DC.B 0,0,$5C,$C0
                DX.B 'SGT'
                DC.L t_clr
                DC.B 0,0,$5E,$C0
                DX.B 'SHI'
                DC.L t_clr
                DC.B 0,0,$52,$C0
                DX.B 'SLE'
                DC.L t_clr
                DC.B 0,0,$5F,$C0
                DX.B 'SLS'
                DC.L t_clr
                DC.B 0,0,$53,$C0
                DX.B 'SLT'
                DC.L t_clr
                DC.B 0,0,$5D,$C0
                DX.B 'SMI'
                DC.L t_clr
                DC.B 0,0,$5B,$C0
                DX.B 'SNE'
                DC.L t_clr
                DC.B 0,0,$56,$C0
                DX.B 'SPL'
                DC.L t_clr
                DC.B 0,0,$5A,$C0
                DX.B 'ST'
                DC.L t_clr
                DC.B 0,0,$50,$C0
                DX.B 'ST.B'
                DC.L t_clr
                DC.B 0,0,$50,$C0
                DX.B 'STOP'
                DC.L t_stop
                DC.B 0,0,$4E,$72
                DX.B 'SUB'
                DC.L t_add
                DC.B 0,0,$90,$40
                DX.B 'SUB.B'
                DC.L t_add
                DC.B 0,0,$90,0
                DX.B 'SUB.L'
                DC.L t_add
                DC.B 0,0,$90,$80
                DX.B 'SUB.W'
                DC.L t_add
                DC.B 0,0,$90,$40
                DX.B 'SUBA'
                DC.L t_adda
                DC.B 0,$80,$90,$C0
                DX.B 'SUBA.L'
                DC.L t_adda
                DC.B 0,$80,$91,$C0
                DX.B 'SUBA.W'
                DC.L t_adda
                DC.B 0,$80,$90,$C0
                DX.B 'SUBI'
                DC.L t_addi
                DC.B 0,0,4,$40
                DX.B 'SUBI.B'

                DC.L t_addi
                DC.B 0,0,4,0
                DX.B 'SUBI.L'
                DC.L t_addi
                DC.B 0,0,4,$80
                DX.B 'SUBI.W'
                DC.L t_addi
                DC.B 0,0,4,$40
                DX.B 'SUBQ'
                DC.L t_addq
                DC.B 0,0,$51,$40
                DX.B 'SUBQ.B'
                DC.L t_addq
                DC.B 0,0,$51,0
                DX.B 'SUBQ.L'
                DC.L t_addq
                DC.B 0,0,$51,$80
                DX.B 'SUBQ.W'
                DC.L t_addq
                DC.B 0,0,$51,$40
                DX.B 'SUBX'
                DC.L t_abcd
                DC.B 0,0,$91,$40
                DX.B 'SUBX.B'
                DC.L t_abcd
                DC.B 0,0,$91,0
                DX.B 'SUBX.L'
                DC.L t_abcd
                DC.B 0,0,$91,$80
                DX.B 'SUBX.W'
                DC.L t_abcd
                DC.B 0,0,$91,$40
                DX.B 'SVC'
                DC.L t_clr
                DC.B 0,0,$58,$C0
                DX.B 'SVS'
                DC.L t_clr
                DC.B 0,0,$59,$C0
                DX.B 'SWAP'
                DC.L t_ext
                DC.B 0,0,$48,$40
                DX.B 'SWAP.L'
                DC.L t_ext
                DC.B 0,0,$48,$40
                DX.B 'TAS'
                DC.L t_clr
                DC.B 0,0,$4A,$C0
                DX.B 'TAS.B'
                DC.L t_clr
                DC.B 0,0,$4A,$C0
                DX.B 'TRAP'
                DC.L t_trap
                DC.B 0,0,$4E,$40
                DX.B 'TRAPV'
                DC.L t_nop
                DC.B 0,0,$4E,$76
                DX.B 'TST'
                DC.L t_clr
                DC.B 0,0,$4A,$40
                DX.B 'TST.B'
                DC.L t_clr
                DC.B 0,0,$4A,0
                DX.B 'TST.L'
                DC.L t_clr
                DC.B 0,0,$4A,$80
                DX.B 'TST.W'
                DC.L t_clr
                DC.B 0,0,$4A,$40
                DX.B 'UNLINK'
                DC.L t_unlk
                DC.B 0,0,$4E,$58
                DX.B 'UNLK'
                DC.L t_unlk
                DC.B 0,0,$4E,$58
                DX.B 'XOR'
                DC.L t_eor
                DC.B 0,0,$B1,$40
                DX.B 'XOR.B'
                DC.L t_eor
                DC.B 0,0,$B1,0
                DX.B 'XOR.L'
                DC.L t_eor
                DC.B 0,0,$B1,$80
                DX.B 'XOR.W'
                DC.L t_eor
                DC.B 0,0,$B1,$40
                DX.B 'XORI'
                DC.L t_andi
                DC.B 0,0,$0A,$40
                DX.B 'XORI.B'
                DC.L t_andi
                DC.B 0,0,$0A,0
                DX.B 'XORI.L'
                DC.L t_andi
                DC.B 0,0,$0A,$80
                DX.B 'XORI.W'
                DC.L t_andi
                DC.B 0,0,$0A,$40
                DC.B -1
                EVEN
                ENDPART

********************************************************************************
* DO - Run command on the PC                                                   *
********************************************************************************
                PART 'cmd_do'
cmd_do:         bra     cmd_call1
                ENDPART

********************************************************************************
* TRAP - Trap-Breakpoints administer                                           *
********************************************************************************
                PART 'cmd_obser'
break_tab:      DC.W 1,gemdos_break,$7E
                DC.W 13,bios_break,$0B
                DC.W 14,xbios_break,$57
                DC.W $2A,aes_break,125
                DC.W $2B,vdi_break,131
                DC.W $13,bios_break,$0B
                DC.W $14,xbios_break,$57
                DC.W -1

cmd_obser:      bsr     get                         ;Follow parameters?
                beq     cmd_o90                     ;Show all trap breakpoints
                cmp.b   #'O',D0
                beq     cmd_oboff
                sf      observe_off(A4)             ;Turn on Observe
                cmp.b   #'K',D0
                beq     cmd_ob4                     ;Delete all trap breakpoints
                bsr     get_term                    ;Get trap number
                swap    D1
                tst.w   D1
                bne     illequa                     ;Number too big!
                swap    D1
                lea     break_tab-4(PC),A1
cmd_ob1:        addq.l  #4,A1
                move.w  (A1)+,D2
                bmi     illequa                     ;Trap not found
                cmp.w   D1,D2
                bne.s   cmd_ob1
                move.w  D1,D7
                move.w  (A1)+,D2                    ;Offset for A4
                moveq   #0,D3
                move.w  (A1)+,D3                    ;Max.Function number.
                tst.w   D0
                beq.s   cmd_ob6                     ;Set trap breakpoints of the annual trap
                cmp.b   #'.',D0
                beq.s   cmd_ob2                     ;Delete trap breakpoints of the annual trap
cmd_ob0:        tst.w   D0
                beq     ret_jump
                cmp.b   #',',D0
                bne     synerr
                bsr     get                         ;Follow parameters?
                cmp.b   #'?',D0
                beq     cmd_ob9                     ;Display parameters
                cmp.b   #'*',D0
                beq.s   cmd_ob6                     ;Set all breakpoints
                bsr     get_term                    ;Get function number
                addq.l  #1,D1
                bmi     illequa                     ;<-1 is not allowed
                subq.l  #1,D1
                bmi.s   cmd_ob6                     ;-1 = Set all breakpoints
                cmp.l   D3,D1
                bhi     illequa                     ;Too great for this trap
                lea     0(A4,D2.w),A1
                cmp.b   #'.',D0                     ;Delete Entry?
                beq.s   cmd_ob8                     ;Yes!=>
                st      0(A1,D1.w)                  ;Set individual trap breakpoint
                bra.s   cmd_ob0
cmd_ob8:        lea     0(A1,D1.w),A2
                sf      (A2)                        ;Set individual trap breakpoint
                bsr     get
                bra.s   cmd_ob0

cmd_ob2:        lea     0(A4,D2.w),A0
cmd_ob3:        sf      0(A0,D3.w)                  ;Delete all enspr.Breakpoints
                dbra    D3,cmd_ob3
                jmp     (A4)

cmd_ob6:        lea     0(A4,D2.w),A0
cmd_ob7:        st      0(A0,D3.w)                  ;Set all Enspr.Breakpoints
                dbra    D3,cmd_ob7
                jmp     (A4)

cmd_ob4:        lea     gemdos_break(A4),A1
                lea     end_of_breaks(A4),A0
cmd_ob5:        clr.b   (A1)+                       ;Delete all trap breakpoints
                cmpa.l  A0,A1
                blo.s   cmd_ob5
                jmp     (A4)

cmd_o90:        lea     break_tab(PC),A6
                moveq   #4,D5                       ;Spend 5 traps
cmd_o91:        pea     _trap(PC)
                jsr     @print_line(A4)
                jsr     @space(A4)
                moveq   #'#',D0
                jsr     @chrout(A4)
                moveq   #0,D1
                move.w  (A6)+,D1
                move.w  D1,D7
                bsr     hexout
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                move.w  (A6)+,D2
                move.w  (A6)+,D3
                bsr.s   cmd_o3o
                dbra    D5,cmd_o91
                jmp     (A4)

cmd_ob9:        bsr.s   cmd_o3o
                jmp     (A4)

cmd_o3o:        moveq   #-1,D1
                lea     0(A4,D2.w),A1
                cmp.w   #$2A,D7
                beq     cmd_o40
                cmp.w   #$2B,D7
                beq.s   cmd_o50
                lea     gemdos_befs,A2
                cmp.w   #1,D7
                beq.s   cmd_o30
                lea     bios_befs,A2
                cmp.w   #$13,D7
                beq.s   cmd_o30
                cmp.w   #$0D,D7
                beq.s   cmd_o30
                lea     xbios_befs,A2
cmd_o30:        moveq   #';',D0
                jsr     @chrout(A4)
                moveq   #25,D6
                bra.s   cmd_o32
cmd_o31:        moveq   #',',D0
                jsr     @chrout(A4)
cmd_o32:        addq.w  #1,D1
                cmp.w   D3,D1
                bhi.s   cmd_o33
                cmp.b   (A2),D1
                bne.s   cmd_o32
                addq.l  #1,A2
                move.b  (A2)+,D0                    ;Stack format over
                rol.b   #2,D0
                andi.b  #3,D0
                beq.s   cmd_o35
                addq.l  #1,A2
cmd_o35:        tst.b   (A2)+
                bne.s   cmd_o35                     ;Function name
                tst.b   0(A1,D1.w)
                beq.s   cmd_o32
                bmi.s   cmd_o34
                moveq   #'*',D0
                jsr     @chrout(A4)
cmd_o34:        bsr     hexbout
                dbra    D6,cmd_o31
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                bra.s   cmd_o30                     ;Next line
cmd_o33:        jsr     c_cleft
                jsr     @c_eol(A4)
                jmp     @crout(A4)

cmd_o50:        lea     vdi_all,A2
                bra.s   cmd_o45
cmd_o40:        lea     aes_all,A2
cmd_o45:        moveq   #';',D0                     ;AES-Observe
                jsr     @chrout(A4)
                moveq   #25,D6
                bra.s   cmd_o42
cmd_o41:        moveq   #',',D0
                jsr     @chrout(A4)
cmd_o42:        moveq   #0,D1
                move.b  (A2)+,D1                    ;Get permitted function number
                beq.s   cmd_o43                     ;End
                tst.b   0(A1,D1.w)                  ;Test on Breakpoint
                beq.s   cmd_o42                     ;nobody there
                bmi.s   cmd_o44                     ;normal Breakpoint
                moveq   #'*',D0
                jsr     @chrout(A4)                 ;There was canceled
cmd_o44:        bsr     hexbout
                dbra    D6,cmd_o41
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                bra.s   cmd_o45                     ;Next line
cmd_o43:        jsr     c_cleft
                jsr     @c_eol(A4)
                jmp     @crout(A4)

cmd_oboff:      st      observe_off(A4)             ;Switch off Observe
                move.l  old_gemdos(PC),$84.w
                move.l  old_aesvdi(PC),$88.w        ;Old vectors in again
                move.l  old_bios(PC),$B4.w
                move.l  old_xbios(PC),$B8.w
                move.l  old_critic(PC),$0404.w
                jmp     (A4)
                ENDPART

********************************************************************************
* BSSCLEAR - BSS-Delete the area of the loaded program                         *
********************************************************************************
                PART 'cmd_bclr'
cmd_bclr:       move.l  basep(A4),D0
                beq     no_prg
                movea.l D0,A0
                move.l  $1C(A0),D0                  ;Length of the BSS area
                movea.l $18(A0),A0                  ;Initial address of the BSS area
                bra.s   cmd_bc2
cmd_bc1:        clr.b   (A0)+                       ;Delete BSS area
cmd_bc2:        subq.l  #1,D0
                bpl.s   cmd_bc1
                jmp     (A4)
                ENDPART

********************************************************************************
* INITREGISTER - Reinitialize registers                                        *
********************************************************************************
                PART 'cmd_ireg'
cmd_ireg:       bsr.s   initreg
                bsr     set_reg
                jmp     (A4)
                ENDPART

                PART 'initreg'
initreg:        move.l  first_free(A4),_pc(A4)
                move.w  #$0300,_sr(A4)
                movea.l #debug_sstack,A0
                adda.l  A4,A0
                move.l  A0,_ssp(A4)                 ;SSP set
                movea.l merk_act_pd(A4),A1
                movea.l 4(A1),A0                    ;Determine the TPA end
                move.l  A1,-(A0)                    ;basepageadr
                clr.l   -(A0)                       ;No return code
                move.l  A0,_usp(A4)                 ;Use USP
                move.l  A0,rega7(A4)
                lea     regs(A4),A0
                moveq   #14,D0
cmd_ir1:        clr.l   (A0)+                       ;Delete all other registers
                dbra    D0,cmd_ir1
                rts
                ENDPART

********************************************************************************
* CURSOR [Art] - Change cursor shape                                           *
********************************************************************************
                PART 'cmd_swchcur'
cmd_swchcur:    moveq   #0,D1                       ;Inverse is Default
                bsr     get
                beq.s   cmd_swc
                bsr     get_term
                cmp.l   #3,D1                       ;max4Cursorformen
                bhi     illequa
                lsl.w   #4,D1                       ;Time 16 (length of a cursor shape)
cmd_swc:        move.w  D1,cursor_form(A4)          ;New cursor shape
                jmp     (A4)
                ENDPART

********************************************************************************
* SWITCH - monitor switching                                                   *
********************************************************************************
                PART 'cmd_switch'
cmd_switch:     move    SR,-(SP)
                ori     #$0700,SR                   ;IRQs lock
                lea     debugger_scr(A4),A0
                movea.l scr_adr(A0),A1
                move.w  #1999,D0
cmd_switch1:    clr.l   (A1)+                       ;Delete the Hires
                clr.l   (A1)+
                clr.l   (A1)+
                clr.l   (A1)+
                dbra    D0,cmd_switch1
                bchg    #6,scr_moni(A0)             ;Switch monitor
                move.b  scr_rez(A0),D0
                lsr.b   #1,D0
                bne.s   cmd_switch2                 ;1=>2 bzw. 2=>1
                moveq   #2,D0
cmd_switch2:    move.b  D0,scr_rez(A0)              ;Switch the resolution
                lea     no_overscan(A4),A1
                bsr     restore_scr
                jsr     @redraw_all(A4)             ;Rebuild screen
                move    (SP)+,SR                    ;Release IRQS
                jmp     (A4)
                ENDPART

********************************************************************************
* CHECKSUMME [Adr][,[Value][,[Wordanz][,Art]]]                                 *
********************************************************************************
                PART 'cmd_checksum'
cmd_checksum:   movea.l dsk_adr(A4),A6              ;defaultadr
                move.w  #$1234,D7                   ;defaultchecksum
                moveq   #0,D4
                move.w  #$FF,D4                     ;defaultwords
                moveq   #0,D5                       ;defaultart (0=ADD, 1=EOR)
                bsr     get
                moveq   #-1,D6
                bsr     get_it                      ;Address (alles erlaubt)
                beq.s   cmd_ch1
                bvc.s   cmd_c1h
                movea.l D1,A6
                bsr     chkcom
                beq.s   cmd_ch1                     ;End of the input
cmd_c1h:        move.l  #$FFFF,D6
                bsr     get_it                      ;Value
                beq.s   cmd_ch1
                bvc.s   cmd_c2h
                move.w  D1,D7
                bsr     chkcom
                beq.s   cmd_ch1                     ;End of the input
cmd_c2h:        move.l  #$FFFFFF,D6
                bsr     get_it                      ;number
                beq.s   cmd_ch1
                bvc.s   cmd_c3h
                move.l  D1,D4
                bsr     chkcom
                beq.s   cmd_ch1                     ;End of the input
cmd_c3h:        cmp.b   #'A',D0                     ;ADD
                beq.s   cmd_ch1
                moveq   #1,D5
                cmp.b   #'X',D0                     ;XOR
                beq.s   cmd_ch1
                cmp.b   #'E',D0                     ;EOR
                bne     synerr
cmd_ch1:        subq.w  #1,D6
                beq     illequa
                tst.w   D5
                beq.s   cmd_ch2
                moveq   #0,D1
cmd_ch5:        move.w  (A6)+,D0
                eor.w   D0,D1
                subq.l  #1,D4
                bpl.s   cmd_ch5
                bra.s   cmd_ch4
cmd_ch2:        moveq   #0,D1
cmd_ch3:        add.w   (A6)+,D1                    ;Calculate checksum
                subq.l  #1,D4
                bpl.s   cmd_ch3
                neg.w   D1
                add.w   D7,D1                       ;Checksum now in D1
cmd_ch4:        pea     cmd_cht(PC)
                jsr     @print_line(A4)
                moveq   #'$',D0
                jsr     @chrout(A4)
                bsr     hexwout                     ;Provide checksum
                jsr     @c_eol(A4)                  ;Delete line residue
                jsr     @crout(A4)                  ;CR
                jmp     (A4)

                SWITCH language
                CASE 0
cmd_cht:        DC.B 'Prüfsumme = ',0
                CASE 1
cmd_cht:        DC.B 'Checksum = ',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* RESET [ALL|VEK] - Reset vectors                                              *
********************************************************************************
                PART 'cmd_reset'
cmd_reset:      lea     cmd_rst1(PC),A6
                bsr     get
                beq.s   cmd_reset3                  ;Vek is default
                cmp.b   #'V',D0                     ;VEK?
                beq.s   cmd_reset3
                cmp.b   #'A',D0                     ;ALL?
                bne     synerr
                tst.b   le_allowed(A4)              ;LE permitted?
                beq     cmd_cont1                   ;No!=>
                tst.b   help_allow(A4)              ;CTRL-HELP permitted?
                bmi     cmd_cont1                   ;Yes! =>
                move.l  A6,-(SP)
                jsr     @print_line(A4)
                lea     cmd_rst2(PC),A0
                jsr     ask_user

                move.w  _fhdle2(A4),D0              ;Protocol file does not exist
                bls.s   cmd_reset1
                move.w  D0,-(SP)
                move.w  #$3E,-(SP)
                trap    #1                          ;Fclose()
                addq.l  #4,SP
cmd_reset1:     bsr     set_vek                     ;Set vectors
                bsr     reset_all

                move.l  old_trap3(PC),$8C.w
                movea.l old_stack(A4),SP
                move    #$0300,SR                   ;USER-Mode
                movea.l old_usp(A4),SP
                moveq   #0,D7
                bra     start

cmd_reset3:     move.l  A6,-(SP)
                jsr     @print_line(A4)
                lea     cmd_rst3(PC),A0
                jsr     ask_user
                bsr     set_vek                     ;Set vectors
                jmp     (A4)

                SWITCH language
                CASE 0
cmd_rst1:       DC.B 'Sicher, daß Sie ',0
cmd_rst2:       DC.B 'alles zurücksetzen wollen? (j/n) ',0
cmd_rst3:       DC.B 'die Systemvektoren zurücksetzen wollen? (j/n) ',0

                CASE 1
cmd_rst1:       DC.B 'Sure you want to reset ',0
cmd_rst2:       DC.B 'all? (y/n) ',0
cmd_rst3:       DC.B 'the system vectors? (y/n) ',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* "..." Output String                                                          *
********************************************************************************
                PART 'cmd_send'
cmd_send:       move.b  (A0)+,D0
                beq.s   cmd_sd1                     ;Line end (pointer back)
                cmp.b   #'"',D0
                beq.s   cmd_sd0                     ;End of the input
                cmp.b   #'\',D0
                bne.s   cmd_sd2
                movea.l A0,A1                       ;Pointer notice
                bsr     get                         ;Get the 1st sequence sign
                cmp.b   #'\',D0
                beq.s   cmd_sd2                     ;'\' but spend
                moveq   #$10,D2                     ;hexadecimal
                bsr     chkval                      ;fits's?
                bcc.s   cmd_sd3                     ;then spend '\'
                lsl.w   #4,D0
                move.w  D0,D1
                bsr     get                         ;Get the second sequence sign
                bsr     chkval                      ;hexadecimal?
                bcc.s   cmd_sd3                     ;Pointer back, '\' output
                or.w    D1,D0                       ;Put together code
                or.w    #$FF00,D0                   ;Code directly to the printer (or in the file)
                bra.s   cmd_sd2                     ;output
cmd_sd3:        movea.l A1,A0                       ;Pointer back
                moveq   #'\',D0                     ;'\' output
cmd_sd2:        jsr     @chrout(A4)                 ;Spend characters
                bra.s   cmd_send
cmd_sd1:        subq.l  #1,A0
cmd_sd0:        move.b  (A0),D0
                cmp.b   #';',D0                     ;At ';'Do not spend CR
                beq.s   cmd_sd5
                jsr     @crout(A4)
cmd_sd5:        jmp     (A4)
                ENDPART

********************************************************************************
* GETCACHE                                                                     *
********************************************************************************
                PART 'cmd_getcach'
cmd_getcach:    bsr.s   getcache
                jmp     (A4)
                ENDPART

********************************************************************************

                PART 'getcache'
getcache:       move.l  reg_pos(A4),D0
                move.l  D0,trace_pos(A4)            ;Displayed REG = current reg
                lea     regs(A4),A5
                movea.l D0,A6
                moveq   #38,D0
getcache1:      move.w  (A6)+,(A5)+                 ;Set
                dbra    D0,getcache1
                bra     rgout                       ;and spend (because of the Closer)
                ENDPART

********************************************************************************
* CLRCACHE                                                                     *
********************************************************************************
                PART 'cmd_clrcach'
cmd_clrcach:    movea.l #trace_buff,A0
                adda.l  A4,A0
                move.l  A0,trace_pos(A4)            ;Position im Tracebuffer
                move.l  A0,reg_pos(A4)
                movea.l #trace_buffend,A1
                adda.l  A4,A1
cmd_cc1:        clr.w   (A0)+                       ;Delete command buffer
                cmpa.l  A1,A0
                blo.s   cmd_cc1
                jmp     (A4)
                ENDPART

********************************************************************************
* SYMBOLTABLE [Symbol]                                                         *
********************************************************************************
                PART 'cmd_symbol'
cmd_symbol:     tst.l   sym_size(A4)
                beq     no_syms                     ;Error if no symbol table available
                movea.l default_adr(A4),A5
                suba.l  A2,A2                       ;Initial address
                moveq   #-1,D0
                movea.l D0,A3                       ;Resignance
                move.l  basep(A4),D0                ;Loading program?
                beq.s   cmd_sy0
                movea.l D0,A2                       ;then only symbol values> output basepage
                movea.l $18(A2),A3
                adda.l  $1C(A2),A3                  ;Pointer behind the BSS segment
cmd_sy0:        bsr     get2xadr
                move.l  A3,D1
                movea.l sym_adr(A4),A6              ;Initial address of the symbol table
cmd_sy1:        cmpa.l  sym_end(A4),A6
                bhs.s   cmd_sy3                     ;End!
                cmpa.l  10(A6),A2                   ;Akt.Label <initial address
                bhi.s   cmd_sy4                     ;not found yet
                tst.l   D1                          ;End address available?
                beq.s   cmd_sy5                     ;Skip, if not!
                cmpa.l  10(A6),A3                   ;aktLabel >Endadresse
                bls.s   cmd_sy3                     ;then ready
cmd_sy5:        bsr.s   sym_out                     ;Spend symbol
                movem.l D1,-(SP)
                jsr     @crout(A4)                  ;Still a Cr
                bsr     check_keyb                  ;Button pressed?
                movem.l (SP)+,D1
                bmi.s   cmd_sy3
                tst.l   D1
                bne.s   cmd_sy1                     ;End address exists => Line content
                dbra    D2,cmd_sy1                  ;Already spent all the labels?
cmd_sy3:        move.l  A5,default_adr(A4)          ;Reset Default-ADR
                jmp     (A4)
cmd_sy4:        lea     14(A6),A6                   ;Label over
                bra.s   cmd_sy1

sym_out:        movem.l D0-A5,-(SP)
                move.l  A6,D1
                jsr     @anf_adr(A4)
                moveq   #'(',D0
                jsr     @chrout(A4)
                moveq   #'.',D0
                jsr     @chrout(A4)
                move.l  (A6),-(SP)
                jsr     @print_line(A4)             ;Spend label names
                moveq   #34,D0
                jsr     spacetab
                addq.l  #8,A6
                move.w  (A6)+,D5
                move.l  (A6)+,D1
                bsr     hexlout                     ;Output value of the symbol
                jsr     @space(A4)
                lea     symtxt(PC),A5
                lea     symtxt1(PC),A3
                moveq   #' ',D0
                cmp.b   #$48,D5
                bne.s   sym_ou2
                moveq   #'L',D0                     ;longLabel
sym_ou2:        jsr     @chrout(A4)
                lsr.w   #8,D5                       ;Information from bit 15-8 to 7-0
                moveq   #-1,D4                      ;Normally: no indirect value
                moveq   #7,D6                       ;Start at bit 7
sym_ou3:        moveq   #' ',D0
                btst    D6,D5                       ;Flag set?
                beq.s   sym_ou4                     ;No => Text over
                tst.b   (A3)
                beq.s   sym_ou31
                moveq   #0,D4                       ;Indirect value available
sym_ou31:       move.b  (A5),D0                     ;Output flag text
sym_ou4:        jsr     @chrout(A4)
                addq.l  #1,A5
                addq.l  #1,A3
                dbra    D6,sym_ou3                  ;alleBits?
                tst.b   D4                          ;<> 0 If constant
                bne.s   sym_ou6                     ;then no indirect value output
                move.l  A6,-(SP)
                moveq   #54,D0
                jsr     spacetab
                movea.l -(A6),A6                    ;Get value of the symbol again
                moveq   #11,D2                      ;Spend max.12 bytes
sym_ou8:        bsr     check_read                  ;Address readable?
                bne.s   sym_ou5                     ;No!
                move.b  (A6)+,D1                    ;Byte
                bsr     hexbout                     ;and spend
                bra.s   sym_ou7                     ;Further...
sym_ou5:        addq.l  #1,A6                       ;Skip byte
                moveq   #'-',D0
                jsr     chrout                      ;"--" output
                jsr     chrout
sym_ou7:        dbra    D2,sym_ou8                  ;Already all 12 bytes?
                movea.l (SP)+,A6
sym_ou6:        movem.l (SP)+,D0-A5
                rts

symtxt:         DC.B '+KGRXDTB'
symtxt1:        DC.B 0,0,0,0,0,1,1,1
                EVEN
                ENDPART

********************************************************************************
* FOPEN Filename                                                               *
********************************************************************************
                PART 'cmd_fopen'
cmd_fopen:      move.w  _fhdle2(A4),D0              ;Protocol file already exists
                bhi     fileer2
                bsr     getnam                      ;Get filenames
                beq     synerr
                moveq   #1,D0
                jsr     graf_mouse                  ;Turn the disk on
                bsr     do_mediach                  ;Trigger Media-Change
                clr.w   -(SP)
                move.l  A2,-(SP)
                move.w  #$3C,-(SP)
                bsr     do_trap_1                   ;fcreate()
                addq.l  #8,SP
                tst.w   D0
                bmi     toserr
                move.w  D0,_fhdle2(A4)              ;Handle of the log file
                jmp     (A4)
                ENDPART

********************************************************************************
* FCLOSE                                                                       *
********************************************************************************
                PART 'cmd_fclose'
cmd_fclose:     move.w  _fhdle2(A4),D0              ;Protocol file does not exist
                bls     file_er
                move.w  D0,-(SP)
                move.w  #$3E,-(SP)
                bsr     do_trap_1                   ;fclose()
                addq.l  #4,SP
                bsr     do_mediach                  ;Trigger Media-Change
                clr.w   _fhdle2(A4)                 ;Make handle invalid
                tst.w   D0
                bmi     toserr
                jmp     (A4)
                ENDPART

********************************************************************************
* FILE command                                                                 *
********************************************************************************
                PART 'cmd_file'
cmd_file:       move.w  _fhdle2(A4),D0              ;Protokol file does not exist
                bls     file_er
                moveq   #1,D0
                jsr     graf_mouse                  ;Turn the disk on
                clr.w   prn_pos(A4)                 ;Reset pointer
                move.b  #1,device(A4)               ;Flag for log file
                bra     inp_loop1                   ;to the next sign
                ENDPART

********************************************************************************
* LINE                                                                         *
********************************************************************************
                PART 'cmd_line'
cmd_line:       moveq   #78,D1
cmd_li1:        moveq   #'-',D0
                jsr     @chrout(A4)
                dbra    D1,cmd_li1
                jsr     @crout(A4)
                jmp     (A4)
                ENDPART

********************************************************************************
* CR [lines]                                                                   *
********************************************************************************
                PART 'cmd_crout'
cmd_crout:      moveq   #0,D1                       ;A Cr is default
                bsr     get
                beq.s   cmd_cr1
                bsr     get_term
                subq.l  #1,D1                       ;For DBRA
                bmi     illequa
                cmp.l   #99,D1
                bhi     illequa                     ;More than 100 does not work!
cmd_cr1:        moveq   #13,D0                      ;Output D1 + 1 CRS
                jsr     @crout(A4)
                dbra    D1,cmd_cr1
                jmp     (A4)
                ENDPART

********************************************************************************
* GETREGISTER [Adr]                                                            *
********************************************************************************
                PART 'cmd_getreg'
cmd_getreg:     lea     $0300.w,A6
                bsr     get
                beq.s   cmd_gr1
                bsr     get_term
                movea.l D1,A6
                bsr     check_write
                bne     illequa
cmd_gr1:        lea     regs(A4),A5
                moveq   #38,D0                      ;Copy 39 Words
cmd_gr2:        move.w  (A6)+,(A5)+
                dbra    D0,cmd_gr2
                jmp     (A4)

                ENDPART

********************************************************************************
* FREE [Drv]                                                                   *
********************************************************************************
                PART 'cmd_free'
cmd_free:       bsr     get
                bne.s   cmd_free1
                moveq   #-1,D0
                move.l  D0,-(SP)
                move.w  #$48,-(SP)
                bsr     do_trap_1
                addq.l  #6,SP
                move.l  D0,D1
                bsr     dezout
                pea     free_txt(PC)
                jsr     @print_line(A4)
                jmp     (A4)
cmd_free1:      move.w  D0,D7                       ;Drive Brands
                sub.w   #'A',D7
                bmi     illdrv
                cmp.w   #$0F,D7
                bhi     illdrv
                bsr     do_mediach                  ;Trigger Media-Change
                bsr.s   drive_free1
                jmp     (A4)

                SWITCH language
                CASE 0
free_txt:       DC.B ' Bytes frei.',13,0
dfree_txt:      DC.B ' Bytes auf dem Laufwerk '
dfr_drv:        DC.B 'X: frei.',13,0

                CASE 1
free_txt:       DC.B ' Bytes free.',13,0
dfree_txt:      DC.B ' Bytes on drive '
dfr_drv:        DC.B 'X: free.',13,0
                ENDS

                EVEN
drive_free:     move.w  #$19,-(SP)
                bsr     do_trap_1                   ;Dgetdrv()
                addq.l  #2,SP
                move.l  D0,D7
drive_free1:    moveq   #1,D0
                jsr     graf_mouse                  ;Turn the disk on
                addq.w  #1,D7
                lea     dfr_drv(PC),A0
                move.b  D7,(A0)
                ori.b   #$40,(A0)                   ;Drive insert into the text
                lea     spaced2(A4),A6
                bsr.s   dfree
                tst.l   D0
                bmi     toserr
                move.l  (A6),D1                     ;Number of free clusters
                move.l  8(A6),D2                    ;Bytes / sector.
                bsr     lmult
                move.l  12(A6),D1                   ;Sectors / clusters
                bsr     lmult
                move.l  D2,D1
                bsr     dezout
                pea     dfree_txt(PC)
                jsr     @print_line(A4)
                rts
                ENDPART

********************************************************************************
* Own DFREE function                                                           *
********************************************************************************
                PART 'dfree'
dfree:          subq.w  #1,D7                       ;News drive
                bmi     dfree_error_own             ;Go to the original routine

                move.w  D7,-(SP)
                move.w  #7,-(SP)
                trap    #13                         ;Getbpb ()
                addq.l  #4,SP
                tst.l   D0
                bmi.s   dfree_error                 ;Device possibly not there!

                movea.l D0,A0                       ;Address of BPB-Blocks
                move.w  10(A0),D6                   ;Fatrec - starting sector there 2.Fat
                move.w  14(A0),D5                   ;NumCl - total number of clusters
                btst    #0,17(A0)
                beq.s   dfree_error_own             ;12-bit FAT => does not work yet!

                clr.l   (A6)+
                clr.l   (A6)+
                clr.l   (A6)+                       ;Delete transfer field first
                clr.l   (A6)
                lea     -12(A6),A6
                move.w  D5,6(A6)                    ;Total number of clusters
                move.w  (A0)+,10(A6)                ;Bytes for sector
                move.w  (A0),14(A6)                 ;Sectors per cluster

                movea.l #allg_buffer,A5
                adda.l  A4,A5                       ;Buffer for a sector of the FAT
                moveq   #0,D4                       ;Number of free clusters = 0
                moveq   #0,D3
dfree0:         move.w  D7,-(SP)                    ;drive
                move.w  D6,-(SP)                    ;Fatrec
                move.w  #2,-(SP)                    ;Always read 2 sectors
                move.l  A5,-(SP)                    ;Buffer for the sector
                clr.w   -(SP)                       ;normally
                move.w  #4,-(SP)
                trap    #13                         ;Rwabs()
                lea     14(SP),SP
                tst.l   D0
                bmi.s   dfree_error                 ;Readfeels
                addq.w  #2,D6                       ;FATER + 2
                movea.l A5,A0
                move.w  #511,D0                     ;512 Cluster Pro 2 sectors of the FAT
                tas.b   D3                          ;1.Stor with the first three clusters?
                bne.s   dfree1                      ;No!=>
                addq.l  #6,A0                       ;The first of the clusters will not
                subq.w  #3,D0                       ;counted in!
                subq.w  #3,D5                       ;3 clusters already deduct
dfree1:         tst.w   (A0)+
                bne.s   dfree2
                addq.w  #1,D4                       ;found a free cluster
dfree2:         subq.w  #1,D5                       ;numcl1
                dbeq    D0,dfree1
                bne.s   dfree0                      ;End not yet reached, it goes on
                move.l  D4,(A6)                     ;Number of free clusters according to D0
dfree_error:    rts

dfree_error_own:addq.w  #1,D7
                move.w  D7,-(SP)
                move.l  A6,-(SP)                    ;Buffer for 4-longs
                move.w  #$36,-(SP)
                bsr     do_trap_1                   ;Dfree(info,drive)
                addq.l  #8,SP
                rts
                ENDPART

********************************************************************************
* FORMAT [DS/SS][,Drive]                                                       *
********************************************************************************
                PART 'cmd_format'
cmd_format:     bsr     get
                moveq   #1,D5                       ;DS is default
                tst.b   D0
                beq.s   cmd_fm2
                cmp.b   #'D',D0
                beq.s   cmd_fm1
                moveq   #0,D5                       ;SS
                cmp.b   #'S',D0
                beq.s   cmd_fm1
                cmp.b   #',',D0
                bne     synerr
                moveq   #1,D5                       ;DS
                bsr     get
                bra.s   cmd_fm6
cmd_fm1:        bsr     get
                cmp.b   #'S',D0
                bne     synerr
                bsr     get
                bsr     chkcom                      ;Does a comma follow?
                beq.s   cmd_fm2                     ;No input
cmd_fm6:        moveq   #1,D6
                bsr     get_it                      ;Drive (0 or 1)
                beq     synerr
                bvc     synerr
                move.w  D1,dsk_drive(A4)
cmd_fm2:        lea     cmd_fm_txt(PC),A0
                jsr     ask_user                    ;Security query
                move.w  dsk_drive(A4),D7            ;The drive
                movea.l #allg_buffer,A6
                adda.l  A4,A6                       ;formatbuffer
                moveq   #1,D0
                jsr     graf_mouse                  ;Mouse pointer as a floppy disk
                moveq   #79,D6                      ;80 traces
cmd_fm3:        move.w  D5,D4
cmd_fm4:        clr.w   -(SP)
                move.l  #$87654321,-(SP)
                move.w  #1,-(SP)
                move.w  D4,-(SP)
                move.w  D6,-(SP)
                move.w  #9,-(SP)
                move.w  D7,-(SP)
                clr.l   -(SP)
                move.l  A6,-(SP)
                move.w  #10,-(SP)
                trap    #14                         ;flopmt()
                lea     26(SP),SP
                tst.l   D0
                bmi.s   cmd_fm5                     ;I suppose that did not work
                dbra    D4,cmd_fm4                  ;two sides?
                dbra    D6,cmd_fm3                  ;80 traces
                clr.w   -(SP)                       ;Non-executable
                move.w  D5,-(SP)
                addq.w  #2,(SP)                     ;2 = one-sided / 3 = double-sided
                move.l  #'MRF',-(SP)               ;Random serial number
                move.l  A6,-(SP)
                move.w  #$12,-(SP)
                trap    #14
                lea     14(SP),SP
                moveq   #1,D0
                move.w  D0,-(SP)
                clr.l   -(SP)
                move.w  D0,-(SP)
                move.w  D7,-(SP)
                clr.l   -(SP)
                move.l  A6,-(SP)
                move.w  #9,-(SP)
                trap    #14
                lea     20(SP),SP
cmd_fm5:        move.l  D0,D7
                move.l  D7,D0
                tst.l   D0
                bmi     toserr
                jmp     (A4)

cmd_fm_txt:     SWITCH language
                CASE 0
                DC.B 'Wollen Sie formatieren? (j/n) ',0
                CASE 1
                DC.B 'Sure you want to format a disk? (y/n) ',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* NAME Sourcefile,Destfile                                                     *
********************************************************************************
                PART 'cmd_name'
cmd_name:       bsr     getnam
                beq     synerr
                lea     spaced(A4),A1
                move.l  (A2)+,(A1)+
                move.l  (A2)+,(A1)+                 ;Copy filenames
                move.l  (A2)+,(A1)+
                move.l  (A2),(A1)
                bsr     get
                bsr     chkcom                      ;A comma?
                bsr     getnam_cont
                moveq   #1,D0
                jsr     graf_mouse                  ;Turn the disk on
                bsr     do_mediach                  ;Trigger Media-Change
                move.l  A2,-(SP)                    ;new
                pea     spaced(A4)                  ;old
                move.l  #$560000,-(SP)              ;frename()
                bsr     do_trap_1
                lea     12(SP),SP
                tst.l   D0
                bmi     toserr
                jmp     (A4)
                ENDPART

********************************************************************************
* FATTRIBUT [Name[,mode]]                                                      *
********************************************************************************
                PART 'cmd_fattrib'
cmd_fattrib:    moveq   #0,D2                       ;Get attributes
                bsr     get
                beq.s   cmd_ft1
                cmp.b   #',',D0
                beq.s   cmd_ft2
                bsr     getnam_cont                 ;Name
                bsr     get
cmd_ft1:        cmp.b   #',',D0
                bne.s   cmd_ft3
cmd_ft2:        moveq   #1,D2                       ;Put attributes
                bsr     get
                bsr     get_term                    ;Get FileMode
cmd_ft3:        lea     fname(A4),A2
                tst.b   (A2)
                beq     synerr                      ;No filename specified

                moveq   #1,D0
                jsr     graf_mouse                  ;Turn the disk on

                bsr     do_mediach                  ;Trigger Media-Change
                move.w  D1,-(SP)                    ;filemode
                move.w  D2,-(SP)                    ;0 = Read, 1 = write
                move.l  A2,-(SP)                    ;Pathname
                move.w  #$43,-(SP)
                bsr     do_trap_1                   ;Fattrib()
                lea     10(SP),SP
                move.l  D0,D2
                bmi     toserr
                pea     fmodes(PC)
                jsr     @print_line(A4)
                bsr     fatt_out                    ;Output File Attributes
                jmp     (A4)

                SWITCH language
                CASE 0
fmodes:         DC.B 'File-Attribute:',0
                CASE 1
fmodes:         DC.B 'File-attributes:',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* RMDIR Folder                                                                 *
********************************************************************************
                PART 'cmd_rmdir'
cmd_rmdir:      moveq   #$3A,D6                     ;Ddelete()
                bsr     getnam                      ;Get filenames
                beq     synerr
                moveq   #1,D0
                jsr     graf_mouse                  ;Turn the disk on
                bsr     do_mediach                  ;Trigger Media-Change
                move.l  A2,-(SP)
                move.w  D6,-(SP)
                bsr     do_trap_1                   ;Because of "-36: Access Denied"
                addq.l  #6,SP
                tst.w   D0
                beq     ret_jump
                cmp.w   #-36,D0
                bne     toserr
                bra.s   do_tos2
                ENDPART

********************************************************************************
* MKDIR Folder                                                                 *
********************************************************************************
                PART 'cmd_mkdir'
cmd_mkdir:      moveq   #$39,D6                     ;Dcreate()
                bsr     getnam                      ;Get filenames
                beq     synerr
do_tos2:        moveq   #1,D0
                jsr     graf_mouse                  ;Turn the disk on
                bsr     do_mediach                  ;Trigger Media-Change
                move.l  A2,-(SP)
                move.w  D6,-(SP)
                bsr     do_trap_1
                addq.l  #6,SP
                tst.w   D0
                bmi     toserr
                jmp     (A4)
                ENDPART

********************************************************************************
* Erase File                                                                   *
********************************************************************************
                PART 'cmd_erafile'
cmd_erase:      movea.l A0,A6
                bsr     get
                beq.s   cmd_erase2                  ;delete everything
                movea.l A6,A0
                bsr     getnam                      ;Get filenames
                beq     synerr
                moveq   #1,D0
                jsr     graf_mouse                  ;Turn the disk on
cmd_erase1:     bsr     do_mediach                  ;Trigger Media-Change
                move.l  A2,-(SP)
                move.w  #$41,-(SP)
                bsr     do_trap_1                   ;Fdelete ()
                addq.l  #6,SP
                tst.w   D0
                bpl.s   cmd_erase1
                jmp     (A4)
cmd_erase2:     bsr     kill_programm               ;Remove old program
                jmp     (A4)
                ENDPART

********************************************************************************
* FDC - Show all FDC tabs                                                      *
********************************************************************************
                PART 'cmd_fdc'
cmd_fdc:        moveq   #';',D0
                jsr     @chrout(A4)                 ;Remark out of the line
                moveq   #8,D2
cmd_fdc1:       move.w  fdc_tab(PC,D2.w),$FFFF8606.w ; Select FDC register
                bsr     read1772                    ;Read register
                move.w  D0,D1
                bsr     hexbout                     ;Spend hexbytes
                jsr     space                       ;And another space
                subq.w  #2,D2                       ;Already all registers?
                bpl.s   cmd_fdc1                    ;No!=>
                moveq   #0,D1
                move.b  $FFFF8609.w,D1
                swap    D1
                move.b  $FFFF860B.w,D1              ;Read DMA address
                lsl.w   #8,D1
                move.b  $FFFF860D.w,D1
                bsr     hexlout                     ;put out as a longword
                jsr     @c_eol(A4)                  ;Delete line residue
                jsr     @crout(A4)                  ;Spend
                jmp     (A4)
fdc_tab:        DC.W $90,$86,$84,$82,$80            ;FDC-Tabelle
                ENDPART

********************************************************************************
* READTRACK [Track[,[Side][,[Adr][,[Drive]]]]]                                *
********************************************************************************
                PART 'cmd_rtrack'
cmd_rtrack:     bsr     get
                moveq   #85,D6
                bsr     get_it                      ;spur
                beq.s   cmd_rt4
                bvc.s   cmd_rt1
                move.w  D1,dsk_track(A4)
                bsr     chkcom
                beq.s   cmd_rt4
cmd_rt1:        moveq   #1,D6
                bsr     get_it                      ;page
                beq.s   cmd_rt4
                bvc.s   cmd_rt2
                move.w  D1,dsk_side(A4)
                bsr     chkcom
                beq.s   cmd_rt4
cmd_rt2:        moveq   #-1,D6
                bsr     get_it                      ;Address (all allowed)
                beq.s   cmd_rt4
                bvc.s   cmd_rt3
                move.l  D1,dsk_adr2(A4)
                bsr     chkcom
                beq.s   cmd_rt4                     ;End of the input
cmd_rt3:        moveq   #1,D6
                bsr     get_it                      ;drive
                beq.s   cmd_rt4
                bvc.s   cmd_rt4
                move.w  D1,dsk_drive(A4)
cmd_rt4:        move.l  first_free(A4),D0
                cmp.l   dsk_adr2(A4),D0             ;At the beginning of the free memory
                bne.s   cmd_rt5                     ;Yes! Then not delete!
                bsr     kill_programm               ;I need the memory!
cmd_rt5:        st      $043E.w                     ;Floppy-VBL locks
                moveq   #1,D0
                jsr     graf_mouse                  ;Turn the disk on
                moveq   #2,D0                       ;Drive A
                tst.w   dsk_drive(A4)
                beq.s   cmd_rt6
                moveq   #4,D0                       ;Drive B
cmd_rt6:        or.w    dsk_side(A4),D0             ;Page
                eori.w  #7,D0                       ;Invert bits for hardware
                andi.w  #7,D0                       ;Only the 3 low bits are influenced
                move    SR,-(SP)                    ;Status Court
                ori     #$0700,SR                   ;Turn off interrupts
                move.b  #14,$FFFF8800.w             ;Select port A of the sound chip
                move.b  $FFFF8800.w,D1              ;Port of lesen
                andi.w  #$F8,D1                     ;Delete bits 0-2
                or.w    D0,D1                       ;set new bits
                move.b  D1,$FFFF8802.w              ;and write to Port A
                move    (SP)+,SR                    ;restoreStatus
                move.w  #$82,$FFFF8606.w            ;Track-Reg.select
                bsr.s   read1772                    ;and reading
                move.w  D0,D7                       ;Remember track
                move.w  dsk_track(A4),D0
                bsr.s   seek                        ;Trace
                bmi     seekerr                     ;timeout
                move.l  dsk_adr2(A4),D0             ;Read track here
                move.l  D0,default_adr(A4)
                move.b  D0,$FFFF860D.w              ;Only the low-byte
                lsr.w   #8,D0
                move.b  D0,$FFFF860B.w              ;Then the Mid byte
                swap    D0
                move.b  D0,$FFFF8609.w              ;and last writing the high-byte
                move.w  #$90,$FFFF8606.w            ;dmaR/wToggeln
                move.w  #$0190,$FFFF8606.w
                move.w  #$90,$FFFF8606.w            ;Select DMA SECTORCOUNT
                moveq   #14,D0                      ;With 14 loading (corresponds to 7KB)
                bsr.s   wrt1772
                move.w  #$80,$FFFF8606.w            ;Command-Reg.select
                moveq   #$E0,D0
                bsr.s   wrt1772                     ;command =>ReadTrack
                bsr.s   fdcwait                     ;Wait until FDC done
                bmi     timeouterr                  ;timeout
                move.w  D7,D0
                bsr.s   seek                        ;Restorate old track
                bmi     seekerr                     ;timeout
                sf      $043E.w                     ;Release Floppy-VBL again
                jmp     (A4)

************************************************************************
* D0 according to DMA-Access                                           *
************************************************************************
wrt1772:        and.l   #$FF,D0
                move.w  D0,$FFFF8604.w              ;FDC-Reg. or DMA-SECTORCOUNT Write
                move    #0,CCR
                rts

************************************************************************
* FDC-Register Read according to D0                                    *
************************************************************************
read1772:       move    #0,CCR
                move.w  $FFFF8604.w,D0              ;FDC-Reg. or DMA-SECTORCOUNT read
                and.l   #$FF,D0
                rts

************************************************************************
* Track D0 Control                                                     *
************************************************************************
seek:           move.w  #$86,$FFFF8606.w            ;Data Reg.select
                bsr.s   wrt1772                     ;Tracknr.to write
                move.w  #$80,$FFFF8606.w            ;Command-Reg.select
                moveq   #$13,D0
                bsr.s   wrt1772                     ;Command => Seek

************************************************************************
* Wait for the FDC work (D0=Status, D0<0 => Timeout)                   *
************************************************************************
fdcwait:        move.w  #$01A0,D0                   ;Wait something until Busy set
litlwt:         dbra    D0,litlwt
                move.l  #$060000,D0                 ;D5 as a timeout counter
readmfp:        btst    #5,$FFFFFA01.w              ;Is the command finished?
                beq.s   read1772
                subq.l  #1,D0
                bne.s   readmfp
                move.w  #$D0,$FFFF8604.w            ;command =>ForceInterrupt
                move.w  #$0100,D0                   ;Delay loop
timeou1:        dbra    D0,timeou1
                moveq   #-1,D0                      ;Put timeout flatting
                rts
                ENDPART

********************************************************************************
* DISKREAD/WRITE - Read / write sectors                                        *
********************************************************************************
                PART 'cmd_dread/write'
cmd_dread:      moveq   #8,D7
                bra.s   cmd_dsk
cmd_dwrite:     move.l  A0,-(SP)
                lea     dwrite_text(PC),A0
                jsr     ask_user                    ;Security query
                movea.l (SP)+,A0
                moveq   #9,D7
cmd_dsk:        bsr.s   get_dsk_par                 ;Get parameters
                moveq   #1,D0
                jsr     graf_mouse                  ;Turn the disk on
                move.w  #1,-(SP)                    ;1 sector
                move.w  dsk_side(A4),-(SP)          ;page
                move.w  dsk_track(A4),-(SP)         ;track
                move.w  dsk_sektor(A4),-(SP)        ;Sector
                move.w  dsk_drive(A4),-(SP)         ;drive
                clr.l   -(SP)                       ;dummy
                move.l  dsk_adr(A4),-(SP)           ;Address
                move.w  D7,-(SP)                    ;floprd =8 /Flopwr =9
                trap    #14
                lea     20(SP),SP
                movea.l dsk_adr(A4),A0
                move.l  A0,default_adr(A4)          ;New DefaultAr
                moveq   #0,D2
                move.w  #255,D1
cmd_ds1:        add.w   (A0)+,D2                    ;Calculate checksum
                dbra    D1,cmd_ds1
                move.w  D2,checksum(A4)             ;and remember
                tst.w   D0
                bmi     toserr                      ;Error reading / writing
                jmp     (A4)

                SWITCH language
                CASE 0
dwrite_text:    DC.B 'Wollen Sie schreiben? (j/n) ',0
                CASE 1
dwrite_text:    DC.B 'Write the sector? (y/n) ',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* Parameter for DISKREAD/WRITE get                                           *
********************************************************************************
                PART 'get_dsk_par'
get_dsk_par:    bsr     get
                moveq   #85,D6
                bsr.s   get_it                      ;Get track (0 to 85)
                beq.s   enddsk                      ;End of the input
                bvc.s   nxtdsk1                     ;No parameter
                move.w  D1,dsk_track(A4)
                bsr     chkcom
                beq.s   enddsk                      ;End of the input
nxtdsk1:        moveq   #0,D6
                move.w  #$FF,D6
                bsr.s   get_it                      ;Get sector (0 to 255)
                beq.s   enddsk
                bvc.s   nxtdsk2
                move.w  D1,dsk_sektor(A4)
                bsr     chkcom
                beq.s   enddsk                      ;End of the input
nxtdsk2:        moveq   #1,D6
                bsr.s   get_it                      ;Get page (0 or 1)
                beq.s   enddsk
                bvc.s   nxtdsk3
                move.w  D1,dsk_side(A4)
                bsr     chkcom
                beq.s   enddsk                      ;End of the input
nxtdsk3:        moveq   #-1,D6
                bsr.s   get_it                      ;Address (all allowed)
                beq.s   enddsk
                bvc.s   nxtdsk4
                move.l  D1,dsk_adr(A4)
                bsr     chkcom
                beq.s   enddsk                      ;End of the input
nxtdsk4:        moveq   #1,D6
                bsr.s   get_it                      ;Drive (0 or 1)
                beq.s   enddsk
                bvc     synerr
                move.w  D1,dsk_drive(A4)
enddsk:         rts

get_it:         tst.b   D0
                beq.s   get_it2
                cmp.b   #',',D0
                beq.s   get_it1
                bsr     get_term                    ;Get parameters
                tst.l   D1
                bmi     illequa
                cmp.l   D6,D1
                bhi     illequa
                move    #2,CCR                      ;Input available (V-Flag set)
                rts
get_it1:        bsr     get
                move    #0,CCR                      ;No entry (all flags deleted)
get_it2:        rts                                 ;End of the input (Z-flag set)
                ENDPART

********************************************************************************
* MOUSEON / MOUSEOFF                                                           *
********************************************************************************
                PART 'cmd_mon'
cmd_mon:        bsr.s   cmd_mon1
                jmp     (A4)
cmd_mon1:       linea   #0 [ Init ]
                move.l  4(A0),D0
                bls.s   cmd_mon2                    ;Address valid?End, if not
                movea.l D0,A1
                clr.w   2(A1)                       ;CONTROL(1)
                clr.w   6(A1)                       ;CONTROL(3)
                movea.l 8(A0),A1
                clr.w   (A1)                        ;Intimate (0) Switch on immediately
                linea   #9 [ Showm ]
cmd_mon2:       rts
                ENDPART

********************************************************************************

                PART 'cmd_moff'
cmd_moff:       linea   #0 [ Init ]
                movea.l 4(A0),A1
                bls.s   cmd_moff1
                clr.w   2(A1)                       ;CONTROL(1)
                clr.w   6(A1)                       ;CONTROL(3)
                movea.l 8(A0),A1
                clr.w   (A1)                        ;Intin (0) Turn off immediately
                linea   #10 [ Hidem ]
cmd_moff1:      jmp     (A4)
                ENDPART

********************************************************************************
* 'SAVE' - Write File                                                          *
********************************************************************************
                PART 'cmd_save'
cmd_save:       bsr     get
                bne.s   cmd_save2                   ;Line end
cmd_save1:      tst.b   fname(A4)                   ;Filename at all?
                beq     illequa
                tst.l   basep(A4)                   ;PRG loaded with LEXEC?
                bne     illequa
                tst.b   D0
                beq.s   cmd_save3
                subq.l  #1,A0                       ;Pointer back
                bra.s   cmd_save3
cmd_save2:      cmp.b   #'&',D0
                beq     degas_write
                cmp.b   #'!',D0
                beq     install_write               ;Write install file
                cmp.b   #',',D0
                beq.s   cmd_save1
                bsr     getnam_cont                 ;Get filenames
                beq     synerr
cmd_save3:      pea     cmd_save9(PC)
                jsr     @print_line(A4)
                pea     fname(A4)
                jsr     @print_line(A4)
                bsr     get
                cmp.b   #',',D0
                bne.s   cmd_save4                   ;No parameters
                bsr     get_parameter
                bcc.s   cmd_save5                   ;Parameter
                bvc     synerr                      ;2nd parameters (should not be!)
cmd_save4:      movea.l merk_anf(A4),A2             ;Remember initial address
                movea.l merk_end(A4),A3             ;Mainted end address
                bra.s   cmd_save6
cmd_save5:      bvs     synerr                      ;2.Parameter is missing
cmd_save6:      move.l  A2,merk_anf(A4)
                move.l  A3,merk_end(A4)
                move.l  A3,D0
                sub.l   A2,D0                       ;Ende-Start = Long
                bls     illequa                     ;Start address larger end address
                pea     cmd_save10(PC)
                jsr     @print_line(A4)
                move.l  A2,D1
                bsr     hexout                      ;'FromAnfadr'
                pea     cmd_save11(PC)
                jsr     @print_line(A4)
                move.l  A3,D1
                bsr     hexout                      ;'the ETA'
                jsr     @space(A4)
                jsr     gleich_out
                sub.l   A2,D1                       ;Programger
                moveq   #10,D2
                bsr     numout                      ;decimal
                pea     cmd_save12(PC)              ;'Bytes.'
                jsr     @print_line(A4)
                lea     cmd_save13(PC),A0
                jsr     ask_user                    ;Security query
                moveq   #1,D0
                bsr     graf_mouse                  ;Turn the disk on
                suba.l  A2,A3                       ;Ende-Start = Long
                bsr     fcreate                     ;Open File
                bsr     fwrite                      ;Write File
                bsr     fclose
                jmp     (A4)                        ;Back in input loop

                SWITCH language
                CASE 0
cmd_save9:      DC.B 'Speichere ',0
cmd_save10:     DC.B ' von ',0
cmd_save11:     DC.B ' bis ',0
cmd_save12:     DC.B ' Bytes.',13,0
cmd_save13:     DC.B 'Wollen Sie speichern? (j/n) ',0

                CASE 1
cmd_save9:      DC.B 'Save ',0
cmd_save10:     DC.B ' from ',0
cmd_save11:     DC.B ' to ',0
cmd_save12:     DC.B ' Bytes.',13,0
cmd_save13:     DC.B 'Save this file? (y/n) ',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* DEGAS-Writing picture                                                        *
********************************************************************************
                PART 'degas_write'
degas_write:    bsr     getnam                      ;Bring away filenames."&" will be overlooked
                beq     synerr
                pea     cmd_save9(PC)
                jsr     @print_line(A4)
                pea     fname(A4)
                jsr     @print_line(A4)
                bsr     get
                tst.b   D0
                bne     syn_error                   ;No parameters
                lea     user_scr(A4),A0
                movea.l scr_adr(A0),A1              ;Address of the screen
                pea     cmd_save10(PC)
                jsr     @print_line(A4)
                move.l  A1,D1
                bsr     hexout                      ;'FromAnfadr'
                jsr     @crout(A4)
                lea     cmd_save13(PC),A0
                jsr     ask_user                    ;Security query
                moveq   #1,D0
                bsr     graf_mouse                  ;Turn the disk on
                bsr     fcreate                     ;Open File
                lea     2.w,A3
                lea     user_scr(A4),A0
                moveq   #0,D0
                move.b  scr_rez(A0),D0
                move.w  D0,-(SP)
                movea.l SP,A2
                bsr     fwrite                      ;Write the resolution
                addq.l  #2,SP
                lea     user_scr(A4),A0
                lea     scr_colors(A0),A2
                lea     32.w,A3
                bsr     fwrite                      ;Write the color palette
                movea.l A1,A2
                movea.w #32000,A3
                bsr     fwrite                      ;Write the picture
                bsr     fclose
                jmp     (A4)                        ;Back in input loop
                ENDPART

********************************************************************************
* Write install file                                                           *
********************************************************************************
                PART 'install_write'
install_write:  bsr     get
                moveq   #0,D7                       ;normalesFile
install_write1: cmp.b   #'H',D0
                bne.s   install_write2              ;'H' for
                moveq   #2,D7                       ;hiddenFile
                bsr     get
install_write2: cmp.b   #'R',D0                     ;'R' for
                bne.s   install_write3
                st      do_resident(A4)             ;autoResident
                bsr     get
                bne.s   install_write1              ;possibly still a 'h'
install_write3: bsr.s   install_name
                bsr     do_mediach                  ;Trigger Media-Change
                move.w  D7,-(SP)                    ;hidden?
                pea     fname(A4)
                move.w  #$3C,-(SP)
                bsr     do_trap_1                   ;fcreate()
                addq.l  #8,SP
                tst.l   D0
                bmi     ioerr                       ;Error opening
                move.w  D0,_fhdle(A4)               ;FileHandle brand
                lea     default_start(A4),A2        ;Initial address
                lea     default_end(A4),A3
                suba.l  A2,A3                       ;Long
                bsr     fwrite
                bsr     fclose
                sf      do_resident(A4)             ;Delete auto-resident
                jmp     (A4)
                ENDPART

********************************************************************************

                PART 'install_name'
install_name:   lea     fname(A4),A0
                movea.l basepage(A4),A2
                movea.l $2C(A2),A2                  ;Address of Environment-String Holen
install_name1:  lea     install_name10(PC),A1
                move.b  (A2)+,D0
                beq.s   install_name6               ;End des environment-strings, nix
                cmp.b   (A1)+,D0
                beq.s   install_name3               ;1st sign is the same!=>
install_name2:  tst.b   (A2)+                       ;String to zero byte
                bne.s   install_name2
                bra.s   install_name1               ;Compare next variable
install_name3:  move.b  (A2)+,D0
                beq.s   install_name1               ;End of the variable, nothing found
                move.b  (A1)+,D1
                beq.s   install_name4               ;found!
                cmp.b   D1,D0
                bne.s   install_name2               ;Unequal, next variable
                bra.s   install_name3               ;Continue to compare
install_name4:  move.b  D0,(A0)+
install_name5:  move.b  (A2)+,(A0)+                 ;Copy path to zero byte
                bne.s   install_name5
                subq.l  #1,A0
install_name6:  lea     install_name8(PC),A1
install_name7:  move.b  (A1)+,(A0)+
                bne.s   install_name7
                rts

install_name8:  DC.B 'BUGABOO.'
install_name9:  DC.B 'INF',0
install_name10: DC.B 'SIGMA=',0
                EVEN
                ENDPART

********************************************************************************
* Read installation 'bugaboo.inf', possibly recreate                           *
********************************************************************************
                PART 'install_read'
install_read:   movem.l D0-A6,-(SP)
                jsr     init_save                   ;Save for the screen
                lea     install_name9(PC),A0
                move.l  #'INF'<<8,(A0)
                bsr.s   install_name                ;Set 'bugaboo.inf' as a name
                clr.w   -(SP)
                pea     fname(A4)                   ;Initial address of the name
                move.w  #$3D,-(SP)
                trap    #1                          ;fopen()
                addq.l  #8,SP
                move.w  D0,D7                       ;everything OK?
                bmi.s   install_read1               ;unableToFopenFile
                lea     default_start(A4),A5        ;Initial address
                lea     default_end(A4),A6
                suba.l  A5,A6                       ;File length
                move.l  A5,-(SP)
                move.l  A6,-(SP)
                move.w  D7,-(SP)                    ;FileHandle on the stack
                move.w  #$3F,-(SP)
                trap    #1                          ;fread()
                lea     12(SP),SP
                move.l  D0,-(SP)
                move.w  D7,-(SP)
                move.w  #$3E,-(SP)
                trap    #1                          ;fclose()
                addq.l  #4,SP
                move.l  (SP)+,D0
                cmpa.l  D0,A6
                bne.s   install_read1               ;Length is not correct
                cmpi.l  #'∑-So',(A5)+
                bne.s   install_read1
                cmpi.w  #'ft',(A5)                  ;Is the identifier?
                bne.s   install_read1
                st      install_load(A4)
                movem.l (SP)+,D0-A6
                rts
install_read1:  lea     default_start(A4),A0
                move.l  #'∑-So',(A0)+               ;Identification of the file
                move.w  #'ft',(A0)
                move.b  #'?',exquantor(A4)          ;suchjoker
                move.b  #'*',alquantor(A4)          ;"
                move.w  #$10,disbase(A4)            ;Number base of the disassembler
                move.w  #16,def_lines(A4)           ;defaultzeilenzahl
                move.w  #16,def_size(A4)            ;Width at dump
                move.w  #$0555,col0(A4)             ;Define debugger colors
                clr.w   col1(A4)
                move.w  #20000,scroll_d(A4)         ;scrollverzögerung
                lea     convert_tab(A4),A0          ;Create conversion table
                moveq   #31,D0
install_read2:  move.b  #'ꣻ',(A0)+
                dbra    D0,install_read2
                moveq   #95,D0
                moveq   #32,D1
install_read3:  move.b  D1,(A0)+
                addq.w  #1,D1
                dbra    D0,install_read3
                moveq   #$7F,D0
install_read4:  move.b  #'ꣻ',(A0)+
                dbra    D0,install_read4
                movem.l (SP)+,D0-A6
                rts
                ENDPART

********************************************************************************
* 'LOAD' - Read program from floppy program                                    *
********************************************************************************
                PART 'cmd_load'
cmd_load:       bsr     get
                beq.s   cmd_load2
                cmp.b   #',',D0
                beq.s   cmd_load1
                bsr     getnam_cont                 ;Get filenames
                beq     synerr
                bsr     get
                cmp.b   #',',D0
                bne.s   cmd_load2                   ;Comma behind the filename is missing!
cmd_load1:      bsr     get_parameter               ;Initial address for load
                bvc     synerr                      ;No 2nd parameter allowed!
                bcc.s   cmd_load3                   ;1st parameter is missing
cmd_load2:      bsr     kill_programm               ;Delete old program first
                movea.l first_free(A4),A2           ;Take first free address in the RAM
cmd_load3:      cmpa.l  #$400000,A2
                bhs.s   cmd_load4                   ;Address too big!
                movea.l A2,A6
                pea     cmd_load5(PC)
                jsr     @print_line(A4)
                move.l  A2,D1
                bsr     hexout                      ;Output the initial address
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                moveq   #1,D0
                bsr     graf_mouse                  ;Turn the disk on
                move.l  A2,-(SP)                    ;and also remember
                bsr     readimg                     ;Import file
                pea     cmd_load6(PC)
                jsr     @print_line(A4)
                move.l  D6,D1                       ;Number of bytes read
                bsr     dezout                      ;Spend number
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                move.l  (SP)+,D1                    ;Start address
                move.l  D1,merk_anf(A4)
                move.l  D1,default_adr(A4)
                add.l   D6,D1                       ;Plus number of charged bytes
                move.l  D1,merk_end(A4)
                pea     cmd_load7(PC)
                jsr     @print_line(A4)
                subq.l  #1,D1
                bsr     hexout                      ;Output end address
                jsr     @c_eol(A4)
                jsr     @crout(A4)
cmd_load4:      jmp     (A4)                        ;That's it

                SWITCH language
                CASE 0
cmd_load5:      DC.B 'Startadresse    :',0
cmd_load6:      DC.B 'Länge           :',0
cmd_load7:      DC.B 'Endadresse      :',0
                CASE 1
cmd_load5:      DC.B 'Start address   :',0
cmd_load6:      DC.B 'Length          :',0
cmd_load7:      DC.B 'End address     :',0
                ENDS

                ENDPART

********************************************************************************
* 'LEXEC' - Load File for Execute                                              *
********************************************************************************
                PART 'cmd_lexec'
cmd_lexec:      tst.b   le_allowed(A4)
                beq     noallow
                moveq   #1,D0
                bsr     graf_mouse                  ;Turn the disk on
                moveq   #79,D1
                lea     spaced(A4),A1
cmd_lexec1:     clr.b   (A1)+                       ;Clear cmdline
                dbra    D1,cmd_lexec1
                bsr     get
                beq.s   cmd_lexec5
                cmp.b   #',',D0                     ;Only a new cmdline?
                beq.s   cmd_lexec2                  ;Yes!=>
                bsr     getnam_cont                 ;Get filenames
                bsr     get
                cmp.b   #',',D0
                bne.s   cmd_lexec5
cmd_lexec2:     bsr     get
                cmp.b   #$22,D0
                bne     synerr
                lea     spaced+1(A4),A1
                moveq   #0,D1                       ;Length of cmdline = 0
cmd_lexec3:     move.b  (A0)+,D0
                beq.s   cmd_lexec4
                cmp.b   #$22,D0
                beq.s   cmd_lexec4
                addq.w  #1,D1
                move.b  D0,(A1)+
                bra.s   cmd_lexec3
cmd_lexec4:     move.b  #13,(A1)+                   ;Pendicate
                move.b  D1,spaced(A4)
cmd_lexec5:     bsr     kill_programm               ;Remove the old program as needed
                bsr     load_symbols                ;Import symbol table (if available)
                bsr     initreg                     ;Initialize Register
                clr.l   merk_end(A4)                ;Delete storage faults
                clr.l   merk_anf(A4)
                bsr     do_mediach                  ;Trigger Media-Change
                clr.l   -(SP)                       ;Pass original Environment
                pea     spaced(A4)                  ;commandline
                pea     fname(A4)                   ;Pointer on name
                move.l  #$4B0003,-(SP)              ;Import program
                bsr     do_trap_1                   ;reading in
                lea     16(SP),SP
                tst.l   D0
                bmi     ioerr                       ;Negative => Laughter, Lach (Error)
                btst    #0,D0                       ;Basepage address of the loaded program
                bne     ioerr                       ;That was probably nothing ...
                movea.l D0,A1                       ;Pointer on the base page
                move.l  A1,basep(A4)                ;Start address of the program

cmd_lexec6:     movea.l 8(A1),A3
                move.l  A3,_pc(A4)                  ;Pc for trace implementing text segm.
                move.l  A3,default_adr(A4)          ;Disassemble-CoR on text segment

                movem.l D0-A6,-(SP)
                ori     #$0700,SR                   ;Irqs lock
                move.l  SP,load3(A4)                ;Stackpnt save
                move.w  (A3),load1(A4)              ;Save the first command
                move.w  #$4AFC,(A3)                 ;Illegally insert
                move.l  $10,load2(A4)
                lea     cmd_lexec7(PC),A0
                move.l  A0,$10.w                    ;illegalInstructionPatchen
                bsr     clr_cache

                lea     lstackend(A4),SP
                move.l  A1,-(SP)                    ;basepageadr
                clr.l   -(SP)
                move.l  #$4B0004,-(SP)              ;Start program
                trap    #1                          ;More parameters are not needed
                movea.l load4(A4),A0
                jmp     (A0)

cmd_lexec7:     move    USP,A0
                movea.l 4(A0),A3                    ;Basepage
                andi    #$FBFF,SR                   ;Release IRQS
                lea     varbase,A4
                movea.l load3(A4),SP                ;Stackpointer back
                move.l  load2(A4),$10.w             ;Illegal instruction on normal
                movea.l 8(A3),A0                    ;Pointer to Text Segment
                move.w  load1(A4),(A0)              ;First command back
                movea.l $24(A3),A3                  ;Pointer to debugger (hopefully)
                move.l  $7C(A3),load5(A4)           ;Stackpnt notice

                bsr     clr_cache
                movem.l (SP),D0-A6

                bsr     reloc_symbols               ;Reloup symbol table
                clr.w   _sr(A4)                     ;SR = 0 (as in the ROM!)
                movea.l basep(A4),A0                ;BasePageAdr of the program
                move.l  24(A0),regs+12*4(A4)        ;Set A4 on BSS segment start
                move.l  16(A0),regs+13*4(A4)        ;Set A5 on Data Segment Start
                movea.l 4(A0),A1                    ;Get P_HITPA
                move.l  A0,-(A1)                    ;basepageadr
                lea     login(PC),A2                ;returnadresse
                move.l  A2,-(A1)                    ;to the stack
                move.l  A1,_usp(A4)                 ;Use usp
                move.l  A1,rega7(A4)                ;Set active stack pointer
                moveq   #8,D0
cmd_lexec8:     clr.l   -(A1)                       ;9 times 0l
                dbra    D0,cmd_lexec8
                move.l  8(A0),-(A1)                 ;Start address
                move.l  8(A0),merk_pc_call(A4)
                clr.w   -(A1)                       ;sr =0
                movea.l #debug_sstack,A2
                adda.l  A4,A2
                move.l  A2,_ssp(A4)                 ;sspInDieRegisterliste
                move.l  A2,-(A1)                    ;SSP also on the stack
                move.l  A1,regs+14*4(A4)            ;A6 on the user stack

                bsr     prg_info                    ;Make a message

                movem.l (SP)+,D0-A6
                jmp     (A4)                        ;That's it
                ENDPART

********************************************************************************
* Automatic "loading" from the RAM (handover by the assembler)                 *
********************************************************************************
                PART 'autoload'
autoload:       moveq   #-1,D0
                move.l  D0,-(SP)
                move.w  #$48,-(SP)
                bsr     do_trap_1                   ;Determine maximum space
                addq.l  #6,SP
                movea.l prg_base(A4),A6             ;Headers address
                moveq   #64,D1                      ;Safety for the stack
                add.l   2(A6),D1                    ;+ TEXT
                add.l   6(A6),D1                    ;+ DATA
                add.l   10(A6),D1                   ;+ BSS
                sub.l   D1,D0
                bmi     autoload10                  ;Memory is not enough!

                clr.w   spalte(A4)                  ;So that the ADR does not stop
                bsr     kill_programm               ;normally unnecessary
                bsr     initreg                     ;Initialize Register
                clr.l   -(SP)                       ;Original Environment
                move.l  cmd_line_adr(A4),-(SP)      ;Commandline
                clr.l   -(SP)                       ;Pointer on name
                move.l  #$4B0005,-(SP)              ;Create basepage
                bsr     do_trap_1                   ;pexec()
                lea     16(SP),SP
                tst.l   D0
                bmi     ioerr                       ;Negative => Laughter, Lach (Error)
                btst    #0,D0                       ;Basepage address of the loaded program
                bne     ioerr                       ;That was probably nothing ...
                movea.l D0,A5
                move.l  A5,basep(A4)

                movea.l prg_base(A4),A6             ;Headers address
                clr.l   prg_base(A4)                ;Prevent renewed call

                move.l  2(A6),D0                    ;Long des text segments
                add.l   6(A6),D0                    ;+ Long des data segments
                lea     28(A6),A0                   ;From here lies the code
                movea.l A0,A3
                lea     256(A5),A1                  ;Here the code should go
                add.l   A1,D0
                movea.l A1,A2                       ;Initial ID of the text segment
autoload1:      move.l  (A0)+,(A1)+                 ;Copy the program
                cmpa.l  D0,A1
                blo.s   autoload1

                lea     8(A5),A0
                move.l  A2,(A0)+                    ;Initial ID of the text segment
                move.l  2(A6),D0
                move.l  D0,(A0)+                    ;Long des text segments
                adda.l  D0,A2
                move.l  A2,(A0)+                    ;Initial Address of the Data Segment
                move.l  6(A6),D0
                move.l  D0,(A0)+                    ;Long des data segments
                adda.l  D0,A2
                move.l  A2,(A0)+                    ;Initial ID of the BSS segment
                move.l  10(A6),(A0)+                ;Long des bss segments

                adda.l  2(A6),A3                    ;+ Text length
                adda.l  6(A6),A3                    ;+ Data-long
                move.l  A3,D6                       ;Evtl.Anfangs address of the symbol table
                tst.w   26(A6)                      ;at all a reloc info?
                bne.s   autoload5                   ;End, if so is
                adda.l  14(A6),A3                   ;+ Length of the symbol table
                tst.l   (A3)                        ;Reloc info available?
                beq.s   autoload5                   ;Finished, if not
                movea.l 8(A5),A2                    ;Initial ID of the text segment
                move.l  A2,D0
                adda.l  (A3)+,A2                    ;First to be relocated address
                moveq   #0,D1
autoload2:      add.l   D0,(A2)
autoload3:      move.b  (A3)+,D1                    ;Relocate the program
                beq.s   autoload5
                cmp.w   #2,D1
                blo.s   autoload4
                adda.w  D1,A2
                bra.s   autoload2
autoload4:      lea     254(A2),A2
                bra.s   autoload3

autoload5:      movea.l D6,A1                       ;The top byte must, since an address
                tst.b   (A1)                        ;is actually a zero byte!
                bne.s   autoload6                   ;Otherwise it's a mistake
                move.l  14(A6),D7
                move.l  D7,sym_size(A4)             ;Remember size of the symbol table
                beq.s   autoload6                   ;No, end
                st      auto_sym(A4)                ;Symbol table through the assembler
                move.l  D6,sym_adr(A4)              ;Address the symbol table Remember
                add.l   D7,D6
                move.l  D6,sym_end(A4)              ;Self address + 1

autoload6:      movea.l basep(A4),A1                ;Here is the BasePage
                move.l  merk_svar(A4),D0
                beq.s   autoload9                   ;No variables passed
                movea.l D0,A3
                lea     simple_vars(A4),A0
                moveq   #9,D0                       ;10 variables are expected
autoload7:      clr.l   (A0)                        ;Delete variable first
                move.w  (A3)+,D1                    ;Is the label still?
                addq.w  #1,D1
                beq.s   autoload8
                move.l  (A3),D1
                add.l   8(A1),D1                    ;Text relative
                move.l  D1,(A0)                     ;Only copy
autoload8:      addq.l  #4,A3
                addq.l  #4,A0
                dbra    D0,autoload7
autoload9:      movea.l $18(A1),A5                  ;Initial address of the BSS area
                movea.l A5,A6
                adda.l  $1C(A1),A6                  ;End address of the BSS area
                bsr     _clear
                bra     cmd_lexec6

;Speicher reicht nicht!
autoload10:     clr.l   prg_base(A4)                ;Prevent renewed call
                clr.b   help_allow(A4)              ;Ctrl-Help forbid!
                moveq   #-39,D0
                bra     toserr                      ;"Memory Full!"
                ENDPART

********************************************************************************
* Command Line from A0 transfer and delete in the input buffer                 *
********************************************************************************
                PART 'do_cmdline'
do_cmdline:     ext.w   D0
                movea.l A0,A2                       ;Beginning of the Kommandline
                lea     _zeile(A4),A1
                move.l  A1,input_pnt(A4)            ;Put batch-pnt
                cmpi.b  #'@',(A0)+                  ;Chamber monkey for direct command
                beq.s   do_cmdline1
                move.l  #'LE "',(A1)+               ;Otherwise always le program
                subq.l  #1,A0
                bsr.s   do_cmdline1                 ;Transfer filenames
                move.b  #'"',-1(A1)                 ;"complete
                clr.b   (A1)                        ;and another null byte
                rts
do_cmdline1:    move.b  (A0)+,(A1)+                 ;Commandline transferred
                dbra    D0,do_cmdline1
                clr.b   (A2)                        ;Commandline now invalid
                rts
                ENDPART

********************************************************************************
* Execute command at startup                                                   *
********************************************************************************
                PART 'autodo'
autodo:         sf      autodo_flag(A4)
                lea     _zeile3(A4),A1
                clr.b   79(A1)                      ;Complete line with zero byte
                cmpi.b  #'$',(A1)
                bne.s   autodo1
                addq.l  #8,A1                       ;The address overlook
autodo1:        cmpi.b  #'@',(A1)+
                bne     main_loop5                  ;autoline?
                lea     _zeile(A4),A0
                movea.l A0,A2
                moveq   #79,D0
autodo2:        move.b  (A1)+,(A2)+                 ;max.80 characters in the input busher
                dbeq    D0,autodo2
                move.l  A0,-(SP)
                jsr     @print_line(A4)             ;Output content of the line
                jsr     @crout(A4)                  ;and the cursor in the next line
                bra     inp_loop1                   ;Evaluate line
                ENDPART

********************************************************************************
* Import symbol table                                                          *
********************************************************************************
                PART 'load_symbols'
load_symbols:   bsr     fopen
                moveq   #28,D1
                lea     spaced2(A4),A6
                bsr     fread                       ;Read file header
                move.l  D0,D1
                moveq   #-118,D0                    ;fatMayBeDefect
                moveq   #28,D2
                cmp.l   D2,D1
                bne     toserr                      ;Have 28 bytes have been read in?
                moveq   #-66,D0                     ;Default error message
                cmpi.w  #$601A,(A6)
                bne     ioerr                       ;No PRG file
                move.l  2(A6),D0
                or.l    6(A6),D0                    ;All segments positive?
                or.l    10(A6),D0
                bmi     ioerr
                move.l  22(A6),prg_flags(A4)
                move.l  14(A6),D7                   ;Symbol table available?
                beq     load_symbols9               ;No, end
                lsl.l   #2,D7                       ;Process four times as much memory
                move.l  D7,-(SP)
                move.w  #$48,-(SP)
                bsr     do_trap_1                   ;Reserve space for the symbol table
                addq.l  #6,SP
                move.l  D0,D6
                bls     toserr                      ;Error with memory assignment
                lsr.l   #2,D7
                move.l  D6,sym_adr(A4)              ;Address the symbol table Remember
                move.l  D6,sym_end(A4)
                add.l   D7,sym_end(A4)              ;End address of the symbol table + 1
                move.l  2(A6),D0                    ;Long des text segments
                add.l   6(A6),D0                    ;+ Long des data segments
                move.w  #1,-(SP)
                move.w  _fhdle(A4),-(SP)
                move.l  D0,-(SP)
                move.w  #$42,-(SP)
                bsr     do_trap_1                   ;Fseek(Offset,Fhandle,relative)
                lea     10(SP),SP
                tst.l   D0
                bmi     ioerr                       ;Error at Seek
                movea.l D6,A6                       ;Start address of the memory for Symab
                move.l  D7,D1                       ;Size of the symbol table
                bsr     fread                       ;Import symbol table
                move.l  D0,D2
                moveq   #-118,D0                    ;fatMayBeDefect
                cmp.l   D1,D2                       ;Everything read?
                bne     toserr
                bsr     fclose                      ;and close file again

                sf      gst_sym_flag(A4)
                movea.l A6,A5                       ;A6 = initial ID of the symbol table
                adda.l  D7,A5                       ;A5 = initial ID of the symbol names
                movea.l A5,A3                       ;Brother = A: I study broth
                movea.l A6,A2                       ;A6 = A2: write pointer to the symbol table

load_symbols1:  move.l  (A6)+,(A2)+
                move.l  (A6)+,(A2)+
                move.l  (A6)+,(A2)+                 ;Copy icon entry
                move.w  (A6)+,(A2)+
                movea.l A6,A0
                lea     -14(A6),A6
                moveq   #7,D0
load_symbols2:  move.b  (A6)+,(A5)+                 ;Max. 8 characters Copy
                dbeq    D0,load_symbols2
                beq.s   load_symbols5               ;Label <8 characters => Next
                cmpi.b  #$48,-5(A2)                 ;extendedGstFormat?
                bne.s   load_symbols4               ;No!=>
                st      gst_sym_flag(A4)            ;gstSymboltabelle
                movea.l A0,A6
                lea     14(A0),A0                   ;Pointer to the follow-up entry
                moveq   #13,D0
load_symbols3:  move.b  (A6)+,(A5)+                 ;Max. 14 characters Copy extension
                dbeq    D0,load_symbols3
                beq.s   load_symbols5
load_symbols4:  clr.b   (A5)+
load_symbols5:  movea.l A0,A6
                cmpa.l  sym_end(A4),A6
                blo.s   load_symbols1
                move.l  A2,sym_end(A4)              ;Set new end
load_symbols6:  move.b  (A3)+,(A2)+                 ;Start the symbol names
                cmpa.l  A5,A2
                blo.s   load_symbols6
                movea.l sym_adr(A4),A0
                movea.l sym_end(A4),A1
                move.l  A1,D7
                sub.l   A0,D7
                move.l  D7,sym_size(A4)             ;Calculate size of the symbol table
                movea.l A1,A2
load_symbols7:  move.l  A1,(A0)+                    ;Set address
                clr.l   (A0)
                lea     10(A0),A0                   ;Pointer to the next entry
load_symbols8:  tst.b   (A1)+                       ;Label over
                bne.s   load_symbols8
                cmpa.l  A2,A0                       ;End?
                blo.s   load_symbols7               ;No!=> Next
                suba.l  sym_adr(A4),A1
                move.l  A1,-(SP)                    ;Length of the symbol table
                move.l  sym_adr(A4),-(SP)           ;Initial address
                move.l  #$4A0000,-(SP)
                bsr     do_trap_1                   ;Mshrink()
                lea     12(SP),SP
load_symbols9:  rts
                ENDPART

********************************************************************************
* Relocate Symbol Table                                                        *
********************************************************************************
                PART 'reloc_symbols'
reloc_text1:    DC.B 'Symboltabelle muß ',0
reloc_text2:    DC.B 'segment-relativ',0
reloc_text3:    DC.B 'programm-relativ',0
reloc_text4:    DC.B ' sein.',13,0
reloc_text5:    DC.B 'fehlerhaft',0
                EVEN

reloc_symbols:  tst.l   sym_size(A4)                ;Remember size of the symbol table
                beq.s   load_symbols9               ;No symbol table
                movea.l basep(A4),A0                ;BasePageAdr of the program
                move.l  $0C(A0),D3                  ;Text -Nen.
                move.l  $14(A0),D4                  ;Data
                move.l  $1C(A0),D5                  ;BSS Len
                move.l  D3,D6
                add.l   D4,D6                       ;Text-only + data-only
                movea.l sym_adr(A4),A1              ;Initial ID of the symbol table
                movea.l sym_end(A4),A2              ;EnDAD + 1 of the symbol table
                moveq   #0,D2                       ;Do not change the label base
reloc_symbols1: move.l  10(A1),D1                   ;Register Equate or Constant
                move.b  8(A1),D0                    ;Symboly
                btst    #1,D0                       ;Text relative?
                beq.s   reloc_symbols2
                cmp.l   D3,D1                       ;label =>TextLen?
                bls.s   reloc_symbols2              ;No =>
                moveq   #3,D2                       ;Failure!
reloc_symbols2: btst    #2,D0                       ;Date-relative?
                beq.s   reloc_symbols4
                cmp.l   D4,D1                       ;label >DataLen
                bls.s   reloc_symbols3
                bset    #0,D2                       ;Impossible
                bra.s   reloc_symbols4
reloc_symbols3: cmp.l   D3,D1                       ;label <TextLen
                bhs.s   reloc_symbols4
                bset    #1,D2                       ;P-impossible
reloc_symbols4: btst    #0,D0                       ;bssRelativ?
                beq.s   reloc_symbols6
                cmp.l   D5,D1                       ;label >BssLen
                bls.s   reloc_symbols5
                bset    #0,D2                       ;Impossible
                bra.s   reloc_symbols6
reloc_symbols5: cmp.l   D6,D1                       ;Label <text-only + data-only
                bhs.s   reloc_symbols6
                bset    #1,D2                       ;P-impossible
reloc_symbols6: lea     14(A1),A1
                cmpa.l  A2,A1
                blo.s   reloc_symbols1              ;End of the symtab has not yet been reached

                tst.b   D2                          ;Symbolic table format not recognized?
                beq.s   reloc_symbols10             ;precisely!=>
                pea     reloc_text1(PC)
                jsr     @print_line(A4)
                lea     reloc_text5(PC),A1
                cmp.b   #3,D2
                beq.s   reloc_symbols9
                cmp.b   #1,D2                       ;Segment-relatively impossible?
                beq.s   reloc_symbols7              ;Yes!=>
                lea     reloc_text2(PC),A1
                moveq   #$18,D1                     ;Symbols also Data & BSS relative
                moveq   #$10,D2
                bra.s   reloc_symbols8
reloc_symbols7: lea     reloc_text3(PC),A1
                moveq   #8,D1                       ;Symbols always text relative
                moveq   #8,D2
reloc_symbols8: move.b  D1,reloc_symbols12+1
                move.b  D2,reloc_symbols13+1
reloc_symbols9: move.l  A1,-(SP)
                jsr     @print_line(A4)
                pea     reloc_text4(PC)
                jsr     @print_line(A4)

reloc_symbols10:movea.l basep(A4),A0                ;BasePageAdr of the program
                movea.l sym_adr(A4),A1              ;Initial ID of the symbol table
                movea.l sym_end(A4),A2              ;EnDAD + 1 of the symbol table
reloc_symbols11:move.l  10(A1),D1                   ;Register Equate or Constant
                move.b  8(A1),D0                    ;Symboly
reloc_symbols12:moveq   #$18,D2                     ;bssOffset
                btst    #0,D0                       ;bssRelativesLabel
                bne.s   reloc_symbols14
                moveq   #8,D2                       ;textOffset
                btst    #1,D0                       ;textRelativesLabel?
                bne.s   reloc_symbols14
reloc_symbols13:moveq   #$10,D2                     ;dataOffset
                btst    #2,D0                       ;dataRelativesLabel
                beq.s   reloc_symbols15
reloc_symbols14:add.l   0(A0,D2.w),D1               ;relocate
reloc_symbols15:move.l  D1,10(A1)                   ;Add value
                lea     14(A1),A1
                cmpa.l  A2,A1
                blo.s   reloc_symbols11             ;End not yet reached
                subq.l  #4,A2
                movea.l A2,A1                       ;Right limit of the Quicksort
                movea.l sym_adr(A4),A0              ;Initial ID of the symbol table
                lea     10(A0),A0                   ;left border of the quick
                ENDPART

********************************************************************************
* Quicksort(A0,A1)                                                             *
********************************************************************************
                PART 'quicks'
quicks:         movem.l A3-A4,-(SP)
                movea.l A0,A2
                movea.l A1,A3
                moveq   #0,D7
                move.l  (A0),D1
                move.l  (A1),D2
                move.l  D1,D0
                add.l   D2,D0
                roxr.l  #1,D0                       ;Mittelwert nehmen
quicks0:        cmp.l   D0,D1
                bhs.s   quicks1
                lea     14(A0),A0
                move.l  (A0),D1
                bra.s   quicks0

quicks1:        cmp.l   D0,D2
                bls.s   quicks2
                lea     -14(A1),A1
                move.l  (A1),D2
                bra.s   quicks1

quicks2:        cmpa.l  A1,A0
                bhi.s   quicks3
                move.l  D1,(A1)
                move.l  D2,(A0)
                lea     -10(A0),A0
                lea     -10(A1),A1
                movem.l (A0),D3-D4
                move.w  8(A0),D5
                move.l  (A1)+,(A0)+
                move.l  (A1)+,(A0)+
                move.w  (A1)+,(A0)+
                movem.l D3-D4,-10(A1)
                move.w  D5,-2(A1)
                lea     14(A0),A0
                lea     -14(A1),A1
                move.l  (A0),D1
                move.l  (A1),D2
quicks3:        cmpa.l  A1,A0
                bls.s   quicks0

                movea.l A0,A4
                cmpa.l  A1,A2
                bhs.s   quicks4
                movea.l A2,A0
                bsr.s   quicks
quicks4:        cmpa.l  A3,A4
                bhs.s   quicks5
                movea.l A4,A0
                movea.l A3,A1
                bsr.s   quicks
quicks5:        movem.l (SP)+,A3-A4
                rts
                ENDPART

********************************************************************************
* Remove old program from memory                                               *
********************************************************************************
                PART 'kill_programm'
kill_programm:  movem.l D0-A6,-(SP)
                tst.l   basep(A4)                   ;No program loaded
                bls.s   kill_programm5
                bsr     clr_cache
                move    SR,merk_quit_sr(A4)
                move.l  $B8.w,load6(A4)
                lea     __rte(PC),A0
                move.l  A0,$B8.w                    ;XBIOS on RTE (Get Time ...)
                ori     #$0700,SR                   ;Irqs lock
                move.l  SP,load3(A4)
                movea.l act_pd(A4),A0
                move.l  basep(A4),(A0)              ;Swedished PRG in ACT_PD
                movea.l merk_act_pd(A4),A0
                move.l  load5(A4),$7C(A0)           ;alterStackpnt
                lea     kill_programm4(PC),A0
                move.l  A0,load4(A4)                ;Prevent Gemdos patch (leap jump)
                clr.w   -(SP)
                trap    #1                          ;Terminate the Childs

kill_programm4: move    merk_quit_sr(A4),SR
                movea.l load3(A4),SP
                movea.l act_pd(A4),A0
                move.l  merk_act_pd(A4),(A0)        ;Reset active PRG
                move.l  load6(A4),$B8.w             ;old trap # 14 back
                movea.l basep(A4),A6                ;Basepage of the Child Process
                move.l  $2C(A6),-(SP)
                move.w  #$49,-(SP)
                trap    #1                          ;Environment release
                addq.l  #6,SP
                move.l  A6,-(SP)
                move.w  #$49,-(SP)
                trap    #1                          ;Release child
                addq.l  #6,SP
                clr.l   basep(A4)                   ;Log off the program
                bsr     cmd_mon1                    ;Mouse again
kill_programm5: move.l  sym_adr(A4),D0
                beq.s   kill_programm6              ;No symbol table available
                tst.b   auto_sym(A4)                ;Symbol table through the assembler?
                bne.s   kill_programm6
                move.l  D0,-(SP)
                move.w  #$49,-(SP)
                trap    #1                          ;Release symbol table
                addq.l  #6,SP
                clr.l   sym_adr(A4)
                clr.l   sym_size(A4)
kill_programm6: movem.l (SP)+,D0-A6
                rts
__rte:          rte
                ENDPART

********************************************************************************
* ';' - Change ascii dump                                                      *
********************************************************************************
                PART 'cmd_achng'
cmd_achng:      ori     #$0700,SR
                bsr     get
                cmp.b   #$22,D0
                bne     synerr                      ;Highly comma at the beginning
                movea.l default_adr(A4),A6
                moveq   #63,D5                      ;Read 64 bytes
cmd_achng1:     move.b  (A0)+,D0
                cmp.b   #'ꣻ',D0
                beq.s   cmd_achng2
                bsr     check_write
                bne.s   cmd_achng2
                move.b  D0,(A6)                     ;Byte
cmd_achng2:     addq.l  #1,A6
                dbra    D5,cmd_achng1
                move.l  A6,default_adr(A4)          ;New DefaultAr
                bsr     get
                cmp.b   #$22,D0
                bne     synerr                      ;Highly comma at the end
                jmp     (A4)
                ENDPART

********************************************************************************
* 'ASCII' - ASCII-Dump                                                         *
********************************************************************************
                PART 'cmd_asc'
cmd_asc:        bsr     get2adr                     ;Max.2 Get parameters
                move.w  D2,-(SP)                    ;Number of lines
                move.l  A3,-(SP)                    ;Resignance
                movea.l A2,A6
                move.l  A6,default_adr(A4)
cmd_asc1:       bsr.s   asc_out                     ;Output ascii dump
                jsr     @crout(A4)                  ;Crony
                bsr     check_keyb                  ;Button pressed?
                bmi.s   cmd_asc3                    ;and!
                tst.l   (SP)                        ;List n lines?
                bne.s   cmd_asc2                    ;No.List to address.
                subi.w  #1,4(SP)                    ;counter Decrement
                bpl.s   cmd_asc1                    ;not null, continue lists
                bra.s   cmd_asc3                    ;and continue as usual
cmd_asc2:       cmpa.l  (SP),A6
                blo.s   cmd_asc1
cmd_asc3:       move.l  A6,default_adr(A4)
                jmp     (A4)                        ;Stack is corrected there
                ENDPART

********************************************************************************

                PART 'asc_out'
asc_out:        lea     spaced2(A4),A0
                st      testwrd(A4)
                move.l  A6,D1
                jsr     @anf_adr(A4)
                lea     convert_tab(A4),A3
                move.b  #')',(A0)+                  ;ID_CHAR for ASCII dump
                move.b  #' ',(A0)+
                move.b  #$22,(A0)+                  ;Spend highly comma
                moveq   #63,D3                      ;Output 64 characters
asc_out1:       move.b  #'-',(A0)                   ;'-' <=> access not possible
                bsr     check_read
                bne.s   asc_out2
                moveq   #0,D0
                move.b  (A6),D0                     ;Copy ASCII characters
                move.b  0(A3,D0.w),(A0)             ;Convert sign
asc_out2:       addq.l  #1,A0
                addq.l  #1,A6
                dbra    D3,asc_out1
                move.b  #$22,(A0)+
                clr.b   (A0)
                sf      testwrd(A4)
                lea     spaced2(A4),A0
                move.w  zeile(A4),D0
                jmp     write_line                  ;Output line
                ENDPART

********************************************************************************
* 'DUMP' - Memory dump                                                         *
********************************************************************************
                PART 'cmd_dump'
cmd_dump:       moveq   #0,D3                       ;Default = Byte
                bsr     get
                beq.s   cmd_dump1                   ;Empty input
                bsr     get_extension               ;Get command text tension to D3
                tst.w   D0
                beq.s   cmd_dump1
                subq.l  #1,A0                       ;Pointer back
cmd_dump1:      bsr     get2adr                     ;Get parameters
                bsr.s   cmd_dump2
                jmp     (A4)
                ENDPART

********************************************************************************

                PART 'cmd_dump2'
cmd_dump2:      move.w  D2,-(SP)                    ;Number of lines
                move.l  A3,-(SP)                    ;Resignance
                move.l  A2,D0
                tst.w   D3
                beq.s   cmd_dump3                   ;At byte widely non-advanced
                btst    #0,D0                       ;Initial address must be straight
                beq.s   cmd_dump3
                addq.l  #1,D0
cmd_dump3:      movea.l D0,A6
                move.l  A6,default_adr(A4)
cmd_dump4:      bsr.s   cmd_dump7                   ;List line
                jsr     @crout(A4)                  ;Crony
                bsr     check_keyb                  ;Button pressed?
                bmi.s   cmd_dump6                   ;and!
                tst.l   (SP)                        ;List n lines?
                bne.s   cmd_dump5                   ;No.List to address.
                subi.w  #1,4(SP)                    ;counterDecrement
                bpl.s   cmd_dump4                   ;not null, continue lists
                bra.s   cmd_dump6                   ;and continue as usual
cmd_dump5:      cmpa.l  (SP),A6
                blo.s   cmd_dump4
cmd_dump6:      move.l  A6,default_adr(A4)
                addq.l  #6,SP                       ;Correct stack
                rts

cmd_dump7:      lea     spaced2(A4),A0
cmd_dump8:      st      testwrd(A4)
                lea     convert_tab(A4),A3
                tst.w   D3                          ;byteDump?
                beq.s   cmd_dump12                  ;Yes: output line
                bra     cmd_dump22                  ;Output line with .w / .l

cmd_dump9:      DC.B '0123456789ABCDEF'
cmd_dump10:     DC.B '0123456789abcdef'

cmd_dump11:     sf      testwrd(A4)
                rts
cmd_dump12:     move.l  A6,D1
                jsr     @anf_adr(A4)
                lea     cmd_dump10(PC),A5
                tst.w   small(A4)
                bne.s   cmd_dump13
                lea     cmd_dump9(PC),A5
cmd_dump13:     moveq   #6,D6                       ;7. column
                move.w  def_size(A4),D2             ;nBytesProZeile
                moveq   #',',D7
                moveq   #0,D0
                subq.w  #1,D2
                bmi.s   cmd_dump11                  ;No bytes per line
cmd_dump14:     move.b  D7,(A0)+                    ;Comma
                bsr     check_read                  ;Access granted?
                bne.s   cmd_dump20                  ;No!=>
                move.b  (A6)+,D0                    ;Get byte from the memory
                move.b  D0,D1
                lsr.b   #4,D0
                move.b  0(A5,D0.w),(A0)+            ;Insert hexbytes
                andi.w  #$0F,D1
                move.b  0(A5,D1.w),(A0)+
cmd_dump15:     addq.w  #3,D6                       ;6 columns more
                dbra    D2,cmd_dump14
cmd_dump16:     move.w  def_size(A4),D7
                neg.w   D6
                addi.w  #54,D6
                moveq   #' ',D1
cmd_dump17:     move.b  D1,(A0)+                    ;tab
                dbra    D6,cmd_dump17
                suba.w  D7,A6
                subq.w  #1,D7
                moveq   #0,D0
                move.b  #$22,(A0)+
cmd_dump18:     bsr     check_read
                bne.s   cmd_dump21
                move.b  (A6)+,D0                    ;Copy ASCII characters
                move.b  0(A3,D0.w),(A0)+            ;Convert sign
cmd_dump19:     dbra    D7,cmd_dump18
                move.b  #$22,(A0)+
                clr.b   (A0)                        ;Line closure
                sf      testwrd(A4)
                lea     spaced2(A4),A0
                move.w  zeile(A4),D0
                jmp     write_line
cmd_dump20:     addq.l  #1,A6
                move.b  #'-',(A0)+
                move.b  #'-',(A0)+
                bra.s   cmd_dump15
cmd_dump21:     addq.l  #1,A6
                move.b  #'-',(A0)+
                bra.s   cmd_dump19

cmd_dump22:     move.l  A6,D1
                addq.l  #1,D1
                andi.b  #$FE,D1                     ;Address now
                movea.l D1,A6
                cmp.w   #1,D3
                bne.s   cmd_dump27                  ;So is long

                jsr     @anf_adr(A4)
                move.b  #'.',(A0)+
                move.b  #'w',(A0)+
                move.b  #' ',(A0)+
                moveq   #8,D6                       ;9.Sole
                move.w  def_size(A4),D4
                addq.w  #1,D4
                lsr.w   #1,D4
                subq.w  #1,D4
                bpl.s   cmd_dump24
                bra     cmd_dump11
cmd_dump23:     move.b  #',',(A0)+                  ;',' output
cmd_dump24:     bsr     check_read
                bne.s   cmd_dump26                  ;Address not readable
                move.w  (A6)+,D1
                bsr     hexwout                     ;Output Word in Hex
cmd_dump25:     addq.w  #5,D6
                dbra    D4,cmd_dump23
                move.w  def_size(A4),D4
                andi.w  #1,D4
                neg.w   D4
                beq     cmd_dump16
                addq.w  #2,D4
                suba.w  D4,A6
                bra     cmd_dump16                  ;ASCII edition
cmd_dump26:     addq.l  #2,A6
                move.b  #'-',(A0)+
                move.b  #'-',(A0)+
                move.b  #'-',(A0)+
                move.b  #'-',(A0)+
                bra.s   cmd_dump25

cmd_dump27:     jsr     @anf_adr(A4)
                move.b  #'.',(A0)+
                move.b  #'l',(A0)+
                move.b  #' ',(A0)+
                moveq   #8,D6                       ;9.Sole
                move.w  def_size(A4),D4
                addq.w  #3,D4
                lsr.w   #2,D4
                subq.w  #1,D4
                bpl.s   cmd_dump29
                bra     cmd_dump11
cmd_dump28:     move.b  #',',(A0)+
cmd_dump29:     bsr     check_read
                bne.s   cmd_dump32
                move.w  (A6)+,D1
                bsr     hexwout                     ;Output Word in Hex
cmd_dump30:     bsr     check_read
                bne.s   cmd_dump33
                move.w  (A6)+,D1
                bsr     hexwout                     ;Output Word in Hex
cmd_dump31:     addi.w  #9,D6
                dbra    D4,cmd_dump28
                move.w  def_size(A4),D4
                andi.w  #3,D4
                neg.w   D4
                beq     cmd_dump16
                addq.w  #4,D4
                suba.w  D4,A6
                bra     cmd_dump16                  ;ASCII edition
cmd_dump32:     addq.l  #2,A6
                move.b  #'-',(A0)+
                move.b  #'-',(A0)+
                move.b  #'-',(A0)+
                move.b  #'-',(A0)+
                bra.s   cmd_dump30
cmd_dump33:     addq.l  #2,A6
                move.b  #'-',(A0)+
                move.b  #'-',(A0)+
                move.b  #'-',(A0)+
                move.b  #'-',(A0)+
                bra.s   cmd_dump31
                ENDPART

********************************************************************************
* 'DISASSEMBLE' / 'LIST' - Disassembling a memory area                         *
********************************************************************************
                PART 'cmd_list/f/disass'
cmd_listf:      move.b  #'L',hexbase                ;Label instead of hex
cmd_list:       st      list_flg(A4)                ;Symbolic
                bra.s   cmd_disass1
cmd_disass:     sf      list_flg(A4)                ;Well, not symbolic
cmd_disass1:    bsr     get2adr                     ;2 Get parameters (including the number of points)
                bsr.s   cmd_disass2
                jmp     (A4)                        ;Stack is corrected there
                ENDPART

********************************************************************************

                PART 'cmd_disass2'
cmd_disass2:    move.w  D2,-(SP)                    ;Number of lines
                move.l  A3,-(SP)                    ;Resignance
                movea.l A2,A6
                move.l  A6,default_adr(A4)
cmd_disass3:    bsr     do_disass                   ;List line
                bne.s   cmd_disass4                 ;Illegal RAM area => No issue
                jsr     @crout(A4)                  ;Crony
cmd_disass4:    bsr     check_keyb                  ;Button pressed?
                bmi.s   cmd_disass6                 ;yes!
                tst.l   (SP)                        ;List n lines?
                bne.s   cmd_disass5                 ;No.List to address.
                subi.w  #1,4(SP)                    ;counterDecrement
                bpl.s   cmd_disass3                 ;not null, continue lists
                bra.s   cmd_disass6                 ;and continue as usual
cmd_disass5:    cmpa.l  (SP),A6
                blo.s   cmd_disass3
cmd_disass6:    move.l  A6,default_adr(A4)
                sf      list_flg(A4)
                addq.l  #6,SP                       ;Correct stack
                rts
                ENDPART

********************************************************************************
* '?' - Expression evaluation, number systems                                  *
********************************************************************************
                PART 'cmd_calc'
cmd_calc:       bsr     get                         ;Get the 1st of the expression
                bsr     get_term                    ;Get a whole expression
                move.w  D0,D7
                movea.l A0,A6
                bsr     hexout
                jsr     @space(A4)                  ;space
                btst    #31,D1                      ;Number negative?
                beq.s   cmd_calc1                   ;otherwise not spend
                moveq   #'(',D0
                jsr     @chrout(A4)
                moveq   #'-',D0
                jsr     @chrout(A4)
                neg.l   D1
                bsr     hexout                      ;Output negative hex number
                neg.l   D1
                moveq   #')',D0
                jsr     @chrout(A4)
                jsr     @space(A4)
cmd_calc1:      bsr     dezout
                jsr     @space(A4)                  ;space
                moveq   #2,D2
                bsr     numout                      ;Binary
                jsr     @space(A4)                  ;space
                moveq   #$22,D0
                jsr     @chrout(A4)                 ;Highly
                moveq   #3,D6                       ;4 ASCII sign
cmd_calc2:      rol.l   #8,D1                       ;start with the top byte
                move.b  D1,D0
                bsr     charcout                    ;issue as ASCII
                dbra    D6,cmd_calc2
                moveq   #$22,D0
                jsr     @chrout(A4)                 ;Highly
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                movea.l A6,A0
                cmp.b   #',',D7                     ;Another deadline?
                beq.s   cmd_calc
                jmp     (A4)                        ;that's it
                ENDPART

********************************************************************************
* 'CLS' - 2. Clear screen page                                                 *
********************************************************************************
                PART 'cmd_cls'
cmd_cls:        move.w  #27,-(SP)
                move.l  #$030002,-(SP)
                trap    #13
                addq.l  #6,SP
                move.w  #'E',-(SP)
                move.l  #$030002,-(SP)
                trap    #13
                addq.l  #6,SP
                jmp     (A4)                        ;To delete (XBIOS & Gemdos call!)
                ENDPART

********************************************************************************
* CONTINUE for Find                                                            *
********************************************************************************
                PART 'cmd_cont'
cmd_cont:       lea     data_buff(A4),A1
                movea.l A1,A5
                movea.l find_cont1(A4),A2           ;Nude address reissue
                movea.l find_cont2(A4),A3           ;Replace the end address
                move.w  find_cont3(A4),D3           ;Length of the search string
                move.b  find_cont0(A4),D0
                subq.b  #1,D0
                beq     cmd_findasc5                ;ascfind
                bpl.s   cmd_cont1

                move.b  find_cont0(A4),D5
                bra.s   cmd_find5                   ;Hunt & Find

cmd_cont1:      pea     cont_txt(PC)
                jsr     @print_line(A4)
                jsr     @crout(A4)
                jmp     (A4)

                SWITCH language
                CASE 0
cont_txt:       DC.B '?Nicht möglich',0
                CASE 1
cont_txt:       DC.B '?Not possible',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* 'FIND' - Find byte sequence                                                  *
********************************************************************************
                PART 'cmd_find'
cmd_find:       bsr     get
                cmp.b   #',',D0
                beq.s   cmd_find2
                subq.l  #1,A0                       ;Pointer back
                bsr     get_parameter
                bra.s   cmd_find4
cmd_find2:      move.l  basep(A4),D1                ;Program loaded with LEXEC
                beq.s   cmd_find3                   ;No basepage, not
                movea.l D1,A6
                movea.l 8(A6),A2                    ;Text-segment address => Startadr
                movea.l 24(A6),A3                   ;Address of BSS-segments => Endadr
                bra.s   cmd_find4
cmd_find3:      move.l  merk_anf(A4),D1             ;Prg loaded with load?
                beq     illequa                     ;No search area!
                movea.l D1,A2                       ;Initial
                movea.l merk_end(A4),A3             ;Selfed
cmd_find4:      bsr     get_such_para
cmd_find5:      move.b  (A1),D4                     ;1st of the search text
                cmp.b   (A2)+,D4
                beq.s   cmd_find8
cmd_find6:      cmpa.l  A3,A2
                blo.s   cmd_find5
                move.b  #2,find_cont0(A4)
cmd_find7:      move.l  A2,default_adr(A4)
                jsr     @crout(A4)
                jmp     (A4)                        ;done
cmd_find8:      bsr     check_keyb                  ;Button pressed?
                bmi.s   cmd_find13                  ;Exit
                move.l  A2,-(SP)                    ;1st sign was the same
                clr.w   D0                          ;Pointer in the search text
                subq.l  #1,A2                       ;Compares the same again
cmd_find9:      move.b  0(A1,D0.w),D4
                cmp.b   (A2)+,D4
                bne.s   cmd_find12                  ;notfound
                addq.w  #1,D0
                cmp.w   D0,D3                       ;End of the search text?
                bhs.s   cmd_find9
                move.l  (SP),D1                     ;Search address is on the stack
                subq.l  #1,D1
                moveq   #' ',D0
                tst.w   spalte(A4)
                bne.s   cmd_find11
                moveq   #';',D0                     ;For the beginning of the line
cmd_find11:     jsr     @chrout(A4)
                bsr     hexlout                     ;Spend address
                jsr     @space(A4)
cmd_find12:     movea.l (SP)+,A2
                bra.s   cmd_find6
cmd_find13:     move.b  D5,find_cont0(A4)           ;Type of search (-1 = Hunt, 0 = Find, 1 = AscFind)
                move.l  A2,find_cont1(A4)           ;Current address
                move.l  A3,find_cont2(A4)           ;Resignance
                move.w  D3,find_cont3(A4)           ;Length of the search string
                bra.s   cmd_find7
                ENDPART

********************************************************************************
* 'HUNT' - Find byte sequence on even addresses                                *
********************************************************************************
                PART 'cmd_hunt'
cmd_hunt:       bsr     get
                cmp.b   #',',D0
                beq.s   cmd_hunt2
                subq.l  #1,A0                       ;Pointer back
                bsr     get_parameter
                bra.s   cmd_hunt4
cmd_hunt2:      move.l  basep(A4),D1                ;Program loaded with LEXEC
                beq.s   cmd_hunt3                   ;No basepage, not
                movea.l D1,A6
                movea.l 8(A6),A2                    ;Text-segment address => Startadr
                movea.l 24(A6),A3                   ;Address of BSS-segments => Endadr
                bra.s   cmd_hunt4
cmd_hunt3:      move.l  merk_anf(A4),D1             ;Prg loaded with load?
                beq     illequa                     ;No search area!
                movea.l D1,A2                       ;Initial
                movea.l merk_end(A4),A3             ;Selfed
cmd_hunt4:      bsr     get_such_para
                move.l  A2,D0
                addq.l  #1,D0                       ;Even on the initial code
                and.w   #-2,D0
                movea.l D0,A2
cmd_hunt5:      move.b  (A1),D0                     ;1st of the search text
                cmp.b   (A2),D0
                beq.s   cmd_hunt8
cmd_hunt6:      addq.l  #2,A2
                cmpa.l  A3,A2
                blo.s   cmd_hunt5
                move.b  #2,find_cont0(A4)
                move.l  A2,default_adr(A4)
                jsr     @crout(A4)
                jmp     (A4)                        ;done

cmd_hunt8:      bsr     check_keyb                  ;Button pressed?
                bmi.s   cmd_find13                  ;Exit
                move.l  A2,-(SP)                    ;1st sign was the same
                clr.w   D0                          ;Pointer in the search text
cmd_hunt9:      move.b  0(A1,D0.w),D4
                cmp.b   (A2),D4
                bne.s   cmd_hunt12                  ;notfound
                addq.l  #1,A2
                addq.w  #1,D0
                cmp.w   D0,D3                       ;End of the search text?
                bhs.s   cmd_hunt9
                move.l  (SP),D1                     ;Search address is on the stack
                moveq   #' ',D0
                tst.w   spalte(A4)
                bne.s   cmd_hunt11
                moveq   #';',D0                     ;For the beginning of the line
cmd_hunt11:     jsr     @chrout(A4)
                bsr     hexlout                     ;Spend address
                jsr     @space(A4)
cmd_hunt12:     movea.l (SP)+,A2                    ;After a closer look nothing nothing
                bra.s   cmd_hunt6
                ENDPART

********************************************************************************
* ASCFIND - Part a Mnemonic search (ASCII search!)                             *
********************************************************************************
                PART 'cmd_findasc'
cmd_findasc:    bsr     get
                cmp.b   #',',D0
                beq.s   cmd_findasc1
                subq.l  #1,A0                       ;Pointer back
                bsr     get_parameter
                bra.s   cmd_findasc3
cmd_findasc1:   move.l  basep(A4),D1                ;Program loaded with LEXEC
                beq.s   cmd_findasc2                ;No basepage, not
                movea.l D1,A6
                movea.l 8(A6),A2                    ;Text-segment address => Startadr
                movea.l 24(A6),A3                   ;Address of BSS-segments => Endadr
                bra.s   cmd_findasc3
cmd_findasc2:   move.l  merk_anf(A4),D1             ;Prg loaded with load?
                beq     illequa                     ;No search area!
                movea.l D1,A2                       ;Initial
                movea.l merk_end(A4),A3             ;Selfed
cmd_findasc3:   cmp.b   #',',D0
                bne     synerr                      ;There is a parameter missing!?!
                lea     data_buff(A4),A1
                movea.l A1,A5                       ;Pointer to the PatternString
cmd_findasc4:   move.b  (A0)+,(A1)+                 ;Patternstring right
                bne.s   cmd_findasc4
cmd_findasc5:   movea.l A3,A1                       ;Since A3 is used by Match ()
                move.b  exquantor(A4),D6            ;'?'-Joker
                move.b  alquantor(A4),D7            ;'*'-Joker
cmd_findasc6:   movea.l A2,A6
                movem.l D0-A6,-(SP)
                bsr     disass                      ;Line from A6 disassemble
                movem.l (SP)+,D0-A6
                lea     spaced(A4),A6               ;Here is the disassembled code
                bsr.s   match
                tst.w   D0
                beq.s   cmd_findasc7                ;Not included
                movem.l D0-A6,-(SP)
                movea.l A2,A6
                bsr     do_disass                   ;List line lists
                jsr     @crout(A4)
                movem.l (SP)+,D0-A6
cmd_findasc7:   bsr     check_keyb                  ;Button pressed?
                bmi.s   cmd_findasc8                ;then abort
                addq.l  #2,A2                       ;to the next opcode
                cmpa.l  A1,A2
                blo.s   cmd_findasc6
                move.b  #2,find_cont0(A4)
                move.l  A2,default_adr(A4)
                jmp     (A4)
cmd_findasc8:   moveq   #1,D5                       ;for Continue
                movea.l A1,A3
                bra     cmd_find13
                ENDPART

********************************************************************************
* match(what,how,where,all,one) - Universal search function with wildcards     *
* match(->A6,->A5,<-A3,D7,D6)                                                  *
********************************************************************************
                PART 'match'
match:          movem.l D1-D2/A0/A5-A6,-(SP)
match_loop1:    move.b  (A5)+,D2
                beq.s   match_not                   ;End of the How String => Nix found
                cmp.b   D6,D2
                beq.s   match_loop1                 ;Existential quantities and
                cmp.b   D7,D2                       ;Allquantors at the beginning
                beq.s   match_loop1
                movea.l A6,A3
match_jump2:    movea.l A5,A0                       ;Pointer back to the beginning
                movea.l A3,A6
match_loop2:    move.b  (A6)+,D1                    ;End of the string achieved?
                beq.s   match_not                   ;then nothing found
                cmp.b   D2,D1                       ;First of the same sign found?
                bne.s   match_loop2                 ;Otherwise continue ...
                movea.l A6,A3                       ;Akt.position notice
match_loop3:    move.b  (A0)+,D0
                beq.s   match_yeah                  ;End of the How String => Found !!
                move.b  (A6)+,D1
                beq.s   match_jump3                 ;End of the string
                cmp.b   D6,D0                       ;Ignore existence quantum
                beq.s   match_loop3
                cmp.b   D7,D0
                beq.s   match_jump1                 ;Allquantor search
                cmp.b   D0,D1                       ;Sign right?
                bne.s   match_jump2                 ;Search again, if not
                bra.s   match_loop3
match_jump3:    cmp.b   D7,D0                       ;No allquantor?
                bne.s   match_not                   ;=> Nothing found
                tst.b   (A0)                        ;Follow search signs?
                bne.s   match_not                   ;then not found
                bra.s   match_yeah                  ;otherwise found
match_jump1:    move.b  (A0)+,D0                    ;Allquantor search
                beq.s   match_yeah                  ;How String End = Found
                cmp.b   D7,D0                       ;Ignore several allquanes
                beq.s   match_jump1
                tst.b   (A6)                        ;String End?
                beq.s   match_jump2                 ;Stringend = not found
match_loop4:    cmp.b   D0,D1                       ;Sign right?
                beq.s   match_loop3                 ;Yes, continue looking for
                move.b  (A6)+,D1                    ;Get next character
                bne.s   match_loop4                 ;String Non-Zuenden => Search
                bra.s   match_jump2                 ;Stringing, continues

match_yeah:     moveq   #-1,D0                      ;Found!
                subq.l  #1,A3                       ;From here the string was found
                bra.s   match_end
match_not:      moveq   #0,D0                       ;Not found
match_end:      movem.l (SP)+,D1-D2/A0/A5-A6
                rts
                ENDPART

********************************************************************************
* 'PRN' - Printer edition                                                      *
********************************************************************************
                PART 'cmd_prnt'
cmd_prnt:       btst    #0,$FFFFFA01.w              ;Busy flag of the printer
                bne     prn_err                     ;Nothing to do
                clr.w   prn_pos(A4)                 ;Reset pointer
                st      device(A4)                  ;Flag for printer
                bra     inp_loop1                   ;to the next sign
                ENDPART

********************************************************************************
* 'MOVE' - Move memory block                                                   *
********************************************************************************
                PART 'cmd_move'
cmd_move:       bsr     get_parameter               ;Start and end
                bcs     synerr
                bvs     synerr                      ;Parameters must be specified
                cmpa.l  A3,A2
                bhs     illequa                     ;Start> = End!
                cmp.b   #',',D0
                bne     synerr
                bsr     get
                bsr     get_term                    ;Get destination address
                cmpa.l  D1,A2                       ;Compare destination with the beginning
                beq     ret_jump
                ori     #$0700,SR
                blo.s   cmd_move2                   ;Goal> Source
                movea.l D1,A6
cmd_move1:      move.b  (A2)+,(A6)+
                cmpa.l  A2,A3                       ;End?
                bne.s   cmd_move1
                jmp     (A4)
cmd_move2:      movea.l A3,A6                       ;A3 notice
                suba.l  A2,A3                       ;How many bytes should be postponed?
                adda.l  D1,A3                       ;Plus destination address (last adr. of the target area)
cmd_move3:      move.b  -(A6),-(A3)
                cmpa.l  A6,A2                       ;End?
                bne.s   cmd_move3
                jmp     (A4)
                ENDPART

********************************************************************************
* 'FILL' - Fill storage area with byte sequence                                *
********************************************************************************
                PART 'cmd_fill'
cmd_fill:       bsr     get_parameter
                bsr     get_such_para               ;Get what he should fill in
                ori     #$0700,SR
cmd_fill1:      moveq   #-1,D4
cmd_fill2:      addq.w  #1,D4
                move.b  0(A1,D4.w),(A2)+
                cmpa.l  A2,A3                       ;End?
                beq     ret_jump                    ;Yes finished
                cmp.w   D3,D4                       ;Length reached?
                blo.s   cmd_fill2
                bra.s   cmd_fill1
                ENDPART

********************************************************************************
* 'COMPARE' - Compare memory areas                                             *
********************************************************************************
                PART 'cmd_compare'
cmd_compare:    bsr     get_parameter               ;Start and end
                bcs     synerr
                bvs     synerr                      ;Parameters must be specified
                cmpa.l  A3,A2
                bhs     illequa                     ;Start> = End!
                cmp.b   #',',D0
                bne     synerr
                bsr     get
                bsr     get_term                    ;Get destination address
                cmpa.l  D1,A2                       ;Compare destination with the beginning
                beq     ret_jump                    ;Cancel, if equal
                movea.l D1,A1
cmd_compare1:   cmpm.b  (A1)+,(A2)+                 ;Compare memory locations
                beq.s   cmd_compare3
                move.l  A1,D1
                subq.l  #1,D1
                moveq   #' ',D0
                tst.w   spalte(A4)
                bne.s   cmd_compare2
                moveq   #';',D0                     ;For the beginning of the line
cmd_compare2:   jsr     @chrout(A4)
                bsr     hexlout                     ;Spend unequal address
                jsr     @space(A4)
cmd_compare3:   bsr     check_keyb                  ;Button pressed?
                bmi.s   cmd_compare4                ;then abort
                cmpa.l  A2,A3                       ;End
                bhs.s   cmd_compare1
cmd_compare4:   move.l  A2,default_adr(A4)
                jsr     @crout(A4)                  ;and still CR to the end
                jmp     (A4)
                ENDPART

********************************************************************************
* Disassemble (Change hex)                                                     *
********************************************************************************
                PART 'cmd_dchng'
cmd_dchng:      bclr    #0,default_adr+3(A4)        ;So that it is certainly straight!
                move.l  default_adr(A4),-(SP)
                ori     #$0700,SR
cmd_dchng1:     bsr     get
                bsr     get_term                    ;Bring expression according to D1
                move.l  D1,D2
                and.l   #$FFFF0000,D2
                bne     illequa                     ;I suppose that did not work
                movea.l default_adr(A4),A6
                bsr     check_write
                bne.s   cmd_dchng2
                move.w  D1,(A6)                     ;pure
cmd_dchng2:     addq.l  #2,default_adr(A4)          ;Replace DefaultArD
                cmp.b   #',',D0                     ;it continues?
                beq.s   cmd_dchng1                  ;yes!
                movea.l (SP)+,A6
                subq.w  #1,zeile(A4)                ;Row back
                sf      list_flg(A4)                ;Issue not symbolic
                bsr     do_disass                   ;Output line again
                jsr     @crout(A4)                  ;and again CR
                jmp     (A4)
                ENDPART

********************************************************************************
* ']' - Change memory (only one parameter!)                                    *
********************************************************************************
                PART 'cmd_schng'
cmd_schng:      bsr     get_such_para3              ;Proteure (Fill Parameters) After (A1)
                lea     default_adr(A4),A2
                movea.l (A2),A6
                ori     #$0700,SR
cmd_schng1:     move.b  (A1)+,D1                    ;Write all data in the memory
                bsr     check_write
                bne.s   cmd_schng2
                move.b  D1,(A6)                     ;are changeable, otherwise nothing.
cmd_schng2:     addq.l  #1,A6
                dbra    D3,cmd_schng1
                move.l  A6,(A2)                     ;Record as a new default_ADR
                jmp     (A4)
                ENDPART

********************************************************************************
* ',' - Change memory (Memory Dump Command)                                    *
********************************************************************************
                PART 'cmd_mchng'
cmd_mchng:      move.l  default_adr(A4),-(SP)
                ori     #$0700,SR
                bsr     get_such_para3              ;Proteure (Fill Parameters) After (A1)
cmd_mchng1:     move.w  D3,D5                       ;Remember length of the expression
                lea     default_adr(A4),A2
                movea.l (A2),A6
cmd_mchng2:     move.b  (A1)+,D1                    ;Write all data in the memory
                bsr     check_write
                bne.s   cmd_mchng3
                move.b  D1,(A6)                     ;are changeable, otherwise nothing.
cmd_mchng3:     addq.l  #1,A6
                dbra    D3,cmd_mchng2
                move.l  A6,(A2)                     ;Record as a new default_ADR
cmd_mchng4:     cmp.b   #$22,D0                     ;Line ends reached?
                beq.s   cmd_mchng5                  ;Yes!
                tst.b   D0
                beq.s   cmd_mchng5
                cmp.b   #',',D0
                bne     synerr
                bsr     get_such_para3              ;N parameters
                dbra    D5,cmd_mchng4               ;and continue testing
                bra.s   cmd_mchng1                  ;The parameter is valid again
cmd_mchng5:     movea.l (SP)+,A6
                clr.w   spalte(A4)
                subq.w  #1,zeile(A4)                ;Row back
                moveq   #0,D3
                bsr     cmd_dump7                   ;Output line again
                addq.w  #1,zeile(A4)
                jmp     (A4)
                ENDPART

********************************************************************************
* '.x' - Change memory (also Word / Long-based)                                *
********************************************************************************
                PART 'cmd_chng'
cmd_chng:       moveq   #'.',D0
                bsr     get_extension               ;Command text tension code according to D3
                bmi     synerr
                ori     #$0700,SR
                move.l  default_adr(A4),-(SP)
                move.w  D3,-(SP)                    ;Remember length
                bra.s   cmd_chng2
cmd_chng1:      bsr     get
cmd_chng2:      bsr     get_term                    ;termHolen
                move.w  (SP),D3                     ;Length
                beq     synerr                      ;.B is prohibited
                bclr    #0,default_adr+3(A4)
                movea.l default_adr(A4),A6
                bsr     check_write
                bne.s   cmd_chng4
                cmp.w   #1,D3                       ;Long abtesten.
                bne.s   cmd_chng3                   ;.L!
                move.w  D1,(A6)
                bra.s   cmd_chng4
cmd_chng3:      move.l  D1,(A6)
cmd_chng4:      cmp.w   #1,D3
                beq.s   cmd_chng5
                addq.l  #2,A6                       ;long2+2=4Byte
cmd_chng5:      addq.l  #2,A6                       ;word =2Byte
                move.l  A6,default_adr(A4)
                tst.b   D0
                beq.s   cmd_chng6
                cmp.b   #',',D0                     ;Is there anything else?
                beq.s   cmd_chng1
cmd_chng6:      move.w  (SP)+,D3                    ;Number basis
                movea.l (SP)+,A6                    ;defaultadr
                clr.w   spalte(A4)
                subq.w  #1,zeile(A4)                ;Row back
                bsr     cmd_dump7                   ;Output line again
                addq.w  #1,zeile(A4)
                jmp     (A4)
                ENDPART

********************************************************************************
* 'RESIDENT' - Program end (memory but not releasing again)                    *
********************************************************************************
                PART 'cmd_resident'
cmd_resident:   movea.l save_data+8(A4),A0          ;Get bus error vector
                cmpi.w  #'∑-',-(A0)
                bne.s   cmd_resident1               ;vektorDesDebuggers?
                cmpi.l  #'Soft',-(A0)
                beq     cmd_exit                    ;End, if so
cmd_resident1:  tst.b   resident(A4)
                bne     ret_jump                    ;Debugger is already resident
                lea     resi_txt(PC),A0
                jsr     ask_user

cmd_resident2:  movea.l save_data(A4),A0            ;Get bus error vector
                cmpi.w  #'∑-',-(A0)
                bne.s   cmd_resident3               ;vektorDesDebuggers?
                cmpi.l  #'Soft',-(A0)
                bne.s   cmd_resident3               ;End, if so
                move.l  old_trap3(PC),$8C.w         ;old trap # 3 vector write back
                bra     cmd_exit1                   ;normalerExit
cmd_resident3:  move.w  _fhdle2(A4),D0              ;Protocol file does not exist
                bls.s   cmd_resident4
                move.w  D0,-(SP)
                move.w  #$3E,-(SP)
                trap    #1                          ;fclose()
                addq.l  #4,SP
cmd_resident4:  tst.b   ass_load(A4)                ;Loading through the assembler?
                bne.s   cmd_resident5               ;Yes!=>
                bsr     reset_all                   ;reset all
                bra.s   cmd_resident6
cmd_resident5:  bsr     copy_sys_vars               ;Always copy system variables back
cmd_resident6:  bsr     set_spez_vek                ;Error vectors again
                pea     @_trap3(A4)
                move.l  #$050023,-(SP)
                trap    #13                         ;Set trap # 3 (on Or.w # $ 2000, (SP): RTE)
                addq.l  #8,SP
                lea     8.w,A0
                lea     save_data(A4),A1
                movea.l A1,A2
                move.w  #361,D1
cmd_resident7:  move.l  (A0)+,(A1)+                 ;$ 8- $ 5AF rescue (with vectors)
                dbra    D1,cmd_resident7

                move.l  $0502.w,old_alt_help
                tst.b   ass_load(A4)                ;Loading through the assembler?
                bne.s   cmd_resident8               ;Yes!=>
                move.l  #alt_help,$0502.w

cmd_resident8:  andi    #$FB00,SR                   ;Release IRQ again
                clr.l   $0426.w                     ;Reset vector invalid

                sf      ass_load(A4)                ;Loading by the assembler finished
                st      resident(A4)                ;Flag put that the debugger resident
                sf      do_resident(A4)             ;Automatic 'Resident' turn off
                move.l  old_stack(A4),-(SP)
                move.w  #$20,-(SP)
                trap    #1                          ;userModusAn
                addq.l  #6,SP
                movea.l old_usp(A4),SP

                bsr     cmd_mon1                    ;Mouse again

                sf      le_allowed(A4)              ;Le is now prohibited

                move.w  #1,-(SP)                    ;debuggerIstResident
                move.l  end_adr(A4),D0
                sub.l   #anfang-256,D0              ;programmlänge +Basepage
                move.l  D0,-(SP)                    ;So much memory remains occupied
                move.w  #$31,-(SP)
                trap    #1                          ;ptermres()

                SWITCH language
                CASE 0
resi_txt:       DC.B 'Wollen Sie den Debugger resident halten? (j/n) ',0
                CASE 1
resi_txt:       DC.B 'Keep the debugger resident? (y/n) ',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* 'EXIT' / 'SYSTEM' / 'QUIT' - Program end                                     *
********************************************************************************
                PART 'cmd_exit'
cmd_exit:       lea     cmd_exit5(PC),A0
                jsr     ask_user

cmd_exit1:      lea     etv_exit.w,A0
                move.l  (A0),D0                     ;Bits 0-31 = 0?
                beq.s   cmd_exit10                  ;=> No vector
                btst    #0,D0                       ;Bit 0 = 1?
                bne.s   cmd_exit10                  ;=> No vector
                jsr     @org_driver(A4)             ;Original keyboard driver pure
                movea.l $040C.w,A0
                jsr     (A0)                        ;Routine start
                jsr     @my_driver(A4)              ;Own driver
cmd_exit10:     clr.l   etv_exit.w                  ;ETV_Exit () - Delete vector
                sf      do_resident(A4)
                moveq   #6,D3                       ;close all open files
cmd_exit2:      move.w  D3,-(SP)
                move.w  #$3E,-(SP)
                trap    #1                          ;Fclose()
                addq.l  #4,SP
                addq.w  #1,D3
                cmp.w   #80,D3
                bne.s   cmd_exit2

                bsr     reset_all

                clr.l   etv_exit.w                  ;ETV_Exit () - Delete vector
                moveq   #14,D0
                bsr     disable_irq                 ;Ring indicator

                move.l  old_trap3(PC),$8C.w
                tst.b   resident(A4)
                beq.s   cmd_exit3
                lea     @_trap3(A4),A0              ;Return Trap # 3
                move.l  A0,$8C.w

cmd_exit3:      movea.l kbshift_adr(A4),A0
                clr.b   (A0)                        ;Delete KBSHIFT status

                sf      le_allowed(A4)              ;Le is now prohibited

                bsr     cmd_mon1                    ;Mouse again

                move.l  quit_stk(A4),D1             ;Return account?
                beq.s   cmd_exit4
                clr.l   quit_stk(A4)                ;Delete return address
                lea     spaced2(A4),A0
                move.l  line_back(A4),D0            ;pcOffset
                move.l  D1,-(SP)
                rts                                 ;and Quit
cmd_exit4:      andi    #$FBFF,SR

                move.l  old_stack(A4),-(SP)
                move.w  #$20,-(SP)
                trap    #1                          ;userModusAn
                addq.l  #6,SP
                movea.l old_usp(A4),SP

                clr.w   -(SP)                       ;Exit to GEMDOS
                trap    #1

                SWITCH language
                CASE 0
cmd_exit5:      DC.B 'Wäre es ihnen genehm, den Debugger zu verlassen? (j/n) ',0
                CASE 1
cmd_exit5:      DC.B 'Would you like to leave the debugger? (y/n) ',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* 'SET' - Change register                                                      *
********************************************************************************
                PART 'cmd_set'
cmd_set:        bsr     get
                move.w  D0,D2                       ;News Make News 1st
                lea     varstab(PC),A1
                lea     w_legalc(PC),A3
                movea.l A0,A2                       ;Remember pointer to possibly variable or number
cmd_se0:        moveq   #-1,D1
                move.w  D2,D0                       ;Pick up 1st
                tst.b   (A1)                        ;End of the table reached?
                bmi     synerr                      ;not found
cmd_se1:        addq.w  #1,D1
                cmpi.b  #' ',0(A1,D1.w)             ;Entry found?
                beq.s   cmd_se5                     ;Yes!
                tst.b   0(A1,D1.w)
                beq.s   cmd_se5                     ;Entry also found
                tst.w   D1                          ;1st sign of the label
                beq.s   cmd_se2                     ;Everything is still allowed
                ext.w   D0
                bmi.s   cmd_se3                     ;Signs> 127 are not allowed!
                tst.b   0(A3,D0.w)                  ;Sign still allowed?
                bne.s   cmd_se3                     ;No!=> Cancel, since unequal
cmd_se2:        cmp.b   0(A1,D1.w),D0               ;Still the same?
cmd_se3:        move    SR,D3
                bsr     get                         ;ever bring the next sign
                move.w  D0,D4                       ;Save if it was the last sign
                move    D3,CCR
                beq.s   cmd_se1                     ;If the same, test next character
                lea     16(A1),A1                   ;Pointer to the next variable
                movea.l A2,A0                       ;Pointer back
                bra.s   cmd_se0                     ;Next
cmd_se5:        move.w  D4,D0
                lea     12(A1),A2
                moveq   #0,D2
                move.w  8(A1),D4                    ;artDerVariable
                move.w  10(A1),D2                   ;Handover parameters
                movea.l (A2),A1                     ;pointer
                cmp.w   #3,D4
                beq.s   cmd_se7                     ;A1 shows on subroutine
                cmp.b   #'=',D0                     ;It just has to come
                bne     synerr
                bsr     get
                bsr     get_term                    ;Evaluate term according to D1
                tst.w   D4                          ;Direct value?
                beq.s   cmd_se8
                adda.l  A4,A1
                cmp.w   #4,D4
                beq.s   cmd_se6                     ;A1 shows on memory cell (Word)
;A1 zeigt auf Speicherzelle (Long)
                move.l  D1,(A1)
                bra.s   cmd_se9
cmd_se6:        cmp.l   D2,D1
                bhi     illequa                     ;Value too big!
                move.w  D1,(A1)
                bra.s   cmd_se9
cmd_se8:        move.l  D1,(A2)                     ;Enter direct value
                bra.s   cmd_se9
cmd_se7:        jsr     (A1)                        ;Call the subroutine
cmd_se9:        jmp     (A4)
cmd_sec1:
                move.w  #col1,D7
                bra.s   cmd_secb
cmd_sec0:       move.w  #col0,D7
cmd_secb:       bsr     tstglzahl
                move.l  D1,D2
                and.l   #$FFFFF000,D2
                bne     illequa
                move.w  D1,0(A4,D7.w)
                lea     $FFFF8240.w,A1
                move.w  col0(A4),(A1)+
                moveq   #14,D1
cmd_secc:       move.w  col1(A4),(A1)+              ;Put the colors
                dbra    D1,cmd_secc
                rts
cmd_sec:        bsr     tstglzahl                   ;SR set
                andi.w  #$7FFF,D1
                move.w  D1,_sr(A4)
                rts
cmd_sed:        bsr     tstglzahl                   ;CCR set
                andi.w  #$FF,D1
                move.b  D1,_sr+1(A4)
                rts
cmd_sebk:       bsr     get_term
                tst.l   D1
                bmi     illbkpt
                cmp.l   #15,D1
                bhi     illbkpt
                mulu    #12,D1
                lea     breakpnt(A4),A1
                adda.w  D1,A1
                bsr     tstglzahl                   ;Get address of the breakpoints
                bclr    #0,D1
                tst.l   D1
                beq.s   cmd_sbk                     ;Zero allowed as an argument
                movea.l D1,A6
                bsr     check_write
                bne     illequa
cmd_sbk:        move.l  D1,(A1)+                    ;Get address of the Breakpoint
                move.w  #-1,(A1)+                   ;stopBreakpoint
                move.l  #1,(A1)                     ;run only once
                rts

w_zahlscache:   tst.b   prozessor(A4)               ;68000 or 68010?
                ble.s   w_zahlscachee               ;then out here
                bsr     tstglzahl                   ;Put ccrc
                DC.W $4E7B,$1002                    ;CACR setzen
w_zahlscachee:  rts

cmd_seme:       moveq   #10,D2                      ;max.10 user variables
                bsr     chkval
                bcc     synerr
                subq.w  #1,D0
                bpl.s   cmd_semx
                moveq   #9,D0
cmd_semx:       move.l  merk_svar(A4),D1
                beq.s   cmd_semxy                   ;No transfer by the assembler
                movea.l D1,A1
                move.w  D0,D1
                mulu    #6,D1
                move.w  #-1,0(A1,D1.w)
cmd_semxy:      lea     simple_vars(A4),A1
                lsl.w   #2,D0
                adda.w  D0,A1
                bsr     get
                bsr.s   tstglzahl
                move.l  D1,(A1)
                rts

cmd_sef:        lea     regs(A4),A1                 ;Dn set
                bra.s   cmd_seh
cmd_seg:        lea     regs+32(A4),A1              ;An set
cmd_seh:        moveq   #8,D2
                bsr     chkval                      ;max.7 is allowed
                bcc     synerr
                lsl.w   #2,D0                       ;mal4 (long)
                adda.w  D0,A1
                bsr     get                         ;"Hopend)
                lea     rega7(A4),A6
                cmpa.l  A6,A1                       ;A7 changed?
                beq.s   cmd_sea                     ;Yes!=> Change
                bsr.s   tstglzahl
                move.l  D1,(A1)                     ;Tab
                rts
cmd_sea:        lea     _usp(A4),A1                 ;SP setzen
                btst    #5,_sr(A4)                  ;Supervisor-Mode?
                beq.s   cmd_seb
                lea     _ssp(A4),A1
cmd_seb:        bsr.s   tstglzahl
                move.l  D1,(A1)
                rts
cmd_sei:        bsr.s   tstglzahl                   ;Change DISBASE / DB
                cmp.w   #10,D1
                beq.s   cmd_sej
                cmp.w   #$10,D1
                bne     illequa                     ;Only decimal & hexadecimal are allowed
cmd_sej:        move.w  D1,disbase(A4)              ;Disbase
                rts
cmd_sek:        bsr.s   tstglzahl                   ;All put
                lea     regs(A4),A0
                moveq   #14,D0
cmd_sel:        move.l  D1,(A0)+
                dbra    D0,cmd_sel
                rts

tstglzahl:      cmp.b   #'=',D0                     ;It just has to come
                bne     synerr
                bsr     get
                bra     get_term                    ;Evaluate term according to D1

                DXSET 8,' '
varstab:        DX.B 'SYMFLAG'
                DC.W 4,-1
                DC.L bugaboo_sym
                DX.B 'RING'
                DC.W 4,1
                DC.L ring_flag
                DX.B 'TRACE'
                DC.W 4,2
                DC.L trace_flag
                DX.B 'TDELAY'
                DC.W 4,-1
                DC.L trace_delay
                DX.B 'MIDI'
                DC.W 4,1
                DC.L midi_flag
                DX.B 'OVERSCAN'
                DC.W 4,1
                DC.L overscan
                DX.B 'SWITCH'
                DC.W 4,1
                DC.L smart_switch
                DX.B 'CACHE'
                DC.W 3,1
                DC.L w_zahlscache
                DX.B 'MEMCHECK'
                DC.W 4,1
                DC.L all_memory
                DX.B 'SHIFT'
                DC.W 4,1
                DC.L shift_flag
                DX.B 'CLICK'
                DC.W 4,1
                DC.L format_flag
                DX.B 'KLICK'
                DC.W 4,1
                DC.L format_flag
                DX.B 'AESFLAG'
                DC.W 4,1
                DC.L no_aes_check
                DX.B 'PC'
                DC.W 1,0
                DC.L _pc
                DX.B 'SCROLLD'
                DC.W 4,-1
                DC.L scroll_d
                DX.B 'CONTERM'
                DC.W 4,1
                DC.L conterm
                DX.B 'COL0'
                DC.W 3,0
                DC.L cmd_sec0
                DX.B 'COL1'
                DC.W 3,0
                DC.L cmd_sec1
                DX.B 'SMALL'
                DC.W 4,1
                DC.L small
                DX.B 'SIZE'
                DC.W 4,16
                DC.L def_size
                DX.B 'LINES'
                DC.W 4,255
                DC.L def_lines
                DX.B 'ALL'
                DC.W 3,0
                DC.L cmd_sek
                DX.B 'USP'
                DC.W 1,0
                DC.L _usp
                DX.B 'SP'
                DC.W 3,0
                DC.L cmd_sea
                DX.B 'SSP'
                DC.W 1,0
                DC.L _ssp
                DX.B 'SR'
                DC.W 3,0
                DC.L cmd_sec
                DX.B 'CCR'
                DC.W 3,0
                DC.L cmd_sed
                DX.B '*'
                DC.W 1,0
                DC.L default_adr
                DX.B 'DISBASE'
                DC.W 3,0
                DC.L cmd_sei
                DX.B 'BUFFER'
                DC.W 1,0
                DC.L dsk_adr
                DX.B 'TRACK'
                DC.W 4,85
                DC.L dsk_track
                DX.B 'SEKTOR'
                DC.W 4,255
                DC.L dsk_sektor
                DX.B 'SECTOR'
                DC.W 4,255
                DC.L dsk_sektor
                DX.B 'SIDE'
                DC.W 4,1
                DC.L dsk_side
                DX.B 'DRIVE'
                DC.W 4,1
                DC.L dsk_drive
                DX.B 'D'
                DC.W 3,0
                DC.L cmd_sef
                DX.B 'A'
                DC.W 3,0
                DC.L cmd_seg
                DX.B 'B'
                DC.W 3,0
                DC.L cmd_sebk
                DX.B 'M'
                DC.W 3,0
                DC.L cmd_seme
                DC.B -1
                EVEN
                ENDPART

********************************************************************************
* Output tab and menu (if necessary)                                           *
********************************************************************************
                PART 'rgout'
rgout:          move.l  zeile(A4),-(SP)             ;Line and (!) Save column
                move.w  entry_old(A4),D0
                cmp.w   entry(A4),D0
                beq.s   rgout1
                jsr     draw_menu
                moveq   #0,D7                       ;select
                move.w  entry(A4),D0
                jsr     sel_menu
rgout1:         bsr     hunt_pc                     ;Possibly label on the beginning of the line
                move.w  upper_line(A4),D0
                neg.w   D0
                addq.w  #2,D0
                move.w  D0,zeile(A4)                ;-3 in normal case
                clr.w   spalte(A4)
                moveq   #'',D0                     ;Closer
                movea.l reg_pos(A4),A5
                movea.l trace_pos(A4),A1
                cmpa.l  A1,A5                       ;Akt.registersatz?
                bne.s   rgout11                     ;No!
                moveq   #' ',D0
rgout11:        bsr     charout
                lea     _regtxt(PC),A1
                bsr     txtout2                     ;Output PC
                andi.b  #$FE,_pc+3(A4)
                andi.b  #$FE,64+3(A5)
                move.l  64(A5),D1
                bsr     hexlout
                bsr     txtout2                     ;Output USP
                andi.b  #$FE,68+3(A5)
                move.l  68(A5),D1
                bsr     hexlout
                bsr     txtout2                     ;Output SSP
                andi.b  #$FE,72+3(A5)
                move.l  72(A5),D1
                bsr     hexlout
                bsr     txtout2                     ;Spend
                bsr     sr_out
                jsr     @space(A4)
                movea.l 64(A5),A6
                lea     spaced(A4),A0
                moveq   #19,D0
rgoutn0:        clr.l   (A0)+                       ;Clear buffer
                dbra    D0,rgoutn0
                bsr     disass
                clr.b   testwrd(A4)                 ;Output no longer in the buffer
                lea     spaced(A4),A0
                move.l  A0,-(SP)
                lea     31(A0),A0
                moveq   #0,D1
                tst.b   (A0)
                beq.s   rgoutnn
                move.b  #'*',(A0)+
                clr.b   (A0)
                moveq   #-1,D1
rgoutnn:        jsr     @print_line(A4)             ;Output the result of the disassembler
                tst.w   D1                          ;Line residue available?
                bne.s   rgoutnm                     ;No!=>
rgoutnm1:       moveq   #' ',D0
                bsr     charout                     ;Spaces output to the end of the line
                tst.w   spalte(A4)                  ;Next line reached?
                bne.s   rgoutnm1                    ;No!=> Next
rgoutnm:        move.w  upper_line(A4),D0
                neg.w   D0
                addq.w  #3,D0
                move.w  D0,zeile(A4)                ;-2 in normal case
                clr.w   spalte(A4)
                moveq   #'',D0                     ;Arrow to the left
                bsr     charout
                lea     _regtxt2(PC),A1
                bsr     txtout2                     ;Output D0-D7
                movea.l reg_pos(A4),A2
                moveq   #7,D6                       ;8Register
rgoutn1:        move.l  (A2)+,D1                    ;Get register
                bsr     hexlout                     ;and spend
                jsr     @space(A4)
                dbra    D6,rgoutn1
                moveq   #'',D0                     ;arrow to the right
                bsr     charout
                bsr     txtout2                     ;Output A0-A7
                moveq   #6,D6                       ;7Register
rgoutn5:        move.l  (A2)+,D1                    ;Get register
                bsr     hexlout                     ;and spend
                jsr     @space(A4)
                dbra    D6,rgoutn5
                movea.l reg_pos(A4),A5
                move.l  68(A5),D1                   ;Assume suspected user mode
                btst    #5,76(A5)                   ;Check supervisor bit
                beq.s   rgoutn4                     ;usermode!
                move.l  72(A5),D1

rgoutn4:        move.l  D1,rega7(A4)                ;Put in7
                bsr     hexlout                     ;Spend StackPNT
                moveq   #' ',D0
                bsr     charout                     ;Spaces output to the end of the line
                moveq   #79,D0
                bsr     draw_line                   ;Draw horizontal line from line 5
                move.w  upper_line(A4),D6
                neg.w   D6
                addq.w  #5,D6
                beq     rgoute5                     ;No headers available!=>
                move.w  def_size(A4),D7
                move.w  #16,def_size(A4)
                neg.w   D6
                subq.w  #1,D6
                movea.l #spez_buff,A0
                adda.l  A4,A0
                lea     regs(A4),A1                 ;Transfer parameters to user trace
rgoute6:        movem.l D1-A6,-(SP)
                jsr     (A0)                        ;Determine address
                movem.l (SP)+,D1-A6
                movea.l D0,A6
                movem.l D0-A6,-(SP)
                move.l  zeile(A4),-(SP)
                move.w  upper_line(A4),D0
                subq.w  #5,D0
                sub.w   D0,D6
                move.w  D6,zeile(A4)
                lea     spaced2(A4),A0
                neg.w   D6
                movea.l #spez_format,A1
                adda.l  A4,A1
                adda.w  D6,A1
                moveq   #0,D3
                moveq   #3,D3
                and.b   -(A1),D3                    ;Get the output width
                moveq   #0,D2
                move.b  (A1),D2
                lsr.b   #4,D2
                addq.b  #1,D2
                move.w  D2,def_size(A4)             ;Size set

                addi.b  #'0'-1,D6
                move.b  D6,(A0)+
                move.b  #':',(A0)+
                bsr     cmd_dump8                   ;Dump output
                move.l  (SP)+,zeile(A4)
                movem.l (SP)+,D0-A6
                lea     256(A0),A0
                dbra    D6,rgoute6
                move.w  D7,def_size(A4)

                move.w  upper_line(A4),D0
                lsl.w   #4,D0
                subq.w  #1,D0
                bsr     draw_line                   ;Beginning of the free screen
rgoute5:        move.l  (SP)+,zeile(A4)             ;Line and (!) Column back
                rts

_regtxt:        DC.B ' PC=',0,' USP=',0,' SSP=',0,' SR=',0
_regtxt2:       DC.B ' D0-D7 ',0
                DC.B ' A0-A7 ',0

txtout2:        move.l  A1,-(SP)
                jsr     @print_line(A4)
txtout2a:       tst.b   (A1)+
                bne.s   txtout2a
                rts

sr_out:         movem.l D0-D7,-(SP)
                move.w  zeile(A4),-(SP)
                move.w  #2,zeile(A4)
                move.w  #40,spalte(A4)
                bsr     clr_maus
                movea.l reg_pos(A4),A5
                move.w  76(A5),D5                   ;SR-Register to fetch
                moveq   #15,D4
sr_out0:        moveq   #-1,D1                      ;NOT LIGHT
                moveq   #0,D2                       ;Not inverse
                moveq   #0,D3                       ;Underlined
                btst    D4,D5
                bne.s   sr_out2
                moveq   #$55,D1                     ;light
sr_out2:        moveq   #0,D0
                move.b  sr_txt(PC,D4.w),D0
                beq.s   sr_out3
                jsr     light_char
                addq.w  #1,spalte(A4)
sr_out3:        dbra    D4,sr_out0
                bsr     set_maus
                move.w  (SP)+,zeile(A4)
                movem.l (SP)+,D0-D7
                rts

sr_txt:         DC.B 'CVZNX',0,0,0,'012',0,0,'S',0,'T'
                ENDPART

********************************************************************************
* 'INFO' - Information about the memory assignment                             *
********************************************************************************
                PART 'cmd_sysinfo'
cmd_sysinfo:    movea.l $04F2.w,A6                  ;_sysbase
                pea     cmd_sysinfotxt1(PC)
                jsr     @print_line(A4)             ;"TOS-Version"
                movea.l 8(A6),A0                    ;Pointer to the ROM
                move.w  $1C(A0),D1                  ;os_conf to fetch
                lea     cmd_sysinfotab1(PC),A0
                btst    #0,D1
                beq.s   cmd_sysinfo1
                addq.l  #5,A0
cmd_sysinfo1:   move.l  A0,-(SP)                    ;NTSC/PAL
                jsr     @print_line(A4)
                moveq   #'-',D0
                jsr     @chrout(A4)
                lsr.w   #1,D1                       ;by 2
                cmp.w   #15,D1
                blo.s   cmd_sysinfo2
                moveq   #15,D1                      ;'???'for unknown country
cmd_sysinfo2:   lsl.w   #2,D1                       ;4
                lea     cmd_sysinfotab2(PC),A0
                adda.w  D1,A0
                move.l  A0,-(SP)                    ;spend the land
                jsr     @print_line(A4)
                jsr     @space(A4)
                moveq   #'0',D0
                add.b   2(A6),D0
                jsr     @chrout(A4)                 ;Output TOS version
                bsr     cmd_sysinfo_sub
                move.b  3(A6),D1
                bsr     hexbout
                pea     cmd_sysinfotxt2(PC)
                jsr     @print_line(A4)             ;" from "
                move.b  $19(A6),D1
                bsr     hexbout
                bsr     cmd_sysinfo_sub
                move.b  $18(A6),D1
                bsr     hexbout
                bsr     cmd_sysinfo_sub
                move.w  $1A(A6),D1
                bsr     hexwout
                pea     cmd_sysinfotxt12(PC)        ;ROM-Base
                jsr     @print_line(A4)
                move.l  8(A6),D1                    ;Base address of the ROMs
                bsr     hexout                      ;output
                pea     cmd_sysinfotxt3(PC)         ;GEMDOS-Version
                jsr     @print_line(A4)
                move.w  #$30,-(SP)
                bsr     do_trap_1                   ;Sversion()
                addq.l  #2,SP
                move.w  D0,D3
                moveq   #10,D2                      ;Number basis
                moveq   #0,D1
                move.b  D3,D1
                bsr     numoutx
                jsr     @chrout(A4)                 ;1.Day
                bsr     cmd_sysinfo_sub
                lsr.w   #8,D3
                move.w  D3,D1
                bsr     numoutx                     ;2.Day

                pea     cmd_sysinfotxt4(PC)         ;AES-Version
                jsr     @print_line(A4)
                move.l  #$0A000100,D0               ;appl_init()
                bsr     aes
                move.l  #$13000100,D0               ;appl_exit()
                bsr     aes
                moveq   #'0',D0
                add.b   spaced2+32(A4),D0           ;AES-Get version number
                jsr     @chrout(A4)                 ;1.Day
                bsr     cmd_sysinfo_sub
                move.b  spaced2+33(A4),D0
                lsr.b   #4,D0
                add.b   #'0',D0
                jsr     @chrout(A4)                 ;2.Day
                pea     cmd_sysinfotxt5(PC)
                jsr     @print_line(A4)             ;"VDI-Version : GDOS is "
                moveq   #-2,D0
                trap    #2                          ;vq_gdos() : GDOS gives?
                addq.w  #2,D0
                bne.s   cmd_sysinfo3                ;GDOs is here!=>
                pea     cmd_sysinfotxt8(PC)
                jsr     @print_line(A4)             ;"not "
cmd_sysinfo3:   pea     cmd_sysinfotxt6(PC)         ;"available"
                jsr     @print_line(A4)             ;"Tank frequency:"
                move.l  $04BA.w,D2
                move.w  #32767,D0
cmd_sysinfo4:   moveq   #$AA,D1
                divu    #$1111,D1
                dbra    D0,cmd_sysinfo4
                sub.l   $04BA.w,D2
                move.l  #-192,D1
                divs    D2,D1                       ;MHz
                and.w   #-2,D1
                ext.l   D1
                tst.b   tt_flag(A4)
                beq.s   cmd_sysinfo5
                moveq   #32,D1
cmd_sysinfo5:   cmp.w   #16,D1                      ;More than 16MHz?
                bhi.s   cmd_sysinfo6                ;Yes!=>
                cmp.w   #8,D1
                shs     D7                          ;D7 = $ FF if hypercache available, but out
                bhs.s   cmd_sysinfo6
                moveq   #8,D1                       ;min 8MHz
cmd_sysinfo6:   moveq   #10,D2
                bsr     numoutx
                pea     cmd_sysinfotxt7(PC)
                jsr     @print_line(A4)             ;" MHz"

                tst.b   tt_flag(A4)                 ;a TT?
                bne.s   cmd_sysinfo7                ;Yes!=> No speed
                tst.b   D7                          ;16MHz Speeder present?
                beq.s   cmd_sysinfo7                ;No!=>
                pea     cmd_sysinfotxt13(PC)
                jsr     @print_line(A4)             ;16MHz Speeder available!
cmd_sysinfo7:

                lea     $08.w,A3
                move    SR,D1
                moveq   #-1,D0
                movea.l SP,A2
                ori     #$0700,SR
                movea.l (A3),A1
                lea     cmd_sysinfo8(PC),A0
                move.l  A0,(A3)
                move.b  $FFFFFC7F.w,D0              ;PC-SPEED available?
cmd_sysinfo8:   move.l  A1,(A3)
                move    D1,SR
                movea.l A2,SP
                addq.b  #1,D0
                beq.s   cmd_sysinfo9                ;No!=>
                pea     cmd_sysinfotxt14(PC)
                jsr     @print_line(A4)             ;available!
cmd_sysinfo9:
                lea     cmd_sysinfo10(PC),A0
                move.l  A0,(A3)
                moveq   #0,D0
                move.b  $FFFF8E21.w,D0              ;Mega Ste present?
                moveq   #-1,D0
cmd_sysinfo10:  move.l  A1,(A3)
                move    D1,SR
                movea.l A2,SP
                tst.b   D0
                beq.s   cmd_sysinfo11
                pea     cmd_sysinfotxt11(PC)
                jsr     @print_line(A4)             ;available!

cmd_sysinfo11:  move    SR,D1
                moveq   #0,D0
                movea.l SP,A2
                ori     #$0700,SR
                movea.l (A3),A1
                lea     cmd_sysinfo12(PC),A0
                move.l  A0,(A3)
                tst.w   $FFFF8A00.w                 ;Blitter then?
                moveq   #-1,D0
cmd_sysinfo12:  move.l  A1,(A3)
                move    D1,SR
                movea.l A2,SP
                tst.w   D0
                beq.s   cmd_sysinfo13
                pea     cmd_sysinfotxt15(PC)
                jsr     @print_line(A4)             ;available!

cmd_sysinfo13:  tst.b   ste_flag(A4)                ;STE-HARDWARE?
                bne.s   cmd_sysinfo15               ;Yes!=> No IMP-MMU
                move    SR,D1
                moveq   #0,D0
                movea.l SP,A2
                ori     #$0700,SR
                movea.l (A3),A1
                lea     cmd_sysinfo14(PC),A0
                move.l  A0,(A3)
                move.b  $FFFF820F.w,D0              ;Read Ste-Register (IMP-MMU => Bus Error)
                moveq   #-1,D0
cmd_sysinfo14:  move.l  A1,(A3)
                move    D1,SR
                movea.l A2,SP
                tst.w   D0
                bne.s   cmd_sysinfo15
                pea     cmd_sysinfotxt16(PC)
                jsr     @print_line(A4)             ;available!

cmd_sysinfo15:  btst    #0,fpu_flag(A4)
                beq.s   cmd_sysinfo16
                pea     cmd_sysinfotxt17(PC)
                jsr     @print_line(A4)             ;available!

cmd_sysinfo16:  move.b  fpu_flag(A4),D0
                lsr.b   #1,D0
                beq.s   cmd_sysinfo17               ;No FPU =>
                cmp.b   #3,D0
                beq.s   cmd_sysinfo17               ;68040Fpu =>
                add.b   #'0',D0
                move.b  D0,-(SP)
                pea     cmd_sysinfotxt21(PC)
                jsr     @print_line(A4)             ;6888x available!
                move.b  (SP)+,D0
                jsr     @chrout(A4)
                jsr     @space(A4)
cmd_sysinfo17:
                lea     $FB0000,A0
                move.l  (A0),D0
                moveq   #99,D1
cmd_sysinfo18:  tst.l   (A0)
                dbra    D1,cmd_sysinfo18
                cmp.l   (A0),D0                     ;The TT has no stable bytes without module
                bne.s   cmd_sysinfo19               ;and!=>
                tst.w   $FA0000
                move.l  (A0),D0                     ;Spectre Cru-Test
                tst.w   $FA001C
                cmp.l   (A0),D0
                beq.s   cmd_sysinfo19
                pea     cmd_sysinfotxt18(PC)
                jsr     @print_line(A4)             ;available!

cmd_sysinfo19:  lea     stacy_tab(PC),A0
                move.w  $FFFF827E.w,D3              ;Register save
                moveq   #0,D2
                moveq   #5,D0                       ;6 Try out values
cmd_sysinfo20:  move.b  (A0)+,D2                    ;Get test value
                move.w  D2,$FFFF827E.w              ;Write value in the register
                moveq   #$0F,D1
                and.w   $FFFF827E.w,D1              ;Read out the register again
                cmp.b   D2,D1                       ;Is worth the value?
                dbne    D0,cmd_sysinfo20            ;Cancel, if not;otherwise =>
                bne.s   cmd_sysinfo21               ;Error, no Stacy =>
                move.w  D3,$FFFF827E.w              ;Register back again
                pea     cmd_sysinfotxt20(PC)
                jsr     @print_line(A4)             ;available!

cmd_sysinfo21:  tst.w   overscan(A4)                ;Overscan available?
                beq.s   cmd_sysinfo22
                pea     cmd_sysinfotxt19(PC)
                jsr     @print_line(A4)             ;available!

cmd_sysinfo22:  tst.b   tt_flag(A4)                 ;ttHardware?
                beq.s   cmd_sysinfo23               ;No!=>
                pea     cmd_sysinfotxt10(PC)
                jsr     @print_line(A4)             ;available!
                bra.s   cmd_sysinfo24

cmd_sysinfo23:  tst.b   ste_flag(A4)                ;STE-HARDWARE?
                beq.s   cmd_sysinfo24               ;No!=>
                pea     cmd_sysinfotxt9(PC)
                jsr     @print_line(A4)             ;available!

cmd_sysinfo24:  tst.b   tt_flag(A4)
                bne.s   cmd_sysinfo25
                pea     cmd_sysinfotxt(PC)
                jsr     @print_line(A4)             ;"Banks"
                moveq   #$03,D0
                and.b   $FFFF8001.w,D0
                bsr.s   bank_out
                moveq   #$0C,D0
                and.b   $FFFF8001.w,D0
                lsr.w   #2,D0
                bsr.s   bank_out
cmd_sysinfo25:  jsr     @c_eol(A4)
                jsr     @crout(A4)
                jmp     (A4)

bank_out:       cmp.b   #3,D0                       ;Unknown configuration?
                beq.s   bank_out1                   ;Yes!=>
                add.b   D0,D0
                addq.b  #7,D0
                moveq   #1,D1
                lsl.w   D0,D1                       ;Size of the bank
                moveq   #10,D2
                bsr     numoutx                     ;output
                moveq   #'k',D0
                bsr.s   cmd_syscout
                bra.s   bank_out2
bank_out1:      moveq   #'-',D0
                bsr.s   cmd_syscout                 ;unknown value
bank_out2:      moveq   #' ',D0
                bra.s   cmd_syscout

cmd_sysinfo_sub:moveq   #'.',D0                     ;"."output
cmd_syscout:    jmp     @chrout(A4)

stacy_tab:      DC.B 1,2,4,8,5,10

                SWITCH language
                CASE 0
cmd_sysinfotxt1:DC.B 'TOS-Version    : ',0
cmd_sysinfotxt2:DC.B ' vom ',0
cmd_sysinfotxt3:DC.B 13,'GEMDOS-Version : ',0
cmd_sysinfotxt4:DC.B 13,'AES-Version    : ',0
cmd_sysinfotxt5:DC.B 13,'VDI-Version    : GDOS ist ',0
cmd_sysinfotxt6:DC.B 'vorhanden',13
                DC.B 'Taktfrequenz   : ',0
cmd_sysinfotxt7:DC.B ' MHz',13
                DC.B 'zus. Hardware  : ',0
cmd_sysinfotxt8:DC.B 'nicht ',0
cmd_sysinfotxt: DC.B 13,'Banks          : ',0
cmd_sysinfotxt9:DC.B 'STE-Hardware ',0
cmd_sysinfotxt10:DC.B 'TT-Hardware ',0
cmd_sysinfotxt11:DC.B 'Mega STE-hardware ',0
cmd_sysinfotxt12:DC.B 13,'OS-Basisadresse: ',0

                CASE 1
cmd_sysinfotxt1:DC.B 'TOS-version    : ',0
cmd_sysinfotxt2:DC.B ' date ',0
cmd_sysinfotxt3:DC.B 13,'GEMDOS-version : ',0
cmd_sysinfotxt4:DC.B 13,'AES-version    : ',0
cmd_sysinfotxt5:DC.B 13,'VDI-version    : GDOS ',0
cmd_sysinfotxt6:DC.B 'loaded.',13
                DC.B 'clock          : ',0
cmd_sysinfotxt7:DC.B ' MHz',13
                DC.B 'hardware       : ',0
cmd_sysinfotxt8:DC.B 'not ',0
cmd_sysinfotxt: DC.B 13,'Banks          : ',0
cmd_sysinfotxt9:DC.B 'STE-hardware ',0
cmd_sysinfotxt10:DC.B 'TT-hardware ',0
cmd_sysinfotxt11:DC.B 'Mega STE-hardware ',0
cmd_sysinfotxt12:DC.B 13,'OS-Baseadr     : ',0
                ENDS

cmd_sysinfotxt13:DC.B '16MHz-Speeder ',0
cmd_sysinfotxt14:DC.B 'PC-Speed ',0
cmd_sysinfotxt15:DC.B 'Blitter ',0
cmd_sysinfotxt16:DC.B 'IMP-MMU ',0
cmd_sysinfotxt17:DC.B 'SFP004 ',0
cmd_sysinfotxt18:DC.B 'Spectre-GCR ',0
cmd_sysinfotxt19:DC.B 'AS-OverScan ',0
cmd_sysinfotxt20:DC.B 'Stacy ',0
cmd_sysinfotxt21:DC.B '6888',0
cmd_sysinfotab1:DC.B 'NTSC',0                       ;0
                DC.B 'PAL',0                        ;1
                DXSET 4,0
cmd_sysinfotab2:DX.B 'USA'                          ;0
                DX.B 'FRG'                          ;1
                DX.B 'FRA'                          ;2
                DX.B 'UK'                           ;3
                DX.B 'SPA'                          ;4
                DX.B 'ITA'                          ;5
                DX.B 'SWE'                          ;6
                DX.B 'SWF'                          ;7
                DX.B 'SWG'                          ;8
                DX.B 'TUR'                          ;9
                DX.B 'FIN'                          ;10
                DX.B 'NOR'                          ;11
                DX.B 'DEN'                          ;12
                DX.B 'SAU'                          ;13
                DX.B 'HOL'                          ;14
                DX.B '???'                          ;>14
                EVEN
                ENDPART

********************************************************************************
* My own little AES call                                                       *
********************************************************************************
                PART 'aes'
aes:            movem.l D0-A6,-(SP)                 ;Save better, you can never know
                lea     spaced2(A4),A0
                clr.l   (A0)+
                clr.l   (A0)                        ;Delete Contrl Array
                movep.l D0,-3(A0)                   ;and enter the new data
                lea     aes_pb(PC),A0
                move.l  A0,D1
aes1:           move.l  A4,D0                       ;relozierenDesArrays
                add.l   D0,(A0)+
                add.l   D0,(A0)+
                add.l   D0,(A0)+
                add.l   D0,(A0)+
                add.l   D0,(A0)
                lea     aes1(PC),A0
                move.w  #$7000,(A0)                 ;MOVEQ #0,D0 insert
                bsr     clr_cache
                move.w  #200,D0
                trap    #2                          ;AES call up
                movem.l (SP)+,D0-A6
                rts

aes_pb:         DC.L spaced2                        ;The AES parameter block
                DC.L spaced2+32                     ;Global-Parameter
                DC.L spaced2+32+30
                DC.L spaced2+32+30
                DC.L spaced2+32+30
                DC.L spaced2+32+30
                ENDPART

********************************************************************************
* 'INFO' - Information about the memory assignment                             *
********************************************************************************
                PART 'cmd_info'
cmd_info:       lea     cmd_info2(PC),A0
                bsr     print_info
                move.l  basepage(A4),D1
                bsr     hexa2out
                jsr     @c_eol(A4)
                lea     cmd_info3(PC),A0
                bsr     print_info
                move.l  end_adr(A4),D1
                bsr     hexa2out
                jsr     @c_eol(A4)
                lea     cmd_info4(PC),A0
                bsr     print_info
                move.l  first_free(A4),D1
                bsr     hexa2out
                jsr     @c_eol(A4)
                lea     cmd_info7(PC),A0
                bsr     print_info
                move.l  save_data+1070(A4),D1
                bsr     hexa2out
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                move.l  basep(A4),D0
                beq.s   cmd_info1                   ;No Exec program there
                movea.l D0,A1
                bsr     prg_info
                jmp     (A4)                        ;that's it
cmd_info1:      move.l  merk_anf(A4),D1
                beq     ret_jump                    ;No load program there
                lea     cmd_info5(PC),A0
                bsr     print_info
                bsr     hexa2out
                jsr     @c_eol(A4)
                lea     cmd_info6(PC),A0
                bsr     print_info
                move.l  merk_end(A4),D1
                subq.l  #1,D1
                bsr     hexa2out
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                jmp     (A4)

                SWITCH language
                CASE 0
cmd_info2:      DC.B 'Start des Debuggers',0
cmd_info3:      DC.B 13,'Ende des Debuggers',0
cmd_info4:      DC.B 13,'Start des freien Speichers',0
cmd_info5:      DC.B 'Start des Programms',0
cmd_info6:      DC.B 13,'Ende des Programms',0
cmd_info7:      DC.B 13,'Ende des freien Speichers',0

                CASE 1
cmd_info2:      DC.B 'Start of the debugger',0
cmd_info3:      DC.B 13,'End of the debugger',0
cmd_info4:      DC.B 13,'Start of free memory',0
cmd_info5:      DC.B 'Start of program',0
cmd_info6:      DC.B 13,'End of program',0
cmd_info7:      DC.B 13,'End of free memory',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* A0 output, spaces to column 45 and a colon follow                            *
********************************************************************************
                PART 'print_info'
print_info:     move.l  A0,-(SP)
                jsr     @print_line(A4)
                moveq   #27,D0
                bsr     spacetab
                moveq   #':',D0
                jmp     @chrout(A4)
                ENDPART

********************************************************************************
* Information about the current program                                        *
********************************************************************************
                PART 'prg_info'
prg_info:       movea.l basep(A4),A1                ;BasePageAdr of the program
                lea     prg_info9(PC),A0            ;Initial address of the text segment
                bsr.s   print_info
                moveq   #8,D2
                bsr     prg_info8                   ;Long from text segments
                lea     prg_info10(PC),A0           ;Initial address of the data segment
                bsr.s   print_info
                moveq   #$10,D2
                bsr     prg_info8                   ;Long from data segments
                lea     prg_info11(PC),A0           ;Initial address of the BSS segment
                bsr.s   print_info
                moveq   #$18,D2
                bsr     prg_info8                   ;Long from bss segments
                move.l  sym_size(A4),D2
                beq.s   prg_info3
                lea     prg_info12(PC),A0
                tst.b   gst_sym_flag(A4)
                beq.s   prg_info1
                lea     prg_info13(PC),A0
prg_info1:      bsr.s   print_info
                moveq   #14,D1                      ;An entry is 14 bytes long
                bsr     ldiv
                move.l  D2,D1
                moveq   #' ',D0                     ;singular
                subq.l  #1,D1
                beq.s   prg_info2
                moveq   #'e',D0                     ;Plural image.
prg_info2:      move.b  D0,prg_info17
                addq.l  #1,D1
                moveq   #10,D2
                bsr     numoutx                     ;Decimal
                pea     prg_info16(PC)
                jsr     @print_line(A4)
                jsr     @c_eol(A4)
prg_info3:      lea     prg_info14(PC),A0           ;last used address
                bsr.s   print_info
                move.l  $18(A1),D1
                add.l   $1C(A1),D1                  ;gives first free address
                bsr     hexa2out
                jsr     @c_eol(A4)
                btst    #0,prg_flags+3(A4)          ;fastLoad?
                beq.s   prg_info4                   ;No!=>
                lea     prg_info18(PC),A0
                bsr.s   prg_info7
prg_info4:      btst    #1,prg_flags+3(A4)          ;prgInFastRam?
                beq.s   prg_info5                   ;No!=>
                moveq   #0,D1
                move.b  prg_flags(A4),D1
                lsr.w   #4,D1
                addq.b  #1,D1
                lsl.w   #7,D1
                st      testwrd(A4)
                lea     prg_info20(PC),A0
                bsr     dezout
                sf      testwrd(A4)
                move.b  #'K',(A0)+
                clr.b   (A0)
                lea     prg_info19(PC),A0
                bsr.s   prg_info7
prg_info5:      btst    #2,prg_flags+3(A4)          ;fastRamMalloc()?
                beq.s   prg_info6                   ;No!=>
                lea     prg_info21(PC),A0
                bsr.s   prg_info7
prg_info6:      jmp     @crout(A4)

prg_info7:      move.l  A0,-(SP)
                jsr     @crout(A4)
                jsr     @print_line(A4)             ;e.g. issue "fast-load on"
                jmp     @c_eol(A4)

prg_info8:      move.l  0(A1,D2.w),D1               ;Get initial address
                bsr     hexa2out                    ;and spend
                pea     prg_info15(PC)
                jsr     @print_line(A4)
                move.l  4(A1,D2.w),D1               ;Length
                bsr     hexlout                     ;and spend
                jmp     @c_eol(A4)

                SWITCH language
                CASE 0
prg_info9:      DC.B 'Start des TEXT-Segments',0
prg_info10:     DC.B 13,'Start des DATA-Segments',0
prg_info11:     DC.B 13,'Start des BSS-Segments',0
prg_info12:     DC.B 13,'Symboltabelle',0
prg_info13:     DC.B 13,'GST-Symboltabelle',0
prg_info14:     DC.B 13,'Erste freie Adresse',0
prg_info15:     DC.B '  Länge:$',0
prg_info16:     DC.B ' Symbol'
prg_info17:     DC.B 'e',0
prg_info18:     DC.B 'Fast-Load: Nur das BSS-Segment wird gelöscht',0
prg_info19:     DC.B 'auch ins TT-Fast-RAM ladbar, TPAsize = '
prg_info20:     DC.B 'xxxxx',0
prg_info21:     DC.B 'Malloc() auch ins TT-Fast-RAM',0

                CASE 1
prg_info9:      DC.B 'Start of TEXT segment',0
prg_info10:     DC.B 13,'Start of DATA segment',0
prg_info11:     DC.B 13,'Start of BSS segment',0
prg_info12:     DC.B 13,'Symbol table',0
prg_info13:     DC.B 13,'GST-Symbol table',0
prg_info14:     DC.B 13,'first free address',0
prg_info15:     DC.B ' length:$',0
prg_info16:     DC.B ' symbol'
prg_info17:     DC.B 's',0
prg_info18:     DC.B "Fast-Load set: clear only the program's declared BSS",0
prg_info19:     DC.B 'loading to TT-Fast-RAM possible, TPAsize = '
prg_info20:     DC.B 'xxxxx',0
prg_info21:     DC.B 'Malloc() also to TT-Fast-RAM',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* Disassembler (Disassemble opcode from A6)                                    *
********************************************************************************
                PART 'do_disass'
do_disass:      bsr     check_read                  ;Access granted?
                bne     _return                     ;End, if not
                lea     spaced2(A4),A0
                movea.l A0,A5
                st      testwrd(A4)
                move.l  A6,D1
                addq.l  #1,D1

                andi.b  #$FE,D1
                movea.l D1,A6
                jsr     @anf_adr(A4)
                tst.b   list_flg(A4)
                beq.s   do_disass6                  ;Non-symbolic
                move.b  #'!',(A0)+                  ;Identification for opcode
                bsr     hunt_symbol
                beq.s   do_disass2                  ;Z = 1 => No label
                bsr     labelout
                moveq   #':',D0
                btst    #5,8(A1)                    ;global?
                beq.s   do_disass1
                move.b  D0,(A0)+                    ;Then spend a colon for global
do_disass1:     move.b  D0,(A0)+
                lea     26(A5),A2
                cmpa.l  A2,A0
                bhs.s   do_disass4                  ;Tab already achieved!
do_disass2:     pea     26(A5)
do_disass3:     move.b  #' ',(A0)+                  ;Tab to 25
                cmpa.l  (SP),A0
                blo.s   do_disass3
                addq.l  #4,SP
do_disass4:     movem.l A0/A3,-(SP)
                bsr     disass
                clr.b   testwrd(A4)                 ;Output no longer in the buffer
                movem.l (SP)+,A0/A3
                lea     spaced(A4),A1
do_disass5:     move.b  (A1)+,(A0)+
                bne.s   do_disass5
                lea     spaced2(A4),A0
                move.w  zeile(A4),D0
                bsr     write_line                  ;Output the result of the disassembler
                move    #$FF,CCR
                rts

do_disass6:     move.b  #'/',(A0)+
                movem.l A3/A5-A6,-(SP)
                bsr     get_dlen                    ;Get length of opcodes
                movem.l (SP)+,A3/A5-A6
                movea.l A6,A1
                moveq   #12,D3
                cmp.w   D3,D0
                bhs.s   do_disass8
                move.w  D0,D3
                bra.s   do_disass8
do_disass7:     move.b  #',',(A0)+                  ;Output opcode in hex
do_disass8:     move.w  (A1)+,D1
                bsr     hexwout
                subq.w  #2,D3
                bne.s   do_disass7
                pea     34(A5)
do_disass9:     move.b  #' ',(A0)+                  ;Tab to 33
                cmpa.l  (SP),A0
                blo.s   do_disass9
                addq.l  #4,SP
                move.b  #';',(A0)+
                move.b  #' ',(A0)+
                move.l  D7,-(SP)
                move.l  sym_size(A4),D7
                clr.l   sym_size(A4)                ;disassemble without symbol table
                movem.l D7-A0/A3,-(SP)
                bsr     disass
                clr.b   testwrd(A4)                 ;Output no longer in the buffer
                movem.l (SP)+,D7-A0/A3
                move.l  D7,sym_size(A4)
                move.l  (SP)+,D7
                lea     spaced(A4),A1
                bra.s   do_disass5
                ENDPART

********************************************************************************
* Get parameters for Go and Call                                               *
********************************************************************************
                PART 'get_gopars'
get_gopars:     bsr     get_parameter               ;Get parameters
                move    SR,D0
                bcc.s   get_gopars1                 ;1.Parameter specified
                movea.l _pc(A4),A2                  ;Then jump to the current pc
get_gopars1:    move    D0,CCR
                bvs.s   get_gopars2                 ;No 2nd parameter
                move.l  A3,D0                       ;zero (then only G,)
                beq.s   get_gopars3                 ;Leave old break # 16
                cmpa.l  #$0400,A3                   ;Self address <$ 400?
                blo     illequa
                move.l  A3,breakpnt+12*16(A4)       ;Address
                move.w  #-1,breakpnt+12*16+4(A4)    ;stopBreakpoint
                move.l  #1,breakpnt+12*16+6(A4)     ;run only once
                bra.s   get_gopars3
get_gopars2:    clr.l   breakpnt+12*16(A4)          ;Break # 16 Delete
get_gopars3:    move.l  A2,_pc(A4)                  ;Set
                rts
                ENDPART

********************************************************************************
* 'CALL' - Call Subroutine                                                     *
********************************************************************************
                PART 'cmd_call'
cmd_call:       tst.b   (A0)                        ;Only C?
                bne.s   cmd_call2                   ;No, parameters come
cmd_call1:      movea.l _pc(A4),A6                  ;Determine command length on the PC
                bsr     get_dlen
                move.l  A6,breakpnt+12*16(A4)       ;Break # 16 put
                move.w  #-1,breakpnt+12*16+4(A4)    ;stopBreakpoint
                clr.l   breakpnt+12*16+6(A4)        ;run only once
                bra.s   go_pc

cmd_call2:      bsr.s   get_gopars                  ;Parameters Get etc.
                movea.l _usp(A4),A0
                movea.l _ssp(A4),A1
                lea     loginc(PC),A2
                btst    #5,_sr(A4)                  ;User- order Supervisor-Stack?
                bne.s   cmd_call3
                move.l  A2,-(A0)                    ;Return account on the user stack
                bra.s   cmd_call4
cmd_call3:      move.l  A2,-(A1)                    ;Return account on the supervisor stack
cmd_call4:      move.l  A1,_ssp(A4)
                move.l  A0,_usp(A4)
                move.l  _pc(A4),merk_pc_call(A4)
                bra.s   go_pc                       ;Breakpoint on the PC?
                ENDPART

********************************************************************************
* 'GO' - Start program                                                         *
********************************************************************************
                PART 'cmd_go'
cmd_go:         bsr     get_gopars                  ;Parameters Get etc.
                ENDPART

********************************************************************************
* Run program from the PC, including init                                      *
********************************************************************************
                PART 'go_pc'
go_pc:          bsr     init_trace                  ;Prepare everything for the program
                bsr     breakset                    ;Insert breakpoints (or test a PC)
                movea.l _usp(A4),A0
                move    A0,USP                      ;Use usp
                movea.l _ssp(A4),SP                 ;SSP set
                tst.b   prozessor(A4)               ;68000?
                bmi.s   go_pc1                      ;yes!=>
                clr.w   -(SP)                       ;68010 or 68020 requires a word more
go_pc1:         movea.l _pc(A4),A0
                move.l  A0,-(SP)                    ;PC on the stack
                move.w  _sr(A4),-(SP)               ;Flags on the stack
                tst.w   D7                          ;Was a breakpoint on the PC?
                bne.s   go_pc2                      ;No!=> Start
                move.l  A0,go_pc12+4                ;PC notice
                move.w  (A0),10(A6)                 ;Command on the PC already remember in the BreakPoint
                move.l  $24.w,go_pc11+2             ;Trace vector mark
                bsr     clr_cache
                move.l  #go_pc10,$24.w              ;Own trace routine pure
                bset    #7,(SP)                     ;traceAn
go_pc2:         movem.l regs(A4),D0-A6
                bset    #7,$FFFFFA07.w              ;Ring-indicator an
                rte                                 ;command
go_pc10:        bclr    #7,(SP)                     ;Trace again
go_pc11:        move.l  #0,$24.w                    ;Old trace vector back
go_pc12:        move.w  #$4AFC,$01234567            ;Breakpoint now insert
                bsr     clr_cache
                rte
                ENDPART

********************************************************************************
* 'IF' - Quit if                                                               *
********************************************************************************
                PART 'cmd_if'
cmd_if:         movea.l A0,A1
                bsr     get
                beq.s   cmd_if2                     ;Output function
                movea.l A1,A0
                lea     untrace_funk(A4),A2
                moveq   #79,D0
cmd_if1:        move.b  (A1)+,(A2)+                 ;Remember user-trace-function
                dbra    D0,cmd_if1
                movea.l #user_trace_buf,A1
                adda.l  A4,A1
                bsr     convert_formel
                bsr     clr_cache
                jmp     (A4)

cmd_if2:        move.l  default_adr(A4),D1
                jsr     @anf_adr(A4)
                moveq   #'I',D0
                jsr     @chrout(A4)
                moveq   #'F',D0
                jsr     @chrout(A4)
                lea     untrace_funk(A4),A1
                cmpi.b  #' ',(A1)
                beq.s   cmd_if3
                jsr     @space(A4)
cmd_if3:        move.b  (A1)+,D0
                beq.s   cmd_if4
                jsr     @chrout(A4)
                bra.s   cmd_if3
cmd_if4:        jsr     @c_eol(A4)
                jsr     @crout(A4)
                jmp     (A4)
                ENDPART

********************************************************************************
* 'UNTRACE' - Untrace                                                          *
********************************************************************************
                PART 'cmd_untrace'
cmd_untrace:    moveq   #-1,D1                      ;Untrace counter on "endless"
                bsr     get
                beq.s   cmd_untrace1                ;start with the tracen, because no parameters
                bsr     get_term                    ;Bring the Untrace Counter
cmd_untrace1:   move.l  D1,untrace_count(A4)
                st      untrace_flag(A4)
                bra.s   cmd_trace2                  ;And off the post ...
                ENDPART

********************************************************************************
* 'TRACE' - Program Trace                                                      *
********************************************************************************
                PART 'cmd_trace'
cmd_trace:      moveq   #1,D1                       ;Delete TraceCount
                bsr     get
                beq.s   cmd_trace1                  ;Trace without parameters
                bsr     get_term
cmd_trace1:     move.l  D1,trace_count(A4)          ;Number of commands to trace
cmd_trace2:     bsr     init_trace                  ;Put on
                bsr     breakset                    ;Set breakpoints
                tst.w   D7                          ;Breakpoint on the PC?
                bne.s   cmd_trace3                  ;No!=>
                movea.l _pc(A4),A0
                move.w  (A0),10(A6)                 ;Get content from the PC and remember
                move.w  #$4AFC,(A0)                 ;Illegally insert
                bsr     clr_cache
cmd_trace3:     andi    #$FB00,SR                   ;Free IRQS
cmd_trace4:     ori.w   #$8000,_sr(A4)              ;traceAn
                lea     trace_excep(PC),A0
                move.l  A0,$24.w
                bsr     in_trace_buff               ;registerInDenTraceBuffer
                movea.l _usp(A4),A0
                move    A0,USP                      ;Use usp
                movea.l _ssp(A4),SP                 ;SSP set
                movea.l _pc(A4),A0                  ;For trap, line-a / f 'a special treatment
                move.w  (A0),D0                     ;To Traceender command
                and.w   #$FFF0,D0                   ;Trap mask
                cmp.w   #$4E40,D0                   ;trap?
                beq.s   cmd_trace7                  ;yes, step directly
                and.w   #$F000,D0                   ;lineA/fMaske
                cmp.w   #$A000,D0                   ;Line-in muoses throwing throws, then
                beq.s   cmd_trace5                  ;otherwise the next command is not drinked
                tst.b   prozessor(A4)               ;68000?
                bmi.s   cmd_trace41                 ;yes!=>
                clr.w   -(SP)                       ;nullFormatWord
cmd_trace41:    move.l  A0,-(SP)                    ;PC on the stack
                move.w  _sr(A4),-(SP)               ;Flags on the stack
                movem.l regs(A4),D0-A6
                rte                                 ;command

cmd_trace5:     lea     cmd_trace6(PC),A1           ;Run Linea (Leef does not work !!!)
                move.w  (A0)+,(A1)                  ;Copy up
                move.l  A0,4(A1)                    ;Set PC + 2 as a return address
                bsr     clr_cache
                move.w  _sr(A4),save_a4
                movem.l regs(A4),D0-A6
                move    save_a4(PC),SR              ;Get SR from the stack
cmd_trace6:     nop                                 ;Place for the opcode
                jmp     $56781234                   ;jumpBack

cmd_trace7:     lea     cmd_trace8+2(PC),A1
                moveq   #$0F,D0
                and.w   (A0)+,D0                    ;Get opcode (from the PC)
                asl.w   #2,D0
                lea     $80.w,A2                    ;Basic Address There Traps
                move.l  0(A2,D0.w),(A1)             ;Insert behind the JMP
                move.l  A0,6(A1)                    ;Use PC + 2 in the 2nd jump
                bsr     clr_cache
                tst.b   prozessor(A4)               ;68000?
                bmi.s   cmd_trace71                 ;yes!=>
                clr.w   -(SP)                       ;nullFormatWord
cmd_trace71:    pea     cmd_trace9(PC)              ;Here's back
                move.w  _sr(A4),D0                  ;Sr get (Trace is!)
                move.w  D0,-(SP)                    ;Sr back to the stack (with trace!)
                or.w    #$2000,D0                   ;SSP set
                and.w   #$7FFF,D0                   ;Trace out
                move    D0,SR                       ;Flag
                movem.l regs(A4),D0-A6
cmd_trace8:     jmp     $56781234                   ;and from in the trap
cmd_trace9:     jmp     $56781234                   ;Jump to the old PC
                ENDPART

********************************************************************************
* The Trace-Exception                                                          *
********************************************************************************
                PART 'trace_exception'
save_a4:        DS.L 1

trace_excep:    move    #$2700,SR                   ;Block all IRQS
                bclr    #7,$FFFFFA07.w              ;Ring indicator
                move.l  A4,save_a4
                lea     varbase,A4
                movem.l D0-A6,regs(A4)              ;All registry save
                move.l  save_a4(PC),regs+48(A4)     ;Now save A4
                move.w  (SP)+,_sr(A4)
                move.l  (SP)+,_pc(A4)
                tst.b   prozessor(A4)               ;68000?
                bmi.s   trace_excep1                ;yes!=>
                addq.l  #6,SP                       ;vectorOffset +PcVerwerfen
trace_excep1:   move    USP,A0
                move.l  A0,_usp(A4)                 ;USP notice
                move.l  SP,_ssp(A4)                 ;Ssp notice
                movea.l default_stk(A4),SP          ;Restore your own stack
                tst.b   untrace_flag(A4)            ;Untrace activated?
                bne.s   trace_excep4                ;Then there's something to do
                subq.l  #1,trace_count(A4)          ;Trace counter already expired?
                bhi     cmd_trace4                  ;No, Next Tracen
trace_excep2:   bsr     breakclr                    ;Remove breakpoints
                bsr     do_vbl                      ;Perform open VBL tasks
                bclr    #7,_sr(A4)                  ;Delete Tracebit
                movea.l _usp(A4),A0
                btst    #5,_sr(A4)                  ;userModeAn?
                beq.s   trace_excep3
                movea.l SP,A0                       ;No, supervisor mode
trace_excep3:   move.l  A0,rega7(A4)                ;Put in7 volunteering this sr
                move.l  _pc(A4),default_adr(A4)     ;Set default address
                jsr     @page1(A4)
                jsr     @my_driver(A4)              ;Own keyboard driver
                bsr     update_pc
                bsr     set_reg                     ;Copy register set
                andi    #$FB00,SR                   ;Free IRQS
                move.l  jmpdispa(A4),-(SP)
                rts
trace_excep4:   subq.l  #1,untrace_count(A4)
                beq.s   trace_excep2                ;Untrace counter expired
                lea     regs(A4),A1                 ;Transfer parameters to user trace
                movea.l #user_trace_buf,A0
                adda.l  A4,A0
                jsr     (A0)                        ;userTraceRoutineAufrufen
                bne.s   trace_excep2                ;Demolition desired
                bra     cmd_trace4                  ;Weiter Tracen
                ENDPART

********************************************************************************
* Start trace (set all parameters, switch drivers)                             *
********************************************************************************
                PART 'init_trace'
init_trace:     movem.l D0-A6,-(SP)
                movea.l _pc(A4),A6                  ;Hot PC
                bsr     check_read                  ;Is the PC at a valid address?
                bne     intern_bus                  ;Cancel, if not
                cmpa.l  #anfang-256,A6
                blo.s   init_trace1                 ;Is the PC in the debugger?
                suba.l  A4,A6
                cmpa.l  #data_buff+8,A6             ;| Execute command
                beq.s   init_trace1                 ;That's allowed!
                cmpa.l  #sekbuff,A6
                blo     ill_mem                     ;"Illegal storage area" (in the debugger)
init_trace1:    bsr     set_buserror                ;Original bus error vector back
                jsr     @org_driver(A4)             ;Original keyboard driver pure
                movea.l kbshift_adr(A4),A0
                andi.b  #$10,(A0)                   ;Delete KBShift status, not caps
                movem.l (SP)+,D0-A6
                jmp     @page2(A4)                  ;originalscreenAn
                ENDPART

********************************************************************************
* Stop trace                                                                   *
********************************************************************************
                PART 'exit_trace'
exit_trace:     movem.l D0-A6,-(SP)
                jsr     @page1(A4)
                bsr     update_pc
                bsr.s   set_reg                     ;Register set in the current
                bsr     breakclr                    ;Breakpoints out again
                jsr     @my_driver(A4)              ;Own drivers back in
                andi    #$FB00,SR                   ;Free IRQS
                movem.l (SP)+,D0-A6
                rts
                ENDPART

********************************************************************************
* Trace buffer end with akt. Supply registers                                  *
********************************************************************************
                PART 'set_reg'
set_reg:        movem.l D0/A5-A6,-(SP)
                lea     regs(A4),A5
                movea.l trace_pos(A4),A6
                moveq   #38,D0
set_reg1:       move.w  (A5)+,(A6)+                 ;leave a "track"
                dbra    D0,set_reg1
                movem.l (SP)+,D0/A5-A6
                rts
                ENDPART

********************************************************************************
* act. Registers in the trace buffer                                             *
********************************************************************************
                PART 'in_trace_buff'
in_trace_buff:  movea.l #trace_buffend,A1
                adda.l  A4,A1
                lea     regs(A4),A5
                movea.l trace_pos(A4),A6
                moveq   #38,D0
in_trace_buff1: move.w  (A5)+,(A6)+                 ;leave a "track"
                dbra    D0,in_trace_buff1
                cmpa.l  A1,A6
                blo.s   in_trace_buff2
                movea.l #trace_buff,A6              ;Pointer again on the beginning
                adda.l  A4,A6
in_trace_buff2: move.l  A6,trace_pos(A4)
                move.l  A6,reg_pos(A4)
                rts
                ENDPART

********************************************************************************
* Run a command on the PC (without treatment of Linea & Traps)                 *
********************************************************************************
                PART 'do_trace_all'
do_trace_all:   movem.l D0-A6,-(SP)
                move.l  SP,_regsav2(A4)
                bsr     breakset                    ;Set breakpoints
                tst.w   D7
                bne.s   do_trace_all1
                movea.l _pc(A4),A0
                move.w  (A0),10(A6)                 ;Get content from the PC and remember
                move.w  #$4AFC,(A0)                 ;Illegally insert
do_trace_all1:  lea     do_trace_excep(PC),A0
                move.l  A0,$24.w
                bsr.s   in_trace_buff               ;registerInDenTraceBuffer
                movea.l _ssp(A4),SP                 ;Take
                tst.b   prozessor(A4)               ;68000?
                bmi.s   do_trace_all2               ;yes!=>
                clr.w   -(SP)                       ;68010 or 68020 requires a word more
do_trace_all2:  move.l  _pc(A4),-(SP)               ;The PC must on'm the stack (start with RTE)
                move.w  _sr(A4),-(SP)               ;Status register already on the stack
                bset    #7,(SP)                     ;Set Tracebit
                movea.l _usp(A4),A0
                move    A0,USP                      ;Use usp
                movem.l regs(A4),D0-A6
                rte                                 ;Routine start
                ENDPART

********************************************************************************
* Run a command on the PC                                                      *
********************************************************************************
                PART 'do_trace'
do_trace:       lea     do_trace_excep(PC),A0
                move.l  A0,$24.w
do_trace1:      movem.l D0-A6,-(SP)
                move.l  SP,_regsav2(A4)
                bsr     breakset                    ;Set breakpoints
                tst.w   D7
                bne.s   do_trace2
                movea.l _pc(A4),A0
                move.w  (A0),10(A6)                 ;Get content from the PC and remember
                move.w  #$4AFC,(A0)                 ;Illegally insert
                bsr     clr_cache
do_trace2:      bsr     in_trace_buff               ;registerInDenTraceBuffer
                movea.l _ssp(A4),SP                 ;Take
                tst.b   prozessor(A4)               ;68000?
                bmi.s   do_trace3                   ;yes!=>
                clr.w   -(SP)                       ;68010 or 68020 requires a word more
do_trace3:      move.l  _pc(A4),-(SP)               ;The PC must on'm the stack (start with RTE)
                move.w  _sr(A4),-(SP)               ;Status register already on the stack
                bset    #7,(SP)                     ;Set Tracebit
                movea.l _usp(A4),A0
                move    A0,USP                      ;Use usp
                movea.l _pc(A4),A0                  ;For trap, line-a / f 'a special treatment
                move.w  (A0),D0                     ;To Traceender command
                and.w   #$FFF0,D0                   ;Trap mask
                cmp.w   #$4E40,D0                   ;trap?
                beq.s   do_trace7                   ;yes, step directly
                and.w   #$F000,D0                   ;lineAMaske
                cmp.w   #$A000,D0                   ;Line-A must be patched, there
                beq.s   do_trace4                   ;otherwise the next command is not tackled
                movem.l regs(A4),D0-A6
                rte                                 ;Routine start

do_trace4:      lea     do_trace6(PC),A1

                move.w  (A0)+,(A1)                  ;Copy up
                addq.l  #6,SP
                tst.b   prozessor(A4)               ;68000?
                bmi.s   do_trace5                   ;and!=>
                addq.l  #2,SP                       ;Format Word
do_trace5:      move.l  A0,4(A1)                    ;So set as a return address
                bsr     clr_cache
                move.w  _sr(A4),-(SP)
                bset    #7,(SP)                     ;Set Tracebit
                movem.l regs(A4),D0-A6
                move    (SP)+,SR                    ;Get SR from the stack
do_trace6:      nop                                 ;Place for the opcode
                jmp     $56781234                   ;jumpBack

do_trace7:      lea     do_trace8+2(PC),A1
                moveq   #$0F,D0
                and.w   (A0)+,D0                    ;opcodeHolen
                asl.w   #2,D0
                lea     $80.w,A2                    ;Basic Address There Traps
                move.l  0(A2,D0.w),(A1)             ;jmp
                move.w  (SP)+,D0                    ;Get SR from the stack (Trace already!)
                addq.l  #2,(SP)                     ;pc+2
                move.l  (SP)+,6(A1)                 ;Insert PC into the 2nd jump
                bsr     clr_cache
                pea     do_trace9(PC)               ;Here's back
                move.w  D0,-(SP)                    ;Sr back to the stack (with trace!)
                or.w    #$2000,D0                   ;SSP set
                and.w   #$7FFF,D0                   ;Trace out
                move    D0,SR                       ;Flag
                movem.l regs(A4),D0-A6
do_trace8:      jmp     $56781234                   ;and from in the trap
do_trace9:      jmp     $56781234                   ;Jump to the old PC

do_trace_excep: ori     #$0700,SR                   ;alleIrQsCanceln
                move.l  A4,save_a4
                lea     varbase,A4
                movem.l D0-A6,regs(A4)              ;All registry save
                move.l  save_a4(PC),regs+48(A4)     ;Now save A4
                move.w  (SP)+,_sr(A4)               ;statusregister
                bclr    #7,_sr(A4)                  ;Delete Tracebit
                move.l  (SP)+,_pc(A4)               ;PC (SP Nun weather normal!)
                tst.b   prozessor(A4)               ;68000 or 68010?
                bmi.s   do_trace_excep1             ;yes!=>
                addq.l  #6,SP                       ;68010 or 68020 has a word + PC more
do_trace_excep1:move.l  _pc(A4),default_adr(A4)     ;pc =Defaultadr
                move    USP,A2
                move.l  A2,_usp(A4)                 ;USP notice
                move.l  SP,_ssp(A4)                 ;Ssp notice
                move.l  SP,rega7(A4)                ;As A7 notice
                btst    #5,_sr(A4)                  ;fallsDieRoutineImUserModeWar,
                bne.s   do_trace_excep2
                move.l  _usp(A4),rega7(A4)          ;Den USP as A7 notice
do_trace_excep2:movea.l _regsav2(A4),SP             ;Restore stack
                bsr     do_vbl                      ;Perform open VBL tasks
                bra     f_trac1
                ENDPART

********************************************************************************
* Command 'B': Handle Breakpoints                                              *
********************************************************************************
                PART 'cmd_bkpt'
cmd_bkpt:       bsr     get                         ;Read sign behind B
                lea     breakpnt(A4),A1
                cmp.b   #'K',D0                     ;breakpointClear?
                beq     cmd_bkpt10
                tst.b   D0                          ;breakpointList
                beq     cmd_bkpt14
                bsr     get_term                    ;alsoBreakSet
                cmp.l   #16,D1
                bhs     illequa                     ;> 15, is not there
                move.w  D1,-(SP)                    ;Break-no. Brands
                tst.b   D0
                beq     cmd_bkpt13                  ;Output each breakpoint
                cmp.b   #'=',D0
                bne     synerr                      ;BN = ADR
                bsr     get
                beq     synerr                      ;Does nothing come anymore
                bsr     get_term                    ;valueHolen!
                move.w  (SP)+,D3                    ;breakNrHolen
                move.w  D3,D7                       ;Number Brands
                addq.l  #1,D1
                and.b   #$FE,D1                     ;Breakpoint on straight address
                tst.l   D1
                beq.s   cmd_bkpt1                   ;Zero allowed as an argument
                movea.l D1,A6
                bsr     check_write
                bne     illbkpt
cmd_bkpt1:      mulu    #12,D3                      ;Time 12 as an index in the table
                lea     0(A1,D3.w),A6
                move.l  D1,(A6)+                    ;Remember address
                move.w  #-1,(A6)                    ;default =StopBreakpointMitCounter1
                moveq   #1,D1
                move.l  D1,2(A6)                    ;Initialize the counter to 1
                tst.b   D0
                beq.s   cmd_bkpt3
                cmp.b   #',',D0                     ;It must follow a comma
                bne     synerr
                bsr     get                         ;The sign after the comma
                beq     synerr                      ;Nothing followed
                cmp.b   #'=',D0
                beq.s   cmd_bkpt4                   ;counterBreakpoint
                cmp.b   #'*',D0
                beq.s   cmd_bkpt6                   ;permanentBreakpoint
                cmp.b   #'?',D0
                beq.s   cmd_bkpt7                   ;userBreakpoint
cmd_bkpt2:      bsr     get_term                    ;Number of runs for the stop breakpoint
                tst.l   D1
                bmi     illequa                     ;That's too far
                move.l  D1,2(A6)                    ;Pass through
cmd_bkpt3:      jmp     (A4)
cmd_bkpt4:      clr.w   (A6)                        ;Put counter breakpoint
cmd_bkpt5:      clr.l   2(A6)                       ;CounterDefault = 0
                bsr     get
                bne.s   cmd_bkpt2
                jmp     (A4)
cmd_bkpt6:      move.w  #1,(A6)                     ;Set permanently
                bra.s   cmd_bkpt5
cmd_bkpt7:      move.w  #2,(A6)                     ;userBreakpointSetzen
                move.w  D7,D1
                mulu    #80,D1
                lea     cond_breaks(A4),A1
                adda.w  D1,A1
                movea.l A0,A2
                moveq   #79,D1
cmd_bkpt8:      move.b  (A2)+,(A1)+                 ;Remember
                dbra    D1,cmd_bkpt8
                moveq   #9,D1
                lsl.w   D1,D7
                lea     0(A4,D7.w),A1
                adda.l  #cond_bkpt_jsr*1,A1         ;Here the code should go
                move.l  A1,2(A6)                    ;Address of the routine
                bsr     convert_formel
                bsr     clr_cache
                move.w  -8(A1),D0
                and.w   #$F0FF,D0
                cmp.w   #$50C0,D0                   ;sxxD0?
                bne.s   cmd_bkpt9
                subq.l  #4,A1                       ;rts:extLD0
                cmpi.w  #$48C0,(A1)
                bne.s   cmd_bkpt9                   ;No ext.L =>
                cmpi.w  #$4880,-(A1)
                bne.s   cmd_bkpt9                   ;no ext.w =>
                move.w  #$4E75,(A1)+
                clr.l   (A1)+
                clr.l   (A1)
cmd_bkpt9:      jmp     (A4)

cmd_bkpt10:     bsr     get                         ;Delete 'BK' - Breakpoints
                beq.s   cmd_bkpt11                  ;Delete all breakpoints
                bsr     get_term                    ;alsoBreakClr
                cmp.l   #16,D1
                bhs     illequa                     ;> 15, is not there
                mulu    #12,D1
                clr.l   0(A1,D1.w)                  ;BK + NUMBER = Best.BreakPPOint Ferborn
                jmp     (A4)
cmd_bkpt11:     lea     breakpnt_end(A4),A0
cmd_bkpt12:     clr.w   (A1)+                       ;Delete all breakpoints
                cmpa.l  A0,A1
                blo.s   cmd_bkpt12
                jmp     (A4)

cmd_bkpt13:     move.w  (SP)+,D2
                move.w  D2,D3
                mulu    #12,D3
                adda.w  D3,A1
                neg.w   D2
                addi.w  #15,D2
                bsr.s   cmd_bkpt21                  ;Spend breakpoint
                jmp     (A4)

cmd_bkpt14:     moveq   #15,D2                      ;Output all 16 breakpoints
                moveq   #0,D7
cmd_bkpt15:     tst.l   (A1)
                beq.s   cmd_bkpt16                  ;Only issue set breakpoints
                moveq   #-1,D7                      ;Min. found a breakpoint
                bsr.s   cmd_bkpt21
cmd_bkpt16:     lea     12(A1),A1
                dbra    D2,cmd_bkpt15
                tst.w   D7
                bne.s   cmd_bkpt17
                pea     cmd_bkpt20(PC)              ;Not found
                jsr     @print_line(A4)
cmd_bkpt17:     jmp     (A4)

                SWITCH language
                CASE 0
cmd_bkpt20:     DC.B '?keine Breakpoints',13,0
                CASE 1
cmd_bkpt20:     DC.B '?no Breakpoints',13,0
                ENDS

                EVEN

cmd_bkpt21:     move.l  (A1),D1
                jsr     @anf_adr(A4)
                moveq   #'B',D0                     ;Spend promptly
                jsr     @chrout(A4)
                move.w  D2,D1
                neg.w   D1
                addi.w  #15,D1
                move.w  D1,D6                       ;Breakpoint number notice
                bsr     hexbout
                moveq   #'=',D0
                jsr     @chrout(A4)
                move.l  (A1),D1
                bsr     hexlout                     ;Spend address
                move.w  4(A1),D1                    ;Type
                subq.w  #1,D1
                beq.s   cmd_bkpt22                  ;permanent
                blo.s   cmd_bkpt23                  ;counter
                bpl.s   cmd_bkpt26                  ;user
                moveq   #' ',D0
                move.l  6(A1),D1                    ;Number of runs for stop
                bmi.s   cmd_bkpt29
                cmp.l   #1,D1
                bls.s   cmd_bkpt29                  ;1 run => end
                moveq   #',',D0
                bra.s   cmd_bkpt24
cmd_bkpt22:     bsr.s   cmd_bkpt31                  ;permanentBreakpoints
                moveq   #'*',D0
                move.l  6(A1),D1                    ;Number of runs for stop
                bmi.s   cmd_bkpt29
                cmp.l   #1,D1
                bls.s   cmd_bkpt29                  ;1 run => end
                jsr     @chrout(A4)
                moveq   #' ',D0
                move.l  6(A1),D1                    ;Number of runs for stop
                bmi.s   cmd_bkpt29
                cmp.l   #1,D1
                bls.s   cmd_bkpt29                  ;1 run => end
                bra.s   cmd_bkpt25
cmd_bkpt23:     bsr.s   cmd_bkpt31                  ;counterBreakpoints
                moveq   #'=',D0
                move.l  6(A1),D1
cmd_bkpt24:     jsr     @chrout(A4)
cmd_bkpt25:     move.l  D2,-(SP)
                bsr     dezout
                move.l  (SP)+,D2
                moveq   #' ',D0
                bra.s   cmd_bkpt29
cmd_bkpt26:     bsr.s   cmd_bkpt31
                moveq   #'?',D0                     ;User-Breakpoints
                jsr     @chrout(A4)
                mulu    #80,D6                      ;* 80 (place for every breakpoint)
                lea     cond_breaks(A4),A2
                adda.w  D6,A2
cmd_bkpt27:     cmpi.b  #' ',(A2)+                  ;Spaces to the left of the condition
                beq.s   cmd_bkpt27
                subq.l  #1,A2
cmd_bkpt28:     move.b  (A2)+,D0                    ;Provide condition
                beq.s   cmd_bkpt30
                jsr     @chrout(A4)
                bra.s   cmd_bkpt28

cmd_bkpt29:     jsr     @chrout(A4)
cmd_bkpt30:     jsr     @c_eol(A4)
                jmp     @crout(A4)

cmd_bkpt31:     moveq   #',',D0
                jmp     @chrout(A4)
                ENDPART

********************************************************************************
* Insert breakpoints                                                           *
********************************************************************************
                PART 'breakset'
breakset4:      moveq   #0,D7                       ;Set flag for breakpoint to pc
                movea.l A1,A6                       ;Address of Breakpoints Merken
                bra.s   breakset2
breakset:       lea     breakpnt(A4),A1             ;Insert breakpoints
                moveq   #-1,D7
                movea.l _pc(A4),A0
                moveq   #16,D1                      ;Counter for 17 Breakpoints
breakset1:      tst.l   (A1)
                beq.s   breakset2                   ;nix
                movea.l (A1),A3                     ;Get address
                cmpa.l  A0,A3                       ;Breakpoint on the PC?
                beq.s   breakset4                   ;Yes!=>
                move.l  A6,-(SP)
                movea.l A3,A6
                bsr     check_write                 ;Breakpoint allowed there?
                movea.l (SP)+,A6
                bne     illbkpt                     ;No!=>
                tst.b   breaks_flag(A4)             ;Breakpoints already in there?
                bne.s   breakset2                   ;Yes!=>
                move.w  (A3),10(A1)                 ;Get content from there and remember
                move.w  #$4AFC,(A3)                 ;Illegally insert
breakset2:      lea     12(A1),A1                   ;Nearest Breakpoint
                dbra    D1,breakset1
                tst.b   observe_off(A4)
                bne.s   breakset3                   ;No Observe !!!
                move.l  #new_gemdos,$84.w           ;trap #1
                move.l  #new_aesvdi,$88.w           ;trap #2
                move.l  #new_bios,$B4.w             ;trap #13
                move.l  #new_xbios,$B8.w            ;trap #14
                move.l  #etv_term,$0408.w           ;New ETV_TERM handler
breakset3:      st      breaks_flag(A4)
                bra     clr_cache
                ENDPART

********************************************************************************
* Remove breakpoints                                                           *
********************************************************************************
                PART 'breakclr'
breakclr:       tst.b   breaks_flag(A4)             ;Breakpoints already outside?
                beq.s   breakclr3                   ;Yes!=>
                lea     breakpnt+12*16(A4),A1
                moveq   #16,D1
breakclr1:      move.l  (A1),D3
                beq.s   breakclr2                   ;There is not that
                movea.l D3,A0                       ;Get address
                move.w  10(A1),(A0)                 ;Insert command
                tst.w   4(A1)
                bpl.s   breakclr2                   ;No stop-breakpoint
                tst.l   6(A1)
                bhi.s   breakclr2                   ;The counter is still greater than zero
                clr.l   (A1)                        ;Remove Breakpoint
breakclr2:      lea     -12(A1),A1
                dbra    D1,breakclr1
                sf      breaks_flag(A4)
                tst.b   observe_off(A4)
                bne.s   breakclr3
                movem.l D0/A0-A3,-(SP)
                lea     save_data-8(A4),A0
                lea     breakclr6(PC),A3
                lea     $84.w,A1
                lea     old_gemdos(PC),A2
                bsr.s   breakclr4
                lea     old_aesvdi(PC),A2
                bsr.s   breakclr4
                lea     $B4.w,A1
                lea     old_bios(PC),A2
                bsr.s   breakclr4
                lea     old_xbios(PC),A2
                bsr.s   breakclr4
                move.l  $0408(A0),$0408.w           ;ETV_TERM on normal
                movem.l (SP)+,D0/A0-A3
breakclr3:      bra     clr_cache

breakclr4:      move.l  (A1),D0                     ;Get old vector
                cmp.l   (A3)+,D0
                beq.s   breakclr5                   ;Vector is still true
                move.l  D0,(A2)
breakclr5:      move.l  0(A0,A1.w),(A1)+            ;Original vector pure
                addq.l  #4,A2
                rts

breakclr6:      DC.L new_gemdos                     ;Pointer to your own subprograms
                DC.L new_aesvdi
                DC.L new_bios
                DC.L new_xbios
                ENDPART

********************************************************************************
* 'DIRECTORY' - Show Directory                                                 *
********************************************************************************
                PART 'cmd_dir'
cmd_dir:        tst.b   batch_flag(A4)
                bne     batch_mode_err
                bsr     get                         ;No parameters?
                beq.s   cmd_dir2
                bsr     getnam_cont                 ;Name according to Fname
                beq.s   cmd_dir2                    ;Name was not specified
                lea     fname(A4),A1                ;Named buffer
                lea     dir_ext(A4),A0
                moveq   #12,D0                      ;Max.13 sign search file
cmd_dir1:       move.b  (A1)+,(A0)+                 ;copy
                dbeq    D0,cmd_dir1
cmd_dir2:       moveq   #1,D0                       ;Turn the disk on
                bsr     graf_mouse
                movem.l D1-A6,-(SP)
                pea     dir_ext(A4)
                movea.l #allg_buffer,A0
                adda.l  A4,A0
                move.l  A0,-(SP)
                bsr     read_dir                    ;Reave Directory
                addq.l  #8,SP
                movem.l (SP)+,D1-A6
                move.l  D0,D7                       ;Number of files found
                bmi     toserr                      ;Failure!=>
                bne.s   cmd_dir4
                pea     cmd_dir3(PC)
                jsr     @print_line(A4)
                bsr     drive_free
                jmp     (A4)

cmd_dir3:       SWITCH language
                CASE 0
                DC.B '?Keine Dateien gefunden',13,0
                CASE 1
                DC.B '?No files',13,0
                ENDS

                EVEN

cmd_dir4:       movea.l #allg_buffer,A6
                adda.l  A4,A6                       ;Pointer to the Directory Buffer
                subq.w  #1,D7
cmd_dir5:       lea     cmd_drb(PC),A0              ;TO YOU "
                btst    #5,1(A6)                    ;File or folder?
                beq.s   cmd_dir6                    ;Folder =>
                lea     cmd_drd(PC),A0              ;THE  "
                lea     cmd_extensions(PC),A1
cmd_dir50:      move.l  (A1)+,D0                    ;next entry from the table
                cmp.l   12(A6),D0                   ;Extension in the table?
                beq.s   cmd_dir6                    ;JA!=> "The"
                tst.l   D0                          ;End of the table?
                bne.s   cmd_dir50                   ;No!=>

                lea     cmd_dra(PC),A0              ;IT  "
cmd_dir6:       move.l  A0,-(SP)
                jsr     @print_line(A4)             ;Line
                movea.l A6,A5
                addq.l  #3,A5                       ;'  ' bzw. '   '
cmd_dir7:       move.b  (A5)+,D0
                beq.s   cmd_dir10
                cmp.b   #' ',D0
                beq.s   cmd_dir7                    ;Spaces over
                cmp.b   #'.',D0
                beq.s   cmd_dir9
cmd_dir8:       jsr     @chrout(A4)
                bra.s   cmd_dir7
cmd_dir9:       cmpi.b  #' ',(A5)                   ;Extension available?
                bne.s   cmd_dir8
cmd_dir10:      btst    #5,1(A6)
                bne.s   cmd_dir11
                moveq   #'\',D0
                jsr     @chrout(A4)                 ;Folder recognition
cmd_dir11:      moveq   #'"',D0
                jsr     @chrout(A4)
                moveq   #20,D0
                bsr     spacetab
                moveq   #';',D0
                jsr     @chrout(A4)                 ;(for load)
                move.l  20(A6),D1
                bsr     dezout
                moveq   #30,D0
                bsr     spacetab

                move.w  24(A6),D1                   ;Get time
                move.w  D1,D0
                rol.w   #5,D0                       ;hours
                and.w   #$1F,D0
                bsr     dirdez                      ;output
                moveq   #':',D0
                jsr     @chrout(A4)
                move.w  D1,D0
                lsr.w   #5,D0                       ;Minute
                and.w   #$3F,D0
                bsr     dirdez                      ;output
                moveq   #':',D0
                jsr     @chrout(A4)
                moveq   #$1F,D0
                and.w   D1,D0                       ;Seconds
                add.w   D0,D0
                bsr     dirdez                      ;output
                jsr     @space(A4)

                move.w  26(A6),D1                   ;Get the date
                moveq   #$1F,D0
                and.w   D1,D0
                bsr     dirdez                      ;spend the day
                moveq   #'-',D0
                jsr     @chrout(A4)
                move.w  D1,D0
                lsr.w   #5,D0
                and.w   #$0F,D0
                bsr     dirdez                      ;spend the month
                moveq   #'-',D0
                jsr     @chrout(A4)
                moveq   #9,D0
                lsr.w   D0,D1
                moveq   #$7F,D0
                and.l   D0,D1
                add.w   #1980,D1
                moveq   #10,D2                      ;(decimal)
                bsr     numoutx                     ;spend the year
                jsr     @space(A4)

                move.b  29(A6),D2                   ;Fileattribute Holen.
                bsr.s   fatt_out                    ;and spend
                bsr     check_keyb                  ;Button pressed?
                bmi.s   cmd_dir16
                lea     32(A6),A6
                dbra    D7,cmd_dir5
                bsr     drive_free
cmd_dir16:      jmp     (A4)

cmd_extensions: DC.L 'PRG ','TOS ','TTP ','ACC ','APP ','PRX ','ACX ',0

fatt_out:       moveq   #0,D1                       ;Bitcounter
                moveq   #0,D3                       ;Adress-Counter
fatt_out1:      btst    D1,D2                       ;Corresponding bit set?
                beq.s   fatt_out3
fatt_out2:      move.b  cmd_drc(PC,D3.w),D0         ;Sign according to D0
                jsr     @chrout(A4)
                addq.w  #1,D3
                cmp.b   #' ',D0
                bne.s   fatt_out2
                subq.w  #4,D3
fatt_out3:      addq.w  #4,D3
                addq.w  #1,D1
                cmp.w   #6,D1
                bne.s   fatt_out1
                jsr     @c_eol(A4)
                jmp     @crout(A4)

cmd_dra:        DC.B 'LO  "',0
cmd_drd:        DC.B 'LE  "',0
cmd_drb:        DC.B 'DIR "',0
cmd_drc:        DC.B 'r/o ','hid ','sys ','vol ','sub ','clo '
                EVEN
dirdez:         divu    #10,D0
                or.l    #' 0 0',D0
                jsr     @chrout(A4)
                swap    D0                          ;Spend the hours
                jmp     @chrout(A4)
                ENDPART

********************************************************************************
* File_anz = read_dir (* target buffer, * search path)                         *
********************************************************************************
                PART 'read_dir'
read_dir:       movea.l 4(SP),A5                    ;Target buffer
                bsr     do_mediach                  ;Trigger Media-Change
                clr.w   (A5)                        ;Empty table
                pea     data_buff(A4)
                move.w  #$1A,-(SP)
                bsr     do_trap_1                   ;fsetdta(dataBuff)
                addq.l  #6,SP
                lea     data_buff(A4),A6            ;Address of DTA-Buffers
                lea     r_dir_fold_flag(PC),A0
                st      (A0)
                lea     r_dir_joker(PC),A0
                moveq   #$37,D0                     ;First read the folders
                bsr.s   read_dir2
                tst.l   D0
                bmi     read_dir25                  ;Error, abort =>
                lea     r_dir_fold_flag(PC),A0
                clr.w   (A0)
                movea.l 8(SP),A0                    ;Pointer to the search path
                tst.b   (A0)
                bne.s   read_dir1
                lea     r_dir_joker(PC),A0
read_dir1:      moveq   #$27,D0                     ;Read the files
read_dir2:      lea     varbase,A4
                move.w  D0,-(SP)
                move.l  A0,-(SP)
                move.w  #$4E,-(SP)
                bsr     do_trap_1
                addq.w  #8,SP
                tst.l   D0
                bmi     read_dir23
read_dir3:      cmpi.b  #'.',30(A6)                 ;Point as the first letter?=> Folder
                beq     read_dir22
                lea     r_dir_buffer(PC),A4         ;Here go the data
                lea     21(A6),A0
                moveq   #0,D7
                move.b  (A0)+,D7                    ;Fileattribute.
                move.w  D7,28(A4)
                lea     24(A4),A1
                move.l  (A0)+,(A1)                  ;Time + date
                move.l  (A0)+,-(A1)                 ;Size from Intel format in 68000er format
                move.b  #' ',(A4)+                  ;Space in front of the filenames
                move.b  #'',(A4)                   ;Flag for folder
                btst    #4,D7                       ;Folder?
                bne.s   read_dir4
                move.b  #' ',(A4)                   ;Space instead of folder recognition
                lea     r_dir_fold_flag(PC),A1
                tst.w   (A1)
                bne     read_dir21                  ;But then away!
read_dir4:      addq.w  #1,A4
                move.b  #' ',(A4)+                  ;And another space turn
                moveq   #0,D6
                moveq   #7,D0
read_dir5:      move.b  (A0)+,D7
                beq.s   read_dir6
                cmp.b   #'.',D7
                beq.s   read_dir6
                move.b  D7,(A4)+                    ;max.8 characters up to zero byte
                dbeq    D0,read_dir5
read_dir6:      tst.w   D0
                bmi.s   read_dir9
                tst.b   D7
                bne.s   read_dir7
                moveq   #-1,D6
read_dir7:      moveq   #' ',D1
read_dir8:      move.b  D1,(A4)+
                dbra    D0,read_dir8
read_dir9:      move.b  #'.',(A4)+                  ;A point behind the filenames
                moveq   #2,D0
                tst.w   D6
                bne.s   read_dir11
read_dir10:     move.b  (A0)+,D7
                beq.s   read_dir11
                cmp.b   #'.',D7
                beq.s   read_dir10
                move.b  D7,(A4)+                    ;max.3 characters up to zero byte
                dbeq    D0,read_dir10
read_dir11:     tst.w   D0
                bmi.s   read_dir13
                moveq   #' ',D1
read_dir12:     move.b  D1,(A4)+                    ;Fill extension with spaces
                dbra    D0,read_dir12
read_dir13:     move.b  #' ',(A4)+                  ;and add a space
                clr.l   (A4)+                       ;Delete until the 20th byte

                movea.l A5,A4
                lea     r_dir_buffer(PC),A3         ;Here are the data
                movem.l (A3),D3-A2                  ;bufferInRegisterHolen (d3D6=filename)
                bra.s   read_dir15
read_dir14:     lea     32(A4),A4
read_dir15:     tst.b   (A4)
                beq.s   read_dir20                  ;End of the table reached
                cmp.l   (A4),D3
                bhi.s   read_dir14
                bne.s   read_dir16
                cmp.l   4(A4),D4
                bhi.s   read_dir14
                bne.s   read_dir16
                cmp.l   8(A4),D5                    ;Compare filenames
                bhi.s   read_dir14
                bne.s   read_dir16
                cmp.l   12(A4),D6
                bhi.s   read_dir14
                bne.s   read_dir16
                cmpa.l  20(A4),A0                   ;Compare length
                bhi.s   read_dir14
                bne.s   read_dir16
                cmpa.l  24(A4),A0                   ;Compare date / time
                bhi.s   read_dir14
                beq.s   read_dir14
read_dir16:     movea.l A4,A3
                bra.s   read_dir18
read_dir17:     lea     32(A4),A4
read_dir18:     tst.b   32(A4)                      ;Search the table end
                bne.s   read_dir17
                clr.w   64(A4)
read_dir19:     movem.l (A4)+,D0-D7
                movem.l D0-D7,(A4)
                lea     -64(A4),A4
                cmpa.l  A3,A4
                bhs.s   read_dir19
                lea     r_dir_buffer(PC),A4         ;Here are the data
                movem.l (A4),D0-D7                  ;bufferInRegisterHolen (d0D7=filename)
                movem.l D0-D7,(A3)
                bra.s   read_dir21
read_dir20:     movem.l D3-A2,(A4)
                clr.w   32(A4)                      ;Delete follow-up entry
read_dir21:     lea     30(A6),A0
                clr.l   (A0)+                       ;Delete filename
                clr.l   (A0)+
                clr.l   (A0)+
                clr.w   (A0)+
read_dir22:     move.l  A4,-(SP)
                lea     varbase,A4
                move.w  #$4F,-(SP)
                bsr     do_trap_1
                addq.w  #2,SP
                movea.l (SP)+,A4
                tst.l   D0
                bpl     read_dir3
read_dir23:     lea     -32(A5),A0
                moveq   #-49,D1
                cmp.l   D1,D0                       ;No further files
                beq.s   read_dir24                  ;Everything ok =>
                moveq   #-33,D1
                cmp.l   D1,D0                       ;file not found
                beq.s   read_dir24                  ;Everything ok =>
                rts                                 ;get off
read_dir24:     lea     32(A0),A0
                tst.b   (A0)                        ;Search the table ends
                bne.s   read_dir24
                suba.l  A5,A0                       ;Size of the table
                move.l  A0,D0
                lsr.l   #5,D0                       ;32 (size of an entry)
read_dir25:     rts

r_dir_fold_flag:DC.W 0                              ;Set if only folders are searched
r_dir_buffer:   DS.B 32                             ;Cache for an entry
r_dir_joker:    DC.B '*.*',0
                EVEN
                ENDPART

********************************************************************************
* D0=get_dlen(A6) - Determine validity and length of the opcodes from A6       *
* All flags are set if the opcode is unknown                                   *
********************************************************************************
                PART 'get_dlen'
get_dlen:       movea.l A6,A5                       ;Remember the initial code of the opcodes
                bsr     check_read                  ;Access granted?
                bne     getdl7                      ;Cancel, if not
                move.b  (A6),D1
                lsr.b   #3,D1
                moveq   #30,D0
                and.w   D1,D0
                lea     distab_tab(PC),A1
                movea.l A1,A3
                adda.w  0(A1,D0.w),A1               ;Table start
                adda.w  2(A3,D0.w),A3               ;Table end
                move.w  (A6)+,D1                    ;to disassembled opcode
                bra.s   getdl1

getdtab:        DC.W $FFFF                          ;00,
                DC.W $FF00                          ;01,
                DC.W $FFFF                          ;02,add2
                DC.W $FFF0                          ;03,
                DC.W $F1FF                          ;04,
                DC.W $FFF8                          ;05,
                DC.W $FFF8                          ;06,
                DC.W $FF00                          ;07,Relative address distance follows
                DC.W $FFF8                          ;08,
                DC.W $FFF8                          ;09,
                DC.W $F1FF                          ;0A,
                DC.W $F1FF                          ;0B,
                DC.W $F03F                          ;0C,<ea> Bit 6-11
                DC.W $FFC0                          ;0D,Everyone
                DC.W $FFC0                          ;0E,changeable
                DC.W $FFC0                          ;0F,Data changeable
                DC.W $FFC0                          ;10,data
                DC.W $FFC0                          ;11,Memory changeable
                DC.W $FFC0                          ;12,everything except #
                DC.W $FFC0                          ;13,control
                DC.W $F1FF                          ;14,
                DC.W $F1FF                          ;15,
                DC.W $FFFF                          ;16,
                DC.W $FFFF                          ;17,
                DC.W $FFFF                          ;18,
                DC.W $FFFF                          ;19,add2
                DC.W $FFFF                          ;1A,add2
                DC.W $FFFF                          ;1B,#-Word with length x follows
                DC.W $FFF8                          ;1C,add2
                DC.W $F1FF                          ;1D,add2
                DC.W $FFFF                          ;1E,add2
                DC.W $FFF8                          ;1F,
                DC.W $FFC0                          ;20,BTST-command
                DC.W $FFF8                          ;21,BKPT-command
                DC.W $F000                          ;22,Linea
                DC.W $F000                          ;23,Linef
                DC.W $FFFF                          ;24,Dn or in (movec)
                DC.W $FFFF                          ;25,add2 68010-Register (movec)

getdl1:         moveq   #$3F,D0                     ;Thydressierungsart
                and.w   4(A1),D0                    ;Mask for address types from table
                add.w   D0,D0                       ;two two as an index
                move.w  getdtab(PC,D0.w),D3         ;Mask (qperator) in D3
                move.w  4(A1),D0                    ;Get a mask again
                ror.w   #7,D0                       ;Make high-byte to Lowbyte
                and.w   #$7E,D0                     ;two two as an index
                and.w   getdtab(PC,D0.w),D3         ;Operate mask
                move.w  4(A1),D0                    ;Word for address types
                bpl.s   getdl2
                and.w   #$FF3F,D3                   ;operandNegativ
getdl2:         tst.b   D0                          ;conditionfeld?
                bpl.s   getdl3
                and.w   #$F0FF,D3                   ;Operator negative.
getdl3:         and.w   D1,D3                       ;Check if D1 fits in the mask
                cmp.w   2(A1),D3                    ;OPCODE from table?
                beq.s   getdl8                      ;Yes, could be the right thing
getdl4:         addq.l  #6,A1                       ;Increase pointer
                cmpa.l  A3,A1                       ;End?
                bne.s   getdl1                      ;No, continue
                move.w  D1,D0                       ;Comparison on Movem
                and.w   #$FB80,D0                   ;Mask for Movem
                cmp.w   #$4880,D0                   ;Opcode for Movem
                bne.s   getdl7                      ;No, opcode not found
                addq.l  #2,A6                       ;Register Mask = 1 Word
                btst    #10,D1                      ;Direction of the Movem
                bne.s   getdl5                      ;Memory in tab!
                move.w  #$01F4,D2
                bsr     _chea
                tst.w   D7
                bne.s   getdl7
                bra.s   getdl6
getdl5:         move.w  #$07EC,D2
                bsr     _chea
                tst.w   D7
                bne.s   getdl7
getdl6:         move.l  A6,D0
                sub.l   A5,D0                       ;Length of the opcodes
                move    #0,CCR                      ;Everything OK
                rts
getdl7:         lea     2(A5),A6
                moveq   #2,D0                       ;Length of the opcodes
                move    #$FF,CCR                    ;failure
                rts

getdl8:         move.w  4(A1),D5                    ;Addressing types Word
                bpl.s   getdl9                      ;Bit for longitude was deleted
                move.w  D1,D0                       ;Toil.value
                and.w   #$C0,D0                     ;Length insulate
                cmp.w   #$C0,D0                     ;Both bits set => Error
                beq.s   getdl4                      ;Test next opcode
getdl9:         andi.w  #$3F3F,D5                   ;Remove superfluous bits
                tst.w   D5
                beq.s   getdl6                      ;If no operator / operand follows => done
                move.w  D5,D0
                movem.l D1-D3,-(SP)
                bsr.s   eaoper                      ;Test operator
                movem.l (SP)+,D1-D3
                tst.w   D7
                bne.s   getdl4                      ;Mo.
                move.w  D5,D0
                lsr.w   #8,D0
                tst.b   D0
                beq.s   getdl6                      ;No operand
                move.l  D1,-(SP)
                bsr.s   eaoper                      ;Test operand
                move.l  (SP)+,D1
                tst.w   D7
                bne     getdl4                      ;Mo.
                bra.s   getdl6

eaoper:         moveq   #0,D7                       ;Erase error flag
                and.w   #$3F,D0                     ;The rest
                add.w   D0,D0                       ;two two as an index
                lea     da(PC),A2                   ;Search hands on table start
                adda.w  -2(A2,D0.w),A2
                jmp     (A2)

                BASE DC.W,da
da:             DC.W ret,add2,ret,ret,ret,ret,addrel
                DC.W ret,ret,ret,ret,chea1,chea2,chea3,chea4
                DC.W chea5,chea6,chea7,chea8,ret,ret,ret,ret
                DC.W ret,add2,add2,addim,add2,add2,add2,ret
                DC.W chea52,check_bkpt,ret,ret,ret,add2

check_bkpt:     cmpi.w  #$4848,-2(A6)               ;bkpt #0 -> breakpt 'String'
                bne     ret
check_bkpt1:    tst.b   (A6)+                       ;String over
                bne.s   check_bkpt1
                move.l  A6,D0
                addq.l  #1,D0                       ;EVEN
                and.b   #$FE,D0
                movea.l D0,A6
                rts

chea1:          move.w  D1,D0
                lsr.w   #3,D0
                and.w   #$38,D0                     ;Isolate bits
                rol.w   #7,D1
                and.w   #7,D1
                or.w    D0,D1
                and.w   #$3F,D1                     ;Isolate valid bits
                move.w  #$01FD,D2
                bra.s   _chea
chea2:          move.w  #$0FFF,D2
                bra.s   _chea
chea3:          move.w  #$01FF,D2
                bra.s   _chea
chea4:          move.w  #$01FD,D2
                bra.s   _chea
chea5:          move.w  #$0FFD,D2
                bra.s   _chea
chea52:         move.w  #$07FD,D2
                bra.s   _chea
chea6:          move.w  #$01FC,D2
                bra.s   _chea
chea7:          move.w  #$07FF,D2
                bra.s   _chea
chea8:          move.w  #$07E4,D2
_chea:          moveq   #0,D7                       ;Erase error flag
                clr.w   D3                          ;Clear mask
                move.w  D1,D0
                and.w   #$38,D0
                cmp.w   #$38,D0
                beq.s   _eafnd1                     ;is a 111xxx style
                lsr.w   #3,D0
                and.w   #7,D0
                bra.s   _eafnd2
_eafnd1:        move.w  D1,D0
                and.w   #7,D0
                cmp.w   #5,D0
                bhs.s   _eafnd3
                addq.w  #7,D0
_eafnd2:        bset    D0,D3
_eafnd3:        and.w   D2,D3                       ;Allowed to compare mask with address type
                beq.s   _chea1
                cmp.w   #1,D3                       ;Dn
                beq.s   ret
                cmp.w   #2,D3                       ;An
                beq.s   ret
                cmp.w   #4,D3                       ;(An)
                beq.s   ret
                cmp.w   #8,D3                       ;(An)+
                beq.s   ret
                cmp.w   #$10,D3                     ;-(An)
                beq.s   ret
                cmp.w   #$20,D3                     ;d(An)
                beq.s   add2
                cmp.w   #$40,D3                     ;d(An,Rx)
                beq.s   add2
                cmp.w   #$80,D3                     ;$xxxx
                beq.s   add2
                cmp.w   #$0100,D3                   ;$xxxxxxxx
                beq.s   add4
                cmp.w   #$0200,D3                   ;d(PC)
                beq.s   add2
                cmp.w   #$0400,D3                   ;d(PC,Rx)
                beq.s   add2
                cmp.w   #$0800,D3                   ;#
                beq.s   addim
_chea1:         moveq   #-1,D7                      ;Error occurred
ret:            rts
addim:          addq.l  #2,A6
                tst.w   4(A1)                       ;Addressing types word from the table
                bmi.s   addim_1                     ;is equipped with a <ln> field
                move.w  4(A1),D1
                btst    #14,D1                      ;Bit = 0 is called .b
                beq.s   ret
                btst    #6,D1
                beq.s   ret                         ;is .w
                bra.s   add2
addim_1:        andi.w  #$C0,D1                     ;<ln> -feld isolate
                beq.s   ret
                cmp.w   #$40,D1
                beq.s   ret                         ;is .w
add2:           addq.l  #2,A6
                rts
addre1:         addq.b  #1,D1                       ;Good.l ??
                bne.s   ret2                        ;No!=>
add4:           addq.l  #4,A6
                rts
addrel:         tst.b   D1                          ;Relative address distance follows
                bne.s   addre1                      ;possibly long distance?
                addq.l  #2,A6                       ;Word-distance
ret2:           rts
                ENDPART

********************************************************************************
* Disassembler                                                                 *
********************************************************************************
                PART 'disass'
                BASE DC.W,distab
distab:
distab0:        DC.W _andi,$023C,$161A
                DC.W _ori,$3C,$161A
                DC.W _eori,$0A3C,$161A
                DC.W _andi,$027C,$1719
                DC.W _ori,$7C,$1719
                DC.W _eori,$0A7C,$1719
                DC.W _btst,$0800,$2002
                DC.W _bclr,$0880,$0F02
                DC.W _bset,$08C0,$0F02
                DC.W _bchg,$0840,$0F02
                DC.W _addi,$0600,$8F1B
                DC.W _andi,$0200,$8F1B
                DC.W _cmpi,$0C00,$8F1B
                DC.W _eori,$0A00,$8F1B
                DC.W _ori,0,$8F1B
                DC.W _subi,$0400,$8F1B
                DC.W _movep_w,$0108,$0B1C
                DC.W _movep_l,$0148,$0B1C
                DC.W _movep_w,$0188,$1C0B
                DC.W _movep_l,$01C8,$1C0B
                DC.W _btst,$0100,$100B
                DC.W _bclr,$0180,$0F0B
                DC.W _bset,$01C0,$0F0B
                DC.W _bchg,$0140,$0F0B
distab1:        DC.W _move_b,$1000,$0C4D
distab2:        DC.W _movea_l,$2040,$4A4D
                DC.W _move_l,$2000,$4C4D
distab3:        DC.W _movea_w,$3040,$4A0D
                DC.W _move_w,$3000,$4C0D
distab4:        DC.W _bkpt,$4848,$21
                DC.W _illegal,$4AFC,0
                DC.W _nop,$4E71,0
                DC.W _reset,$4E70,0
                DC.W _rte,$4E73,0
                DC.W _rtd,$4E74,$19
                DC.W _rtr,$4E77,0
                DC.W _rts,$4E75,0
                DC.W _trapv,$4E76,0
                DC.W _movec,$4E7A,$2425
                DC.W _movec,$4E7B,$2524
                DC.W _stop,$4E72,$19
                DC.W _ext_w,$4880,9
                DC.W _ext_l,$48C0,9
                DC.W _link,$4E50,$1908
                DC.W _move,$4E60,$1808
                DC.W _move,$4E68,$0818
                DC.W _swap,$4840,9
                DC.W _trap,$4E40,3
                DC.W _unlk,$4E58,8
                DC.W _chk,$4180,$4B10
                DC.W _move,$42C0,$0F16
                DC.W _move,$44C0,$1650
                DC.W _move,$46C0,$5710
                DC.W _jmp,$4EC0,$13
                DC.W _move,$40C0,$0F17
                DC.W _jsr,$4E80,$13
                DC.W _nbcd,$4800,$0F
                DC.W _pea,$4840,$13
                DC.W _lea,$41C0,$0A13
                DC.W _clr,$4200,$800F
                DC.W _neg,$4400,$800F
                DC.W _negx,$4000,$800F
                DC.W _not,$4600,$800F
                DC.W _tas,$4AC0,$0F
                DC.W _tst,$4A00,$800F
                DC.W _extb,$49C0,9
distab5:        DC.W _db,$50C8,$1E89
                DC.W _s,$50C0,$8F
                DC.W _addq,$5000,$8E04
                DC.W _subq,$5100,$8E04
distab6:        DC.W _bra,$6000,7
                DC.W _bsr,$6100,7
                DC.W _b,$6000,$87
distab7:        DC.W _moveq_l,$7000,$0B01
distab8:        DC.W _divu,$80C0,$4B10
                DC.W _divs,$81C0,$4B10
                DC.W _sbcd,$8100,$0B09
                DC.W _sbcd,$8108,$1405
                DC.W _or,$8000,$8B10
                DC.W _or,$8100,$8E0B
distab9:        DC.W _suba_w,$90C0,$4A0D
                DC.W _suba_l,$91C0,$4A4D
                DC.W _subx,$9100,$8B09
                DC.W _subx,$9108,$9405
                DC.W _sub,$9000,$8B0D
                DC.W _sub,$9100,$8E0B
distabA:        DC.W _linea,$A000,$22
distabB:        DC.W _cmpa_w,$B0C0,$4A0D
                DC.W _cmpa_l,$B1C0,$4A4D
                DC.W _cmp,$B000,$8B0D
                DC.W _cmpm,$B108,$9506
                DC.W _eor,$B100,$8F0B
distabC:        DC.W _mulu,$C0C0,$4B10
                DC.W _muls,$C1C0,$4B10
                DC.W _abcd,$C100,$0B09
                DC.W _abcd,$C108,$1405
                DC.W _exg,$C140,$090B
                DC.W _exg,$C148,$080A
                DC.W _exg,$C188,$0B08
                DC.W _and,$C000,$8B10
                DC.W _and,$C100,$8E0B
distabD:        DC.W _adda_w,$D0C0,$4A0D
                DC.W _adda_l,$D1C0,$4A4D
                DC.W _addx,$D100,$8B09
                DC.W _addx,$D108,$9405
                DC.W _add,$D000,$8B0D
                DC.W _add,$D100,$8E0B
distabE:        DC.W _asl_w,$E1C0,$11

                DC.W _asr_w,$E0C0,$11
                DC.W _asl,$E100,$8904
                DC.W _asr,$E000,$8904
                DC.W _asl,$E120,$890B
                DC.W _asr,$E020,$890B
                DC.W _lsl_w,$E3C0,$11
                DC.W _lsr_w,$E2C0,$11
                DC.W _lsl,$E108,$8904
                DC.W _lsr,$E008,$8904
                DC.W _lsl,$E128,$890B
                DC.W _lsr,$E028,$890B
                DC.W _rol_w,$E7C0,$11
                DC.W _ror_w,$E6C0,$11
                DC.W _rol,$E118,$8904
                DC.W _ror,$E018,$8904
                DC.W _rol,$E138,$890B
                DC.W _ror,$E038,$890B
                DC.W _roxl_w,$E5C0,$11
                DC.W _roxr_w,$E4C0,$11
                DC.W _roxl,$E110,$8904
                DC.W _roxr,$E010,$8904
                DC.W _roxl,$E130,$890B
                DC.W _roxr,$E030,$890B
distabF:        DC.W _linef,$F000,$23
disend:

disass:         lea     spaced(A4),A0
                cmpa.l  #anfang,A6
                blo.s   o_disanf
                cmpa.l  A4,A6                       ;Disassemble in the debugger is prohibited!
                bhs.s   o_disanf
                move.b  #'*',(A0)+                  ;imDebugger
o_disanf:       move.w  disbase(A4),D2              ;Number base for the disassembler
                st      testwrd(A4)                 ;Set flagbyte for issue
                movea.l A6,A5
                bsr     check_read                  ;Access granted?
                bne     o_dserr                     ;Cancel, if not
                move.b  (A6),D1
                lsr.b   #3,D1
                moveq   #30,D0
                and.w   D1,D0
                lea     distab_tab(PC),A1
                movea.l A1,A3
                adda.w  0(A1,D0.w),A1               ;Table start
                adda.w  2(A3,D0.w),A3               ;Table end
                move.w  (A6)+,D1                    ;to disassembled value in D1
                bra     o_dis1

                BASE DC.W,distab_tab
distab_tab:     DC.W distab0
                DC.W distab1

                DC.W distab2
                DC.W distab3
                DC.W distab4
                DC.W distab5
                DC.W distab6
                DC.W distab7
                DC.W distab8
                DC.W distab9
                DC.W distabA
                DC.W distabB
                DC.W distabC
                DC.W distabD
                DC.W distabE
                DC.W distabF
                DC.W disend

_roxl_w:        DC.B 'ROXL.W',0
_roxr_w:        DC.B 'ROXR.W',0
_ror_w:         DC.B 'ROR.W',0
_rol_w:         DC.B 'ROL.W',0
_lsl_w:         DC.B 'LSL.W',0
_lsr_w:         DC.B 'LSR.W',0
_asl_w:         DC.B 'ASL.W',0
_asr_w:         DC.B 'ASR.W',0
_adda_l:        DC.B 'ADDA.L',0
_adda_w:        DC.B 'ADDA.W',0
_cmpa_w:        DC.B 'CMPA.W',0
_cmpa_l:        DC.B 'CMPA.L',0
_suba_w:        DC.B 'SUBA.W',0
_suba_l:        DC.B 'SUBA.L',0
_moveq_l:       DC.B 'MOVEQ',0
_ext_w:         DC.B 'EXT.W',0
_ext_l:         DC.B 'EXT.L',0
_move_b:        DC.B 'MOVE.B',0
_move_w:        DC.B 'MOVE.W',0
_move_l:        DC.B 'MOVE.L',0
_movea_w:       DC.B 'MOVEA.W',0
_movea_l:       DC.B 'MOVEA.L',0
_movep_w:       DC.B 'MOVEP.W',0
_movep_l:       DC.B 'MOVEP.L',0
_illegal:       DC.B 'ILLEGAL',0
_db:            DC.B 'DB',0
_s:             DC.B 'S',0
_b:             DC.B 'B',0

_add:           DC.B 'ADD',0
_and:           DC.B 'AND',0
_asl:           DC.B 'ASL',0
_asr:           DC.B 'ASR',0
_addq:          DC.B 'ADDQ',0
_addx:          DC.B 'ADDX',0
_abcd:          DC.B 'ABCD',0
_addi:          DC.B 'ADDI',0
_andi:          DC.B 'ANDI',0
_bsr:           DC.B 'BSR',0
_bra:           DC.B 'BRA',0
_btst:          DC.B 'BTST',0
_bclr:          DC.B 'BCLR',0
_bset:          DC.B 'BSET',0
_bchg:          DC.B 'BCHG',0
_cmp:           DC.B 'CMP',0
_clr:           DC.B 'CLR',0
_cmpm:          DC.B 'CMPM',0
_cmpi:          DC.B 'CMPI',0
_chk:           DC.B 'CHK',0
_divu:          DC.B 'DIVU',0
_divs:          DC.B 'DIVS',0
_eor:           DC.B 'EOR',0
_extb:          DC.B 'EXTB',0
_exg:           DC.B 'EXG',0
_eori:          DC.B 'EORI',0
_jmp:           DC.B 'JMP',0
_jsr:           DC.B 'JSR',0
_lea:           DC.B 'LEA',0
_lsr:           DC.B 'LSR',0
_lsl:           DC.B 'LSL',0
_link:          DC.B 'LINK',0
_move:          DC.B 'MOVE',0
_movec:         DC.B 'MOVEC',0
_mulu:          DC.B 'MULU',0
_muls:          DC.B 'MULS',0
_nop:           DC.B 'NOP',0
_neg:           DC.B 'NEG',0
_not:           DC.B 'NOT',0
_negx:          DC.B 'NEGX',0
_nbcd:          DC.B 'NBCD',0
_or:            DC.B 'OR',0
_ori:           DC.B 'ORI',0
_pea:           DC.B 'PEA',0
_rts:           DC.B 'RTS',0
_rtd:           DC.B 'RTD',0
_rol:           DC.B 'ROL',0
_ror:           DC.B 'ROR',0
_roxl:          DC.B 'ROXL',0
_roxr:          DC.B 'ROXR',0
_rte:           DC.B 'RTE',0
_rtr:           DC.B 'RTR',0
_reset:         DC.B 'RESET',0
_sub:           DC.B 'SUB',0
_subq:          DC.B 'SUBQ',0
_swap:          DC.B 'SWAP',0
_subx:          DC.B 'SUBX',0
_subi:          DC.B 'SUBI',0
_sbcd:          DC.B 'SBCD',0
_stop:          DC.B 'STOP',0
_tst:           DC.B 'TST',0
_trap:          DC.B 'TRAP',0
_trapv:         DC.B 'TRAPV',0
_tas:           DC.B 'TAS.B',0
_unlk:          DC.B 'UNLK',0
_bkpt:          DC.B 'BKPT',0
_linea:         DC.B 'LINEA',0
_linef:         DC.B 'LINEF',0
                EVEN
o_msktab:       DC.W $FFFF                          ;00, nothing
                DC.W $FF00                          ;01, Lower Byte = # - Data (MoveQ)
                DC.W $FFFF                          ;02, #-Bitnr.in the following word
                DC.W $FFF0                          ;03, Pure # number for trap in bits 0-3
                DC.W $F1FF                          ;04, Data # for AddQ etc bits 9-11
                DC.W $FFF8                          ;05, -(An) Bits 0-2
                DC.W $FFF8                          ;06, (An)+ Bits 0-2
                DC.W $FF00                          ;07, Relative address distance follows
                DC.W $FFF8                          ;08, An Bits 0-2
                DC.W $FFF8                          ;09, Dn Bits 0-2
                DC.W $F1FF                          ;0A, An Bits 9-11

                DC.W $F1FF                          ;0B, Dn Bits 9-11
                DC.W $F03F                          ;0C, <ea> Bit 6-11
                DC.W $FFC0                          ;0D, Everyone
                DC.W $FFC0                          ;0E, changeable
                DC.W $FFC0                          ;0F, Data changeable
                DC.W $FFC0                          ;10, data
                DC.W $FFC0                          ;11, Memory changeable
                DC.W $FFC0                          ;12, everything except #
                DC.W $FFC0                          ;13, control
                DC.W $F1FF                          ;14, -(An) Bits 9-11
                DC.W $F1FF                          ;15, (An)+ Bits 9-11
                DC.W $FFFF                          ;16, CCR
                DC.W $FFFF                          ;17, SR
                DC.W $FFFF                          ;18, USP
                DC.W $FFFF                          ;19, #-Number with 16 bit. Word (to SR, Stop)
                DC.W $FFFF                          ;1A, #-Number with 8bit follow.Word (to CCR)
                DC.W $FFFF                          ;1B, #-Word with length x follows
                DC.W $FFF8                          ;1C, d(An) Bits 0-2
                DC.W $F1FF                          ;1D, d(An) Bits 9-11
                DC.W $FFFF                          ;1E, Relative address distance in the following word (DBRA,..)
                DC.W $F000                          ;1F, # 12-Bit
                DC.W $FFC0                          ;20, Everything except # & on (BTST!)
                DC.W $FFF8                          ;21, # 3-Bit (BKPT!)
                DC.W $F000                          ;22, Linea
                DC.W $F000                          ;23, Linef
                DC.W $FFFF                          ;24, Dn or An       (movec)
                DC.W $FFFF                          ;25, 68010-Register (movec)

o_dis1:         moveq   #$3F,D0                     ;Thydressierungsart
                and.w   4(A1),D0                    ;Mask for address types from table
                add.w   D0,D0                       ;two two as an index
                move.w  o_msktab(PC,D0.w),D3        ;Mask (qperator) in D3
                move.w  4(A1),D0                    ;Get a mask again
                ror.w   #7,D0                       ;Make high-byte to Lowbyte
                and.w   #$7E,D0                     ;two two as an index
                and.w   o_msktab(PC,D0.w),D3        ;Operate mask
                move.w  4(A1),D0                    ;Word for address types
                bpl.s   o_dis10
                and.w   #$FF3F,D3                   ;operandNegativ
o_dis10:        tst.b   D0                          ;conditionfeld?
                bpl.s   o_dis11
                and.w   #$F0FF,D3                   ;Operator negative.
o_dis11:        and.w   D1,D3                       ;Check if D1 fits in the mask
                cmp.w   2(A1),D3                    ;OPCODE from table?
                beq     o_dis_2                     ;Yes, could be the right thing
o_dis8:         addq.l  #6,A1                       ;Increase pointer
                cmpa.l  A3,A1                       ;End?
                bne.s   o_dis1                      ;No, continue

                move.w  D1,D0                       ;Comparison on Movem
                and.w   #$FB80,D0                   ;Mask for Movem
                cmp.w   #$4880,D0                   ;Opcode for Movem
                bne.s   o_opcde                     ;No, opcode not found

                lea     o_movem(PC),A2              ;"Move.w"
o_mvm1:         move.b  (A2)+,(A0)+                 ;inBuffer
                bne.s   o_mvm1
                subq.l  #1,A0                       ;Write pointer to the 0
                btst    #6,D1                       ;Lengthbit test
                beq.s   o_mvm2
                move.b  #'l',-2(A0)                 ;warLangwort
o_mvm2:         btst    #10,D1                      ;Direction of the Movem
                bne.s   o_mvm3                      ;Memory in tab!
                move.w  D1,-(SP)                    ;Upgrode Brands
                move.w  (A6)+,D1                    ;Get register mask
                moveq   #$38,D0
                and.w   (SP),D0                     ;opcode
                cmp.w   #$20,D0
                beq.s   o_mvm_2                     ;Target address type - (AN)
                bsr.s   o_turn
o_mvm_2:        bsr.s   o_msko
                move.w  (SP)+,D1                    ;Retry opcode
                move.b  #',',(A0)+                  ;Comma between source and destination
                bsr.s   o_mvm4                      ;Disassembling the target
                tst.w   D7
                bne.s   o_opcde
                bra     o_disok
o_mvm4:         move.w  #$01F4,-(SP)
                bra     o_disea

o_mvm3:         move.w  (A6)+,-(SP)
                move.w  D1,-(SP)
                bsr.s   o_mvm5                      ;Disassembling the source
                move.b  #',',(A0)+
                move.w  (SP)+,D0
                move.w  (SP)+,D1                    ;registermaske
                tst.w   D7
                bne.s   o_opcde
                bsr.s   o_turn
                bsr.s   o_msko
                bra     o_disok
o_mvm5:         move.w  #$07EC,-(SP)
                bra     o_disea

o_movem:        DC.B 'movem.w ',0
                EVEN

o_opcde:        move.b  #'d',(A0)+                  ;'???'+Chr$(0)
                move.b  #'c',(A0)+
                move.b  #'.',(A0)+
                move.b  #'w',(A0)+
                move.b  #' ',(A0)+
                move.w  (A5),D1
                bsr     o_disa1f
                clr.b   (A0)+
                lea     2(A5),A6
                rts

o_turn:         movem.w D0/D4,-(SP)
                moveq   #15,D0
o_turn1:        add.w   D1,D1
                roxr.w  #1,D4
                dbra    D0,o_turn1
                move.w  D4,D1
                movem.w (SP)+,D0/D4
                rts

o_msko:         moveq   #15,D0
                movem.w D2-D3,-(SP)
                moveq   #0,D3                       ;No register issued
o_msko2:        tst.w   D1
                beq.s   o_msko1
                bclr    D0,D1
                dbne    D0,o_msko2
                beq.s   o_msko1
                bsr.s   o_rgout
                bmi.s   o_msko1
                tst.w   D1
                beq.s   o_msko1
                btst    D0,D1
                beq.s   o_msko3
                move.b  #'-',(A0)+
o_msko4:        bclr    D0,D1
                dbeq    D0,o_msko4
                addq.w  #1,D0                       ;1 Add: Now D0 is Correct Reg
                bsr.s   o_rgout
                bmi.s   o_msko1
                tst.w   D1
                bne.s   o_msko3                     ;Yes, there are other registers
o_msko1:        movem.w (SP)+,D2-D3
                rts
o_msko3:        move.b  #'/',(A0)+
                bra.s   o_msko2

o_rgout:        add.w   D0,D0
                move.b  o_rgtab(PC,D0.w),D2
                tst.b   D3
                bmi.s   o_rgou2                     ;No registry "header" spend more
                cmp.b   #'D',D2
                beq.s   o_rgou1
                st      D3                          ;Data registry no longer
                bra.s   o_rgou3
o_rgou1:        tst.b   D3
                bne.s   o_rgou2
                moveq   #1,D3                       ;Data register issued
o_rgou3:        move.b  D2,(A0)+
o_rgou2:        move.b  o_rgtab+1(PC,D0.w),(A0)+
                lsr.w   #1,D0
                subq.w  #1,D0
                rts
o_rgtab:        DC.B 'A7A6A5A4A3A2A1A0D7D6D5D4D3D2D1D0'
                EVEN

o_dis_2:        bsr.s   o_dis2
                blo     o_dis8                      ;No, did not agree
                rts

o_dis2:         move.w  4(A1),D5                    ;D5 = address types word
                move.w  (A1),D0                     ;A1 = pointer to the Mnemonic table.
                lea     distab(PC),A2               ;Search hands on table start
                lea     0(A2,D0.w),A2
                move.l  A0,-(SP)
o_dis3:         move.b  (A2)+,D0                    ;A2 = pointer on Mnemonic plain text
                beq.s   o_dis32                     ;done
                cmp.b   #'A',D0
                blo.s   o_dis31                     ;Space should not be changed
                cmp.b   #'Z'+1,D0
                bhs.s   o_dis31
                add.b   #$20,D0                     ;to convert to lowercase letters
o_dis31:        move.b  D0,(A0)+
                bra.s   o_dis3

o_contb:        DC.B 't f hilscccsneeqvcvsplmigeltgtle'
                EVEN

o_dis32:        movea.l (SP)+,A2                    ;Start of Mnemonics
                tst.b   D5                          ;Addressing types Word
                bpl.s   o_dis4                      ;Bit for Condition not set
                move.w  D1,D0                       ;to disassembled date
                bclr    #7,D5                       ;Delete bit for condition now
                and.w   #$0F00,D0                   ;Isolate the 4-bit condition
                lsr.w   #7,D0
                cmp.w   #2,D0
                bne.s   o_disco
                cmpi.b  #'d',-2(A0)
                bne.s   o_disco                     ;DF is becoming DBRA (whereas SF is not SRA)
                cmpi.b  #'b',-1(A0)
                bne.s   o_disco
                move.b  #'r',(A0)+                  ;dbra
                move.b  #'a',(A0)+
                bra.s   o_dis4
o_disco:        move.b  o_contb(PC,D0.w),(A0)+
                move.b  o_contb+1(PC,D0.w),(A0)+
o_dis4:         tst.w   D5                          ;Addressing types Word
                bpl.s   o_dis5                      ;Bit for longitude was deleted
                move.b  #'.',(A0)+
                move.b  #'b',(A0)+                  ;.B as a preployment
                move.w  D1,D0                       ;Toil.value
                and.w   #$C0,D0                     ;Length insulate
                beq.s   o_dis7                      ;.B, ready
                cmp.w   #$C0,D0                     ;Both bits set?
                beq.s   o_dserr                     ;yes, demolition
                cmp.w   #$40,D0                     ;.W?
                beq.s   o_dis6
                move.b  #'l',-1(A0)

                bra.s   o_dis7
o_dis6:         move.b  #'w',-1(A0)
o_dis7:         bclr    #15,D5
o_dis5:         tst.w   D5
                beq.s   o_disok                     ;If no operator / operand follows => end
                suba.l  A0,A2
                move.l  A2,D0
                addq.l  #7,D0
o_dis55:        move.b  #' ',(A0)+
                dbra    D0,o_dis55
                tst.w   D5
                beq.s   o_disok                     ;Defined disassembled, no parameters
                move.b  D5,D3                       ;push in D3
                movem.l D1-D5/A1-A3,-(SP)
                bsr.s   o_dout
                movem.l (SP)+,D1-D5/A1-A3
                tst.w   D7
                bne.s   o_dserr
                move.w  D5,D3
                lsr.w   #8,D3
                tst.b   D3
                beq.s   o_disok                     ;No operand
                movem.l D1/D5,-(SP)
                move.b  #',',(A0)+
                bsr.s   o_dout
                movem.l (SP)+,D1/D5
                tst.w   D7
                bne.s   o_dserr
o_disok:        clr.b   (A0)+                       ;0 as end marks
                move    #0,CCR
                rts
o_dserr:        move    #$FF,CCR                    ;Flags set as a sign for errors
                lea     spaced(A4),A0               ;A0 on initial value
                lea     2(A5),A6
                rts

o_dout:         clr.w   D7                          ;Erase error flag
                moveq   #$3F,D0                     ;possibly isolate length flag
                and.w   D3,D0                       ;to be treated word
                add.w   D0,D0                       ;two two as an index
                lea     o_doutt(PC),A2              ;Table start
                adda.w  -2(A2,D0.w),A2              ;more offsetadr
                jmp     (A2)                        ;and nothing like you

                BASE DC.W,o_doutt
o_doutt:        DC.W o_disa1,o_disa2,o_disa3
                DC.W o_disa4,o_disa5,o_disa6,o_disa7
                DC.W o_disa8,o_disa9,o_disaa,o_disab
                DC.W o_disac,o_disad,o_disae,o_disaf
                DC.W o_disa10,o_disa11,o_disa12,o_disa13
                DC.W o_disa14,o_disa15,o_disa16,o_disa17
                DC.W o_disa18,o_disa19,o_disa1a,o_disa1b
                DC.W o_disa1c,o_disa1d,o_disa1e,o_disa1f
                DC.W o_disa2x,o_disa2y,o_linea,o_linef
                DC.W o_movec1,o_movec2

o_movec1:       move.b  (A6),D1                     ;Output Data or Address Registers (MOVEC)
                lsr.w   #4,D1
                cmpi.b  #',',-1(A0)
                bne.s   o_movc1
                addq.l  #2,A6                       ;If target addressing, Word overlook
o_movc1:        btst    #3,D1
                bne     o_disa8                     ;address register
                bra     o_disa9                     ;Data register
o_movec2:       move.w  (A6),D0
                cmpi.b  #',',-1(A0)
                bne.s   o_movc2
                addq.l  #2,A6                       ;If target addressing, Word overlook
o_movc2:        and.w   #$0FFF,D0
                lea     o_movr1(PC),A2
o_movc3:        tst.w   (A2)+                       ;End of the tab list?
                bmi.s   o_movc4
                cmp.w   -2(A2),D0                   ;Register type found?
                beq.s   o_movc4
o_movc5:        tst.b   (A2)+                       ;Register name
                bne.s   o_movc5
                move.l  A2,D1
                addq.l  #1,D1                       ;EVEN
                and.w   #$FFFE,D1
                movea.l D1,A2
                bra.s   o_movc3                     ;to the next register
o_movc4:        move.b  (A2)+,(A0)+                 ;Copy register names
                bne.s   o_movc4
                subq.l  #1,A0                       ;Pointer back to the zero byte
                rts
o_movr1:        DC.B 0,0,'SFC',0
                EVEN
                DC.B 0,1,'DFC',0
                EVEN
                DC.B 0,2,'CACR',0
                EVEN
                DC.B 8,0,'USP',0
                EVEN
                DC.B 8,1,'VBR',0
                EVEN
                DC.B 8,2,'CAAR',0
                EVEN
                DC.B 8,3,'MSP',0
                EVEN
                DC.B 8,4,'ISP',0
                EVEN
                DC.B -1,-1,'???',0
                EVEN

o_linea:        move.w  D1,D0
                and.w   #$0FF0,D0
                bne.s   o_lina2
                bsr     o_disa3                     ;#3-Bit-number
                lsl.w   #4,D1
                lea     o_lineat(PC,D1.w),A2
                move.b  #' ',(A0)+
                move.b  #';',(A0)+
o_lina3:        moveq   #15,D0
o_lina1:        move.b  (A2)+,(A0)+
                dbra    D0,o_lina1
                rts
o_lina2:        move.b  #'#',(A0)+
                and.l   #$0FFF,D1
                jmp     numout

                DXSET 16,' '
o_lineat:       DX.B 'Init'
                DX.B 'Put pixel'
                DX.B 'Get pixel'
                DX.B 'Line'
                DX.B 'Horizontal line'
                DX.B 'Filled rectangle'
                DX.B 'Filled polygon'
                DX.B 'Bitblt'
                DX.B 'Textblt'
                DX.B 'Show mouse'
                DX.B 'Hide mouse'
                DX.B 'Transform mouse'
                DX.B 'Undraw sprite'
                DX.B 'Draw sprite'
                DX.B 'Copy raster form'
                DX.B 'Seedfill'

o_linef:        move.w  D1,D0
                bsr     o_lina2                     ;12-Bit-number output
                move.w  D1,D0
                move.b  #' ',(A0)+
                move.b  #';',(A0)+
                bclr    #0,D0                       ;RTS?
                bne.s   o_linf1                     ;yes! =>
                move.l  linef_base(A4),D1
                beq.s   o_linf2                     ;unknown TOS-Version
                movea.l D1,A2
                move.l  0(A2,D0.w),D1
                move.b  #'j',(A0)+
                move.b  #'s',(A0)+
                move.b  #'r',(A0)+
                move.b  #' ',(A0)+
                lea     _illegal(PC),A2
                cmp.w   max_linef(A4),D0
                bgt     o_lina3                     ;invalid
                btst    #1,D0
                bne     o_lina3                     ;invalid
                jmp     numout
o_linf1:        move.b  #'r',(A0)+
                move.b  #'t',(A0)+
                move.b  #'s',(A0)+
                tst.w   D0
                beq.s   o_disa5_2
                move.b  #' ',(A0)+
                lsl.w   #2,D0
                move.w  D0,D1
                bsr     o_turn                      ;and reflect before
                bra     o_msko                      ;Output tab
o_linf2:        subq.l  #2,A0
                clr.b   (A0)+
                rts

o_disa1:        and.l   #$FF,D1
o_disff:        move.b  #'#',(A0)+
                bra     _numout

o_disa2:        moveq   #$3F,D1
                and.w   (A6)+,D1
                bra.s   o_disff

o_disa3:        moveq   #$0F,D0
                and.l   D0,D1
                bra.s   o_disff

o_disa1f:       and.l   #$FFFF,D1
                jmp     hexout                      ;z.B. DC.W $A000

o_disa4:        moveq   #7,D0
                rol.w   D0,D1
                and.l   D0,D1
                bne.s   o_disa1
                moveq   #8,D1
                bra.s   o_disa1

o_disa5:        move.b  #'-',(A0)+
o_disa5_1:      move.b  #'(',(A0)+
                bsr.s   o_disa8
                move.b  #')',(A0)+
o_disa5_2:      rts

o_disa6:        move.b  #'(',(A0)+
                bsr.s   o_disa8
                move.b  #')',(A0)+
                move.b  #'+',(A0)+
                rts

o_disa7:        move.b  #'.',-5(A0)
                move.b  #'w',-4(A0)
                tst.b   D1                          ;Relative address distance follows
                beq.s   o_disa1e
                cmp.b   #-1,D1
                beq.s   o_dislng
                move.b  #'s',-4(A0)
                ext.w   D1
                ext.l   D1
                add.l   A6,D1
                bra.s   _symbol_numout
o_dislng:       move.b  #'l',-4(A0)
                move.l  (A6),D1
                add.l   A6,D1
                addq.l  #4,A6
                bra.s   _symbol_numout
o_disa1e:       move.w  (A6),D1
                ext.l   D1
                add.l   A6,D1
                addq.l  #2,A6
_symbol_numout: jmp     symbol_numout

o_disa8:        moveq   #7,D0
                and.w   D0,D1
                cmp.w   D0,D1
                bne.s   o_disa8_1
                move.b  #'S',(A0)+
                move.b  #'P',(A0)+
                rts
o_disa8_1:      move.b  #'A',(A0)+
                add.w   #'0',D1
                move.b  D1,(A0)+
                rts

o_disa9:        move.b  #'D',(A0)+
                and.w   #7,D1
                add.w   #'0',D1
                move.b  D1,(A0)+
                rts

o_disaa:        rol.w   #7,D1
                bra.s   o_disa8

o_disab:        rol.w   #7,D1
                bra.s   o_disa9

o_disa14:       rol.w   #7,D1
                bra     o_disa5

o_disa15:       rol.w   #7,D1
                bra     o_disa6

o_disa16:       move.b  #'C',(A0)+
                move.b  #'C',(A0)+
                move.b  #'R',(A0)+
                rts

o_disa17:       move.b  #'S',(A0)+
                move.b  #'R',(A0)+
                rts

o_disa18:       move.b  #'U',(A0)+
                move.b  #'S',(A0)+
                move.b  #'P',(A0)+
                rts

o_disa19:       move.b  #'#',(A0)+
                moveq   #0,D1
                move.w  (A6)+,D1
                bra.s   _numout

o_disa2y:       cmpi.w  #$4848,-2(A6)               ;BKPT #0 ?
                bne.s   o_disa2y1
                subq.l  #8,A0
                move.b  #'B',(A0)+
                move.b  #'R',(A0)+
                move.b  #'E',(A0)+
                move.b  #'A',(A0)+                  ;bkpt #0 in BREAKPT change
                move.b  #'K',(A0)+
                move.b  #'P',(A0)+
                move.b  #'T',(A0)+
                move.b  #' ',(A0)+
                move.b  #'''',(A0)+
                moveq   #45,D1                      ;Transfer a maximum of 46 characters
o_disa2y0:      move.b  (A6)+,(A0)+                 ;Copy comment
                dbeq    D1,o_disa2y0
                beq.s   o_disa2y2                   ;Stringing? Yes!=>
o_disa2y3:      tst.b   (A6)+                       ;otherwise to the string end
                bne.s   o_disa2y3
o_disa2y2:      move.b  #'''',-1(A0)
                move.l  A6,D1
                addq.l  #1,D1
                and.b   #$FE,D1                     ;EVEN
                movea.l D1,A6
                rts                                 ;Complete!
o_disa2y1:      moveq   #7,D0
                and.l   D0,D1
                bra.s   o_disa1aa
o_disa1a:       moveq   #0,D1
                move.w  (A6)+,D1
                and.w   #$FF,D1
o_disa1aa:      move.b  #'#',(A0)+
_numout:        jmp     numout                      ;Spend number

o_disa1b:       tst.w   4(A1)                       ;Addressing types word from the table
                bmi.s   o_disa1b_1                  ;is equipped with a <ln> field
                move.w  4(A1),D1
                btst    #14,D1                      ;Bit = 0 is called .b
                beq.s   o_disa1a
                btst    #6,D1
                beq     o_disa19                    ;is .w
                bne.s   o_disa1b_2
o_disa1b_1:     and.w   #$C0,D1                     ;<ln> -feld isolate
                beq.s   o_disa1a
                cmp.w   #$40,D1
                beq     o_disa19                    ;is .w
o_disa1b_2:     move.b  #'#',(A0)+
                move.l  (A6)+,D1                    ;is .l
                bra     _symbol_numout

o_disa1d:       rol.w   #7,D1
o_disa1c:       move.w  D1,-(SP)
                move.w  (A6)+,D1
                bpl.s   o_disa1c1
                neg.w   D1                          ;signed!
                move.b  #'-',(A0)+
o_disa1c1:      bsr     o_disa1f
                move.w  (SP)+,D1
                move.b  #'(',(A0)+
                bsr     o_disa8
                move.b  #')',(A0)+
                rts

o_disea:        moveq   #0,D7                       ;Erase error flag
                clr.w   D3                          ;Clear mask
                moveq   #$38,D0
                and.w   D1,D0
                cmp.w   #$38,D0
                beq.s   o_eafn1                     ;is a 111xxx style
                lsr.w   #3,D0
                bra.s   o_eafn2
o_eafn1:        moveq   #7,D0
                and.w   D1,D0
                cmp.w   #5,D0
                bhs.s   o_eafn3
                addq.w  #7,D0
o_eafn2:        bset    D0,D3
o_eafn3:        and.w   (SP)+,D3                    ;Allowed to compare mask with address type
                beq.s   o_disea1
                cmp.w   #$01,D3                     ;Dn
                beq     o_disa9
                cmp.w   #$02,D3                     ;An
                beq     o_disa8
                cmp.w   #$04,D3                     ;(An)
                beq     o_disa5_1
                cmp.w   #$08,D3                     ;(An)+
                beq     o_disa6
                cmp.w   #$10,D3                     ;-(An)
                beq     o_disa5
                cmp.w   #$20,D3                     ;d(An)
                beq.s   o_disa1c
                cmp.w   #$40,D3                     ;d(An,Rx)
                beq.s   o_disa20
                cmp.w   #$80,D3                     ;$xxxx
                beq.s   o_disa21
                cmp.w   #$0100,D3                   ;$xxxxxxxx
                beq.s   o_disa22
                cmp.w   #$0200,D3                   ;d(PC)
                beq     o_disa23
                cmp.w   #$0400,D3                   ;d(PC,Rx)
                beq     o_disa24
                cmp.w   #$0800,D3                   ;#
                beq     o_disa1b
o_disea1:       st      D7                          ;Error occurred
                rts

o_disa20:       move.w  D1,-(SP)                    ;d(An,Rn.x)
                move.w  (A6)+,D1
                move.w  D1,-(SP)
                and.l   #$FF,D1
                tst.b   D1
                bpl.s   o_disa200                   ;signed!
                neg.b   D1
                move.b  #'-',(A0)+
o_disa200:      jsr     numout
                move.w  2(SP),D1                    ;EMERGES D1 (oncode) Holes
                move.b  #'(',(A0)+
                bsr     o_disa8                     ;Spend
                move.w  (SP)+,D1
                addq.l  #2,SP                       ;Normalize stack
                bra     o_disa24_1

o_disa21:       move.w  (A6)+,D1                    ;$xxxx
                ext.l   D1
                bsr.s   sym_numout
                move.b  #'.',(A0)+
                move.b  #'w',(A0)+
                rts

o_disa22:       move.l  (A6)+,D1                    ;$xxxxxxxx
sym_numout:     movem.l D0-D1/A1,-(SP)
                swap    D1
                cmp.w   #$FF,D1
                bne.s   sym_numout0
                ext.w   D1
sym_numout0:    swap    D1
                move.l  sym_buffer(A4),D0           ;Symbol table loaded?
                beq.s   sym_numout1                 ;No!=>
                movea.l D0,A1
                move.w  bugaboo_sym(A4),D0          ;Use internal symbol table?
                bne.s   sym_numout1                 ;No!=>
                move.w  sym_anzahl(A4),D0
                bra.s   sym_numout5
sym_numout3:    cmp.l   (A1),D1                     ;Value found?
                bne.s   sym_numout4                 ;No!=>
                addq.l  #8,A1
sym_numout6:    move.b  (A1)+,(A0)+                 ;Copy symbol names
                bne.s   sym_numout6
                subq.l  #1,A0                       ;Pointer to the zero byte
                movem.l (SP)+,D0-D1/A1
                rts                                 ;that's it
sym_numout4:    lea     32(A1),A1                   ;Pointer to the next symbol
sym_numout5:    dbra    D0,sym_numout3              ;already tested all entries?
sym_numout1:    movem.l (SP)+,D0-D1/A1
sym_numout2:    bra     _symbol_numout

o_disa23:       move.w  (A6),D1                     ;d(PC)
                ext.l   D1
                add.l   A6,D1
                addq.w  #2,A6
                bsr.s   sym_numout2
                move.b  #'(',(A0)+
                move.b  #'P',(A0)+
                move.b  #'C',(A0)+
                move.b  #')',(A0)+
                rts

o_disa24:       move.w  (A6),D1                     ;d(PC,Rn.x)
                move.w  D1,-(SP)
                ext.w   D1
                ext.l   D1
                add.l   A6,D1
                addq.w  #2,A6
                bsr     _symbol_numout
                move.w  (SP)+,D1                    ;EMERGES D1(oncode) Holes
                move.b  #'(',(A0)+
                move.b  #'P',(A0)+
                move.b  #'C',(A0)+
o_disa24_1:     move.b  #',',(A0)+
                move.b  #'D',(A0)+
                tst.w   D1
                bpl.s   o_disa24_2                  ;is Dn
                move.b  #'A',-1(A0)
o_disa24_2:     move.w  D1,D0
                rol.w   #4,D0
                and.w   #7,D0
                add.w   #'0',D0
                move.b  D0,(A0)+
                move.b  #'.',(A0)+
                move.b  #'W',(A0)+
                btst    #11,D1
                beq.s   o_disa24_3
                move.b  #'L',-1(A0)
o_disa24_3:     move.b  #')',(A0)+
                rts
o_disad:        move.w  #$0FFF,-(SP)
                bra     o_disea
o_disae:        move.w  #$01FF,-(SP)
                bra     o_disea
o_disaf:        move.w  #$01FD,-(SP)
                bra     o_disea

o_disa10:       move.w  #$0FFD,-(SP)
                bra     o_disea
o_disa11:       move.w  #$01FC,-(SP)
                bra     o_disea
o_disa12:       move.w  #$07FF,-(SP)
                bra     o_disea
o_disa2x:       move.w  #$07FD,-(SP)                ;Btst #x,n(PC)
                bra     o_disea
o_disa13:       move.w  #$07E4,-(SP)
                bra     o_disea
o_disac:        move.w  D1,D0
                lsr.w   #3,D0
                and.w   #$38,D0                     ;Isolate bits
                rol.w   #7,D1
                and.w   #7,D1
                or.w    D0,D1
                and.w   #$3F,D1                     ;Isolate valid bits
                move.w  #$01FD,-(SP)
                bra     o_disea
                ENDPART

********************************************************************************
* Expression                                                                   *
********************************************************************************
                PART 'convert_formel'
convert_formel: movea.l #formel,A6
                adda.l  A4,A6
                movea.l #linebuf,A5
                adda.l  A4,A5
                moveq   #0,D7                       ;top stack level
                bsr     getb
                bsr.s   un_if
                move.w  #$4A80,(A1)+                ;TST.L D0
                move.w  #$4E75,(A1)+                ;and attach an RTS
                clr.l   (A1)                        ;(because it looks better)
                rts

un_if:          bsr     un_a
un_ifl0:        moveq   #0,D1
un_ifl:         cmp.b   #'=',D0                     ;Get the condition
                bne.s   un_ifl1
                bset    #1,D1
                bne     synerr
                bsr     getb
                bra.s   un_ifl
un_ifl1:        cmp.b   #'<',D0
                bne.s   un_ifl2
                bset    #2,D1
                bne     synerr
                bsr     getb
                bra.s   un_ifl
un_ifl2:        cmp.b   #'>',D0
                bne.s   un_ifl3
                bset    #3,D1
                bne     synerr
                bsr     getb
                bra.s   un_ifl

un_cond:        DC.W 0,$57C0,$54C0,$52C0,$53C0,$55C0,$56C0,$FFFF
;                       SEQ   SHS   SHI   SLS   SLO   SNE

un_ifl3:        move.w  un_cond(PC,D1.w),D1
                bmi     synerr                      ;<=> stated
                beq     un_ifen
                move.w  D1,un_ifl4+2
                bsr     un_opti
                move.b  #10,-(A6)
                move.b  D7,-(A6)
                bsr     un_a
                moveq   #19,D1
                bsr     un_put
                bne.s   un_ifl0

                movea.l -4(A5),A2                   ;Address of the last command
                cmpi.w  #$203C,(A2)                 ;MOVE.L #,D0
                bne.s   un_ifl5
                move.l  2(A2),D1
                movea.l A2,A1
                bra.s   un_ifl7
un_ifl5:        cmpi.b  #$70,(A2)+                  ;MOVEQ #,D0
                bne     un_ifl6                     ;Then just not optimized ...
                move.b  (A2),D1
                ext.w   D1
                ext.l   D1
un_ifl7:        subq.l  #4,A5                       ;and discard the command
                lea     un_ifl4+2(PC),A3
                btst    #1,(A3)
                beq.s   un_if13                     ;BEQ or BNE are not inverted
                btst    #2,(A3)
                bne.s   un_if12
un_if13:        bchg    #0,(A3)                     ;conditionInvertieren
un_if12:        movea.l -4(A5),A2                   ;Address of the penultimate command
                movea.l A2,A1
                cmpi.w  #$2F3C,(A2)                 ;MOVE.L #,-(SP)
                beq     un_if11
                cmpi.w  #$2F29,(A2)                 ;MOVE.L n(A1),-(SP)
                beq     un_ifl9
                cmpi.w  #$2F00,(A2)                 ;MOVE.L D0,-(SP)
                bne     int_err                     ;How could the command only occur ???
                movea.l -8(A5),A2
                cmpi.w  #$48C0,(A2)                 ;EXT.L D0
                bne.s   un_if15                     ;No!
                move.w  #$0C40,D2                   ;CMPI.W #,D0
                movea.l A2,A1
                subq.l  #8,A5
                movea.l -4(A5),A2
                cmpi.w  #$4880,(A2)                 ;EXT.L D0 discard
                bne.s   un_if16
                move.w  #$0C00,D2                   ;CMPI.B #,D0
                movea.l A2,A1
                subq.l  #4,A5
                andi.w  #$FF,D1                     ;reduce byte
                movea.l -4(A5),A2
                cmpi.w  #$1010,(A2)                 ;MOVE.B (A0),D0
                bne.s   un_if17
                move.w  #$0C10,D2                   ;CMPI.B #,(A0)
                movea.l A2,A1
                subq.l  #4,A5
un_if17:        move.l  A1,(A5)+
                move.w  D2,(A1)+
                move.w  D1,(A1)+
                bra.s   un_if14
un_if16:        movea.l -4(A5),A2
                cmpi.w  #$3010,(A2)                 ;MOVE.W (A0),D0
                bne.s   un_if18
                move.w  #$0C50,D2                   ;CMPI.W #,(A0)
                movea.l A2,A1
                subq.l  #4,A5
un_if18:        move.l  A1,(A5)+
                move.w  D2,(A1)+
                move.w  D1,(A1)+
                bra.s   un_if14
un_if15:        move.w  #$0C80,D2                   ;CMPI.L #,D0
                cmpi.w  #$2010,(A2)                 ;MOVE.L (A0),D0
                bne.s   un_if19
                move.w  #$0C90,D2                   ;CMPI.L #,(A0)
                subq.l  #4,A5
                movea.l A2,A1
un_if19:        move.l  A1,-(A5)
                addq.l  #4,A5
                move.w  D2,(A1)+
                move.l  D1,(A1)+
                bra.s   un_if14
un_if11:        move.w  #$203C,(A1)                 ;MOVE.L #,D0
                addq.l  #6,A1                       ;Maintain value
                move.l  A1,(A5)+
                move.w  #$0C80,(A1)+                ;CMPI.L #,D0
                move.l  D1,(A1)+
                bra.s   un_if14
un_ifl9:        move.w  #$0CA9,(A1)+                ;CMPI.L #,n(A1)
                move.w  (A1),D2
                move.l  D1,(A1)+
                move.w  D2,(A1)+
                bra.s   un_if14

un_ifl6:        move.l  A1,(A5)+
                move.w  #$B09F,(A1)+                ;CMP.L (SP)+,D0
un_if14:        move.l  A1,(A5)+
un_ifl4:        move.w  #$4E71,(A1)+                ;Scc D0
                move.l  A1,(A5)+
                move.w  #$4880,(A1)+                ;EXT.W D0
                move.l  A1,(A5)+
                move.w  #$48C0,(A1)+                ;EXT.L D0
                bra     un_ifl0
un_ifen:        rts

un_a:           bsr     un_eausd
un_al:          cmp.b   #'+',D0                     ;Addition
                bne.s   un_a1
                bsr     un_opti
                move.b  #20,-(A6)
                move.b  D7,-(A6)
                bsr     getb
                bsr     un_eausd
                moveq   #29,D1
                bsr     un_put
                bne.s   un_al
                move.w  #$D09F,D2                   ;ADD.L (SP)+,D0
                move.w  #$0680,D3                   ;ADDI.L #,D0
                move.w  #$5080,D4                   ;ADDQ.L #,D0
                moveq   #0,D5
                bsr     un_o_a
                bra.s   un_al
un_a1:          cmp.b   #'-',D0                     ;subtraction
                bne.s   un_a2
                bsr     un_opti
                move.b  #21,-(A6)
                move.b  D7,-(A6)
                bsr     getb
                bsr     un_eausd
                moveq   #29,D1
                bsr     un_put
                bne.s   un_al
                move.w  #$909F,D2                   ;SUB.L (SP)+,D0
                move.w  #$0480,D3                   ;SUBI.L #,D0
                move.w  #$5180,D4                   ;SUBQ.L #,D0
                moveq   #0,D5
                bsr     un_o_a
                cmp.w   -2(A1),D2                   ;Not optimizable?
                bne.s   un_a10                      ;but!=>
                move.w  #$4480,(A1)+                ;NEG.L D0
un_a10:         bra.s   un_al
un_a2:          cmp.b   #'|',D0                     ;OR
                bne.s   un_a3
                bsr     un_opti
                move.b  #22,-(A6)
                move.b  D7,-(A6)
                bsr     getb
                bsr     un_eausd
                moveq   #29,D1
                bsr     un_put
                bne     un_al
                move.w  #$809F,D2                   ;OR.L (SP)+,D0
                move.w  #$80,D3                     ;ORI.L #,D0
                moveq   #0,D4
                moveq   #0,D5
                bsr     un_o_a
                bra     un_al
un_a3:          cmp.b   #'^',D0                     ;EOR
                bne.s   un_a4
                bsr     un_opti
                move.b  #23,-(A6)
                move.b  D7,-(A6)
                bsr     getb
                bsr     un_eausd
                moveq   #29,D1
                bsr     un_put
                bne     un_al
                move.l  #$221FB181,D2               ;MOVE.L (SP)+,D1:EOR.L D0,D1
                move.w  #$0A80,D3
                moveq   #0,D4
                moveq   #-1,D5
                bsr     un_o_a
                bra     un_al
un_a4:          cmp.b   #'<',D0                     ;SHL
                bne.s   un_a5
                cmpi.b  #'<',(A0)
                bne.s   un_a5
                bsr     un_opti
                move.b  #24,-(A6)
                move.b  D7,-(A6)
                addq.l  #1,A0
                bsr     getb
                bsr.s   un_eausd
                moveq   #29,D1
                bsr     un_put
                bne     un_al
                move.l  #$221FE1A9,D2               ;MOVE.L (SP)+,D1:LSL.L D0,D1
                move.l  D2,D3
                move.l  #$E188,D4                   ;LSL.L #,D0
                moveq   #1,D5
                bsr     un_o_a
                bra     un_al
un_a5:          cmp.b   #'>',D0                     ;SHR
                bne.s   un_aend
                cmpi.b  #'>',(A0)
                bne.s   un_aend
                bsr     un_opti
                move.b  #25,-(A6)
                move.b  D7,-(A6)
                addq.l  #1,A0
                bsr     getb
                bsr.s   un_eausd
                moveq   #29,D1
                bsr     un_put
                bne     un_al
                move.l  #$221FE0A9,D2               ;MOVE.L (SP)+,D1:LSR.L D0,D1
                move.l  D2,D3
                move.l  #$E088,D4                   ;LSR.L #,D0
                moveq   #1,D5
                bsr     un_o_a
                bra     un_al
un_aend:        rts

un_eausd:       bsr     un_term
un_eal:         cmp.b   #'*',D0                     ;multiplication
                bne.s   un_ea1
                bsr     un_opti
                move.b  #30,-(A6)
                move.b  D7,-(A6)
                bsr     getb
                bsr     un_term
                moveq   #39,D1
                bsr     un_put
                bne.s   un_eal
                move.l  A1,(A5)+
                move.w  #$221F,(A1)+                ;MOVE.L (SP)+,D1
                move.l  A1,(A5)+
                move.w  #$C0C1,(A1)+                ;MULU   D1,D0
                bra.s   un_eal
un_ea1:         cmp.b   #'/',D0                     ;Division
                bne.s   un_ea2
                bsr     un_opti
                move.b  #31,-(A6)
                move.b  D7,-(A6)
                bsr     getb
                bsr     un_term
                moveq   #39,D1
                bsr     un_put
                bne.s   un_eal
                move.l  A1,(A5)+
                move.w  #$221F,(A1)+                ;MOVE.L (SP)+,D1
                move.l  A1,(A5)+
                move.w  #$82C0,(A1)+                ;DIVU D0,D1
                move.l  A1,(A5)+
                move.w  #$2001,(A1)+                ;MOVE.L D1,D0
                move.l  A1,(A5)+
                move.w  #$48C0,(A1)+                ;EXT.L D0
                bra.s   un_eal
un_ea2:         cmp.b   #'&',D0
                bne.s   un_ea3
                bsr     un_opti
                move.b  #32,-(A6)
                move.b  D7,-(A6)
                bsr     getb

                bsr.s   un_term
                moveq   #39,D1
                bsr     un_put
                bne     un_eal
                move.w  #$C09F,D2                   ;AND.L (SP)+,D0
                move.w  #$0280,D3                   ;ANDI.L #,D0
                moveq   #0,D4
                moveq   #0,D5
                bsr     un_o_a
                bra     un_eal
un_ea3:         cmp.b   #'%',D0
                bne.s   un_eaend
                bsr     un_opti
                move.b  #33,-(A6)
                move.b  D7,-(A6)
                bsr     getb
                bsr.s   un_term
                moveq   #39,D1
                bsr     un_put
                bne     un_eal
                move.l  A1,(A5)+
                move.w  #$221F,(A1)+                ;MOVE.L (SP)+,D1
                move.l  A1,(A5)+
                move.w  #$22C0,(A1)+                ;DIVU D0,D1
                move.l  A1,(A5)+
                move.w  #$2001,(A1)+                ;MOVE.L D1,D0
                move.l  A1,(A5)+
                move.w  #$4240,(A1)+                ;CLR.W D0
                move.l  A1,(A5)+
                move.w  #$4840,(A1)+                ;SWAP D0
                bra     un_eal
un_eaend:       rts

un_term:        cmp.b   #'!',D0
                bne.s   un_t1
                move.b  #40,-(A6)
                move.b  D7,-(A6)
                bsr     getb
                bsr.s   un_t1
                moveq   #49,D1
                bsr     un_put
                bne.s   un_t0
                move.l  A1,(A5)+
                move.w  #$57C0,(A1)+                ;SEQ D0
                move.l  A1,(A5)+
                move.w  #$4880,(A1)+                ;EXT.W D0
                move.l  A1,(A5)+
                move.w  #$48C0,(A1)+                ;EXT.L D0
un_t0:          rts

un_t1:          cmp.b   #'~',D0
                bne.s   un_ter1
                move.b  #41,-(A6)
                move.b  D7,-(A6)
                bsr     getb
                bsr.s   un_ter1
                moveq   #49,D1
                bsr     un_put
                bne.s   un_ter0
                cmpi.b  #$70,-2(A1)                 ;MOVEQ?
                bne.s   un_ter10
                not.b   -1(A1)
                bra.s   un_ter0
un_ter10:       cmpi.w  #$203C,-6(A1)
                bne.s   un_ter11
                not.l   -4(A1)
                bra.s   un_ter0
un_ter11:       move.l  A1,(A5)+
                move.w  #$4680,(A1)+                ;NOT.L D0
un_ter0:        rts

un_ter1:        cmp.b   #'-',D0
                beq.s   un_ter3
                cmp.b   #'+',D0
                bne.s   un_ter2
                bsr     getb                        ;Positive sign
un_ter2:        bsr.s   un_fakt
                rts
un_ter3:        bsr     getb                        ;Negative sign
                move.b  #42,-(A6)
                move.b  D7,-(A6)
                bsr.s   un_fakt
                moveq   #49,D1
                bsr     un_put
                bne.s   un_ter4
                cmpi.b  #$70,-2(A1)                 ;MOVEQ?
                bne.s   un_ter30
                neg.b   -1(A1)
                bra.s   un_ter4
un_ter30:       cmpi.w  #$203C,-6(A1)
                bne.s   un_ter31
                neg.l   -4(A1)
                bra.s   un_ter4
un_ter31:       move.l  A1,(A5)+
                move.w  #$4480,(A1)+                ;NEG.L D0
un_ter4:        rts

un_fakt:        cmp.b   #'(',D0
                beq.s   un_fakt1
                cmp.b   #'{',D0
                beq.s   un_fakt3
                bsr     get_pointer                 ;Is it a pointer on a variable?
                beq.s   un_fakt0                    ;End, if so!=>
                jsr     get_zahl                    ;otherwise bring normal number to D1
                move.l  D1,D2
                ext.w   D2
                ext.l   D2
                cmp.l   D1,D2
                bne.s   un_fakt2
                move.l  A1,(A5)+
                ori.w   #$7000,D1                   ;MOVEQ #number,D0
                move.w  D1,(A1)+
un_fakt0:       rts
un_fakt2:       move.l  A1,(A5)+
                move.w  #$203C,(A1)+                ;MOVE.L #number,D0
                move.l  D1,(A1)+                    ;Use the number
                rts
un_fakt1:       bsr     getb                        ;Clip
                addq.w  #1,D7                       ;Increase stack level
                bsr     un_if                       ;Evaluate expression in the bracket
                cmp.b   #')',D0
                bne.s   _misbrak                    ;Brace must follow
                subq.w  #1,D7                       ;A stack plane back
                bsr     getb
                cmp.b   #'.',D0
                beq.s   un_fak1                     ;Extension available
un_fak0:        rts
_misbrak:       jmp     misbrak
un_fak1:        bsr     getb
                cmp.b   #'L',D0
                beq.s   un_fak0
                move.l  A1,(A5)+
                move.w  #$48C0,(A1)+                ;EXT.L D0
                cmp.b   #'W',D0
                beq.s   un_fak0
                move.l  A1,(A5)+
                subq.l  #2,A1
                move.l  #$488048C0,(A1)+            ;EXT.W D0:EXT.L D0
                cmp.b   #'B',D0
                bne     _synerr
                rts

un_fakt3:       bsr     getb
                addq.w  #1,D7
                bsr     un_if
                cmp.b   #'}',D0
                bne.s   _misbrak
                subq.w  #1,D7
                bsr     getb
                movea.l -4(A5),A2                   ;Address of the last command
                move.l  A1,(A5)+
                move.w  #$2040,(A1)+                ;MOVEA.L D0,A0 is default

                cmpi.w  #$203C,(A2)                 ;MOVE.L #,D0?
                bne.s   un_fakt7
                subq.l  #4,A5                       ;MOVEA.L D0,A0 Sweet again
                movea.l A2,A1
                move.w  #$41F9,(A1)+                ;LEA Adr.L,A0 insert
                move.w  (A1),D1                     ;Get upper word of value
                beq.s   un_fakt9
                addq.b  #1,D1                       ;Short possible?
                bne.s   un_fak10
un_fakt9:       move.w  #$41F8,(A2)                 ;to LEA Adr.W,A0
                move.w  2(A1),(A1)+                 ;Attend address on short
                subq.l  #4,A1
un_fak10:       addq.l  #4,A1                       ;Address remains in it
                bra.s   un_fakt6

un_fakt7:       cmpi.w  #$2029,(A2)                 ;MOVE.L n(A1),D0
                bne.s   un_fakt8
                ori.w   #$40,(A2)                   ;to MOVEA.L n(A1),A0
                subq.l  #4,A5
                subq.l  #2,A1                       ;MOVEA.L D0,A0 remove
                bra.s   un_fakt6

un_fakt8:       cmpi.w  #$2010,(A2)                 ;MOVE.L (A0),D0
                bne.s   un_fakt6
                ori.w   #$40,(A2)                   ;to MOVEA.L (A0),A0
                subq.l  #4,A5
                subq.l  #2,A1                       ;MOVEA.L D0,A0 remove

un_fakt6:       move.l  A1,(A5)+
                move.w  #$3010,(A1)+                ;MOVE.W (A0),D0
                move.l  A1,(A5)+
                move.w  #$48C0,(A1)+                ;EXT.L D0
                cmp.b   #'.',D0
                bne.s   un_fakt5
                bsr     getb
                cmp.b   #'W',D0
                beq.s   un_fakt4
                move.w  #$1010,-4(A1)               ;MOVE.B (A0),D0
                move.w  #$4880,-2(A1)               ;EXT.W D0
                move.l  A1,(A5)+
                move.w  #$48C0,(A1)+                ;EXT.L D0
                cmp.b   #'B',D0
                beq.s   un_fakt4
                move.w  #$2010,-6(A1)               ;MOVE.L (A0),D0
                subq.l  #8,A5                       ;EXT.W D0 & EXT.L D0 take out of the list
                subq.l  #4,A1
                cmp.b   #'L',D0
                bne.s   synterr
un_fakt4:       bsr     getb
un_fakt5:       rts
synterr:        jmp     synerr

;Stack testen
un_put:         cmp.b   (A6),D7                     ;Stack depth equal?
                bne.s   un_put1                     ;End, if not
                moveq   #0,D2
                move.b  1(A6),D2                    ;Get the operation
                cmp.b   D2,D1
                blo.s   un_put1                     ;I did not grown the operation
                addq.l  #2,A6                       ;Stack
                move    #4,CCR                      ;Set z-flag
un_put1:        rts

un_opti:        movea.l -4(A5),A2                   ;Get address of the last command
                cmpi.w  #$203C,(A2)                 ;MOVE.L #,D0
                beq.s   un_opt1
                cmpi.w  #$2029,(A2)                 ;MOVE.L n(A1),D0
                beq.s   un_opt2
                move.l  A1,(A5)+
                move.w  #$2F00,(A1)+                ;MOVE.L D0,-(SP)
                rts
un_opt1:        move.w  #$2F3C,(A2)                 ;to MOVE.L #,-(SP)
                rts
un_opt2:        move.w  #$2F29,(A2)                 ;to MOVE.L n(A1),-(SP)
                rts

un_o_a:         movea.l -4(A5),A2                   ;Address of the last command
                move.b  1(A2),D1
                ext.w   D1
                ext.l   D1
                cmpi.b  #$70,(A2)                   ;MOVEQ #,D0
                beq.s   un_o_a1
                move.l  2(A2),D1
                cmpi.w  #$203C,(A2)                 ;MOVE.L #,D0
                bne.s   un_o_ae                     ;No optimization possible
un_o_a1:        movea.l -8(A5),A2
                cmpi.w  #$2F00,(A2)                 ;MOVE.L D0,-(SP)?
                bne.s   un_o_ae                     ;No optimization possible
                subq.l  #4,A5
                movea.l A2,A1
                tst.l   D1
                bmi.s   un_o_a2                     ;0-8? Optimize to Quick (0 falls away)
                cmp.l   #9,D1
                blo.s   un_o_a3
un_o_a2:        subq.w  #1,D5
                beq.s   un_o_ag
                move.w  D3,(A1)+                    ;???I.L #,D0 to take
                move.l  D1,(A1)+
                rts
un_o_a3:        tst.w   D4                          ;OPCODE for Quick available?
                beq.s   un_o_a2                     ;No!^^^
                tst.l   D1
                beq.s   un_o_a4                     ;NULL as a parameter?
                andi.w  #7,D1
                ror.w   #7,D1
                or.w    D1,D4
                move.w  D4,(A1)+                    ;Write AddQ / SubQ
                rts
un_o_a4:        subq.l  #4,A5                       ;At zero as parameters no opcode
                movea.l A2,A1                       ;to produce
                rts

un_o_ae:        tst.l   D5
                bne.s   un_o_af
                move.l  A1,(A5)+                    ;Remember address
                move.w  D2,(A1)+                    ;???.L (SP)+,D0 to take
                rts
un_o_af:        move.l  A1,(A5)+
                move.l  D2,(A1)+
                rts

un_o_ag:        movea.l -8(A5),A2
                cmpi.b  #$70,(A2)
                beq.s   un_o_ak
                cmpi.w  #$203C,(A2)
                bne.s   un_o_al
un_o_ak:        ori.b   #2,(A2)                     ;instead of D0 now D1
un_o_al:        move.l  A1,(A5)+
                move.l  D1,D2
                ext.w   D2
                ext.l   D2
                cmp.l   D1,D2
                bne.s   un_o_ah
                ori.w   #$7000,D1                   ;MOVEQ #,D1
                move.w  D1,(A1)+
                bra.s   un_o_ai
un_o_ah:        move.w  #$203C,(A1)+                ;MOVE.L #,D1
                move.l  D1,(A1)+
un_o_ai:        move.l  A1,(A5)+
                move.w  D3,(A1)+
                rts
                ENDPART

********************************************************************************
* Claiming the lower routines in the "get"-Routine                             *
********************************************************************************
getb:           jmp     get

********************************************************************************
* Pointer variable? (Part of Convert_formula)                                  *
********************************************************************************
                PART 'get_pointer'
get_pointer:    movea.l A0,A2                       ;Act.position right
                move.w  D0,D2                       ;Akt.mark save
                cmp.b   #'P',D0
                bne.s   get_po1
                bsr.s   getb
                cmp.b   #'C',D0
                bne     get_poe                     ;End, not found
                moveq   #64,D1                      ;Offset for PC
                bra     get_pof                     ;found and now generate code
get_po1:        cmp.b   #'C',D0
                bne.s   get_po2
                bsr.s   getb
                cmp.b   #'C',D0
                bne.s   get_poe
                bsr.s   getb
                cmp.b   #'R',D0
                bne.s   get_poe
                bsr.s   getb
                move.l  A1,(A5)+
                move.l  #$1029004D,(A1)+            ;MOVE.B 77(A1),D0
                move.l  A1,(A5)+
                move.w  #$4880,(A1)+                ;EXT.W D0
get_poh:        move.l  A1,(A5)+
                move.w  #$48C0,(A1)+                ;EXT.L D0
                move    #$FF,CCR                    ;found
                rts
get_po2:        cmp.b   #'U',D0
                bne.s   get_po3
                bsr.s   getb
                cmp.b   #'S',D0
                bne.s   get_poe
                bsr.s   getb
                cmp.b   #'P',D0
                bne.s   get_poe
                moveq   #68,D1                      ;Offset for USP
                bra     get_pof
get_po3:        cmp.b   #'S',D0
                bne.s   get_po6
                bsr.s   getb
                cmp.b   #'P',D0
                beq.s   get_po4
                cmp.b   #'S',D0
                beq.s   get_po5
                cmp.b   #'R',D0
                bne.s   get_poe
                bsr     getb
                move.l  A1,(A5)+
                move.l  #$3029004C,(A1)+            ;MOVE.W 76(A1),D0
                bra.s   get_poh
get_po4:        moveq   #60,D1                      ;Pointer to the SP
                bra     get_pof
get_po5:        bsr     getb
                cmp.b   #'P',D0
                bne.s   get_poe
                moveq   #72,D1                      ;Pointer to the SSP
                bra.s   get_pof
get_poe:        move.w  D2,D0                       ;Reset variables because nothing found
                movea.l A2,A0
                move    #0,CCR                      ;nothing found
                rts
get_po6:        moveq   #0,D1                       ;Offset for the data registers
                cmp.b   #'^',D0
                bne.s   get_poe
                bsr     getb
                cmp.b   #'D',D0
                beq.s   get_po8
                cmp.b   #'A',D0
                beq.s   get_po7
                cmp.b   #'B',D0
                bne.s   get_poe
                bsr     getb
                cmp.b   #'C',D0
                bne.s   get_poe
                bsr     getb
                moveq   #16,D2
                jsr     chkval
                bhs.s   _synerr
                move.l  A1,(A5)+
                move.w  #$2029,(A1)+                ;MOVE.L n(A1),D0
                mulu    #12,D0
                addi.w  #breakpnt+6-regs,D0
                move.w  D0,(A1)+
                bsr     getb
                move    #$FF,CCR                    ;found
                rts
_synerr:        jmp     synerr
get_po7:        moveq   #32,D1                      ;Offset for the address registers
get_po8:        bsr     getb
                moveq   #8,D2
                jsr     chkval                      ;Get 0-7
                bcc.s   _synerr
                lsl.w   #2,D0                       ;Time 4 (registers are longwords)
                add.w   D0,D1
get_pof:        move.l  A1,(A5)+
                move.w  #$2029,(A1)+                ;MOVE.L n(A1),D0
                move.w  D1,(A1)+                    ;Use offset
                bsr     getb                        ;Follow
                cmp.b   #'.',D0
                beq.s   get_po9                     ;There is still an extension
get_pog:        move    #$FF,CCR                    ;found
                rts
get_po9:        bsr     getb                        ;Get extension
                cmp.b   #'L',D0
                beq.s   get_p11                     ;With Long only get the following sign
                move.w  -(A1),D1
                subq.l  #2,A1
                cmp.b   #'W',D0
                beq.s   get_p10
                cmp.b   #'B',D0
                bne.s   _synerr
                bsr     getb
                move.w  #$7000,(A1)+                ;MOVEQ #0,D0
                move.l  A1,(A5)+
                move.w  #$1029,(A1)+                ;MOVE.B n(A1),D0
                addq.w  #3,D1
                move.w  D1,(A1)+
                bra.s   get_pog
get_p10:        bsr     getb
                move.w  #$7000,(A1)+                ;MOVEQ #0,D0
                move.l  A1,(A5)+
                move.w  #$3029,(A1)+                ;MOVE.W n(A1),D0

                addq.w  #2,D1
                move.w  D1,(A1)+
                bra.s   get_pog
get_p11:        bsr     getb
                bra.s   get_pog
                ENDPART

********************************************************************************
* Fcreate()                                                                    *
********************************************************************************
                PART 'fcreate'
fcreate:        movem.l D0-A7,-(SP)
                jsr     do_mediach                  ;Media-Change trigger
                pea     fname(A4)
                move.w  #$41,-(SP)
                bsr     do_trap_1                   ;Fdelete()
                addq.l  #6,SP
                clr.w   -(SP)
                pea     fname(A4)
                move.w  #$3C,-(SP)
                bsr     do_trap_1                   ;Fcreate()
                addq.l  #8,SP
                tst.l   D0
                bmi.s   fread0                      ;Error opening
                move.w  D0,_fhdle(A4)               ;FileHandle brand
                movem.l (SP)+,D0-A7
                rts
                ENDPART

********************************************************************************
* Fwrite: Write A3 bytes from A2                                               *
********************************************************************************
                PART 'fwrite'
fwrite:         move.l  A3,-(SP)                    ;Save length to conceal
                move.l  A2,-(SP)                    ;Basic address
                move.l  A3,-(SP)                    ;Long
                move.w  _fhdle(A4),-(SP)
                move.w  #$40,-(SP)
                bsr.s   do_trap_1                   ;Fwrite()
                lea     12(SP),SP
                cmp.l   (SP)+,D0                    ;All bytes written?
                beq.s   fwrite1                     ;Mistake
                jmp     dskfull
fwrite1:        rts
                ENDPART

********************************************************************************
* Fclose                                                                       *
********************************************************************************
                PART 'fclose'
fclose:         move.w  _fhdle(A4),-(SP)
                move.w  #$3E,-(SP)
                bsr.s   do_trap_1                   ;Fclose()
                addq.l  #4,SP
                tst.l   D0
                bmi.s   fread0
                rts
                ENDPART

********************************************************************************
* Fopen(fname)                                                                 *
********************************************************************************
                PART 'fopen'
fopen:          jsr     do_mediach                  ;Media-Change trigger
                clr.w   -(SP)                       ;fopen for read-only
                pea     fname(A4)                   ;Initial address of the name
                move.w  #$3D,-(SP)
                bsr.s   do_trap_1                   ;Fopen()
                addq.l  #8,SP
                tst.l   D0                          ;everything OK?
                bmi.s   fread0                      ;unableToFopenFile
                move.w  D0,_fhdle(A4)               ;FileHandle brand
                rts
                ENDPART

********************************************************************************
* Fread D1 Bytes ab A6                                                         *
********************************************************************************
                PART 'fread'
fread:          move.l  A6,-(SP)                    ;Address, where to be read
                move.l  D1,-(SP)                    ;Read D1 bytes
                move.w  _fhdle(A4),-(SP)            ;FileHandle on the stack
                move.w  #$3F,-(SP)
                bsr.s   do_trap_1                   ;Fread()
                lea     12(SP),SP
                tst.l   D0
                bpl.s   fread1
fread0:         jmp     ioerr
fread1:         rts
                ENDPART

********************************************************************************
* Reads a File (name from Fname) from the address A6                           *
* Number of D6.L                                                               *
********************************************************************************
                PART 'readimg'
readimg:        movea.l A0,A5                       ;Remember current chrgat
                bsr.s   fopen                       ;open file
                move.l  #$01000000,D1               ;Enlist
                bsr.s   fread                       ;Read data
                move.l  D0,D6                       ;Number of read bytes remember
                bsr.s   fclose
                movea.l A5,A0                       ;Restore ChrGet Pointer
                rts
                ENDPART

********************************************************************************
* General GEMDOS jump                                                          *
********************************************************************************
                PART 'do_trap_1'
do_trap_1:      move.l  A0,D0
                lea     _regsav(A4),A0
                movem.l D0-D7/A1-A7,(A0)
                move.l  (SP)+,-(A0)                 ;Rescue
                andi    #$FB00,SR                   ;Free IRQS
                movea.l act_pd(A4),A0
                move.l  merk_act_pd(A4),(A0)        ;Debugger active
                trap    #1
                lea     varbase(PC),A4
                tst.l   basep(A4)                   ;Other program loaded?
                beq.s   do_trap1                    ;no
                movea.l act_pd(A4),A0
                move.l  basep(A4),(A0)              ;Otherwise the other program active
do_trap1:       movea.l D0,A0
                movem.l _regsav(A4),D0-D7/A1-A7
                exg     A0,D0
                move.l  _regsav2(A4),(SP)
                rts
                ENDPART

********************************************************************************
* Exception evaluation                                                         *
********************************************************************************
                PART 'exception'
                DC.L newstart
except_start:
excep_no        SET 2
                OPT O-,W-
                REPT 62
                DC.L 'XBRA'
                DC.L xbra_id
                DS.L 1
                move.l  #excep_no<<24,-(SP)
                bra     except1
excep_no        SET excep_no+1
                ENDR
                OPT O+,W+
                DC.L 'XBRA'
                DC.L xbra_id
old_privileg:   DS.L 1
own_privileg:   movem.l D0-D2,-(SP)
                move.l  A1,-(SP)
                move.l  A0,-(SP)
                movea.l 22(SP),A0
                move.w  (A0),D0
                move.w  D0,D1
                and.w   #$FFC0,D0
                cmp.w   #$40C0,D0
                bne     own_privileg6
                move.l  #$30004E71,$03F2.w
                move.l  #$4E714E75,$03F6.w
                move.w  D1,D0
                and.w   #7,D0
                lsl.w   #8,D0
                lsl.w   #1,D0
                or.w    D0,$03F2.w
                move.w  D1,D0
                and.w   #$38,D0
                lsl.w   #3,D0
                or.w    D0,$03F2.w
                moveq   #2,D2
                cmp.w   #$0180,D0
                beq     own_privileg6
                tst.w   D0
                beq.s   own_privileg4
                cmp.w   #$0140,D0
                beq.s   own_privileg2
                cmp.w   #$01C0,D0
                bne.s   own_privileg3
                and.w   #7,D1
                beq.s   own_privileg1
                addq.w  #2,D2
                move.w  4(A0),$03F6.w
own_privileg1:  addq.w  #2,D2
                move.w  2(A0),$03F4.w
                bra.s   own_privileg5
own_privileg2:  addq.w  #2,D2
                move.w  2(A0),$03F4.w
own_privileg3:  and.w   #7,D1
                cmp.w   #7,D1
                bne.s   own_privileg5
                move    USP,A1
                andi.w  #$F3FF,$03F2.w
                add.l   D2,22(SP)
                move    SR,-(SP)
                ori     #$0700,SR
                DC.B 'Nz',$00,$02
                or.l    #$0808,D0
                DC.B 'N{',$00,$02
                move    (SP)+,SR
                move.w  20(SP),D0
                jsr     $03F2.w
                move    A1,USP
                movea.l (SP)+,A0
                movea.l (SP)+,A1
                movem.l (SP)+,D0-D2
                rte
own_privileg4:  add.l   D2,22(SP)
                ori.w   #$10,$03F2.w
                jsr     clr_cache
                lea     20(SP),A0
                movem.l 8(SP),D0-D2
                jsr     $03F2.w
                movea.l (SP)+,A0
                movea.l (SP)+,A1
                adda.w  #12,SP
                rte
own_privileg5:  add.l   D2,22(SP)
                jsr     clr_cache
                movea.l (SP)+,A0
                movea.l (SP)+,A1
                move.w  12(SP),D0
                jsr     $03F2.w
                movem.l (SP)+,D0-D2
                rte
own_privileg6:  movea.l (SP)+,A0
                movea.l (SP)+,A1
                movem.l (SP)+,D0-D2
own_privileg7:  jmp     $12344321

log_var:        DC.W 0

loginc:         subq.l  #4,SP
login:          illegal                             ;Supervisor-Mode an, but SR rescue!
                DC.L 'MRET'
                moveq   #232,D7                     ;Cancel by RTS
                bra.s   login_trace1
login_trace:    illegal                             ;Supervisor-Mode an, but SR rescue
                DC.L 'MRET'
                moveq   #234,D7                     ;Cancel by RTS at Trace RTS
login_trace1:   clr.l   _pc(A4)                     ;PC is invalid!
                bra     except_cont_a               ;Get Format Word from the stack

except1:        move    #$2700,SR                   ;Block all IRQS
                move.l  A4,-(SP)
                lea     varbase(PC),A4
                movem.l D0-A6,regs(A4)              ;All registry save
                move.l  (SP)+,regs+48(A4)           ;Now save A4
                move.b  (SP),D7                     ;exceptionNumber
                addq.l  #4,SP                       ;PC from BSR.S (S.O.) does not matter
                move.w  (SP)+,_sr(A4)               ;Get SR
                move.l  (SP)+,D0                    ;Get PC
                move.l  D0,_pc(A4)
                move.w  D7,D6
                sub.w   #32,D6                      ;Default value for D6 = trap number
                cmp.b   #4,D7
                bne     except_cont_a               ;No 'illegal instruction' (bkpt?)

                movea.l D0,A0
                cmpa.l  #start,A0
                blo.s   except_bkpt                 ;Within the debugger?
                cmpa.l  #varbase,A0
                bhs.s   except_bkpt
                cmpi.l  #'MRET',2(A0)
                bne     except_cont_a               ;Unknown Illegal Operation
                jmp     6(A0)                       ;Back to the caller

;Breakpoint in der Liste suchen
except_bkpt:    moveq   #16,D6                      ;17 Breakpoints
                lea     breakpnt(A4),A0
except_bkpt1:   cmp.l   (A0)+,D0                    ;PC=Breakpoint?
                beq.s   except_bkpt2
                addq.l  #8,A0                       ;Type, Counter & (PC)
                dbra    D6,except_bkpt1
                bra     except_cont_a               ;Illegal Instruction

except_bkpt2:   moveq   #234,D7                     ;'Break'
                tst.b   ssc_flag(A4)                ;Cancel with Shift + Shift + Control desired
                bne     except_bkpt12
                moveq   #236,D7                     ;Permanent-breakpoint as a default abort
                move.w  (A0)+,D0                    ;breakpointtypHolen
                subq.w  #1,D0                       ;Flag
                beq     except_bkpt12               ;permanent
                bcs.s   except_bkpt9                ;counter
                bpl.s   except_bkpt10               ;user
                subq.l  #1,(A0)+                    ;Counter downhill
                bls     except_bkpt11               ;<= Zero?=> Cancel
except_bkpt3:   movea.l _pc(A4),A1
                move.l  A1,-(SP)
                move.w  _sr(A4),-(SP)
                move.w  #$4E71,D0                   ;NOP
                bset    #7,(SP)                     ;warTraceAn? (&TraceAn)
                bne.s   except_bkpt4                ;No RTE, if so!
                move.w  #$4E73,D0                   ;RTE
except_bkpt4:   move.w  D0,except_bkpt8             ;Insert NOP or RTE
                move.l  A1,except_bkpt7+4           ;PC notice
                move.w  (A0),(A1)                   ;Insert command on the PC
                move.l  $24.w,except_bkpt6+2        ;Trace vector mark
                move.l  #except_bkpt5,$24.w         ;Own trace routine pure
                jsr     clr_cache
                movem.l regs(A4),D0-A6
                rte                                 ;command
except_bkpt5:   bclr    #7,(SP)                     ;Trace again
except_bkpt6:   move.l  #0,$24.w                    ;Old trace vector back
except_bkpt7:   move.w  #$4AFC,$01234567            ;Insert Breakpoint again
                jsr     clr_cache
except_bkpt8:   nop                                 ;If TRACE was out
                move.l  $24.w,-(SP)                 ;Call trace again
                rts
except_bkpt9:   addq.l  #1,(A0)+                    ;Counter the breakpoints increase
                bra.s   except_bkpt3                ;Go on
except_bkpt10:  movea.l SP,A6
                movea.l default_stk(A4),SP          ;set off own stack
                movem.l A0/A6,-(SP)
                lea     regs(A4),A1                 ;Transfer parameters to user breakpoints
                movea.l (A0),A0                     ;Get address of the check routine
                jsr     (A0)                        ;Test condition
                movem.l (SP)+,A0/A6
                addq.l  #4,A0
                movea.l A6,SP
                beq     except_bkpt3                ;User-Breakpoint doesn`t do anything
                moveq   #237,D7                     ;User-Breakpoint
                bra.s   except_bkpt12
except_bkpt11:  moveq   #235,D7                     ;Meter
except_bkpt12:  neg.w   D6
                add.w   #16,D6                      ;breakpoint number (016)
except_cont_a:  tst.b   prozessor(A4)               ;68000?
                bmi.s   except40                    ;yes! =>
                move.w  (SP)+,D0                    ;Vector Offset
                rol.w   #5,D0                       ;*4 *2
                and.w   #$1E,D0                     ;Stack type
                move.w  stack_f_tab(PC,D0.w),D0
                jmp     stack_f_tab(PC,D0.w)
                BASE DC.W,stack_f_tab
stack_f_tab:    DC.W stack_f_0,stack_f_1,stack_f_2,stack_f_exit
                DC.W stack_f_exit,stack_f_exit,stack_f_exit,stack_f_exit
                DC.W stack_f_8,stack_f_9,stack_f_A,stack_f_B
                DC.W stack_f_exit,stack_f_exit,stack_f_exit,stack_f_exit

stack_f_B:      moveq   #84,D0                      ;Long Bus Cycle Fault Stack Frame (46 Words)
                bra.s   stack_f_A0
stack_f_A:      moveq   #24,D0                      ;Short Bus Cycle Fault Stack Frame (16 Words)
stack_f_A0:     move.w  2(SP),_fcreg(A4)            ;Special Status Word
                move.l  8(SP),_zykadr(A4)           ;Data Cycle Fault address
                adda.l  D0,SP
                bra.s   except_cont_b
stack_f_9:
stack_f_8:      addq.l  #8,SP                       ;68010 Buserror-Format (10 Words)
stack_f_2:      addq.l  #4,SP                       ;Instruction Address overlook
stack_f_0:
stack_f_1:
stack_f_exit:   bra.s   except_cont_b

except40:       and.w   #$FF,D7
                cmp.b   #4,D7
                bhs.s   except_cont_b               ;No bus or address error
                move.w  _sr(A4),_fcreg(A4)          ;Functioncode-Register Mkopieren
                move.l  _pc(A4),_zykadr(A4)         ;Copy cycle address
                move.w  (SP)+,_befreg(A4)           ;Command register
                move.w  (SP)+,_sr(A4)               ;Now comes the SR
                move.l  (SP)+,_pc(A4)               ;And it follows the PC
except_cont_b:  move.l  SP,_ssp(A4)                 ;SSP notice
                move    USP,A0
                move.l  A0,_usp(A4)                 ;and also the USP
                bclr    #7,_sr(A4)                  ;Trace!
                btst    #5,_sr(A4)                  ;User-Mode an?
                beq.s   excep50
                movea.l SP,A0                       ;No, supervisor mode
excep50:        move.l  A0,rega7(A4)                ;Put in7
                move.l  merk_stk(A4),D0             ;Trace until RTS?
                beq.s   excep51                     ;No =>
                clr.l   merk_stk(A4)                ;Delete Flag and ADR
                move.l  D0,_pc(A4)                  ;Set proper return address
excep51:        movea.l default_stk(A4),SP          ;Restore your own stack
                bsr     update_pc
                bsr     breakclr                    ;Remove breakpoints
                bsr     do_vbl                      ;Perform open VBL tasks
                cmp.b   #232,D7
                bne.s   excep510                    ;Cancel by RTS?
                move.l  merk_pc_call(A4),_pc(A4)
excep510:       cmpi.l  #cmd_trace9,_pc(A4)         ;Cancel in the trap at Untrace?
                bne.s   excep52                     ;No!=> Next
                lea     cmd_trace9+2(PC),A0         ;Address of the PC
                move.l  (A0),_pc(A4)                ;Get right PC
excep52:        cmp.w   #16,D6                      ;internet Breakpoint?
                beq     excep9g                     ;Yes!=> Spend nothing
                move.w  D7,-(SP)
                bsr     exc_out                     ;Output exceptiontext
                move.w  (SP)+,D7
                cmp.b   #4,D7                       ;Bus or address errors?
                bhi     excep9g
                beq     excep9x

;Bus- or address error treatment
                lea     exc_tx3(PC),A0              ;'writing on'
                btst    #4,_fcreg+1(A4)             ;Check R / W bit
                beq.s   except6                     ;Write access =>
                lea     exc_tx4(PC),A0              ;'Reading from'
except6:        move.l  A0,-(SP)
                jsr     @print_line(A4)
                move.l  _zykadr(A4),D1              ;Access address
                jsr     hexa2out                    ;Spend address
                pea     exc_tx2(PC)
                jsr     @print_line(A4)             ;'Function code:'
                moveq   #7,D0
                and.b   _fcreg+1(A4),D0
                or.w    #'0',D0
                jsr     @chrout(A4)                 ;Output function code
                moveq   #'-',D0
                jsr     @chrout(A4)
                moveq   #'B',D0
                btst    #3,_fcreg+1(A4)             ;With command execution or exception?
                beq.s   except7
                moveq   #'E',D0
except7:        jsr     @chrout(A4)                 ;Output field sign
                move.l  _pc(A4),D0
                btst    #0,D0                       ;PC upgrade?
                bne.s   except90
                movea.l D0,A6
                tst.b   prozessor(A4)               ;68010 or 68020?
                bpl.s   except70                    ;yes =>
                lea     -10(A6),A6
                bsr     check_read                  ;Test 10 bytes before
                bne.s   except90                    ;Nothing to do
                lea     10(A6),A6
                bsr     check_read                  ;pcLesbar?
                bne.s   except90                    ;Not to make
                addq.l  #2,A6                       ;pc+2
                move.w  _befreg(A4),D3              ;Get command register
                moveq   #9,D1                       ;max10WordsTesten
except8:        cmp.w   -(A6),D3                    ;Command found?
                dbeq    D1,except8
                bne.s   except90                    ;Nothing to do
except70:       move.l  A6,_pc(A4)                  ;Replace the PC
                move.l  A6,default_adr(A4)
except90:       jsr     @c_eol(A4)                  ;Delete line residue
                bsr     c_crlr                      ;Still spending a CR
                bra.s   excep9g
excep9x:        movea.l _pc(A4),A0                  ;Illegal command
                move.w  (A0)+,D0
                cmp.w   #$4AFC,D0
                bne.s   excep9x1
                bsr     in_trace_buff               ;Illegal notice
                addq.l  #2,_pc(A4)                  ;PC from the next command
                bra.s   excep9x2
excep9x1:       cmp.w   #$4848,D0                   ;breakpt "string"?
                bne.s   excep9g
                move.l  A0,input_pnt(A4)            ;Remember pointer to the line
                bsr     in_trace_buff               ;Illegal notice
excep9x3:       tst.b   (A0)+
                bne.s   excep9x3                    ;String over
                move.l  A0,D0
                addq.l  #1,D0
                and.b   #$FE,D0                     ;EVEN
                movea.l D0,A0
                move.l  A0,_pc(A4)                  ;and remember new pc
excep9x2:       st      illegal_flg(A4)             ;Remember flag for it
excep9g:        move.l  trace_pos(A4),reg_pos(A4)   ;Execute old home
                cmp.b   #10,D7
                bhs.s   excep9d
                lea     main_loop,A0
                move.l  A0,jmpdispa(A4)             ;Jumpdispatcher on main loop
excep9d:        moveq   #0,D0
                move.b  trap_abort(A4),D0           ;Cancel by Trap?
                beq.s   excep9e                     ;No, no demolition
                lea     traptab(PC),A0
                adda.w  -2(A0,D0.w),A0
                jsr     (A0)                        ;Call first sub-program
                clr.b   trap_abort(A4)
excep9e:        tst.b   dobef_flag(A4)              ;| Command executed?
                beq.s   excep9q
                move.l  data_buff(A4),default_adr(A4) ;Reset Default-ADR
                move.l  data_buff+4(A4),_pc(A4)     ;Reset PC
                st      assm_flag(A4)               ;Enter with the line assembler
                sf      dobef_flag(A4)
                cmp.b   #234,D7                     ;Cancel by Breakpoint?
                bhs.s   excep9q                     ;Everything ok, if so!
                sf      assm_flag(A4)               ;Cancel entry with the line assembler
excep9q:        movea.l kbshift_adr(A4),A0          ;Only keep CAPS / LOCK
                andi.b  #$10,(A0)
                sf      merk_shift(A4)
                lea     exc_back_tab(PC),A0
                tst.b   ssc_flag(A4)                ;Cancel with Shift + Shift?
                bne.s   excep9s                     ;=> And fast-exit?
excep9r:        move.b  (A0)+,D0
                bmi.s   excep9y                     ;Autore premium vector?
                cmp.b   D7,D0
                bne.s   excep9r                     ;found???
excep9s:        btst    #1,help_allow(A4)           ;Bit 1: Auto-Return
                beq.s   excep9y                     ;Auto-Return? No =>
                st      fast_exit(A4)               ;Immediately with Ctrl + Help out
excep9y:        bclr    #1,help_allow(A4)
                sf      ssc_flag(A4)                ;Clear abort flag
                jmp     (A4)

exc_out:        sf      testwrd(A4)                 ;Issue necessarily on the screen
                tst.w   spalte(A4)
                beq.s   exc_ou0
                jsr     @crout(A4)
exc_ou0:        jsr     @space(A4)
                moveq   #0,D1
                move.b  D7,D1
                jsr     dezout                      ;Spend vector number
                jsr     @space(A4)
                moveq   #'-',D0
                jsr     @chrout(A4)
                jsr     @space(A4)
                lea     extxtab(PC),A0
exc_ou1:        move.b  (A0)+,D0
                beq.s   exc_ou6
                cmp.b   #-1,D0
                beq.s   exc_ou3                     ;'Unknown exception #' '
                cmp.b   D7,D0
                bne.s   exc_ou1
exc_ou2:        tst.b   (A0)+                       ;Find text beginning
                bne.s   exc_ou2
exc_ou3:        move.l  A0,-(SP)
                jsr     @print_line(A4)             ;Output error message
exc_ou5:        tst.b   (A0)+                       ;Years Stringende
                bne.s   exc_ou5
                cmpi.b  #'#',-2(A0)                 ;'#' Before the zero byte?
                bne.s   exc_ou4
                move.w  D6,D1
;                tst.b   D7
;                bmi.s   exc_o50
                and.w   #$0F,D1
;exc_o50:        and.l   #$FF,D1
                jsr     hexout                      ;Otherwise d6 still spend
exc_ou4:        pea     exc_tx1(PC)
                jsr     @print_line(A4)             ;'At address'
                move.l  _pc(A4),D1
                cmp.l   #do_trace9,D1
                bne.s   exc_o40                     ;Do_trace when running a trap?
                movea.l D1,A1
                move.l  2(A1),D1                    ;Piece from the JMP
exc_o40:        moveq   #0,D0
                move.b  (A0),D0                     ;pcOffsetHolen
                sub.l   D0,D1                       ;Disconnect the PC offset
                addq.l  #1,D1
                and.b   #$FE,D1                     ;PC now straight
                move.l  D1,_pc(A4)                  ;Replace the PC
                move.l  D1,default_adr(A4)
                jsr     hexa2out                    ;Output PC with '$'
                cmp.b   #3,D7
                bls.s   exc_o10
                jsr     @c_eol(A4)                  ;Delete line residue
                bra     c_crlr                      ;Still spending a CR
exc_ou6:        tst.b   (A0)+                       ;Text to zero byte
                bne.s   exc_ou6
                addq.l  #1,A0                       ;PC offset overlooked
                bra.s   exc_ou1
exc_o10:        rts

exc_back_tab:   DC.B 232,233,$C1,$C2,-1
extxtab:        DC.B 2,0,'Bus Error',0,0
                DC.B 3,0,'Address Error',0,0
                DC.B 4,0,'Illegal Instruction',0,0
                DC.B 5,0,'Zero Divide',0,2
                DC.B 6,0,'CHK, CHK2 Instruction',0,2
                DC.B 7,0,'cpTRAPcc, TRAPcc, TRAPV Instruction',0,2
                DC.B 8,0,'Privilege Violation',0,0
                DC.B 9,0,'Trace',0,0
                DC.B 10,0,'Line 1010 Emulator',0,0
                DC.B 11,0,'Line 1111 Emulator',0,0
                DC.B 12,0,'Exception #12',0,0
                DC.B 13,0,'Coprocessor Protocol Violation',0,0
                DC.B 14,0,'Format Error',0,0
                DC.B 15,0,'Uninitialized Interrupt',0,0
                DC.B 16,0,'Exception #16',0,0
                DC.B 17,0,'Exception #17',0,0
                DC.B 18,0,'Exception #18',0,0
                DC.B 19,0,'Exception #19',0,0
                DC.B 20,0,'Exception #20',0,0
                DC.B 21,0,'Exception #21',0,0
                DC.B 22,0,'Exception #22',0,0
                DC.B 23,0,'Exception #23',0,0
                DC.B 24,0,'Spurious Interrupt',0,0
                DC.B 25,0,'Level 1 Interrupt Auto Vector',0,0
                DC.B 26,0,'Level 2 Interrupt Auto Vector (HBL)',0,0
                DC.B 27,0,'Level 3 Interrupt Auto Vector',0,0
                DC.B 28,0,'Level 4 Interrupt Auto Vector (VBL)',0,0
                DC.B 29,0,'Level 5 Interrupt Auto Vector',0,0
                DC.B 30,0,'Level 6 Interrupt Auto Vector',0,0
                DC.B 31,0,'Level 7 Interrupt Auto Vector',0,0
                DC.B 32,35,36,37,38,39,40,41,42,43,44,47,0,'Trap #',0,2
                DC.B 33,34,45,46,0,'Stopped at Trap #',0,2
                DC.B 48,0,'FPU Unordered Condition',0,0
                DC.B 49,0,'FPU Inexact result',0,0
                DC.B 50,0,'FPU Division by zero',0,0
                DC.B 51,0,'FPU Underflow',0,0
                DC.B 52,0,'FPU Operand Error',0,0
                DC.B 53,0,'FPU Overflow',0,0
                DC.B 54,0,'FPU Not a Number (NAN)',0,0
                DC.B 55,0,'Exception #55',0,0
                DC.B 56,0,'PMMU Configuration',0,0
                DC.B 57,0,'PMMU Illegal Operation',0,0
                DC.B 58,0,'PMMU Access Level',0,0
                DC.B 59,0,'Exception #59',0,0
                DC.B 60,0,'Exception #60',0,0
                DC.B 61,0,'Exception #61',0,0
                DC.B 62,0,'Exception #62',0,0
                DC.B 63,0,'Exception #63',0,0

                DC.B 78,0,'External demolition',0,0

                DC.B $C1,$C2,0,'End of the program Trap #',0,2
                DC.B $D2,0,'Illegal parameters at Trap #',0,2
                DC.B 230,0,'Demolition [SHIFT][SHIFT]',0,0
                DC.B 231,0,'[CTRL][ALT][HELP] pressed',0,0
                DC.B 232,233,0,'End RTS',0,0
                DC.B 234,0,'Abort Breakpoint #',0,0
                DC.B 235,0,'Stop-Breakpoint #',0,0
                DC.B 236,0,'Permanent-Breakpoint #',0,0
                DC.B 237,0,'User-Breakpoint #',0,0
                DC.B -1,'Unknown Exception #',0

exc_tx1:        DC.B ' at Address ',0
exc_tx2:        DC.B ', FC:',0
exc_tx3:        DC.B ' writing at ',0
exc_tx4:        DC.B ' reading from ',0
                EVEN
                ENDPART

********************************************************************************
* Set the PC out of the debugger                                               *
********************************************************************************
                PART 'update_pc'
update_pc:      move.l  A0,-(SP)
                movea.l _pc(A4),A0                  ;act. Get PC
                cmpa.l  #new_gemdos,A0
                bne.s   update_pc1
                movea.l old_gemdos(PC),A0           ;Out of the Gemdos routine
update_pc1:     cmpa.l  #new_bios,A0
                bne.s   update_pc2
                movea.l old_bios(PC),A0             ;Out of the BIOS routine
update_pc2:     cmpa.l  #new_xbios,A0
                bne.s   update_pc3
                movea.l old_xbios(PC),A0            ;Out of the XBIOS routine
update_pc3:     cmpa.l  #new_aesvdi,A0
                bne.s   update_pc4
                movea.l old_aesvdi(PC),A0           ;Out of the AES routine
update_pc4:     cmpa.l  #login,A0
                bne.s   update_pc6
                movea.l merk_pc_call(A4),A0
update_pc6:     cmpa.l  _pc(A4),A0
                beq.s   update_pc5
                move.l  A0,_pc(A4)                  ;Possibly. Corrected PC
                movea.l rega7(A4),A0
                andi.w  #$7FFF,(A0)                 ;Trace out
update_pc5:     movea.l (SP)+,A0
                rts
                ENDPART

********************************************************************************
* The Gemdos-Handler                                                           *
********************************************************************************
                PART 'new_gemdos'
                DC.L 'XBRA'
                DC.L xbra_id
old_gemdos:     DS.L 1
new_gemdos:     movem.l D0-A6,save_all_reg
                lea     varbase(PC),A4
                move.l  A0,merk_a0(A4)              ;For break at "lineinput"
                lea     6(SP),A0                    ;gives the space of the function number on SVSP
                tst.b   prozessor(A4)               ;68000?
                bmi.s   new_gemdos1                 ;yes!=>
                addq.l  #2,A0                       ;68010 or 68020 has a word more
new_gemdos1:    btst    #5,(SP)                     ;Call for U-mode?
                bne.s   new_gemdos2                 ;No, S-mode
                move    USP,A0
new_gemdos2:    move.w  (A0),D0                     ;Get function number
                bmi.s   new_gemdos5                 ;Negative?=> immediately
                cmp.w   #$7E,D0                     ;Max. function number exceeded?
                bhi.s   new_gemdos5                 ;=> immediately
                lea     gemdos_break(A4),A1
                lea     0(A1,D0.w),A2
                tst.b   (A2)                        ;Cancel flag set for the function?
                bmi.s   new_gemdos6                 ;Yes!=> Cancel
                tst.b   ssc_flag(A4)                ;Cancel with Shift + Shift + Control desired?
                bne     new_gemdos8
                move.l  save_data+$0408-8(A4),$0408.w ;ETV_TERM on normal
                movea.l act_pd(A4),A0               ;Active program
                move.l  (A0),D2
                cmp.l   merk_act_pd(A4),D2          ;No program loaded?
                beq.s   new_gemdos3
                move.l  basep(A4),D1                ;Basepage of direct recharged PRG
                cmp.l   D2,D1                       ;Is your own child not active?
                bne.s   new_gemdos4                 ;Then skip security queries
new_gemdos3:    move.l  #etv_term,$0408.w           ;Own ETV_TERM handler
                tst.w   D0                          ;pterm0
                beq.s   new_gemdos7
                cmp.w   #$31,D0                     ;Ptermres always cancel (at your own child)
                beq.s   new_gemdos7
                cmp.w   #$4C,D0                     ;Pterm
                beq.s   new_gemdos7
new_gemdos4:    tst.b   (A2)
                sne     (A2)                        ;Reset Break Flag
new_gemdos5:    movem.l save_all_reg(PC),D0-A6
                move.l  old_gemdos(PC),-(SP)
                rts                                 ;Perform normal trap # 1

new_gemdos6:    move.l  2(SP),D1                    ;get the PC
                subq.l  #2,D1                       ;PC back on the trap
                cmp.l   first_free(A4),D1           ;Smaller than the beginning of the memory?
                blo.s   new_gemdos4                 ;=> continue immediately
                cmp.l   save_data+$0436-8(A4),D1    ;Above the RAM?
                bhs.s   new_gemdos4                 ;=> continue immediately
                lea     0(A1,D0.w),A2
                move.b  #1,(A2)                     ;Put breaking
                move.b  #2,trap_abort(A4)           ;Cancel by Gemdos
                movem.l save_all_reg(PC),D0-A6
                move.l  #$21<<24,-(SP)
                pea     except1(PC)
                rts
new_gemdos7:    move.b  #2,trap_abort(A4)           ;Cancel by Gemdos
                movem.l save_all_reg(PC),D0-A6
                move.l  #$C1<<24,-(SP)
                pea     except1(PC)
                rts                                 ;Program end
new_gemdos8:    move.l  2(SP),D1                    ;get the PC
                subq.l  #2,D1                       ;PC back on the trap
                movea.l act_pd(A4),A2
                cmp.l   (A2),D1                     ;Smaller than the debugger?
                blo.s   new_gemdos4                 ;=> continue immediately
                cmp.l   save_data+$0436-8(A4),D1    ;Above the RAM?
                bhs.s   new_gemdos4                 ;=> continue immediately
                move.b  #2,trap_abort(A4)           ;Cancel by Gemdos
                movem.l save_all_reg(PC),D0-A6
                move.l  #$21<<24,-(SP)
                pea     except1(PC)
                rts
                ENDPART

********************************************************************************
* The XBIOS-Handler                                                            *
********************************************************************************
                PART 'new_xbios'
                DC.L 'XBRA'
                DC.L xbra_id
old_xbios:      DS.L 1
new_xbios:      movem.l D0-A6,save_all_reg
                lea     varbase(PC),A4
                lea     6(SP),A0                    ;gives the space of the function number on SVSP
                tst.b   prozessor(A4)               ;68000?
                bmi.s   new_xbios1                  ;yes!=>
                addq.l  #2,A0                       ;68010 or 68020 has a word more
new_xbios1:     btst    #5,(SP)                     ;Call for U-mode?
                bne.s   new_xbios2                  ;No, S-mode
                move    USP,A0
new_xbios2:     move.w  (A0),D0                     ;Get function number
                bmi.s   new_xbios4                  ;Negative?=> immediately
                cmp.w   #$57,D0                     ;Max.funktion number exceeded?
                bhi.s   new_xbios4                  ;=> immediately
                lea     xbios_break(A4),A0
                lea     0(A0,D0.w),A0
                tst.b   (A0)                        ;Cancel flag set for the function?
                bmi.s   new_xbios6                  ;Yes!=> Cancel
                tst.b   ssc_flag(A4)                ;Cancel with Shift + Shift + Control desired?
                bne.s   new_xbios5
new_xbios3:     tst.b   (A0)
                sne     (A0)                        ;Reset Break Flag
new_xbios4:     movem.l save_all_reg(PC),D0-A6
                move.l  old_xbios(PC),-(SP)
                rts                                 ;Perform normal trap # 14

new_xbios5:     lea     spaced2(A4),A0              ;dummy
new_xbios6:     move.l  2(SP),D1                    ;get the PC
                subq.l  #2,D1                       ;PC back on the trap
                cmp.l   first_free(A4),D1           ;Smaller than the beginning of the memory?
                blo.s   new_xbios3                  ;=> continue immediately
                cmp.l   rom_base(A4),D1             ;The operating system?
                bhs.s   new_xbios3                  ;=> continue immediately
                move.b  #1,(A0)                     ;brew
                move.b  #6,trap_abort(A4)           ;Cancel by XBIOS
                bsr.s   do_vbl                      ;Perform open VBL tasks
                movem.l save_all_reg(PC),D0-A6
                move.l  #$2E<<24,-(SP)
                pea     except1(PC)
                rts
                ENDPART

********************************************************************************
* Perform open VBL tasks                                                       *
********************************************************************************
                PART 'do_vbl'
do_vbl:         movem.l D0/A0-A1,-(SP)
                tst.l   $045A.w                     ;New color palette?
                beq.s   do_vbl2
                movea.l $045A.w,A0
                lea     $FFFF8240.w,A1
                moveq   #7,D0
do_vbl1:        move.l  (A0)+,(A1)+
                dbra    D0,do_vbl1
                clr.l   $045A.w
do_vbl2:        tst.l   $045E.w                     ;New screen address
                beq.s   do_vbl3
                move.l  $045E.w,D0
                move.l  D0,$044E.w
                lsr.l   #8,D0
                move.b  D0,$FFFF8203.w
                lsr.w   #8,D0
                move.b  D0,$FFFF8201.w
do_vbl3:        movem.l (SP)+,D0/A0-A1
                rts
                ENDPART

********************************************************************************
* The Bios-Handler                                                             *
********************************************************************************
                PART 'new_bios'
                DC.L 'XBRA'
                DC.L xbra_id
old_bios:       DS.L 1
new_bios:       movem.l D0-A6,save_all_reg
                lea     varbase(PC),A4
                lea     6(SP),A0                    ;gives the space of the function number on SVSP
                tst.b   prozessor(A4)               ;68000?
                bmi.s   new_bios1                   ;yes!=>
                addq.l  #2,A0                       ;68010 or 68020 has a word more
new_bios1:      btst    #5,(SP)                     ;Call for U-mode?
                bne.s   new_bios2                   ;No, S-mode
                move    USP,A0
new_bios2:      move.w  (A0),D0                     ;Get function number
                bmi.s   new_bios4                   ;Negative?=> immediately
                cmp.w   #$0B,D0                     ;Max.funktion number exceeded?
                bhi.s   new_bios4                   ;=> immediately
                lea     bios_break(A4),A0
                lea     0(A0,D0.w),A0
                tst.b   (A0)                        ;Cancel flag set for the function?
                bmi.s   new_bios6                   ;Yes!=> Cancel
                tst.b   ssc_flag(A4)                ;Cancel with Shift + Shift + Control desired?
                bne.s   new_bios5
new_bios3:      tst.b   (A0)
                sne     (A0)                        ;Reset Break Flag
new_bios4:      movem.l save_all_reg(PC),D0-A6
                move.l  old_bios(PC),-(SP)
                rts                                 ;Perform normal trap # 13

new_bios5:      lea     spaced2(A4),A0              ;dummy
new_bios6:      move.l  2(SP),D1                    ;get the PC
                subq.l  #2,D1                       ;PC back on the trap
                cmp.l   first_free(A4),D1           ;Smaller than the beginning of the memory?
                blo.s   new_bios3                   ;=> continue immediately
                cmp.l   rom_base(A4),D1             ;The operating system?
                bhs.s   new_bios3                   ;=> continue immediately
                move.b  #1,(A0)                     ;Put breaking
                move.b  #4,trap_abort(A4)           ;Cancel by BIOS
                movem.l save_all_reg(PC),D0-A6
                move.l  #$2D<<24,-(SP)
                pea     except1(PC)
                rts
                ENDPART

********************************************************************************
* TRAP #2-Entry                                                                *
********************************************************************************
                PART 'new_aesvdi'
save_all_reg:   DS.L 15

                DC.L 'XBRA'
                DC.L xbra_id
old_aesvdi:     DS.L 1
new_aesvdi:     movem.l D0-A6,save_all_reg
                movea.l SP,A6
                lea     varbase(PC),A4
                movea.l default_stk(A4),SP          ;Restore your own stack
                move.l  2(A6),D7                    ;get the PC
                subq.l  #2,D7                       ;PC back on the trap
                cmp.l   first_free(A4),D7           ;Smaller than the beginning of the memory?
                blo     new_aesvdi9                 ;=> continue immediately
                cmp.l   rom_base(A4),D7             ;The operating system?
                bhs     new_aesvdi9                 ;=> continue immediately
                tst.w   D0                          ;End of the program?
                beq     new_aesvdi10
                cmp.w   #-2,D0                      ;gdosTest?
                beq     new_aesvdi9                 ;run immediately
                cmp.w   #-1,D0                      ;Hergenden test for 2.2
                beq     new_aesvdi9                 ;run immediately
                bsr     check_d1                    ;Address for user fashion valid?
                movea.l D1,A0                       ;parameterBlockAdresse
                movea.l (A0),A1                     ;Get Control Field Address
                cmp.w   #115,D0                     ;VDI
                beq.s   new_aesvdi1
                cmp.w   #200,D0                     ;AES
                beq.s   new_aesvdi5
                cmp.w   #201,D0                     ;AES
                beq.s   new_aesvdi5
                bra     new_aesvdi13                ;Müll
new_aesvdi1:    moveq   #4,D2                       ;VDI-Parameter-Block valid?
                moveq   #0,D0
new_aesvdi2:    move.l  0(A0,D0.w),D1
                beq.s   new_aesvdi4
                bsr     check_d1                    ;Address for the User-Mode valid?
new_aesvdi3:    addq.l  #4,D0
                dbra    D2,new_aesvdi2
                move.w  (A1),D2                     ;get Opcode
                cmp.w   #132,D2
                bhi     new_aesvdi13
                lea     vdi_break(A4),A0
                bra.s   new_aesvdi8
new_aesvdi4:    cmp.w   #2,D2
                beq.s   new_aesvdi3
                bra     new_aesvdi13

new_aesvdi5:    moveq   #5,D2                       ;AES-Parameter-Block valid?
                moveq   #0,D0
new_aesvdi6:    move.l  0(A0,D0.w),D1
                bsr     check_d1                    ;Address for the User-Mode valid?
                addq.l  #4,D0
                dbra    D2,new_aesvdi6
                move.w  (A1),D2                     ;get Opcode
                cmp.w   #131,D2
                bhi.s   new_aesvdi13
                lea     aes_all(PC),A0

new_aesvdi7:    move.b  (A0)+,D0                    ;Is there the function ever?
                beq.s   new_aesvdi13
                cmp.b   D0,D2
                bne.s   new_aesvdi7
                lea     aes_break(A4),A0
new_aesvdi8:    tst.w   D2
                bmi.s   new_aesvdi13
                lea     0(A0,D2.w),A0
                tst.b   (A0)
                bmi.s   new_aesvdi14                ;Cancellation!
                tst.b   ssc_flag(A4)                ;Cancel with Shift + Shift + Control desired?
                bne.s   new_aesvdi15
                tst.b   (A0)
                sne     (A0)                        ;Reset Break Flag
new_aesvdi9:    movea.l A6,SP
                movem.l save_all_reg(PC),D0-A6
                move.l  old_aesvdi(PC),-(SP)
                rts

new_aesvdi10:   lea     gemdos_break+$4C(A4),A0
                tst.b   (A0)
                bpl.s   new_aesvdi11
                move.b  #1,(A0)                     ;Put break if desired
new_aesvdi11:   move.l  save_data+$0408-8(A4),$0408.w ;etv_term on normal
                movea.l act_pd(A4),A0               ;Active program
                move.l  (A0),D0
                cmp.l   merk_act_pd(A4),D0          ;No program loaded?
                beq.s   new_aesvdi12
                move.l  basep(A4),D7                ;Basepage of direct recharged PRG
                cmp.l   D0,D7                       ;Is your own child not active?
                bne.s   new_aesvdi9                 ;Then run skip
new_aesvdi12:   move.l  #etv_term,$0408.w           ;Own ETV_TERM handler
                clr.b   trap_abort(A4)              ;Cancel by AES / VDI
                movea.l A6,SP
                movem.l save_all_reg(PC),D0-A6
                move.l  #$C2<<24,-(SP)
                pea     except1(PC)
                rts                                 ;Program end at Trap # 2

new_aesvdi13:   tst.w   no_aes_check(A4)
                bne.s   new_aesvdi9
                clr.b   trap_abort(A4)              ;No abort by AES / VDI
                movea.l A6,SP
                movem.l save_all_reg(PC),D0-A6
                move.l  #$D2<<24,-(SP)
                pea     except1(PC)
                rts                                 ;Illegal parameters at Trap # 2

new_aesvdi14:   move.b  #1,(A0)                     ;Put breaking
new_aesvdi15:   move.b  #8,trap_abort(A4)           ;Cancel by AES / VDI
                movea.l A6,SP
                movem.l save_all_reg(PC),D0-A6
                move.l  #$22<<24,-(SP)
                pea     except1(PC)
                rts

check_d1:       cmp.l   #$0800,D1                   ;Address too small?
                blo.s   check_d
                cmp.l   save_data+$0436-8(A4),D1    ;Address too big?
                bhs.s   check_d
                rts
check_d:        addq.l  #4,SP
                bra.s   new_aesvdi13
                ENDPART

********************************************************************************
* The functions of the operating system                                        *
********************************************************************************
                PART 'do_(x)bios/gemdos/vdi/aes'
                BASE DC.W,traptab
traptab:        DC.W do_gemdos                      ;2
                DC.W do_bios                        ;4
                DC.W do_xbios                       ;6
                DC.W do_vdiaes                      ;8

do_bios:        lea     do_get4+1(PC),A0            ;(X)BIOS
                lea     bios_befs(PC),A1            ;Table of command name
                bra.s   do_gem
do_xbios:       lea     do_get4(PC),A0              ;XBIOS
                lea     xbios_befs(PC),A1           ;Table of command name
                bra.s   do_gem
do_gemdos:      lea     do_get1(PC),A0
                lea     gemdos_befs(PC),A1          ;Table of command name
do_gem:         jsr     @space(A4)
                move.l  A0,-(SP)
                jsr     @print_line(A4)
                pea     do_get3(PC)
                jsr     @print_line(A4)
                movea.l rega7(A4),A0                ;Pointer to the parameters
                move.w  (A0)+,D1                    ;The function number
                jsr     hexbout                     ;Output the number
                jsr     @space(A4)
                bsr     gleich_out                  ;spend '='
                jsr     @space(A4)
                move.w  D1,D0
                lsr.w   #8,D0                       ;Upper byte of the function number <> 0?
                tst.b   D0
                bne     do_gemi                     ;then error =>
do_gem1:        move.b  (A1)+,D0
                bmi     do_gemi                     ;Table end => not found
                cmp.b   D0,D1
                beq.s   do_gem3                     ;Function found =>
                move.b  (A1)+,D0                    ;Stack format over
                rol.b   #2,D0
                andi.b  #3,D0
                beq.s   do_gem2
                addq.l  #1,A1
do_gem2:        tst.b   (A1)+
                bne.s   do_gem2                     ;Function name
                bra.s   do_gem1                     ;Next
do_gem3:        moveq   #1,D2
                swap    D2                          ;9.Parameter for flipping () = Move.L # $ 10000, D2
                move.b  (A1)+,D2                    ;Get stack format
                move.b  D2,D0
                rol.b   #2,D0
                and.b   #3,D0
                beq.s   do_gem8
                move.b  (A1)+,D0                    ;extended Parameter
                lsl.w   #8,D0
                or.w    D0,D2
do_gem8:        move.l  A1,-(SP)
                jsr     @print_line(A4)             ;Output the function name
                moveq   #'(',D0
                jsr     @chrout(A4)
                moveq   #0,D4                       ;Do not spend a comma yet
do_gem4:        move.l  D2,D3
                and.w   #3,D3
                beq     do_gemx                     ;No further parameters
                tst.b   D4
                beq.s   do_gem9
                moveq   #',',D0
                jsr     @chrout(A4)                 ;Issue a comma
do_gem9:        btst    #1,D3
                bne.s   do_gem5
                moveq   #0,D1
                move.w  (A0)+,D1                    ;Bring Word from the stack
                cmp.w   #$FFFF,D1
                bne.s   do_ge90
                moveq   #-1,D1
do_ge90:        moveq   #'w',D0
                bra.s   do_gem6
do_gem5:        move.l  (A0)+,D1                    ;Long from the stack
                moveq   #'l',D0                     ;for Long
do_gem6:        move.b  D0,-(SP)                    ;Extension notice
                jsr     @chrout(A4)                 ;Output extension
                moveq   #':',D0
                jsr     @chrout(A4)
                addq.l  #1,D1                       ;-1?
                bne.s   do_gem7
                moveq   #'-',D0
                jsr     @chrout(A4)                 ;Output sign
                moveq   #2,D1                       ;1 output (with "-" before, ie -1)
do_gem7:        subq.l  #1,D1
                jsr     hexout                      ;Output hex count
                move.b  (SP)+,D0
                cmp.b   #'l',D0                     ;A Long issued?
                bne.s   do_gem71                    ;No!=>
                tst.l   D1
                ble.s   do_gem71                    ;sure an invalid address =>
                move.l  A6,-(SP)
                movea.l D1,A6
                bsr     check_read                  ;Remember address
                bne.s   do_gem74
                cmpa.l  #$FF0000,A6
                bhi.s   do_gem74                    ;Guaranteed invalid =>
                moveq   #':',D0
                jsr     @chrout(A4)
                moveq   #'"',D0
                jsr     @chrout(A4)
                moveq   #31,D1                      ;Max. 32 characters output
do_gem72:       move.b  (A6)+,D0
                beq.s   do_gem73                    ;Output the string in quotation marks
                jsr     @chrout(A4)
                dbra    D1,do_gem72
do_gem73:       moveq   #'"',D0
                jsr     @chrout(A4)
do_gem74:       movea.l (SP)+,A6
do_gem71:       lsr.l   #2,D2                       ;Get next parameter
                moveq   #-1,D4                      ;From now on a comma after each parameter
                bra     do_gem4                     ;and testing =>
do_gemi:        pea     do_get2(PC)
                jsr     @print_line(A4)
do_gemx:        moveq   #')',D0                     ;Clamp too, end
                jsr     @chrout(A4)
                jsr     @c_eol(A4)
                jmp     @crout(A4)

do_get1:        DC.B 'GEMDOS',0
do_get2:        DC.B 'illfunc(',0

                SWITCH language
                CASE 0
do_get3:        DC.B ' - Funktion #$',0
                CASE 1
do_get3:        DC.B ' - Function #$',0
                ENDS

do_get4:        DC.B 'XBIOS',0
                EVEN

do_vdiaes:      move.l  regs(A4),D0
                movea.l regs+4(A4),A6
                cmp.w   #115,D0
                beq     do_vdi
                pea     do_aet1(PC)
                jsr     @print_line(A4)             ;Output AES message
                pea     do_get3(PC)
                jsr     @print_line(A4)
                movea.l (A6),A2                     ;CONTRL-FIELD-ADR
                move.w  (A2),D1                     ;Get function number
                jsr     hexbout                     ;Output function number
                jsr     @space(A4)
                bsr     gleich_out                  ;' = ' output
                jsr     @space(A4)
                lea     aes_befs(PC),A0
do_aes1:        move.b  (A0)+,D2                    ;Get function number
                bmi     do_aesi                     ;Illegal, as table tenders
                movea.l A0,A1                       ;Remember command header
do_aes2:        tst.b   (A0)+                       ;Commandheader overlooked
                bne.s   do_aes2
do_aes3:        addq.w  #1,D2                       ;Increase function number
                cmp.b   D2,D1
                beq.s   do_aes5                     ;found
do_aes4:        tst.b   (A0)+                       ;Command end
                bgt.s   do_aes4
                beq.s   do_aes3                     ;It was just a command to get => next one
                bra.s   do_aes1                     ;Test next block
do_aes5:        move.l  A1,-(SP)

                jsr     @print_line(A4)             ;Output command header
                moveq   #'_',D0
                jsr     @chrout(A4)
do_aes6:        move.b  (A0)+,D0
                ble.s   do_aes7                     ;<=0 => end
                jsr     @chrout(A4)
                bra.s   do_aes6
do_aes7:        moveq   #'(',D0
                jsr     @chrout(A4)
do_aes8:        moveq   #')',D0                     ;Clamp too, end
                jsr     @chrout(A4)
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                pea     do_aet2(PC)
                jsr     @print_line(A4)
                move.l  (A6)+,D1                    ;control
                jsr     hexa2out
                pea     do_aet3(PC)
                jsr     @print_line(A4)
                move.l  (A6)+,D1                    ;global
                jsr     hexa2out
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                pea     do_aet4(PC)
                jsr     @print_line(A4)
                move.l  (A6)+,D1                    ;int_in
                jsr     hexa2out
                pea     do_aet5(PC)
                jsr     @print_line(A4)
                move.l  (A6)+,D1                    ;int_out
                jsr     hexa2out
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                pea     do_aet6(PC)
                jsr     @print_line(A4)
                move.l  (A6)+,D1                    ;addr_in
                jsr     hexa2out
                pea     do_aet7(PC)
                jsr     @print_line(A4)
                move.l  (A6),D1                     ;addr_out
                jsr     hexa2out
                jsr     @c_eol(A4)
                jmp     @crout(A4)
do_aesi:        pea     do_get2(PC)
                jsr     @print_line(A4)             ;"illfunc("
                bra     do_aes8                     ;")" and end
do_aet1:        DC.B ' AES',0

                SWITCH language
                CASE 0
do_aet2:        DC.B '  control  ab ',0
do_aet3:        DC.B '  global   ab ',0
do_aet4:        DC.B '  int_in   ab ',0
do_aet5:        DC.B '  int_out  ab ',0
do_aet6:        DC.B '  addr_in  ab ',0
do_aet7:        DC.B '  addr_out ab ',0

                CASE 1
do_aet2:        DC.B '  control  at ',0
do_aet3:        DC.B '  global   at ',0
do_aet4:        DC.B '  int_in   at ',0
do_aet5:        DC.B '  int_out  at ',0
do_aet6:        DC.B '  addr_in  at ',0
do_aet7:        DC.B '  addr_out at ',0
                ENDS

                EVEN

do_vdi:         pea     do_vdt1(PC)
                jsr     @print_line(A4)             ;Output VDI message
                pea     do_get3(PC)
                jsr     @print_line(A4)
                movea.l (A6),A2                     ;get contrl-Feld-Adr
                move.w  (A2),D1                     ;Get function number
                jsr     hexbout                     ;Output function number
                jsr     @space(A4)
                bsr     gleich_out                  ;' = ' output
                jsr     @space(A4)
                lea     vdi_befs(PC),A0
                tst.w   D1
                bls     do_vdii                     ;Number is crap
                cmp.w   #11,D1
                beq     do_vdi6                     ;Advanced graphics functions
                cmp.w   #39,D1
                bls.s   do_vdi5
                subi.w  #60,D1
                cmp.w   #40,D1
                blo     do_vdii                     ;Number is crap
                cmp.w   #71,D1
                bhi     do_vdii                     ;Number is crap
                bra.s   do_vdi5
do_vdi4:        tst.b   (A0)+                       ;String over
                bne.s   do_vdi4
do_vdi5:        dbra    D1,do_vdi4
do_vdi3:        moveq   #'v',D0
                jsr     @chrout(A4)
                move.l  A0,-(SP)
                jsr     @print_line(A4)
                moveq   #'(',D0
                jsr     @chrout(A4)
do_vdi8:        moveq   #')',D0                     ;Clamp too, end
                jsr     @chrout(A4)
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                pea     do_vdt2(PC)
                jsr     @print_line(A4)
                move.l  (A6)+,D1                    ;control
                jsr     hexa2out
                pea     do_vdt3(PC)
                jsr     @print_line(A4)
                move.l  (A6)+,D1                    ;intin
                jsr     hexa2out
                pea     do_vdt4(PC)
                jsr     @print_line(A4)
                move.l  (A6)+,D1                    ;intout
                jsr     hexa2out
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                moveq   #22,D0
                bsr     spacetab
                pea     do_vdt5(PC)
                jsr     @print_line(A4)
                move.l  (A6)+,D1                    ;ptsin
                jsr     hexa2out
                pea     do_vdt6(PC)
                jsr     @print_line(A4)
                move.l  (A6),D1                     ;ptsout
                jsr     hexa2out
                jsr     @c_eol(A4)
                jmp     @crout(A4)
do_vdii:        pea     do_get2(PC)
                jsr     @print_line(A4)             ;"illfunc("
                bra.s   do_vdi8                     ;")" and end
do_vdi6:        lea     vdi2bef(PC),A0
                move.w  10(A2),D1                   ;Advanced function number
                subq.w  #1,D1
                bmi.s   do_vdii
                cmp.w   #9,D1
                bhi.s   do_vdii
do_vdi7:        subq.w  #1,D1
                bmi     do_vdi3
do_vdi9:        tst.b   (A0)+
                bne.s   do_vdi9
                bra.s   do_vdi7

do_vdt1:        DC.B ' VDI',0

                SWITCH language
                CASE 0
do_vdt2:        DC.B '  control ab ',0
do_vdt3:        DC.B '  intin   ab ',0
do_vdt4:        DC.B '  ptsin   ab ',0
do_vdt5:        DC.B 'intout  ab ',0
do_vdt6:        DC.B '  ptsout  ab ',0

                CASE 1
do_vdt2:        DC.B '  control at ',0
do_vdt3:        DC.B '  intin   at ',0
do_vdt4:        DC.B '  ptsin   at ',0
do_vdt5:        DC.B 'intout  at ',0
do_vdt6:        DC.B '  ptsout  at ',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* Other vectors                                                                *
********************************************************************************
********************************************************************************
* ALT+Help-vector                                                              *
********************************************************************************
                PART 'alt_help'
                DC.L 'XBRA'
                DC.L xbra_id
old_alt_help:   DS.L 1
alt_help:       lea     varbase(PC),A4
                movea.l kbshift_adr(A4),A0
                moveq   #4,D0                       ;Control?
                and.b   (A0),D0
                beq.s   alt_help1                   ;nein! =>Hardcopy
                sf      le_allowed(A4)              ;Le is prohibited
                sf      help_allow(A4)              ;Ctrl + Help also prohibit
                clr.l   merk_svar(A4)               ;No marker handover
                clr.l   prg_base(A4)                ;No transferred program
                addq.l  #8,SP                       ;2 subroutine levels back
                addq.w  #1,$0452.w                  ;Release VBL Metaphore again
                move.w  #-1,$04EE.w                 ;Reset the dull laughter
                movem.l (SP)+,D0-A6
                andi.w  #$7FFF,(SP)                 ;Trace!
                move.l  #231<<24,-(SP)
                pea     except1(PC)
                rts
alt_help1:      move.l  old_alt_help(PC),-(SP)
                rts
                ENDPART

********************************************************************************
* etv_term to intercept at the debugging program                               *
********************************************************************************
                PART 'etv_term'
etv_term:       move    #$2700,SR                   ;Block all IRQS
                lea     varbase(PC),A4
                movea.l default_stk(A4),SP          ;Restore your own stack
                movea.l act_pd(A4),A0
                movea.l (A0),A0                     ;Pointer to the BasePage of the Act.PRG
                movem.l $6C(A0),D0-D3
                movem.l D0-D3,regs+44(A4)
                move.l  $68(A0),regs(A4)            ;Retrieve d0
                movea.l $7C(A0),A6                  ;aktStack
                movea.l (A6)+,A5                    ;USP / SSP (opposite of act.stack)
                move.w  (A6)+,_sr(A4)               ;statusreg
                move.l  (A6)+,_pc(A4)
                movem.l (A6)+,D1-A2
                move.l  A6,rega7(A4)
                movem.l D1-A2,regs+4(A4)
                btst    #5,_sr(A4)                  ;SupervisorModus active?
                bne.s   etv_te2
                exg     A5,A6                       ;Replace USP & SSP
etv_te2:        move.l  A5,_usp(A4)
                move.l  A6,_ssp(A4)
                move.l  merk_a0(A4),regs+32(A4)     ;Use A0 again
                jsr     @page1(A4)                  ;debuggerscreenAn
                bsr     breakclr                    ;Remove breakpoints
                moveq   #1,D6
                moveq   #$31,D7                     ;Program end at trap # 1
                bsr     exc_out                     ;Fault text and ADR output
                move.b  #2,trap_abort(A4)           ;Cancel by Gemdos
                lea     main_loop,A0
                move.l  A0,jmpdispa(A4)             ;Jumpdispatcher on main loop
                bra     excep9d                     ;Completion by the exception routine
                ENDPART

********************************************************************************
* Own etv_critic-Handler                                                       *
********************************************************************************
                PART 'etv_critic'
                DC.L 'XBRA'
                DC.L xbra_id
old_critic:     DS.L 1
etv_critic:     move.l  4(SP),D0
                movem.l D1-A6,-(SP)
                lea     varbase(PC),A4
                move.l  D0,D1
                moveq   #0,D0
                bsr     graf_mouse                  ;Mouse pointer as a arrow
                move.l  D1,D0
                lea     etv_tx3(PC),A0
                lea     etv_txt(PC),A1              ;From here the texts are used
                moveq   #3,D1                       ;4 lines
etv_critic1:    move.l  A0,(A1)+                    ;Insert empty text
                addq.l  #6,A1
                dbra    D1,etv_critic1
                move.w  D0,D1                       ;Drive in D1
                addi.w  #'A',D1
                swap    D0                          ;Error number in D0
                lea     etv_tab(PC),A0
etv_critic2:    move.b  (A0)+,D2
                beq.s   etv_critic4                 ;Table end => Remove Default
                bmi.s   etv_critic5                 ;Value negative => error number
etv_critic3:    cmpi.b  #-1,(A0)+                   ;to--1
                bne.s   etv_critic3
                bra.s   etv_critic2                 ;and continue
etv_critic4:    lea     etv_dtab(PC),A0
                bra.s   etv_critic6
etv_critic5:    cmp.b   D0,D2                       ;Error number found?
                bne.s   etv_critic2                 ;No => continue searching
etv_critic6:    move.b  (A0)+,D3                    ;Read everything up to the positive value
                bmi.s   etv_critic6
                move.b  D3,etv_siz+1                ;Insert the width of the alert
                lea     etv_txt(PC),A1              ;From here the texts are used
                movea.l A1,A2                       ;In case of emergency destroyed by drive
etv_critic7:    tst.b   (A0)                        ;Test 1st
                bmi.s   etv_critic9                 ;All enough, if negative
                move.l  A0,(A1)+                    ;Insert line card
                addq.l  #6,A1
etv_critic8:    move.b  (A0)+,D3
                beq.s   etv_critic7                 ;Line end?
                cmp.b   #'#',D3                     ;ID for the drive
                bne.s   etv_critic8
                lea     -1(A0),A2                   ;Add address of the drive ID
                move.b  D1,(A2)                     ;Insert drive
                bra.s   etv_critic8
etv_critic9:    move.w  etv_siz(PC),D3
                subi.w  #11,D3
                move.w  D3,etv_but
                movem.l D0/A2,-(SP)
                lea     etv_critic_rsc(PC),A0
                jsr     @form_do(A4)                ;Alert
                move.w  D0,D1
                movem.l (SP)+,D0/A2
                ext.w   D0
                ext.l   D0
                move.l  #$010000,D2                 ;Flag for "again"
                cmp.w   #1,D1                       ;Cancellation?
                beq.s   etv_critic10                ;yes =>
                move.l  D2,D0                       ;Rehearse
etv_critic10:   cmp.w   #-17,D0                     ;Disk changed?
                bne.s   etv_critic11
                move.l  D2,D0                       ;Then try again
etv_critic11:   move.b  #'#',(A2)                   ;Delete drive identifier
                movem.l (SP)+,D1-A6
                rts

etv_critic_rsc: DC.W 0,0
etv_siz:        DC.W 18
                DC.W 8,1
                DC.W 1,1
etv_txt:        DC.L 0
                DC.W 8
                DC.W 1,2
                DC.L 0
                DC.W 8
                DC.W 1,3
                DC.L 0
                DC.W 8
                DC.W 1,4
                DC.L 0
                DC.W 8

                DC.W 2,6
                DC.L etv_tx1
                DC.W $24
etv_but:        DC.W 10,6
                DC.L etv_tx2
                DC.W $26                            ;Default
                DC.W -1

etv_tab:        DC.B -1,-9,-15

                SWITCH language
                CASE 0
etv_dtab:       DC.B 31
                DC.B 'Ausgabegerät antwortet nicht!',0
                DC.B 'Ist es eventuell nicht ange-',0
                DC.B 'geschaltet?',0,-1
                DC.B -2,-3,-5,-6
                DC.B 28
                DC.B 'Floppy #: antwortet nicht.',0
                DC.B 'Bitte überprüfen und eine',0
                DC.B 'Disk einlegen.',0,-1
                DC.B -4,-7,-8,-10,-11,-12,-16
                DC.B 29
                DC.B 'Daten auf Disk #: defekt?',0
                DC.B 'Prüfen Sie die Disk und die',0
                DC.B 'Verbindungskabel.',0,-1
                DC.B -13
                DC.B 27
                DC.B 'Disk in Floppy #: ist',0
                DC.B 'schreibgeschützt. Vor dem',0
                DC.B 'nächsten Versuch',0
                DC.B 'Schreibschutz entfernen.',0,-1
                DC.B -14
                DC.B 29
                DC.B 'Die Anwendung kann die Disk',0
                DC.B 'in Floppy #: nicht lesen',0,-1
                DC.B -17
                DC.B 27
                DC.B 'Bitte Disk # in Floppy A:',0
                DC.B 'einlegen.',0,-1
                DC.B 0
etv_tx1:        DC.B ' ABBRUCH ',0
etv_tx2:        DC.B ' NOCHMAL ',0

                CASE 1                              ;~
etv_dtab:       DC.B 31
                DC.B 'Output device does not answer!',0
                DC.B 'Is it possibly not switched',0
                DC.B 'on?',0,-1
                DC.B -2,-3,-5,-6
                DC.B 28
                DC.B 'Floppy #: does not answer.',0
                DC.B 'Please check and have a',0
                DC.B 'disk inserted.',0,-1
                DC.B -4,-7,-8,-10,-11,-12,-16
                DC.B 29
                DC.B 'Data on disc #: defective?',0
                DC.B 'Check the disk and the',0
                DC.B 'connection cable.',0,-1
                DC.B -13
                DC.B 27
                DC.B 'Disk in Floppy #: is',0
                DC.B 'read-only. Before the',0
                DC.B 'next attempt,',0
                DC.B 'remove write protection.',0,-1
                DC.B -14
                DC.B 29
                DC.B 'The application can be the disk',0
                DC.B 'in Floppy #: do not read',0,-1
                DC.B -17
                DC.B 27
                DC.B 'Please insert disk #',0
                DC.B 'in Floppy A:.',0,-1
                DC.B 0
etv_tx1:        DC.B ' CANCEL ',0
etv_tx2:        DC.B ' AGAIN ',0
                ENDS

etv_tx3:        DC.B ' ',0
                EVEN
                ENDPART

********************************************************************************
* swv_vec - Vector in screen switching (no switchover)                         *
********************************************************************************
swv_vec:        rts

********************************************************************************
* Delete the entire memory & reset                                             *
********************************************************************************
                PART 'kill_all'
kill_all:       move    #$2700,SR
                lea     init_scr(A4),A0
                bsr     restore_scr                 ;Screen setting, as at startup
                lea     kill_a2(PC),A0
                moveq   #13,D0
                lea     8.w,A1
kill_a1:        move.l  (A0)+,(A1)+
                dbra    D0,kill_a1
                jmp     8.w
kill_a2:        lea     kill_a4(PC),A0
                move.l  A0,8.w
                lea     kill_a5(PC),A0
                moveq   #0,D0
                move.l  D0,D1
                move.l  D0,D2
                move.l  D0,D3
                move.l  D0,D4
                move.l  D0,D5
                move.l  D0,D6
                move.l  D0,D7
                movea.l D0,A1
                movea.l D0,A2
                movea.l D0,A3
                movea.l D0,A4
                movea.l D0,A5
                movea.l D0,A6
kill_a3:        movem.l D0-D7/A1-A6,(A0)
                lea     $38(A0),A0
                bra.s   kill_a3
kill_a4:        movea.l 4.w,A0
                jmp     (A0)
kill_a5:
                ENDPART

********************************************************************************
* Test if memory is from A6 readbar (Z deleted, if not)                        *
********************************************************************************
                PART 'check_read'
check_read:     tst.w   all_memory(A4)              ;Storage test?
                bne.s   check_read4                 ;No!=>
                cmpa.l  #$400000,A6                 ;until here no bus error through the MFP
                blo.s   check_read4
                cmpa.l  rom_base(A4),A6             ;Safe in the ROM?(necessary for 1040 STE)
                bhs.s   check_read1                 ;then continue =>
                cmpa.l  #$FA0000,A6                 ;Reading the ROM range Mg.
                blo.s   check_read5
check_read1:    cmpa.l  #$FF0000,A6
                blo.s   check_read4
                tst.b   tt_flag(A4)                 ;a TT?
                beq.s   check_read2                 ;No!=>
                cmpi.l  #$1357BD13,$05A8.w          ;No FAST MEM?
                bne.s   check_read2                 ;exactly =>
                cmpa.l  #$01000000,A6
                blo.s   check_read2                 ;Below the Fast Mems
                cmpa.l  $05A4.w,A6
                blo.s   check_read4                 ;i'm Fast Meme
check_read2:    movem.l D0-D2/A0-A1,-(SP)
                move    SR,D1                       ;Status Run
                movea.l SP,A0                       ;Stackpnt save
                ori     #$0700,SR                   ;Block all IRQS
                move.l  8.w,D2
                lea     check_read3(PC),A1
                move.l  A1,8.w
                moveq   #-1,D0
                tst.b   (A6)                        ;Access granted?
                moveq   #0,D0
check_read3:    move    D1,SR                       ;Statusreg Back
                movea.l A0,SP                       ;Stackpnt back
                move.l  D2,8.w                      ;Bus error vector back
                tst.w   D0                          ;Flag
                movem.l (SP)+,D0-D2/A0-A1
                rts
check_read4:    move    #$FF,CCR                    ;Z set, access allowed
                rts
check_read5:    move    #0,CCR                      ;Z deleted because access is not allowed
                rts
                ENDPART

********************************************************************************
* Test if the memory from A6 is writable (Z = 1, if yes)                       *
********************************************************************************
                PART 'check_write'
check_write:    tst.w   all_memory(A4)              ;Storage test?
                bne.s   check_write1                ;No!=>
                cmpa.w  #8,A6
                blo.s   check_write2                ;ROM area!
                cmpa.l  #$400000,A6
                blo.s   check_write1                ;Below Phystop => OK!
                tst.b   tt_flag(A4)                 ;a TT?
                beq.s   check_write2                ;No!=> Write not possible
                cmpi.l  #$1357BD13,$05A8.w          ;No FAST MEM?
                bne.s   check_write2                ;exactly => write not possible
                cmpa.l  #$01000000,A6
                blo.s   check_write2                ;Below the FAST MEMS => Error
                cmpa.l  $05A4.w,A6
                bhs.s   check_write2                ;Above the FAST MEMS
check_write1:   move    #$FF,CCR                    ;Set Z-flag
                rts
check_write2:   move    #0,CCR                      ;Delete Z-Flag
                rts
                ENDPART

********************************************************************************
* Keywart-Routine                                                              *
********************************************************************************
                PART 'check_keyb'
check_keyb:     tst.l   tmacro_pointer(A4)          ;Tmacro active?
                bne.s   check_3                     ;then no cancellation
                tst.l   tmacro_def_key(A4)          ;TMACRO definition active?
                bne.s   check_3                     ;then no cancellation
                jsr     @conin(A4)
                cmp.b   #27,D0                      ;esc
                beq.s   check_4                     ;=> Cancel
                cmp.b   #' ',D0                     ;No Space
                bne.s   check_3                     ;=> Do nothing
                bsr     clr_keybuff                 ;Clear keyboard buffer
check_2:        jsr     @conin(A4)                  ;Wait for key
                beq.s   check_2
                cmp.b   #27,D0                      ;esc
                beq.s   check_4                     ;=> Cancel
check_3:        move    #0,CCR                      ;Put flags for further
                rts
check_4:        move    #$FF,CCR                    ;Set flags for demolition
                rts
                ENDPART

********************************************************************************
* unique initialization                                                    *
********************************************************************************
                PART 'init_all'
init_all:       lea     varbase(PC),A4
                movea.l (SP),A3                     ;Pointer to Default Data (return address)
                moveq   #start-anfang-6,D0
                add.l   D0,(SP)                     ;Return account behind the default data
                pea     get_sysbase(PC)
                move.w  #$26,-(SP)
                trap    #14                         ;Get {{$ 4F2} +8} to A0
                addq.l  #6,SP
                movem.l $24(A0),A1-A2               ;KBShift and ACT_PD from Blitter-Tos
                cmpi.w  #$0102,$02(A0)              ;Is it the blitter-tos or later?
                bge.s   init2                       ;YES!
                lea     $0E1B.w,A1                  ;KBSHIFT-ADR (before the blitter TOS)
                lea     $602C.w,A2                  ;actPd (vorDemBlitterTos)
                move.w  $1C(A0),D0                  ;OS_CONF HOLEN
                lsr.w   #1,D0                       ;Ignore PAL / NTSC Fashion
                subq.w  #4,D0                       ;Spanish TOS 1.0?
                bne.s   init2                       ;No!=>
                lea     $873C-$602C(A2),A2          ;ACT_PD of the Spanish TOS 1.0
init2:          move.l  A1,kbshift_adr(A4)
                move.l  A2,act_pd(A4)
                move.l  8(A3),serial(A4)            ;Remember serial number
                move.l  8(SP),basepage(A4)

                movea.l #ende,A3
                adda.l  A4,A3                       ;Pointer to the end of the program

                lea     install_name9(PC),A0
                move.l  #'SYM'<<8,(A0)
                bsr     install_name                ;'BUGABOO.SYM' name
                move.w  #$2F,-(SP)
                trap    #1                          ;Fgetdta()
                addq.l  #2,SP
                movea.l D0,A6                       ;Remember ADR
                pea     dta_buffer(A4)
                move.w  #$1A,-(SP)
                trap    #1                          ;Fsetdta(New buffer)
                addq.l  #6,SP
                move.w  #7,-(SP)
                pea     fname(A4)
                move.w  #$4E,-(SP)
                trap    #1                          ;Fopen()
                addq.l  #8,SP
                tst.l   D0
                bmi.s   init20                      ;Not found =>
                clr.w   -(SP)
                pea     fname(A4)
                move.w  #$3D,-(SP)
                trap    #1                          ;Fopen(BUGABOO.SYM)
                addq.l  #8,SP
                move.l  D0,D7
                bmi.s   init20                      ;Error =>
                move.l  dta_buffer+26(A4),D6        ;Size of the file
                move.l  A3,sym_buffer(A4)
                move.l  A3,-(SP)
                move.l  D6,-(SP)
                addq.w  #1,D6
                and.b   #$FE,D6                     ;Check out the program end
                adda.l  D6,A3
                move.w  D7,-(SP)                    ;FileHandle on the stack
                move.w  #$3F,-(SP)
                trap    #1                          ;Fread()
                lea     12(SP),SP
                movea.l sym_buffer(A4),A0
                addq.l  #4,sym_buffer(A4)           ;Pointer behind the header
                cmpi.l  #'∑SYM',(A0)                ;Is that too a symbol table?
                bne.s   init22                      ;No!=>
                cmp.l   D0,D6                       ;All bytes read?
                beq.s   init21                      ;Yes!=>
init22:         clr.l   sym_buffer(A4)
init21:         subq.l  #4,D6                       ;Head header
                lsr.l   #5,D6                       ;Length of the table by 32
                move.w  D6,sym_anzahl(A4)           ;Number Remember
                move.w  D7,-(SP)
                move.w  #$3E,-(SP)
                trap    #1                          ;Fclose()
                addq.l  #4,SP
init20:         move.l  A6,-(SP)
                move.w  #$1A,-(SP)
                trap    #1                          ;Fsetdta(alter Buffer)
                addq.l  #6,SP

                move.l  A3,end_adr(A4)
                suba.l  #anfang-256,A3              ;programmlänge +Basepage
                move.l  A3,-(SP)                    ;= Long
                move.l  basepage(A4),-(SP)          ;Initial address
                move.l  #$4A0000,-(SP)
                trap    #1                          ;Mshrink()
                lea     12(SP),SP

                moveq   #-2,D0                      ;Clear keyboard macros
                move.l  D0,tmacro_tab(A4)
                move.l  D0,tmacro_tab_end(A4)

                pea     -1.w
                move.w  #$48,-(SP)
                trap    #1                          ;Request ADR of the largest memory block
                addq.l  #6,SP
                move.l  D0,D7
                move.l  D0,-(SP)
                move.w  #$48,-(SP)
                trap    #1                          ;Reserve total memory
                addq.l  #6,SP
                move.l  D0,first_free(A4)           ;Remember (for 'Load for Execute')
                add.l   D0,D7
                move.l  D7,end_of_mem(A4)
                move.l  D0,-(SP)
                move.w  #$49,-(SP)
                trap    #1                          ;Release memory
                addq.l  #6,SP

                bra     install_read                ;First read installation

get_sysbase:    move    SR,D7                       ;Determine the processor
                ori     #$0700,SR
                movea.l SP,A6
                moveq   #-1,D1                      ;68000
                movea.l $10.w,A2                    ;Illegal save
                lea     check_proz(PC),A0
                move.l  A0,$10.w                    ;new illegal
                DC.W $42C0                          ;MOVE CCR,D0
                moveq   #0,D1                       ;68010
                DC.W $49C0                          ;EXTB.L D0
                moveq   #1,D1                       ;68020
                DC.W $4E7A,$02                      ;MOVE CACR,D0
                bset    #9,D0                       ;Data-cache and (?)
                DC.W $4E7B,$02                      ;MOVE D0,CACR
                DC.W $4E7A,$02                      ;MOVE CACR,D0
                bclr    #9,D0                       ;Is the data cache?
                beq.s   check_proz                  ;No!=>
                moveq   #2,D1                       ;68030
                DC.W $4E7B,$02                      ;MOVE D0,CACR
check_proz:     movea.l A6,SP
                move.l  A2,$10.w                    ;old illegal vector back
                lea     varbase(PC),A4
                move.b  D1,prozessor(A4)            ;Remember processor
                bgt.s   check_proz1                 ;68020 or higher? Yes!=>
                move.w  _return(PC),clr_cache       ;68000/10: Do not delete cache
check_proz1:
                moveq   #0,D1                       ;No FPU
                movea.l $08.w,A0
                move.l  #check_fpu,$08.w
                tst.w   $FFFFFA40.w                 ;SFP004?
                addq.w  #1,D1
check_fpu:      movea.l A6,SP
                movea.l $2C.w,A1
                movea.l $34.w,A2
                move.l  #check_fpu3,$2C.w
                move.l  #check_fpu3,$34.w
                DC.L $F2800000                      ;FNOP
                DC.W $F327                          ;FSAVE -(SP)
                move.w  (SP),D0
                cmp.b   #24,D0                      ;68881?
                beq.s   check_fpu2                  ;Yes! =>
                cmp.b   #60,D0                      ;68882?
                beq.s   check_fpu1                  ;Yes! =>
                addq.w  #2,D1
check_fpu1:     addq.w  #2,D1
check_fpu2:     addq.w  #2,D1
check_fpu3:     movea.l A6,SP
                move.l  A0,$08.w
                move.l  A1,$2C.w
                move.l  A2,$34.w
                move.b  D1,fpu_flag(A4)

                movea.l $08.w,A5
                lea     check_ste(PC),A0
                move.l  A0,$08.w
                moveq   #0,D0                       ;no STE
                move.w  $FFFF9202.w,D1              ;1040STE? (Read joystickports)
                moveq   #-1,D0                      ;STE available!
check_ste:      move.b  D0,ste_flag(A4)
                lea     check_tt(PC),A0
                move.l  A0,$08.w
                moveq   #0,D0                       ;no TT
                move.w  $FFFF8400.w,D1              ;Color register of the TT read
                moveq   #-1,D0                      ;TT available
check_tt:       move.b  D0,tt_flag(A4)
                move.l  A5,$08.w
                movea.l A6,SP
                move    D7,SR

                movea.l $04F2.w,A0                  ;Sybase holen
                movea.l 8(A0),A0                    ;Bring the initial address of the ROMs
                move.l  A0,rom_base(A4)
_return:        rts
                ENDPART

********************************************************************************
* Everything mgl.initialize                                                    *
********************************************************************************
                PART 'init'
init:           ori     #$0700,SR
                lea     8.w,A0
                lea     save_data(A4),A1
                movea.l A1,A2
                move.w  #361,D1
init1:          move.l  (A0)+,(A1)+                 ;$8-$5AF save
                dbra    D1,init1
                move.l  D0,132(A2)                  ;Trap #3 insert
                move.l  $04BA.w,merk_it(A4)

                linea   #0 [ Init ]
                movem.l (A1),A0-A2
                move.l  76(A2),s_w_font(A4)
                move.l  76(A1),farbfont(A4)

                moveq   #0,D1
                tst.b   tt_flag(A4)
                bne.s   init3
                bsr     vsync_test
                bsr     vsync_test
                bsr     vsync_test                  ;OverScan dart
                sne     D1
                and.w   #1,D1
init3:          move.w  D1,overscan(A4)             ;Flag notice

                lea     init_scr(A4),A1
                movea.l A1,A2
                bsr     save_scr
                sf      scr_overscan(A2)            ;OverScan take for the user program

                lea     debugger_scr(A4),A1
                movea.l A1,A3
                move.w  col0(A4),(A3)+
                moveq   #14,D0
init4:          move.w  col1(A4),(A3)+
                dbra    D0,init4
                clr.b   scr_offset(A1)              ;STE-Register will not be used
                clr.b   scr_hscroll(A1)
                move.b  scr_sync(A2),scr_sync(A1)   ;Take over Sync
                st      scr_overscan(A1)            ;Overscan for the debugger always out
                moveq   #2,D0                       ;The high resolution
                tst.b   tt_flag(A4)                 ;a TT?
                bne.s   init5                       ;Yes!=> Always the high resolution
                move.b  scr_moni(A2),scr_moni(A1)   ;The current monitor
                beq.s   init5                       ;The high resolution =>
                moveq   #1,D0                       ;otherwise choose the middle
init5:          move.b  D0,scr_rez(A1)
                move.l  #hires+255,D0
                add.l   A4,D0
                clr.b   D0
                move.l  D0,scr_adr(A1)              ;Screen page for the debugger

                move.w  #5,upper_line(A4)
                move.w  #400,upper_offset(A4)
                move.w  #20,down_lines(A4)

                move.w  #319,mausx(A4)
                move.w  #199,mausy(A4)
                st      mausflg(A4)
                st      mausmove(A4)
                st      _dumpflg(A4)

;Tastatur initialisieren
                move.w  #$1111,timer_c_bitmap(A4)   ;200Hz-Timer-Divider (on 50Hz)
                lea     iorec_IKBD(A4),A0
                lea     iorec_puffer(A4),A1
                move.l  A1,(A0)+
                clr.l   (A0)                        ;Set keyboard buffer and empty

                move.l  #$0E0001,-(SP)
                trap    #14                         ;Iorec(keyboard)
                addq.l  #4,SP
                movea.l D0,A0
                clr.l   6(A0)                       ;Clear keyboard buffer

                moveq   #-1,D0
                move.l  D0,-(SP)
                move.l  D0,-(SP)
                move.l  D0,-(SP)
                move.w  #$10,-(SP)
                trap    #14
                lea     14(SP),SP
                movea.l D0,A0
                lea     29(A0),A1
                move.l  A1,save_clrkbd(A4)
                clr.b   (A1)                        ;~ Nude. Button Delete (Auto Repeat!)

                lea     std_keytab(A4),A1
                move.l  (A0)+,(A1)+
                move.l  (A0)+,(A1)+                 ;Put keyboard plates
                movea.l (A0),A0
                lea     caps_tab(A4),A2
                move.l  A2,(A1)
                moveq   #31,D0
init6:          move.l  (A0)+,(A2)+                 ;Copy Caps / Lock table
                dbra    D0,init6
                movea.l (A1),A0
                move.b  #'A',$63(A0)
                move.b  #'B',$64(A0)
                move.b  #'C',$65(A0)                ;Change the occupancy of the zoom block
                move.b  #'D',$66(A0)
                move.b  #'E',$4A(A0)
                move.b  #'F',$4E(A0)
                move.b  #',',$71(A0)

                lea     stab(PC),A0                 ;Pointer to the keyboard table
init7:          tst.w   (A0)+
                beq.s   init10                      ;End of the table
                tst.w   (A0)                        ;asciiCode=0?
                beq.s   init9                       ;=> Next
                movea.l -4(A1),A2                   ;Shift table
                cmpi.b  #1,2(A0)                    ;shift?
                beq.s   init8                       ;tastePatchen
                movea.l -8(A1),A2                   ;normal table
init8:          moveq   #0,D0
                move.b  3(A0),D0                    ;Get ASCII code from the table
                move.b  0(A2,D0.w),1(A0)            ;Copy ASCII code
init9:          addq.l  #4,A0                       ;Pointer to the next key
                bra.s   init7

init10:         move.w  #$22,-(SP)
                trap    #14
                addq.l  #2,SP
                movea.l D0,A0
                lea     kbdvbase(A4),A1
                moveq   #8,D0
init11:         move.l  (A0)+,(A1)+                 ;Save
                dbra    D0,init11

                pea     -1.w
                move.w  #$23,-(SP)
                trap    #14                         ;Kbrate()
                addq.l  #6,SP
                move.w  D0,kbd_r_init(A4)           ;Take original values

************************************************************************
* Save / set vectors                                                   *
************************************************************************
                move.l  $84.w,old_gemdos
                move.l  $88.w,old_aesvdi
                move.l  $B4.w,old_bios
                move.l  $B8.w,old_xbios
                move.l  $0404.w,old_critic
                bsr     set_vek

                move.b  #2,find_cont0(A4)           ;Forbid
                move.w  #1,dsk_sektor(A4)           ;Read / write defaults for sector
                move.l  #sekbuff,D0
                add.l   A4,D0
                move.l  D0,dsk_adr(A4)              ;For sector read
                move.l  first_free(A4),dsk_adr2(A4) ;for Track Read

                lea     merk_internal(A4),A0
                moveq   #15,D0
init12:         move.b  D0,$FFFF8800.w
                move.b  $FFFF8800.w,(A0)+           ;Remember Sound Chip Register
                dbra    D0,init12
                lea     regtabl(PC),A2
                moveq   #20,D0
init13:         movea.w (A2)+,A1
                move.b  (A1),(A0)+                  ;Remember other registers
                dbra    D0,init13

                clr.b   merk_user(A4)
                move.l  first_free(A4),default_adr(A4) ;as a default address
                move.l  #'*.*'<<8,dir_ext(A4)       ;Directory buffer with '*. *' Occupy
                movea.l #trace_buff,A0
                adda.l  A4,A0
                move.l  A0,trace_pos(A4)            ;positionImTracebuffer
                move.l  A0,reg_pos(A4)
                movea.l #user_trace_buf,A0
                adda.l  A4,A0
                move.l  #$70004E75,(A0)             ;moveq #0,d0:rts
                jsr     clr_cache
                clr.l   untrace_funk(A4)            ;Delete text of the demolition function
                lea     code_tab,A0
                moveq   #-1,D0
init14:         addq.l  #1,D0
                move.w  D0,D1
                lsl.w   #4,D1
                tst.b   0(A0,D1.w)
                bpl.s   init14
                move.w  D0,tablen(A4)
                rts
                ENDPART

********************************************************************************
* All back                                                                     *
********************************************************************************
                PART 'reset_all'
copy_sys_vars:  move.l  $04BA.w,D1
                lea     save_data(A4),A0
                lea     8.w,A1
                move.w  #361,D0
copy_sys_vars1: move.l  (A0)+,(A1)+                 ;Storage block back
                dbra    D0,copy_sys_vars1
                move.l  hz200_time(A4),D0
                cmp.l   D0,D1
                bhs.s   copy_sys_vars2              ;Prevent timer underflow (hard disk!)
                move.l  D0,D1
copy_sys_vars2: move.l  D1,hz200_time(A4)
                move.l  D1,$04BA.w                  ;Important for hard disk!
                rts

reset_all:      ori     #$0700,SR                   ;All lock everything

                moveq   #$13,D0
                jsr     @ikbd_send(A4)              ;Keyboard out


                bsr.s   copy_sys_vars

                tst.b   resident(A4)
                beq.s   reset_all1
                lea     @_trap3(A4),A0
                move.l  A0,$8C.w                    ;Insert your own trap
reset_all1:     bsr     kill_programm               ;Possibly. Remove loaded PRG

                ori     #$0700,SR                   ;All lock everything
                lea     merk_internal(A4),A0
                moveq   #15,D0
reset_all2:     move.b  D0,$FFFF8800.w
                move.b  (A0)+,$FFFF8802.w           ;Sound chip tab Back
                dbra    D0,reset_all2
                lea     regtabl(PC),A2
                moveq   #20,D0
reset_all3:     movea.w (A2)+,A1                    ;Other registers back
                move.b  (A0)+,(A1)
                dbra    D0,reset_all3

                bsr     ikbd_reset                  ;keyboardReset
                move.b  #3,$FFFFFC04.w              ;midiReset
                move.b  #$95,$FFFFFC04.w

                move.w  #$22,-(SP)
                trap    #14
                addq.l  #2,SP
                movea.l D0,A1
                lea     kbdvbase(A4),A0
                moveq   #8,D0
reset_all4:     move.l  (A0)+,(A1)+                 ;and back again
                dbra    D0,reset_all4

                bsr     update_pc

                lea     init_scr(A4),A0
                lea     no_overscan(A4),A1
                bsr     restore_scr

                moveq   #$80,D0
                jsr     @ikbd_send(A4)
                moveq   #1,D0                       ;keyboardReset
                jmp     @ikbd_send(A4)
                ENDPART

********************************************************************************
* Set Vectors                                                                  *
********************************************************************************
                PART 'set_vek'
set_vek:        lea     etv_critic(PC),A0
                move.l  A0,$0404.w                  ;New ETV_CRITIC handler
                IFEQ ^^SYMTAB
                bsr.s   set_spez_vek                ;Error vectors pure
                move.l  #$31415926,$0426.w          ;Resource
                move.l  #do_reset,$042A.w           ;reset Vector Bend
                lea     swv_vec(PC),A0
                move.l  A0,$046E.w                  ;New vector for monitor switching
                ENDC
                rts
                ENDPART

********************************************************************************
* Insert error vectors                                                         *
********************************************************************************
                PART 'set_spez_vek'
set_spez_vek:   lea     except_start+8(PC),A0
                lea     $08.w,A1
                movea.l $5C.w,A2                    ;A secure finish ...
                movea.l $04F2.w,A3
                movea.l 8(A3),A3                    ;PTR on the Rome
                moveq   #2,D1
set_spez_vek1:  moveq   #7,D2
                and.w   D1,D2                       ;Insulate bit position
                moveq   #7,D4                       ;bit70In
                sub.w   D2,D4                       ;Convert bit 0..7
                move.w  D1,D3
                lsr.w   #3,D3                       ;Determine byte position
                btst    D4,set_spez_tab(PC,D3.w)
                bne.s   set_spez_vek3
                tst.b   tt_flag(A4)                 ;a TT?
                bne.s   set_spez_vek2               ;Yes!=>
                tst.b   (A1)                        ;Vector already occupied?
                bne.s   set_spez_vek3               ;Sure no!=>
set_spez_vek2:  cmpa.l  (A1),A2                     ;Vector shows on bombs?
                bne.s   set_spez_vek4               ;No!=>
set_spez_vek3:  cmpa.w  #$2C,A1                     ;lineF?
                beq.s   set_spez_vek4               ;then nothing =>
                move.l  (A1),(A0)+
                move.l  A0,(A1)                     ;Clink in the vector
                subq.l  #4,A0
set_spez_vek4:  lea     22(A0),A0                   ;Skip routine
                addq.l  #4,A1                       ;to the next vector
                addq.w  #1,D1
                cmp.w   #64,D1
                bne.s   set_spez_vek1
                tst.b   tt_flag(A4)                 ;a TT?
                beq.s   set_spez_vek5               ;No!=>
                lea     old_privileg(PC),A1
                movea.l $20.w,A0                    ;current vector (shows in the bugaboo!)

                move.l  A0,own_privileg7+2-old_privileg(A1)
                move.l  -(A0),(A1)+                 ;Copy old vector for xbra
                move.l  A1,$20.w                    ;Own vector for privilege injury
set_spez_vek5:  rts

set_spez_tab:   DC.B %11111111                      ;$00-$1C   Bit = 1 : Vector always occupy
                DC.B %11001111                      ;$20-$3C
                DC.B %11111111                      ;$40-$5C   Bit = 0 : Vector only if necessary
                DC.B %11010111                      ;$60-$7C
                DC.B %0                             ;$80-$9C
                DC.B %0                             ;$A0-$BC
                DC.B %11111111                      ;$C0-$DC
                DC.B %11111111                      ;$E0-$FC
                ENDPART

********************************************************************************
* Register, which must be reset with reset                                     *
********************************************************************************
regtabl:        DC.W $8001,$FA01,$FA03,$FA05,$FA07
                DC.W $FA09,$FA0B,$FA0D,$FA0F,$FA11,$FA13,$FA15,$FA17
                DC.W $FA19,$FA1B,$FA1D,$FA27,$FA29,$FA2B,$FA2D,$FA2F

********************************************************************************
* RESET-vector (D0,A0,A5-A6 are invalid)                                       *
********************************************************************************
                PART 'do_reset'
reset_txt:      DC.B '?RESET',13,0
                EVEN

do_reset:       lea     varbase(PC),A0              ;A0 is anyway
                move    SR,_sr(A0)                  ;S=1,T=0
                movem.l D0-A6,regs(A0)              ;D0,A0,A5,A6 have been changed
                movea.l A0,A4                       ;Put Varbase properly
                move    USP,A0
                move.l  A0,_usp(A4)                 ;That is also unchanged
                clr.l   $0426.w                     ;Reset-Valid killen
                move.l  save_data+1178(A4),$04A2.w
                movea.l default_stk(A4),SP          ;Stackpointer get back
                lea     merk_internal(A4),A0
                moveq   #15,D0
do_reset1:      move.b  D0,$FFFF8800.w
                move.b  (A0)+,$FFFF8802.w           ;Sound-Chip-Register return
                dbra    D0,do_reset1
                lea     regtabl(PC),A2
                moveq   #20,D0
do_reset2:      movea.w (A2)+,A1                    ;other Register return
                move.b  (A0)+,(A1)
                dbra    D0,do_reset2

                move.b  #3,$FFFFFC00.w              ;Keyboard-Reset
                move.b  #$96,$FFFFFC00.w
                move.b  #3,$FFFFFC04.w              ;MIDI-Reset
                move.b  #$95,$FFFFFC04.w

                move.b  $FFFFFA01.w,D0              ;Color monitor?
                bmi.s   do_reset6                   ;Yes!=>
                lea     $FFFFFA21.w,A0              ;Timer B Data-Register
                lea     $FFFFFA1B.w,A1              ;Timer B Control-Register
                move.b  #$10,(A1)                   ;Timer B Set output level Low
                moveq   #1,D4
                move.b  #0,(A1)                     ;Timer B fuses
                move.b  #240,(A0)                   ;Timer B Set to 240
                move.b  #8,(A1)                     ;Timer B: Events
do_reset3:      move.b  (A0),D0
                cmp.b   D4,D0                       ;counter = 1?
                bne.s   do_reset3                   ;No!=>
do_reset4:      move.b  (A0),D4                     ;Read start value
                move.w  #615,D3                     ;616 times the value must remain constant
do_reset5:      cmp.b   (A0),D4                     ;Still constant?
                bne.s   do_reset4                   ;No!=> Again
                dbra    D3,do_reset5                ;next pass
                move.b  #$10,(A1)                   ;Timer B output low
                move.b  #2,$FFFF8260.w              ;put on monochrome

do_reset6:      lea     no_overscan(A4),A1
                lea     init_scr(A4),A0
                bsr     restore_scr

                moveq   #$80,D0
                jsr     @ikbd_send(A4)
                moveq   #1,D0                       ;Keyboard-RESET
                jsr     @ikbd_send(A4)

                jsr     @cursor_off(A4)             ;Cursor
                lea     debugger_scr(A4),A0
                movea.l scr_adr(A0),A0
                move.w  #1999,D0
do_reset7:      clr.l   (A0)+                       ;Delete the Hires
                clr.l   (A0)+
                clr.l   (A0)+
                clr.l   (A0)+
                dbra    D0,do_reset7
                jsr     @redraw_all(A4)             ;Rebuild screen
                tst.w   spalte(A4)
                beq.s   do_reset8
                jsr     @crout(A4)                  ;Output Cr, if the cursor does not column 0
do_reset8:      jsr     @c_eol(A4)                  ;delete a line
                pea     reset_txt(PC)
                jsr     @print_line(A4)             ;Time a little message
                bsr     breakclr                    ;Eventually BreakPoints Remove
                jsr     @page1(A4)                  ;debuggerscreenAn
                clr.b   kbshift(A4)
                move.l  #$31415926,$0426.w          ;Resource
                move.l  #do_reset,$042A.w           ;Reset-Vector bend
                jmp     (A4)
                ENDPART

********************************************************************************
* Install original bus error vector                                            *
********************************************************************************
                PART 'set_buserror'
set_buserror:   move.l  #except_start+12,$08.w
                rts
                ENDPART

********************************************************************************
* Own keyboard driver (if necessary).                                          *
********************************************************************************
                PART 'my_driver'
my_driver:      move    SR,-(SP)
                ori     #$0700,SR
                movem.l D0-A6,-(SP)
                lea     varbase(PC),A4
                tst.b   do_resident(A4)             ;Do not install when resident
                bne     my_driver8                  ;is desired

                lea     merk_user(A4),A0
                tas.b   (A0)+
                bne.s   my_driver3                  ;Drivers are already in it
                moveq   #15,D0
my_driver1:     move.b  D0,$FFFF8800.w
                move.b  $FFFF8800.w,(A0)+           ;Sound-Chip-Register notice
                dbra    D0,my_driver1

                lea     regtabl(PC),A2
                moveq   #20,D0
my_driver2:     movea.w (A2)+,A1
                move.b  (A1),(A0)+                  ;MFP Register (+GLUE) notice
                dbra    D0,my_driver2

                lea     $FFFF8800.w,A0
                lea     $FFFF8802.w,A1
                move.b  #7,(A0)
                move.b  D0,(A1)                     ;Tonguerators from (D0 = -1 S.O.)
                moveq   #0,D0
                move.b  #8,(A0)
                move.b  D0,(A1)
                move.b  #9,(A0)                     ;All volumes to zero
                move.b  D0,(A1)
                move.b  #10,(A0)
                move.b  D0,(A1)

                lea     $FFFFFA01.w,A0
                moveq   #0,D0
                movep.l D0,0(A0)
                movep.l D0,8(A0)
                movep.l D0,$10(A0)
                move.b  #$48,$FFFFFA17.w
                bset    #2,2(A0)
                move.b  #$C0,$FFFFFA23.w            ;Program Timer C to 200Hz
                ori.b   #$50,$FFFFFA1D.w            ;Timer C start

                moveq   #$60,D0
                move.b  D0,$FFFFFA09.w              ;Release 200Hz-Timer & Keyboard
                move.b  D0,$FFFFFA15.w

                bsr     ikbd_reset

my_driver3:     lea     mfp_irq(PC),A2
                cmpa.l  $0118.w,A2
                beq.s   my_driver4
                moveq   #6,D0
                bsr     install_irq
                lea     spez_keyb(PC),A2
                cmpa.l  A0,A2
                beq.s   my_driver4
                move.l  A0,old_spez_keyb
                move.l  A0,old_ikbd
                clr.b   kbstate(A4)
                clr.b   kbd_repeat_on(A4)

my_driver4:     lea     hz200_irq(PC),A2
                cmpa.l  $0114.w,A2
                beq.s   my_driver5
                moveq   #5,D0
                bsr     install_irq
                move.l  A0,old_hz200

my_driver5:     bsr     clr_keybuff
                andi.b  #$10,kbshift(A4)
                moveq   #$80,D0
                jsr     @ikbd_send(A4)              ;Keyboard-RESET
                moveq   #1,D0
                jsr     @ikbd_send(A4)
                move.b  $FFFFFC00.w,D0
                bpl.s   my_driver6
                move.b  $FFFFFC02.w,D0
                nop
                move.b  $FFFFFC02.w,D0
                nop
                move.b  $FFFFFC02.w,D0
                nop
                move.b  $FFFFFC02.w,D0
                nop
                move.b  $FFFFFC02.w,D0
                nop
my_driver6:     andi.b  #$9F,$FFFFFA11.w            ;200Hz-Timer & Keyboard release
                move.l  $70.w,D0
                lea     my_vbl(PC),A0
                cmpa.l  D0,A0
                beq.s   my_driver7
                move.l  D0,old_vbl
                move.l  A0,$70.w

my_driver7:     lea     etv_critic(PC),A0
                movea.l $0404.w,A1
                cmpa.l  A1,A0
                beq.s   my_driver8
                move.l  A1,old_critic
                move.l  A0,$0404.w
my_driver8:     movem.l (SP)+,D0-A6
                move    (SP)+,SR
                rts
ikbd_reset:     moveq   #0,D1                       ;OverScan the end
                lea     debugger_scr(A4),A0
                bsr     check_screen                ;the DebuggerScreen an?
                beq.s   ikbd_reset1                 ;Yes!=>
                move.w  overscan(A4),D1             ;overScan
                lsl.w   #6,D1                       ;$00:no OverScan, $40:overScan
ikbd_reset1:    moveq   #3,D0
                or.b    D1,D0
                move.b  D0,$FFFFFC00.w              ;Keyboard-Reset
                moveq   #$96,D0
                or.b    D1,D0
                move.b  D0,$FFFFFC00.w
                rts
                ENDPART

********************************************************************************
* Original keyboarding strumbs and.                                            *
********************************************************************************
                PART 'org_driver'
org_driver:     move    SR,-(SP)
                ori     #$0700,SR
                movem.l D0-A6,-(SP)
                lea     varbase(PC),A4
                moveq   #6,D0
org_driver1:    move    #$2300,SR
org_driver2:    tst.b   kbstate(A4)                 ;Still a package on the road?
                bne.s   org_driver2                 ;Yes!=> Wait
                move    #$2700,SR                   ;IRQS
                tst.b   kbstate(A4)                 ;Just a package?
                bne.s   org_driver1                 ;Yes!=> wait
                movea.l old_spez_keyb(PC),A2
                tst.w   shift_flag(A4)
                bne.s   org_driver3
                lea     spez_keyb(PC),A2
org_driver3:    bsr.s   install_irq                 ;IKBD-vector
                moveq   #5,D0
                movea.l old_hz200(PC),A2
                bsr.s   install_irq                 ;200Hz-Timer
                bsr     clr_keybuff
                move.l  old_vbl(PC),$70.w           ;VBL-vector
                move.l  old_critic(PC),$0404.w

                lea     merk_user(A4),A0
                clr.b   (A0)+                       ;Driver now outside
                moveq   #15,D0
org_driver4:    move.b  D0,$FFFF8800.w
                move.b  (A0)+,$FFFF8802.w           ;Sound-Chip-Register return
                dbra    D0,org_driver4

                lea     regtabl(PC),A2
                moveq   #20,D0
org_driver5:    movea.w (A2)+,A1                    ;Other registers back
                move.b  (A0)+,(A1)
                dbra    D0,org_driver5

                lea     ikbd_string(A4),A0          ;String to the keyboard?
                moveq   #0,D0
                move.b  (A0)+,D0
                beq.s   org_driver7
                subq.w  #1,D0
org_driver6:    move.b  (A0)+,D0
                jsr     @ikbd_send(A4)              ;to the keyboard
                dbra    D0,org_driver6

org_driver7:    tst.w   ring_flag(A4)               ;Ring-Indicator test?
                bne.s   org_driver8                 ;No =>
                moveq   #14,D0
                bsr.s   enable_irq                  ;allow Ring-Indicator
org_driver8:    movea.l save_clrkbd(A4),A0
                clr.b   (A0)                        ;Auto-Repeat-Taste Clear!
                movem.l (SP)+,D0-A6
                move    (SP)+,SR
                rts
                ENDPART

********************************************************************************
* MFP-Routine                                                                  *
********************************************************************************
********************************************************************************
* MFP Put interrupt vector                                                     *
* D0 = Vector number                                                           *
* A2 = Address of the new IRQ-Routine                                           *
* >A0 = Address of the old IRQ-Routine                                          *
********************************************************************************
                PART 'install_irq'
install_irq:    movem.l D0-D2/A1-A2,-(SP)
                bsr.s   disable_irq
                move.l  D0,D2
                lsl.w   #2,D2
                addi.l  #$0100,D2
                movea.l D2,A1
                movea.l (A1),A0                     ;Remember old vector
                move.l  A2,(A1)                     ;Set new vector
                bsr.s   enable_irq
                movem.l (SP)+,D0-D2/A1-A2
                rts
                ENDPART

********************************************************************************
* MFP-IRQ D0 lock                                                              *
********************************************************************************
                PART 'disable_irq'
disable_irq:    movem.l D0-D1/A0-A1,-(SP)
                lea     $FFFFFA01.w,A0
                lea     $12(A0),A1
                bsr.s   bselect
                bclr    D1,(A1)
                lea     6(A0),A1
                bsr.s   bselect
                bclr    D1,(A1)
                lea     $0A(A0),A1
                bsr.s   bselect
                bclr    D1,(A1)
                lea     $0E(A0),A1
                bsr.s   bselect
                bclr    D1,(A1)
                movem.l (SP)+,D0-D1/A0-A1
                rts
                ENDPART

********************************************************************************
* MFP-IRQ D0 release                                                           *
********************************************************************************
                PART 'enable_irq'
enable_irq:     movem.l D0-D1/A0-A1,-(SP)
                lea     $FFFFFA01.w,A0
                lea     6(A0),A1
                bsr.s   bselect
                bset    D1,(A1)
                lea     $12(A0),A1
                bsr.s   bselect
                bset    D1,(A1)
                movem.l (SP)+,D0-D1/A0-A1
                rts
                ENDPART

********************************************************************************
* Bit/Register number determine for MFP                                        *
********************************************************************************
                PART 'bselect'
bselect:        move.b  D0,D1
                cmp.b   #8,D1
                blt.s   bselec1
                subq.w  #8,D1
                rts
bselec1:        addq.l  #2,A1
                rts
                ENDPART

********************************************************************************
* That's it for the MFP                                                        *
********************************************************************************

********************************************************************************
* Switch on the Debugger screen                                                *
********************************************************************************
                PART 'page1'
page1:          movem.l A0-A1/A4,-(SP)
                lea     varbase(PC),A4
                tst.b   do_resident(A4)             ;become automatically resident?
                bne.s   page11                      ;then do not switch
                lea     debugger_scr(A4),A0
                bsr.s   check_screen                ;Is the Debugger screen?
                beq.s   page11                      ;Yes!=>
                lea     user_scr(A4),A1
                bsr.s   set_screen                  ;Switch on the Debugger screen
page11:         movem.l (SP)+,A0-A1/A4
                rts
                ENDPART

********************************************************************************
* Switch on the graphic side of the third-party program                        *
********************************************************************************
                PART 'page2'
page2:          movem.l A0-A1/A4,-(SP)
                lea     varbase(PC),A4
                lea     user_scr(A4),A0
                bsr.s   check_screen                ;Is the user screen?
                beq.s   page21                      ;Yes!=>
                lea     debugger_scr(A4),A1
                bsr.s   set_screen                  ;Turn on the user screen
page21:         movem.l (SP)+,A0-A1/A4
                rts
                ENDPART

********************************************************************************
* OverScan Turn on or off (D0=$40 bzw. D0=$00)                                 *
********************************************************************************
rs_save         SET ^^RSCOUNT
                RSRESET
scr_colors:     RS.W 16                             ;The 16 colors
scr_adr:        RS.L 1                              ;The video address
scr_offset:     RS.B 1                              ;Offset to next line (STE only)
scr_hscroll:    RS.B 1                              ;Horizontal Bit-wise Scroll (STE only)
scr_rez:        RS.B 1                              ;The video resolution
scr_sync:       RS.B 1                              ;the Sync-Bit from Shifters
scr_moni:       RS.B 1                              ;The monitor ($ 00: B / W $ 40: color)
scr_overscan:   RS.B 1                              ;Overscan ($ 00: Yes $ FF: No)
                RSEVEN
scr_struct      EQU ^^RSCOUNT                       ;Size of the screen structure
                RSSET rs_save
                PART 'check_screen'                 ;Page of the structure A0 active?
check_screen:   move.l  D0,-(SP)
                bsr     get_scradr                  ;Get the current screen address
                cmp.l   scr_adr(A0),D0              ;Is the address correct?
                movem.l (SP)+,D0                    ;Z = 1, if yes
                rts
                ENDPART

********************************************************************************

                PART 'set_screen'                   ;Save according to Structure A1, set structure A0
set_screen:     bsr.s   save_scr                    ;Save screen values according to A1
                bra     restore_scr                 ;from A0
                ENDPART

********************************************************************************

                PART 'save_scr'                     ;Screen parameters from A1 Save
save_scr:       movem.l D0-D1/A2-A3,-(SP)
                move    SR,-(SP)
                ori     #$0700,SR                   ;Block all IRQS
                tst.w   smart_switch(A4)            ;Normal switching?
                beq.s   save_scr0                   ;And!=>
                bsr     vsync_test                  ;Wait beam return
save_scr0:      move.w  #$0777,D0                   ;3 color bits in old STS
                move.b  tt_flag(A4),D1
                or.b    ste_flag(A4),D1
                beq.s   save_scr1                   ;No Ste / TT =>
                move.w  #$0FFF,D0                   ;4 color bits at the STE or TT
save_scr1:      lea     $FFFF8240.w,A2
                lea     scr_colors(A1),A3
                moveq   #15,D1
save_scr2:      move.w  (A2)+,(A3)
                and.w   D0,(A3)+                    ;Save the colors
                dbra    D1,save_scr2
                bsr     get_scradr
                move.l  D0,scr_adr(A1)              ;save the current video address
                clr.b   scr_offset(A1)
                clr.b   scr_hscroll(A1)
                move.b  $FFFF820A.w,scr_sync(A1)    ;save the Sync-Bit
                tst.b   tt_flag(A4)                 ;a TT?
                beq.s   save_scr3                   ;No =>
                moveq   #7,D0
                and.b   $FFFF8262.w,D0
                move.b  D0,scr_rez(A1)              ;save the TT resolution
                bra.s   save_scr5
save_scr3:      tst.b   ste_flag(A4)
                beq.s   save_scr4                   ;no STE =>
                move.b  $FFFF820F.w,scr_offset(A1)  ;Offset to next line
                move.b  $FFFF8265.w,scr_hscroll(A1) ;Horizontal Bit-wise Scroll
save_scr4:      moveq   #3,D0
                and.b   $FFFF8260.w,D0
                moveq   #2,D1
                cmp.b   D1,D0                       ;Value valid?
                bls.s   save_scr6                   ;Yes!=>
                move.w  D1,D0                       ;otherwise accept St-High
save_scr6:      move.b  D0,scr_rez(A1)              ;save the current resolution
                moveq   #$80,D0
                and.b   $FFFFFA01.w,D0
                lsr.b   #1,D0
                move.b  D0,scr_moni(A1)             ;The current monitor
save_scr5:      move    (SP)+,SR
                movem.l (SP)+,D0-D1/A2-A3
                rts
                ENDPART

********************************************************************************

                PART 'restore_scr'                  ;Reset screen parameters from A0 (A1: old setting)
restore_scr:    tst.w   smart_switch(A4)            ;Normal switching?
                beq.s   restore_it                  ;Yes!=>
                move.l  $70.w,-(SP)
                move    SR,-(SP)
                move.l  #restore_vbl,$70.w
                sf      restore_vbl_flag
                andi    #~$0700,SR                  ;IRQS again
restore_scr0:   tst.b   restore_vbl_flag            ;Switch over VBL
                beq.s   restore_scr0
                move    (SP)+,SR
                move.l  (SP)+,$70.w
                rts

restore_vbl_flag:DC.W 0
restore_vbl:    bsr.s   restore_it
                st      restore_vbl_flag
                rte

restore_it:     movem.l D0-A2,-(SP)
                move    SR,-(SP)
                ori     #$0700,SR                   ;Block all IRQS
                tst.w   overscan(A4)                ;overScanActive?
                beq.s   restore_scr4                ;No!=>
                moveq   #$96,D0                     ;Switch off overscan
                tst.b   scr_overscan(A0)            ;Use Overscan?
                bne.s   restore_scr3                ;No!=>
                tst.w   smart_switch(A4)            ;In the VBL switched?
                bne.s   restore_it0                 ;Yes!=>
                bsr     vsync_test                  ;Wait beam return
;It is important that still lost some bars before switching
;Otherwise, "in good time" is still switched, i. it always flickers!
restore_it0:    moveq   #0,D0
                cmpi.b  #2,scr_rez(A0)              ;SM124?
                bne.s   restore_scr1                ;No!=>
                moveq   #-1,D0
restore_scr1:   moveq   #15,D1
                lea     $FFFF8240.w,A2
restore_scr2:   move.w  D0,(A2)+                    ;All colors on black
                dbra    D1,restore_scr2
                moveq   #$D6,D0                     ;Overscan turn on
restore_scr3:   move.b  D0,$FFFFFC00.w              ;Overscan switch
restore_scr4:   tst.b   tt_flag(A4)                 ;a TT?
                beq.s   restore_scr5                ;No!=>
                moveq   #$F8,D0
                and.b   $FFFF8262.w,D0
                or.b    scr_rez(A0),D0              ;Set the resolution of the TT
                move.b  D0,$FFFF8262.w
                bra.s   restore_scr6
restore_scr5:   lea     $FFFF8800.w,A2
                move.b  #14,(A2)
                moveq   #$BF,D0
                and.b   (A2),D0                     ;switch the monitor
                or.b    scr_moni(A0),D0
                move.b  D0,2(A2)
                move.b  scr_rez(A0),$FFFF8260.w     ;Set new resolution
restore_scr6:   move.l  scr_adr(A0),D0
                bsr.s   set_scradr                  ;Set new video address
                move.b  scr_sync(A0),$FFFF820A.w    ;Set new sync
                tst.b   ste_flag(A4)
                beq.s   restore_scr7                ;No Ste =>
                move.b  scr_offset(A0),$FFFF820F.w  ;Offset to next line
                move.b  scr_hscroll(A0),$FFFF8265.w ;Horizontal Bit-wise Scroll
restore_scr7:   movem.l scr_colors(A0),D0-D7
                movem.l D0-D7,$FFFF8240.w           ;and put the colors
                move    (SP)+,SR
                movem.l (SP)+,D0-A2
                rts
                ENDPART

********************************************************************************

                PART 'set_scradr'                   ;Set screen address in D0
set_scradr:     tst.b   tt_flag(A4)                 ;a TT?
                bne.s   set_scradr0                 ;Yes!=>
                tst.b   ste_flag(A4)                ;A STE present?
                beq.s   set_scradr1                 ;No!=>
set_scradr0:    move.b  D0,$FFFF820D.w              ;low-Byte set
set_scradr1:    lsr.w   #8,D0
                move.b  D0,$FFFF8203.w              ;mid-Byte set
                swap    D0
                move.b  D0,$FFFF8201.w              ;high-Byte set
                rts
                ENDPART

********************************************************************************

                PART 'get_scradr'                   ;Get screen address according to D0
get_scradr:     moveq   #0,D0
                move.b  $FFFF8201.w,D0              ;high-Byte get
                swap    D0
                move.b  $FFFF8203.w,D0              ;mid-Byte get
                lsl.w   #8,D0
                tst.b   tt_flag(A4)                 ; a TT?
                bne.s   get_scradr0                 ;Yes!=>
                tst.b   ste_flag(A4)                ; a holding?
                beq.s   get_scradr1                 ;No!=>
get_scradr0:    move.b  $FFFF820D.w,D0              ;low-Byte pick up
get_scradr1:    rts
                ENDPART

********************************************************************************

                PART 'vsync_test'                   ;Wait outlose gap, Overscan test
vsync_test:     move.b  $FFFF8203.w,D0              ;Initial address of the video image
                moveq   #$7D,D1
                add.b   D0,D1                       ;End address of the normal screen
vsync_test1:    cmp.b   $FFFF8207.w,D1              ;Wait until the video counter on
                bne.s   vsync_test1                 ;End has arrived
vsync_test2:    cmp.b   $FFFF8207.w,D1              ;and wait for him until he is
                beq.s   vsync_test2                 ;again
                cmp.b   $FFFF8207.w,D0              ;= Start address of the video image?
                rts
                ENDPART

********************************************************************************
* Switches through trap # 3 into the supervisor mode                           *
********************************************************************************
                PART '_trap3'
                DC.L 'XBRA'
                DC.L xbra_id
old_trap3:      DS.L 1
_trap3:         bset    #5,(SP)                     ;Supervisor-Mode an
                rte
                ENDPART

********************************************************************************
* Internal bus error vector                                                    *
********************************************************************************
                PART 'intern_bus'
                DC.L 'XBRA'
                DC.L xbra_id
old_intern_bus: DS.L 1
intern_bus:     ori     #$0700,SR                   ;All IRQS!
                lea     varbase(PC),A4
                movea.l default_stk(A4),SP          ;Restore stack
                sf      assm_flag(A4)               ;Reset all flags
                sf      illegal_flg(A4)
                clr.l   prg_base(A4)
                sf      do_resident(A4)
                sf      do_resident(A4)
                sf      autodo_flag(A4)
                sf      fast_exit(A4)
                sf      testwrd(A4)                 ;Issue necessarily on the screen
                tst.w   spalte(A4)
                beq.s   int_bus
                jsr     @crout(A4)                  ;CR only if necessary
int_bus:        bsr     breakclr                    ;Remove breakpoints
ill_mem:        pea     int_bus_txt(PC)
                jsr     @print_line(A4)
                jsr     @crout(A4)
                jmp     all_normal                  ;Cancel command

                SWITCH language
                CASE 0
int_bus_txt:    DC.B 'Illegaler Speicherbereich!',0
                CASE 1
int_bus_txt:    DC.B 'Illegal memory!',0
                ENDS

                EVEN
                ENDPART

********************************************************************************
* Printer driver                                                               *
********************************************************************************
********************************************************************************
* CR + LF to the printer                                                       *
********************************************************************************
                PART 'prncr'
prncr:          moveq   #13,D0
                bsr.s   prnout
                moveq   #10,D0
                ENDPART

********************************************************************************
* Sign in D0 to the printer                                                    *
********************************************************************************
                PART 'prnout'
prnout:         movem.l D2/A6,-(SP)
                btst    #0,$FFFFFA01.w              ;Busy-Flag of the printer
                bne.s   prnout4                     ;Printer occupied
                andi.w  #$FF,D0
                lea     $FFFF8800.w,A6
                st      $043E.w
                moveq   #7,D2
                move.b  D2,(A6)                     ;Select Data Directors
                lsl.w   #8,D2
                move.b  2(A6),D2                    ;Get content
                bset    #7,D2                       ;Port B on issue
                movep.w D2,0(A6)                    ;Write data direction registers
                ori.w   #$0F00,D0
                movep.w D0,0(A6)                    ;Spend characters on port b
                moveq   #$0E,D2
                move.b  D2,(A6)                     ;Select port A
                lsl.w   #8,D2
                move.b  (A6),D2                     ;Read tab
                bclr    #5,D2                       ;Strobe-Bit Clear
                movep.w D2,0(A6)                    ;Strobe low
                moveq   #19,D0
prnout1:        dbra    D0,prnout1
                movep.w D2,0(A6)                    ;Strobe low
                moveq   #19,D0
prnout2:        dbra    D0,prnout2
                bset    #5,D2                       ;Strobe-Bit set
                movep.w D2,0(A6)                    ;Strobe high
                clr.w   $043E.w
prnout3:        btst    #0,$FFFFFA01.w              ;Busy-Flag Test the printer
                bne.s   prnout3                     ;Wait until printer ready
                moveq   #-1,D0                      ;everything OK
                movem.l (SP)+,D2/A6
                rts
prnout4:        moveq   #0,D0                       ;Error (Printer Busy)
                movem.l (SP)+,D2/A6
                rts
                ENDPART

********************************************************************************
* keyboard driver                                                              *
********************************************************************************
********************************************************************************
* Sign in D0 to the keyboard processor                                         *
********************************************************************************
                PART 'ikbd_send'
ikbd_send:      movem.l D0-D1/A0,-(SP)
                move.w  #5000,D1
                lea     $FFFFFC00.w,A0
ikbd_send0:     move.b  (A0),D2                     ;As in the ROM ...
                btst    #1,D2
                dbne    D1,ikbd_send0               ;However with: Timeout
                move.w  #950,D0
ikbd_send1:     bsr.s   ikbd_send2                  ;still a little delay
                dbra    D0,ikbd_send1
                movem.l (SP)+,D0-D1/A0
                move.b  D0,$FFFFFC02.w              ;and from the byte ...
ikbd_send2:     rts
                ENDPART

********************************************************************************
* Delete keyboard buffer (necessary after switching the drivers)               *
********************************************************************************
                PART 'clr_keybuff'
clr_keybuff:    sf      akt_maust(A4)
                sf      kbd_repeat_on(A4)
                move.w  #-1,maus_flag(A4)           ;Reset flag again
                clr.b   maus_merk(A4)
                clr.b   maus_merk2(A4)
                move.l  $04BA.w,D0
                move.l  D0,maus_time2(A4)
                move.l  D0,maus_time(A4)
                clr.b   kbd_r_key(A4)
                clr.b   kbd_r_verz(A4)              ;delete everything
                clr.b   kbd_r_cnt(A4)
                clr.l   iorec_IKBD+4(A4)            ;Clear keyboard buffer
                rts
                ENDPART

********************************************************************************
* Nude button code according to D0                                             *
********************************************************************************
                PART 'conin'
conin:          movem.l D1-D2/A0-A1,-(SP)
                move.l  tmacro_pointer(A4),D0       ;TMacro active?
                bne.s   conin7                      ;Yes! =>
conin0:         lea     iorec_IKBD(A4),A0
                moveq   #0,D0
                move    SR,-(SP)
                ori     #$0700,SR
                move.w  4(A0),D1
                cmp.w   6(A0),D1
                beq.s   conin2                      ;Cancel when no key pressed
                addq.b  #4,D1
                movea.l (A0),A1
                move.l  0(A1,D1.w),D0               ;Get keyboard code
                tst.l   tmacro_def_key(A4)
                beq.s   conin10                     ;no active TMacro-Definition
                btst    #4,kbshift(A4)              ;CAPS?
                bne.s   conin3                      ;then without transformation out
                cmp.l   tmacro_def_key(A4),D0       ;TMACRO definition key? (recursive)
                beq.s   conin30                     ;Then end (no key)
conin10:        lea     tmacro_tab(A4),A1
conin4:         move.l  (A1),D2
                addq.l  #2,D2                       ;-2
                beq.s   conin3                      ;End of the table, no TMacro
                move.l  (A1)+,D2
                cmp.l   D2,D0
                beq.s   conin6                      ;TMacro found
conin5:         move.l  (A1)+,D2
                addq.l  #1,D2                       ;TMacro overlook (until -1)
                bne.s   conin5
                bra.s   conin4                      ;next TMacro
conin3:         move.w  D1,4(A0)
conin2:         move    (SP)+,SR
conin20:        movem.l (SP)+,D1-D2/A0-A1
                tst.l   D0
                rts
conin30:        moveq   #0,D0                       ;no button
                bra.s   conin3                      ;end

conin6:         move.l  (A1),D0                     ;New key code
                move.w  D0,D2
                lsr.w   #8,D2
                and.w   #$7F,D2
                move.w  D2,tmacro_repeat(A4)        ;Number of repetitions
                and.w   #$80FF,D0
                move.l  A1,tmacro_pointer(A4)       ;Pointer brands
                bra.s   conin3                      ;end

conin7:         movea.l D0,A1
                subq.w  #1,tmacro_repeat(A4)        ;Number of repeats-1
                bpl.s   conin9                      ;Still???=>
                addq.l  #4,A1                       ;Pointer to the next button
                move.l  (A1),D0                     ;Get next key of the TMacro
                move.w  D0,D2
                lsr.w   #8,D2
                and.w   #$7F,D2
                move.w  D2,tmacro_repeat(A4)        ;Number of repetitions
                addq.l  #1,D0
                beq.s   conin8                      ;End of Tmacro
conin9:         move.l  (A1),D0                     ;Get key code again
                and.w   #$80FF,D0
                move.l  A1,tmacro_pointer(A4)       ;Remember new pointer
                bra.s   conin20                     ;that wars
conin8:         clr.l   tmacro_pointer(A4)          ;Macro is over
                bra     conin0                      ;Get next key
                ENDPART

********************************************************************************
* Keyboard-IRQ from Mfp                                                        *
********************************************************************************
                PART 'mfp_irq'
                DC.L 'XBRA'
                DC.L xbra_id
old_ikbd:       DS.L 1
mfp_irq:        movem.l D0-D3/A0-A5,-(SP)
                lea     varbase(PC),A4
k_mfp1:         lea     $FFFFFC04.w,A1
                bsr.s   k_dokbd                     ;MIDI-IRQ
                subq.l  #4,A1
                bsr.s   k_dokbd                     ;Keyboard-IRQ
                btst    #4,$FFFFFA01.w
                beq.s   k_mfp1                      ;Another IRQ?
                movem.l (SP)+,D0-D3/A0-A5
                bclr    #6,$FFFFFA11.w              ;Release IRQ again
                rte

k_dokbd:        movem.l D2/A1,-(SP)
                move.b  (A1),D2
                bpl.s   k_isys3                     ;IRQ-Request?
                btst    #0,D2                       ;Receiver-Buffer full?
                beq.s   k_isys2
                tst.w   midi_flag(A4)               ;MIDI-keyboard?
                beq.s   k_isys1                     ;Yes! =>
                cmpa.w  #$FC00,A1                   ;Keyboard?
                beq.s   k_isys1                     ;then normal
                move.b  2(A1),D0                    ;Get dummy sign
                bra.s   k_isys2                     ;and end
k_isys1:        bsr.s   k_avint
k_isys2:        andi.b  #$20,D2
                beq.s   k_isys3
                move.b  2(A1),D0
k_isys3:        movem.l (SP)+,D2/A1
                rts
                ENDPART

********************************************************************************
* Get signs of ACIA                                                            *
********************************************************************************
                PART 'k_avint'
k_avint:        move.b  2(A1),D0                    ;Pick sign

                tst.b   kbstate(A4)                 ;A package is in rolling
                bne.s   k_arpak

                and.w   #$FF,D0
                cmp.w   #$F6,D0
                blo     k_arkey                     ;Only ne 'key

                sub.w   #$F6,D0
                move.b  k_kbsta1(PC,D0.w),kbstate(A4)
                move.b  k_kbind1(PC,D0.w),kbindex(A4)

                addi.w  #$F6,D0
                cmpi.w  #$F8,D0
                blt.s   k_avin1
                cmpi.w  #$FB,D0
                bgt.s   k_avin1
                move.b  D0,maus_paket_2(A4)
k_avin1:        rts

k_kbsta1:       DC.B 1,2,3,3,3,3,4,5,6,6
k_kbind1:       DC.B 7,5,2,2,2,2,6,2,1,1

k_arpak:        cmpi.b  #6,kbstate(A4)
                bhs.s   k_arpk2                     ;Ignore joystick data
                lea     k_arjmt(PC),A2
                moveq   #0,D2
                move.b  kbstate(A4),D2
                subq.b  #1,D2
                mulu    #12,D2
                movea.l 0(A2,D2.w),A0
                adda.l  A4,A0
                movea.l 4(A2,D2.w),A1
                adda.l  A4,A1
                movea.l 8(A2,D2.w),A2               ;Address of the routine
                moveq   #0,D2
                move.b  kbindex(A4),D2
                suba.l  D2,A1
                move.b  D0,(A1)
                subq.b  #1,kbindex(A4)              ;Paket complete?
                bne.s   k_arpk1                     ;NO!
                jsr     (A2)                        ;Irq routine
k_arpk2:        clr.b   kbstate(A4)                 ;Release package flag again
k_arpk1:        rts

k_arjmt:        DC.L stat_paket,maus_paket_1,k_arpk1
                DC.L maus_paket_1,maus_paket_2,mausvek
                DC.L maus_paket_2,zeit_paket,mausvek
                DC.L zeit_paket,joydat0,k_arpk1
                DC.L joydat0,joydat2,k_arpk1
                ENDPART

********************************************************************************

                PART 'k_arkey'
k_arkey:        move.b  kbshift(A4),D1
                cmp.b   #$2A,D0
                bne.s   k_ark1
                bset    #1,D1                       ;Shift (links) pressed
                bra.s   k_ark10
k_ark1:         cmp.b   #$AA,D0
                bne.s   k_ark2
                bclr    #1,D1                       ;Shift (links) released
                bra.s   k_ark10
k_ark2:         cmp.b   #$36,D0
                bne.s   k_ark3
                bset    #0,D1                       ;Shift (rechts) pressed
                bra.s   k_ark10
k_ark3:         cmp.b   #$B6,D0
                bne.s   k_ark4
                bclr    #0,D1                       ;Shift (rechts) released
                bra.s   k_ark10
k_ark4:         cmp.b   #$1D,D0
                bne.s   k_ark5
                bset    #2,D1                       ;Control pressed
                bra.s   k_ark10
k_ark5:         cmp.b   #$9D,D0
                bne.s   k_ark6
                bclr    #2,D1                       ;Control released
                bra.s   k_ark10
k_ark6:         cmp.b   #$38,D0
                bne.s   k_ark7
                bset    #3,D1                       ;Alternate pressed
                bra.s   k_ark10
k_ark7:         cmp.b   #$B8,D0
                bne.s   k_ark8
                bclr    #3,D1                       ;Alternate released
                moveq   #0,D3
                move.b  kbalt(A4),D3                ;Alternate-keypad button?
                beq.s   k_ark10
                move.b  D1,kbshift(A4)              ;New KBSHIFT status
                move.w  D3,D0                       ;as current ASCII-Code
                moveq   #0,D1                       ;NO Scancode!
                moveq   #0,D2                       ;KBShift also delete
                bra     k_insert                    ;and in the keyboard buffer
k_ark8:         cmp.b   #$3A,D0
                bne.s   k_ark11
                lea     clickdata(PC),A0
                bsr     do_sound                    ;CAPS LOCK- Click
                bchg    #4,D1                       ;bit Inverteren.
k_ark10:        move.b  D1,kbshift(A4)              ;New KBSHIFT status
k_arkxx:        rts
k_ark11:        tst.b   D0
                bmi.s   k_ark13                     ;Button released
                tst.b   kbd_r_key(A4)
                bne.s   k_ark12                     ;A key is already repeated
                move.b  D0,kbd_r_key(A4)
                move.b  kbd_r_init(A4),kbd_r_verz(A4)
                move.b  kbd_r_rate(A4),kbd_r_cnt(A4)
                cmp.b   #$53,D0                     ;DELETE pressed?
                bne.s   k_arkin                     ;No?=> Way
                cmpi.b  #%1100,kbshift(A4)          ;CTRL+ALT = Warmstart the Debugger
                beq.s   k_aaa10
                cmpi.b  #%1101,kbshift(A4)          ;CTRL+ALT+RSHFT = Cold-Boot
                bne.s   k_arkin
                bra     kill_all
k_aaa10:        movea.l 4.w,A0
                jmp     (A0)                        ;And off the post

k_ark12:        clr.b   kbd_r_verz(A4)              ;Reset to default
                clr.b   kbd_r_cnt(A4)
                sf      kbd_repeat_on(A4)
                bra.s   k_arkin

k_ark13:        moveq   #0,D1
                move.b  D1,kbd_r_key(A4)
                move.b  D1,kbd_r_verz(A4)           ;delete everything
                move.b  D1,kbd_r_cnt(A4)
                tst.b   kbd_repeat_on(A4)
                beq.s   k_arkxx                     ;Delete only if no auto repeat
                sf      kbd_repeat_on(A4)
                bra     clr_keybuff

k_arkin:        btst    #0,conterm+1(A4)
                beq.s   k_arkii
                lea     clickdata(PC),A0
                bsr     do_sound                    ;CAPS LOCK- Click
k_arkii:        moveq   #0,D1
                move.b  D0,D1                       ;Scancode brands
                movea.l std_keytab(A4),A0           ;Normal table
                and.w   #$7F,D0                     ;Delete bit for released
                moveq   #$0C,D2                     ;Ctrl or old pressed?
                and.b   kbshift(A4),D2
                bne.s   k_ark16                     ;then do not change
                btst    #4,kbshift(A4)              ;capsLockAktiv?
                beq.s   k_ark14
                movea.l caps_keytab(A4),A0          ;capsTastaturtabelle
k_ark14:        btst    #0,kbshift(A4)
                bne.s   k_ark15                     ;shiftLinks?
                btst    #1,kbshift(A4)
                beq.s   k_ark16                     ;Shift right?
k_ark15:        movea.l shift_keytab(A4),A0         ;Softcast

k_ark16:        move.b  0(A0,D0.w),D0               ;Get characters from the table
;ASCII-Code in D0 / Scancode in D1

                btst    #3,kbshift(A4)              ;Alternate pressed?
                beq.s   k_arkbu                     ;nö
                lea     keyboard_tab-2(PC),A0
k_ark17:        addq.l  #2,A0
                move.b  (A0)+,D2
                beq.s   k_ark19                     ;End of the table => out
                cmp.b   D2,D1                       ;ScanCode found
                bne.s   k_ark17                     ;Next
                moveq   #3,D2
                and.b   kbshift(A4),D2
                sne     D2                          ;When Shift key pressed, then
                ext.w   D2
                move.b  1(A0,D2.w),D0               ;Get new ASCII code
                bra.s   k_arkbu

                SWITCH language
                CASE 0
keyboard_tab:   DC.B $1A,'\','@'
                DC.B $27,'{','['
                DC.B $28,'}',']'
                DC.B 0

                CASE 1
keyboard_tab:   DC.B $1A,'\','@'
                DC.B $27,'{','['
                DC.B $28,'}',']'
                DC.B 0

                CASE 2
keyboard_tab:   DC.B $1A,'{','['
                DC.B $1B,'}',']'
                DC.B $28,0,'\'
                DC.B $2B,'~','@'
                DC.B 0
                ENDS

                EVEN

k_ark19:        cmp.b   #$62,D1
                bne.s   k_arkbu                     ;Help
                moveq   #3,D2
                and.b   kbshift(A4),D2              ;SHIFT?
                beq.s   k_ark1y                     ;=>
                move.b  _dumpflg(A4),D0
                subq.b  #1,D0
                beq.s   k_ark1y
                sf      _dumpflg(A4)                ;Hardcopy trigger
k_ark1y:        andi.b  #$10,kbshift(A4)
                rts
k_arkbu:        and.w   #$FF,D1                     ;Only interest the lower 8 bits
                moveq   #0,D3
                moveq   #0,D2
                move.b  kbshift(A4),D2
                bclr    #4,D2                       ;CAPS/LOCK path
                btst    #3,D2                       ;Alternate pressed?
                beq.s   k_arkbt                     ;No!
                cmp.b   #103,D1                     ;Buttons 0-9 pressed on the keypad?
                blo.s   k_arkbt
                cmp.b   #112,D1
                bhi.s   k_arkbt
                subi.b  #'0',D0
                move.b  kbalt(A4),D3                ;Get old alternate value
                mulu    #10,D3                      ;Tens position to the left
                add.b   D0,D3                       ;new value
                cmp.w   #256,D3                     ;Overflow?
                blo.s   k_arkbq
                moveq   #0,D3                       ;If so, then delete
k_arkbq:        move.b  D3,kbalt(A4)
                bra.s   k_arkex                     ;cancellation
k_arkbt:        bclr    #1,D2                       ;Only forward a Shift key
                beq.s   k_arkbv
                bset    #0,D2
k_arkbv:        cmp.b   #$3B,D1                     ;small F1
                blo.s   k_arkbw
                cmp.b   #$44,D1                     ;Greater F10
                bhi.s   k_arkbw                     ;then abort
                subi.b  #$3B,D1
                move.w  D1,D0                       ;In the ASCII code
                moveq   #0,D1                       ;Delete Scan Code
                ori.b   #$80,D2                     ;F-key flag in the KBShift
                btst    #0,D2                       ;shift?
                beq.s   k_insert                    ;No!
                addi.w  #10,D0                      ;for F-buttons with SHIFT + 10
                andi.b  #$FE,D2                     ;Shift guy.
                bra.s   k_insert
k_arkbw:        moveq   #$0C,D3
                and.b   D2,D3                       ;Alternate or Control?
                beq.s   k_insert                    ;Apparently not!
                cmp.b   #2,D1
                blo.s   k_arkbx                     ;'1'
                cmp.b   #13,D1
                bhi.s   k_arkbx                     ;until "'"
                subq.w  #2,D1
                move.w  D1,D0
                ori.w   #$8000,D0                   ;Flag for marker keys
                moveq   #0,D1                       ;Delete ScanCode
                bra.s   k_insert
k_arkbx:        cmp.b   #41,D1
                bne.s   k_insert                    ;'#'
                move.w  #$800C,D0                   ;S.O., only all at once
                moveq   #0,D1                       ;Delete ScanCode
k_insert:       cmp.b   #$72,D1                     ;Enter?
                bne.s   k_arkby
                moveq   #$1C,D1                     ;Take scanc code for return
k_arkby:        asl.w   #8,D2                       ;kbshiftInDieBits815
                or.w    D1,D2                       ;scancodeRein
                swap    D2                          ;off to the upper word
                or.w    D0,D2                       ;Mask ascii code
                move.l  D2,D0
                bsr.s   into_kbd_buff
                beq.s   k_arkex                     ;Sign was not accepted
                clr.b   kbalt(A4)                   ;Delete Alternate Button
k_arkex:        rts
                ENDPART

********************************************************************************
* Add key code in D0 to the keyboard buffer (z = 1 => no place)                *
********************************************************************************
                PART 'into_kbd_buff'
into_kbd_buff:  movem.l D0-D1/A0-A1,-(SP)
                tst.b   tmacro_def_flag(A4)         ;Button after Control-ESC?
                bne     into_kbd_buff50             ;yes! =>
                lea     iorec_IKBD(A4),A0
                move.w  6(A0),D1                    ;Tail-Index
                addq.b  #4,D1                       ;plus 4 (Size of an entry)
                cmp.w   4(A0),D1                    ;= Head-Index?
                beq     into_kbd_buff2              ;then stop
                movea.l (A0),A1                     ;Buffer
                cmp.l   #$0801001B,D0               ;Alt-ESC?
                beq.s   into_kbd_buff13             ;do not take over
                cmp.l   #$0401001B,D0               ;Control-ESC?
                beq.s   into_kbd_buff13             ;do not take over
                move.l  D0,0(A1,D1.w)               ;Add key code
                move.w  D1,6(A0)                    ;Replace Tail Index
into_kbd_buff13:tst.l   tmacro_def_key(A4)          ;TMacro-Definition active
                beq.s   into_kbd_buff5              ;No!=>

                cmp.l   #$0801001B,D0               ;Alt-ESC
                bne.s   into_kbd_buff12
                movea.l tmacro_def_adr(A4),A0
                moveq   #-1,D1                      ;TMacro-Definition to lock
                move.l  D1,(A0)
                lea     tmacro_tab(A4),A0
into_kbd_buff14:move.l  (A0)+,D1                    ;Search the table ends
                addq.l  #2,D1
                bne.s   into_kbd_buff14
                move.l  4(A0),D0
                addq.l  #1,D0                       ;Delete TMacro?
                beq.s   into_kbd_buff16             ;yes!=>
into_kbd_buff15:move.l  (A0),-4(A0)                 ;Table end (-2) over
                move.l  (A0)+,D1
                addq.l  #1,D1
                bne.s   into_kbd_buff15
                moveq   #-2,D1
                move.l  D1,-(A0)                    ;Set new table ends
into_kbd_buff16:clr.l   tmacro_def_key(A4)
                clr.l   tmacro_def_adr(A4)
                bra     into_kbd_buff3              ;End and out (Puuuuuhhhhh!)

into_kbd_buff12:movea.l tmacro_def_adr(A4),A0
                cmp.l   -4(A0),D0                   ;Same button as before?
                bne.s   into_kbd_buff4              ;No!=>
                addq.b  #1,-2(A0)                   ;otherwise only increase the number
                bpl.s   into_kbd_buff3              ;Max. Number (128) not yet reached
                move.b  #$7F,-2(A0)                 ;Max number number and new entry
into_kbd_buff4: lea     tmacro_tab_end(A4),A1
                cmpa.l  A1,A0                       ;Table ends achieved?
                bhs.s   into_kbd_buff3              ;then no longer take key code
                move.l  D0,(A0)+                    ;Remember key code
                move.l  A0,tmacro_def_adr(A4)       ;Remember elevated pointer
                bra.s   into_kbd_buff3

into_kbd_buff5: cmp.l   #$0401001B,D0               ;escControl
                bne.s   into_kbd_buff3              ;No!=>
                st      tmacro_def_flag(A4)         ;Also wait to be assigned key
                bra.s   into_kbd_buff3              ;out here =>

into_kbd_buff50:tst.l   D0
                beq.s   into_kbd_buff3
                cmp.l   #$0401001B,D0               ;escControl
                beq.s   into_kbd_buff3              ;DAT is ignored
                cmp.l   #$0801001B,D0               ;Alt-Esc
                beq.s   into_kbd_buff3              ;dat is also ignored
                sf      tmacro_def_flag(A4)
                lea     tmacro_tab(A4),A0
                suba.l  A1,A1
into_kbd_buff8: move.l  (A0),D1
                addq.l  #2,D1                       ;End of the table found
                beq.s   into_kbd_buff7              ;yes!=>
                cmp.l   (A0)+,D0                    ;Is there already a macro with this key?
                bne.s   into_kbd_buff10             ;no!=>
                lea     -4(A0),A1                   ;Pointer brands
into_kbd_buff10:move.l  (A0)+,D1
                addq.l  #1,D1                       ;TMacro overlooked (up to -1)
                bne.s   into_kbd_buff10
                move.l  A1,D1
                beq.s   into_kbd_buff8              ;Next TMacro
into_kbd_buff9: move.l  (A0),(A1)+                  ;Remove Tmacro
                move.l  (A0)+,D1
                addq.l  #2,D1
                bne.s   into_kbd_buff9
                movea.l A1,A0
                bra.s   into_kbd_buff11
into_kbd_buff7: addq.l  #4,A0                       ;Pointer behind the table end (-2)
into_kbd_buff11:lea     tmacro_tab_end(A4),A1
                cmpa.l  A1,A0
                bhs.s   into_kbd_buff3              ;bufferVoll =>
                move.l  D0,tmacro_def_key(A4)       ;Start TMacro Definition
                move.l  D0,(A0)+                    ;Remember key code
                move.l  A0,tmacro_def_adr(A4)
into_kbd_buff3: moveq   #-1,D0                      ;Delete Z-Flag
into_kbd_buff2: movem.l (SP)+,D0-D1/A0-A1
                rts
                ENDPART

********************************************************************************
* the 200Hz-Timer-Routine                                                      *
********************************************************************************
                PART 'hz200_irq'
                DC.L 'XBRA'
                DC.L xbra_id
old_hz200:      DS.L 1
hz200_irq:      addq.l  #1,$04BA.w                  ;200Hz-Timer raise
                movem.l D0-A6,-(SP)
                lea     varbase(PC),A4
                rol.w   timer_c_bitmap(A4)
                bpl     hz200i4                     ;on 50Hz turn down
                tst.b   kbd_r_key(A4)               ;Button pressed?
                beq.s   hz200i2
                tst.b   kbd_r_verz(A4)              ;Delay expired?
                beq.s   hz200i1
                subq.b  #1,kbd_r_verz(A4)
                bne.s   hz200i2
hz200i1:        subq.b  #1,kbd_r_cnt(A4)
                bne.s   hz200i2
                st      kbd_repeat_on(A4)           ;It runs the auto repeat
                move.b  kbd_r_rate(A4),kbd_r_cnt(A4)
                move.b  kbd_r_key(A4),D0
                bsr     k_arkin                     ;tasteInDenTastaturbuffer
hz200i2:        move.b  akt_maust(A4),D2
                cmpi.w  #32,mausy(A4)               ;In the upper 2 lines
                blo.s   hz200i3                     ;Take button directly
                tst.b   no_dklick(A4)               ;No double click query?
                bne.s   hz200i3                     ;Exactly => Take button
                subq.b  #1,maus_merk2(A4)
                bgt.s   hz200i8
                clr.b   maus_merk2(A4)
                andi.b  #3,D2
                beq.s   hz200i6                     ;No button pressed
                tst.w   maus_flag(A4)
                beq.s   hz200i7                     ;Double-click report
                move.l  $04BA.w,D0
                move.l  D0,maus_time2(A4)
                sub.l   maus_time(A4),D0            ;As long as the button is pressed
                cmp.l   #40,D0
                bhs.s   hz200i3                     ;A long click!-> HZ200i5 (!!!)
                sf      maus_flag(A4)
                move.b  D2,maus_merk(A4)            ;Remember button

                bra.s   hz200i4
hz200i7:        lsl.b   #2,D2                       ;Double-click report
                move.b  #5,maus_merk2(A4)           ;timeout
                bra.s   hz200i3
;hz200i5:lsl.b   #4,d2                   ;Long click Report
;        bra.s   hz200i3
hz200i6:        move.l  $04BA.w,D0
                move.l  D0,maus_time(A4)
                sub.l   maus_time2(A4),D0           ;As long as the key was pressed
                move.b  maus_merk(A4),D2            ;Get old mouse button status
                cmp.l   #30,D0
                bhs.s   hz200i3                     ;It was a simple click (or no one)!
                sf      maus_flag+1(A4)
                bra.s   hz200i4                     ;Never speak => Double click risk
hz200i3:        move.b  D2,maustast(A4)             ;Set mouse button status
hz200i8:        move.w  #-1,maus_flag(A4)           ;Reset flag again
                clr.b   maus_merk(A4)
hz200i4:        movem.l (SP)+,D0-A6
                bclr    #5,$FFFFFA11.w              ;Release IRQ again
                rte
                ENDPART

********************************************************************************
* New Keyboard-Routine for the demolition                                      *
********************************************************************************
                PART 'spez_keyb'
                DC.L 'XBRA'
                DC.L xbra_id
old_spez_keyb:  DS.L 1
spez_keyb:      tst.b   prozessor+varbase           ;68000?
                bmi.s   spez_keyb1                  ;and!=>
                clr.w   -(SP)                       ;68010 or 68020 requires a word more
spez_keyb1:     pea     spez_keyb2(PC)
                move    SR,-(SP)                    ;for the RTE
                move.l  old_spez_keyb(PC),-(SP)
                rts

spez_keyb2:     movem.l D0-D1/A0-A1,-(SP)
                lea     varbase(PC),A1
                movea.l kbshift_adr(A1),A0
                move.b  (A0),D0                     ;Current KBSHIFT value
                moveq   #3,D1
                and.w   D1,D0                       ;Shift-Status
                lea     merk_shift(A1),A0
                cmp.b   (A0),D1                     ;Both shift keys pressed?
                bne.s   spez_keyb3                  ;No!=>
                cmp.b   D1,D0
                beq.s   spez_keyb3                  ;and now not anymore?
                st      ssc_flag(A1)                ;Cancel flag
                movea.l act_pd(A1),A0
                move.l  18(SP),D0                   ;Hot PC
                cmp.l   (A0),D0                     ;Before the current program?
                blo.s   spez_keyb4                  ;then not cancel yet
                cmp.l   rom_base(A1),D0             ;in the ROM
                bhs.s   spez_keyb4                  ;Then get away!
                movem.l (SP)+,D0-D1/A0-A1
                move.l  #230<<24,-(SP)
                pea     except1(PC)
                rts                                 ;Exceptionnummer for Shift-Break and demolition
spez_keyb3:     move.b  D0,(A0)
spez_keyb4:     movem.l (SP)+,D0-D1/A0-A1
                rte
                ENDPART

********************************************************************************
* IKBD Mouse-Handler                                                           *
********************************************************************************
                PART 'mausvek'
mausvek:        movem.l D0-A6,-(SP)
                move.b  (A0)+,D0
                move.b  D0,D1
                and.b   #$F8,D1                     ;mausheader ?
                cmp.b   #$F8,D1
                bne.s   mausve1
                and.w   #3,D0                       ;Insulate mouse buttons
                move.b  D0,akt_maust(A4)            ;and remember

                move.b  (A0)+,D0                    ;Delta X
                or.b    (A0),D0                     ;Delta Y
                beq.s   mausve1                     ;Mouse not moved

                lea     debugger_scr(A4),A1
                tst.b   scr_moni(A1)
                bne.s   mausve6                     ;Is color => no dynamic mouse
                move.l  $04BA.w,D0
                sub.l   mausf_time(A4),D0           ;Make a difference to the last call
                move.l  $04BA.w,mausf_time(A4)      ;200Hz-Timerwert brands
                subq.l  #3,D0                       ;> 5ms back?
                bhs.s   mausve6                     ;then end
                move.b  -(A0),D0
                add.b   D0,(A0)+                    ;otherwise double the coordinates
                move.b  (A0),D0
                add.b   D0,(A0)
mausve6:        move.w  mausx(A4),D0                ;Get old X position
                move.b  -(A0),D1
                ext.w   D1
                add.w   D1,D0                       ;New x-position
                tst.w   D0
                bpl.s   mausve2
                moveq   #0,D0
mausve2:        cmp.w   #631,D0
                blo.s   mausve3
                move.w  #630,D0
mausve3:        move.w  D0,mausx(A4)                ;X-position save

                move.w  mausy(A4),D0                ;other Y-Position
                move.b  1(A0),D1
                ext.w   D1
                add.w   D1,D0                       ;New Y position
                tst.w   D0
                bpl.s   mausve4
                moveq   #0,D0
mausve4:        cmp.w   #399,D0
                blo.s   mausve5
                move.w  #399,D0
mausve5:        move.w  D0,mausy(A4)                ;Y-Position save

                clr.b   mausmove(A4)                ;Mouse was moved
mausve1:        movem.l (SP)+,D0-A6
                rts
                ENDPART

********************************************************************************
* VBL-Routine                                                                  *
********************************************************************************
                PART 'my_vbl'
                DC.L 'XBRA'
                DC.L xbra_id
old_vbl:        DS.L 1
my_vbl:         addq.l  #1,vbl_count2+varbase
                bmi     vbl_exi
                movem.l D0-A6,-(SP)
                lea     varbase(PC),A4
                ori     #$0700,SR                   ;Please do not disturb!
                addq.l  #1,vbl_count1(A4)
                tst.w   $04A6.w                     ;Anyway present?
                beq.s   vblk08                      ;No!=>
                tst.w   $043E.w                     ;Drive locked?
                bne.s   vblk08                      ;Yes!=>
                move.w  $FFFF8604.w,D0              ;Fdc-status beings
                tst.b   D0
                bmi.s   vblk08                      ;Engine is still running =>
                move.b  #14,$FFFF8800.w
                moveq   #7,D1
                or.b    $FFFF8800.w,D1              ;deselect All Drives
                move.b  D1,$FFFF8802.w
vblk08:         bclr    #6,$FFFFFA0F.w              ;Release Ring Indicator
                bsr     kb_save                     ;Save screen possibly
                tst.b   curflag(A4)
                bpl.s   my_vbl2                     ;Cursor is out!
                subq.b  #1,curflag+1(A4)            ;Timer backwards
                bne.s   my_vbl5
                move.b  #$20,curflag+1(A4)          ;Return timer
                bchg    #6,curflag(A4)              ;Cursor state (1 = on / 0 = OFF)
                move.b  mausoff(A4),D0
                move.b  mausmove(A4),D1
                move.b  mausflg(A4),D2
                bsr     flash_cursor
                tst.b   D2
                beq.s   my_vbl5
                move.b  D0,mausoff(A4)
                move.b  D1,mausmove(A4)
my_vbl5:        tst.b   set_lock(A4)
                bne.s   my_vbl2                     ;Cursor in VBL set is prohibited
                btst    #1,maustast(A4)             ;Left mouse button (long)
                bne.s   my_vbl1
my_vbl2:        tas.b   mausmove(A4)                ;Was the mouse moved?
                bne.s   my_vbl4
                tst.b   mausoff(A4)
                bne.s   my_vbl4
                bsr     undraw_sprite               ;Undraw, if necessary
                move.w  mausx(A4),D0
                move.w  mausy(A4),D1
                bsr.s   draw_sprite                 ;Replace mouse
my_vbl4:        tst.b   kbd_r_key(A4)               ;Button pressed?
                beq.s   vbl_end
                bsr     undraw_sprite               ;Then turn off the mouse
vbl_end:        movem.l (SP)+,D0-A6
vbl_exi:        rte
my_vbl1:        move.w  upper_line(A4),D0
                lsl.w   #4,D0                       ;Time 16
                sub.w   mausy(A4),D0                ;Position cursor
                neg.w   D0
                bmi.s   my_vbl2
                bset    #6,curflag(A4)
                beq.s   my_vbl3
                bsr     flash_cursor
my_vbl3:        lsr.w   #4,D0
                move.w  D0,zeile(A4)
                move.w  mausx(A4),D0
                lsr.w   #3,D0
                move.w  D0,spalte(A4)
                move.w  #$FF20,curflag(A4)
                bsr     flash_cursor                ;Cursor again
                bra.s   my_vbl2

draw_sprite:    lea     mausbuffer(A4),A2
                move.w  sprite_no(A4),D2
                lsl.w   #7,D2                       ;108 (sprite length)
                lea     debugger_scr(A4),A0
                tst.b   scr_moni(A0)                ;Colour?
                lea     sprite(PC),A0
                adda.w  D2,A0
                bne.s   draw_sprite2                ;Is color =>
                move.w  D1,D2
                lsl.w   #2,D2
                add.w   D2,D1                       ;MAL 80
                lsl.w   #4,D1
                moveq   #$0F,D2                     ;Division
                and.w   D0,D2                       ;X-coordinate
                lsr.w   #4,D0                       ;X by 16 parts
                add.w   D0,D0
                lea     debugger_scr(A4),A1
                movea.l scr_adr(A1),A1              ;Screen address
                adda.w  D1,A1                       ;+ Y-Offset
                adda.w  D0,A1                       ;+ X-Offset
                move.l  A1,(A2)+
                moveq   #15,D1                      ;Number of lines (minus 1)
draw_sprite1:   move.l  (A0)+,D3                    ;Mask
                lsr.l   D2,D3                       ;push in the right position
                not.l   D3
                move.l  (A1),(A2)+                  ;Save background
                and.l   D3,(A1)                     ;Make a mask
                move.l  (A0)+,D3                    ;Get fuel data
                lsr.l   D2,D3                       ;push in the right position
                or.l    D3,(A1)                     ;Retouch data
                lea     80(A1),A1                   ;Next line
                dbra    D1,draw_sprite1
                clr.b   mausflg(A4)                 ;"Mouse and" flag
                rts
draw_sprite2:   lsr.w   #1,D1                       ;Color
                move.w  D1,D2
                lsl.w   #2,D2
                add.w   D2,D1                       ;(Y/2)*160 = Y-Offset
                lsl.w   #5,D1
                moveq   #$0F,D2                     ;Division
                and.w   D0,D2                       ;X-coordinate
                lsr.w   #4,D0                       ;X through 32 parts
                lsl.w   #2,D0
                lea     debugger_scr(A4),A1
                movea.l scr_adr(A1),A1              ;Screen address
                adda.w  D1,A1                       ;+ Y-Offset
                adda.w  D0,A1                       ;+ X-Offset
                move.l  A1,(A2)+
                moveq   #7,D1                       ;Number of lines (minus 1)
draw_sprite3:   tst.w   D1
                bne.s   draw_sprite4
                addq.l  #8,A0                       ;Still a line
draw_sprite4:   move.l  (A0)+,D3                    ;Mask
                lsr.l   D2,D3                       ;push in the right position
                not.l   D3
                move.l  (A1),(A2)+                  ;Save background
                move.l  4(A1),(A2)+
                and.w   D3,4(A1)
                and.w   D3,6(A1)
                swap    D3                          ;Make a mask
                and.w   D3,(A1)
                and.w   D3,2(A1)
                move.l  (A0)+,D3                    ;Get fuel data
                lsr.l   D2,D3                       ;push in the right position
                or.w    D3,4(A1)
                or.w    D3,6(A1)
                swap    D3                          ;Retouch data
                or.w    D3,(A1)
                or.w    D3,2(A1)
                addq.l  #8,A0                       ;a line over
                lea     160(A1),A1                  ;Next line

                dbra    D1,draw_sprite3
                clr.b   mausflg(A4)                 ;"Mouse and" flag
                rts

undraw_sprite:  tas.b   mausflg(A4)                 ;Was the mouse on?
                bne.s   undraw_sprite2
                movem.l D1/A0-A2,-(SP)
                lea     mausbuffer(A4),A0
                movea.l (A0)+,A1                    ;Screen position of the buffer
                lea     debugger_scr(A4),A2
                tst.b   scr_moni(A2)
                bne.s   undraw_sprite3              ;is color
                moveq   #15,D1                      ;Number of lines (minus 1)
undraw_sprite1: move.l  (A0)+,(A1)                  ;Return Buffer
                lea     80(A1),A1
                dbra    D1,undraw_sprite1
                movem.l (SP)+,D1/A0-A2
undraw_sprite2: rts
undraw_sprite3: moveq   #7,D1                       ;Number of lines (minus 1)
undraw_sprite4: move.l  (A0)+,(A1)+                 ;Return Buffer
                move.l  (A0)+,(A1)
                lea     156(A1),A1
                dbra    D1,undraw_sprite4
                movem.l (SP)+,D1/A0-A2
                rts

sprite:         DC.L %11000000000000000000000000000000 ;The normal mouse pointer
                DC.L %0
                DC.L %11100000000000000000000000000000
                DC.L %1000000000000000000000000000000
                DC.L %11110000000000000000000000000000
                DC.L %1100000000000000000000000000000
                DC.L %11111000000000000000000000000000
                DC.L %1110000000000000000000000000000
                DC.L %11111100000000000000000000000000
                DC.L %1111000000000000000000000000000
                DC.L %11111110000000000000000000000000
                DC.L %1111100000000000000000000000000
                DC.L %11111111000000000000000000000000
                DC.L %1111110000000000000000000000000
                DC.L %11111111100000000000000000000000
                DC.L %1111111000000000000000000000000
                DC.L %11111111110000000000000000000000
                DC.L %1111111100000000000000000000000
                DC.L %11111111111000000000000000000000
                DC.L %1111100000000000000000000000000
                DC.L %11111110000000000000000000000000
                DC.L %1101100000000000000000000000000
                DC.L %11101111000000000000000000000000
                DC.L %1000110000000000000000000000000
                DC.L %11001111000000000000000000000000
                DC.L %110000000000000000000000000
                DC.L %10000111100000000000000000000000
                DC.L %11000000000000000000000000
                DC.L %111100000000000000000000000
                DC.L %11000000000000000000000000
                DC.L %11100000000000000000000000
                DC.L %0
                DC.W 65535,0,65535,0,65535,0,65529,0,65535,0,65529,0,65535,0,65535,0
                DC.W 65535,0,65535,0,65535,0,65535,0,65535,0,65535,0,65535,0,65535,0
                DC.W 65535,0,65535,0,65535,0,63519,0,65535,0,64287,0,65535,0,64287,0
                DC.W 65535,0,64287,0,65535,0,64287,0,32767,0,30751,0,16382,0,16382,0
                ENDPART

********************************************************************************
* Turn on / off                                                                *
********************************************************************************
                PART 'clr_maus'
clr_maus:       st      mausoff(A4)                 ;Mouse must be turned off
                bra     undraw_sprite               ;and away from screen
                ENDPART

********************************************************************************

                PART 'set_maus'
set_maus:       tst.b   kbd_r_key(A4)               ;Button pressed?
                bne.s   set_ma1                     ;=> Mouse do not display again immediately
                clr.b   mausmove(A4)                ;Mouse was moved => is displayed
set_ma1:        clr.b   mausoff(A4)                 ;Mouse is allowed again
                rts
                ENDPART

********************************************************************************

                PART 'graf_mouse'
graf_mouse:     bsr.s   clr_maus
                move.w  D0,sprite_no(A4)            ;Set new mouse pointer
                bra.s   set_maus
                ENDPART

********************************************************************************
* Mouse query (something triggered?)                                           *
********************************************************************************
                PART 'mauschk'
mauschk:        btst    #1,maustast(A4)             ;Left button pressed?
                beq.s   mausch1                     ;No!
                clr.b   mausprell(A4)               ;note that it was pressed
                bra     mausch4                     ;end
mausch1:        tas.b   mausprell(A4)               ;Was the key released?
                bne     mausch4                     ;NÖ => End
                moveq   #0,D0
                moveq   #0,D1
                move.w  mausx(A4),D0
                move.w  mausy(A4),D1
                lsr.w   #3,D0
                lsr.w   #4,D1                       ;Convert to character coordinates
                cmp.w   #1,D1                       ;Not in the menu lines
                bhi.s   mausch2
                lsr.w   #3,D0                       ;X Div 8 (0 to 9)
                tst.w   D1
                beq.s   mauscc1
                add.w   #10,D0                      ;2nd row starts at 10 (up to 19)
mauscc1:        moveq   #31,D1
                bset    D1,D0                       ;Flag for F button
                move    #$FF,CCR                    ;Action
                rts
mausche:        move    #0,CCR                      ;Nothing was done
                rts
sr_tab:         DC.B 13,10,9,8,4,3,2,1,0
                EVEN
mausch2:        tst.w   D0
                beq     mausch5
                move.l  reg_pos(A4),D7
                cmp.l   trace_pos(A4),D7
                bne     mauschf                     ;They have to be the same
                move.w  D0,D7
                cmp.w   #2,D1                       ;Not in the Special Register
                bhi.s   mausch3
                pea     mausc22(PC)                 ;Return address in A7 output
                lea     _pc(A4),A0
                moveq   #5,D0                       ;X-Stroke cord (Y-cord = D1)
                cmp.w   D0,D7
                blo.s   mausche
                cmp.w   #13,D7                      ;Change PC
                blo     form_inp
                lea     _usp(A4),A0
                moveq   #18,D0
                cmp.w   D0,D7
                blo.s   mausche
                cmp.w   #26,D7                      ;Change usp
                blo     form_inp
                lea     _ssp(A4),A0
                moveq   #31,D0
                cmp.w   D0,D7
                blo.s   mausche
                cmp.w   #39,D7                      ;Change SSP
                blo     form_inp
                cmp.w   #41,D7
                blo.s   mausche
                cmp.w   #49,D7
                bhs.s   mausche                     ;Not the SR
                addq.l  #4,SP
                sub.w   #41,D7
                move.b  sr_tab(PC,D7.w),D1
                move.w  _sr(A4),D0
                bchg    D1,D0                       ;Invert flag (Do not change trace flag)
                move.w  D0,_sr(A4)
mausc22:        bsr     set_reg
                bsr     rgout
                move    #0,CCR
                rts
mausch3:        cmp.w   #4,D1                       ;Not in the normal registers
                bhi.s   mauschf
                move.w  D1,D3
                subq.w  #3,D3
                lsl.w   #5,D3                       ;Time 32 (Offset Data / Address Register)
                subq.w  #8,D0
                bmi.s   mauschf
                divu    #9,D0
                move.w  D0,D4
                swap    D0
                cmp.w   #8,D0
                beq.s   mauschf
                move.w  D4,D0
                mulu    #9,D0
                addq.w  #8,D0                       ;Calculate X coordinate
                lsl.w   #2,D4
                add.w   D4,D3
                lea     regs(A4),A0
                adda.w  D3,A0                       ;Pointer to appropriate registers
                bsr     form_inp
                bra.s   mausc22

mauschf:        tst.b   mausscroll_on(A4)
                bne.s   mauschf1                    ;it is already scrolled
                btst    #0,maustast(A4)
                beq.s   mauschg                     ;Right mouse button is necessary!
mauschf1:       move.w  mausy(A4),D0
                cmp.w   #8,D0
                bhi.s   mauschh
                move.l  #$05480000,D0               ;Shift+Control+Cursor up
                bra.s   mauschi
mauschh:        cmp.w   #391,D0
                blo.s   mauschg
                move.l  #$05500000,D0               ;Shift+Control+Cursor down
mauschi:        st      mausscroll_on(A4)
                sf      mausscroll_flg1(A4)         ;"Mouse scrolling was on"
                btst    #0,maustast(A4)
                beq.s   mauschii                    ;Right mousekey?No!=>
                st      mausscroll_flg1(A4)         ;Mouse scrolling was on
mauschii:       move    #$FF,CCR                    ;Action
                rts
mauschg:        sf      mausscroll_on(A4)
                move    #0,CCR
                rts

mausch4:        btst    #0,maustast(A4)             ;Right key pressed?
                beq.s   mausc41                     ;No!
                sf      mausprell2(A4)              ;note that it was pressed
                bra.s   mauschf                     ;end
mausc41:        tas.b   mausprell2(A4)              ;Was the key released?
                bne     mausc90                     ;NÖ => Double click?
                bclr    #7,mausscroll_flg1(A4)      ;Was mouse scrolling?
                bne.s   mauschf                     ;then out
                move.w  mausx(A4),D0
                move.w  mausy(A4),D1
                lsr.w   #3,D0
                lsr.w   #4,D1                       ;Convert to character coordinates
                cmp.w   #1,D1                       ;Not in the menu lines
                bls.s   mauschf
                move.w  D0,D3                       ;Remember X coordinate
                lea     screen(A4),A0
                move.w  D1,D2
                lsl.w   #2,D2                       ;MAL 80
                add.w   D2,D1
                lsl.w   #4,D1
                adda.w  D0,A0

                adda.w  D1,A0
                lea     mauttab(PC),A1
                movea.l A1,A2
mausc42:        move.b  (A1)+,D0
                beq.s   mausc43                     ;Table ends & no separator caught
                cmp.b   (A0),D0
                bne.s   mausc42                     ;Still uneven!
                bra     mauschf
mausc43:        movea.l A2,A1
                subq.w  #1,D3
                bmi.s   mausc45                     ;Left edge reaches (String from A0)
                subq.l  #1,A0
mausc44:        move.b  (A1)+,D0
                beq.s   mausc43                     ;Table ends & no separator caught
                cmp.b   (A0),D0
                bne.s   mausc44                     ;Still uneven!
                addq.l  #1,A0
mausc45:        movea.l A0,A6                       ;Link border brands
mausc46:        movea.l A2,A1
                addq.w  #1,D3
                cmp.w   #79,D3
                bhs.s   mausc48                     ;Right edge reaches (String from A6)
                addq.l  #1,A0
mausc47:        move.b  (A1)+,D0
                beq.s   mausc46                     ;Table ends & no separator caught
                cmp.b   (A0),D0
                bne.s   mausc47                     ;Still uneven!
                subq.l  #1,A0
mausc48:        lea     spaced2(A4),A1
                movea.l A1,A2
                move.b  #' ',(A1)+                  ;Space in front of the string
mausc49:        move.b  (A6)+,(A1)+                 ;String the court
                cmpa.l  A0,A6
                bls.s   mausc49
                clr.b   (A1)+
                jsr     @cursor_off(A4)             ;Switch off the cursor
mausc4b:        tst.b   (A2)
                beq.s   mausc4c
                tst.b   ins_mode(A4)
                beq.s   mausc4a
                bsr     c_ins                       ;Insert sign
mausc4a:        move.b  (A2)+,D0
                jsr     @chrout(A4)
                bra.s   mausc4b
mausc4c:        bsr     cursor_on
                bra     mauschf                     ;end

mauttab:        DC.B '!&`*\+{}[]-~|/ ^=,;:Ø<>#()?',0
                EVEN

mausch5:        lea     reg_pos(A4),A6
                movea.l #trace_buffend,A5
                adda.l  A4,A5
                movea.l #trace_buff,A3
                adda.l  A4,A3
                move.l  (A6),D2
                move.l  D2,D3
                cmp.w   #2,D1                       ;fuller
                beq.s   mausch7
                cmp.w   #3,D1                       ;Arrow above
                beq.s   mausch6
                cmp.w   #4,D1                       ;Arrow below
                bne.s   mausch9                     ;Then nothing wrong
                cmp.l   trace_pos(A4),D2
                beq.s   mausch6b                    ;End?=> Cancel
                addi.l  #78,(A6)
                cmpa.l  (A6),A5
                bhi.s   mausch8
                move.l  A3,(A6)
                bra.s   mausch8
mausch6:        subi.l  #78,D2
                cmp.l   A3,D2
                bhs.s   mausch6a
                move.l  A5,(A6)
                move.l  A5,D2
                bra.s   mausch6
mausch6a:       move.l  D2,(A6)
                cmp.l   trace_pos(A4),D2
                bne.s   mausch8
                move.l  D3,(A6)
mausch6b:       bsr     c_bell                      ;Border reached
                bra.s   mausch9
mausch7:        move.l  trace_pos(A4),(A6)
mausch8:        jsr     @cursor_off(A4)
                bsr     rgout
                bsr     cursor_on
mausch9:        move    #0,CCR
                rts

;Doppelklick
mausc90:        btst    #3,maustast(A4)             ;Double click left?
                beq     mauschf                     ;No!=> End
                move.w  mausx(A4),D0
                move.w  mausy(A4),D1
                lsr.w   #3,D0
                lsr.w   #4,D1                       ;Convert to character coordinates
                cmp.w   #1,D1                       ;Not in the menu lines
                bls     mauschf
                move.w  D0,D3                       ;Remember X coordinate
                lea     screen(A4),A0
                move.w  D1,D2
                lsl.w   #2,D2                       ;MAL 80
                add.w   D2,D1
                lsl.w   #4,D1
                adda.w  D0,A0
                adda.w  D1,A0
                lea     mauttab(PC),A1
                movea.l A1,A2
mausc92:        move.b  (A1)+,D0
                beq.s   mausc93                     ;Table ends & no separator caught
                cmp.b   (A0),D0
                bne.s   mausc92                     ;Still uneven!
                bra     mauschf
mausc93:        movea.l A2,A1
                subq.w  #1,D3
                bmi.s   mausc95                     ;Left edge reaches (String from A0)
                subq.l  #1,A0
mausc94:        move.b  (A1)+,D0
                beq.s   mausc93                     ;Table ends & no separator caught
                cmp.b   (A0),D0
                bne.s   mausc94                     ;Still uneven!
                addq.l  #1,A0
mausc95:        movea.l A0,A6                       ;Link border brands
mausc96:        movea.l A2,A1
                addq.w  #1,D3
                cmp.w   #79,D3
                bhs.s   mausc9a                     ;Right edge reaches (String from A6)
                addq.l  #1,A0
mausc97:        move.b  (A1)+,D0
                beq.s   mausc96                     ;Table ends & no separator caught
                cmp.b   (A0),D0
                bne.s   mausc97                     ;Still uneven!
                subq.l  #1,A0
mausc9a:        lea     spaced2(A4),A1
                movea.l A1,A2
mausc9b:        move.b  (A6)+,(A1)+                 ;String the court
                cmpa.l  A0,A6
                bls.s   mausc9b
                clr.b   (A1)+                       ;Terminate string with zero
                st      err_flag(A4)
                move.l  SP,err_stk(A4)              ;If an error occurs
                movea.l A2,A0
                moveq   #0,D0
                move.b  (A0)+,D0
                jsr     get_zahl
                jsr     @cursor_off(A4)
                bsr.s   do_dopp                     ;Double-click
                bsr     cursor_on
                bra     mauschf
mausc9z:        sf      err_flag(A4)
                move    #0,CCR
                rts                                 ;end
                ENDPART

********************************************************************************
* "Double click" version (D1 initial address)                                  *
********************************************************************************
                PART 'do_dopp'
do_dopp:        movea.l D1,A2
                sf      err_flag(A4)
                tst.l   sym_size(A4)
                sne     list_flg(A4)                ;Symbolically disassembled (if symbols DA)
                suba.l  A3,A3
                move.w  def_lines(A4),D2            ;Default line number
                subq.w  #1,D2
                move.l  basep(A4),D0                ;Program loaded with Le?
                beq.s   mausc9w                     ;Dump or disa
                movea.l D0,A0
                cmpa.l  8(A0),A2                    ;<TextSegment
                blo.s   mausc9v                     ;dump
                cmpa.l  16(A0),A2                   ;>DataSegment
                bhs.s   mausc9v                     ;dump
mausc9u:        bsr     cmd_disass2                 ;Spend disassemble
                bra.s   mausc9x
mausc9w:        tst.w   format_flag(A4)
                bne.s   mausc9u                     ;disassemble
mausc9v:        moveq   #0,D3                       ;Byte-Dump
                bsr     cmd_dump2                   ;Spend dump
mausc9x:        move.l  default_adr(A4),D1
                jsr     @anf_adr(A4)
                clr.b   maustast(A4)
                rts
                ENDPART

********************************************************************************
* Form-Input                                                                   *
* A0 - Pointer to memory location where the input is stored (Long)             *
* D0 - X-Coordinate                                                            *
* D1 - Y-Coordinate                                                            *
********************************************************************************
                PART 'form_inp'
form_inp:       addq.b  #1,set_lock(A4)             ;Prevent cursoring in the VBL
                jsr     @cursor_off(A4)
                bsr     clr_maus
                move.l  zeile(A4),-(SP)
                move.l  A0,-(SP)
                move.w  D0,D6                       ;Left X coordinate
                move.w  D7,spalte(A4)               ;Akt.Cursor's position
                move.w  D6,D7                       ;Right x coordinate
                addq.w  #7,D7                       ;8 characters input
                sub.w   upper_line(A4),D1
                move.w  D1,zeile(A4)
form_inp1:      bsr     cursor_on
form_inp2:      moveq   #27,D0                      ;With Esc Speechigen
                btst    #0,maustast(A4)             ;Right mouse button pressed?
                bne.s   form_inp3                   ;Yes!=> Execute ESC
                jsr     @conin(A4)                  ;Get key code
                beq.s   form_inp2                   ;Button has been pressed
form_inp3:      jsr     @cursor_off(A4)
form_inp4:      btst    #0,maustast(A4)
                bne.s   form_inp4                   ; Wait for letting go
                bclr    #28,D0                      ;caps/lockWeg
                cmp.l   #$4B0000,D0                 ;Cursor left
                bne.s   form_inp5
                cmp.w   spalte(A4),D6
                beq.s   form_inp1                   ;Left edge already reached
                subq.w  #1,spalte(A4)
                bra.s   form_inp1
form_inp5:      cmp.l   #$4D0000,D0                 ;Cursor right
                bne.s   form_inp6
                cmp.w   spalte(A4),D7
                beq.s   form_inp1                   ;Right edge already reached
                addq.w  #1,spalte(A4)
                bra.s   form_inp1
form_inp6:      cmp.l   #$610000,D0
                beq.s   form_inp13                  ;UNDO =cancellation
                cmp.w   #27,D0
                beq.s   form_inp13                  ;ESC = cancellation
                cmp.w   #13,D0
                beq.s   form_inp10                  ;Return = Number
                cmp.w   #'0',D0
                blo.s   form_inp8
                cmp.w   #'9',D0
                bls.s   form_inp9
form_inp7:      cmp.w   #'A',D0                     ;Only hex pays allowed!
                blo.s   form_inp8
                cmp.w   #'F',D0
                bls.s   form_inp9
                bclr    #5,D0
                bne.s   form_inp7
form_inp8:      bsr     c_bell                      ;Pling, because key not allowed
                moveq   #0,D0
form_inp9:      jsr     @chrout(A4)                 ;Output / execute characters
                cmp.w   spalte(A4),D7
                bhs     form_inp1                   ;End of the input reached
                move.w  D7,spalte(A4)
                bra     form_inp1
form_inp10:     move.w  D6,spalte(A4)               ;Cursor at entry start
                bsr     calc_crsr                   ;A0 shows on position in the screen memory
                moveq   #0,D1
                moveq   #7,D7
form_inp11:     move.b  (A0)+,D0

                sub.b   #$30,D0
                cmp.w   #9,D0
                bls.s   form_inp12                  ;Get hex number
                subq.w  #7,D0
form_inp12:     rol.l   #4,D1                       ;A Nibble to the left
                or.b    D0,D1                       ;and paste the number
                dbra    D7,form_inp11
                movea.l (SP),A0
                move.l  D1,(A0)                     ;Change register
form_inp13:     addq.l  #4,SP                       ;Register address from the stack
                move.l  (SP)+,zeile(A4)
                bsr.s   cursor_on
                bsr     set_maus
                subq.b  #1,set_lock(A4)             ;Prevent cursoring in the VBL
                move    #0,CCR
                rts
                ENDPART

********************************************************************************
* Cursor Turn on / off                                                         *
********************************************************************************
                PART 'cursor_on'
cursor_on:      bsr.s   flash_cursor                ;Cursor represent
                move.w  #$FF20,curflag(A4)          ;Cursor is on
cursor_:        rts
                ENDPART

********************************************************************************

                PART 'cursor_off'
cursor_off:     bclr    #7,curflag(A4)              ;Stop cursor immediately
                bclr    #6,curflag(A4)
                beq.s   cursor_
                bra.s   flash_cursor
                ENDPART

********************************************************************************
* Cursor inverter.                                                             *
********************************************************************************
                PART 'flash_cursor'
cursor_tab:     DC.B $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                DC.B $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
                DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$FF
                DC.B $FF,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$81,$FF

flash_cursor:   movem.l D0-D3/A0-A2,-(SP)
                move.w  cursor_form(A4),D0
                lea     cursor_tab(PC,D0.w),A2
                bsr     clr_maus
                bsr     calc_crsr
                lea     debugger_scr(A4),A1
                tst.b   scr_moni(A1)
                bne.s   fcurso1                     ;Color =>
                lsl.w   #4,D2                       ;Curses * 1280.
                add.w   D3,D2                       ;+Cursor split
                movea.l scr_adr(A1),A1
                adda.w  D2,A1                       ;+ Screen address
                moveq   #15,D2
fcurso0:        move.b  (A2)+,D0
                eor.b   D0,(A1)
                lea     80(A1),A1
                dbra    D2,fcurso0
                bra.s   fcurso3
fcurso1:        lsl.w   #4,D2                       ;Curses * 1280.
                move.w  D3,D1
                andi.w  #-2,D3
                add.w   D3,D3
                add.w   D3,D2                       ;+ (split And2) *2
                andi.w  #1,D1
                add.w   D1,D2                       ;+ (split And1)
                movea.l scr_adr(A1),A1
                adda.w  D2,A1                       ;+ Screen address
                moveq   #7,D2
fcurso2:        move.b  (A2)+,D0
                or.b    (A2)+,D0
                eor.b   D0,(A1)
                lea     160(A1),A1
                dbra    D2,fcurso2
fcurso3:        bsr     set_maus
                movem.l (SP)+,D0-D3/A0-A2
                rts
                ENDPART

********************************************************************************
* Save image (possibly) as Screendump                                          *
********************************************************************************
                PART 'kb_save'
kb_save:        movem.l D0-A6,-(SP)
                tst.b   _dumpflg(A4)                ;Hardcopy (as a picture)?
                bne     kb_save3
                move.b  #1,_dumpflg(A4)

                lea     debugger_scr(A4),A3

                lea     prn_buff(A4),A1
                clr.b   (A1)+
                move.b  scr_rez(A3),(A1)+           ;Remember resolution (0-2)
                lea     scr_colors(A3),A0
                moveq   #7,D0
kb_save1:       move.l  (A0)+,(A1)+                 ;Get color
                dbra    D0,kb_save1                 ;already read all color registers

                jsr     do_mediach                  ;Trigger Media-Change
                moveq   #'1',D0
                add.b   scr_rez(A3),D0
                move.b  D0,kb_nam2                  ;Set extension ("1" - "3")
                clr.w   -(SP)
                pea     kb_name(PC)
                move.w  #$3C,-(SP)
                bsr     do_trap_1                   ;Fopen("?:\BUG_?.PI?",0)
                addq.l  #8,SP
                move.w  D0,D7
                bmi.s   kb_save2                    ;Error occurred
                pea     prn_buff(A4)
                pea     34.w
                move.w  D7,-(SP)
                move.w  #$40,-(SP)
                bsr     do_trap_1                   ;Fwrite(Handle,34.L,Resolution + colors)
                lea     12(SP),SP
                tst.w   D0
                bmi.s   kb_save2                    ;Error occurred
                move.l  scr_adr(A3),-(SP)           ;= Screen address
                pea     32000.w                     ;Size of the screen
                move.w  D7,-(SP)                    ;filehandle
                move.w  #$40,-(SP)
                bsr     do_trap_1                   ;Fwrite(Handle,32000.L,Screen)
                lea     12(SP),SP
                tst.l   D0
                bmi.s   kb_save2                    ;Error occurred
                move.w  D7,-(SP)                    ;Filehandle
                move.w  #$3E,-(SP)
                bsr     do_trap_1                   ;Fclose(Handle)
                addq.l  #4,SP
kb_save2:       st      _dumpflg(A4)
                addq.b  #1,kb_nam1
                cmpi.b  #'Z',kb_nam1
                ble.s   kb_save3                    ;Change filenames for the next time
                move.b  #'A',kb_nam1
kb_save3:       movem.l (SP)+,D0-A6
                rts

init_save:      move.w  #$19,-(SP)
                trap    #1                          ;Dgetdrv() - Determine the current drive
                addq.l  #2,SP
                addi.b  #'A',D0
                move.b  D0,kb_name                  ;and remember
                rts
kb_name:        DC.B 'x:\BUG_'
kb_nam1:        DC.B 'A.PI'
kb_nam2:        DC.B 'x',0
                EVEN
                ENDPART

********************************************************************************
* Character Edition + Screen Editor                                            *
********************************************************************************
********************************************************************************
* The screen editor                                                            *
********************************************************************************
                PART 'scr_edit'
scr_edit:       st      ignore_autocrlf(A4)         ;Press CR / LF after the function
                moveq   #$11,D0
                jsr     @ikbd_send(A4)              ;Keyboard weather and
                tst.l   quit_stk(A4)
                bne.s   scr_ed0
                move.l  #$04220067,D0               ;ctrl+g (go!!!)
scr_ed0:        bsr     cursor_on                   ;Turn on the cursor
scr_ed1:        jsr     @conin(A4)                  ;Get key code
                bne.s   scr_ed2                     ;Button has been pressed
                bsr     mauschk                     ;Evaluate mouse
                bpl.s   scr_ed1                     ;No mouse reaction
scr_ed2:        jsr     @cursor_off(A4)             ;Switch off the cursor
                move.l  D0,D7                       ;Remember key code
                bsr     do_fkeys                    ;F buttons?
                bmi.s   scr_edit                    ;Function executed
                bsr     do_scrkeys                  ;Screen Page Switching
                bmi.s   scr_edit                    ;Function executed
                swap    D7
                move.b  D7,direct(A4)               ;Scan-Code Merks / Direct Mode and
                lea     stab(PC),A0
                movea.l A0,A1
scr_ed3:        move.w  (A0)+,D0                    ;Nothing found
                beq.s   scr_ed4                     ;Ascii code not found => Output
                cmp.l   (A0)+,D7                    ;Keyboard code code in the table?
                bne.s   scr_ed3                     ;No, continue searching!
                lea     0(A1,D0.w),A0
                jsr     (A0)                        ;Function
                clr.b   direct(A4)
                bra.s   scr_edit
scr_ed4:        tst.b   ins_mode(A4)
                beq.s   scr_ed5
                bsr     c_ins                       ;Insert sign
scr_ed5:        swap    D7
                move.w  D7,D0
                jsr     @chrout(A4)
                bra.s   scr_edit

                BASE DC.W,stab
stab:           DC.W cache_up,0,$084B               ;Alt+left
                DC.W cache_down,0,$084D             ;Alt+right
                DC.W cache_fix,0,$0847              ;Alt+clr/home
                DC.W cache_get,0,$0852              ;Alt+Insert
                DC.W c_cup,0,$48                    ;Cursor up
                DC.W c_cdown,0,$50                  ;Cursor down
                DC.W c_cleft,0,$4B                  ;Cursor left
                DC.W c_cright,0,$4D                 ;Cursor right
                DC.W c_clrhome,$37,$0147            ;Clr/Home
                DC.W c_home,0,$47                   ;Home
                DC.W c_eop,$1B,1                    ;ESC
                DC.W c_del,$7F,$53                  ;Delete
                DC.W c_ins,0,$52                    ;Insert
                DC.W c_eol,$7F,$0153                ;Shift+Delete
                DC.W c_undo,0,$61                   ;UNDO
                DC.W c_inson,$30,$0152              ;Shift+Insert
                DC.W c_cdl,$0D,$041C                ;Ctrl+Return
                DC.W c_clrli,$7F,$0453              ;Ctrl+Delete
                DC.W c_insli,0,$0452                ;Ctrl+Insert
                DC.W c_scrup,$32,$0150              ;Shift+Down
                DC.W c_scrdown,$38,$0148            ;Shift+Up
                DC.W c_asyup,0,$0550                ;Shift+Control+Down
                DC.W c_asydown,0,$0548              ;Shift+Control+Up
                DC.W c_scrlft,0,$044B               ;Control+Left
                DC.W c_scrrgt,0,$044D               ;Control+Right
                DC.W c_scrlft,$34,$014B             ;Shift+Left
                DC.W c_scrrgt,$36,$014D             ;Shift+Right
                DC.W c_end,$0D,$1C                  ;Return
                DC.W c_tab,9,$0F                    ;Tab
                DC.W c_bakspc,8,$0E                 ;Backspace
                DC.W c_bakspc,8,$010E               ;Shift+Backspace
                DC.W set_pc,$70,$0419               ;CTRL+p
                DC.W set_bkpt,$62,$0430             ;CTRL+b
                DC.W set_go,$67,$0422               ;CTRL+g
                DC.W c_lline,$6D,$0832              ;Alt+M
                DC.W c_sline,$6D,$0432              ;Control+M
                DC.W do_help,0,$0462                ;CTRL+HELP
                DC.W sdo_help,0,$0562               ;SHIFT+CTRL+HELP
                DC.W set_trace,$59,$042C            ;CTRL-Y : F1
                DC.W set_skip,$53,$041F             ;CTRL-S : F5
                DC.W set_do_pc,$41,$041E            ;CTRL-A : F2
                DC.W 0

;************************************************************************
;* CTRL-Functions                                                       *
;************************************************************************
set_trace:      jmp     f_trace
set_skip:       jmp     f_skip
set_do_pc:      jmp     f_do_pc

set_go:         move.w  upper_offset(A4),D0
                lea     screen(A4),A0
                adda.w  D0,A0
                move.w  zeile(A4),D0
                mulu    #80,D0
                adda.w  D0,A0
                movea.l default_adr(A4),A6
                jsr     @get_line(A4)               ;Etv. Address on the beginning of the line
                move.l  A6,_pc(A4)
                bra     go_pc                       ;Start program

set_pc:         move.w  upper_offset(A4),D0
                lea     screen(A4),A0
                adda.w  D0,A0
                move.w  zeile(A4),D0
                mulu    #80,D0
                adda.w  D0,A0
                movea.l default_adr(A4),A6
                jsr     @get_line(A4)               ;Etv. Address on the beginning of the line
                move.l  A6,_pc(A4)
                bsr     set_reg
                bra     rgout                       ;Set

set_bkpt:       jsr     @conin(A4)                  ;Get key code
                beq.s   set_bkpt                    ;Button has been pressed
                andi.w  #$FF,D0
                subi.w  #'0',D0
                bmi.s   set_bk2                     ;Code too small
                cmp.w   #9,D0
                bls.s   set_bk3
                subq.w  #7,D0
                bmi.s   set_bk2
                cmp.w   #15,D0                      ;No 0-9 / A-F
                bls.s   set_bk3
                subi.w  #32,D0                      ;lowercase letters
                cmp.w   #10,D0
                blo.s   set_bk2
                cmp.w   #15,D0
                bhi.s   set_bk2
set_bk3:        move.w  D0,D7
                mulu    #12,D7
                move.w  upper_offset(A4),D0
                lea     screen(A4),A0
                adda.w  D0,A0
                move.w  zeile(A4),D0
                mulu    #80,D0
                adda.w  D0,A0
                movea.l default_adr(A4),A6
                jsr     @get_line(A4)               ;Etv. Address on the beginning of the line
                move.l  A6,D1
                btst    #0,D1
                bne.s   set_bk2                     ;odd
                movea.l D1,A6
                bsr     check_write
                bne.s   set_bk2
                lea     breakpnt(A4),A1
                move.l  D1,0(A1,D7.w)
                move.w  #-1,4(A1,D7.w)              ;Stop-Breakpoint
                move.l  #1,6(A1,D7.w)               ;run only once
set_bk2:        rts

cache_get:      jmp     getcache
cache_fix:      pea     @cursor_off(A4)
                moveq   #2,D1
                bra     mausch5
cache_up:       pea     @cursor_off(A4)
                moveq   #3,D1
                bra     mausch5
cache_down:     pea     @cursor_off(A4)
                moveq   #4,D1
                bra     mausch5

sdo_help:       moveq   #-1,D7                      ;No PC conversion
                bra.s   do_help00
do_help:        move.b  fast_exit(A4),D7
do_help00:      clr.b   fast_exit(A4)
                tst.b   help_allow(A4)              ;Ctrl help allowed?
                bpl     cmd_exit1                   ;return
                tst.l   quit_stk(A4)                ;2.Test: Indirect call?
                beq     cmd_exit1                   ;return
                clr.l   line_back(A4)               ;Line no the return line = 0
                movea.l basep(A4),A0                ;Major address
                movea.l 8(A0),A1                    ;Initial ID of the TEXT segment
                move.l  $18(A0),D2                  ;Initial ID of the BSS segment
                add.l   $1C(A0),D2                  ;+ Long from BSS segments
                moveq   #-1,D0
                tst.b   D7                          ;End of the program?
                bne.s   do_help0                    ;then no PC conversion
                move.l  _pc(A4),D0                  ;the current PC
                bsr.s   do_help2
do_help0:       move.l  D0,line_back(A4)            ;PC offset calculated
                lea     simple_vars(A4),A2
                lea     spaced2(A4),A3
                moveq   #9,D1
do_help1:       move.l  (A2)+,D0
                bsr.s   do_help2                    ;Convert the 10 user variables to offsets
                move.l  D0,(A3)+
                dbra    D1,do_help1
                bra     cmd_exit1                   ;Then out!

do_help2:       cmp.l   A1,D0
                blo.s   do_help3                    ;smaller than the TEXT segment
                cmp.l   D2,D0                       ;Pointer behind the BSS segment
                bhs.s   do_help3                    ;Greater than the DATA Segment
                sub.l   A1,D0
                rts
do_help3:       moveq   #-1,D0                      ;Dat was probably nothing
                rts

;************************************************************************
;* F-buttons management                                                 *
;************************************************************************
do_fkeys:       tst.l   D0                          ;F buttons?
                bpl.s   do_fke4                     ;No, end
                swap    D7
                andi.w  #$0C00,D7                   ;control /AlternateTesten
                bne.s   do_fke5                     ;pressed => Menu bar
                addq.w  #1,D0                       ;1 to 20
                moveq   #0,D7                       ;select
                bsr     sel_menu                    ;Select entry
                add.w   D0,D0                       ;(fTaste1)*2
                lea     f_jumps,A0
                adda.w  -2(A0,D0.w),A0
                jsr     (A0)                        ;Run F key
                jsr     @desel_menu(A4)             ;Possibly selected menu item deselect
do_fke5:        move    #$FF,CCR
do_fke4:        rts

;************************************************************************
;* Alternate / Control '1' until '9'                                      *
;************************************************************************
do_scrkeys:     tst.w   D0
                bpl     do_sck4
                move.w  upper_offset(A4),D1
                lea     screen(A4),A1
                adda.w  D1,A1
                movea.l #scr_buff,A0
                adda.l  A4,A0
                andi.w  #$7FFF,D7
                mulu    #1606,D7
                adda.w  D7,A0                       ;Pointer to the screen
                swap    D0
                andi.w  #$0400,D0                   ;Control?= right
                bne.s   do_sck2                     ;Yes
                tst.w   (A0)                        ;Screen already saved?
                beq.s   do_sck5                     ;No => End
                addq.l  #2,A0
                move.l  (A0)+,zeile(A4)             ;Cursor
                move.w  down_lines(A4),D0
                subq.w  #1,D0                       ;Normally 19
do_sck1:        moveq   #4,D1
do_sck11:       move.l  (A0)+,(A1)+                 ;Screen back
                move.l  (A0)+,(A1)+
                move.l  (A0)+,(A1)+
                move.l  (A0)+,(A1)+
                dbra    D1,do_sck11
                dbra    D0,do_sck1
                jsr     @redraw_all(A4)             ;Rebuild screen
                bra.s   do_sck5
do_sck2:        st      (A0)                        ;Screen exists
                addq.l  #2,A0
                move.l  zeile(A4),(A0)+             ;Remember cursor
                movea.l A0,A2
                move.l  #'    ',D1
                move.w  #399,D0
do_sck21:       move.l  D1,(A2)+
                dbra    D0,do_sck21
                move.w  down_lines(A4),D0
                subq.w  #1,D0                       ;Normally 19
do_sck3:        moveq   #4,D1
do_sck31:       move.l  (A1)+,(A0)+                 ;Save screen
                move.l  (A1)+,(A0)+
                move.l  (A1)+,(A0)+
                move.l  (A1)+,(A0)+
                dbra    D1,do_sck31
                dbra    D0,do_sck3
do_sck5:        move    #$FF,CCR
do_sck4:        rts
                ENDPART

********************************************************************************
* Output text on the stack to zero byte in line D0                             *
********************************************************************************
                PART 'print_inv_line'
print_inv_line: movem.l D0-D7/A1-A6,-(SP)
                move.l  zeile(A4),-(SP)
                movea.l 64(SP),A6                   ;Initial address of the text
                clr.w   spalte(A4)
                move.w  D0,zeile(A4)
print_inv_line1:moveq   #0,D0
                move.b  (A6)+,D0
                beq.s   print_inv_line3
                moveq   #-1,D1
                moveq   #-1,D2
                bsr     light_char                  ;Invert 8 characters
                addq.w  #1,spalte(A4)
                bra.s   print_inv_line1
print_inv_line3:move.l  (SP)+,zeile(A4)
                movem.l (SP)+,D0-D7/A1-A6
                move.l  (SP)+,(SP)                  ;Copy return address via string address
                rts
                ENDPART

********************************************************************************
* Output text on the stack to zero byte                                        *
********************************************************************************
                PART 'print_line'
print_line:     movem.l D0-D7/A1-A6,-(SP)
                movea.l 60(SP),A6                   ;Initial address of the text
print_line1:    move.b  (A6)+,D0
                beq.s   print_line3
                cmp.b   #13,D0
                bne.s   print_line2
                jsr     @c_eol(A4)
                jsr     @crout(A4)
                bra.s   print_line1
print_line2:    bsr.s   chrout
                bra.s   print_line1
print_line3:    movem.l (SP)+,D0-D7/A1-A6
                move.l  (SP)+,(SP)                  ;Copy return address via string address
                rts
                ENDPART

********************************************************************************
* Spaces to cursor in column D0                                                *
********************************************************************************
                PART 'spacetab'
spacetab:       tst.b   device(A4)                  ;Printer or file output?
                bne.s   spacetab1                   ;But then away
                cmp.w   spalte(A4),D0
                beq.s   spacetab2                   ;Cursor in the column
                bsr.s   space
                bra.s   spacetab
spacetab1:      cmp.w   prn_pos(A4),D0              ;Tab for'n printers
                beq.s   spacetab2
                bsr.s   space
                bra.s   spacetab1
spacetab2:      rts
                ENDPART

********************************************************************************
* A few global issues                                                          *
********************************************************************************
                PART 'crout'
crout:          movem.l D0-A6,-(SP)
                lea     prn_buff(A4),A0
                moveq   #0,D1
                move.w  prn_pos(A4),D1
                tst.b   device(A4)
                bmi     c_prncr                     ;to the printer
                bne.s   c_filecr                    ;or in a file
                clr.w   spalte(A4)
                bsr     c_cdown
                movem.l (SP)+,D0-A6
                rts
                ENDPART

********************************************************************************

                PART 'space'
space:          move.l  D0,-(SP)
                moveq   #' ',D0
                bsr.s   chrout
                move.l  (SP)+,D0
                rts
                ENDPART

********************************************************************************

                PART 'gleich_out'
gleich_out:     moveq   #'=',D0
                ENDPART

********************************************************************************
* Spend characters in D0                                                       *
********************************************************************************
                PART 'chrout'
chrout:         tst.b   testwrd(A4)                 ;Issue in the buffer?
                bne.s   chrout1                     ;Yes!
                tst.b   D0
                beq.s   chrout2                     ;Null bytes are not spent
                bra     charout                     ;then spend
chrout1:        move.b  D0,(A0)+                    ;Sign in output bushing
chrout2:        rts
                ENDPART

********************************************************************************
* File                                                                         *
********************************************************************************
                PART 'c_file'
c_file:         lea     prn_buff(A4),A0
                moveq   #0,D1
                move.w  prn_pos(A4),D1
                cmp.b   #32,D0
                blo.s   c_file1
                cmp.b   #'ꣻ',D0
                bne.s   c_file0
c_file1:        moveq   #'.',D0
c_file0:        move.b  D0,0(A0,D1.w)               ;abInDenBuffer
                addq.w  #1,prn_pos(A4)
                cmpi.w  #80,prn_pos(A4)
                bhs.s   c_prncr
                movem.l (SP)+,D0-A6
                rts

c_filecr:       move.b  #13,0(A0,D1.w)              ;cr
                move.b  #10,1(A0,D1.w)              ;lf
                move.w  _fhdle2(A4),D0              ;Handle <= 0 => Error
                bls.s   c_filec1
                pea     prn_buff(A4)
                addq.l  #2,D1
                move.l  D1,-(SP)
                move.w  D0,-(SP)
                move.w  #$40,-(SP)
                bsr     do_trap_1
                lea     12(SP),SP
                clr.w   prn_pos(A4)                 ;Reset pointer
                cmp.l   D0,D1
                bne.s   c_filec2
                movem.l (SP)+,D0-A6
                rts
c_filec1:       jmp     file_er
c_filec2:       jmp     dskfull
                ENDPART

********************************************************************************
* Printer edition                                                              *
********************************************************************************
                PART 'c_prn'
c_prn:          lea     prn_buff(A4),A0
                move.w  prn_pos(A4),D1
                cmp.b   #32,D0
                blo.s   c_prn1
                cmp.b   #'ꣻ',D0
                bne.s   c_prn0
c_prn1:         moveq   #'.',D0
c_prn0:         move.b  D0,0(A0,D1.w)               ;From in the Buffer
                addq.w  #1,prn_pos(A4)
                cmpi.w  #80,prn_pos(A4)
                bhs.s   c_prncr
                movem.l (SP)+,D0-A6
                rts


c_prncr:        clr.b   0(A0,D1.w)
                btst    #0,$FFFFFA01.w              ;printer busy =>
                bne.s   c_prn5                      ;nothing to do
                lea     prn_buff(A4),A5
c_prn3:         move.b  (A5)+,D0
                beq.s   c_prn4                      ;End of the line
                bsr     prnout                      ;Sign to the printer
                bra.s   c_prn3                      ;Go on
c_prn4:         bsr     prncr                       ;CR + LF to the printer
                clr.w   prn_pos(A4)                 ;Reset pointer
                movem.l (SP)+,D0-A6
                rts
c_prn5:         jmp     prn_err
                ENDPART

********************************************************************************
* Output characters in D0 with character conversion                            *
********************************************************************************
                PART 'charcout'
charcout:       movem.l D0-A6,-(SP)
                lea     convert_tab(A4),A0
                andi.w  #$FF,D0
                move.b  0(A0,D0.w),D0               ;Convert sign
                bra.s   c_char1
                ENDPART

********************************************************************************
* Output characters in D0 without control characters                           *
********************************************************************************
                PART 'charout'
charout:        movem.l D0-A6,-(SP)
c_char1:        tst.b   device(A4)
                bmi.s   c_prn
                bne     c_file
                bsr     char_out
                bsr.s   c_cright
                movem.l (SP)+,D0-A6
                rts
                ENDPART

********************************************************************************
* Control sign                                                                 *
********************************************************************************
                PART 'c_cxxx'
c_cright:       addq.w  #1,spalte(A4)
                cmpi.w  #79,spalte(A4)
                ble.s   c_cdow2
c_crlr:         clr.w   spalte(A4)
c_cdown:        addq.w  #1,zeile(A4)
                move.w  zeile(A4),D1
                addq.w  #1,D1
                cmp.w   down_lines(A4),D1
                ble.s   c_cdow2
                move.w  down_lines(A4),zeile(A4)
                subq.w  #1,zeile(A4)
                bra     scrollup
c_cdow2:        rts

c_inson:        not.b   ins_mode(A4)                ;Autoinsert on / off
                bra     set_ins_flag

c_tab:          tst.b   ins_mode(A4)
                bne.s   c_tab2                      ;insertModeAn! =>
                move.w  spalte(A4),D6
                addq.w  #8,D6
                andi.w  #248,D6
                cmp.w   #80,D6
                bne.s   c_tab1
                subq.w  #1,D6
c_tab1:         move.w  D6,spalte(A4)
                rts
c_tab2:         bsr     c_ins                       ;Insert sign
                bsr.s   c_cright                    ;Cursor a position to the right
                moveq   #7,D6
                and.w   spalte(A4),D6
                bne.s   c_tab2                      ;Tab position achieved?
                rts

c_eol:          tst.b   device(A4)                  ;Issue deflection?
                bne     c_rts                       ;then spend nothing
                movem.l D1-A0,-(SP)
                bsr     calc_crsr                   ;positionInA0,SpalteInD3
                neg.w   D3
                add.w   #79,D3
c_eol1:         move.b  #' ',(A0)+
                dbra    D3,c_eol1
                movem.l (SP)+,D1-A0
                move.w  zeile(A4),D0
                bra     update_line

;Rest des Bildschirms löschen
c_eop:          bsr     calc_crsr                   ;positionInA0,SpalteInD3
                lea     screen+2080(A4),A1
c_eop1:         move.b  #' ',(A0)+                  ;Remainder Delete the page
                cmpa.l  A1,A0
                blo.s   c_eop1
                move.w  zeile(A4),D0
c_eop2:         bsr     update_line                 ;Provide a page residue
                addq.w  #1,D0
                cmp.w   down_lines(A4),D0
                blo.s   c_eop2
                rts

c_del:          bsr     calc_crsr                   ;Address in A0, column in D3
                neg.w   D3
                add.w   #78,D3                      ;Put = hack
                bmi.s   c_del1
                lea     1(A0),A1
c_del2:         move.b  (A1)+,(A0)+
                dbra    D3,c_del2
c_del1:         move.b  #' ',(A0)+                  ;Space to the line end
                move.w  zeile(A4),D0
                bra     update_line                 ;Provide line

c_bakspc:       bsr     calc_crsr                   ;Address in A0, column in D3
                tst.w   D3
                beq.s   c_rts
                cmpi.w  #10,spalte(A4)
                bne.s   c_baksp1
                cmpi.b  #'>',-1(A0)
                beq.s   c_rts
                cmpi.b  #'0',-1(A0)
                beq.s   c_rts
                cmpi.b  #'',-1(A0)
                beq.s   c_rts
c_baksp1:       subq.w  #1,spalte(A4)
                neg.w   D3
                add.w   #79,D3                      ;D3 = 79-D3
                lea     -1(A0),A1
c_baksp2:       move.b  (A0)+,(A1)+
                dbra    D3,c_baksp2
                move.b  #' ',(A1)+                  ;Space to the line end
                move.w  zeile(A4),D0
                bra     update_line                 ;Provide line
c_rts:          rts

c_ins:          bsr     calc_crsr                   ;Address in A0, column in D3
                neg.w   D3
                add.w   #79,D3                      ;D3 = 79-D3
                lea     0(A0,D3.w),A1
                lea     1(A0,D3.w),A0
                beq.s   c_ins2                      ;Insert in the last column
                subq.w  #1,D3
c_ins1:         move.b  -(A1),-(A0)
                dbra    D3,c_ins1
c_ins2:         move.b  #' ',-(A0)                  ;Space use
                move.w  zeile(A4),D0
                bra     update_line                 ;Provide line

c_cleft:        subq.w  #1,spalte(A4)
                bpl.s   c_rts
                move.w  #79,spalte(A4)
c_cup:          subq.w  #1,zeile(A4)
                bpl.s   c_rts
                clr.w   zeile(A4)
                bra     scrolldwn

c_bell:         lea     bell_data(PC),A0

;************************************************************************
;* Sound from A0 "sound"                                                *
;************************************************************************
do_sound:       movem.l D0/A1,-(SP)
                lea     $FFFF8800.w,A1
do_sound1:      move.w  (A0)+,D0
                bmi.s   do_sound2
                movep.w D0,0(A1)
                bra.s   do_sound1
do_sound2:      movem.l (SP)+,D0/A1
                rts

;************************************************************************
;* sound data                                                           *
;************************************************************************
bell_data:      DC.W $34,$0100,$0200,$0300,$0400,$0500,$0600
                DC.W $07FE,$0810,$0900,$0A00,$0B00,$0C10,$0D09,-1
clickdata:      DC.W $3B,$0100,$0200,$0300,$0400,$0500,$0600
                DC.W $07FE,$0810,$0D03,$0B80,$0C01,-1

;************************************************************************
;* Continue screen commands                                             *
;************************************************************************
c_clrhome:      bsr     clr_maus
                lea     debugger_scr(A4),A0
                movea.l scr_adr(A0),A0
                move.w  upper_offset(A4),D0
                lsl.w   #4,D0
                adda.w  D0,A0
                move.w  down_lines(A4),D0
c_clho1:        moveq   #79,D1
c_clho3:        clr.l   (A0)+                       ;Delete 20 lines
                clr.l   (A0)+
                clr.l   (A0)+
                clr.l   (A0)+
                dbra    D1,c_clho3
                dbra    D0,c_clho1
                bsr     set_maus
                move.l  D2,-(SP)
                move.w  upper_offset(A4),D0
                lea     screen(A4),A0
                adda.w  D0,A0
                move.w  down_lines(A4),D0           ;Delete 20 lines
                move.l  #'    ',D2
c_clho2:        moveq   #4,D1
c_clho4:        move.l  D2,(A0)+
                move.l  D2,(A0)+
                move.l  D2,(A0)+
                move.l  D2,(A0)+
                dbra    D1,c_clho4
                dbra    D0,c_clho2
                move.l  (SP)+,D2
c_home:         clr.l   zeile(A4)
                rts

c_undo:         lea     _zeile2(A4),A6
                tst.b   (A6)
                beq.s   c_undo2
                clr.w   spalte(A4)
                jsr     @c_eol(A4)                  ;delete a line
c_undo1:        move.b  (A6)+,D0                    ;Pour buffer into the line
                beq.s   c_undo2
                bsr     charout                     ;Spend characters
                bra.s   c_undo1
c_undo2:        rts

c_lline:        move.w  spalte(A4),-(SP)
                lea     _zeile3(A4),A6
                tst.b   (A6)
                beq.s   c_llin2                     ;No line in the buffer
                moveq   #0,D0
                clr.w   spalte(A4)
                moveq   #78,D7
c_llin1:        move.b  (A6)+,D0                    ;Pour buffer into the line
                bsr     charout                     ;Spend characters
                dbra    D7,c_llin1
c_llin2:        move.w  (SP)+,spalte(A4)
                rts

c_sline:        move.w  spalte(A4),-(SP)
                clr.w   spalte(A4)
                bsr     calc_crsr
                lea     _zeile3(A4),A1
                moveq   #19,D0
c_slin1:        move.l  (A0)+,(A1)+                 ;zeileInBuffer
                dbra    D0,c_slin1
                move.w  (SP)+,spalte(A4)
                rts

c_cdl:          move.w  down_lines(A4),zeile(A4)
                subq.w  #1,zeile(A4)
                clr.w   spalte(A4)
                bsr     calc_crsr
                cmpi.b  #'$',(A0)
                bne.s   c_cdl1
                move.w  #8,spalte(A4)
c_cdl1:         rts

c_clrli:        move.w  zeile(A4),D0
                bra     scroll_up2

c_insli:        move.w  zeile(A4),D0
                bra     scroll_dn2

c_end:          clr.b   direct(A4)
                clr.w   spalte(A4)
                bsr     calc_crsr
                lea     _zeile(A4),A1
                moveq   #19,D0
c_end1:         move.l  (A0)+,(A1)+                 ;zeileInBuffer
                dbra    D0,c_end1
c_end2:         cmpi.b  #' ',-(A1)                  ;Remove spaces at the end of the line
                beq.s   c_end2
                addq.w  #1,A1
                clr.b   (A1)
                lea     _zeile(A4),A0
                lea     _zeile2(A4),A1
c_end3:         move.b  (A0)+,(A1)+                 ;Copy line into the undo buffer
                bne.s   c_end3
                jsr     @crout(A4)                  ;CR still spend
                addq.l  #4,SP                       ;Stack back
                rts

c_scrup:        move.l  zeile(A4),-(SP)
                move.w  down_lines(A4),zeile(A4)
                subq.w  #1,zeile(A4)
                bsr     c_cdown
                move.l  (SP)+,zeile(A4)
                rts

c_scrdown:      move.l  zeile(A4),-(SP)
                clr.w   zeile(A4)
                bsr     c_cup
                move.l  (SP)+,zeile(A4)
                rts

c_asyup:        move.l  zeile(A4),-(SP)
                move.w  down_lines(A4),zeile(A4)
                subq.w  #1,zeile(A4)
                bsr     c_cdown
                move.l  (SP)+,zeile(A4)
                move.w  zeile(A4),D0
                subq.w  #1,D0
                bpl.s   c_asyu1
                moveq   #0,D0
c_asyu1:        move.w  D0,zeile(A4)
                rts

c_asydown:      move.l  zeile(A4),-(SP)
                clr.w   zeile(A4)
                bsr     c_cup
                move.l  (SP)+,zeile(A4)
                move.w  zeile(A4),D0
                addq.w  #1,D0
                cmp.w   down_lines(A4),D0
                blo.s   c_asyd1
                move.w  down_lines(A4),D0
                subq.w  #1,D0
c_asyd1:        move.w  D0,zeile(A4)
                rts

c_scrlft:       clr.w   spalte(A4)
                bsr     calc_crsr
                moveq   #0,D0
                cmpi.b  #'$',(A0)
                bne.s   c_scrl1
                moveq   #10,D0
c_scrl1:        move.w  D0,spalte(A4)
                rts

c_scrrgt:       move.w  #79,spalte(A4)
                bsr     calc_crsr
                moveq   #78,D0
c_scrr1:        cmpi.b  #' ',-(A0)
                dbne    D0,c_scrr1
                bne.s   c_scrr2
                moveq   #78,D0
c_scrr2:        addq.w  #1,D0
                move.w  D0,spalte(A4)
                rts
                ENDPART

********************************************************************************
* ASC sign in D0 on screen                                                     *
********************************************************************************
                PART 'char_out'
char_out:       movem.l D0-D3/A0-A1,-(SP)
                and.w   #$FF,D0
                bsr     clr_maus
                bsr     calc_crsr
                move.b  D0,(A0)                     ;Insert ASCII characters
                lea     debugger_scr(A4),A1
                tst.b   scr_moni(A1)
                bne     char_o3                     ;Color monitor
                movea.l s_w_font(A4),A0             ;font address
                adda.w  D0,A0                       ;Plus ASCII code (= character ID)
                lsl.w   #4,D2                       ;Curses * 1280.
                add.w   D3,D2                       ;+Cursor split
                movea.l scr_adr(A1),A1
                adda.w  D2,A1                       ;+ Screen address
                move.b  (A0),(A1)
                move.b  $0100(A0),$50(A1)
                move.b  $0200(A0),$A0(A1)
                move.b  $0300(A0),$F0(A1)
                move.b  $0400(A0),$0140(A1)
                move.b  $0500(A0),$0190(A1)
                move.b  $0600(A0),$01E0(A1)
                move.b  $0700(A0),$0230(A1)         ;Output the character
                move.b  $0800(A0),$0280(A1)
                move.b  $0900(A0),$02D0(A1)
                move.b  $0A00(A0),$0320(A1)
                move.b  $0B00(A0),$0370(A1)
                move.b  $0C00(A0),$03C0(A1)
                move.b  $0D00(A0),$0410(A1)
                move.b  $0E00(A0),$0460(A1)
                move.w  upper_line(A4),D0
                neg.w   D0
                addq.w  #4,D0
                cmp.w   zeile(A4),D0
                beq.s   char_o2
                move.w  zeile(A4),D0
                addq.w  #1,D0
                beq.s   char_o2
                move.b  $0F00(A0),$04B0(A1)
char_o2:        bsr     set_maus
                movem.l (SP)+,D0-D3/A0-A1
                rts
char_o3:        movea.l farbfont(A4),A0
                adda.w  D0,A0                       ;Get address of the sign
                lsl.w   #4,D2                       ;Curses * 1280.
                move.w  D3,D1
                andi.w  #-2,D3
                add.w   D3,D3
                add.w   D3,D2                       ;+ (split and -2) * 2
                andi.w  #1,D1
                add.w   D1,D2                       ;+ (split and 1)
                movea.l scr_adr(A1),A1
                adda.w  D2,A1                       ;+ Screen address
                move.b  (A0),(A1)
                move.b  $0100(A0),$A0(A1)
                move.b  $0200(A0),$0140(A1)
                move.b  $0300(A0),$01E0(A1)
                move.b  $0400(A0),$0280(A1)         ;Spend characters
                move.b  $0500(A0),$0320(A1)
                move.b  $0600(A0),$03C0(A1)
                move.b  $0700(A0),$0460(A1)
                bsr     set_maus
                movem.l (SP)+,D0-D3/A0-A1
                rts
                ENDPART

********************************************************************************
* Calculate cursor position (pointer in A0)                                    *
********************************************************************************
                PART 'calc_crsr'
calc_crsr:      lea     screen(A4),A0
                move.w  zeile(A4),D2
                add.w   upper_line(A4),D2
                move.w  D2,D1
                lsl.w   #2,D1                       ;times 80
                add.w   D1,D2
                lsl.w   #4,D2
                move.w  spalte(A4),D3
                adda.w  D2,A0
                adda.w  D3,A0
                rts
                ENDPART

********************************************************************************
* Draw line (Line in D0)                                                       *
********************************************************************************
                PART 'draw_line'
draw_line:      movem.l D0-D4,-(SP)
                bsr     clr_maus
                moveq   #2,D3
                lea     debugger_scr(A4),A0
                cmpi.b  #2,scr_rez(A0)
                beq.s   draw_line1                  ;s/w =>
                moveq   #4,D3
                and.b   #$FE,D0
draw_line1:     mulu    #80,D0
                movea.l scr_adr(A0),A0
                lea     0(A0,D0.w),A0
                moveq   #-1,D1
                moveq   #39,D2
draw_line2:     move.w  D1,(A0)
                adda.w  D3,A0                       ;Plane-Offset on it
                dbra    D2,draw_line2
                bsr     set_maus
                movem.l (SP)+,D0-D4
                rts
                ENDPART

********************************************************************************
* Deflect line on device                                                       *
********************************************************************************
                PART 'wrt_dev'
wrt_dev:        movea.l A0,A2
                lea     prn_buff(A4),A1
write_dev4:     move.b  (A0)+,D0                    ;Line in the output busher
                beq.s   write_dev7
                cmp.b   #32,D0
                blo.s   write_dev5
                cmp.b   #'ꣻ',D0
                bne.s   write_dev6
write_dev5:     moveq   #'.',D0
write_dev6:     move.b  D0,(A1)+
                bra.s   write_dev4
write_dev7:     suba.l  A2,A0
                subq.l  #1,A0
                move.w  A0,prn_pos(A4)              ;Pointer behind the line (for CR)
                movem.l (SP)+,D0-A6
                rts
                ENDPART

********************************************************************************
* Provide line D0                                                              *
********************************************************************************
                PART 'update_line'
update_line:    movem.l D0-A6,-(SP)
                bsr     clr_maus
                lea     screen(A4),A0
                add.w   upper_line(A4),D0
                move.w  D0,D1
                lsl.w   #2,D1                       ;times 80
                add.w   D1,D0
                lsl.w   #4,D0
                adda.w  D0,A0                       ;Address of the screen line
                moveq   #80,D1
                bra.s   write_linee
                ENDPART

********************************************************************************
* Spend text from A0                                                           *
* D0 - Line (-5 to 19)                                                         *
********************************************************************************
                PART 'write_line'
write_line:     movem.l D0-A6,-(SP)
                tst.b   device(A4)                  ;Output to file, on printer?
                bne.s   wrt_dev                     ;then
                bsr     clr_maus
                lea     screen(A4),A2
                add.w   upper_line(A4),D0
                move.w  D0,D1
                lsl.w   #2,D1                       ;TIMES 80
                add.w   D1,D0
                lsl.w   #4,D0
                adda.w  D0,A2                       ;Address of the screen line
                lea     80(A2),A3
                move.b  (A3),D1                     ;Save 1stone of the follow-up line
                movea.l A0,A1
write_line0:    move.b  (A1)+,(A2)+                 ;Determine length of the line
                bne.s   write_line0
                move.b  D1,(A3)                     ;1.Done the follow-up line back
                subq.l  #1,A2
write_line00:   cmpa.l  A3,A2
                bhs.s   write_line01
                move.b  #' ',(A2)+                  ;Fill the rest of the line with SPACE
                bra.s   write_line00
write_line01:   suba.l  A0,A1
                move.w  A1,D1
                subq.w  #1,D1                       ;Length of the line

write_linee:    lsl.w   #4,D0                       ;Time 1280
                lea     debugger_scr(A4),A1
                movea.l scr_adr(A1),A2
                adda.w  D0,A2
                moveq   #80,D2
                cmp.w   D1,D2
                bhs.s   write_line1
                moveq   #80,D1
write_line1:    sub.w   D1,D2
                tst.b   scr_moni(A1)                ;Colour?
                bne     write_line11                ;Yes!=>
                movea.l s_w_font(A4),A1
                bra.s   write_line3
write_line2:    moveq   #0,D0
                move.b  (A0)+,D0
                lea     0(A1,D0.w),A3
                move.b  (A3),(A2)+
                move.b  $0100(A3),79(A2)
                move.b  $0200(A3),159(A2)
                move.b  $0300(A3),239(A2)
                move.b  $0400(A3),319(A2)
                move.b  $0500(A3),399(A2)
                move.b  $0600(A3),479(A2)
                move.b  $0700(A3),559(A2)
                move.b  $0800(A3),639(A2)
                move.b  $0900(A3),719(A2)
                move.b  $0A00(A3),799(A2)
                move.b  $0B00(A3),879(A2)
                move.b  $0C00(A3),959(A2)
                move.b  $0D00(A3),1039(A2)
                move.b  $0E00(A3),1119(A2)
                move.b  $0F00(A3),1199(A2)
write_line3:    dbra    D1,write_line2
                tst.w   D2
                beq.s   write_line10
                moveq   #80,D0
                sub.w   D2,D0
                moveq   #0,D3
                moveq   #15,D1
write_line4:    move.w  D2,D4
                lsr.w   #1,D4
                bhs.s   write_line5
                move.b  D3,(A2)+
write_line5:    lsr.w   #1,D4
                bhs.s   write_line6
                move.w  D3,(A2)+
write_line6:    lsr.w   #1,D4
                bhs.s   write_line9
                bra.s   write_line8
write_line7:    move.l  D3,(A2)+
write_line8:    move.l  D3,(A2)+
write_line9:    dbra    D4,write_line7
                adda.w  D0,A2
                dbra    D1,write_line4
write_line10:   bsr     set_maus
                movem.l (SP)+,D0-A6
                rts

write_line11:   move.w  A2,D4
                moveq   #0,D5
                movea.l farbfont(A4),A1
                bra.s   write_line13
write_line12:   moveq   #0,D0
                move.b  (A0)+,D0
                lea     0(A1,D0.w),A3
                move.b  (A3),(A2)+
                move.b  $0100(A3),159(A2)
                move.b  $0200(A3),319(A2)
                move.b  $0300(A3),479(A2)
                move.b  $0400(A3),639(A2)
                move.b  $0500(A3),799(A2)
                move.b  $0600(A3),959(A2)
                move.b  $0700(A3),1119(A2)
                bchg    D5,D4
                beq.s   write_line13
                addq.l  #2,A2
write_line13:   dbra    D1,write_line12
                tst.w   D2
                beq.s   write_line19
                moveq   #0,D3
                bclr    D5,D4
                beq.s   write_line14
                move.b  D3,(A2)+
                move.b  D3,159(A2)
                move.b  D3,319(A2)
                move.b  D3,479(A2)
                move.b  D3,639(A2)
                move.b  D3,799(A2)
                move.b  D3,959(A2)
                move.b  D3,1119(A2)
                subq.w  #1,D2
write_line14:   bra.s   write_line16
write_line15:   move.w  D3,(A2)+
                move.w  D3,158(A2)
                move.w  D3,318(A2)
                move.w  D3,478(A2)
                move.w  D3,638(A2)
                move.w  D3,798(A2)
                move.w  D3,958(A2)
                move.w  D3,1118(A2)
write_line16:   dbra    D2,write_line15
write_line19:   bsr     set_maus
                movem.l (SP)+,D0-A6
                rts
                ENDPART

********************************************************************************
* Screen roll up a line                                                        *
********************************************************************************
                PART 'scroll_up'
scroll_up:      movem.l D0-A3,-(SP)
                bsr     clr_maus
                lea     debugger_scr(A4),A0
                movea.l scr_adr(A0),A0
                move.w  upper_offset(A4),D0
                lsl.w   #4,D0
                adda.w  D0,A0
                lea     1280(A0),A1
                move.w  down_lines(A4),D0
                lsl.w   #4,D0
                subq.w  #1,D0
scr_up1:        movem.l (A1)+,D2-D7/A2-A3
                movem.l D2-D7/A2-A3,(A0)
                movem.l (A1)+,D2-D7/A2-A3           ;Copy 80 bytes
                movem.l D2-D7/A2-A3,32(A0)
                movem.l (A1)+,D2-D5
                movem.l D2-D5,64(A0)
                lea     80(A0),A0
                dbra    D0,scr_up1
                moveq   #79,D0
scr_up4:        clr.l   (A0)+                       ;26. Line Delete
                clr.l   (A0)+
                clr.l   (A0)+
                clr.l   (A0)+
                dbra    D0,scr_up4
                bsr     set_maus
                lea     screen(A4),A0
                adda.w  upper_offset(A4),A0
                lea     80(A0),A1
                move.w  down_lines(A4),D0
                subq.w  #1,D0
scr_up2:        movem.l (A1)+,D2-D7/A2-A3           ;Scroll 21 lines
                movem.l D2-D7/A2-A3,(A0)
                movem.l (A1)+,D2-D7/A2-A3
                movem.l D2-D7/A2-A3,32(A0)
                movem.l (A1)+,D2-D5
                movem.l D2-D5,64(A0)
                lea     80(A0),A0
                dbra    D0,scr_up2
                move.l  #'    ',D1                  ;26. Line Delete
                moveq   #4,D0
scr_up3:        move.l  D1,(A0)+
                move.l  D1,(A0)+
                move.l  D1,(A0)+
                move.l  D1,(A0)+
                dbra    D0,scr_up3
                bsr.s   scroll_delay
                movem.l (SP)+,D0-A3
                rts
                ENDPART

********************************************************************************
* When Ctrl is pressed, delay during scrolling                                 *
********************************************************************************
                PART 'scroll_delay'
scroll_delay:   btst    #2,kbshift(A4)              ;Ctrl pressed?
                beq.s   scroll_delay2               ;End, if not
                moveq   #0,D0
                move.w  scroll_d(A4),D0             ;Get scroll delay
                lsl.l   #4,D0
scroll_delay1:  subq.l  #1,D0
                bne.s   scroll_delay1               ;and delay
scroll_delay2:  rts
                ENDPART

********************************************************************************
* Screen to scroll down a line                                                 *
********************************************************************************
                PART 'scroll_dn'
scroll_dn:      movem.l D0-A3,-(SP)
                bsr     clr_maus
                lea     debugger_scr(A4),A0
                movea.l scr_adr(A0),A0
                lea     32000(A0),A0
                movea.l A0,A1
                lea     1280(A0),A0
                move.w  down_lines(A4),D0
                lsl.w   #4,D0
                subq.w  #1,D0
scr_dn1:        movem.l -32(A1),D2-D7/A2-A3
                movem.l D2-D7/A2-A3,-(A0)
                lea     -64(A1),A1
                movem.l (A1),D2-D7/A2-A3
                movem.l D2-D7/A2-A3,-(A0)
                lea     -16(A1),A1
                movem.l (A1),D2-D5
                movem.l D2-D5,-(A0)
                dbra    D0,scr_dn1
                moveq   #79,D0
scr_dn2:        clr.l   -(A0)
                clr.l   -(A0)
                clr.l   -(A0)
                clr.l   -(A0)
                dbra    D0,scr_dn2
                bsr     set_maus
                lea     screen+2080(A4),A0
                lea     screen+2000(A4),A1
                move.w  down_lines(A4),D0
                subq.w  #1,D0
scr_dn4:        movem.l -32(A1),D2-D7/A2-A3
                movem.l D2-D7/A2-A3,-(A0)
                lea     -64(A1),A1
                movem.l (A1),D2-D7/A2-A3
                movem.l D2-D7/A2-A3,-(A0)
                lea     -16(A1),A1
                movem.l (A1),D2-D5
                movem.l D2-D5,-(A0)
                dbra    D0,scr_dn4
                move.l  #'    ',D1
                moveq   #4,D0
scr_dn5:        move.l  D1,-(A0)                    ;Clear the top line
                move.l  D1,-(A0)
                move.l  D1,-(A0)
                move.l  D1,-(A0)
                dbra    D0,scr_dn5
                bsr     scroll_delay
                movem.l (SP)+,D0-A3
                rts
                ENDPART

********************************************************************************
* Screen from D0 to scroll up a line                                           *
********************************************************************************
                PART 'scroll_up2'
scroll_up2:     movem.l D0-A3,-(SP)
                bsr     clr_maus
                lea     debugger_scr(A4),A0
                movea.l scr_adr(A0),A0
                lea     32000-1280(A0),A1
                move.w  D0,D2
                add.w   upper_line(A4),D2
                mulu    #1280,D2
                adda.w  D2,A0
                cmpa.l  A1,A0
                beq.s   scr_upf
scr_upa:        move.l  1280(A0),(A0)+
                move.l  1280(A0),(A0)+
                move.l  1280(A0),(A0)+
                move.l  1280(A0),(A0)+
                cmpa.l  A1,A0
                bls.s   scr_upa
scr_upf:        moveq   #79,D1
scr_upb:        clr.l   (A0)+
                clr.l   (A0)+
                clr.l   (A0)+
                clr.l   (A0)+
                dbra    D1,scr_upb
                lea     screen(A4),A0
                adda.w  upper_offset(A4),A0
                lea     screen+2000(A4),A1
                mulu    #80,D0
                adda.w  D0,A0
                cmpa.l  A1,A0
                beq.s   scr_upe
scr_upc:        move.l  80(A0),(A0)+
                move.l  80(A0),(A0)+
                move.l  80(A0),(A0)+
                move.l  80(A0),(A0)+
                cmpa.l  A1,A0
                bne.s   scr_upc
scr_upe:        move.l  #'    ',D1
                moveq   #4,D0
scr_upd:        move.l  D1,(A0)+
                move.l  D1,(A0)+
                move.l  D1,(A0)+
                move.l  D1,(A0)+
                dbra    D0,scr_upd
                bsr     set_maus
                movem.l (SP)+,D0-A3
                rts
                ENDPART

********************************************************************************
* Screen from D0 to scroll down a line                                         *
********************************************************************************
                PART 'scroll_dn2'
scroll_dn2:     movem.l D0-A3,-(SP)
                bsr     clr_maus
                lea     debugger_scr(A4),A0
                movea.l scr_adr(A0),A0
                move.w  D0,D1
                mulu    #1280,D1
                move.w  upper_offset(A4),D2
                lsl.w   #4,D2
                lea     0(A0,D2.w),A1
                adda.w  D1,A1
                lea     32000-1280(A0),A0
                cmpa.l  A1,A0
                beq.s   scr_dne
scr_dna:        move.l  -(A0),1280(A0)
                move.l  -(A0),1280(A0)
                move.l  -(A0),1280(A0)
                move.l  -(A0),1280(A0)
                cmpa.l  A1,A0
                bgt.s   scr_dna
scr_dne:        moveq   #79,D1
scr_dnb:        clr.l   (A0)+
                clr.l   (A0)+
                clr.l   (A0)+
                clr.l   (A0)+
                dbra    D1,scr_dnb
                lea     screen(A4),A0
                adda.w  upper_offset(A4),A0
                mulu    #80,D0
                lea     0(A0,D0.w),A1
                lea     screen+2000(A4),A0
                cmpa.l  A1,A0
                bls.s   scr_dnf
scr_dnc:        move.l  -(A0),80(A0)
                move.l  -(A0),80(A0)
                move.l  -(A0),80(A0)
                move.l  -(A0),80(A0)
                cmpa.l  A1,A0
                bne.s   scr_dnc
scr_dnf:        move.l  #'    ',D1
                moveq   #4,D0
scr_dnd:        move.l  D1,(A0)+
                move.l  D1,(A0)+
                move.l  D1,(A0)+
                move.l  D1,(A0)+
                dbra    D0,scr_dnd
                bsr     set_maus
                movem.l (SP)+,D0-A3
                rts
                ENDPART

********************************************************************************
* High and downscrew with dump / ...                                           *
********************************************************************************
                PART 'scrollup'
scrollup:       movem.l D0-A6,-(SP)
                cmpi.b  #$50,direct(A4)             ;Cursor down pressed?
                bne.s   scruf
                clr.b   direct(A4)
                lea     screen+1920(A4),A1
                move.w  down_lines(A4),D7
                subq.w  #1,D7
scru1:          movea.l A1,A0
                jsr     @get_line(A4)               ;Get the base address (if available)
                move.b  D0,D6                       ;IDNT_CHAR brands
                cmp.b   #'.',D6
                beq.s   fnd_up2                     ;dumpW/l
                cmp.b   #',',D6
                beq.s   fnd_up1                     ;dump
                cmp.b   #'!',D6
                beq     fnd_up4                     ;Disassemble (symbolic)
                cmp.b   #')',D6
                beq     fnd_up5                     ;asciiDump
                cmp.b   #'/',D6
                beq     fnd_up6                     ;disassemble (normal)
                cmp.b   #'(',D6
                beq     fnd_up7                     ;Symbolica
                cmp.b   #'&',D6
                beq     fnd_up8                     ;sourcetext
scru2:          lea     -80(A1),A1
                dbra    D7,scru1                    ;NIX go in the line
                bra.s   scrue
scruf:          bsr     scroll_up
scrue:          movem.l (SP)+,D0-A6
                rts
fnd_up1:        adda.w  def_size(A4),A6             ;New initial address
                move.l  A6,default_adr(A4)
                move.w  down_lines(A4),zeile(A4)
                moveq   #0,D3
                jsr     cmd_dump7                   ;Output hex data (+ ASCII)
                bsr     scroll_up
                move.w  down_lines(A4),zeile(A4)
                subq.w  #1,zeile(A4)
                move.w  #10,spalte(A4)
                bra.s   scrue
fnd_up2:        bsr     getb
                moveq   #1,D3                       ;Width in Bytes -1 (default for Word)
                cmp.b   #'W',D0
                beq.s   fnd_up3
                cmp.b   #'L',D0
                bne.s   scru2                       ;That was probably nothing
                moveq   #3,D3                       ;3 bytes + 1 = width
fnd_up3:        adda.w  def_size(A4),A6             ;New initial address
                move.l  A6,default_adr(A4)
                move.w  down_lines(A4),zeile(A4)
                jsr     cmd_dump7                   ;Output line
                bsr     scroll_up
                move.w  down_lines(A4),zeile(A4)
                subq.w  #1,zeile(A4)
                move.w  #10,spalte(A4)
                bra.s   scrue

fnd_up4:        st      list_flg(A4)                ;symbolic output
fnd_up6:        move.l  A6,D1
                addq.l  #1,D1
                andi.b  #$FE,D1                     ;Address now
                movea.l D1,A6
                move.l  A3,-(SP)
                bsr     get_dlen                    ;Length of opcodes in the last line
                movea.l (SP)+,A3
                move.w  down_lines(A4),zeile(A4)
                bsr     do_disass                   ;Opcode disassemble and output
                bne     scrue                       ;Illegal RAM area, no scroll
                move.l  A6,default_adr(A4)          ;Here begins the next opcode
                bsr     scroll_up
                move.w  down_lines(A4),zeile(A4)
                subq.w  #1,zeile(A4)
                move.w  #10,spalte(A4)
                sf      list_flg(A4)
                bra     scrue

fnd_up5:        lea     64(A6),A6                   ;New initial address
                move.l  A6,default_adr(A4)
                move.w  down_lines(A4),zeile(A4)
                jsr     asc_out                     ;Output ASCII data
                bsr     scroll_up
                move.w  down_lines(A4),zeile(A4)
                subq.w  #1,zeile(A4)
                move.w  #10,spalte(A4)
                bra     scrue
fnd_up7:        lea     14(A6),A6
                cmpa.l  sym_end(A4),A6
                bhs     scrue                       ;End reached => Ignore symbol table
                move.w  down_lines(A4),zeile(A4)
                clr.w   spalte(A4)
                jsr     sym_out
                bsr     scroll_up
                move.w  down_lines(A4),zeile(A4)
                subq.w  #1,zeile(A4)
                move.w  #10,spalte(A4)
                bra     scrue

fnd_up8:        moveq   #4,D6                       ;Get 5 digits (line number)
                moveq   #0,D7
fnd_up9:        bsr     getb                        ;Pick up
                sub.b   #'0',D0
                bmi     scru2                       ;do not scroll
                cmp.b   #10,D0
                bhs     scru2                       ;do not scroll
                ext.w   D0
                mulu    #10,D7
                add.w   D0,D7
                dbra    D6,fnd_up9
                movea.l ass_vector(A4),A5
                move.l  A5,D0
                beq     scru2                       ;do not scroll
                move.w  D7,D0                       ;Line number
                addq.w  #1,D0
                beq     scrue
                move.w  down_lines(A4),zeile(A4)    ;Scroll SourceText
                clr.w   spalte(A4)
                jsr     src_out                     ;Output line
                move    SR,D0
                move.w  down_lines(A4),zeile(A4)
                subq.w  #1,zeile(A4)
                move.w  #10,spalte(A4)
                move    D0,CCR
                bne     scrue                       ;out Of Source
                bsr     scroll_up
                bra     scrue
                ENDPART

********************************************************************************

                PART 'scrolldwn'
scrolldwn:      movem.l D0-A6,-(SP)
                cmpi.b  #$48,direct(A4)             ;Cursor up pressed?
                bne.s   scrdf
                clr.b   direct(A4)
                lea     screen(A4),A1
                move.w  upper_offset(A4),D7
                adda.w  D7,A1
                move.w  down_lines(A4),D7
                subq.w  #1,D7
scrd1:          movea.l A1,A0
                jsr     @get_line(A4)               ;Get the base address (if available)
                move.b  D0,D6                       ;IDNT_CHAR brands
                cmp.b   #'.',D6
                beq.s   fnd_dwn2                    ;dumpW/l
                cmp.b   #',',D6
                beq.s   fnd_dwn1                    ;dump
                cmp.b   #'!',D6
                beq     fnd_dwn4                    ;Disassemble (symbolic)
                cmp.b   #')',D6
                beq     fnd_dwn0                    ;asciiDump
                cmp.b   #'/',D6
                beq.s   fnd_dwn5                    ;disassemble (normal)
                cmp.b   #'(',D6
                beq     fnd_dwn6                    ;Symbolica
                cmp.b   #'&',D6
                beq     fnd_dwn100                  ;sourcetext
scrd2:          lea     80(A1),A1
                dbra    D7,scrd1                    ;NIX go in the line
                bra.s   scrde
scrdf:          bsr     scroll_dn
scrde:          movem.l (SP)+,D0-A6
                rts

fnd_dwn1:       suba.w  def_size(A4),A6             ;New initial address
                move.l  A6,default_adr(A4)
                clr.w   zeile(A4)
                bsr     scroll_dn
                moveq   #0,D3
                jsr     cmd_dump7                   ;Output hex data (+ ASCII)
                move.w  #10,spalte(A4)
                bra.s   scrde
fnd_dwn2:       bsr     getb
                moveq   #1,D3                       ;Width in Bytes -1 (default for Word)
                cmp.b   #'W',D0
                beq.s   fnddwn3
                cmp.b   #'L',D0
                bne.s   scrd2                       ;That was probably nothing
                moveq   #3,D3                       ;3 bytes + 1 = width
fnddwn3:        suba.w  def_size(A4),A6             ;New initial address
                move.l  A6,default_adr(A4)
                clr.w   zeile(A4)
                bsr     scroll_dn
                jsr     cmd_dump7                   ;Output line
                move.w  #10,spalte(A4)
                bra.s   scrde
fnd_dwn4:       st      list_flg(A4)                ;Symbolically shut off
fnd_dwn5:       move.l  A6,D2
                addq.l  #1,D2
                and.b   #$FE,D2                     ;Address now
                moveq   #30,D5
fnddwn8:        movea.l D2,A6
                suba.w  D5,A6
                tst.l   basep(A4)                   ;Loading program?
                beq.s   fnddwn80                    ;Next, if not
                movea.l basep(A4),A0                ;Get basepage address
                cmpa.l  A0,A6                       ;disassemblerPointer
                blo.s   fnddwn80                    ;<BasePage => Make nothing
                movea.l 8(A0),A0                    ;textSegmentStart
                cmpa.l  A0,A6
                bhs.s   fnddwn80                    ;> = Text Segment Start => Make nothing
                movea.l A0,A6                       ;otherwise: address = text segment start
fnddwn80:       move.l  A6,D0
                bpl.s   fnddwn6
                suba.l  A6,A6
fnddwn6:        movea.l A6,A0
                movem.l D2/D5/A3,-(SP)
                bsr     get_dlen                    ;Length of opcodes in the first line
                move    SR,D0
                movem.l (SP)+,D2/D5/A3
                cmpa.l  D2,A6                       ;Still too small?
                blo.s   fnddwn6                     ;Continue disassemble
                beq.s   fnddwn7                     ;fits!
fnddwn9:        subq.w  #2,D5                       ;Try a little less
                bhi.s   fnddwn8
                movea.l D2,A0
                subq.l  #2,A0                       ;It's not better
                bra.s   fnddwn5
fnddwn7:        tst.w   D0
                beq.s   fnddwn9                     ;Error during disassemble
fnddwn5:        movea.l A0,A6
                bsr     check_read                  ;Access possible?
                bne     scrde                       ;No scrolling, if not.
                clr.w   zeile(A4)
                move.l  A6,default_adr(A4)          ;Here begins the next opcode
                bsr     scroll_dn
                bsr     do_disass                   ;Opcode disassemble and output
                move.w  #10,spalte(A4)
                sf      list_flg(A4)                ;Symbolically
                bra     scrde
fnd_dwn0:       lea     -64(A6),A6                  ;New initial address
                move.l  A6,default_adr(A4)
                clr.l   zeile(A4)
                bsr     scroll_dn
                jsr     asc_out                     ;Output ASCII data
                move.w  #10,spalte(A4)
                bra     scrde

fnd_dwn6:       lea     -14(A6),A6
                cmpa.l  sym_adr(A4),A6
                blo     scrde                       ;Enter the beginning => Ignore symbol table
                clr.l   zeile(A4)
                bsr     scroll_dn
                jsr     sym_out
                move.w  #10,spalte(A4)
                bra     scrde

fnd_dwn100:     moveq   #4,D6                       ;Get 5 digits (line number)
                moveq   #0,D7
fnd_dwn101:     bsr     getb                        ;Pick up
                sub.b   #'0',D0
                bmi     scrd2                       ;do not scroll
                cmp.b   #10,D0
                bhs     scrd2                       ;do not scroll
                ext.w   D0
                mulu    #10,D7
                add.w   D0,D7
                dbra    D6,fnd_dwn101
                movea.l ass_vector(A4),A5
                move.l  A5,D0
                beq     scrd2                       ;do not scroll
                move.w  D7,D0                       ;Line number
                subq.w  #1,D0
                blo     scrde                       ;Line number -1?=> out!
                clr.l   zeile(A4)
                bsr     scroll_dn
                jsr     src_out                     ;Output line
                move.w  #10,spalte(A4)
                bra     scrde
                ENDPART

********************************************************************************

                PART 'get_line'
get_line:       moveq   #19,D0
get_line1:      cmpi.l  #'    ',(A0)+
                dbne    D0,get_line1
                bne.s   get_line3
get_line2:      moveq   #0,D0                       ;empty line
                rts
get_line3:      subq.l  #4,A0
                bsr     getb                        ;Get characters from the input bug
                beq.s   get_line8                   ;Empty input
                moveq   #$10,D2                     ;default number basis
                cmp.b   #'0',D0
                blo.s   get_line4                   ;nix
                cmp.b   #'9',D0                     ;Sign a number?
                bls.s   get_line5                   ;yes
get_line4:      jsr     numbas                      ;Evaluate number basis
                bmi.s   get_line7                   ;No, no number!
                move.w  D3,D2                       ;Set new number basis
                bsr     getb
get_line5:      jsr     chkval                      ;Lfd. Sign valid?
                bhs.s   get_line2                   ;No, nothing found
                moveq   #0,D1                       ;Preject of D1
                pea     get_line6(PC)
                movem.l D2-D7/A1-A6,-(SP)
                jmp     w_zahlj                     ;Read the number
get_line6:      movea.l D1,A6                       ;Remember address
get_line7:      cmp.b   #'>',D0
                beq.s   get_line8
                cmp.b   #'',D0
                beq.s   get_line8
                cmp.b   #'0',D0
                bne.s   get_line9
get_line8:      bsr     getb
get_line9:      tst.b   D0
                rts
                ENDPART

********************************************************************************
* Screen after line start addresses = Browse PC                                *
********************************************************************************
                PART 'hunt_pc'
hunt_pc:        movem.l D0-D6/A0-A6,-(SP)
                move.l  merk_pc(A4),D0
                cmp.l   _pc(A4),D0
                beq     hunt_p3
                move.l  _pc(A4),merk_pc(A4)
                moveq   #-1,D7
                move.l  zeile(A4),-(SP)
                lea     screen(A4),A1
                move.w  upper_offset(A4),D6
                adda.w  D6,A1
                clr.w   zeile(A4)                   ;Line = 0
                move.w  down_lines(A4),D6           ;20 lines
                subq.w  #1,D6
hunt_p1:        movea.l A1,A0
                cmpi.b  #'$',(A0)+
                bne.s   hunt_p4
                moveq   #0,D1
                moveq   #7,D2                       ;Get 8 hex digits
hunt_p6:        move.b  (A0)+,D0
                sub.b   #'0',D0
                cmp.b   #9,D0
                bls.s   hunt_p5
                subq.b  #7,D0
                cmp.b   #15,D0
                bls.s   hunt_p5
                sub.b   #32,D0
hunt_p5:        tst.b   D0
                bmi.s   hunt_p4
                cmp.b   #15,D0
                bhi.s   hunt_p4
                lsl.l   #4,D1
                or.b    D0,D1
                dbra    D2,hunt_p6
                cmpi.b  #'>',(A0)
                beq.s   hunt_p7
                cmpi.b  #'',(A0)
                beq.s   hunt_p7
                cmpi.b  #'0',(A0)
                bne.s   hunt_p4
hunt_p7:        bsr.s   line_char
                cmp.b   #'>',D0
                beq.s   hunt_p2
                move.w  D6,D7
                neg.w   D7
                add.w   down_lines(A4),D7
                subq.w  #1,D7
hunt_p2:        move.w  #9,spalte(A4)
                jsr     @chrout(A4)
                bra.s   hunt_p8
hunt_p4:        ori.w   #$8000,D7
hunt_p8:        addq.w  #1,zeile(A4)
                lea     80(A1),A1
                dbra    D6,hunt_p1                  ;NIX go in the line
                move.l  (SP)+,zeile(A4)
hunt_p3:        movem.l (SP)+,D0-D6/A0-A6
                rts
                ENDPART

********************************************************************************
* Spend address on the beginning of the line (in D1)                           *
********************************************************************************
                PART 'anf_adr'
anf_adr:        jsr     hexa2out                    ;Output the default address
                bsr.s   line_char                   ;Determine characters at the beginning of the line
                jmp     @chrout(A4)                 ;and spend
                ENDPART

********************************************************************************
* Determine characters at the beginning of the line                            *
********************************************************************************
                PART 'line_char'
line_char:      moveq   #'>',D0
                cmp.l   _pc(A4),D1                  ;PC in this line?
                bne.s   line_char8                  ;No!=>
                moveq   #'0',D0                     ;Highlight PC
                movem.l D1-D2/A0-A2,-(SP)
                movea.l $08.w,A1
                movea.l SP,A2
                move.l  #line_char7,$08.w
                movea.l D1,A0                       ;Pointer to the PC
                move.b  (A0),D2                     ;Get the top 4 bits of the opcodes
                lsr.b   #4,D2
                subq.b  #5,D2                       ;Scc <ea> or DBcc Dn,<label>
                beq.s   line_char1
                subq.b  #1,D2                       ;Bcc <label>
                bne.s   line_char7                  ;No more meaningful opcode =>
                moveq   #$0F,D2
                and.b   (A0),D2                     ;condition-mask
                cmp.b   #1,D2
                bhi.s   line_char2                  ;Condition Test
                bra.s   line_char9                  ;BRA or BSR => out
line_char1:     move.w  #$F0C0,D2
                and.w   (A0),D2
                cmp.w   #$50C0,D2
                bne.s   line_char7                  ;kein Scc <ea> or DBcc Dn,<label> =>
                moveq   #$0F,D2
                and.b   (A0),D2                     ;Condition-mask
line_char2:     lea     anf_adr_tab(PC),A0
line_char3:     tst.b   D2                          ;Position reached?
                beq.s   line_char5                  ;Yes!=>
line_char4:     tst.b   (A0)+                       ;Skip entry
                bpl.s   line_char4
                subq.b  #1,D2                       ;and count Condition
                bra.s   line_char3
line_char5:     move.b  (A0)+,D2                    ;The CCR mask
                and.w   _sr(A4),D2                  ;The CCR tab
line_char6:     move.b  (A0)+,D1
                bmi.s   line_char7                  ;Condition is not fulfilled!
                cmp.b   D2,D1                       ;Conditions met?
                bne.s   line_char6                  ;No!=>
line_char9:     moveq   #'',D0                     ;Condition is fulfilled!
line_char7:     move.l  A1,$08.w
                movea.l A2,SP
                movem.l (SP)+,D1-D2/A0-A2
line_char8:     rts

;               SR-Maske,Ergebnis{,Ergebnis},-1
anf_adr_tab:    DC.B $00,$00,-1                     ;0-T  : 1
                DC.B $01,$02,-1                     ;1-F  : 0
                DC.B $05,$00,-1                     ;2-HI : /C and /Z
                DC.B $05,$01,$04,-1                 ;3-LS : C or Z
                DC.B $01,$00,-1                     ;4-CC : /C
                DC.B $01,$01,-1                     ;5-CS : C
                DC.B $04,$00,-1                     ;6-NE : /Z
                DC.B $04,$04,-1                     ;7-EQ : Z
                DC.B $02,$00,-1                     ;8-VC : /V
                DC.B $02,$02,-1                     ;9-VS : V
                DC.B $08,$00,-1                     ;A-PL : /N
                DC.B $08,$08,-1                     ;B-MI : N
                DC.B $0A,$0A,$00,-1                 ;C-GE : N and V or /N and /V
                DC.B $0A,$08,$02,-1                 ;D-LT : N and /V or /N and V
                DC.B $0E,$0A,$00,-1                 ;E-GT : N and V and /Z or /N and /V and /Z
                DC.B $0E,$04,$08,$02,-1             ;F-LE : Z or N and /V or /N and V
                EVEN
                ENDPART

********************************************************************************
* String from A0, wait for yes or no                                           *
********************************************************************************
                PART 'ask_user'
ask_user:       movem.l D0-A6,-(SP)
                move.l  A0,-(SP)
                jsr     @print_line(A4)
                jsr     @c_eol(A4)                  ;Delete line residue
                move.w  spalte(A4),D7               ;Remember
ask_user1:      bsr     c_bell                      ;bell
                bsr     cursor_on                   ;Turn on the cursor
ask_user2:      jsr     @conin(A4)                  ;Get key code
                beq.s   ask_user2                   ;Button has been pressed
                jsr     @cursor_off(A4)             ;Switch off the cursor
                jsr     @chrout(A4)                 ;Spend characters
                move.w  D7,spalte(A4)               ;Cursor back
                bclr    #5,D0                       ;in capital letters

                SWITCH language
                CASE 0
                cmp.b   #'J',D0                     ;Yes
                CASE 1
                cmp.b   #'Y',D0                     ;yeah!
                ENDS

                beq.s   ask_user3                   ;That's probably
                cmp.b   #'N',D0
                bne.s   ask_user1                   ;That was probably the wrong button
                jsr     @crout(A4)
                jmp     (A4)
ask_user3:      jsr     @crout(A4)
                movem.l (SP)+,D0-A6
                rts
                ENDPART

********************************************************************************
* Insert/Overwrite Show                                                        *
********************************************************************************
                PART 'set_ins_flag'
set_ins_flag:   lea     8*14+menu+2(PC),A0          ;Note zero word (+2!)
                tst.b   ins_mode(A4)                ;insert Mode on?
                bne.s   set_ins_flag1               ;Yes!=>

                SWITCH language
                CASE 0
                move.l  #'Over',(A0)+
                move.l  #'wrt ',(A0)
                bra.s   set_ins_flag2
set_ins_flag1:  move.l  #'Inse',(A0)+
                move.l  #'rt  ',(A0)

                CASE 1
                move.l  #'Over',(A0)+
                move.l  #'wrt ',(A0)
                bra.s   set_ins_flag2
set_ins_flag1:  move.l  #'Inse',(A0)+
                move.l  #'rt  ',(A0)
                ENDS

set_ins_flag2:  bsr.s   draw_menu
                move.w  #-1,entry(A4)               ;Delete old entry
                rts
                ENDPART

********************************************************************************
* Menu item Deselect                                                           *
********************************************************************************
                PART 'draw_menu'
draw_menu:      movem.l D0-A6,-(SP)
                bsr     clr_maus
                moveq   #0,D0
                moveq   #0,D3
                pea     menu(PC)                    ;New menu
                bsr     print_inv_line
                moveq   #1,D0
                moveq   #-1,D3
                pea     menu2(PC)
                bsr     print_inv_line
                move.w  D3,entry_old(A4)            ;Delete old entry
                move.w  D3,entry(A4)                ;Delete old entry
                bsr     set_maus
                movem.l (SP)+,D0-A6
                rts
                ENDPART

********************************************************************************
* Spring bar of the menu functions                                             *
********************************************************************************
                PART 'menu'
                DXSET 8,' '

                SWITCH language
                CASE 0
menu:           DX.B 'Trace'
                DX.B 'Do PC'
                DX.B 'Tracrts'
                DX.B 'Ttraps'
                DX.B 'Skip PC'
                DX.B 'Source'
                DX.B 'Hexdump'
                DX.B 'Disassm'
                DX.B 'List'
                DX.B 'Switch'
                DC.W 0
menu2:          DX.B 'Tr68020'
                DX.B 'Tnosubs'
                DX.B 'Tracrte'
                DX.B 'Go'
                DX.B 'xxxxxxxx'
                DX.B 'Marker'
                DX.B 'Breakp'
                DX.B 'Info'
                DX.B 'Direct'
                DX.B 'Quit'

                CASE 1
menu:           DX.B 'Trace'
                DX.B 'Do PC'
                DX.B 'Tracrts'
                DX.B 'Ttraps'
                DX.B 'Skip PC'
                DX.B 'Source'
                DX.B 'Hexdump'
                DX.B 'Disassm'
                DX.B 'List'
                DX.B 'Switch'
                DC.W 0
menu2:          DX.B 'Tr68020'
                DX.B 'Tnosubs'
                DX.B 'Tracrte'
                DX.B 'Go'
                DX.B 'xxxxxxxx'
                DX.B 'Marker'
                DX.B 'Breakp'
                DX.B 'Info'
                DX.B 'Direct'
                DX.B 'Quit'
                ENDS

                DC.B 0
                EVEN
                ENDPART

********************************************************************************
* Menu item Deselect                                                           *
********************************************************************************
                PART 'desel_menu'
desel_menu:     move.w  entry_old(A4),D0            ;Nothing selects?
                bmi.s   desel_menu1                 ;then end
                lea     iorec_IKBD(A4),A0
                move.w  4(A0),D1
                cmp.w   6(A0),D1
                bne.s   desel_menu1                 ;Cancel when a button pressed
                move.w  #-1,entry_old(A4)           ;Delete old entry
                moveq   #-1,D7                      ;deselect
                bsr.s   sel_menu
                moveq   #-1,D0
                move.w  D0,entry_old(A4)            ;Delete old entry
                move.w  D0,entry(A4)                ;Delete current entry
desel_menu1:    rts
                ENDPART

********************************************************************************
* Invert menu item (Nummer in D0=1-n)                                          *
********************************************************************************
                PART 'sel_menu'
sel_menu:       movem.l D0-D4/A0,-(SP)
                move.w  entry_old(A4),D1            ;Nothing selects?
                bmi.s   sel_menu1                   ;then end
                cmp.w   D1,D0
                beq.s   sel_menu5                   ;Still the old entry?
                move.w  D1,D0
                move.w  #-1,entry_old(A4)           ;Delete old entry
                moveq   #-1,D7
                bsr.s   sel_menu                    ;deselect
sel_menu1:      movem.l (SP),D0-D4/A0
                move.w  D0,entry_old(A4)            ;Selected entry
                move.w  D0,entry(A4)
                subq.w  #1,D0
                lea     menu(PC),A0
                moveq   #0,D1                       ;Line 0
                cmp.w   #9,D0                       ;Entry> 9
                bls.s   sel_menu3                   ;No!
                lea     menu2(PC),A0
                moveq   #1,D1                       ;line 1
                sub.w   #10,D0
sel_menu3:      lsl.w   #3,D0                       ;8 (width of an entry)
                move.l  zeile(A4),-(SP)             ;Line and (!) Save column
                move.w  D1,zeile(A4)
                move.w  D0,spalte(A4)
                adda.w  D0,A0                       ;Pointer on the string
                bsr     clr_maus                    ;Switch off the mouse
                moveq   #0,D3
                moveq   #-1,D1
                move.w  D7,D2
                tst.w   D1                          ;In line 0
                beq.s   sel_menu6                   ;underline
                moveq   #-1,D3
sel_menu6:      moveq   #7,D4
sel_menu4:      moveq   #0,D0
                move.b  (A0)+,D0
                bsr     light_char                  ;Invert 8 characters
                addq.w  #1,spalte(A4)
                dbra    D4,sel_menu4
                bsr     set_maus                    ;The mouse is allowed again
                move.l  (SP)+,zeile(A4)
sel_menu5:      movem.l (SP)+,D0-D4/A0
                rts
                ENDPART

********************************************************************************
* form_do run from A0                                                          *
********************************************************************************
                PART 'form_do'
form_do:        movem.l D1-A6,-(SP)
                move.l  zeile(A4),-(SP)
                addq.b  #1,set_lock(A4)             ;Prevent cursoring in the VBL
                st      no_dklick(A4)               ;No double click
                move.w  curflag(A4),-(SP)
                bsr     cursor_off
                suba.l  A5,A5                       ;Current button>
                movea.l A0,A1
                bsr     desel_abuttons              ;All the exit-buttons desectic
                tst.b   8(A1)                       ;Only a redfraw?
                bmi.s   formddx
                move.b  #1,9(A1)                    ;Clear background
formddx:        bsr     objc_draw                   ;Tree trees
                move.b  #2,9(A1)                    ;at redraw only the frame
                bra.s   form_d0
form_d7:        bsr     c_bell                      ;bell
form_d0:        btst    #1,maustast(A4)             ;Left button pressed?
                beq.s   form_d1                     ;No!
                clr.b   mausprell(A4)               ;note that it is pressed
                bsr     find_button                 ;Mouse over a button?
form_button:    bne.s   formd8a                     ;No!
                btst    #5,9(A5)                    ;exitButton?
                beq.s   formd8b                     ;Yes!
                btst    #0,9(A5)                    ;Selected?
                bne.s   formd8b                     ;everything OK
                bsr     desel_abuttons              ;exitButtonsDeselektieren
                suba.l  A5,A5                       ;No more current button
formd8b:        cmpa.l  A0,A5                       ;Still the same button?
                beq.s   form_d0                     ;then ignore
                btst    #6,9(A0)                    ;radioButton?
                bne.s   formd8c                     ;Yes!=> Special treatment
                bchg    #0,9(A0)                    ;Button Select / Deselect
formd88:        movea.l A0,A5                       ;Remember button number
                movea.l A1,A0                       ;Pointer on the object tree
                bsr     objc_draw                   ;New tree
                bra.s   form_d0
formd8a:        bsr     desel_abuttons              ;exitButtonsDeselektieren
                suba.l  A0,A0
                move.l  A5,D0                       ;No current buttons there?
                beq.s   form_d0                     ;then not red
                bra.s   formd88
formd8c:        moveq   #0,D0
                move.b  8(A0),D0                    ;The radio button number
                movem.l A0,-(SP)
                movea.l A1,A0
form8d:         tst.w   (A0)                        ;Tree end?
                bmi.s   form8e                      ;Yes!
                lea     10(A0),A0                   ;Pointer on next object
                btst    #6,-1(A0)                   ;radioButton?
                beq.s   form8d                      ;No?=> Do not deselect
                cmp.b   -2(A0),D0                   ;Is it the ER.Radio button?
                bne.s   form8d                      ;No?=> Do not deselect
                bclr    #0,-1(A0)                   ;deselectButton
                bra.s   form8d                      ;Next
form8e:         movem.l (SP)+,A0
                bset    #0,9(A0)                    ;Select button
                bra.s   formd88

form_d1:        tas.b   mausprell(A4)               ;Was the key released?
                bne.s   form_d2                     ;NO => END
                bsr     find_button                 ;Button under the mouse?
                beq.s   formd1a                     ;Yes => possiblyexit
                bsr     desel_abuttons              ;exitButtonsDeselektieren
                movea.l A1,A0                       ;Pointer on the object tree
                bsr     objc_draw                   ;New tree
formd1b:        move.l  A5,D0
                beq     form_d7                     ;No button was selected
                bra     form_d0                     ;Go on
formd1a:        btst    #5,9(A5)                    ;exitButton?
                beq.s   formd1b                     ;No => Continue
                bra     form_exit                   ;end

form_d2:        suba.l  A5,A5                       ;Current button>
                bsr     conin
                beq     form_d0                     ;No button pressed
                moveq   #27,D1
                btst    D1,D0                       ;Wheel pressed?
                beq.s   form_d20                    ;No!=>
                cmp.w   #'A',D0
                blo.s   form_d20                    ;smaller than 'a', DAT does not work
                cmp.w   #'Z',D0
                bls.s   form_d24                    ;bigger than 'z' does not work either
                cmp.w   #'a',D0
                blo.s   form_d20                    ;Small letters go too
                cmp.w   #'z',D0
                bhi.s   form_d20
                sub.w   #32,D0
form_d24:       sub.w   #'A',D0
                moveq   #0,D2                       ;Button No. Clear
                movea.l A1,A0
find_d21:       tst.w   (A0)
                bmi.s   find_d22                    ;Nothing found
                lea     10(A0),A0                   ;Next entry (flags unaffected)
                btst    #2,-1(A0)                   ;button?
                beq.s   find_d21                    ;No button
                addq.w  #1,D2                       ;Increase Button No.
                dbra    D0,find_d21
                btst    #4,-1(A0)                   ;disabled?
                bne.s   find_d22                    ;Yes, then end
                btst    #5,-1(A0)                   ;exitButton?
                bne     form_d34                    ;then out
                lea     -10(A0),A0
                move    #$FF,CCR                    ;Button found
                bra.s   form_d23
find_d22:       moveq   #0,D2                       ;No button selected
                move    #0,CCR
form_d23:       bra     form_button

form_d20:       bsr     cursor_off
                cmp.w   #13,D0                      ;return
                beq.s   form_return                 ;defaultButtonSuchen
                swap    D0
                cmp.b   #$61,D0                     ;undo
                beq.s   form_abort
                bra     form_d7                     ;No edit object there

;Abbruch
form_abort:     jsr     @cursor_off(A4)
                sf      no_dklick(A4)
                jsr     @redraw_all(A4)             ;Rebuild screen
                move.w  (SP)+,curflag(A4)
                btst    #6,curflag(A4)
                beq.s   form_ax12
                bsr     flash_cursor
form_ax12:      sf      no_dklick(A4)               ;Double-click again
                subq.b  #1,set_lock(A4)             ;Cursor may be set again in the VBL
                move.l  (SP)+,zeile(A4)
                movem.l (SP)+,D1-A6
                tst.w   spalte(A4)
                beq.s   formab1                     ;CR, if cursor is not in column 0
                jsr     @crout(A4)
formab1:        jmp     (A4)                        ;then back in the main loop

;Ende
form_exit:      moveq   #0,D0
                bsr.s   redraw_all                  ;Rebuild screen
                move.w  D2,D0
                move.w  (SP)+,curflag(A4)
                btst    #6,curflag(A4)
                beq.s   formx12
                bsr     flash_cursor
formx12:        sf      no_dklick(A4)               ;Double-click again
                subq.b  #1,set_lock(A4)             ;Cursor may be set again in the VBL
                move.l  (SP)+,zeile(A4)
                movem.l (SP)+,D1-A6
                tst.w   D0                          ;Flag
                rts

; Return = Select Default Button + End
form_return:    moveq   #0,D2                       ;Button No. Clear
                movea.l A1,A0
form_d3:        tst.w   (A0)
                bmi.s   form_d33                    ;No DEFAULT button
                lea     10(A0),A0                   ;Pointer on next element
                btst    #2,-1(A0)
                beq.s   form_d3                     ;No button
                addq.w  #1,D2                       ;Increase Button No.
                btst    #1,-1(A0)                   ;defaultButton?
                beq.s   form_d3
                btst    #4,-1(A0)                   ;disabled?
                bne.s   form_d3                     ;I suppose that did not work
form_d34:       bsr     desel_abuttons              ;deselectAllButtons
                bset    #0,-1(A0)
                movea.l A1,A0
                bsr.s   objc_draw                   ;Draw tree again
                bra.s   form_exit                   ;overAndOut
form_d33:       tst.w   D2
                bne     form_d7                     ;No DEFAULT button
                bra.s   form_exit                   ;No button!
                ENDPART

********************************************************************************
* Rebuild screen                                                               *
********************************************************************************
                PART 'redraw_all'
redraw_all:     movem.l D0-A6,-(SP)
                lea     debugger_scr(A4),A0
                tst.b   scr_moni(A0)
                beq.s   redraw_all2                 ;s/wHatPause
                bsr     clr_maus
                movea.l scr_adr(A0),A0              ;For color: 2nd plan
                moveq   #-1,D0
                clr.w   D0                          ;D0 = $ FFFF0000 (2 bytes shorter)
                move.w  #1999,D1
redraw_all1:    and.l   D0,(A0)+
                and.l   D0,(A0)+
                and.l   D0,(A0)+
                and.l   D0,(A0)+
                dbra    D1,redraw_all1
                bsr     set_maus
redraw_all2:    move.l  zeile(A4),-(SP)             ;Line and (!) Save column
                move.w  upper_line(A4),D0
                neg.w   D0
                move.w  D0,zeile(A4)
                clr.w   spalte(A4)
                bsr     draw_menu
                move.w  #-1,entry(A4)               ;Delete old entry
                sf      testwrd(A4)
                sf      device(A4)
                jsr     rgout
                moveq   #0,D0
redraw_all3:    bsr     update_line                 ;Provide all lines
                addq.w  #1,D0
                cmp.w   down_lines(A4),D0
                bne.s   redraw_all3
                move.l  (SP)+,zeile(A4)             ;Line and (!) Column back
                movem.l (SP)+,D0-A6

                rts
                ENDPART

********************************************************************************
* Object draw (ab A0)                                                          *
********************************************************************************
                PART 'objc_draw'
objc_draw:      movem.l D0-A3/A5,-(SP)
                move.l  zeile(A4),-(SP)
                jsr     @cursor_off(A4)
                clr.w   button_nr(A4)
                lea     debugger_scr(A4),A2
                tst.b   scr_moni(A2)
                lea     objc_draw_tab2(PC),A2       ;Hift table for color
                bne.s   objc_draw1                  ;Color Monitor =>
                lea     objc_draw_tab(PC),A2        ;Spring table for b / w
objc_draw1:     movem.w 4(A0),D6-D7
                subi.w  #80,D6
                neg.w   D6
                lsr.w   #1,D6                       ;Center object
                subi.w  #25,D7
                neg.w   D7
                lsr.w   #1,D7
objc_draw2:     moveq   #0,D0
                moveq   #0,D1
                moveq   #0,D2
                moveq   #0,D3
                moveq   #0,D4
                movem.w (A0)+,D0-D4
                tst.w   D0
                bmi.s   objc_draw4
                move.w  D4,D5
                andi.w  #$1F,D5
                add.w   D5,D5
                movea.w 0(A2,D5.w),A3
                cmp.w   #6,D5
                bls.s   objc_draw3
                movea.w (A2),A3
objc_draw3:     adda.l  A2,A3
                add.w   D6,D0
                add.w   D7,D1
                bsr     clr_maus
                jsr     (A3)
                bra.s   objc_draw2
objc_draw4:     bsr     set_maus
                move.l  (SP)+,zeile(A4)
                movem.l (SP)+,D0-A3/A5
                rts

                BASE DC.W,objc_draw_tab
objc_draw_tab:  DC.W objc_draw_text                 ;0 Default
                DC.W objc_draw_bordr                ;1
                DC.W objc_draw_frame                ;2
                DC.W objc_draw_icon                 ;3

                BASE DC.W,objc_draw_tab2
objc_draw_tab2: DC.W objc_drawfftext                ;0 Default
                DC.W objc_drawfbordr                ;1
                DC.W objc_drawfframe                ;2
                DC.W objc_drawficon                 ;3

objc_draw_icon: lsr.w   #8,D4                       ;Outgraduate routines for b / w
                lsl.w   #4,D1                       ;Time 16
                swap    D2
                clr.w   D2
                or.w    D3,D2
                movea.l D2,A3                       ;Icons address
                lea     debugger_scr(A4),A5
                movea.l scr_adr(A5),A5
                move.w  D1,D2
                lsl.w   #2,D2                       ;Times 80
                add.w   D2,D1
                lsl.w   #4,D1
                add.w   D1,D0
                adda.w  D0,A5                       ;Address on the screen
                move.w  D4,D2
                andi.w  #$F0,D2
                lsr.w   #1,D2
                addq.w  #7,D2                       ;Icon height
                andi.w  #$0F,D4
objc_draw_icon1:move.w  D4,D1
                movea.l A5,A1
objc_draw_icon2:move.b  (A3)+,(A1)+
                dbra    D1,objc_draw_icon2
                lea     80(A5),A5
                dbra    D2,objc_draw_icon1
                rts

objc_draw_bordr:movem.w D0-D3,-(SP)
                lsl.w   #3,D0
                lsl.w   #4,D1
                lsl.w   #3,D2
                lsl.w   #4,D3
                subq.w  #4,D0
                subq.w  #4,D1
                addq.w  #8,D2
                addq.w  #8,D3
                bsr     clr_box
                movem.w (SP)+,D0-D3
objc_draw_frame:lsl.w   #3,D0
                lsl.w   #4,D1
                lsl.w   #3,D2
                lsl.w   #4,D3
                subq.w  #1,D0
                subq.w  #1,D1
                addq.w  #1,D2
                addq.w  #1,D3
                bsr     draw_box
                subq.w  #1,D0
                subq.w  #1,D1
                addq.w  #2,D2
                addq.w  #2,D3
                bsr     draw_box
                subq.w  #3,D0
                subq.w  #3,D1
                addq.w  #6,D2
                addq.w  #6,D3
                bra     draw_box

objc_draw_text: swap    D2
                clr.w   D2
                or.w    D3,D2
                move.w  D0,spalte(A4)
                move.w  D1,zeile(A4)
                movea.l D2,A3
                movem.l D0-D3,-(SP)
                btst    #2,D4
                beq.s   objc_draw_text1
                cmpi.b  #' ',(A3)
                bne.s   objc_draw_text1
                addq.l  #1,A3
                moveq   #'A',D0
                add.w   button_nr(A4),D0
                or.w    #$FF00,D0                   ;Put a small letter
                bra.s   objc_draw_text0
objc_draw_text1:moveq   #0,D0
                move.b  (A3)+,D0
                beq.s   objc_draw_text5
objc_draw_text0:moveq   #-1,D1                      ;Delete Light Mask
                moveq   #0,D2                       ;Delete Inverse Mask
                moveq   #0,D3                       ;Unleashine mask
                btst    #0,D4                       ;selected (invers) ?
                beq.s   objc_draw_text2
                moveq   #-1,D2                      ;Inverse
objc_draw_text2:btst    #4,D4                       ;disabled (light) ?
                beq.s   objc_draw_text3
                moveq   #$55,D1                     ;Light
objc_draw_text3:btst    #2,D4                       ;No button?
                bne.s   objc_draw_text9
                btst    #6,D4                       ;Fat?
                beq.s   objc_draw_text9
                or.w    #$0100,D0                   ;Put up
objc_draw_text9:tst.b   D4                          ;Editable?
                bpl.s   objc_draw_text4
                moveq   #-1,D3                      ;underlineAn
objc_draw_text4:bsr     light_char                  ;Spend characters
                addq.w  #1,spalte(A4)               ;Next column
                bra.s   objc_draw_text1

objc_draw_text5:movem.l (SP)+,D0-D3
                btst    #2,D4                       ;text or Button
                beq.s   objc_draw_text8             ;Text without frame
                addq.w  #1,button_nr(A4)
                movea.l D2,A3
                moveq   #-8,D2
objc_draw_text6:addq.l  #8,D2
                tst.b   (A3)+
                bne.s   objc_draw_text6
                lsl.w   #4,D1
                lsl.w   #3,D0
                subq.w  #1,D0
                subq.w  #1,D1
                addq.w  #1,D2
                moveq   #17,D3
                bsr     draw_box
objc_draw_text7:subq.w  #1,D0
                subq.w  #1,D1
                addq.w  #2,D2
                addq.w  #2,D3
                bsr     draw_box
                bclr    #1,D4
                bne.s   objc_draw_text7
objc_draw_text8:rts

objc_drawficon: lsl.w   #4,D1                       ;Outgraduate routines for color
                swap    D2
                clr.w   D2
                or.w    D3,D2
                movea.l D2,A3                       ;Icons address

                move.w  D1,D2
                lsl.w   #2,D2                       ;TIMES 80
                add.w   D2,D1
                lsl.w   #4,D1
                lea     debugger_scr(A4),A1
                movea.l scr_adr(A1),A1
                adda.w  D1,A1                       ;+ Screen address
                move.w  D4,D1
                lsr.w   #8,D4
                move.w  D4,D5
                andi.w  #$F0,D5
                lsr.w   #2,D5
                addq.w  #3,D5                       ;Icon height
                andi.w  #$0F,D4
                tst.b   D1
                bpl.s   objc_drawficon4             ;omit every 2nd line
objc_drawficon1:move.w  D4,D3                       ;output x bytes
objc_drawficon2:move.w  D0,D2
                move.w  D0,D1
                andi.w  #-2,D1
                add.w   D1,D1                       ;+ (spalteAnd2) *2
                andi.w  #1,D2
                add.w   D2,D1                       ;+ (spalteAnd1)
                move.b  (A3)+,0(A1,D1.w)
                addq.w  #1,D0                       ;Next column
                dbra    D3,objc_drawficon2
                sub.w   D4,D0                       ;Column counter Back
                subq.w  #1,D0
                move.w  D4,D3
objc_drawficon3:move.w  D0,D2
                move.w  D0,D1
                andi.w  #-2,D1
                add.w   D1,D1                       ;+ (splitAnd2) *2
                andi.w  #1,D2
                add.w   D2,D1                       ;+ (splitAnd1)
                move.b  (A3)+,D2
                or.b    D2,0(A1,D1.w)
                addq.w  #1,D0                       ;Next column
                dbra    D3,objc_drawficon3
                sub.w   D4,D0                       ;Column counter Back
                subq.w  #1,D0
                lea     160(A1),A1
                dbra    D5,objc_drawficon1
                rts
objc_drawficon4:move.w  D4,D3                       ;output x bytes
objc_drawficon5:move.w  D0,D2
                move.w  D0,D1
                andi.w  #-2,D1
                add.w   D1,D1                       ;+ (splitAnd2) *2
                andi.w  #1,D2
                add.w   D2,D1                       ;+ (splitAnd1)
                move.b  (A3)+,0(A1,D1.w)
                addq.w  #1,D0                       ;Next column
                dbra    D3,objc_drawficon5
                sub.w   D4,D0                       ;Column counter Back
                subq.w  #1,D0
                adda.w  D4,A3                       ;Line over
                addq.l  #1,A3
                lea     160(A1),A1
                dbra    D5,objc_drawficon4
                rts

objc_drawfbordr:movem.w D0-D3,-(SP)
                lsl.w   #3,D0
                lsl.w   #4,D1
                lsl.w   #3,D2
                lsl.w   #4,D3
                subq.w  #4,D0
                subq.w  #8,D1
                addq.w  #8,D2
                addi.w  #14,D3
                bsr     clr_fbox
                movem.w (SP)+,D0-D3
objc_drawfframe:lsl.w   #3,D0
                lsl.w   #4,D1
                lsl.w   #3,D2
                lsl.w   #4,D3
                subq.w  #1,D0
                subq.w  #2,D1
                addq.w  #1,D2
                addq.w  #2,D3
                bsr     drawfbox
                subq.w  #1,D0
                subq.w  #2,D1
                addq.w  #2,D2
                addq.w  #4,D3
                bsr     drawfbox
                subq.w  #3,D0
                subq.w  #6,D1
                addq.w  #6,D2
                addi.w  #12,D3
                bra     drawfbox

objc_drawfftext:swap    D2
                clr.w   D2
                or.w    D3,D2
                move.w  D0,spalte(A4)
                move.w  D1,zeile(A4)
                movea.l D2,A3
                movem.l D0-D2,-(SP)
                btst    #2,D4
                beq.s   objc_drawfftxt1
                cmpi.b  #' ',(A3)
                bne.s   objc_drawfftxt1
                addq.l  #1,A3
                moveq   #'A',D0
                add.w   button_nr(A4),D0
                or.w    #$FF00,D0
                bra.s   objc_drawfftxt0
objc_drawfftxt1:moveq   #0,D0
                move.b  (A3)+,D0
                beq.s   objc_drawfftxt5
objc_drawfftxt0:moveq   #-1,D1                      ;Delete Light Mask
                moveq   #0,D2                       ;Delete Inverse Mask
                moveq   #0,D3                       ;Unleashine mask
                btst    #0,D4                       ;selected (invers) ?
                beq.s   objc_drawfftxt2
                moveq   #-1,D2                      ;Inverse
objc_drawfftxt2:btst    #4,D4                       ;disabled (light) ?
                beq.s   objc_drawfftxt3
                moveq   #$55,D1                     ;Light
objc_drawfftxt3:btst    #2,D4                       ;No button?
                bne.s   objc_drawfftxt9
                btst    #6,D4                       ;Fat?
                beq.s   objc_drawfftxt9
                or.w    #$0100,D0                   ;Mark fat
objc_drawfftxt9:tst.b   D4                          ;Editable?
                bpl.s   objc_drawfftxt4
                moveq   #-1,D3                      ;underline on
objc_drawfftxt4:bsr     light_char
                addq.w  #1,spalte(A4)
                bra.s   objc_drawfftxt1
objc_drawfftxt5:movem.l (SP)+,D0-D2

                btst    #2,D4                       ;text or Button
                beq.s   objc_drawfftxt8             ;Text without frame
                addq.w  #1,button_nr(A4)
                movea.l D2,A3
                moveq   #-8,D2
objc_drawfftxt6:addq.l  #8,D2
                tst.b   (A3)+
                bne.s   objc_drawfftxt6
                lsl.w   #4,D1
                lsl.w   #3,D0
                subq.w  #1,D0
                subq.w  #2,D1
                addq.w  #1,D2
                moveq   #20,D3
                bsr     drawfbox
objc_drawfftxt7:subq.w  #1,D0
                subq.w  #2,D1
                addq.w  #2,D2
                addq.w  #4,D3
                bsr     drawfbox
                bclr    #1,D4
                bne.s   objc_drawfftxt7
objc_drawfftxt8:rts
                ENDPART

********************************************************************************
* Deselect all buttons (D0 = Selected Button)                                  *
********************************************************************************
                PART 'desel_abuttons'
desel_abuttons: movem.l D3/A0,-(SP)
                movea.l A1,A0
                moveq   #0,D3
                moveq   #0,D0
desel_abuttons1:tst.w   (A0)                        ;Tree end?
                bmi.s   desel_abuttons2             ;Yes!
                lea     10(A0),A0                   ;Pointer on next object
                btst    #2,-1(A0)                   ;button?
                beq.s   desel_abuttons1
                addq.w  #1,D3                       ;buttonNr+1
                btst    #5,-1(A0)                   ;exitButton?
                beq.s   desel_abuttons1             ;No => Button not deselect
                btst    #6,-1(A0)                   ;radioButton?
                bne.s   desel_abuttons1             ;Do not deselect
                btst    #0,-1(A0)                   ;Button Selected?
                beq.s   desel_abuttons1             ;No!
                move.w  D3,D0                       ;Button no brands
                bclr    #0,-1(A0)                   ;deselectButton
                bra.s   desel_abuttons1             ;Next
desel_abuttons2:movem.l (SP)+,D3/A0
                rts
                ENDPART

********************************************************************************
* Find button under the mouse (A0 points to the D2 button, flags!)             *
********************************************************************************
                PART 'find_button'
find_button:    move.w  mausx(A4),D0
                move.w  mausy(A4),D1
                lsr.w   #3,D0
                lsr.w   #4,D1                       ;Convert to character coordinates
                movem.w 4(A1),D6-D7                 ;Get width & height
                subi.w  #80,D6
                neg.w   D6
                lsr.w   #1,D6                       ;Center object
                subi.w  #25,D7
                neg.w   D7
                lsr.w   #1,D7
                sub.w   D6,D0
                bmi.s   find_button3
                sub.w   D7,D1                       ;Convert mouse coordinates in offsets
                bmi.s   find_button3
                moveq   #0,D2                       ;Button No. Clear
                movea.l A1,A0
find_button1:   tst.w   (A0)
                bmi.s   find_button3                ;Nothing found
                lea     10(A0),A0                   ;Next entry (flags unaffected)
                btst    #2,-1(A0)                   ;button?
                beq.s   find_button1                ;No button
                addq.w  #1,D2                       ;Increase Button No.
                btst    #4,-1(A0)                   ;disabled?
                bne.s   find_button1                ;Yes, ignore
                move.w  -10(A0),D3                  ;Get X coordinate
                cmp.w   D3,D0
                blo.s   find_button1                ;X too small
                movea.l -6(A0),A2                   ;textadresse
                moveq   #-1,D4
find_button2:   addq.l  #1,D4                       ;TEXTLE ERMITTELN.
                tst.b   (A2)+
                bne.s   find_button2
                add.w   D4,D3                       ;Width
                cmp.w   D3,D0
                bhs.s   find_button1                ;X too big
                move.w  -8(A0),D3                   ;Get Y-coordinate
                cmp.w   D3,D1
                blo.s   find_button1                ;Y too small
                addq.w  #1,D3                       ;Height (because text, always a line)
                cmp.w   D3,D1
                bhs.s   find_button1                ;Y too big
                lea     -10(A0),A0
                move    #$FF,CCR                    ;Button found
                rts
find_button3:   moveq   #0,D2                       ;No button selected
                move    #0,CCR
                rts
                ENDPART

********************************************************************************
* Output character D0 to cursor position (D1 = Lightmask, D2 = XOR value)      *
********************************************************************************
                PART 'light_char'
light_char:     movem.l D0-A1,-(SP)
                move.w  D1,D6
                move.w  D2,D7
                move.w  D3,D5
                move.w  zeile(A4),D2
                move.w  D2,D1
                lsl.w   #2,D1                       ;times 80
                add.w   D1,D2
                lsl.w   #4,D2
                move.w  spalte(A4),D3
                move.w  D0,D4
                lsr.w   #8,D4                       ;Negative.B? => Fat / small
                and.w   #$FF,D0
                lea     debugger_scr(A4),A1
                tst.b   scr_moni(A1)
                bne     light2                      ;Color monitor
                movea.l s_w_font(A4),A0             ;fontadresse
                adda.w  D0,A0                       ;Plus ASCII code (= character ID)
                lsl.w   #4,D2                       ;Curses * 1280.
                add.w   D3,D2                       ;+Cursorspalte
                movea.l scr_adr(A1),A1
                adda.w  D2,A1                       ;+ Screen address
                moveq   #15,D1                      ;Number of lines
                move.w  #$0100,D2                   ;Offset for the character set
                moveq   #80,D3                      ;Offset for the screen
                tst.b   D4                          ;bes.Attribute
                bmi.s   light10                     ;Small?then there
                bne.s   light6                      ;Fat?then there
light1:         move.b  (A0),D0                     ;Get out of the font
                and.b   D6,D0                       ;light
                eor.b   D7,D0                       ;inverse
                tst.w   D1
                bne.s   light4
                or.b    D5,D0                       ;Underline
light4:         move.b  D0,(A1)                     ;On the screen
                adda.w  D2,A0
                adda.w  D3,A1
                rol.b   #1,D6                       ;Rotate the mask
                dbra    D1,light1
                movem.l (SP)+,D0-A1
                rts
light6:         move.b  (A0),D0                     ;Get out of the font
                move.b  D0,D4
                lsr.b   #1,D4                       ;Sign a bit to the left
                or.b    D4,D0                       ;and insert again => fat
                and.b   D6,D0                       ;light
                eor.b   D7,D0                       ;inverse
                tst.w   D1
                bne.s   light7
                or.b    D5,D0                       ;Underline
light7:         move.b  D0,(A1)                     ;On the screen
                adda.w  D2,A0
                adda.w  D3,A1
                rol.b   #1,D6                       ;Rotate the mask
                dbra    D1,light6
                movem.l (SP)+,D0-A1
                rts
light10:        movea.l farbfont(A4),A0             ;fontadresse
                adda.w  D0,A0                       ;Plus ASCII code (= character ID)
                moveq   #7,D1                       ;Number of lines
light11:        move.b  (A0),D0                     ;Get out of the font
                and.b   D6,D0                       ;light
                eor.b   D7,D0                       ;inverse
                move.b  D0,(A1)                     ;On the screen
                lea     256(A0),A0
                adda.w  D3,A1
                rol.b   #1,D6                       ;Rotate the mask
                dbra    D1,light11
                moveq   #7,D1                       ;The remaining 8 lines
light13:        move.b  D7,D0                       ;inverse?
                tst.w   D1
                bne.s   light14
                or.b    D5,D0                       ;Underline
light14:        move.b  D0,(A1)                     ;On the screen
                adda.w  D3,A1
                dbra    D1,light13
                movem.l (SP)+,D0-A1
                rts

light2:         movea.l farbfont(A4),A0
                adda.w  D0,A0                       ;Get address of the sign
                lsl.w   #4,D2                       ;Curses * 1280.
                move.w  D3,D1
                andi.w  #-2,D3
                add.w   D3,D3
                add.w   D3,D2                       ;+ (split and -2) * 2
                andi.w  #1,D1
                add.w   D1,D2                       ;+ (split and 1)
                movea.l scr_adr(A1),A1
                adda.w  D2,A1                       ;+ Screen address
                moveq   #7,D1                       ;Number of lines
                move.w  #$0100,D2                   ;Offset for the character set
                move.w  #160,D3                     ;Offset for the screen
                tst.b   D4
                bmi.s   light20                     ;Ignore
                bne.s   light8                      ;Spend out fat
light3:         move.b  (A0),D0                     ;Get out of the font
                and.b   D6,D0                       ;light
                eor.b   D7,D0                       ;inverse
                tst.w   D1
                bne.s   light5
                or.b    D5,D0                       ;Underline
light5:         move.b  D0,(A1)                     ;On the screen
                adda.w  D2,A0
                adda.w  D3,A1
                rol.b   #1,D6                       ;Rotate the mask
                dbra    D1,light3
                movem.l (SP)+,D0-A1
                rts
light8:         move.b  (A0),D0                     ;Get out of the font
                move.b  D0,D4
                lsr.b   #1,D4                       ;Sign a bit to the left
                or.b    D4,D0                       ;and insert again => fat
                and.b   D6,D0                       ;light
                eor.b   D7,D0                       ;inverse
                tst.w   D1
                bne.s   light9
                or.b    D5,D0                       ;Underline
light9:         move.b  D0,(A1)                     ;On the screen
                adda.w  D2,A0
                adda.w  D3,A1
                rol.b   #1,D6                       ;Rotate the mask
                dbra    D1,light8
                movem.l (SP)+,D0-A1
                rts
light20:        movea.l farbfont(A4),A0             ;fontadresse
                adda.w  D0,A0                       ;Plus ASCII code (= character ID)
                moveq   #3,D1                       ;Number of lines (only 4 pixels high !!!)
light21:        move.b  (A0),D0                     ;Get out of the font
                and.b   D6,D0                       ;light
                eor.b   D7,D0                       ;inverse
                move.b  D0,(A1)                     ;On the screen
                lea     512(A0),A0
                adda.w  D3,A1
                rol.b   #1,D6                       ;Rotate the mask
                dbra    D1,light21
                moveq   #3,D1                       ;The remaining 4 lines
light23:        move.b  D7,D0                       ;inverse?
                tst.w   D1
                bne.s   light24
                or.b    D5,D0                       ;Underline
light24:        move.b  D0,(A1)                     ;On the screen
                adda.w  D3,A1
                dbra    D1,light23
                movem.l (SP)+,D0-A1
                rts
                ENDPART

********************************************************************************
* Frame (b / w) draw (D0-X, D1-Y, D2 width, D3 height)                         *
********************************************************************************
                PART 'draw_box'
draw_box:       movem.l D0-A2,-(SP)
                move.l  D3,D6
                subq.l  #1,D6                       ;Remember height-1
                move.l  D0,D7
                add.w   D0,D2
                add.w   D1,D3                       ;Calculate right lower corner
                lea     debugger_scr(A4),A0
                movea.l scr_adr(A0),A0              ;Screen address
                movea.l A0,A1
                mulu    #80,D1
                mulu    #80,D3
                adda.w  D1,A0                       ;Line address of the upper line
                adda.w  D3,A1                       ;Line address of the lower line

                move.w  D0,D4
                lsr.w   #3,D0
                adda.w  D0,A0
                adda.w  D0,A1
                addq.w  #8,D7                       ;Replace the left edge
                and.w   #$03F8,D7
                and.w   #7,D4
                move.b  draw_box_mask1(PC,D4.w),D5
                bne.s   draw_box1
                moveq   #-1,D5
draw_box1:      or.b    D5,(A0)
                or.b    D5,(A1)
                move.b  draw_box_mask2+1(PC,D4.w),D5
                move.w  D6,D0
                movea.l A0,A2
draw_box2:      or.b    D5,(A2)                     ;Draw left edge
                lea     80(A2),A2
                dbra    D0,draw_box2
                addq.l  #1,A0
                addq.l  #1,A1
                move.w  D2,D4
                sub.w   D7,D2
                bmi.s   draw_box4
                lsr.w   #3,D2
                subq.w  #1,D2
                bmi.s   draw_box4
                moveq   #-1,D5
draw_box3:      move.b  D5,(A0)+                    ;Draw the upper and lower line
                move.b  D5,(A1)+
                dbra    D2,draw_box3
draw_box4:      andi.w  #7,D4
                move.b  draw_box_mask1+1(PC,D4.w),D5
                not.b   D5
                or.b    D5,(A0)
                or.b    D5,(A1)
                move.b  draw_box_mask2+1(PC,D4.w),D5
draw_box5:      lea     80(A0),A0
                or.b    D5,(A0)                     ;Draw right line
                dbra    D6,draw_box5
                movem.l (SP)+,D0-A2
                rts

draw_box_mask1: DC.B %11111111,%1111111,%111111,%11111,%1111,%111
                DC.B %11,%1,%0
draw_box_mask2: DC.B %1,%10000000,%1000000,%100000,%10000,%1000
                DC.B %100,%10,%1,%10000000
                EVEN
                ENDPART

********************************************************************************
* Box (S/W) Clear (D0-X, D1-Y, D2-width, D3-height)                            *
********************************************************************************
                PART 'clr_box'
clr_box:        movem.l D0-D4/D7-A1,-(SP)
                subq.w  #1,D2
                add.w   D0,D2
                subq.w  #1,D3
                mulu    #80,D1
                lea     debugger_scr(A4),A0
                movea.l scr_adr(A0),A0
                adda.w  D1,A0
                move.w  D0,D1
                lsr.w   #3,D1
                adda.w  D1,A0
                move.w  D0,D1
                andi.w  #7,D1
                move.b  clr_box_tab(PC,D1.w),D1
                move.w  D3,D7
                movea.l A0,A1
clr_box1:       and.b   D1,(A0)
                lea     80(A0),A0
                dbra    D7,clr_box1
                addq.w  #8,D0
                lsr.w   #3,D0
                move.w  D2,D4
                lsr.w   #3,D4
                sub.w   D0,D4
                subq.w  #1,D4
                bmi.s   clr_box4
clr_box2:       move.w  D3,D7
                addq.l  #1,A1
                movea.l A1,A0
clr_box3:       clr.b   (A0)
                lea     80(A0),A0
                dbra    D7,clr_box3
                dbra    D4,clr_box2
clr_box4:       addq.w  #1,A1
                andi.w  #7,D2
                move.b  clr_box_tab+1(PC,D2.w),D1
                not.w   D1
clr_box5:       and.b   D1,(A1)
                lea     80(A1),A1
                dbra    D3,clr_box5
                movem.l (SP)+,D0-D4/D7-A1
                rts

clr_box_tab:    DC.B %0,%10000000,%11000000,%11100000,%11110000,%11111000
                DC.B %11111100,%11111110,%11111111
                EVEN
                ENDPART

********************************************************************************
* Draw frame (color) (D0-X, D1-Y, D2 width, D3 height)                         *
********************************************************************************
                PART 'drawfbox'
drawfbox:       movem.l D0-A1/A5-A6,-(SP)
                lea     debugger_scr(A4),A0
                movea.l scr_adr(A0),A0
                lsr.w   #1,D1
                mulu    #160,D1                     ;(Y/2) * 160 (Line width) =>
                adda.w  D1,A0                       ;Offset for the line
                lsr.w   #1,D3                       ;Height / 2, since internally 400 y resolution
                subq.w  #3,D3                       ;Height -1 for the DBRA & -2 for 1. & last line
                moveq   #$0F,D1
                and.w   D0,D1
                move.w  D1,D4
                neg.w   D4
                add.w   #16,D4
                sub.w   D4,D2                       ;So much pixels are already gone from the width
                move.w  D2,D4
                and.w   #$03F0,D2
                lsr.w   #4,D2                       ;By 16 => Word number
                subq.w  #1,D2                       ;For DBRA
                and.w   #$0F,D4
                lsl.w   #2,D4                       ;Pointer to the end mask (also long table)
                lsl.w   #2,D1                       ;* 4, because Long table
                and.w   #$03F0,D0
                lsr.w   #2,D0                       ;By 16 times 4 => by 4
                adda.w  D0,A0                       ;Column
                lea     clr_fbox4(PC),A6
                lea     clr_fbox5(PC),A5
                moveq   #-1,D7
                lea     160(A0),A1                  ;Pointer ever to the next line
                move.l  0(A5,D1.w),D0
                or.l    D0,(A0)+
                move.w  D2,D0
                bmi.s   draw_fbox2
draw_fbox1:     move.l  D7,(A0)+                    ;Pull
                dbra    D0,draw_fbox1
draw_fbox2:     move.l  0(A6,D4.w),D0
                or.l    D0,(A0)                     ;Mask
                movea.l A1,A0
draw_fbox3:     lea     160(A0),A1                  ;Pointer ever to the next line
                move.l  draw_fbox_tab2(PC,D1.w),D0
                or.l    D0,(A0)+
                move.w  D2,D0
                bmi.s   draw_fbox5
draw_fbox4:     addq.l  #4,A0
                dbra    D0,draw_fbox4
draw_fbox5:     move.l  draw_fbox_tab1(PC,D4.w),D0
                bne.s   draw_fbox6
                move.l  draw_fbox_tab1-4(PC),D0
                subq.l  #4,A0
draw_fbox6:     or.l    D0,(A0)                     ;Mask
                movea.l A1,A0
                dbra    D3,draw_fbox3               ;Whole height go through
                lea     160(A0),A1                  ;Pointer ever to the next line
                move.l  0(A5,D1.w),D0
                or.l    D0,(A0)+
                move.w  D2,D0
                bmi.s   draw_fbox8
draw_fbox7:     move.l  D7,(A0)+                    ;Pull
                dbra    D0,draw_fbox7
draw_fbox8:     move.l  0(A6,D4.w),D0
                or.l    D0,(A0)                     ;Mask
                movem.l (SP)+,D0-A1/A5-A6
                rts

                DC.L $010001
draw_fbox_tab1: DC.L 0
draw_fbox_tab2: DC.L $80008000,$40004000,$20002000,$10001000,$08000800,$04000400
                DC.L $02000200,$01000100,$800080,$400040,$200020,$100010
                DC.L $080008,$040004,$020002,$010001
                ENDPART

********************************************************************************
* Clear box (color) (D0-X, D1-Y, D2 width, D3 height)                          *
********************************************************************************
                PART 'clr_fbox'
clr_fbox:       movem.l D0-A1,-(SP)
                lea     debugger_scr(A4),A0
                movea.l scr_adr(A0),A0
                and.l   #$0FFF,D0
                and.l   #$0FFF,D1
                and.l   #$0FFF,D2
                and.l   #$0FFF,D3
                mulu    #80,D1                      ;(Y/2) * 160 (Line width) =>
                adda.w  D1,A0                       ;Offset for the line
                lsr.w   #1,D3                       ;Height / 2, since internally 400 y resolution
                subq.w  #1,D3                       ;Height -1 for the DBRA
                moveq   #$0F,D1
                and.w   D0,D1
                move.w  D1,D4
                neg.w   D4
                add.w   #16,D4
                sub.w   D4,D2                       ;So much pixels are already gone from the width
                move.w  D2,D4
                andi.w  #$03F0,D2
                lsr.w   #4,D2                       ;By 16 => Word number
                subq.w  #1,D2                       ;For DBRA
                and.w   #$0F,D4
                lsl.w   #2,D4                       ;Pointer to the end mask (also long table)
                lsl.w   #2,D1                       ;* 4, because Long table
                andi.w  #$03F0,D0
                lsr.w   #2,D0                       ;By 16 times 4 => by 4
                adda.w  D0,A0                       ;Column
clr_fbox1:      lea     160(A0),A1                  ;Pointer ever to the next line
                move.l  clr_fbox4(PC,D1.w),D0
                and.l   D0,(A0)+
                move.w  D2,D0
                bmi.s   clr_fbox3
clr_fbox2:      clr.l   (A0)+                       ;delete a line
                dbra    D0,clr_fbox2
clr_fbox3:      move.l  clr_fbox5(PC,D4.w),D0
                and.l   D0,(A0)                     ;Mask
                movea.l A1,A0
                dbra    D3,clr_fbox1                ;Whole height go through
                movem.l (SP)+,D0-A1
                rts

clr_fbox4:      DC.L 0,$80008000,$C000C000,$E000E000,$F000F000,$F800F800
                DC.L $FC00FC00,$FE00FE00,$FF00FF00,$FF80FF80,$FFC0FFC0,$FFE0FFE0
                DC.L $FFF0FFF0,$FFF8FFF8,$FFFCFFFC,$FFFEFFFE
clr_fbox5:      DC.L $FFFFFFFF,$7FFF7FFF,$3FFF3FFF,$1FFF1FFF,$0FFF0FFF,$07FF07FF
                DC.L $03FF03FF,$01FF01FF,$FF00FF,$7F007F,$3F003F,$1F001F
                DC.L $0F000F,$070007,$030003,$010001
                ENDPART

********************************************************************************

resvalid        EQU $0426
resvector       EQU $042A
_p_cookies_     EQU $05A0
                IF 0
                PART 'hunt_cookie'                  ;Cookie D0.l Search (n = 1, not found)
;Search cookie named D0.L.
;Parameter:  D0.l : nameDesCookies
;            D0.l : Value of the found cookies
;             N=1  : Cookie not found (D0.L = length of the previous JARS)

hunt_cookie:    movem.l D1-D2/A0,-(SP)
                move.l  D0,D2                       ;Sought names looking for
                move.l  _p_cookies_.w,D0            ;Get hands on the Cookie Jar
                beq.s   hunt_cookie_ex              ;is empty => not found nothing
                movea.l D0,A0
hunt_cookie_l:  move.l  (A0)+,D1                    ;Get name of a cookie
                move.l  (A0)+,D0                    ;and get the value
                cmp.l   D2,D1                       ;Entry found?
                beq.s   hunt_cookie_f               ;Yes!=>
                tst.l   D1                          ;End there list?
                bne.s   hunt_cookie_l               ;No!=> Compare Next
hunt_cookie_ex: moveq   #-1,D0                      ;N-Flag = 1, i. Nix found
hunt_cookie_f:  movem.l (SP)+,D1-D2/A0
                rts
                ENDPART

********************************************************************************

                PART 'insert_cookie'                ;Cookie D0.l ins Cookie Jar
;eigenen Cookie in das Cookie jar
;Parameter:  D0.l : nameDesCookies
;            D1.l : Value of the cookies
;            D2.l : Length of any Cookie Jars (Longworten)
;            A0.l : Address of any Cookie Jars to be set up
;            D0.w : 0 - Everything ok, cookie has been entered
;                    1 - like (1), but now resetfest, i.e resident
;                    2 - as (2), but not reset-resistant
;                   <0 - Error occurred, cookie not entered
insert_cookie:  movem.l D2-D5/A1,-(SP)
                move.l  D2,D5                       ;Length of a possibly list
                move.l  _p_cookies_.w,D3            ;Get hands on the Cookie Jar
                beq.s   insert_cookie_s             ;is empty => Set up list
                movea.l D3,A1
                moveq   #0,D4                       ;Number of slots
insert_cookie_h:addq.w  #1,D4                       ;Increase slot number
                movem.l (A1)+,D2-D3                 ;Get name and value of a cookie
                tst.l   D2                          ;Found empty cookie?
                bne.s   insert_cookie_h             ;No => Continue search
                cmp.l   D3,D4                       ;all slots belated?
                beq.s   insert_cookie_n             ;Yes!=> Create new list
                movem.l D0-D3,-8(A1)                ;Insert new cookie & listende
                moveq   #0,D0                       ;everything OK!
                bra.s   insert_cookie_x             ;and out

insert_cookie_s:moveq   #2,D4
                cmp.l   D4,D2                       ;Less than 2 entries?
                blo.s   insert_cookie_e             ;That's a mistake!(List too small!)
                move.l  resvector.w,old_resvector
                move.l  resvalid.w,old_resvalid     ;Remember old reset vector
                move.l  #cookie_reset,resvector.w
                move.l  #$31415926,resvalid.w       ;and own
                move.l  A0,_p_cookies_.w            ;Initialize Cookie Jar
                moveq   #0,D3                       ;Mark: End of Cookie List
                exg     D2,D3                       ;Number of slots according to D3
                movem.l D0-D3,(A0)                  ;Use name and value of the cookie
                moveq   #1,D0                       ;List resetfest furnishings, everything ok
                bra.s   insert_cookie_x             ;and out

insert_cookie_e:moveq   #-1,D0                      ;Error, cookie not entered
                bra.s   insert_cookie_x             ;and out

;Reset-solid routine to remove the cookie JARS
old_resvalid:   DS.L 1                              ;altesResetValid
                DC.L 'XBRA'                         ;XBRA Protocol.
                DC.L 'BUG2'                         ;Σ-soft identifier, cookie list
old_resvector:  DS.L 1                              ;Alter reset vector
cookie_reset:   clr.l   _p_cookies_.w               ;Cookie Jar remove
                move.l  old_resvector(PC),resvector.w ;Reset-Vector back
                move.l  old_resvalid(PC),resvalid.w
                jmp     (A6)                        ;Continue with the reset

insert_cookie_n:cmp.l   D5,D4                       ;Is the place?
                ble.s   insert_cookie_e             ;No => Error and out
                movea.l _p_cookies_.w,A1            ;Get the beginning of the list again
                move.l  A0,_p_cookies_.w            ;Enter new cookie jar
                subq.w  #2,D4                       ;Do not copy the end (-1 for DBRA)
insert_cookie_m:move.l  (A1)+,(A0)+                 ;Copy entries of the list
                move.l  (A1)+,(A0)+
                dbra    D4,insert_cookie_m
                move.l  D5,D3                       ;Number of slots
                movem.l D0-D3,(A0)                  ;Enter your own element + listend
                moveq   #2,D0                       ;Everything ok, stay resident
insert_cookie_x:movem.l (SP)+,D2-D5/A1
                rts
                ENDPART
                ENDC

********************************************************************************

                PART 'STOP-Icon'
stop_icn:       DC.L $7FFE00                        ;The stop sign
                DC.L $C00300
                DC.L $01BFFD80
                DC.L $037FFEC0
                DC.L $06FFFF60
                DC.L $0DFFFFB0
                DC.L $1BFFFFD8
                DC.L $37FFFFEC
                DC.L $6FFFFFF6
                DC.L $DFFFFFFB
                DC.L $B181860D
                DC.L $A0810205
                DC.L $A4E73265
                DC.L $A7E73265
                DC.L $A3E73265
                DC.L $B1E73205
                DC.L $B8E7320D
                DC.L $BCE7327D
                DC.L $A4E7327D
                DC.L $A0E7027D
                DC.L $B1E7867D
                DC.L $BFFFFFFD
                DC.L $DFFFFFFB
                DC.L $6FFFFFF6
                DC.L $37FFFFEC
                DC.L $1BFFFFD8
                DC.L $0DFFFFB0
                DC.L $06FFFF60
                DC.L $037FFEC0
                DC.L $01BFFD80
                DC.L $C00300
                DC.L $7FFE00
                ENDPART

********************************************************************************
* long data areas that BSR would "optimize" JSR                                *
********************************************************************************
                PART 'TOS-Funktionsnamen'
gemdos_befs:    DC.B 0,0,'Pterm0',0,1,0,'Cconin',0,2,1,'Cconout',0
                DC.B 3,0,'Cauxin',0,4,1,'Cauxout',0,5,1,'Cprnout',0
                DC.B 6,1,'Crawio',0,7,0,'Crawcin',0,8,0,'Cnecin',0
                DC.B 9,3,'Cconws',0,10,3,'Cconrs',0,11,0,'Cconis',0
                DC.B 14,1,'Dsetdrv',0,16,0,'Cconos',0,17,0,'Cprnos',0
                DC.B 18,0,'Cauxis',0,19,0,'Cauxos',0,25,0,'Dgetdrv',0
                DC.B 26,3,'Fsetdta',0,32,3,'Super',0,42,0,'Tgetdate',0
                DC.B 43,1,'Tsetdate',0,44,0,'Tgettime',0,45,1,'Tsettime',0
                DC.B 47,0,'Fgetdta',0,48,0,'Sversion',0,49,6,'Ptermres',0
                DC.B 54,7,'Dfree',0,57,3,'Dcreate',0,58,3,'Ddelete',0
                DC.B 59,3,'Dsetpath',0,60,7,'Fcreate',0,61,7,'Fopen',0
                DC.B 62,1,'Fclose',0,63,57,'Fread',0,64,57,'Fwrite',0
                DC.B 65,3,'Fdelete',0,66,22,'Fseek',0,67,23,'Fattrib',0
                DC.B 69,1,'Fdup',0,70,5,'Fforce',0,71,7,'Dgetpath',0
                DC.B 72,2,'Malloc',0,73,3,'Mfree',0,74,45,'Mshrink',0
                DC.B 75,253,0,'Pexec',0,76,1,'Pterm',0,78,7,'Fsfirst',0
                DC.B 79,0,'Fsnext',0,86,61,'Frename',0,87,23,'Fdatime',0
;From here: Network Extensions (see ST-Magazine 11/89)
                DC.B $60,0,'Nversion',0,$62,57,'Frlock',0,$63,13,'Frunlock',0
                DC.B $64,13,'Flock',0,$65,1,'Funlock',0,$66,1,'Fflush',0
                DC.B $7B,2,'Unlock',0,$7C,2,'Lock',0
                DC.B -1                             ;End of the table

bios_befs:      DC.B 0,3,'Getmpb',0,1,1,'Bconstat',0,2,1,'Bconin',0
                DC.B 3,5,'Bconout',0,4,93,1,'Rwabs',0,5,9,'Setexec',0
                DC.B 6,0,'Tickcal',0,7,1,'Getbpb',0,8,1,'Bcostat',0
                DC.B 9,1,'Mediach',0,10,0,'Drvmap',0,11,1,'Kbshift',0
                DC.B -1
                EVEN

xbios_befs:     DC.B 0,61,'Initmous',0,1,2,'Ssbrk',0,2,0,'Physbase',0
                DC.B 3,0,'Logbase',0,4,0,'Getrez',0,5,31,'Setscreen',0
                DC.B 6,3,'Setpalette',0,7,5,'Setcolor',0,8,91,21,'Floprd',0
                DC.B 9,91,21,'Flopwr',0,10,95,149,'Flopfmt',0,11,0,'Getdsb',0
                DC.B 12,13,'Midiws',0,13,13,'Mfpint',0,14,1,'Iorec',0
                DC.B 15,85,5,'Rsconf',0,16,42,'Keytbl',0,17,0,'Random',0
                DC.B 18,91,0,'Protobt',0,19,91,21,'Flopver',0,20,0,'Scrdmp',0
                DC.B 21,5,'Cursconf',0,22,2,'Settime',0,23,0,'Gettime',0
                DC.B 24,0,'Bioskeys',0,25,13,'Ikbdws',0,26,1,'Jdisint',0
                DC.B 27,1,'Jenabint',0,28,5,'Giaccess',0,29,1,'Offgibit',0
                DC.B 30,1,'Ongibit',0,31,213,0,'Xbtimer',0,32,3,'Dosound',0
                DC.B 33,1,'Setprt',0,34,0,'Kbdvbase',0,35,5,'Kbrate',0
                DC.B 36,3,'Prtblk',0,37,0,'Vsync',0,38,3,'Supexec',0
                DC.B 39,0,'Puntaes',0

                DC.B 41,5,'Floprate',0              ;from TOS 1.4 - StePrate set

                DC.B 42,119,0,'DMAread',0,43,119,0,'DMAwrite',0,44,1,'Bconmap',0 ;TT - function

                DC.B 48,1,'Meta_init',0             ;From here: New features for Meta-DOS
                DC.B 49,5,'open',0                  ;An extension for large volumes
                DC.B 50,1,'close',0                 ;d.h. CD-ROM, etc.
                DC.B 51,85,0,'read',0
                DC.B 53,5,'seek',0
                DC.B 54,5,'status',0
                DC.B 59,21,'start_aud',0            ;From here: Special functions for
                DC.B 60,5,'stop_aud',0              ;The CDAR504 - say Ataris CD-ROM
                DC.B 61,21,'set_songtime',0
                DC.B 62,21,'get_toc',0
                DC.B 63,5,'disc_info',0

                DC.B 64,1,'Blitmode',0              ;From TOS 1.2 - blitter test

                DC.B 80,1,'_EsetShift',0            ;TT - Functions
                DC.B 81,0,'_EgetShift',0,82,1,'_EsetBank',0
                DC.B 83,5,'_EsetColor',0,84,53,'_EsetPalette',0
                DC.B 85,53,'_EgetPalette',0,86,1,'_EsetGray',0
                DC.B 87,1,'_EsetSmear',0
                DC.B -1
                EVEN

vdi_befs:       DC.B 1,0,'_openwk',0,'_clswk',0,'_clrwk',0,'_updwk',0,'di_esc',0
                DC.B '_pline',0,'_pmarker',0,'_gtext',0,'_fillarea',0
                DC.B '_cellarray',0,-1,0
                DC.B 'st_height',0,'st_rotation',0,'s_color',0
                DC.B 'sl_type',0,'sl_width',0,'sl_color',0,'sm_type',0
                DC.B 'sm_height',0,'sm_color',0,'st_font',0,'st_color',0
                DC.B 'sf_interior',0,'sf_style',0,'sf_color',0,'q_color',0
                DC.B 'q_cellarray',0,'_locator',0,'_valuator',0
                DC.B '_choice',0,'_string',0,'swr_mode',0,'sin_mode',0
                DC.B '_illegal',0,'ql_attributes',0,'qm_attributes',0
                DC.B 'qf_attributes',0,'qt_attributes',0,'st_alignment',0
                DC.B '_opnvwk',0,'_clsvwk',0,'q_extnd',0,'_contourfill',0
                DC.B 'sf_perimeter',0,'_get_pixel',0,'st_effects',0,'st_point',0
                DC.B 'sl_ends',0,'ro_cpyfm',0,'r_trnfm',0,'sc_form',0
                DC.B 'sf_updat',0,'sl_udsty',0,'r_recfl',0,'qin_mode',0
                DC.B 'qt_extent',0,'qt_width',0,'ex_timv',0,'st_load_fonts',0
                DC.B 'st_unload_fonts',0,'rt_cpyfm',0,'_show_c',0,'_hide_c',0
                DC.B 'q_mouse',0,'ex_butv',0,'ex_motv',0,'ex_curv',0
                DC.B 'q_key_s',0,'s_clip',0,'qt_name',0,'qt_fontinfo',0
                EVEN
vdi2bef:        DC.B '_bar',0,'_arc',0,'_pie',0,'_circle',0,'_ellipse',0
                DC.B '_ellarc',0,'_ellpie',0,'_rbox',0,'_rfbox',0,'_justified',0

aes_befs:       DC.B 9,'appl',0
                DC.B 'init',0,'read',0,'write',0,'find',0,'tplay',0,'trecord',0
                DC.B 'bvset',0,'yield',-1
                DC.B 18,'appl',0,'exit',-1
                DC.B 19,'evnt',0
                DC.B 'keybd',0,'button',0,'mouse',0,'mesag',0,'timer',0
                DC.B 'multi',0,'dclick',-1
                DC.B 29,'menu',0
                DC.B 'bar',0,'icheck',0,'ienable',0,'tnormal',0,'text',0
                DC.B 'register',0,'unregister',-1
                DC.B 39,'objc',0
                DC.B 'add',0,'delete',0,'draw',0,'find',0,'offset',0,'order',0
                DC.B 'edit',0,'change',-1
                DC.B 49,'form',0
                DC.B 'do',0,'dial',0,'alert',0,'error',0,'center',0,'keybd',0
                DC.B 'button',-1
                DC.B 69,'graf',0
                DC.B 'rubberbox',0,'dragbox',0,'movebox',0,'growbox',0,'shrinkbox',0
                DC.B 'watchbox',0,'slidebox',0,'handle',0,'mouse',0,'mkstate',-1
                DC.B 79,'scrap',0
                DC.B 'read',0,'write',0,'clear',-1
                DC.B 89,'fsel',0,'input',0,'exinput',-1
                DC.B 99,'wind',0,'create',0,'open',0,'close',0,'delete',0,'get',0
                DC.B 'set',0,'find',0,'update',0,'calc',0,'new',-1
                DC.B 109,'rsrc',0,'load',0,'free',0,'gaddr',0,'saddr',0,'obfix',-1
                DC.B 119,'shel',0,'read',0,'write',0,'get',0,'put',0,'find',0
                DC.B 'envrn',0,'rdef',0,'wdef',-1
                DC.B 129,'xgrf',0,'stepcalc',0,'2box',-1
                DC.B -1
aes_all:        DC.B 10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,30,31,32,33
                DC.B 34,35,36,40,41,42,43,44,45,46,47,50,51,52,53,54,55,56,70,71
                DC.B 72,73,74,75,76,77,78,79,80,81,82,90,91,100,101,102,103,104
                DC.B 105,106,107,108,109,110,111,112,113,114,120,121,122,123,124
                DC.B 125,126,127,130,131,0
vdi_all:        DC.B 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19
                DC.B 20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39
                DC.B 100,101,102,103,104,105,106,107,108,109
                DC.B 110,111,112,113,114,115,116,117,118,119,120
                DC.B 121,122,123,124,125,126,127,128,129,130,131
                DC.B 0
                EVEN
                ENDPART

********************************************************************************
* The BSS area                                                                 *
********************************************************************************
varbase:        jmp     ret_jump
                BSS
                RSSET 6                             ;Variables of the keyboard / screen & mouse driver
@chrout         EQU chrout-varbase                  ;relative varbase-Offsets
@print_line     EQU print_line-varbase
@c_eol          EQU c_eol-varbase
@crout          EQU crout-varbase
@page1          EQU page1-varbase
@page2          EQU page2-varbase
@my_driver      EQU my_driver-varbase
@org_driver     EQU org_driver-varbase
@form_do        EQU form_do-varbase
@ikbd_send      EQU ikbd_send-varbase
@redraw_all     EQU redraw_all-varbase
@space          EQU space-varbase
@init           EQU init-varbase
@set_ins_flag   EQU set_ins_flag-varbase
@c_clrhome      EQU c_clrhome-varbase
@anf_adr        EQU anf_adr-varbase
@get_line       EQU get_line-varbase
@cursor_off     EQU cursor_off-varbase
@cache_up       EQU cache_up-varbase
@desel_menu     EQU desel_menu-varbase
@conin          EQU conin-varbase
@scr_edit       EQU scr_edit-varbase
@_trap3         EQU _trap3-varbase

;Variables of the keyboard / screen & mouse driver
curflag:        RS.W 1                              ;<>0: Cursor an
zeile:          RS.W 1                              ;Curses 0-24.
spalte:         RS.W 1                              ;cursorspalte079
debugger_scr:   RS.B scr_struct                     ;datenDesDebuggerScreens
user_scr:       RS.B scr_struct                     ;datenDesUserScreens
init_scr:       RS.B scr_struct                     ;Data of the screen at the debugger start
no_overscan:    RS.B scr_struct                     ;nurNullbytes,DHOverScanWarAus (resetPhase)
                RSEVEN
farbfont:       RS.L 1                              ;Font addresses
s_w_font:       RS.L 1

std_keytab:     RS.L 1                              ;Pointer to keyboard tables
shift_keytab:   RS.L 1
caps_keytab:    RS.L 1
kbdvbase:       RS.L 9
iorec_IKBD:     RS.L 2
iorec_puffer:   RS.L 64
ikbd_string:    RS.B 20                             ;String for the keyboard
vbl_count1:     RS.L 1
vbl_count2:     RS.L 1
tmacro_pointer: RS.L 1                              ;Displays on active TMACRO (0 = no Tmacro)
tmacro_repeat:  RS.W 1                              ;Number of repetitions of a button
tmacro_def_key: RS.L 1                              ;<> 0 => TMACRO definition active (Mastencode)
tmacro_def_adr: RS.L 1                              ;Act.PNT AUF TMACRO-DEFINITION
tmacro_def_flag:RS.B 1                              ;<> 0 => Waiting for key at Control-Esc
                RSEVEN
timer_c_bitmap: RS.W 1
kbd_r_init:     RS.B 1
kbd_r_rate:     RS.B 1
stat_paket:     RS.B 7
maus_paket_1:   RS.B 5
maus_paket_2:   RS.B 3
zeit_paket:     RS.B 6
joydat0:        RS.B 2
joydat2:        RS.B 0
kbstate:        RS.B 1
kbindex:        RS.B 1                              ;Variables of the keyboard driver
kbshift:        RS.B 1
kbalt:          RS.B 1
kbd_repeat_on:  RS.B 1
kbd_r_key:      RS.B 1
kbd_r_verz:     RS.B 1
kbd_r_cnt:      RS.B 1
merk_shift:     RS.B 1
                RSEVEN
mausx:          RS.W 1                              ;mausx (0639)
mausy:          RS.W 1                              ;mausy (0399)
maus_time:      RS.L 1                              ;200Hz meter reading at the last click
maus_time2:     RS.L 1                              ;200Hz counter reading when pressing a mouse button
mausf_time:     RS.L 1                              ;Timer for the mouse acceleration
maus_flag:      RS.W 1                              ;= 0 => Double-click possible (button n.pressed)
sprite_no:      RS.W 1                              ;Spite Number (0 = arrow, 1 = floppy disk)
button_nr:      RS.W 1                              ;aktButtonBeiObjcDraw
ass_load:       RS.B 1                              ;<> 0 => Loading through the assembler
maus_merk:      RS.B 1                              ;Mainted mouse button status
maus_merk2:     RS.B 1
no_dklick:      RS.B 1                              ;<> 0 => No double click query
fast_exit:      RS.B 1                              ;<> 0 => immediately with Ctrl + Help
le_allowed:     RS.B 1                              ;<> 0 => LE is allowed
resident:       RS.B 1                              ;<>0 =>DebuggerIstResident
akt_maust:      RS.B 1                              ;Mouse button for the 200Hz timer
maustast:       RS.B 1                              ;bit0Rachts,B1Links,B2ReB2Dop
mausprell:      RS.B 1                              ;to develop
mausprell2:     RS.B 1
mausoff:        RS.B 1                              ;<> 0 => No mission by the VBL
mausmove:       RS.B 1                              ;<> 0 => Mouse was moved
mausflg:        RS.B 1                              ;= 0 => mouse not there
ssc_flag:       RS.B 1                              ;<> 0 => Shift + Shift + Control pressed
set_lock:       RS.B 1                              ;<> 0 => Cursorating in the VBL is prohibited
device:         RS.B 1                              ;outputStandardDevice
testwrd:        RS.B 1                              ;<> 0 => Output in buffer, otherwise screen
gst_sym_flag:   RS.B 1                              ;<> 0 => Load symbol table
install_load:   RS.B 1                              ;<> 0 => Installation has been loaded
prozessor:      RS.B 1                              ;1:68000,0:68010,1:68020,2:68030
fpu_flag:       RS.B 1                              ;bit0:SfP004,Bit1:68881,Bit2:68882
ste_flag:       RS.B 1                              ;-1: ste hardware available
tt_flag:        RS.B 1                              ;-1: TT available
batch_flag:     RS.B 1                              ;<>0 =>BatchModeAn
ignore_autocrlf:RS.B 1                              ;<> 0 => CR / LF Do not output automatically
                RSEVEN
entry:          RS.W 1                              ;Selected menu item
entry_old:      RS.W 1                              ;Last selected menu item
mausbuffer:     RS.L 17                             ;Background storage of the mouse

untrace_flag:   RS.W 1                              ;<>0 =>UntraceAn
untrace_funk:   RS.B 80                             ;The entered user trace function
untrace_count:  RS.L 1                              ;Counter for Untrace
trace_count:    RS.L 1                              ;Counter for trace
cond_breaks:    RS.B 1280                           ;Place for 16 breakpoints

;Allgemeine Variablen
data_buff:      RS.B 80
fname:          RS.B 80                             ;Buffer for the act.File name
_dumpflg:       RS.B 1                              ;= 0 => screendump auf disk
dobef_flag:     RS.B 1                              ;<> 0 => | command has been executed
help_allow:     RS.B 1                              ;<> 0 => with help back to the assembler
list_flg:       RS.B 1                              ;Symbolic disassemble in "Disa" if <> 0
direct:         RS.B 1                              ;<> 0 => Direct mode (for scrolling)
autodo_flag:    RS.B 1                              ;<> 0 => Run Ctrl + M command
auto_sym:       RS.B 1                              ;<> 0 => Symbols of the assembler are used
mausscroll_on:  RS.B 1                              ;<> 0 => mouse scrolling is on
mausscroll_flg1:RS.B 1                              ;<> 0 => mouse scrolling was on
illegal_flg:    RS.B 1                              ;<>0 =>InScrEditCtrl+crsrLeftFürIllegal
find_cont0:     RS.B 1                              ;1=hunt,0=find,1=ascfind
                RSEVEN
prg_flags:      RS.L 1                              ;Flags of the loaded program
find_cont1:     RS.L 1                              ;Act. Address for Continue
find_cont2:     RS.L 1                              ;End address for Continue
find_cont3:     RS.W 1                              ;Length of the search string
default_adr:    RS.L 1                              ;DefaultArD for various operations
_fhdle:         RS.W 1                              ;FileHandle for I / O operations
_fhdle2:        RS.W 1                              ;FileHandle for log on disk
dir_ext:        RS.B 14                             ;Place for the search path with you
spaced:         RS.B 258                            ;Buffer for e.g. the disassembler
spaced2:        RS.B 258                            ;a second general buffer

default_start:  RS.B 10                             ;'∑-Soft'
do_resident:    RS.W 1                              ;<> 0 => 'Resident' automatically execute
alquantor:      RS.B 1                              ;'*' Wildcard
exquantor:      RS.B 1                              ;'?' Wildcard
overscan:       RS.W 1                              ;<>0 =>OverScanIstAn
midi_flag:      RS.W 1                              ;<> 0 => No MIDI keyboard query
ring_flag:      RS.W 1                              ;<> 0 => No ring indicator test
shift_flag:     RS.W 1                              ;<> 0 => SHIFT-SHIFT cancel implemented
ins_mode:       RS.W 1                              ;<>0 =>InsertModeAn
cursor_form:    RS.W 1                              ;The cursor shape
disbase:        RS.W 1                              ;Number base of the disassembler
format_flag:    RS.W 1                              ;<> 0 => Disassemble with double-click, otherwise dump
def_lines:      RS.W 1                              ;defaultzeilenanzahl (normal=16)
def_size:       RS.W 1                              ;Width at dump (normal = 16)
scroll_d:       RS.W 1                              ;scrollverzögerung
trace_delay:    RS.W 1                              ;Delay according to F1, ...
trace_flag:     RS.W 1                              ;0=list,1=disassembleBeiTrace (f1,f2,...)
smart_switch:   RS.W 1                              ;0 = normal screen switching, 1 = switching in the VBL
small:          RS.W 1                              ;<> 0 => small line
col0:           RS.W 1                              ;Background color of the debugger
col1:           RS.W 1                              ;Foreground color ""
conterm:        RS.W 1                              ;House 0 = 1 => Connect you
no_aes_check:   RS.W 1                              ;<> 0 => No AES / VDI parameter test
all_memory:     RS.W 1                              ;<> 0 => No memory access test
bugaboo_sym:    RS.W 1                              ;<> 0 => Use internal symbol table
_zeile3:        RS.B 80                             ;3rd Line Buffer (for Help)
convert_tab:    RS.B 256                            ;Character conversion table
                RS.L 1                              ;Must be zero!
tmacro_tab:     RS.L 1024                           ;TMACro table
tmacro_tab_end: RS.L 1                              ;End of the TMACro table
default_end:    RS.W 0

_regsav2:       RS.L 1
_regsav:        RS.L 16                             ;registerBeiDoTrap1

trap_abort:     RS.B 1                              ;Trap number, which triggered
observe_off:    RS.B 1                              ;<> 0 => Switch off Observe
gemdos_break:   RS.B 126                            ;For Gemdos (0-126)
bios_break:     RS.B 12                             ;for BIOS (0-11)
xbios_break:    RS.B 87                             ;for XBIOS (0-87)
aes_break:      RS.B 136                            ;for AES (Function NOR)
vdi_break:      RS.B 132                            ;for VDI (Function NOR)
end_of_breaks:  RS.B 0
breaks_flag:    RS.B 1                              ;<> 0 => Breakpoints used
breakpnt:       RS.W 102                            ;Storage for the breakpoints
breakpnt_end:   RS.W 0
input_pnt:      RS.L 1                              ;Pointer to the input busher (for macros)


;The registers of the program to be debugged
regs:           RS.L 15                             ;D0-D7/A0-A6
rega7:          RS.L 1                              ;A7
_pc:            RS.L 1                              ;PC  64 String is fix !!!
_usp:           RS.L 1                              ;USP 68
_ssp:           RS.L 1                              ;SSP 72
_sr:            RS.W 1                              ;SR  76
_fcreg:         RS.W 1                              ;Function code register (bus error)
_zykadr:        RS.L 1                              ;Cycle Address (Bus Error)
_befreg:        RS.W 1                              ;Command register (bus error)

merk_stk:       RS.L 1                              ;traceUntilRts
merk_a0:        RS.L 1                              ;A0 for BREAK at "LineInput"
merk_pc:        RS.L 1                              ;PC for marking at the beginning of the line
merk_pc_call:   RS.L 1                              ;Messed PC for call

dsk_track:      RS.W 1
dsk_sektor:     RS.W 1
dsk_side:       RS.W 1
dsk_drive:      RS.W 1                              ;for RS, RW, RT
dsk_adr:        RS.L 1
dsk_adr2:       RS.L 1
checksum:       RS.W 1

;Everything just for the "Load for Execute"-command
load1:          RS.W 1                              ;1. Command of the program (for breakpoint)
load2:          RS.L 1                              ;Illegal vector
load3:          RS.L 1                              ;alterStackpointer
load4:          RS.L 1                              ;Return address to Term
load5:          RS.L 1                              ;prgInternerStack (regsaveBase)
load6:          RS.L 1                              ;Gets Trap # 14
                RS.L 40                             ;stackBeiLoadForExecute
lstackend:      RS.L 0

assm_flag:      RS.W 1                              ;<> 0 => Enter with the line assembler
tablen:         RS.W 1                              ;Number of mnemonics in the table
op_buffer:      RS.B 64                             ;Buffer opcode and line info

upper_line:     RS.W 1                              ;Screen layout
upper_offset:   RS.W 1                              ;For the screen driver
down_lines:     RS.W 1
rom_base:       RS.L 1                              ;Start address of the Rome
jmpdispa:       RS.L 1                              ;Jump address for INP_LOOP
end_adr:        RS.L 1                              ;endadresseDesDebuggers
merk_svar:      RS.L 1                              ;Pointer to the marker table
basep:          RS.L 1                              ;Basepage of the invited program
trace_pos:      RS.L 1                              ;positionImTracebuffer
reg_pos:        RS.L 1                              ;Act. Display position in the trace buffer
merk_anf:       RS.L 1                              ;Start / End Address to Load
merk_end:       RS.L 1
err_stk:        RS.L 1                              ;Commercial stack for double-click
err_flag:       RS.B 1                              ;<> 0 => instead of mistakes to muff f (finish)
first_call:     RS.B 1                              ;= 0 => Output Alert
merk_it:        RS.L 1                              ;Members 200Hertz timer (protection!)
sym_adr:        RS.L 1                              ;Pointer to the symbol table
sym_size:       RS.L 1                              ;Size of the symbol table
sym_end:        RS.L 1                              ;End address of the symbol table + 1
prg_base:       RS.L 1                              ;<> 0 => PRGBASISADR in RAM (Automatic)
max_linef:      RS.W 1                              ;$9CcAlsMaxLineFOpcode
merk_quit_sr:   RS.W 1                              ;srBeiKillProgram
linef_base:     RS.L 1                              ;Basicadr the linef-jumping table
save_clrkbd:    RS.L 1                              ;Auto-Repeat Flag Address (always delete content)
first_free:     RS.L 1                              ;Pointer behind the debugger
end_of_mem:     RS.L 1                              ;End of free memory
act_pd:         RS.L 1                              ;Current Process Pointer
merk_act_pd:    RS.L 1                              ;actPdBeimDebuggerstart
kbshift_adr:    RS.L 1                              ;Address of the KBShift variable
serial:         RS.L 1                              ;The serial number from the header (to the display)
quit_stk:       RS.L 1                              ;Recurrence 14 when calling the residents ver
ass_vector:     RS.L 1                              ;ADR there Assembler Vector Crackle
_zeile:         RS.B 82                             ;Line input bug
_zeile2:        RS.B 82                             ;2.-line buffer (for Undo)
prn_pos:        RS.W 1                              ;Position in the pressure buffer
prn_buff:       RS.B 258                            ;Buffer for printer output
simple_vars:    RS.L 10                             ;Max.10 "Simple" user variables
merk_internal:  RS.L 10                             ;All MGL registers
merk_user:      RS.L 10                             ;All Mgl registers of the prg
cmd_line_adr:   RS.L 1                              ;Handover of a command-line with output
sym_buffer:     RS.L 1                              ;Pointer to the symbol table
sym_anzahl:     RS.W 1                              ;Number of symbols in the table
old_stack:      RS.L 1                              ;alterSsp
old_usp:        RS.L 1                              ;alter usp.
line_back:      RS.L 1                              ;PC offset as return to the assembler
hz200_time:     RS.L 1                              ;Members HZ200 Timerstand (WG.The Hardisk!)
caps_tab:       RS.B 128                            ;Keyboard table for caps / lock
dta_buffer:     RS.B 44                             ;The DTA buffer for FSFirst ()
default_stk:    RS.L 1                              ;A7 for errors
basepage:       RS.L 1                              ;basepageDesDebuggers
save_data:      RS.B $05B0-8                        ;Copy of the storage area from $ 8 to $ 5AF
screen:         RS.B 2080                           ;80 * 26 characters on a screen
                RS.L 64                             ;Rechenstack for convert_formel
formel:         RS.L 0
spez_format:    RS.B 10                             ;Output width for spec_buff
spez_buff:      RS.W 2560                           ;10 * 256 bytes for the 10 formula parts
scr_buff:       RS.W 8030                           ;10 screens can be saved
linebuf:        RS.L 2048                           ;Max.2048 lines per convert_formel (optimize)
debug_sstack:   RS.L 0                              ;8K stack for the debugging program
cond_bkpt_jsr:  RS.W 4096                           ;16 * 512 Bytes for Breakpoints
user_trace_buf: RS.W 512                            ;1KUserTraceRoutine
allg_buffer:    RS.B 14000                          ;Buffer for you and format
allg_buf_end:   RS.W 0                              ;End des buffers
trace_buff:     RS.W 9984                           ;Buffer for 256 Trace PC + Register
trace_buffend:  RS.L 512                            ;Internal stack
sekbuff:        RS.B 512                            ;Buffer for a sector
drv_table:      RS.L 16                             ;Space for the serial numbers of 16 drives
hires:          RS.B 32000+1280*2+255               ;The screen memory from the debugger
                RSEVEN
ende:           RS.L 0                              ;Dummy for the end
                RSBSS
                END
