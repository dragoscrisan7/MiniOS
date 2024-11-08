%define break xchg bx, bx

BYTES_IN_SECTOR                    equ 512
SECTORS_IN_HEAD                    equ 18
HEADS_IN_CYLINDER                  equ 2
CYLINDER_MBR_SSL                   equ 1
TOTAL_KERNEL_CYLINDERS             equ 6
LAST_KERNEL_CYLINDER               equ CYLINDER_MBR_SSL + TOTAL_KERNEL_CYLINDERS
DMA_BOUNDARY_ALERT                 equ 0xDC00 ; 0xDC00 + 0x2400 == 0x10000 and DMA gives boundary error

[org 0x7E00]
[bits 16]
SSL:
    mov bx, 0x1000                  ; memory 0x9200 - 0x9FFFF is unused
    mov    es, bx
    xor    bx, bx                   ; read to address ES:BX == 0x10000

    mov ch, 0x01                    ; start reading cylinder 1 (0 was for MBR, SSL)
    mov cl, 0x01                    ; always read from sector 1
.read_next_cylinder:
    mov dh, 0x00                    ; start reading the first head

    .read_next_head:
        cmp bx, DMA_BOUNDARY_ALERT
        jb .read
        mov    bx, es
        add    bx, 0x1000
        mov es, bx
        xor bx, bx                      ; ES:DC00h -> ES+1000h:0000h
    .read:
        mov ah, 0x02                    ; read function
        mov    al, SECTORS_IN_HEAD      ; read all the sectors from a head
        int    13h
        jc    .error
        
        add bx, SECTORS_IN_HEAD * BYTES_IN_SECTOR
        
        inc dh                          ; go to next head
        cmp dh, HEADS_IN_CYLINDER
        jb .read_next_head

    inc ch                              ; go to next cylinder
    cmp ch, LAST_KERNEL_CYLINDER
    jb  .read_next_cylinder
    je  .success
  
.error:
    cli                    ; we should reset drive and retry, but we hlt
    hlt                     
 
.success:       
    cli                    ; starting RM to PM32 transition
    lgdt    [GDT]
    mov        eax,    cr0
    or        al,        1
    mov        cr0,    eax     ; we are in protected mode but we need to set the CS register  
    jmp        8:.bits32       ; we change the CS to 8 (index of FLAT_DESCRIPTOR_CODE32 entry)

.bits32:
[bits 32]
    mov    ax, 16       ; index of FLAT_DESCRIPTOR_DATA32 entry
    mov    ds, ax
    mov    es, ax      
    mov    gs, ax      
    mov    ss, ax      
    mov    fs, ax 
    
    cld
    mov edi,    0x200000                ; destination
    mov esi,    0x10000                    ; initial source
    xor    ebx,    ebx
    mov    ecx,    TOTAL_KERNEL_CYLINDERS    
.copy_next_cylinder:                        ; for ecx in range(TOTAL_KERNEL_CYLINDERS)
    push    ecx                             ; save ecx
    mov        ecx,    HEADS_IN_CYLINDER    
    .copy_next_head:                        ; for ecx in range(HEADS_IN_CYLINDER)
        push    ecx                              ; save ecx
        cmp        ebx,    DMA_BOUNDARY_ALERT    ; do we have to increase esi?
        jb        .copy
        add        esi,    0x10000               ; increase esi
        xor        ebx,    ebx
    .copy:                            
        push    esi                              ; save initial source
        add        esi,    ebx                   ; initial source += offset
        mov        ecx,    SECTORS_IN_HEAD * BYTES_IN_SECTOR
        rep        movsb                         ; copy
        add        ebx,    SECTORS_IN_HEAD * BYTES_IN_SECTOR
        pop        esi                           ; restore initial source
        pop        ecx                           ; restore ecx
        loop .copy_next_head
    pop        ecx                               ; restore ecx
    loop .copy_next_cylinder
    
    mov    [ds:0xb8000], BYTE 'O'
    mov    [ds:0xb8002], BYTE 'K'
    
    
is_A20_on?:   
    pushad
    mov edi,0x112345  ;odd megabyte address.
    mov esi,0x012345  ;even megabyte address.
    mov [esi],esi     ;making sure that both addresses contain diffrent values.
    mov [edi],edi     ;(if A20 line is cleared the two pointers would point to the address 0x012345 that would contain 0x112345 (edi)) 
    cmpsd             ;compare addresses to see if the're equivalent.
    popad
    jne A20_on        ;if not equivalent , A20 line is set.

    cli               ;if equivalent , the A20 line is cleared.
    hlt
 
A20_on:
    
    
    mov    eax, 0x201000    ; the hardcoded ASMEntryPoint of the Kernel
    call   eax
    
    cli                     ; this should not be reached 
    hlt

;
; Data
;
GDT:
    .limit  dw  GDTTable.end - GDTTable - 1
    .base   dd  GDTTable

FLAT_DESCRIPTOR_CODE32  equ 0x00CF9A000000FFFF  ; Code: Execute/Read
FLAT_DESCRIPTOR_DATA32  equ 0x00CF92000000FFFF  ; Data: Read/Write
FLAT_DESCRIPTOR_CODE16  equ 0x00009B000000FFFF  ; Code: Execute/Read, accessed
FLAT_DESCRIPTOR_DATA16  equ 0x000093000000FFFF  ; Data: Read/Write, accessed
FLAT_DESCRIPTOR_CODE64  equ 0x00AF9A000000FFFF  ; Long Mode Code: Execute/Read
FLAT_DESCRIPTOR_DATA64  equ 0x00AF92000000FFFF  ; Long Mode Data: Read/Write
    
GDTTable:
    .null     dq 0                         ;  0
    .code32   dq FLAT_DESCRIPTOR_CODE32    ;  8
    .data32   dq FLAT_DESCRIPTOR_DATA32    ; 16
    .code16   dq FLAT_DESCRIPTOR_CODE16    ; 24
    .data16   dq FLAT_DESCRIPTOR_DATA16    ; 32
    .code64   dq FLAT_DESCRIPTOR_CODE64    ; 40
    .data64   dq FLAT_DESCRIPTOR_DATA64    ; 48
    .end: