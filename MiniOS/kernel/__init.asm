;;-----------------_DEFINITIONS ONLY_-----------------------
;; IMPORT FUNCTIONS FROM C
%macro IMPORTFROMC 1-*
%rep  %0
    %ifidn __OUTPUT_FORMAT__, win32 ; win32 builds from Visual C decorate C names using _ 
    extern _%1
    %1 equ _%1
    %else
    extern %1
    %endif
%rotate 1 
%endrep
%endmacro

;; EXPORT TO C FUNCTIONS
%macro EXPORT2C 1-*
%rep  %0
    %ifidn __OUTPUT_FORMAT__, win32 ; win32 builds from Visual C decorate C names using _ 
    global _%1
    _%1 equ %1
    %else
    global %1
    %endif
%rotate 1 
%endrep
%endmacro

%define break xchg bx, bx
%define PAGE_SIZE 4096
%define BIT_PRESENT 0x1
%define BIT_READ_WRITE 0x2
%define BIT_USER_ACCESSIBLE 0x4

IMPORTFROMC KernelMain

TOP_OF_STACK                equ 0x200000
KERNEL_BASE_PHYSICAL        equ 0x200000
;;-----------------^DEFINITIONS ONLY^-----------------------

segment .text
[BITS 32]
ASMEntryPoint:
    cli
    MOV     DWORD [0x000B8000], 'O1S1'
%ifidn __OUTPUT_FORMAT__, win32
    MOV     DWORD [0x000B8004], '3121'                  ; 32 bit build marker
%else
    MOV     DWORD [0x000B8004], '6141'                  ; 64 bit build marker
%endif



    MOV     ESP, TOP_OF_STACK                           ; just below the kernel
    
    ;cr0.pe is set in ssl as well as the gdt initialisation

    ; Enable PAE (CR4.PAE = 1)
    push eax              
    mov eax, cr4
    or eax, 0x20        ; Set the PAE bit (bit 5)
    mov cr4, eax        ; Write the updated value back to CR4
    pop eax 

    ; Load PML4 base address into CR3
    lea eax, [pml4_table]
    mov cr3, eax

    ; Enable long mode (IA32_EFER.LME = 1) before enabling paging
    mov ecx, 0xC0000080         ; MSR address for IA32_EFER
    rdmsr                       ; Read the current value of IA32_EFER
    or eax, 0x100               ; Set the LME bit (bit 8)
    wrmsr 

    ; Enable paging (CR0.PG = 1)
    push eax              
    mov eax, cr0 
    or eax, 0x80000000     ; Set the PG bit (bit 31) to enable paging
    mov cr0, eax           ; Write the updated value back to CR0
    pop eax         


    jmp 8:.bits64

;;--------------------------------------------------------

[BITS 64]
.bits64:
    mov  ax,    16       ; index of FLAT_DESCRIPTOR_DATA64 entry
    mov  ds,    ax
    mov  es,    ax
    mov  gs,    ax
    mov  ss,    ax
    mov  fs,    ax

    ; Invalidate identity-mapping (optional step)
    ; mov rax, PML4
    ; mov QWORD [rax], 0

    ; Invalidate all entries in the TLB (Translation Lookaside Buffers)
    mov rax,    cr3
    mov cr3,    rax

    ; Jump to the kernel
    mov rax, KernelMain
    call rax

    break
    cli
    hlt

[BITS 32]
__cli:
    CLI
    RET

__sti:
    STI
    RET

__magic:
    XCHG    BX,BX
    RET
    
__enableSSE:                ;; enable SSE instructions (CR4.OSFXSR = 1)  
    MOV     EAX, CR4
    OR      EAX, 0x00000200
    MOV     CR4, EAX
    RET
    
EXPORT2C ASMEntryPoint, __cli, __sti, __magic, __enableSSE

;;-----------------_DATA SEGMENT_-----------------------
segment .data 

; Level 4 Page Map (PML4)
pml4_table:
    dq pdpt_table + BIT_PRESENT + BIT_READ_WRITE
    times 511 dq 0

; Page Directory Pointer Table (PDPT)
pdpt_table:
    dq pd_table + BIT_PRESENT + BIT_READ_WRITE
    times 511 dq 0

; Page Directory Table (PD)
pd_table:
    dq pt_table + BIT_PRESENT + BIT_READ_WRITE
    times 511 dq 0

; Page Table (PT) - Identity maps the first 2MB of memory
pt_table:
    dq 0x00000000 + BIT_PRESENT + BIT_READ_WRITE   ; Map 0x00000000 - 0x00000FFF
    dq 0x00001000 + BIT_PRESENT + BIT_READ_WRITE   ; Map 0x00001000 - 0x00001FFF
    dq 0x00002000 + BIT_PRESENT + BIT_READ_WRITE   ; Map 0x00002000 - 0x00002FFF
    dq 0x00003000 + BIT_PRESENT + BIT_READ_WRITE   ; Map 0x00003000 - 0x00003FFF
    dq 0x00004000 + BIT_PRESENT + BIT_READ_WRITE   ; Map 0x00004000 - 0x00004FFF
    ; Continue mapping up to 4MB
    times 512 - 5 dq 0