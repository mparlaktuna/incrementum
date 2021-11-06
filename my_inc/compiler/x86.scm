(define-module (compiler x86)
  #:export (compile))
  
(define wordsize 8)

(define nil-tag #x3F)

(define boolean-mask #xBF)
(define boolean-bit  #x06)
(define true-tag     #x6F)
(define false-tag    #x2F)

(define fixnum-shift 2)
(define fixnum-mask  #x03)
(define fixnum-tag   #x00)

(define char-shift 8)
(define char-mask  #x3F)
(define char-tag   #x0F)

(define object-mask #x07)
(define cons-tag    #x01)
(define vector-tag  #x05)
(define string-tag  #x06)

(define heap-align-mask #xF8)

(define ax  'rax)
(define bx  'rbx)
(define cx  'rcx)
(define dx  'rdx)
(define si  'rsi)
(define di  'rdi)
(define bp  'rbp)
(define sp  'rsp)
(define r8  'r8)
(define r9  'r9)
(define r10 'r10)
(define r11 'r11)
(define r12 'r12)
(define r13 'r13)
(define r14 'r14)
(define r15 'r15)

(define (emit-program expr)
  (emit-program-header)
  ;; (if (letrec? program) (emit-letrec program)
  (emit-scheme-entry expr (make-initial-env '() '())))

(define (make-initial-env lvars labels)
  (map cons lvars labels))

(define (emit-tail-expr si env expr)
  (emit-generic-expr si env #t expr))

(define (immediate? expr)
  (or (null? expr)
      (fixnum? expr)
      (boolean? expr)
      (char? expr)))
      
(define fixnum?
  (let* ((bit-length (- (* wordsize 8) fixnum-shift))
         (lower-bound (- (expt 2 (- bit-length 1))))
         (upper-bound (- (expt 2 (- bit-length 1)) 1)))
    (lambda (x)
      (and (integer? x) (exact? x) (<= lower-bound x upper-bound)))))
  
(define (immediate-rep x)
  (cond
   ((fixnum? x) (ash x fixnum-shift))
   ((boolean? x) (if x true-tag false-tag))
   ((null? x) nil-tag)
   ((char? x) (logior (ash (char->integer x) char-shift) char-tag))
   (else #f)))

(define (emit-immediate value . args)
  (emit-mov (immediate-rep value) (if (null? args) ax (car args))))

(define (emit-generic-expr si env tail expr)
  (cond
   ((immediate? expr) (emit-immediate expr) (when tail (emit-ret)))
   ;; ((variable? expr) (emit-variable-ref env expr) (when tail (emit-ret)))
   ;; ((if? expr) (emit-if si env tail expr))
   ;; ((and? expr) (emit-and-expr si env tail expr))
   ;; ((or? expr) (emit-or-expr si env tail expr))
   ;; ((let!? expr) (emit-let si env tail expr))
   ;; ((apply? expr env) (emit-apply si env tail expr))
   ;; ((begin? expr) (emit-begin si env tail (cdr expr)))
   ;; ((primitive-call? expr)
   ;;  (emit-primitive-call si env expr) (when tail (emit-ret)))
   (else (format #f "~s is not a valid expression" expr))))

(define (emit-scheme-entry expr env)
  (emit-function-header "l_scheme_entry")
  (emit-tail-expr (- wordsize) env expr)
  (emit-ret))

(define (compile expr file-name)
  (with-output-to-file file-name
    (lambda ()
      (emit-program expr))))

(define (emit . c)
  (apply format (cons #t c))
  (format #t "\n"))

(define (emit-ret)
  (emit "	ret"))

(define (emit-function-header label)
  (emit "	.globl	~a" label)
  (emit "	.type	~a,	@function" label)
  (emit-label label))

(define (emit-label label)
  (emit "~a:" label))

(define (emit-store-context)
  (emit-save #x08 bx ax)
  (emit-save #x20 si ax)
  (emit-save #x28 di ax)
  (emit-save #x30 bp ax)
  (emit-save #x38 sp ax))

(define (emit-load-context)
  (emit-load #x38 bx sp)
  (emit-load #x30 bx bp)
  (emit-load #x28 bx di)
  (emit-load #x20 bx si)
  (emit-load #x08 bx bx))

(define (load-stack)
  (emit-mov cx sp))

(define (load-heap)
  (emit-mov dx bp))

(define (store-context)
  (emit-mov ax bx)
  )

(define (emit-program-header)
  (emit "	.text")
  (emit-function-header "scheme_entry")	;

  (emit-store-context)
  (load-stack)
  (load-heap)
  (store-context)

  (emit-call "l_scheme_entry")

  (emit-load-context)

  (emit-ret))


(define (emit-call label)
  (emit "	call	~a" label))

(define (emit-instruction instr src dst)
  (emit "	~s	~s~s,	%~s" instr (if (symbol? src) '% '$) src dst))

(define (emit-mov src dst)
  (emit-instruction 'mov src dst))

(define (emit-save-instruction instr offset src dst)
  (emit "	~s	~s~s,	~s(%~s)" instr (if (symbol? src) '% '$) src offset dst))

(define (emit-save offset src dst)
  (emit-save-instruction 'mov offset src dst))

(define (emit-load-instruction instr offset src dst)
  (emit "	~s	~s(%~s),	%~s" instr offset src dst))

(define (emit-load offset src dst)
  (emit-load-instruction 'mov offset src dst))
