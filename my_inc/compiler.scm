(define-module (compiler)
  #:export (compile))
	    

;; (define compile-port
;;   (make-parameter
;;     (current-output-port)
;;     (lambda (p)
;;        (unless (output-port? p)
;;          (error 'compile-port (format "not an output port ~s" p)))
;;        p)))

(define (emit-program expr)
  (emit-program-header))


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

(define (emit-program-header)
  (emit "	.text")
  (emit-function-header "scheme_entry")	;

;;   ;; ;; store context ( base ptr in rax )
;;   ;; (emit-save #x08 bx ax)
;;   ;; (emit-save #x20 si ax)
;;   ;; (emit-save #x28 di ax)
;;   ;; (emit-save #x30 bp ax)
;;   ;; (emit-save #x38 sp ax)

;;   ;; ;; load stack
;;   ;; (emit-mov cx sp)

;;   ;; ;; load heap
;;   ;; (emit-mov dx bp)

;;   ;; ;; store context
;;   ;; (emit-mov ax bx)

;;   ;; (emit-call "l_scheme_entry")

;;   ;; ;; load context
;;   ;; (emit-load #x38 bx sp)
;;   ;; (emit-load #x30 bx bp)
;;   ;; (emit-load #x28 bx di)
;;   ;; (emit-load #x20 bx si)
;;   ;; (emit-load #x08 bx bx)

    (emit-ret))

;; (define (emit-program program))
  ;; (emit-program-header)
  ;; (if (letrec? program) (emit-letrec program)
  ;;     (emit-scheme-entry program (make-initial-env '() '()))))
