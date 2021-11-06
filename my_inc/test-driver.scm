(define-module (test-driver)
  #:export (*all-tests*
	    clear-tests
            display-tests
            add-tests-with-string-output))

(define *all-tests* '())

(define (clear-tests)
  (set! *all-tests* '()))

(define (display-tests)
  (format #t "Current test:\n")
  (map (lambda (t)
	 (let ((test-name (car t))
	       (count (length (cdr t))))
	   (format #t "Test name: ~a Count: ~a" test-name count)))
       *all-tests*))  

(define-syntax add-tests-with-string-output
  (syntax-rules (=>)
    [(_ test-name [expr -> output-string] ...)
     (set! *all-tests*
        (cons
           '(test-name [expr string  output-string] ...)
            all-tests))]))

