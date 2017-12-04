#lang racket

(require "expand-arithm.rkt" "object-code.rkt")

(define (read-all)
  (let loop ()
    (define x (read))
    (if (eof-object? x)
        '()
        (cons x (loop)))))

(define compile (compose object-code inject-expr expand))

(define (compile-test x)
  (let* ((expanded (inject-expr (expand x)))
         (coded (object-code expanded)))
    (displayln (format "given: ~A" x))
    (displayln (format "expanded: ~A" expanded))
    (displayln (format "coded: ~A" coded))
    (displayln "")
    coded))

(define (compile-file filename [comp-func compile])
  (string-join
   (map comp-func (with-input-from-file filename read-all)) "\n"))

(compile-file "tests/gcd.txt" compile-test)
;(compile-file "tests/cond.txt" compile-test)
;(compile-file "tests/lambda.txt" compile-test)
;(compile-file "tests/begin.txt" compile-test)
;(compile-file "tests/define.txt" compile-test)
;(compile-file "tests/lambda-cond.txt" compile-test)
;(compile-file "tests/lambda-begin.txt" compile-test)
;(compile-file "tests/application.txt" compile-test)
;(compile-file "tests/let.txt" compile-test)



; TODO
; string->symbol - в js должен быть аналог, лень искать
; нужно разобраться с ; и лишними скобками
; применение функции без аргументов разворачивается в (f ((lambda ((lambda () (return ())))
