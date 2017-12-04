#lang racket

(provide object-code)

(define debug #f)

(define/match (object-code x)
  (((list 'expr x)) (format "~A;" (object-code x)))
  (((list 'return x)) (format "return ~A;" (object-code x)))
  (((list 'define id expr)) (format-const id expr))
  (((list 'lambda args body ...)) (format-lambda args body))
  (((list 'if pred alt1 alt2)) (format-if pred alt1 alt2))
  (((list 'when pred then)) (format-when pred then))
  (((list* op args)) (format-op op args))
  ((x) ;(displayln x)
       (format-term x)))


(define (format-let id expr) ;когда добавлю expand-let
  (format "let ~A = ~A" id (object-code expr)))

(define (format-const id expr)
  (format "const ~A = ~A" id (object-code expr)))

(define (format-arg-list sym args)
  (string-join (map object-code args) sym))

(define (format-lambda args body)
  (when debug (displayln (format "LAMBDA args: ~A; body: ~A" args body)))
  (if (list? args)
      (format "(~A) => {\n ~A \n}\n" (format-arg-list ", " args) (format-arg-list "\n" body))
      (format "~A => {\n ~A \n}\n" args (format-arg-list "\n" body))))

(define predefined-ops
  '((+ . "+")
    (- . "-")
    (* . "*")
    (/ . "/")
    (and . "&&")
    (or . "||")
    (remainder . "%") ;binary
    (> . ">") ;binary
    (< . "<") ;binary
    (= . "==") ;binary
    )) 

(define predefined-funcs
  '((display . "print")))

(define (format-op op args)
  (when debug (displayln (format "FORMAT op: ~A; args: ~A" op args)))
  (let ((pre-op (assq op predefined-ops)))
    (if (false? pre-op)
        (let ((pre-func (assq op predefined-funcs))
              (formated-args (format-arg-list ", " args)))
          (if (false? pre-func)
              (if (list? op)
                  (format "(~A)(~A)" (object-code op) formated-args)
                  (format "~A(~A)" (object-code op) formated-args))
              (format "~A(~A)" (cdr pre-func) formated-args)))
        (format "(~A)" (format-arg-list (cdr pre-op) args)))))

(define (format-term x)
  (when debug (displayln (format "term: ~A" x)))
  (if (boolean? x)
      (if x "true" "false")
      (format "~S" x)))

(define (format-if cnd tbr fbr)
  (format "if (~A) {\n ~A \n}else{\n ~A \n}\n" (object-code cnd) (object-code tbr) (object-code fbr)))

(define (format-when pred then)
  (when debug (displayln (format "pred: ~A; when: ~A" pred then)))
  (format "if (~A) {\n ~A \n}\n" (object-code pred) (object-code then)))
