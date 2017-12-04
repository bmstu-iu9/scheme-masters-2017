#lang racket

(provide expand return inject-expr)

(define debug #t)

(define op-bin (list '+ '- '* '/))
(define op-cmp (list '> '< '>= '<= '= ))
(define is-op-lambda? (lambda (x) (and (list? x)(eq? 'lambda (car x)))))

(define (each-two lst)
  (if (or (null? lst)(null? (cdr lst)))
      (list)
      (cons (cons (car lst)(cadr lst)) (each-two (cdr lst)))))

(define (to-lambda lst)
  `(lambda () ,@lst))

(define (inject-return expr)
  (match expr
    ((list 'when stmt ... last) `(when ,@stmt ,(inject-return last)))
    ((list 'if stmt alt1 alt2) `(if ,stmt ,(inject-return alt1) ,(inject-return alt2)))
    (_ `(return ,expr))))


(define (inject-expr expr)
  (match expr
    ((list 'when stmt ... last) `(when ,@(inject-expr stmt) ,last))
    ((list 'if stmt alt1 alt2) `(if ,stmt ,(inject-expr alt1) ,(inject-expr alt2)))
    (_ `(expr ,expr))))

;(inject-return '(when  (> x 0) (+ 1 2) (- 3 4)))
;(inject-return '(when  (> x 0) when (+ 1 2) when (- 3 4)))
;(inject-return '(if (> a b) (begin 1 2 3)(begin 4 5 6)))
               
(define return (lambda (x) x))

; (+) (*) (or) (and) без аргументов не поддерживаются
(define (expand-bin op arg)
  (when debug (displayln (format "BIN op: ~A; arg: ~A" op arg)))
  (when (not (null? arg))
    (if (not (null? (cdr arg)))
        `(,op ,(expand (car arg)) ,(expand (cons op (cdr arg))))
        `(,op ,(expand (car arg))))))

(define (expand-cmp op arg)
  (when debug (displayln (format "CMP op: ~A; arg: ~A" op arg)))
  (let* ((pairs (each-two arg))
         (comps (map
                 (lambda (x) `(,op ,(expand (car x))
                                   ,(expand (cdr x))))
                 pairs)))
    `(and ,@comps)))

(define (expand-lambda arg body last)
  (when debug (displayln (format "LAMBDA arg: ~A; body: ~A" arg body)))
  `(lambda ,arg ,@(map (compose inject-expr expand) body) ,(inject-return (expand last))))

(define (expand-operator op arg)
  (when debug (displayln (format "OPERATOR op: ~A; arg: ~A" op arg)))
  (let ((exp-op (if (list? op) (cons (car op) (map expand (cdr op))) op)))
    (if (null? arg)
        `(,exp-op)
        `(,exp-op ,@(map expand arg)))));(,(expand (to-lambda arg)))))))

(define (expand-application app arg)
  (when debug (displayln (format "APPLICATION app: ~A; arg: ~A" app arg)))
  (if (null? arg)
      `(,(expand app))
      `(,(expand app) ,@(map expand arg))));(,(expand (to-lambda arg))))))

(define (expand-begin body)
  (when debug (displayln (format "BEGIN body: ~A;" body)))
  (expand `((lambda () ,@body))))

(define (expand-when pred alt)
  (when debug (displayln (format "WHEN pred: ~A; alt: ~A" pred alt)))
  `(when ,(expand pred) ,(expand `(lambda () ,@alt))))

(define (expand-if pred alt1 alt2)
  (when debug (displayln (format "IF pred: ~A; alt1: ~A; alt2: ~A" pred alt1 alt2)))
  `(if ,(expand pred) ; '(pred)', а не '(pred ;)'
       ,(expand alt1)
       ,(expand alt2)))

(define (expand-cond preds conds)
  (when debug (displayln (format "COND preds: ~A; conds: ~A; " preds conds)))
  (define (cond-to-if ps cs)
    (if (not (null? ps))
        (match-let (((cons b bs) ps)
                    ((cons e es) cs))
          (if (eq? 'else b)
              `((lambda () ,@e))
              (if (null? e)
                  `(if ,b ,b ,(cond-to-if bs es))
                  `(if ,b ((lambda () ,@e)) ,(cond-to-if bs es)))))
        '(when #f #f))) ; тупик
  (expand (cond-to-if preds conds)))

(define (expand-quote form)
  (when debug (displayln (format "QUOTE form: ~A" form)))
  (cond ((null? form) '(list))
        ((symbol? form)
         `(string->symbol ,(symbol->string form))) ; nahua?
        ((pair? form)
         `(list ,@(map expand-quote form)))
        ((vector? form)
         `(vector ,@(map expand-quote (vector->list form))))
        (else form)))

(define (expand-define f args body)
  (when debug (displayln (format "DEFINE f: ~A; args: ~A; body: ~A" f args body)))
  `(define ,f ,(expand `(lambda ,args . ,body))))

(define (expand x)
  (match x
    ((list 'quote t) (expand-quote t))
    ((list 'define (cons f args) e1 ..1) (expand-define f args e1))
    ((list 'define name expr) `(define ,name ,(expand expr)))
    ((list 'lambda arg body ... last) (expand-lambda arg body last))
    ((list 'begin body ...) (expand-begin body))
    ((list 'when pred alt ...) (expand-when pred alt))
    ((list 'if pred alt1 alt2) (expand-if pred alt1 alt2))
    ((list 'cond (list pred expr ... ) ... ) (expand-cond pred expr))
    ((list op arg ...) #:when (member op op-bin) (expand-bin op arg))
    ((list op arg ...) #:when (member op op-cmp) (expand-cmp op arg))
    ((list op arg ...) #:when (is-op-lambda? op)(expand-application op arg))
    ((list op arg ...) (expand-operator op arg))
    (_ x)));(format "leaf ~A" x))))

;(expand '(+ 1 2 3))
;(expand '(+))
;(expand '(+ 1 2 (* 6 7 8)))
;
;(expand '(> 1 2))
;(expand '(> 1 2 3))
;
;(expand '((compose a b) 100))
;(expand '((lambda (x) x y z) 100))
;(expand '((lambda x x) 100))
;
;(expand '(begin 1 2 3))
;
;(expand '(when #t 1 2 3))
;(expand '(when (> a b) 1 2 3))
;
;(expand '(if #t 1 2))
;(expand '(if #t (begin 1 2 3) 4))

;(expand '(cond))
;(expand '(cond (#t)))
;(expand '(cond (#t 1 2 3)(#t 4 5 6)(#t 7 8 9)))
;(expand '(cond ((> 1 2) 1)(else 3)))

;(expand '(define x (lambda (x) (+ 10 x))))
;(expand '(define (f x) (+ 10 x)))
;(expand '(define (f) (begin 1 2 3)))