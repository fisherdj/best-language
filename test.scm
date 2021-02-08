(cons (cons ((vau (a) e a) quote)
            (vau (a) e a))
      ((vau () e e)))

(cons (cons 'define
            (vau (name exp) env
                 (cons (cons name (eval exp env))
                       env)))
      ((vau () e e)))

(define current-env (vau () e e))

(define self-eval (vau-rec self-eval () #f self-eval))

(define all-args (vau-rec rec arg env (cons rec (cons arg env))))

(define list
  (vau-rec list a e
           (if (pair? a)
               (cons (eval (car a) e)
                     (eval (cons list (cdr a)) e))
               (if (null? a)
                   ()
                   (car 0)))))

(define with
  (vau (name expr body) env
       (eval body
             (cons (cons name (eval expr env))
                   env))))

(define length
  (vau-rec length (a) e
           (with r (eval a e)
                 (if (pair? r)
                     (+ 1 (eval (list length (list quote (cdr r)))
                                ()))
                     0))))

(define size
  (vau-rec size (a) e
           (with r (eval a e)
                 (if (pair? r)
                     (+ (eval (list size (list quote (car r)))
                              ())
                        (eval (list size (list quote (cdr r)))
                              ()))
                     1))))

;; necessary to force eagerness in Haskell
;; is the behavior seen with (loop) a bug?
(define force
  (vau (a) e
       (with r (eval a e)
             (if (size r) r 0))))

(define vau-loop
  (vau-rec vau-loop () #f
           (eval (list vau-loop) ())))

;; would like to put error here, but would require implementing error primitive

(define lambda
  (vau (arglist body) env
       (with f (eval (list vau arglist #f body) env)
             (vau true-arg true-env
                  (eval (cons f
                              (eval (cons list true-arg)
                                    true-env))
                        ())))))

;; (define lambda-rec
;;   (vau (name arglist body) env
;;        (vau-rec true-rec true-arg true-env
;;                 (eval (cons (eval (list vau arglist #f body)
;;                                   (force (cons (cons name true-rec)
;;                                                env)))
;;                             (force (eval (cons list true-arg) true-env)))
;;                       ()))))

(define lambda-rec
  (vau (name arglist body) env
       (vau-rec true-rec true-arg true-env
                (eval (cons (eval (list vau arglist #f body)
                                  (cons (cons name true-rec)
                                        env))
                            (eval (cons list true-arg) true-env))
                      ()))))

(define defun
  (vau (name arglist body) env
       (eval (list define name
                   (list lambda-rec name arglist body))
             env)))

(defun combine (f arg env)
  (eval (cons f arg) env))

(define loop
  (lambda-rec loop () (loop)))

(define factorial
  (lambda-rec factorial (n)
              (if (= n 0)
                  1
                  (* n (factorial (- n 1))))))

(define factorial-iter
  (lambda-rec factorial (acc n)
              (if (= n 0)
                  acc
                  (factorial (* acc n) (- n 1)))))

(define not
  (lambda (x)
    (if x #f #t)))

(define bool->string
  (lambda (x)
    (if x "#t" "#f")))

;; (define list (lambda x x))

(define leak-env
  (define 5 5))

(define leak-rec-env
  (lambda-rec the-leak ()
              (define 6 6)))

(define begin
  (vau-rec begin l env
           (with r (eval (car l) env)
                 (if (null? (cdr l))
                     r
                     (eval (cons begin (cdr l)) env)))))

(defun read ()
  (effect 'read))

(defun write (x)
  (effect (list 'write x)))

(define repl
  (lambda-rec repl (env)
              (begin (write '>)
                     (write (eval (read) env))
                     (repl env))))

(define foldl
  (lambda-rec foldl (f init l)
              (if (null? l)
                  init
                  (foldl f (f (car l) init) (cdr l)))))

(defun reverse (l) (foldl cons () l))
(defun foldr (f init l) (foldl f init (reverse l)))

(defun app-2 (x y)
  (foldr cons y x))

(defun append x
  (foldr app-2 () x))

(define map
  (lambda-rec map (f l)
              (if (null? l)
                  ()
                  (cons (f (car l)) (map f (cdr l))))))

(defun apply (f l)
  (eval (cons f
              (map (lambda (x) (list quote x))
                   l))
        ()))

(define filter
  (lambda-rec filter (f l)
              (if (null? l)
                  ()
                  (if (f (car l))
                      (cons (car l)
                            (filter f (cdr l)))
                      (filter f (cdr l))))))

(defun choose x
  (effect (cons 'choose x)))

(defun choose-bool x
  (choose #t #f))

(defun fail ()
  (effect 'fail))

(define nd-run-all-rec
  (vau-rec nd-run-all-rec (exp) env
           (capture eff cont
                    (if (if (pair? eff)
                            (= (car eff) 'choose)
                            #f)
                        ;; (apply append
                        ;;        (map cont (cdr eff)))
                        (apply append
                               (map (lambda (c)
                                      (nd-run-all-rec (cont c)))
                                    (cdr eff)))
                        (if (= eff 'fail)
                            ()
                            (with r (effect eff)
                                  (nd-run-all-rec (cont r)))))
                    (eval exp env))))

(define nd-run-all
  (vau (exp) env
       (eval (list nd-run-all-rec (list list exp))
             env)))

(define powerset
  (lambda (l)
    (nd-run-all (filter choose-bool l))))
;; (nd-run-all (with r (cons (choose-bool) (choose-bool)) (if (car r) (fail) (if (cdr r) r (fail)))))

(defun gentag ()
  (effect 'gentag))

(define with-tags-from
  (vau-rec with-tags-from (n-exp exp) env
           (with n (eval n-exp env)
                 (capture eff cont
                          (if (= eff 'gentag)
                              (with-tags-from (+ n 1) (cont n))
                              (with r (effect eff)
                                    (with-tags-from n (cont r))))
                          (eval exp env)))))

(define with-tags
  (vau (exp) env
       (with-tags-from 0 (eval exp env))))
;; (with-tags (repl (current-env)))
