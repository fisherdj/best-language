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

(define list (lambda x x))

(define leak-env
  (define 5 5))

(define leak-rec-env
  (lambda-rec the-leak ()
	      (define 6 6)))
