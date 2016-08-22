;;
(define (sqrt-iter guess x last)
	(if (good-enough? guess x last)
		guess
	(sqrt-iter (improve guess x) x guess)))

(define (improve guess x)
	(average guess (/ x guess)))

(define (average x y)
	(/ (+ x y) 2))

(define (good-enough? guess x last)
	(< (abs (- guess last)) (* guess 0.000000000000001)))

(define (sqrt x)
	(sqrt-iter 1.0 x x))

;;
(define (cbrt x)
	(cbrt-iter 1.0 x x))

(define (cbrt-iter guess x last)
	(if (good-enough? guess x last)
		guess
	(cbrt-iter (improve-cbrt guess x) x guess)))

(define (improve-cbrt guess x)
	(/ (+ (/ x (square guess)) (* 2 guess)) 3))
	
;; 1.11
(define (f n)
	(if (< n 3)
		n
		(+ (+ (f (- n 1)) (* (f (- n 2)) 2)) (* (f (- n 3)) 3))))
		
(define (f-iter a b c n)
	(if (= n 3)
		(+ (+ a (* 2 b)) (* 3 c))
		(f-iter (+ (+ a (* 2 b)) (* 3 c)) a b (- n 1))))
		
;; 1.12
(define (pasc r c)
	(cond 
		((or (< r c) (< c 1)) 0)
		((and (= r 1) (= c 1)) 1)
		(else (+ (pasc (- r 1) c) (pasc (- r 1) (- c 1))))))

;; 1.16
(define (even? n)
	(= (remainder n 2) 0))

(define (exp-iter b n a)
	(cond ((= n 0) a)
		((even? n) (exp-iter (square b) (/ n 2) a))
		(else (exp-iter b (- n 1) (* a b)))))

;; 1.17
(define (double a)
	(* 2 a))

(define (halve a)
	(/ a 2))

(define (fast-mult a b)
	(cond ((= b 1) a)
		((even? b) (double (fast-mult a (/ b 2))))
		(else (+ a (fast-mult a (- b 1))))))

;; 1.18
(define (fast-mult-iter a b c)
	(cond ((= b 0) c)
		((even? b) (fast-mult-iter (double a) (/ b 2) c))
		(else (fast-mult-iter a (- b 1) (+ c a)))))

;; 1.21
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))

;; 1.22
(define (prime? n)
	(= n (smallest-divisor n)))
(define (timed-prime-test n)
	(start-prime-test n (runtime)))
(define (start-prime-test n start-time)
	(if (fast-prime? n 100)
		(report-prime n (- (runtime) start-time))
		#f))
(define (report-prime n elapsed-time)
	(newline)
	(display n)
	(display " *** ")
	(display elapsed-time)
	#t)
(define (search-for-primes n num)
	(cond ((= num 0) )
	((= (modulo n 2) 0) (search-for-primes (+ n 1) num))
	((timed-prime-test n) (search-for-primes (+ n 2) (- num 1)))
	(else (search-for-primes (+ n 2) num))))

;; 1.23
(define (next n)
	(if (= n 2) 3 (+ n 2)))

;; 1.24
(define (fast-prime? n times)
	(cond ((= times 0) true)
		((mr-test n) (fast-prime? n (- times 1)))
		(else false)))
(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))
(define (expmod base exp m)
	(cond ((= exp 0) 1)
		((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
		(else (remainder (* base (expmod base (- exp 1) m)) m))))

;; 1.29
(define (cube x) (* x x x))

(define (simpson f a b n)
	(define (odd? n)
		(= (modulo n 2) 1))
	(define h
		(/ (- b a) n))
	(define (yk k)
		(f (+ a (* k h))))
	(define (coeff k)
		(cond ((or (= k 0) (= k n))
			1.0)
		((odd? k)
			4.0)
		(else
			2.0)))
	(define (y-terms count)
		(if (> count n)
			0
			(+ (* (coeff count) (yk count)) (y-terms (+ count 1)))))
	(* (/ h 3.0) (y-terms 0)))

;; 1.30
(define (inc n) (+ n 1))
(define (sum term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ result (term a)))))
	(iter a 0))
(define (sum-cubes a b)
	(sum cube a inc b))

;; 1.31
(define (fact-term n)
	n)	
(define (product term next a b)
	(if (> a b)
		1
		(* (term a) (product term next (next a) b))))
(define (factorial-prod n)
	(product fact-term inc 1 n))
(define (plus-two n)
	(+ n 2))
(define (pi-term n)
	(* (/ (- n 1) n) (/ (+ n 1) n)))
(define pi-approx
	(* 4.0 (product pi-term plus-two 3 10000)))
(define (product-iter term next a b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (* result (term a)))))
	(iter a 1))
(define (factorial-prod-iter n)
	(product-iter fact-term inc 1 n))

;; 1.32
(define (accumulate combiner null-value term a next b)
	(if (> a b)
		null-value
		(combiner (term a) (accumulate combiner null-value term (next a) next b))))
(define (add-acc a b)
	(+ a b))
(define (mult-acc a b)
	(* a b))
(define (sum-acc term a next b)
	(accumulate add-acc 0 term a next b))
(define (sum-cubes-acc a b)
	(sum-acc cube a inc b))
(define (product-acc term a next b)
	(accumulate-iter mult-acc 1 term a next b))
(define (factorial-acc n)
	(product-acc fact-term 1 inc n))
(define (accumulate-iter combiner null-value term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (combiner (term a) result))))
	(iter a null-value))

;; 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
	(cond ((> a b)
			null-value)
		((filter a)
			(combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter)))
		(else
			(filtered-accumulate combiner null-value term (next a) next b filter))))
(define (sum-prime-square a b)
	(filtered-accumulate add-acc 0 square a inc b prime?))

;; 1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(display guess)
		(newline)
		(let ((next (f guess)))
		(if (close-enough? guess next)
			next
			(try next))))
	(try first-guess))

(define (golden-ratio)
	(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;; 1.36
(define (x-fixed-pt)
	(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

(define (x-fixed-pt-damp)
	(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 8.0))

;; 1.37
(define (cont-frac n d k)
	(define (term i)
		(if (>= i k)
			(/ (n i) (d i))
			(/ (n i) (+ (d i) (term (+ i 1))))))
	(define (term-iter i ans)
		(if (<= i 1)
			(/ (n 1) ans)
			(term-iter (- i 1) (+ (d (- i 1)) (/ (n i) ans)))))
	(term 1))
	;;(term-iter k (d k)))

(define (one-over-golden k)
	(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k))

;; 1.38
(define (e-cf)
	(define (d-func i)
		(if (or (= i 1) (not (= (modulo (- i 2) 3) 0)))
			1
			(* (+ (/ (- i 1) 3) 1) 2)))
	(+ 2 (cont-frac (lambda (i) 1.0) d-func 1000)))

;; 1.39
(define (tan-cf x-int k)
	(let ((x (exact->inexact x-int)))
	(cont-frac (lambda (i) (if (= i 1) x (- (expt x 2)))) (lambda (i) (+ i (- i 1))) k)))

