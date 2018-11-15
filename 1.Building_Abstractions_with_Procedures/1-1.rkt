#lang sicp

(define (square x)
  (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
;(f 5)

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))
(define (abs2 x)
  (if (< x 0)
      (- x)
      x))
;(abs2 -2)

; 构造 >= 运算
(define (>= x y)
  (not (< x y)))
;(>= 1 0)

; E 1.2
;(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
;   (* 3 (- 6 2) (- 2 7)))
; -37/150

; E 1.3
(define (sum-of-squares-of-two-larger-numbers a b c)
  ; 判断较大的两个数
  (cond ((and (< a b) (< a c))
         (sum-of-squares b c))
        ((and (< b a) (< b c))
         (sum-of-squares a c))
        (else (sum-of-squares a b))))
;(sum-of-squares-of-two-larger-numbers 1 1 1)

; E 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
;(test 0 (p))

; sqrt
; 从任意值开始，重复 校验 -> 改进的过程
(define (sqrt x)
  
  (define (sqrt-iter guess)

    (define (good-enough?)
      (< (abs (- 1 (/ (improved-guess) guess))) 0.00000001))
    (define (improved-guess)
      (average guess (/ x guess)))
    
    ; 判断精度是否达到要求
    (if (good-enough?)
        guess
        ; 精度不达标时,改进该估计值
        (sqrt-iter (improved-guess))))
  
  (sqrt-iter 1.0))

(define (average a b)
  (/ (+ a b) 2))

; E 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;(sqrt 2.25)

; (new-if true (display "true") (display "bad"))

; E 1.7
(define (new-good-enough? guess new-guess x)
  (< (abs (- 1 (/ new-guess guess))) 0.00000001))
;(sqrt 40000000000000000000000)

; E 1.8
(define (cube-root x)
  (define (cube-root-iter guess x)
    (if (good-enough? guess (improve guess x) x)
        guess
        (cube-root-iter (improve guess x) x)))
  (define (good-enough? guess new-guess x)
    (< (abs (- 1 (/ new-guess guess))) 0.00000001))
  (define (improve y x)
    (/ (+ (/ x (square y)) (* 2 y)) 3))
  (cube-root-iter 1.0 x))

;(cube-root 8000000000000000000)












