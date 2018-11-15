#lang sicp

(define (factorial n)
  (define (fact-iter i f)
    (if (> i n)
        f
        (fact-iter (+ i 1) (* f i))))
  (fact-iter 1 1))

(define (sum-of-1-to-n n)
  (define (iter i sum)
    (if (> i n)
        sum
        (iter (+ i 1) (+ sum i))))
  (iter 1 0))

; 定义通用循环过程,参数:计数器定义,结束条件,迭代后处理
(define (iter i-init i-stop procedure result-init)
  (define (iter-proc i result)
    (if (= i i-stop)
        result
        (iter-proc (+ i 1) (procedure i result))))
  (iter-proc i-init result-init))
; 尝试失败


; -- 1.2.2 --

; E 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f-iterative n))





