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

(define (f-iterative n)
  (define (iter f-i-2 f-i-1 f-i i)
    (define f-i+1
      (+ f-i
         (* 2 f-i-1)
         (* 3 f-i-2)))
    (if (= i n)
        f-i
        (iter f-i-1
              f-i
              f-i+1
              (+ i 1))))
  (if (< n 3)
      n
      (iter 0 1 2 2)))

; E 1.12
(define (pascal row col)
  (if (or (= col 1) (= col row))
      1
      (+ (pascal (- row 1) (- col 1))
         (pascal (- row 1) col))))

; E 1.14
; 所需空间的增长数量级为 θ(a+n)
; 递归树的最长链是 (cc a n)->(cc a n-1)->(cc a n-2)...->(cc a 1)->(cc a-1 1)->(cc a-2 1)...->(cc 0 1)
; 步骤次数的增长数量级为 ?

; E 1.15
; a. 5次
; b. 所需空间的增长数量级为 θ([log3(a/0.1)])，步骤次数的增长数量级也是 θ([log3(a/0.1)])


