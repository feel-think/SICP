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


; -- 1.2.4 --

(define (expt b n)
  (define (expt-iter n-remains product)
    (if (= n-remains 0)
        product
        (expt-iter (- n-remains 1)
                   (* b product))))
  (expt-iter n 1))

(define (even? x)
  (= (remainder x 2) 0))
(define (square x) (* x x))

(define (fast-expt-1 b n)
  (cond ((= n 0) 1)
        ((even? n)
         (square (fast-expt-1 b (/ n 2))))
        (else (* b (fast-expt-1 b (- n 1))))))

; E 1.16
(define (fast-expt b n)
  (define (expt-iter x n-remains product)
    (cond ((= n-remains 0)
           product)
          ((even? n-remains)
           (expt-iter (square x)
                      (/ n-remains 2)
                      product))
          (else
           (expt-iter x
                      (- n-remains 1)
                      (* product x)))))
  (expt-iter b n 1))

(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))

; E 1.18
(define (multiply a b)
  (define (iter x y sum)
    (cond ((= y 0)
           sum)
          ((even? y)
           (iter (double x) (halve y) sum))
          (else
           (iter x (- y 1) (+ sum x)))))
  (iter a b 0))

; E 1.19
; 加快斐波那契数计算的思路为：
; 将一次转换视为一个元转换 T0，当剩余的迭代次数为偶数时，将两个元转换 T0 合并为一个新的转换 T1
; 同时，所需的迭代次数就减半了。
; 若剩余迭代次数为奇数，就使用当前的转换式 Tk 进行一次转换，同时剩余迭代次数减一
; 直至剩余迭代次数为 0 时，当前的 a 即为所需的斐波那契数
; 对 a, b 应用两次 Tpq 转换，对照 T 的形式，得到新的转换 Tp'q' 的参数如下：
; p' = p^2 + q^2
; q' = q^2 + 2pq

; 树递归方法
(define (fib1 n)
  (define (iter f.i-1 f.i i)
    (if (= n i)
        f.i
        (iter f.i (+ f.i-1 f.i) (+ i 1))))
  (if (< n 2)
      n
      (iter 0 1 1)))

; 线性迭代方法
(define (fib n)
  ; 三种情况：
  ; 1. 剩余迭代次数 i 为 0，返回 a
  ; 2. i 为偶数，计算新的 T，i 减半
  ; 3. i 为奇数，对 (a, b) 进行一次转换，i 减一
  ; 迭代过程中只需保存 p q 即可构建出 T
  ; a = fib(n), b= fib(n-1), i 为剩余的迭代次数
  (define (iter a b p q i)
    (cond ((= i 0)
           a)
          ((even? i)
           (iter a
                 b
                 (+ (square p) (square q))
                 (+ (square q) (* 2 p q))
                 (halve i)))
          (else
           (iter (+ (* b q) (* a q) (* a p))
                 (+ (* b p) (* a q))
                 p
                 q
                 (- i 1)))
          ))
  (if (< n 2)
      n
      (iter 1 0 0 1 (- n 1))))