(local {: map} (require :fennel.utils))
(local fv (require :fennel.view))

(local h (require :helpers))


;; PROBLEM 1
;; Find the sum of all the multiples of 3 or 5 below 1000
; immutable, tail-recursive version - cool!
(lambda p1 [?n ?s]
  (let [nn (or ?n 0)
        ss (or ?s 0)]
    (if (= nn 1000)
      ss
      (p1 (+ nn 1) (+ ss (h.zero-if-not-divisible nn 3 5))))))

(print (p1))

; The version below uses a mutable variable (`s`) and runs slightly faster than
; the above, but I like TCO and recursion so the one above is my favourite.
; (fn p1 []
;   (var s 0)
;   (for [i 1 999] (set s (+ s (zero-if-not-divisible i 3 5))))
;   s)


;; PROBLEM 2
; By considering the terms in the Fibonacci sequence whose values do not exceed four million, ; find the sum of the even-valued terms.
(fn p2 []
  (local f (h.fib-gen))  ; init the coroutine
  (fn rip [s]
    (let [v (f)]
      (if (< 4000000 v)
        s
        (rip (+ s (h.zero-if-not-divisible v 2))))))
  (rip 0))

(print (p2))

; I dislike this solution because it is essentially imperitive
; (fn p2 []
;   (var done? false)
;   (local f (fib-gen))  ; init the coroutine
;   (var s 0)
;   (while (not done?)
;     (var v (f))  ; get a new fib
;     (set done? (< 4000000 v))
;     (set s (+ s (zero-if-not-divisible v 2))))
;   s)

;; PROBLEM 3
; What is the largest prime factor of the number 600851475143?

(fn p3 []
  (local P 600851475143)
  (local sqrt_P (math.sqrt P))
  (local pg (h.prime-gen))
  (var max 0)
  (fn rip []
    (let [p (pg)]
      (if (h.divisible? P p)
        (set max p))
      (if (not (< p sqrt_P))
        max
        (rip))))
  (rip))

(print (p3))
