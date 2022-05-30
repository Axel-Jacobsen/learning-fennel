(local h (require :helpers))
(local fv (require :fennel.view))


;; PROBLEM 1
;; Find the sum of all the multiples of 3 or 5 below 1000
; immutable, tail-recursive version - cool!
(lambda p1 [?n ?s]
  (let [nn (or ?n 0)
        ss (or ?s 0)]
    (if (= nn 1000)
      ss
      (p1 (+ nn 1) (+ ss (h.zero-if-not-divisible nn 3 5))))))

(h.print-time p1)


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

(h.print-time p2)


;; PROBLEM 3
; What is the largest prime factor of the number 600851475143?
(fn p3 [] (h.max (h.prime-factors 600851475143)))

(h.print-time p3)
