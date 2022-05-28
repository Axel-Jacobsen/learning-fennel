(local u (require :utils))

;; PROBLEM 1
;; Find the sum of all the multiples of 3 or 5 below 1000
; immutable, tail-recursive version - cool!
(lambda p1 [?n ?s]
  (let [nn (or ?n 0)
        ss (or ?s 0)]
    (if (= nn 1000)
      ss
      (p1 (+ nn 1) (+ ss (u.zero-if-not-divisible nn 3 5))))))

(print (p1))

; The version below uses a mutable variable (`s`) and runs slightly faster than
; the above, but I like TCO and recursion so the one above is my favourite.
; (fn p1 []
;   (var s 0)
;   (for [i 1 999] (set s (+ s (u.zero-if-not-divisible i 3 5))))
;   s)


;; PROBLEM 2
; By considering the terms in the Fibonacci sequence whose values do not exceed four million, ; find the sum of the even-valued terms.
; I dislike this solution because it is essentially imperitive
(fn p2 []
  (var done? false)
  (local f (u.fibs))
  (var s 0)
  (while (not done?)
    (var v (f))
    (set done? (< 4000000 v))
    (set s (+ s (u.zero-if-not-divisible v 2))))
  s)

(print (p2))
