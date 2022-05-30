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


;; PROBLEM 4
; What is the largest palindrome that is a product of two three-digit numbers
(fn check-palindrome [str]
  (if
    (h.empty? str) true
    (= (h.str-idx str 1) (h.str-idx str -1)) (check-palindrome (string.sub str 2 -2))
    false))

(fn p4 []
  ; gross brute force, but fennel is fast enough
  ; ideally we iterate through pairs of three
  ; digit numbers in order of decreasing product,
  ; but I am not sure how to do this.
  (local brute-force-results [])
  (fn rip [n1 n2]
    (if (and (< 0 n1) (< 0 n2))
      (let [sn (tostring (* n1 n2))]
        (if (check-palindrome sn)
            (table.insert brute-force-results (* n1 n2)))
        (if (= n1 100)
          (rip 999 (- n2 1))
          (rip (- n1 1) n2)))))
  (do
    (rip 999 999)
    (h.max brute-force-results)))

(h.print-time p4)
