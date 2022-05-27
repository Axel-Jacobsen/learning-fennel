; Find the sum of all the multiples of 3 or 5 below 1000

; mutable ver - more readable?
(fn p1 []
  (var s 0)
  (for [i 1 999]
    (if (or (= 0 (% i 5)) (= 0 (% i 3)))
      (set s (+ s i))))
  s)

; immutable, recursive ver - cooler?
(lambda p1p [?n ?s]
  (let [nn (or ?n 0)
        ss (or ?s 0)]
    (if (= nn 1000)
      ss
      (p1p (+ nn 1) (if (or (= 0 (% nn 5)) (= 0 (% nn 3)))
                      (+ ss nn)
                      ss)))))


(p1)
(p1p)

; By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
(lambda fibgen [?a ?b]
  (let [aa (or ?a 0)
        bb (or ?b 1)]
    (fibgen bb (+ aa bb))
    (coroutine.yield (+ aa bb))))

(fn fibs []
  (coroutine.wrap fibgen))

(each [_ value (pairs (fibs)) :until (< value 4000000)]
  (print value))
