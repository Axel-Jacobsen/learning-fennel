(local h (require :helpers))
(local fv (require :fennel.view))
(local {: map} (require :fennel.utils))


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
(fn p4 []
  ; gross brute force, but fennel is fast enough
  ; ideally we iterate through pairs of three
  ; digit numbers in order of decreasing product,
  ; but I am not sure how to do this.
  (local brute-force-results [])
  (fn rip [n1 n2]
    (if (and (< 0 n1) (< 0 n2))
      (let [sn (tostring (* n1 n2))]
        (if (h.palindrome? sn)
          (table.insert brute-force-results (* n1 n2)))
        (if (= n1 100)
          (rip 999 (- n2 1))
          (rip (- n1 1) n2)))))
  (do
    (rip 999 999)
    (h.max brute-force-results)))

(h.print-time p4)


;; PROBLEM 5
; What is the smallest positive number that is evenly
; divisible by all of the numbers from 1 to 20?
; (i.e. LCM 1..20)
(fn p5 []
  (local nats (h.natural-numbers 1 20))
  (accumulate [lcm 1
               v nats]
    (h.lcm lcm v)))

(h.print-time p5)


;; PROBLEM 6
; Find the difference between the sum of the squares of the first
; one hundred natural numbers and the square of the sum.
(fn p6 []
  ; consuming iterators, so we need 2!
  (local nats1 (h.natural-numbers 1 100))
  (local nats2 (h.natural-numbers 1 100))
  (- (^ (accumulate [s 0
                     v nats2]
          (+ s v)) 2)
     (accumulate [ss 0
                  v nats1]
       (+ ss (^ v 2)))
     ))

(h.print-time p6)


;; PROBLEM 7
; What is the 10001st prime number?
; Current runtime is 755 seconds ~= 12 min 30 sec
; need to come back and make my sieve better!!
(fn p7 []
  (let [ps (h.prime-gen 1000000)]
    (h.ith! ps 10001)))

(h.print-time p7)

;; PROBLEM 8
; find the largest product of 7 consecutive digits in this big ol num
(fn p8 []
  (let [data (with-open [fin (io.open :data_inputs/p8.txt)]
               (string.gsub (fin:read :*a) "\n" ""))
        window-len 13
        nats (h.iter-collect! (h.natural-numbers 1 (- (# data) window-len)))]
    (h.max
      (map
        nats
        (fn [i]
          (h.prod (map
                    (h.split (string.sub data i (+ i (- window-len 1))))
                    tonumber)))))
    ))

(h.print-time p8)


;; PROBLEM 9
; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
; Find the product abc.
(fn p9 []
  ; we know c > b and a
  ; Loop from c = 1000 to 1
  ; b from c to 1
  ; a =  1000 - b - c
  ; check for pyth trippy
  (fn rip [c b]
    (let [a (- (- 1000 c) b)]
      (if
        (= (^ c 2) (+ (^ b 2) (^ a 2))) (h.prod [a b c])
        (if (= b 1)
          (rip (- c 1) (- (- 1000 c) 1))
          (rip c (- b 1))))))
  (rip 998 1))

(h.print-time p9)


;; PROBLEM 10
; Find the sum primes below 2 million
(fn p10 []
  (accumulate [s 0
               p (h.prime-gen 2000000)]
    (+ s p)))

(h.print-time p10)
