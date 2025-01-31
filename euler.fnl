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

(h.print-time p1 1)


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

(h.print-time p2 2)


;; PROBLEM 3
; What is the largest prime factor of the number 600851475143?
(fn p3 [] (h.max (h.prime-factors 600851475143)))

(h.print-time p3 3)


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

(h.print-time p4 4)


;; PROBLEM 5
; What is the smallest positive number that is evenly
; divisible by all of the numbers from 1 to 20?
; (i.e. LCM 1..20)
(fn p5 []
  (local nats (h.natural-numbers 1 20))
  (accumulate [lcm 1
               v nats]
    (h.lcm lcm v)))

(h.print-time p5 5)


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

(h.print-time p6 6)


;; PROBLEM 7
; What is the 10001st prime number?
; Current runtime is 755 seconds ~= 12 min 30 sec
; need to come back and make my sieve better!!
(fn p7 []
  (let [ps (h.prime-gen 1000000)]
    (h.ith! ps 10001)))

(h.print-time p7 7)

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

(h.print-time p8 8)


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

(h.print-time p9 9)


;; PROBLEM 10
; Find the sum primes below 2 million
(fn p10 []
  (accumulate [s 0
               p (h.prime-gen 2000000)]
    (+ s p)))

(h.print-time p10 10)


;; PROBLEM 12
; What is the value of the first triangle number to have over five hundred divisors?
(fn p12 []
  (local n (h.natural-numbers 1))
  (fn rip [s]
    (let [csum (+ s (n))
          nd (h.num-divisors csum)]
      (if (< 500 nd)
        csum
        (rip csum))))
  (rip 0))

(h.print-time p12 12)


;; PROBLEM 14
; Find longest collatz sequence starting below 1 million
(fn p14 []

  (local tbl {})

  (fn collatz [n]
    (if
      (?. tbl n) (. tbl n)
      (let [res (if
                  (= n 1) 1
                  (h.even? n) (+ 1 (collatz (/ n 2)))
                  (+ 1 (collatz (+ (* 3 n) 1))))]
        (tset tbl n res)
        res)))

  (var max 0)
  (var argmax 0)
  (each [n (h.natural-numbers 1 999999)]
    (let [cl (collatz n)]
      (if (< max cl)
        (do
          (set max cl)
          (set argmax n)))))
  argmax)

(h.print-time p14 14)


;; PROBLEM 15
; How many (only right or down) routes are there through a 20×20 grid?
; (40 choose 20)
;
(fn p15 []
  (local num (h.natural-numbers 21 40))
  (local denom (h.natural-numbers 1 20))
  (accumulate [p 1
               n d (h.zip num denom)]
    (* p (/ n d))))

(h.print-time p15 15)

;; PROBLEM 22
; Sort the names and go from there
(fn p22 []
  (let [data (with-open [fin (io.open :data_inputs/p22_names.txt)]
               (h.split (fin:read :*a) "(%w+)"))
        letters (collect [i v (ipairs (h.split "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))] (values v i))]
    (table.sort data)
    (accumulate [s 0
                 i name (ipairs data)]
      (+ s (* i (h.sum (map (h.split name) #(. letters $))))))))

(h.print-time p22 22)


;; PROBLEM 25
; Find the index of the first 1000 digit Fibonacci Number
(fn p25 []
  (let [phi (/ (+ 1 (math.sqrt 5)) 2)]
    (math.ceil (/
                (+ (/ (math.log (math.sqrt 5)) (math.log 10)) (- 1000 1))
                (/ (math.log phi) (math.log 10))))))

(h.print-time p25 25)

;; PROBLEM 40
(fn p40 []
  (local interesting-digits [1 10 100 1000 10000 100000 1000000])
  (local N (h.natural-numbers 1))
  (var current-digits [])
  (fn get-next-digit []
    (if (h.empty? current-digits)
      (do
        (set current-digits (h.split (tostring (N))))
        (get-next-digit))
      (tonumber (table.remove current-digits 1))))

  (accumulate [s 1
               i (h.natural-numbers 1 1000000)]
    (let [e (get-next-digit)]
      (* s (if (h.in interesting-digits i) e 1)))))

(h.print-time p40 40)


;; PROBLEM 42
; tn = 0.5 * n * (n + 1)
; c = {-1 \pm \sqrt{1 + 8c}} / 2
(fn p42 []
  (let [data (with-open [fin (io.open :data_inputs/p42_words.txt)]
               (h.split (string.gsub (fin:read :*a) "\"" "") "(%w+)"))
        letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
    (length (h.filter data (fn [word]
                             (h.triangle-num?
                               (h.sum
                                 (map
                                   (h.split word)
                                   #(string.find letters $)))))))))

(h.print-time p42 42)


;; PROBLEM 49
; The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
; increases by 3330, is unusual in two ways: (i) each of the three terms
; are prime, and, (ii) each of the 4-digit numbers are permutations of
; one another.

; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
; primes, exhibiting this property, but there is one other 4-digit increasing sequence.

; What 12-digit number do you form by concatenating the three terms in this sequence?
(fn is-perm [s1 s2]
  (let [s1lst (h.instances (h.split (tostring s1)))
        s2lst (h.instances (h.split (tostring s2)))]
      (h.deep-eq s1lst s2lst)))

(fn p49 []
  (let [primes (h.primes-between 1000 10000)]

    ))

(h.print-time p49 49)
