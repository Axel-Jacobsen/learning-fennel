;; Helper file for my Fennel
;; Implementing it all from scratch for fun!
(local fv (require :fennel.view))
(local u (require :fennel.utils))

(fn divisible? [v i] "true if v is divisible by i" (= 0 (% v i)))
(fn even? [v] "true if v is even" (divisible? v 2))
(fn odd? [v] "true if v is even" (not (even? v)))
(fn empty? [seq] "true if seq is empty" (= (# seq) 0))
(fn head [seq] "first el of seq" (. seq 1))
(fn tail [seq] "rest of seq" (table.unpack seq 2))
(fn bool->int [bool] "false -> 0, true -> 1" (if bool 1 0))

(fn gcd [a b]
  "greatest common divisor"
  (if
    (< a b) (gcd b a)
    (divisible? a b) b
    (gcd b (% a b))))

(fn lcm [a b]
  "lowest common multiple"
  (* a (/ b (gcd a b))))

(fn any [seq func]
  "true if (func x) is true for any x in seq"
  (if (empty? seq)
    false
    (let [[x & xs] seq]
      (if (func x)
        true
        (any xs func)))))

(fn all [seq func]
  "true if (func x) is true for all x in seq"
  (not (any seq #(not (func $)))))

; this could be more general
; smth like val * kroneker-delta(f(val, seq)) where f returns a bool
(lambda zero-if-not-divisible [v ...]
  "v if v is divisible by any of the subsequent args, else 0"
  (if (any [...] #(divisible? v $)) v 0))

(fn sum [seq]
  (accumulate [s 0
               _ n (ipairs seq)]
    (+ s n)))

(fn prod [seq]
  (accumulate [s 1
               _ n (ipairs seq)]
    (* s n)))

(fn max [seq]
  (accumulate [s (. seq 1)
               _ n (ipairs seq)]
    (math.max s n)))

(fn min [seq]
  (accumulate [s (. seq 1)
               _ n (ipairs seq)]
    (math.min s n)))

(fn filter [seq f]
  "filter out values where (not (f v)) for v in seq"
  (local res [])
  (each [_ v (ipairs seq)]
    (match (f v)
      x (if x (table.insert res v))))
  res)

(fn count [seq v]
  "return number of instances of v in seq"
  (accumulate [s 0
               _ e (ipairs seq)]
    (+ s (bool->int (= v e)))))

;; Iterator

(fn iter-collect! [itr ?f]
  "writing this manually because I cannot for the life of me understand why icollect isn't working"
  (local f (or ?f #$))
  (local vs [])
  (each [e itr]
    (table.insert vs (f e)))
  vs)

(fn filter-iter! [itr f]
  "filter out values where (not (f v)) for v in iter - do not use in excessively large chains"
  ; Stacking calls to filter-iter! is leading to stack overflow. POC:
  ;   (var ii (natural-numbers 2))
  ;   (for [i 1 200]
  ;     (set ii (filter-iter! ii #(not (divisible? $ i)))))
  ;   (ii)
  ; Use filter-iter-chain! instead
  (fn g []
    (let [v (itr)]
      (match (f v)
        x (if x (coroutine.yield v))))
    (g))
  (coroutine.wrap g))

(fn filter-iter-chain! [itr fs]
  "filter out values where (not (all (f v))) for f in fs for v in itr - does not return an iterator"
  (fn g []
    (let [v (itr)]
      (if (all fs #($ v))
        v
        (g))))
  (g))

(fn take-iter! [itr n]
  (local res [])
  (for [_ 1 n]
    (table.insert res (itr)))
  res)

(fn ith! [itr n]
  (assert (< 0 n))
  (fn rip [i]
    (let [v (itr)]
      (if (= i n)
        v
        (rip (+ i 1)))))
  (rip 1))

(fn arrangements [seq]
  (fn ps [sq]
    (if
      (empty? sq) (coroutine.yield [])
      (each [i e (ipairs sq)]
        (table.remove sq i)
        (local inner_pset (arrangements sq))
        (each [ee inner_pset]
          (do
            (table.insert ee 1 e)
            (coroutine.yield ee)))
        (table.insert sq i e))))
  (coroutine.wrap (partial ps seq)))

;; Number things
(fn natural-numbers-coroutine [?start ?end ?step]
  "natural numbers from 0 or start to infinity or end (inclusive)"
  (fn ns [n end step]
    (coroutine.yield n)
    (if
      (or (= end nil) (< n end)) (ns (+ n step) end step) ; end is nil or n < end
      (coroutine.yield nil))) ; n > end, stop iteration
  (coroutine.wrap (partial ns (or ?start 0) ?end (or ?step 1))))

(fn natural-numbers [?start ?end ?step]
  "natural numbers from 0 or start to infinity or end (inclusive)"
  (local step (or ?step 1))
  (local end ?end)
  (var n (- (or ?start 0) step))  ; pre-emptively subtract step
  (fn ns []
   (if
      (or (= end nil) (< n end)) (do
                                   (set n (+ n step))
                                   n) ; end is nil or n < end
      nil)) ; n > end, stop iteration
  ns)

(fn fib-gen []
  (var a 0)
  (var b 1)
  (fn fibs []
    (do
      (local tmp a)
      (set a b)
      (set b (+ tmp b))
      (+ a b)))
  fibs)

(fn prime-gen []
  (local itr (natural-numbers 3 nil 2))
  (local filters [])
  (fn sieve []
    (let [x (filter-iter-chain! itr filters)]
      (coroutine.yield x)
      (table.insert filters #(not (divisible? $ x)))
      (sieve)))
  (coroutine.wrap (lambda [] (do
                               (coroutine.yield 2)
                               (sieve)))))

(fn prime? [n]
  (local pg (prime-gen))
  (fn inner []
    (let [p (pg)]
      (if
        (< n 0) true
        (= n p) true
        (divisible? n p) false
        (inner))))
  (inner n))

(fn prime-factors [P]
  (local factors [])
  (fn rip [n d]
    (if
      (= 1 n) factors
      (divisible? n d) (do
                         (table.insert factors d)
                         (rip (/ n d) d))
      (rip n (+ 1 d))))
  (rip P 2))


;; Debuggers
(lambda print-time [f ...]
  "print runtime of f"
  (print f "value and runtime:" (let [t0 (os.clock)
                                      v (f ...)
                                      dt (- (os.clock) t0)]
                                  (values v dt))))

;; String manipulation
(fn str-idx [s i]
  (string.sub s i i))

(fn split [s]
  (let [t []]
    (string.gsub s "." #(table.insert t $))
    t))

(fn palindrome? [str]
  (if
    (empty? str) true
    (= (str-idx str 1) (str-idx str -1)) (palindrome? (string.sub str 2 -2))
    false))

;; Exports
{: divisible?
 : even?
 : odd?
 : prime?
 : empty?
 : palindrome?
 : filter-iter!
 : take-iter!
 : iter-collect!
 : ith!
 : head
 : tail
 : gcd
 : lcm
 : count
 : max
 : min
 : any
 : all
 : zero-if-not-divisible
 : sum
 : prod
 : filter
 : natural-numbers
 : fib-gen
 : str-idx
 : split
 : prime-gen
 : prime-factors
 : print-time}
