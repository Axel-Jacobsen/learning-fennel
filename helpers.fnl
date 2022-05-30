;; Helper file for my Fennel
;; Implementing it all from scratch for fun!
(local fv (require :fennel.view))
(local u (require :fennel.utils))

(fn divisible? [v i] "true if v is divisible by i" (= 0 (% v i)))
(fn even? [v] "true if v is even" (divisible? v 2))
(fn odd? [v] "true if v is even" (not (even? v)))

(fn any [seq func]
  "true if (func x) is true for any x in seq"
  (if (= (# seq) 0)
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


;; filter, reduce

(fn sum [seq]
  (accumulate [s 0
               _ n (ipairs seq)]
    (+ s n)))

(fn filter [seq f]
  "filter out values where (not (f v)) for v in seq"
  (local res [])
  (each [_ v (ipairs seq)]
    (match (f v)
      x (if x (table.insert res v))))
  res)


;; Iterator

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

(fn take-iter! [n itr]
  (local res [])
  (for [_ 1 n]
    (table.insert res (itr)))
  res)

(lambda natural-numbers [?start]
  (lambda ns [n]
    (coroutine.yield n)
    (ns (+ n 1)))
  (coroutine.wrap (partial ns (or ?start 0))))

(fn fib-gen []
  "fibonacci number generator"
  (lambda fibs [?a ?b]
    (let [aa (or ?a 0)
          bb (or ?b 1)]
      (coroutine.yield (+ aa bb))
      (fibs bb (+ aa bb))))
  (coroutine.wrap fibs))

(fn prime-gen []
  (local itr (natural-numbers 2))
  (local filters [])
  (fn sieve []
    (let [x (filter-iter-chain! itr filters)]
      (coroutine.yield x)
      (table.insert filters #(not (divisible? $ x)))
      (sieve)))
  (coroutine.wrap sieve))

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

;; Debuggers
(lambda print-time [f ...]
  "print runtime of f"
  (print f "value and runtime:" (let [t0 (os.clock)
                              v (f ...)
                              t1 (os.clock)]
                          (values v (- t1 t0)))))


{: divisible?
 : even?
 : odd?
 : prime?
 : any
 : all
 : zero-if-not-divisible
 : sum
 : filter
 : filter-iter!
 : take-iter!
 : natural-numbers
 : fib-gen
 : prime-gen
 : print-time}
