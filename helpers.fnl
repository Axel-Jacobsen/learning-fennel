;; Helper file for my Fennel
;; Implementing it all from scratch for fun!

; What are the meaningful differences between
; a coroutine vs closure? Is one faster?

(local fv (require :fennel.view))
(local u (require :fennel.utils))

;; Basics
(fn != [e1 e2] (not (= e1 e2)))
(fn is-table? [maybe-table] (= "table" (type maybe-table)))
(fn divisible? [v i] "true if v is divisible by i" (= 0 (% v i)))
(fn even? [v] "true if v is even" (divisible? v 2))
(fn odd? [v] "true if v is even" (not (even? v)))
(fn empty? [seq] "true if seq is empty" (= (# seq) 0))
(fn head [seq] "first el of seq" (. seq 1))
(fn tail [seq] "rest of seq" (table.unpack seq 2))
(fn bool->int [bool] "false -> 0, true -> 1" (if bool 1 0))
(fn int->bool [int]  "0 -> false, else true" (if (= 0 int) false true))
(fn int? [int] "1 if int is an integer else 0" (= int (// int 1)))

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

(fn in [seq value]
  (any seq #(= $ value)))


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
  (if (= (type seq) "function") (accumulate [s 1
                                             n seq]
                                  (* s n))
    (= (type seq) "table") (accumulate [s 1
                                        _ n (ipairs seq)]
                             (* s n))
    (do
      (assert false (.. "invalid seq type for prod - " (type seq))))))

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

(fn ones [size]
  (assert (< 0 size))
  (local v [])
  (for [_ 1 size]
    (table.insert v 1))
  v)

(fn zeros [size]
  (assert (< 0 size))
  (local v [])
  (for [_ 1 size]
    (table.insert v 0))
  v)

(fn instances [seq]
  "return a map from element to number of instances in seq"
  (local instance-map {})
  (each [_ e (ipairs seq)]
    (let [ee (tostring e)]
      (if (?. instance-map ee)
        (tset instance-map ee (+ 1 (. instance-map ee)))
        (tset instance-map ee 1))))
  instance-map)

(fn keys [tbl] (icollect [k _ (pairs tbl)] k))

(fn table-length [tbl] (length (keys tbl)))

(fn zip [itr1 itr2]
  (fn yield []
    (let [el1 (itr1)
          el2 (itr2)]
      (if (or (= nil el1) (= nil el2))
        nil
        (values el1 el2))))
  yield)

(fn deep-eq [e1 e2]
  "true if e1 has all the same keys and all the same values as e2"
  ; could be more efficient via shortcut eval
  (if
    (not (and (is-table? e1) (is-table? e2))) (= e1 e2)
    (!= (table-length e1) (table-length e2)) false
    (accumulate [is-eq true
                 k1 v1 (pairs e1)]
      (and is-eq (deep-eq v1 (. e2 k1))))))

;; String manipulation
(fn array-str-concat [arr-of-str]
  (accumulate [str ""
               _ s (ipairs arr-of-str)]
    (.. str s)))

(fn str-idx [s i]
  (string.sub s i i))

(fn split [str ?capture]
  (let [t []
        d (or ?capture ".")]
    (string.gsub str d #(table.insert t $))
    t))

(fn palindrome? [str]
  (if
    (empty? str) true
    (= (str-idx str 1) (str-idx str -1)) (palindrome? (string.sub str 2 -2))
    false))

(fn hash-args [...]
  (array-str-concat (u.map [...] tostring)))


;; Dynamic programming stuff
(fn cache [f]
  (local tbl {})

  (fn c [...]
    (let [arghash (hash-args ...)]
      (if (?. tbl arghash) ; are args in tbl?
        (. tbl arghash)
        (do
          (tset tbl arghash (f ...))
          (. tbl arghash)))))
  c)


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
  "probably slow"
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

(fn indicies [seq ?f ?arr]
  "returns indicies of seq where f is true"
  (local idxs (or ?arr []))
  (let [f (or ?f int->bool)]
    (each [i v (ipairs seq)]
      (if (f v)
        (table.insert idxs i))))
  idxs)

(fn indicies-w-offset [seq ?f ?arr ?offset]
  "returns indicies of seq where f is true - very bespoke function, kinda weird"
  (local idxs (or ?arr []))
  (local offset (or ?offset 0))
  (let [f (or ?f int->bool)]
    (each [i v (ipairs seq)]
      (if (f v)
        (table.insert idxs (+ offset i)))))
  idxs)


;; Math
(fn gcd [a b]
  "greatest common divisor"
  (if
    (< a b) (gcd b a)
    (divisible? a b) b
    (gcd b (% a b))))

(fn lcm [a b]
  "lowest common multiple"
  (* a (/ b (gcd a b))))

(fn stirlings-approx [n]
  (* (math.sqrt (prod [2 math.pi n])) (math.pow (/ n (math.exp 1)) n)))

(fn fact-stirling [n k]
  (/ (stirlings-approx n) (* (stirlings-approx k) (stirlings-approx (- n k)))))

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

(fn Fn [n]
  (local phi (/ (+ 1 (math.sqrt 5)) 2))
  (local psi (- 1 phi))
  (/ (- (math.pow phi n) (math.pow psi n)) (math.sqrt 5)))

(fn triangle-num? [n]
  (int? (/ (- (math.sqrt (+ 1 (* 8 n))) 1) 2)))

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

(fn num-divisors [P]
  (let [pfs (prime-factors P)
        insts (instances pfs)]
    (prod (icollect [_ v (pairs insts)] (+ 1 v)))))

(fn prime? [n] (= 1 (# (prime-factors n))))

(fn prime-gen-brute []
  "infinite and relatively slow"
  (local nats (natural-numbers 3 nil 2))
  (fn rip []
    (local n (nats))
    (if (prime?  n)
      n
      (rip)))
  rip)

(fn prime-gen [?primes-under]
  "not infinite but ~~speedy~~"
  (local sh (or ?primes-under 15000))
  (local primes (ones sh))
  (var i 2)
  (fn rip []
    (if
      (< sh i) nil
      (bool->int (. primes i)) (do
                                 (local tmp i)
                                 (for [j i (# primes) i] (tset primes j 0))
                                 (while (= 0 (. primes i)) (set i (+ i 1)))
                                 tmp)))
  rip)

(fn prime-gen-2 [?size-hint]
  "infinite and ~~speedy~~"
  "Fennel / Lua being 1-indexed messes everything up"
  (var lower-limit 1)
  (var limit (or ?size-hint 100000))
  (local primes (ones limit))
  (local prime-num-list [])
  (tset primes 1 0)

  (var i 2)
  (fn rip []
    (if (= (# primes) i)
      (do
        ; Need to keep a list of the old primes
        ; And need to figure out how to add indicies
        (indicies-w-offset primes nil prime-num-list (- lower-limit 1))
        (var tmp (+ 1 (- limit lower-limit)))
        (set lower-limit limit)
        (set limit (+ lower-limit (* 2 tmp)))
        (for [ii 1 (# primes)] (tset primes ii 1))  ; reset old table
        (for [ii lower-limit limit] (table.insert primes 1))  ; add new primes
        (each [_ p (ipairs prime-num-list)]
          (local base (if (= 0 (% lower-limit p)) 0 (- p (% lower-limit p))))  ; new-lower + (p - (new-lower % p))
          (for [ii base (# primes) p] (tset primes (+ 1 ii) 0)))
        (set i 1)
        (while (and (< i (# primes)) (= 0 (. primes i))) (set i (+ i 1)))
        (var prime-num (- (+ lower-limit i) 1))
        (set i (+ 1 i))
        (while (and (< i (# primes)) (= 0 (. primes i))) (set i (+ i 1)))
        prime-num)
      (do
        (local prime-num (+ i (- lower-limit 1)))
        (for [j (+ i prime-num) (# primes) prime-num] (tset primes j 0))
        (set i (+ i 1))
        (while (and (< i (# primes)) (= 0 (. primes i))) (set i (+ i 1)))
        prime-num)))
  rip)

(fn primes-between [start-num end-num]
  (var tbl [])
  (let [pg (prime-gen end-num)]
    (var p -1)
    (while (< p start-num)
      (set p (pg)))
    (table.insert tbl p)
    (each [p pg]
      (table.insert tbl p))
    tbl))

;, Debuggers
(lambda print-time [f ?msg ...]
  "print runtime of f"
  (let [t0 (os.clock)
        v (f ...)
        dt (- (os.clock) t0)]
    (print (or ?msg f) "\t value " v "\t\t runtime " dt)))


;; Exports
{: !=
 : is-table?
 : divisible?
 : even?
 : odd?
 : prime?
 : empty?
 : palindrome?
 : cache
 : filter-iter!
 : take-iter!
 : iter-collect!
 : ith!
 : head
 : tail
 : int->bool
 : bool->int
 : int?
 : gcd
 : lcm
 : count
 : instances
 : keys
 : table-length
 : deep-eq
 : zip
 : max
 : min
 : any
 : all
 : in
 : zero-if-not-divisible
 : sum
 : prod
 : filter
 : natural-numbers
 : fib-gen
 : Fn
 : triangle-num?
 : stirlings-approx
 : str-idx
 : split
 : num-divisors
 : prime-gen
 : prime-gen-2
 : prime-gen-brute
 : prime-factors
 : primes-between
 : print-time}
