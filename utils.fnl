;; Utils file for my Fennel
;; Implementing it all from scratch for fun!

(fn divisible? [v i] (= 0 (% v i)))
(fn even? [v] (divisible? v 2))
(fn odd? [v] (not (even? v)))

(fn any [seq func]
  (if (= (# seq) 0)
    false
    (let [[x & xs] seq]
      (if (func x)
        true
        (any xs func)))))

(fn all [seq func]
  (not (any seq #(not (func $)))))

; this could be more general
; smth like val * kroneker-delta(f(val, seq)) where f returns a bool
(lambda zero-if-not-divisible [v ...]
  (if (any [...] #(divisible? v $)) v 0))

(fn fibgen []
  (lambda fibs [?a ?b]
    (let [aa (or ?a 0)
          bb (or ?b 1)]
      (coroutine.yield (+ aa bb))
      (fibs bb (+ aa bb))))
  (coroutine.wrap fibs))

(fn print-time [f]
  (print f " runtime: " (let [t0 (os.clock)
                              _ (f)
                              t1 (os.clock)]
                          (- t1 t0))))


{:divisible? divisible?
 :even? even?
 :odd? odd?
 :any any
 :all all
 :zero-if-not-divisible zero-if-not-divisible
 :fibgen fibgen
 :print-time print-time}
