;; Utils file for my Fennel
;; Implementing it all from scratch for fun!


(fn any [seq func]
  (if (= (# seq) 0)
    false
    (let [[x & xs] seq]
      (if (func x)
        true
        (any xs func)))))

(lambda zero-if-not-divisible [v ...]
  (if (any [...] (fn [i] (= 0 (% v i))))
    v
    0))

(lambda fibgen [?a ?b]
  (let [aa (or ?a 0)
        bb (or ?b 1)]
    (coroutine.yield (+ aa bb))
    (fibgen bb (+ aa bb))))

(fn fibs []
  (coroutine.wrap fibgen))

(fn print-time [f]
  (print f " runtime: " (let [t0 (os.clock)
                              _ (f)
                              t1 (os.clock)]
                          (- t1 t0))))

(fn is-even? [v] (= 0 (% v 2)))

{:any any
 :zero-if-not-divisible zero-if-not-divisible
 :fibgen fibgen
 :fibs fibs
 :print-time print-time}
