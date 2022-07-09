(ns turing)

(defn new-tape [n val] (vec (for [x (range n)] val)))

(defn expand-tape [tape n val]
  (vec (concat tape (new-tape n val))))

;; tm-state is a pair (<state> <head-pos>)
;; tmap is a state transition map
(defn state-update [tm-state tape tmap] 
  (let [[state head-pos] tm-state
        head (get tape head-pos)
        [dir write next] (get tmap (list state head))]    
    (letfn [(incr-pos [i]
              (if (and (= dir 'L) (> i 0)) (- i 1)
                    (if (= dir 'R) (+ i 1) i)))
            (update-tape [tape]
              (let [n (count tape)
                    tape (assoc tape head-pos write)
                    new-tape (if (>= (incr-pos head-pos) n)
                               (conj tape 0)
                               tape)]
                new-tape))]
      (list (list next (incr-pos head-pos))
             (update-tape tape)))))


(defn step-n [n mstate tape smap]
  (if (= n 0)
      (list mstate tape)
      (let [[new-state new-tape] (state-update mstate tape smap)]
        (recur (- n 1) new-state new-tape smap))))


(defn print-machine [mstate tape]
  (do
    (println tape)
    (let [[state idx] mstate
          nspace (reduce + (map (comp count str) tape))]
      (println (format (str "%" nspace "s") state)))))


;; transition map maps machine state to (<direction> <write-symbol> <next-state>)
(def BB3 {'(A 0) '(R 1 B)
          '(A 1) '(R 1 H)
          '(B 0) '(R 0 C)
          '(B 1) '(R 1 B)
          '(C 0) '(L 1 C)
          '(C 1) '(L 1 A)})

(def BB4 {'(A 0) '(R 1 B)
          '(A 1) '(L 1 B)
          '(B 0) '(L 1 A)
          '(B 1) '(L 0 C)
          '(C 0) '(R 1 H)
          '(C 1) '(L 1 D)
          '(D 0) '(R 1 D)
          '(D 1) '(R 0 A)})

(def BB5 {'(A 0) '(R 1 B)
          '(A 1) '(L 1 C)
          '(B 0) '(R 1 C)
          '(B 1) '(R 1 B)
          '(C 0) '(R 1 D)
          '(C 1) '(L 0 E)
          '(D 0) '(L 1 A)
          '(D 1) '(L 1 D)
          '(E 0) '(R 1 H)
          '(E 1) '(L 0 A)})

(defn -main
  [& args]
  (let [tape (new-tape 20 0)
        init '(A 15)
        [state tape] (step-n 1 init tape BB5)]
    (print-machine state tape)))
