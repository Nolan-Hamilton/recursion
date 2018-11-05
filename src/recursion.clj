(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (and (empty? (rest coll)) (not(empty? coll)))
    true
    false))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
	(if (empty? a-seq)
		a-seq
		(if (pred? (first a-seq))
			(my-drop-while pred? (rest a-seq))
			a-seq)))

(defn seq= [seq-1 seq-2]
	(if (= seq-1 seq-2)
		true
		false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (conj (my-map f (rest seq-1) (rest seq-2)) 
          (f (first seq-1) (first seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (if (= k 1)
      n
      (* n (power n (dec k))))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (conj (my-repeat (dec how-many-times) what-to-repeat)
          what-to-repeat)))

(defn my-range [up-to]
  (if (< up-to 1)
    ()
    (conj (my-range (dec up-to)) (dec up-to))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (conj (tails (rest a-seq)) (seq a-seq))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (conj (inits (reverse (rest (reverse a-seq)))) (seq a-seq))))

(defn rotate [a-seq n]
  (if (= 0 n)
    ()
    (conj (rotate 
          (reverse (conj (reverse (rest a-seq)) (first a-seq)))
          (dec n))
          ;;((get a-seq (dec (count a-seq))
          (reverse (conj (reverse (rest a-seq)) (first a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (conj (seq a-seq) '())
    (rotate a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper 
        (conj freqs {(first a-seq) (inc (get freqs (first a-seq)))})
        (rest a-seq))
      (my-frequencies-helper 
        (conj freqs {(first a-seq) 1})
        (rest a-seq)))))
;      (cons (my-frequencies-helper freqs (rest a-seq)) 
;            {(first a-seq) (inc (get freqs (first a-seq)))})
;      (update (my-frequencies-helper freqs (rest a-seq)) 
;              (first a-seq) inc)
;      (conj (my-frequencies-helper freqs (rest a-seq)) 
;            {(first a-seq) 1}))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (concat (repeat (second (first a-map)) (first (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    '()
    (conj (my-take (dec n) (rest coll)) (first coll))))

(defn my-drop [n coll]
  (if (empty? coll)
    coll
    (if (< n 1)
      coll
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (if (empty? a-seq)
    a-seq
    [(my-take (int (/ (count a-seq) 2)) a-seq) 
     (my-drop (int (/ (count a-seq) 2)) a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (and (not (empty? a-seq)) (not (empty? b-seq)))
    (if (< (first a-seq) (first b-seq))
      (conj (seq-merge (rest a-seq) b-seq) (first a-seq))
      (conj (seq-merge a-seq (rest b-seq)) (first b-seq)))
;      (conj (conj (seq-merge (rest a-seq) (rest b-seq)) (first b-seq)) (first a-seq))
;      (conj (conj (seq-merge (rest a-seq) (rest b-seq)) (first a-seq)) (first b-seq)))
    (if (empty? a-seq)
      b-seq
      a-seq)))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (if (= (count a-seq) 2)
      (seq-merge (first (halve a-seq)) (second (halve a-seq)))
      (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq)))))))
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn take-incrementing [a-seq]
  (loop [theSeq a-seq
         accum ()]
	  (if (< (count theSeq) 2)
	    theSeq
	    (if (or (< (first theSeq) (second theSeq)) 
              (= (first theSeq) (second theSeq)))
        (recur (rest theSeq) (cons (second theSeq) accum))
        (conj (reverse accum) (first a-seq))))))

(defn take-decrementing [a-seq]
  (loop [theSeq a-seq
         accum ()]
	  (if (empty? theSeq)
	    theSeq
	    (if (or (> (first theSeq) (second theSeq)) 
              (= (first theSeq) (second theSeq)))
        (recur (rest theSeq) (cons (second theSeq) accum))
        (conj (reverse accum) (first a-seq))))))

(defn monotonic? [a-seq]
  (if (< (count a-seq) 2)
    true
    (if (apply <= a-seq)
      true
      (if (apply >= a-seq)
        true
        false))))
;  (if (== a-seq (take-incrementing a-seq))
;    true
;    (if (== a-seq (take-decrementing a-seq))
;      true
;      false)))


(defn split-into-monotonics [a-seq]
;  (loop [;theSeq a-seq
;         init-set (rest (reverse (inits a-seq)))
;         ;init-count (count init-set)
;         accum '()]
;		 (if (empty? init-set)
;		   accum
;		   (recur (rest (reverse (inits (drop (count (first (my-drop-while monotonic? init-set))) a-seq)))) 
;		          (conj accum (first (reverse (take-while monotonic? init-set))))))))
)

;  (if (< (count a-seq) 3)
;    a-seq
;    (if (or (< (first a-seq) (second a-seq)) 
;            (= (first a-seq) (second a-seq)))
;      (let [inc-row (take-incrementing a-seq)
;            breakPoint (drop (first (reverse inc-row)) a-seq)]
;        (conj (split-into-monotonics breakPoint) inc-row))
;      (let [dec-row (take-decrementing a-seq)
;            breakPoint2 (drop (first (reverse dec-row)) a-seq)]
;        (conj (split-into-monotonics breakPoint2) dec-row))
;      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn swap-num [a-set num]
  (loop [counter 1
         accum []]
    (if (empty? a-set)
      a-set
      (if (> num (count a-set))
        a-set
        (if (= counter 1)
          (recur (inc counter) (conj accum (get a-set (dec num))))
          (if (> counter (count a-set))
            accum
            (if (= counter num)
              (recur (inc counter) (conj accum (first a-set)))
              (recur (inc counter) (conj accum (get a-set (dec counter)))))))))))
            
;  (if (empty? a-set)
;    a-set
;    (if (> num (count a-set))
;      a-set
;      (let [uno (first a-set)
;            swapper (drop (dec num) a-set)]
;        )

(defn permutations [a-set]
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn swap2 [a-seq]
	 (if (< (count a-seq) 2)
	   a-seq
	   (conj (conj (rest (rest a-seq)) (first a-seq)) (second a-seq))))

(defn swap-rest [a-seq]
  (if (empty? a-seq)
    a-seq
    (conj (swap-rest (rest (swap2 a-seq))) (swap2 a-seq))))

(defn init-swap [a-seq]
  (loop [init-swaps (swap-rest a-seq)
         accum '()]
    (if (empty? init-swaps)
      accum
      (recur (rest init-swaps) (concat (inits (first init-swaps)) accum)))))

(defn all-combos [a-seq]
  (loop [rots (rotations a-seq)
         accum '()]
	  (if (empty? rots)
	    accum
	    (recur (rest rots) (concat (init-swap (first rots)) accum)))))

(defn powerset-p2 [a-set]
  ;((set (map set (map sort (all-combos a-set))))))
  (map sort (all-combos a-set)))

(defn powerset-p1 [a-set]
  (let [rots (rotations a-set)]
    (loop [rots2 rots
           accum '()]
      (if (empty? rots2)
        ;(set (map set (map sort accum)))
        (map sort accum)
        (recur (rest rots2) (concat accum (inits (first rots2))))))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{a-set}
    (set (map set (concat (powerset-p1 a-set) (powerset-p2 a-set))))))
