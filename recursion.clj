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
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

