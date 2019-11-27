(ns four-clojure.core
  (:gen-class))


(defn detect-palindrome
  [xs]
  (= (seq xs) (reverse xs)))


(defn lazy-fib
  ([]
   (lazy-seq (cons 0 (cons 1 (lazy-fib 1 (+ 0 1))))))
  ([prev-val next-val]
   (lazy-seq (cons next-val (lazy-fib next-val (+ prev-val next-val))))))


(defn get-the-caps
  [my-str]
  (apply str (filter #(Character/isUpperCase %) my-str)))


(defn duplicate-seq
  [xs]
  (apply concat (map #(list % %) xs)))


(defn my-some
  [pred xs]
  (first (filter #(pred %) xs)))


(defn compress-seq
  [xs]
  (->> xs
       (partition 2 1 [:end])
       (filter #(not= (first %)
                      (second %)))
       (map first)))


(defn my-range
  [start end]
  (if (= start end)
    '()
    (cons start (my-range (inc start) end))))


(defn my-factorial
  [n]
  (reduce * (range 1 (inc n))))


(defn my-flatten
  [xs]
  (loop [xs xs
         result '()]
    (let [curr (first xs)]
      (cond (empty? xs)
            result

            (sequential? curr)
            (recur (rest xs) (concat result (my-flatten curr)))

            :else
            (recur (rest xs) (concat result (list curr)))))))


(defn replicate-seq
  [xs reps]
  (mapcat #(take reps (iterate identity %)) xs))


(defn my-interpose
  [value xs]
  (butlast (interleave xs (repeat (count xs) value))))


(defn pack-seq
  [xs]
  (partition-by identity xs))


(defn drop-every-nth
  [xs n]
  (->> (iterate inc 1)
       (interleave xs)
       (partition 2)
       (remove #(zero? (mod (second %) n)))
       (map #(first %))))


(defn split-seq
  [at xs]
  (list (take at xs) (drop at xs)))


(defn half-true? [& bools]
  (boolean (and (some true? bools)
                (not (every? true? bools)))))


(defn my-zipmap [keys values]
  (apply merge (map (fn [key value] {key value}) keys values)))


(defn gcd [a b]
  (let [divisors (+ (/ (max a b) 2) 1)]
    (->> divisors
         (range 1)
         (filter #(and (zero? (mod a %))
                       (zero? (mod b %))))
         last)))


(defn intersection [s1 s2]
  (into #{} (filter s1 s2)))


(defn square-fn [n]
  (fn [x] (reduce * (take n (repeat x)))))


(defn my-iterate [fun start]
  (lazy-seq (cons start (my-iterate fun (fun start)))))


(defn compare-me [fun x y]
  (cond
    (and (not (fun x y)) (not (fun y x))) :eq
    (fun x y) :lt
    (fun y x) :gt))


(defn symmetric? [tree]
  (let [mirror-fn (fn mirror [tree]
                    (if (nil? tree)
                      nil
                      (list (first tree)
                            (mirror (last tree))
                            (mirror (second tree)))))]
    (= tree (mirror-fn tree))))


(defn cartesian-product [xs ys]
  (into #{} (mapcat (fn [x] (map (fn [y] [x y]) ys)) xs)))


(defn product-digits [x y]
  (letfn [(get-digits [value]
            (let [next-value (quot value 10)
                  next-digit (mod value 10)]
              (if (zero? next-value)
                (list (mod value 10))
                (cons next-digit (get-digits next-value)))))]
    (reverse (get-digits (* x y)))))


(defn my-group-by [fun col]
  (->> col
       (map (fn [x] [(fun x) x]))
       (reduce (fn [acc x]
                 (update-in acc
                            [(first x)]
                            concat
                            (list (second x))))
               {})))


(defn symmetric-diff [s1 s2]
  (reduce (fn [acc val]
            (if (acc val)
              (disj acc val)
              (conj acc val)))
          s2 s1))


(defn dot [xs ys]
  (apply + (map (fn [x y] (* x y)) xs ys)))


(defn binary->int [bin]
  (:sum (reduce (fn [{:keys [sum power]} digit]
                  (if (= (str digit) "0")
                    {:sum sum :power (inc power)}
                    {:sum (+ sum (reduce * (repeat power 2)))
                     :power (inc power)}))
                {:sum 0 :power 0}
                (reverse bin))))


(defn infix-calc [& args]
  (:result (reduce (fn [{:keys [result op]} val]
                     (if (fn? val)
                       {:result result :op val}
                       {:result (op result val) :op op}))
                   {:result 0 :op +}
                   args)))


(defn index-me [s]
  (mapv (fn [x y] [x y]) s (range 0 (count s))))


(defn pascal-tri [n]
  (cond (= n 1) [1]
        (= n 2) [1 1]
        (> n 2) (reduce (fn [acc _]
                          (concat [1]
                                  (map (partial apply +) (partition 2 1 acc))
                                  [1]))
                        [1 1]
                        (range (- n 2)))))


(defn my-map [fun coll]
  (if (empty? coll)
    '()
    (lazy-seq (cons (fun (first coll))
                    (my-map fun (rest coll))))))


(defn valid-tree? [tree]
  (if (sequential? tree)
    (if (= (count tree) 3)
      (let [[top left right] tree]
        (and (valid-tree? left) (valid-tree? right)))
      false)
    (if (nil? tree)
      true
      false)))


(defn less-than-sum-sqr-digits [coll]
  (count (filter (fn [val]
                   (let [sum-sqr-digits
                         (loop [number val
                                acc 0]
                           (if (zero? number)
                             acc
                             (let [last-digit (mod number 10)]
                               (recur (int (/ number 10))
                                      (+ acc (* last-digit last-digit))))))]
                     (< val sum-sqr-digits))) coll)))


(defn translate-card-str [card-str]
  (let [[first second] card-str
        suit-map {\D :diamond
                  \S :spade
                  \H :heart
                  \C :club}
        rank-map {\2 0
                  \3 1
                  \4 2
                  \5 3
                  \6 4
                  \7 5
                  \8 6
                  \9 7
                  \T 8
                  \J 9
                  \Q 10
                  \K 11
                  \A 12}]
    {:suit (suit-map first)
     :rank (rank-map second)}))

(defn lcm [& numbers]
  (let [max-num (apply max numbers)]
    (loop [possible-lcm max-num]
      (if (every? #(zero? (mod possible-lcm %)) numbers)
        possible-lcm
        (recur (+ possible-lcm max-num))))))

(defn pascal-trapezoid [meep]
  (let [next (if (= (count meep) 1)
               (concat meep meep)
               (concat [(first meep)]
                       (map (partial apply +') (partition 2 1 meep))
                       [(last meep)]))]
    (lazy-seq (cons meep (pascal-trapezoid next)))))

(defn tree-into-tables [outer-map]
  (apply merge (flatten (for [[outer-key inner-map] outer-map]
                          (map (fn [[inner-key inner-value]]
                                 {[outer-key inner-key] inner-value})
                               inner-map)))))



(defn pairwise-disjoint? [super-set]
  (letfn [(disjoint? [s1 s2]
            (let [difference (clojure.set/difference s1 s2)]
              (= difference s1)))]
    (->> super-set
         (reduce (fn [acc subset-set]
                   (if (disjoint? subset-set acc)
                     (clojure.set/union acc subset-set)
                     (reduced false))))
         boolean)))

(defn flip [func]
  (fn [& args] (apply func (reverse args))))

(defn rotate-seq [amount s]
  (letfn [(forward [prev] (concat (rest prev) [(first prev)]))
          (backward [prev] (conj (butlast prev) (last prev)))]
    (if (pos? amount)
      (nth (iterate forward s) amount)
      (nth (iterate backward s) (- amount)))))
