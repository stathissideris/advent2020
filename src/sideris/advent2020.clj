(ns sideris.advent2020
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn data
  ([filename]
   (data filename #"\n"))
  ([filename regex]
   (-> (slurp filename) (str/split regex))))

(defn parse-int [x] (Integer/parseInt x))
(defn parse-long [x] (Long/parseLong x))

;;;; day 6

(def input (-> (data "resources/6.txt" #"\n\n")))

;; part 1
(->> input
     (map #(str/replace % "\n" ""))
     (map set)
     (map count)
     (reduce +))

;; or...

(transduce
 (comp (map #(str/replace % "\n" ""))
       (map set)
       (map count))
 + input)

;; part 2

(->> input
     (map (fn [group]
            (->> (str/split group #"\n")
                 (map set)
                 (apply set/intersection)
                 count)))
     (reduce +))

;;;; day 7

(defn parse-bag [[quantity col1 col2]]
  {:quantity (parse-int quantity)
   :color    (str col1 " " col2)})

(defn parse-rule [line]
  (let [[col1 col2 & contents]
        (-> line
            (str/replace #"bags|bag|contain|,|\." "")
            (str/split #" +"))
        contents (->> (partition 3 contents)
                      (map parse-bag)
                      (map (juxt :color :quantity))
                      (into {}))]
    [(str col1 " " col2) contents]))

(def rules (->> (data "resources/7.txt")
                (into {} (map parse-rule))))

;; part 1
(declare bag-contains?)
(def bag-contains?
  (memoize
   (fn [target container]
     (or (get-in rules [container target])
         (some (partial bag-contains? target) (keys (get rules container)))))))

(count (filter (partial bag-contains? "shiny gold") (keys rules)))

;; part 2

(declare bag-count)
(def bag-count
  (memoize
   (fn [bag]
     (let [contents (get rules bag)]
       (+ (reduce + (vals contents))
          (reduce +
                  (for [[inner n] contents]
                    (* n (bag-count inner)))))))))

(bag-count "shiny gold")

;;; day 8

(defn parse-program [filename]
  (let [lines (data filename #"\n")]
    (vec
     (for [line lines]
       (condp #(str/starts-with? %2 %1) line
         "nop" [:nop (parse-int (subs line 4))]
         "acc" [:acc (parse-int (subs line 4))]
         "jmp" [:jmp (parse-int (subs line 4))])))))

;; part 1

(defn advance [{:keys [ptr acc executed program] :as state}]
  (cond (executed ptr)          (reduced {:infinite acc})
        (= ptr (count program)) (reduced {:done acc})
        :else
        (let [[instr param] (get program ptr)]
          (condp = instr
            :nop (-> state
                     (update :ptr inc)
                     (update :executed conj ptr))
            :acc (-> state
                     (update :ptr inc)
                     (update :acc + param)
                     (update :executed conj ptr))
            :jmp (-> state
                     (update :ptr + param)
                     (update :executed conj ptr))))))

(defn execute [program]
  (->> {:ptr      0
        :acc      0
        :executed #{}
        :program  program}
       (iterate advance)
       (drop-while (complement reduced?))
       first
       deref))

(execute (parse-program "resources/8.txt"))

;; part 2

(defn mutate-program [program idx]
  (let [[instr param] (get program idx)]
    (condp = instr
            :acc :INVALID
            :nop (assoc-in program [idx 0] :jmp)
            :jmp (assoc-in program [idx 0] :nop))))

(let [program (parse-program "resources/8.txt")]
  (->> (range (count program))
       (map (partial mutate-program program))
       (remove (partial = :INVALID))
       (map execute)
       (drop-while :infinite)
       first))
