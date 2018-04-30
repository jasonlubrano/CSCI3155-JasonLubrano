(ns plmini.core)

(defn add [n1 n2]
  (+ n1 n2))
  

(defn adder[e1]
  (let [y e1]
    (fn [z] (+ y z))))

(def add5
  (adder 5))

(let [my-vector [1 2 3 4]
      my-map {:m1 "foo"}
      my-list (list 2 4 6 8)]
  (list
    (print (str "\n" my-map "\n" my-vector "\n" my-list "\n"))))
   

(defn mul [n1 n2]
  (* n1 n2))

(defn foldleft
  [ns]
  (if (empty? ns)
    1
    (mul (first ns)
       (foldleft (rest ns)))))

(defn foldright
  [ns]
  (if (empty? ns)
    1
    (mul (last ns)
       (foldright (butlast ns)))))

