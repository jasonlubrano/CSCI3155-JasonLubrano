;;(ns plmini.core
  ;;(:gen-class)
(ns plmini.core
  (:require [instaparse.core :as insta]))

(defn -main []
  (println "Hello, World!")
  (println (+ 2 1)))

(def parserab
  (insta/parser
    "S = AB*
    A = 'a'| B 
    B = 'b'| Epsilon
    ep = Epsilon"))