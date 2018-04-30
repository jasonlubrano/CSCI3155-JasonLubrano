;;(ns plmini.core
  ;;(:gen-class))
(ns plmini.core
  (:require [instaparse.core :as insta]))


;;Run, 

(defn -main []
  (println "Hello, World!")
  (println (+ 2 1)))

(def parserab
  (insta/parser
    "S = A*
    A = 'a'| B 
    B = 'b'| Epsilon"
    :output-format :hiccup))

(def parseabrcompact
  (insta/parser
    "S = ('a' | 'b')*"))


(def parserabright
  (insta/parser
    "S = 'a' S | Epsilon"))    
  
(def parserableft
  (insta/parser
    "S = S 'a' | Epsilon"))

(def parserambiguous
  (insta/parser
    "S = A A
     A = 'a'*"))

(def parsermap
  (insta/parser
    "S = map
    map = 'map'"))


