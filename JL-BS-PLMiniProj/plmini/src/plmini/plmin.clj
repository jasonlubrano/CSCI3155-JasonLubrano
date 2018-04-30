(ns plmini.plmin
  (:require [compojure.core :as comp]))

;;we might have to transfer from instaparse to another library
;;due to syntax, it is not allowing me to make the following:
;;QUOTE and VARQUOTE 

(def ClojureParser
  (comp/parser
    "
    FILE = FORM* Epsilon
    FORM = LIT | LST | VCT | MAP | READMAC
    LIT = STR | NUM | CHAR | NIL | BOOLEAN | KEYWORD | GENSYMBOL | PARAMNAME
    LST = '(' FROM* ')'
    VCT = '[' FORM* ']'
    MAP = '{' (FORM FORM)* '}'
    READMAC = LAMBDA | METADATA | REGEX | VARQUOTE | HOSTEXPR | SET | TAG | DISCARD | DISPATCH | DEREF | QUOTE | BACKTICK | UNQUOTE | UNQUOTESPLIC | GENSYM
    SET = '#{' FORM* '}'
    BACKTICK = '`' FORM
    UNQUOTE = '~' FORM
    QUOTESPLIC = '~@' FORM
    TAG = '^' FORM FORM
    DEREF = '@' FORM
    GENSYM = GENSYMBOL '#'
    LAMBDA = '#(' FORM* ')'
    METADATA = '#^' (MAP FORM | FORM)
    HOSTEXPR = '#+' FORM FORM
    DISCARD = '#_' FORM
    DISPATCH = '#' GENSYMBOL FORM
    REGEX = '#' STR
    STR = STRING
    NUM = FLOAT | HEX | BIN | BIGN | LONG
    CHAR = CHAR_NAMED | CHAR_ANY | CHAR_U
    KEYWORD = MACROKEY | SIMPLEKEY
    SIMPLEKEY = ':' GENSYMBOL
    MACROKEY = ':' ':' GENSYMBOL
    GENSYMBOL = NS_SYMBOL | SYMBOL
    "))
    


