(ns lela.reader
  (:require [monads.core :as m]
            [lela.parser :refer :all]))

(defn to-string [p]
  (m/bind p #(parser (apply str %))))

(defn char-seq [ps]
  (to-string (m/seq ps)))

(def symbol-punct (one-of "*+!-_?"))

(def symbol-char (m/plus [alpha digit symbol-punct]))

(def namespace-char (m/plus [symbol-char
                             (char-seq [(is-char \.) symbol-char])]))

(def rest-of-symbol
  (to-string (none-or-more
              (m/plus [symbol-char
                       (char-seq [(is-char \:) symbol-char])]))))

(def symbol-name
  (char-seq [alpha rest-of-symbol]))

(def namespace-prefix
  (char-seq [alpha
             (to-string (one-or-more namespace-char))
             (is-char \/)]))

(def match-symbol
  (p-do
   [sym (m/plus [(m/bind (char-seq [namespace-prefix symbol-name])
                         #(parser (symbol %)))
                 symbol-name])]
   (symbol sym)))

(def match-string-doublequote
  (p-do
   [_ (m/seq [(is-char \\) (is-char \")])]
   \"))

(def match-string-newline
  (p-do
   [_ (m/seq [(is-char \\) (is-char \n)])]
   \newline))

(def match-string-tab
  (p-do
   [_ (m/seq [(is-char \\) (is-char \t)])]
   \tab))

(def match-string-backspace
  (p-do
   [_ (m/seq [(is-char \\) (is-char \b)])]
   \backspace))

(def match-string-return
  (p-do
   [_ (m/seq [(is-char \\) (is-char \r)])]
   \return))

(def match-string-backslash
  (p-do
   [_ (m/seq [(is-char \\) (is-char \\)])]
   \\))

(def match-string-formfeed
  (p-do
   [_ (m/seq [(is-char \\) (is-char \f)])]
   \formfeed))

(def not-backslash
  (char-test #(not (or (= % \\)
                       (= % \")))))

(def match-string
  (p-do
   [_ (is-char \")
    s (to-string (none-or-more (m/plus [not-backslash
                                        match-string-backslash
                                        match-string-doublequote
                                        match-string-tab
                                        match-string-backspace
                                        match-string-return
                                        match-string-formfeed
                                        match-string-newline])))
    _ (is-char \")]
   s))

(defn str-to-int [int-str]
  (reduce #(-> %1
               (* 10)
               (+ (condp = %2
                      \0 0
                      \1 1
                      \2 2
                      \3 3
                      \4 4
                      \5 5
                      \6 6
                      \7 7
                      \8 8
                      \9 9)))
          0 int-str))

(def integer
  (p-do
   [negate? (optional (is-char \-))
    num-str (to-string (one-or-more digit))]
   (let [magnitude (str-to-int num-str)]
     (if negate?
       (* -1 magnitude)
       magnitude))))

#_(def float
  (to-string (m/seq [integer (is-char \.) integer])))

#_(def ratio
  (to-string (m/seq [integer (is-char \/) integer])))

(def match-number
  (m/plus [integer
           #_float
           #_ratio]))

(def match-newline
  (p-do
   [_ (is-string "\\newline")]
   \newline))

(def match-space
  (p-do
   [_ (is-string "\\ ")]
   \space))

(def match-tab
  (p-do
   [_ (is-string "\\tab")]
   \tab))

(def match-backspace
  (p-do
   [_ (is-string "\\backspace")]
   \backspace))

(def match-return
  (p-do
   [_ (is-string "\\return")]
   \return))

(def match-formfeed
  (p-do
   [_ (is-string "\\formfeed")]
   \formfeed))

(def match-character
  (m/plus [match-newline
           match-space
           match-tab
           match-backspace
           match-return
           match-formfeed
           (p-do
            [_ (is-char \\)
             c next-char]
            c)]))

(def match-nil
  (p-do
   [_ (is-string "nil")]
   nil))

(def match-boolean
  (m/plus [(p-do [_ (is-string "true")] true)
           (p-do [_ (is-string "false")] false)]))

(def unqualified-key
  (p-do
   [_ (is-char \:)
    name rest-of-symbol]
   (keyword name)))

(def ns-qualified-key
  (p-do
   [_ (is-char \:)
    name (char-seq [namespace-prefix
                    symbol-name])]
   (keyword name)))

(def default-ns-key
  (p-do
   [_ (is-char \:)
    _ (is-char \:)
    ns current-namespace
    name rest-of-symbol]
   (keyword (str ns "/" name))))

(def match-keyword
  (m/plus [default-ns-key
           ns-qualified-key
           unqualified-key]))

(declare match-form)

(def match-list
  (p-do
   [_ (is-char \()
    contents (none-or-more match-form)
    _ (is-char \))]
   (if (nil? contents)
     (list)
     contents)))

(def match-vector
  (p-do
   [_ (is-char \[)
    contents (none-or-more match-form)
    _ (is-char \])]
   (cons 'vector contents)))

(def match-hash-set
  (p-do
   [_ (is-string "#{")
    contents (none-or-more match-form)
    _ (is-char \})]
   (cons 'hash-set contents)))

(def match-hash-map
  (p-do
   [_ (is-char \{)
    contents (none-or-more match-form)
    _ (is-char \})]
   (cons 'hash-map contents)))

(def whitespace
  (one-of " \r\n\t,"))

(def match-quoted
  (p-do
   [_ (is-char \')
    form match-form]
   (list 'quote form)))

(def match-deref
  (p-do
   [_ (is-char \@)
    form match-form]
   (list 'deref form)))

(def match-meta-data
  (p-do
   [_ (is-char \^)
    form match-form
    meta match-form]
   (list 'with-meta form meta)))

(def match-comment
  (m/seq [(is-char \;)
          (none-or-more (char-test (partial not= \newline)))
          (is-char \newline)]))

(def match-ignore
  (m/seq [(is-string "#_") match-form]))

(def match-var-quote
  (p-do
   [_ (is-string "#'")
    sym match-symbol]
   (list 'var sym)))

(def match-form
  (p-do
   [_ (none-or-more (m/plus [whitespace
                             match-comment
                             match-ignore]))
    expr (m/plus [match-boolean
                  match-keyword
                  match-symbol
                  match-string
                  match-number
                  match-character
                  match-nil
                  match-hash-map
                  match-hash-set
                  match-vector
                  match-list
                  match-var-quote
                  match-quoted
                  match-deref
                  match-meta-data])]
   expr))
