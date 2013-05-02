(ns lela.parser
  (:require [monads.core :as m]
            [monads.macros :as mm])
  (:import [monads.core state-transformer]))

#_(def parser (m/state-t m/maybe))
(deftype parser-m [v mv f]
  clojure.lang.IFn
  (invoke [_ s]
    (if f
      (if-let [[v ss] (mv s)]
        ((f v) ss)
        nil)
      [v s]))

  m/Monad
  (do-result [_ v]
    (parser-m. v nil nil))
  (bind [mv f]
    (parser-m. nil mv f))

  m/MonadZero
  (zero [_]
    (constantly nil))
  (plus-step [mv mvs]
    (parser-m. nil
               (fn [s]
                 (loop [[mv & mvs] (cons mv mvs)]
                   (when mv
                     (if-let [result (mv s)]
                       result
                       (recur mvs)))))
               (fn [v] (parser-m. v nil nil)))))

(defn parser [v]
  (parser-m. v nil nil))

(defmacro p-do [bindings expr]
  `(monads.macros/do lela.parser/parser ~bindings ~expr))

(defn optional [p]
  (m/plus [p (parser nil)]))

(declare one-or-more)

(defn none-or-more [p]
  (optional (one-or-more p)))

(defn one-or-more [p]
  (p-do
   [a p
    as (none-or-more p)]
   (cons a as)))

(def next-char
  (reify
    m/Monad
    (do-result [_ v]
      (parser v))
    (bind [mv f]
      (fn [{:keys [text column line] :as state
            :or {column 0 line 0}}]
        (when (> (count text) 0)
          (let [c (first text)
                column (if (= c \newline) 0 (inc column))
                line (if (= c \newline) (inc line) line)
                new-state (-> state
                              (update-in [:text] subs 1)
                              (assoc :column column :line line))]
            ((f c) new-state)))))))

(def current-namespace
  (reify
    m/Monad
    (do-result [_ v]
      (parser v))
    (bind [mv f]
      (fn [{:keys [ns] :as state}]
        ((f ns) state)))))

(defn char-test [pred]
  (p-do
   [c next-char
    :when (pred c)]
   c))

(defn is-char [c]
  (char-test #(= c %)))

(defn one-of [coll]
  (char-test (set coll)))

(def alpha (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def digit (one-of "0123456789"))

;; TODO: Make faster
(defn is-string [s]
  (if (empty? s)
    (parser s)
    (p-do
     [_ (is-char (first s))
      _ (is-string (subs s 1))]
     s)))
