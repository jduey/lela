(ns lela.test.parser
  (:require [clojure.test :refer :all]
            [monads.core :as m]
            [lela.parser :refer :all]))

(deftest test-parser-plus
  (let [cnt (atom 0)
        alt1 (p-do
              [:let [_ (swap! cnt inc)]
               _ (is-char \a)]
              \a)
        alt2 (p-do
              [:let [_ (swap! cnt inc)]
               _ (is-char \b)]
              \b)]
    ((m/plus [alt1 alt2]) {:text "a"})
    (is (= 1 @cnt))

    ((m/plus [(m/plus [(is-char \a) (is-char \b)]) (is-char \c)]) {:text "c"})))

(deftest test-optional
  (let [opt-a (optional (is-char \a))]
    (is (= [\a {:text "" :column 1 :line 0}]
           (opt-a {:text "a"})))
    (is (= [nil {:text "b"}]
           (opt-a {:text "b"})))))

(deftest test-next-char
  (is (nil? ((m/bind next-char #(parser %)) {:text ""})))
  (is (= [\a {:text "b" :column 1 :line 0}]
         ((m/bind next-char #(parser %)) {:text "ab"}))))
