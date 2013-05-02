(ns lela.test.reader
  (:require [clojure.test :refer :all]
            [monads.core :as m]
            [lela.parser :as p]
            [lela.reader :refer :all]))

(deftest test-to-string
  (is (= ["bogus" {:text ""}]
         (update-in ((to-string (p/is-string "bogus")) {:text "bogus"})
                    [1] select-keys [:text]))))

(deftest test-symbol-punct
  (is (= [\- {:text ""}]
         (update-in (symbol-punct {:text "-"})
                    [1] select-keys [:text]))))

(deftest test-namespace-char
  (is (= [".b" {:text ""}]
         (update-in (namespace-char {:text ".b"})
                    [1] select-keys [:text]))))

(deftest test-rest-of-symbol
  (is (= ["rest" {:text " bogus"}]
         (update-in (rest-of-symbol {:text "rest bogus"}) [1] select-keys [:text])))
  (is (= ["bo:gus" {:text ""}]
         (update-in (rest-of-symbol {:text "bo:gus"}) [1] select-keys [:text])))
  (is (= ["rest" {:text "/bogus"}]
         (update-in (rest-of-symbol {:text "rest/bogus"}) [1] select-keys [:text])))
  (is (= ["rest-of" {:text " bogus"}]
         (update-in (rest-of-symbol {:text "rest-of bogus"}) [1] select-keys [:text]))))

(deftest test-symbol-name
  (is (nil? (symbol-name {:text "1bogus"})))
  (is (= ["a1bogus" {:text ""}]
         (update-in (symbol-name {:text "a1bogus"}) [1] select-keys [:text]))))

(deftest test-namespace-prefix
  (is (nil? (namespace-prefix {:text "namespace"})))
  (is (= ["namespace/" {:text ""}]
         (update-in (namespace-prefix {:text "namespace/"})
                    [1] select-keys [:text]))))

(deftest test-match-symbol
  (is (= ['ns/bogus {:text ""}]
         (update-in (match-symbol {:text "ns/bogus"})
                    [1] select-keys [:text])))
  (is (= ['n1/b4gus {:text ""}]
         (update-in (match-symbol {:text "n1/b4gus"})
                    [1] select-keys [:text])))
  (is (= ['bogus {:text ""}]
         (update-in (match-symbol {:text "bogus"})
                    [1] select-keys [:text]))))

(deftest test-match-tab
  (is (= [\tab {:text ""}]
         (update-in (match-string-tab {:text "\\t"}) [1] select-keys [:text]))))

(deftest test-match-string
  (is (= ["first second" {:text ""}]
         (update-in (match-string {:text "\"first second\""}) [1] select-keys [:text])))
  (is (= ["first\"second" {:text ""}]
         (update-in (match-string {:text "\"first\\\"second\""}) [1] select-keys [:text])))
  (is (= ["first\nsecond" {:text ""}]
         (update-in (match-string {:text "\"first\\nsecond\""}) [1] select-keys [:text])))
  (is (= ["first\bsecond" {:text ""}]
         (update-in (match-string {:text "\"first\\bsecond\""}) [1] select-keys [:text])))
  (is (= ["first\tsecond" {:text ""}]
         (update-in (match-string {:text "\"first\\tsecond\""}) [1] select-keys [:text])))
  (is (= ["first\rsecond" {:text ""}]
         (update-in (match-string {:text "\"first\\rsecond\""}) [1] select-keys [:text])))
  (is (= ["first\fsecond" {:text ""}]
         (update-in (match-string {:text "\"first\\fsecond\""}) [1] select-keys [:text])))
  (is (= ["first\\second" {:text ""}]
         (update-in (match-string {:text "\"first\\\\second\""}) [1] select-keys [:text]))))

(deftest test-str-to-int
  (is (= 1234 (str-to-int "1234")))
  (is (= 0 (str-to-int ""))))

(deftest test-match-number
  (is (= [-9854 {:text ""}] (update-in (match-number {:text "-9854"}) [1] select-keys [:text])))
  (is (= [9854 {:text ""}] (update-in (match-number {:text "9854"}) [1] select-keys [:text]))))

(deftest test-match-char
  (is (= [\newline {:text " \\n"}]
         (update-in (match-character {:text "\\newline \\n"}) [1] select-keys [:text])))
  (is (= [\n {:text "bogus"}]
         (update-in (match-character {:text "\\nbogus"}) [1] select-keys [:text])))
  (is (= [\space {:text ""}]
         (update-in (match-character {:text "\\ "}) [1] select-keys [:text])))
  (is (= [\tab {:text " \\t"}]
         (update-in (match-character {:text "\\tab \\t"}) [1] select-keys [:text])))
  (is (= [\backspace {:text ""}]
         (update-in (match-character {:text "\\backspace"}) [1] select-keys [:text])))
  (is (= [\return {:text ""}]
         (update-in (match-character {:text "\\return"}) [1] select-keys [:text])))
  (is (= [\formfeed {:text ""}]
         (update-in (match-character {:text "\\formfeed"}) [1] select-keys [:text]))))

(deftest test-match-nil
  (is (= [nil {:text ""}]
         (update-in (match-nil {:text "nil"}) [1] select-keys [:text]))))

(deftest test-match-boolean
  (is (= [true {:text ""}]
         (update-in (match-boolean {:text "true"}) [1] select-keys [:text])))
  (is (= [false {:text ""}]
         (update-in (match-boolean {:text "false"}) [1] select-keys [:text]))))

(deftest test-match-keyword
  (is (= [:key1 {:text ""}]
         (update-in (match-keyword {:text ":key1"}) [1] select-keys [:text])))
  (is (= [:bogus/key1 {:text ""}]
         (update-in (match-keyword {:text ":bogus/key1"}) [1] select-keys [:text])))
  (is (= [:bogus/key1 {:text ""}]
         (update-in (match-keyword {:ns "bogus" :text "::key1"}) [1] select-keys [:text]))))

(deftest test-match-list
  (is (= [(list :a 14 (list :inner 91)) {:text ""}]
         (update-in (match-list {:text "(:a 14 (:inner 91))"})
                    [1] select-keys [:text]))))

(deftest test-match-vector
  (is (= [(list 'vector :a 14 (list 'vector :inner 91)) {:text ""}]
         (update-in (match-vector {:text "[:a 14 [:inner 91]]"})
                    [1] select-keys [:text]))))

(deftest test-match-hash-set
  (is (= [(list 'hash-set :a 14 (list 'hash-set :inner 91)) {:text ""}]
         (update-in (match-hash-set {:text "#{:a 14 #{:inner 91}}"})
                    [1] select-keys [:text]))))

(deftest test-match-hash-map
  (is (= [(list 'hash-map :a 14 :b (list 'hash-map :inner 91)) {:text ""}]
         (update-in (match-hash-map {:text "{:a 14 :b {:inner 91}}"})
                    [1] select-keys [:text]))))

(deftest test-match-quoted
  (is (= [(list 'quote 'bogus) {:text ""}]
         (update-in (match-quoted {:text "'bogus"})
                    [1] select-keys [:text]))))

(deftest test-match-form
  (is (= [:a {:text ""}]
         (update-in (match-form {:text ":a"})
                    [1] select-keys [:text])))
  (is (= [14 {:text ""}]
         (update-in (match-form {:text "14"})
                    [1] select-keys [:text])))
  (is (= ['ando {:text ""}]
         (update-in (match-symbol {:text "ando"})
                    [1] select-keys [:text])))
  (is (= [(list :a 14 (list :inner 91)) {:text ""}]
         (update-in (match-form {:text "(:a 14 (:inner 91))"})
                    [1] select-keys [:text])))
  (is (= [(list 'vector :a 14 (list 'vector :inner 91)) {:text ""}]
         (update-in (match-form {:text "[:a 14 [:inner 91]]"})
                    [1] select-keys [:text])))
  (is (= [(list 'hash-set :a 14 (list 'hash-set :inner 91)) {:text ""}]
         (update-in (match-form {:text "#{:a 14 #{:inner 91}}"})
                    [1] select-keys [:text])))
  (is (= [(list 'hash-map :a 14 :b (list 'hash-map :inner 91)) {:text ""}]
         (update-in (match-form {:text "{:a 14 :b {:inner 91}}"})
                    [1] select-keys [:text])))
  (is (= [(list 'var 'bogus) {:text ""}]
         (update-in (match-form {:text "#'bogus"})
                    [1] select-keys [:text])))
  (is (= [:keep {:text ""}]
         (update-in (match-form {:text "; comment \n :keep"})
                    [1] select-keys [:text])))
  (is (= [:keep {:text ""}]
         (update-in (match-form {:text "#_ comment \n :keep"})
                    [1] select-keys [:text]))))
