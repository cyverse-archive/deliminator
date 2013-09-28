(ns deliminator.core-test
  (:require [clojure.test :refer :all]
            [deliminator.core :refer :all]))

(defn- test-parser
  [[desc csv delim expected]]
  (testing desc
    (is (= expected (parse-excerpt csv delim)))))

(def ^:private tests
  [["Simple CSV with LF."
    "testing\n"
    \,
    [[["testing"]] 8]]

   ["Simple CSV with CR."
    "testing\r"
    \,
    [[["testing"]] 8]]

   ["Simple CSV with CRLF."
    "testing\r\n"
    \,
    [[["testing"]] 9]]

   ["Multiple fields."
    "one,two\n"
    \,
    [[["one" "two"]] 8]]

   ["Multiple records."
    "one\ntwo\n"
    \,
    [[["one"] ["two"]] 8]]

   ["Multiple fields and records."
    "one,two\nthree,four\n"
    \,
    [[["one" "two"] ["three" "four"]] 19]]

   ["Quoted fields."
    "\"one\",\"two\"\n"
    \,
    [[["one" "two"]] 12]]])

(deftest a-test
  (dorun (map test-parser tests)))
