(ns deliminator.core
  (:use [slingshot.slingshot :only [throw+]]))

(declare embedded-quote quoted-field unquoted-field term record)

(defn- embedded-quote
  [recs rec fld [c & cs] len pos delim]
  (let [add-field #(conj rec (apply str fld))
        add-rec   #(conj recs (add-field))]
    (cond (nil? c)       [recs len]
          (= c \")       #(quoted-field recs rec (conj fld c) cs len (inc pos) delim)
          (= c delim)    #(record recs (add-field) cs len (inc pos) delim)
          (= c \newline) #(record (add-rec) [] cs (inc pos) (inc pos) delim)
          (= c \return)  #(term (add-rec) [] cs (inc pos) (inc pos) delim)
          :else          (throw+ {:type     ::unescaped-quote-in-quoted-field
                                  :position pos}))))

(defn- quoted-field
  [recs rec fld [c & cs] len pos delim]
  (cond (nil? c) [recs len]
        (= c \") #(embedded-quote recs rec fld cs len (inc pos) delim)
        :else    #(quoted-field recs rec (conj fld c) cs len (inc pos) delim)))

(defn- unquoted-field
  [recs rec fld [c & cs] len pos delim]
  (let [add-field #(conj rec (apply str fld))
        add-rec   #(conj recs (add-field))]
    (cond (nil? c)       [recs len]
          (= c delim)    #(record recs (add-field) cs len (inc pos) delim)
          (= c \newline) #(record (add-rec) [] cs (inc pos) (inc pos) delim)
          (= c \return)  #(term (add-rec) [] cs (inc pos) (inc pos) delim)
          (= c \")       (throw+ {:type     ::quote-in-unquoted-field
                                  :position pos})
          :else          #(unquoted-field recs rec (conj fld c) cs len (inc pos) delim))))

(defn- term
  [recs rec [c & cs :as all-cs] len pos delim]
  (if (= c \newline)
    #(record recs rec cs (inc len) (inc pos) delim)
    #(record recs rec all-cs len pos delim)))

(defn- record
  [recs rec [c & cs] len pos delim]
  (cond (nil? c)       [recs len]
        (= c \newline) #(record (conj recs (conj rec "")) [] cs (inc pos) (inc pos) delim)
        (= c \return)  #(term (conj recs (conj rec "")) [] cs (inc pos) (inc pos) delim)
        (= c delim)    #(record recs (conj rec "") cs (inc pos) (inc pos) delim)
        (= c \")       #(quoted-field recs rec [] cs len (inc pos) delim)
        :else          #(unquoted-field recs rec [c] cs len (inc pos) delim)))

(defn parse-excerpt
  [cs delim]
  (trampoline #(record [] [] cs 0 0 delim)))
