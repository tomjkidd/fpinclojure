(ns fpinclojure.option
  "A reference implementation of the Option[A] type

  Represents Some(v) and None types

  To make congruent with Clojure:
  * nil is used to represent None
  * [:some v] is used to represent Some(v)
  * map and other helpers will pass f as first argument"
  (:refer-clojure :exclude [try map filter some sequence])
  (:require [clojure.core :as clj]))

(defn some
  "Return the representation of a Some(v)"
  [v]
  [:some v])

(def none
  "Return the represenation of a None"
  nil)

(defn try
  "(try thunk: Unit -> A): Option[A]

  Evaluates a function to create an A, which might fail.

  In the case of failure, nil is returned, otherwise the thunks value is returned."
  [thunk]
  (try
    (thunk)
    (catch Exception e none)))

(defn map
  "(map f: A -> B oa: Option[A]): Option[B]

  When oa has a value, use f to create a new value and wrap it in a Some"
  [f [t v :as oa]]
  (if (= :some t)
    (some (f v))
    none))

(defn flat-map
  "(flat-map f: A -> Option[B] oa: Option[a]): Option[B]

  When oa has a value, extract it and apply f to attempt to produce an Option[B] directly.
  f represents a function that might fail to produce a B.

  Called `flat` because f is applied directly to produce Option[B], in contrast to
  where map has to then `raise` or `lift` the result using `some`."
  [f [t v :as oa]]
  (if (= :some t)
    (f v)
    none))

(defn get-or-else
  "(get-or-else oa:Option[A] default: <Unit -> A | A>): A

  Introduced `<>` syntax to show that default can be a thunk OR a value directly to allows
  non-strict evaluation only in the case where you need to get an A.

  This is the only retrieval function in the API for Option[A] that allows us to return to A
  instead of Option[A]. We must provide a default in case oa is None, but when you call this
  function you are effectively saying you want to handle the case with external context to
  drop out of the Option[A] type.

  I had thought about reversing the order of the args here, and may in the future in order to
  allow more complex compositions to not have to change threading macro calls mid calculation."
  [[t v :as oa] default]
  (if (= :some t)
    v
    (if (fn? default)
      (default)
      default)))

(defn or-else
  "(or-else oa:Option[A] default-option: <Unit -> Option[A] | Option[A]>): Option[A]"
  [[ta va :as oa] default-option]
  (if (= :some ta)
    oa
    (if (fn? default-option)
      (default-option)
      default-option)))

(defn filter
  "(filter pred: A -> Boolean oa: Option[A]): Option[A]

  Allows us to change a Some(v) to a None when (pred v) is false."
  [pred [t v :as oa]]
  (if (= :some t)
    (if (pred v) oa none)
    none))

(defn lift
  "(lift f: A -> B):(Option[A] -> Option[B])

  Allows a normal function A -> B to transform an Option[A] to an Option[B]"
  [f]
  (partial map f))

(defn map2
  "(map2 f: A -> B -> C): (oa: Option[A] -> ob: Option[B]) -> Option[C]

  Combines two Option values given a function that knows how to combine the underlying values

  Take a function in the form A -> B -> C and returns a function that does Option[A] -> Option[B] -> Option[C].
  This allows us to take any function that was useful with no idea what Option is, and work
  with that function using Options, so we don't have to create special purpose Option functions"
  [f]
  (fn [[ta va :as oa] [tb vb :as ob]]
    (if (and (= :some ta)
             (= :some tb))
      (some (f va vb))
      none)))

(defn sequence
  "(sequence oas:List[Option[A]]): Option[List[A]]"
  [oas]
  (if (every? #(not= none %) oas)
    (some (clojure.core/map #(get-or-else % none) oas))
    none))

(defn traverse
  "(traverse f:A->Option[B] oas:List[Option[A]]): Option[List[B]]"
  [f oas]
  (let [obs (reduce (fn [acc oa]
                      (if (= none acc)
                        (reduced none)

                        (when-let [[tb vb] (flat-map f oa)]
                          (cons vb acc))))
                    (list)
                    oas)]
    (when obs
      (some (reverse obs)))))

;; Now that defs are made for each function a good question to investigate is:
;; Which of this functions are essential? Can some be implemented in terms of others?

(defn map2
  [f]
  (fn [oa ob]
    (flat-map (fn [a]
                (map (fn [b]
                       (f a b))
                     ob))
              oa)))
