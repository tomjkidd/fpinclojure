(ns fpinclojure.option-test
  (:refer-clojure :exclude [map filter some sequence])
  (:require [clojure.test :refer [deftest testing is]]
            [fpinclojure.option :refer [some none map flat-map get-or-else or-else filter
                                        lift map2 sequence traverse] :as opt])
  (:import [java.lang Math]))


(deftest try-use
  (testing "When try wraps a computation that throws, None is returned"
    (is (= (opt/try #(/ 1 0))
           none))))

(deftest map-use
  (testing "A function of A -> B can be used with an Option[A] to produce an Option[B]"
    (is (= (some 10)
           (->> (some 2)
                (map #(* 5 %)))))))

(deftest flat-map-use
  (testing "A function A -> Option[B] can be used with an Options[A] to produce Option[B]"
    (is (= (some 10)
           (->> (some 2)
                (flat-map #(some (* 5 %))))))))

(deftest get-or-else-use
  (testing "A Some(v) can deliver v, and a None will deliver a default"
    (is (= 10 (get-or-else (some 10) 0)))
    (is (= 0 (get-or-else none 0)))
    (is (= none (get-or-else none none)))
    (is (= 0 (get-or-else none #(identity 0))))))

(deftest or-else-use
  (testing "A Some(v) can deliver Some(v), and a None will deliver a Some(default)"
    (is (= (some 10) (or-else (some 10) (some 0))))
    (is (= (some 0) (or-else none (some 0))))
    (is (= (some 0) (or-else none #(some 0))))))

(deftest filter-use
  (testing "A Some(v) can be kept or changed to a None based on a predicate function"
    (is (= (some 10) (filter even? (some 10))))
    (is (= none (filter odd? (some 10))))
    (is (= none (filter (constantly true) none)))))

(deftest lift-use
  (testing "A lifted function produces Option values"
    (let [lifted-abs (lift #(java.lang.Math/abs %))]
      (is (= (some 10) (lifted-abs (some -10))))
      (is (= none (lifted-abs none))))))

(deftest map2-add
  (testing "Demonstrate how to use an int-specific function with Option[Int]"
    (let [map2-add (map2 +)]
      (is (= (some 3)
             (map2-add (some 1)
                       (some 2))))
      (is (= none
             (map2-add (some 1)
                       none))))))

(deftest sequence-use
  (testing "Ensure that sequence can properly extract all values from a list of Options"
    (is (= none (sequence (list (some 1) (some 2) none))))
    (is (= (some (list 1 2 3)) (sequence (list (some 1) (some 2) (some 3)))))))

(deftest traverse-use
  (testing ""
    (let [parse-int #(opt/try (fn [] (some (Integer/parseInt %))))]
      (is (= none (traverse parse-int (list (some "1") none (some "3")))))
      (is (= (some (list 1 2 3)) (traverse parse-int (list (some "1") (some "2") (some "3"))))))))

(deftest fun-use
  (testing "A bunch of things together can compose"
    (is (= 101
           (get-or-else (->> 10
                             some
                             (map #(* 10 %))
                             (filter even?)
                             (map inc))
                        0)))))
