(ns aoc2018-7
  (:require [clojure.core]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as cset]))

(defn 선행-후행-업무분리
  "입력된 값을 분석하여 현재 업무를 기준으로 선행되어야 할 업무를 입력
   입력값: Step C must be finished before step A can begin.
   출력값: {A #{C}}"
  [입력값]
  (->> 입력값
       (re-seq #"([A-Z]) must be finished before step ([A-Z]) can begin.")
       (map #(rest %))
       (reduce
         (fn [업무분리 [선행업무 후행업무]]
           (if (contains? 업무분리 후행업무)
             (update 업무분리 후행업무 (fn [v] (conj v 선행업무)))
             (assoc 업무분리 후행업무 (sorted-set 선행업무)))) {})))

(def data-load
  (->>  (io/resource "day7.sample-light.txt")
        slurp
        선행-후행-업무분리))

(defn 최초시작업무검색
  "선행 업무가 없는 업무를 검색합니다."
  [전체업무]
  (let [선행업무
        (->>
         (cset/difference (apply cset/union (vals 전체업무)) (set (keys 전체업무)))
         first)]
    선행업무))
