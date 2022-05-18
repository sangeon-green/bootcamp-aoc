(ns aoc2020-1
  (:require [clojure.core]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; [https://adventofcode.com/2020/day/1](https://adventofcode.com/2020/day/1)

;; ## 파트 1
;; 더해서 2020이 되는 두 숫자의 곱을 구하시오. (두 숫자는 유일하다고 가정)

;; 예) 1721 979 366 299 675 1456 의 경우 1721 * 299 = 514579 를 출력

(def 데이터로드 (->> (io/resource "2020_day1.sample.txt")
                (slurp)
                (str/split-lines)
                (map #(Integer/parseInt %))))

(defn  원하는_값[목표값 숫자백터]
  (->> (for [x1 숫자백터
             x2 숫자백터
             :when (= (+ x1 x2) 목표값)]
         [x1 x2])
       first))


;; ## 파트 2
;; 같은 입력이 주어질 때, 더해서 2020이 되는 세 숫자의 합을 구하시오.

;; 예) 2020 = 979 + 366 + 675, 곱하면 241861950 을 출력


(defn  원하는_값2 [목표값 숫자백터]
  (->> (for [x1 숫자백터
             x2 숫자백터
             :when (= (+ x1 x2) 목표값)]
         [x1 x2])
       first))



(comment
  (->> 데이터로드
       (원하는_값 2020)
       (apply *)))