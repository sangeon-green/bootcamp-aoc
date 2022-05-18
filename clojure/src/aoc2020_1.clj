(ns aoc2020-1
  (:require [clojure.core]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; [https://adventofcode.com/2020/day/1](https://adventofcode.com/2020/day/1)
(def 데이터로드
  "파일을 읽어서 숫자 벡터를 반환"
  (->> (io/resource "2020_day1.sample.txt")
       slurp
       str/split-lines
       (map #(Integer/parseInt %))))


(defn 목표값일치여부 [숫자벡터 목표값]
  "숫자 벡터와 목표 값을 받아서 숫자 벡터의 합이 목표값과 같은지 비교하는 로직
   입력:
     숫자벡터: [1, 2, 3...]
     목표값: 10
   출력: 숫자벡터의 합과 목표값이 같은 경우 true, 그 외는 false"
  (= (apply + 숫자벡터) 목표값))

;; ## 파트 1
;; 더해서 2020이 되는 두 숫자의 곱을 구하시오. (두 숫자는 유일하다고 가정)
;; 예) 1721 979 366 299 675 1456 의 경우 1721 * 299 = 514579 를 출력
"데이터와 목표 값을 받아서 데이터 중 두 개의 값을 가지는 순열로 만들고, 해당 순열의 합 목표값과 같은 경우 두 값은 반환
   입력:
     데이터: [1, 2, 3, 4, 5, 6, 11, 14 ...]
     목표값: 10
   출력: [4, 6]"
(defn  part1-resolve [목표값 데이터]
  (->> (for [x 데이터 y 데이터
             :when (목표값일치여부 [x y] 목표값)]
         [x y])
       first))

;; ## 파트 2
;; 같은 입력이 주어질 때, 더해서 2020이 되는 세 숫자의 합을 구하시오.
;; 예) 2020 = 979 + 366 + 675, 곱하면 241861950 을 출력
(defn  part2-resolve [목표값 데이터]
  "데이터와 목표 값을 받아서 데이터 중 세개의 값을 가지는 순열로 만들고, 해당 순열의 합 목표값과 같은 경우 세 값은 반환
   입력:
     데이터: [2, 3, 4, 5, 6, 11, 14 ...]
     목표값: 10
   출력: [2, 3, 6]"
  (->> (for [x 데이터 y 데이터 z 데이터
             :when (목표값일치여부 [x y z] 목표값)]
         [x y z])
       first))

(comment
  (->> 데이터로드
       (part1-resolve 2020)
       (apply *))
  (->> 데이터로드
       (part2-resolve 2020)
       (apply *)))

;; 아래 값을 쓰면 될면 for 를 안쓰고 한번에 될듯 하다..
;; https://github.com/clojure/math.combinatorics
;; COMBINATIONS
; all the unique ways of taking t different elements from items
;; (combo/combinations [1 2 3] 2)
;;=> ((1 2) (1 3) (2 3))