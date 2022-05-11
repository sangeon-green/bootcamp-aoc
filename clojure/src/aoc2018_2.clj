(ns aoc2018-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

(defn count-same-char [string]
    "스트링 한 줄을 캐릭터로 쪼갠 후 같은 2, 3번 중복되는 캐릭터 카운트 반환
   입력: string
   출력: [2번 반복된 캐릭터 여부 1 아니면 0, 2번 반복된 캐릭터 여부 1 아니면 0]"
  (loop [text string
         count-same-map {}]
    (if text
      (let [char (first text)
            count (get count-same-map char 0)]
        (recur (next text) (assoc count-same-map char (+ count 1))))
      (let [counut-same-map-values (vals count-same-map)]
        (loop [counut-same-map-value counut-same-map-values has-same-count-two 0 has-same-count-tree 0]
          (if counut-same-map-value
            (let [first-value (first counut-same-map-value)]
              (recur (next counut-same-map-value)
                     (if (== first-value 2) 1 has-same-count-two)
                     (if (== first-value 3) 1 has-same-count-tree)))
            [has-same-count-two has-same-count-tree]))))))

(defn day2-part1 [data]
   "string vector를 입력 받아서 각 줄마다 2,3번 반복되는 캐릭터 숫자
   입력: data -> string vector
   출력: ...."
  (loop [same-char-count-two 0 same-char-count-three 0 inputs data]
    (if inputs
      #_{:clj-kondo/ignore [:redundant-do]}
      (do
        (let [count-same-char-result (count-same-char (first inputs))]
          (recur (+ same-char-count-two (if (> (first count-same-char-result) 0) 1 0))
                 (+ same-char-count-three (if (> (last count-same-char-result) 0) 1 0))
                 (next inputs))))
      (* same-char-count-two same-char-count-three))))

(comment
  (defn get-data
    "입력: 파일 경로
     출력: 파일을 읽어서 stirng vector 반환"
    [filename]
    (->> (io/resource filename)
         slurp
         (str/split-lines)))

  ((day2-part1 (get-data "day2.sample.txt"))
  ))


;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.


;; #################################
;; ###        Refactoring        ###
;; #################################
