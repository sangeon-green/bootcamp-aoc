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
      (let [count-same-char-result (count-same-char (first inputs))]
        (recur (+ same-char-count-two (if (> (first count-same-char-result) 0) 1 0))
               (+ same-char-count-three (if (> (last count-same-char-result) 0) 1 0))
               (next inputs)))
      (* same-char-count-two same-char-count-three))))

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

(defn find-same-position-chars
  " 입력된 두 문자열을 비교하여 같은 index에 있는 문자열을 반환함
   입력: 원본 문자열, 비교가 될 문자열
   출력: 원본과 비교하여 동일한 위치에 동일한 문자를 추출하여 하나의 문자열로 만듬"
  [origin-string target-string]
  (let [compare-string-bool (map = origin-string target-string)]
    (str/join
     (for [index (range (count origin-string))
           :let [origin-char (nth origin-string index) target-char (nth compare-string-bool index)]]
       (when target-char origin-char)))))

(defn make-filter-same-length
  "비교문자열이 계속 증가하다보니 원본 문자열 길이를 넘어서 발생하는 에러를 막기 위해 target-string length를 제한함"
  [origin-string-length target-string-length]
  (= origin-string-length (+ 1 (count target-string-length))))

(defn day2-part2
  "문자열 벡터를 받아서 첫 번째 문자열부터 문자열 백터와 비교하여 같은 포지션에 있는 문자있다면 반환하고 해당 동작을 반복하면서 문자열 생성
   문제를 정상적으로 이해했는지는 알 수 없음..
   입력: data는 문자열 백터임"
  [data]
  (let [data-string-length (count (first data))
        filter-length-condition (partial make-filter-same-length data-string-length)]
    (->> (for [origin-index (range (count data))
               target-index (range (count data))
               :let [origin-string (nth data origin-index)
                     target-string (nth data target-index)]
               :when (< origin-index target-index)]
           (find-same-position-chars origin-string target-string))
         (filter filter-length-condition)
        ;; (filter (partial #(= data-string-length inc (count %)))) 필터를 따로 안 만들고 넣고 싶었는데
         first)))

(comment
  (defn get-data
    "입력: 파일 경로
     출력: 파일을 읽어서 stirng vector 반환"
    [filename]
    (->> (io/resource filename)
         slurp
         (str/split-lines)))

  (day2-part1 (get-data "day2.sample.txt"))
  (day2-part2 (get-data "day2.sample.txt"))
  (day2-part2 ["bbczcbd" "abcecbc" "bbcwcbd" "abczcfa" "cbczcbd"])
  (make-filter-same-length 7 "abcdefghij")
  (partial make-filter-same-length 7 "abcdefghij"))

;; #################################
;; ###        Refactoring        ###
;; #################################