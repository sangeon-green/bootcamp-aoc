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

;;일단 소스
(def day2-source
  (->> (io/resource "day2.sample.txt")
       slurp ;;메모리라..더 좋은게 필요한데..
       (str/split-lines))
  )

(println day2-source)

(def result {:0 1})
;; map을 써보자 {}

(defn getChars [text]
  (println "text" text)
  (println "first text" (into [] (first text)))
  (println "text 2", text)
  ;; 왜 캐릭터가 안나오지??
  (for [c1 (into [] (first text))] (println "char" c1) )
  (println "###########")
  ;; map {}을 만들어서 char 별로 카운트 함
  ;; result에 숫자를 키로 하는 추가
  )

(defn getlines [value]
  ;; 왜 value는 vector 안에 쌓여있지?
  (println "value" value)
  (for [text (first value)] (getChars [(seq text)])))

;; result의 value를 곱하는 로직
(println(vals(result)))
(def solve(reduce * vals(result)))

(defn day2-1 []
  (getlines[day2-source])
  (solve))


(day2-1)


((map #(str %) day2-source))


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
