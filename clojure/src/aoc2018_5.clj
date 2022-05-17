(ns aoc2018-5
  (:require [clojure.string]
            [clojure.core]))

;; https://stackoverflow.com/questions/19749624/how-to-cast-a-character-to-int-in-clojure
;; 문자열을 숫자로 바꾸면 값을 가져오기가 수월함 -> 정규식을 안써도 됨
(def 데이터로드 (->> "resources/day5.sample copy.txt"
                (slurp)
                (mapv int);; last시 속도 빠르게
                ))

(defn 반응대상? [직전문자-ascii 현재문자-ascii]
  "현재 문자와 직전의 문자가 같은지 확인
  입력: 
    직전문자-ascii 97 (a)
    현재문자-ascii 65 (A)
   출력: 같은 종류의 대소문자인 경우(Aa) true 아닌경우 false "
  (= (abs (- 직전문자-ascii 현재문자-ascii)) 32))

(defn 반응후살아남는것 [확인대상]
  "문자열을 읽어서 바로 옆의 값과 같은 종류의 대소문자일 경우에는 제거되는 로직"
  (->> 확인대상
       (reduce (fn [반응되지않는문자모음 비교대상]
                 (if (empty? 반응되지않는문자모음)
                   (conj 반응되지않는문자모음 비교대상)
                   ;; 미사용이라 _로 하면 되는걸 확인
                   (if (반응대상? (last 반응되지않는문자모음) 비교대상)
                     (pop 반응되지않는문자모음)
                     (conj 반응되지않는문자모음 비교대상))))
               [])))

;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

(comment (->> 데이터로드
              반응후살아남는것
              count))

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.
(defn 문자-삭제 [비교원본 문자]
  (filter #((complement contains?) 문자 %) 비교원본))

(defn a-z-까지-하나씩-삭제 [polymer]
  (for [알파벳-소문자-ascii-index (range (int \a) (int \z))]
    (문자-삭제 polymer #{알파벳-소문자-ascii-index (- 알파벳-소문자-ascii-index 32)})))

(comment (->> 데이터로드
              a-z-까지-하나씩-삭제
              (map #(반응후살아남는것 %))
              (map count)
              (apply min)))


;; 물어보고 싶은거
#dbg
 (defn 반응후살아남는것2 [확인대상]
   (->> (let [확인 확인대상 반응되지않는문자모음 []]
           (loop [match (first 확인) 반응되지않는문자모음1 []]
             (if (empty? 반응되지않는문자모음1)
               (conj 반응되지않는문자모음 match)
               (if (반응대상? (last 반응되지않는문자모음) match)
                 (pop 반응되지않는문자모음)
                 (conj 반응되지않는문자모음 match)))
             (recur (rest 확인) (conj 반응되지않는문자모음 반응되지않는문자모음1)))
           반응되지않는문자모음)))

(comment (->> 데이터로드
              반응후살아남는것2
              count))
