(ns aoc2018-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)


(defn find-pattern-match-string
  "입력된 값을 특정 패턴과 매치해서 반환
   입력: 문자열
   출력: 패턴과 매치된 문자열"
  [source-string]
  (re-find (re-matcher #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" source-string)))

(defn string-to-list
  "입력된 값을 패턴을 통해 1차적으로 거른 가져온 후 숫자로 변환함
   입력: 문자열 #1 @ 704,926: 5x4
   출력: 숫자 리스트 (1 740 926 5 4)"
  [source-string]
  (map #(Integer/parseInt %) (rest (find-pattern-match-string source-string))))

(defn list-data-to-map
   "리스트로 들어온 값을 키와 매핑해서 맵으로 변환함
   입력: 숫자 리스트 (1 740 926 5 4)
   출력: {:key 1 :x 740 :y 926 :width 5 :height 4}"
  [number-list]
  ((fn [[key x y width heigt]] {:key key :x x :y y :width width :heigt heigt}) number-list))
       
(defn map-to-locations
  "맵으로 생성된 데이터 중 필요한 데이터만 추출 후 좌표 영역을 반환
   입력: {:key 1 :x 740 :y 926 :width 5 :height 4}
   출력: [740 926] [740 927]..."
  [{:keys [x y width heigt]}]
  (for [x-location (range x (+ x width))
        y-location (range y (+ y heigt))]
    [x-location y-location]))

(defn list-to-freq
  " 입력된 맵 리스트를 좌표계로 변환 해서 출력
   입력: ({:key 1 :x 740 :y 926 :width 5 :height 4} ....)
   출력: [740 926] [740 927]..."
  [locations-list]
  (frequencies (mapcat map-to-locations locations-list)))

;; 하고 싶은데 안된거
;; (defn map-to-locations
;;    "맵으로 생성된 데이터 중 필요한 데이터만 추출 후 좌표 영역을 반환
;;    입력: {:key 1 :x 740 :y 926 :width 5 :height 4}
;;    출력: [740 926] [740 927]..."
;;   [[x y width heigt]]
;;   (println "x y width heigt" x y width heigt)
;;   (for [x-location (range x (+ x width))
;;         y-location (range y (+ y heigt))]
;;     [x-location y-location]))

;; (defn list-to-freq
;;   "
;;    입력: ({:key 1 :x 740 :y 926 :width 5 :height 4} ....)
;;    출력: [740 926] [740 927]..."
;;   [{:keys [x y width heigt]}]
;;   (frequencies 
;;    (mapcat map-to-locations [x y width heigt])))

(defn count-overlap-locations
  "입력된 맵의 value가 2 이상이면 중복된 좌표라고 판단하고 해당 조건이 true인 수를 반환함
  입력: { [258 956] 1, [431 212] 2, [688 427] 1 }
  출력: 1"
  [frequencies-list]
  (count (filter #(> % 1)(vals frequencies-list))))

(defn day3-part1
  "주어진 파일을 읽고 좌표계로 변환하여 중복되는 좌표의 총 수를 구하시오"
  [data]
  (count-overlap-locations (list-to-freq (map list-data-to-map (map string-to-list data))) )
)



;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)



(comment
 (defn get-data
    "입력: 파일 경로
     출력: 파일을 읽어서 stirng vector 반환"
    [filename]
    (->> (io/resource filename)
         slurp
         (str/split-lines)))

  (day3-part1 (get-data "day3.sample.txt"))
 )
