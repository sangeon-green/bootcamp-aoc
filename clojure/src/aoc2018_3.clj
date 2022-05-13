(ns aoc2018-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


;; 공통함수 영역
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
   출력: [740 926] 1 [740 927] 2..."
  [locations-list]
  (frequencies (mapcat map-to-locations locations-list)))

(defn make-usable-data
  "입력: 파일 경로
   출력: (1 740 926 5 4)"
  [filename]
  (->> (io/resource filename)
       slurp
       (str/split-lines)
       (map string-to-list)
       (map list-data-to-map)))

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

(defn count-overlap-locations
  "입력된 맵의 value가 2 이상이면 중복된 좌표라고 판단하고 해당 조건이 true인 수를 반환함
  입력: { [258 956] 1, [431 212] 2, [688 427] 1 }
  출력: 1"
  [frequencies-list]
  (count (filter #(> % 1) (vals frequencies-list))))

(defn day3-part1
  "주어진 파일을 읽고 좌표계로 변환하여 중복되는 좌표의 총 수를 구하시오"
  [data]
  (count-overlap-locations (list-to-freq data)))

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(defn day3-part2
  "일정 범위에 있는 좌표계(source-locations) 중 한번도 중복되지 않은 좌표계를 가진 구조체의 key를 반환
   입력 : 
     전체좌표계 {[4 3] 2, [2 3] 1, [2 5] 1, [3 3] 2, [5 4] 1, [6 3] 1, [3 4] 2, ....}
     중복여부-확인-좌표정보 ({:key 1, :x 1, :y 3, :width 4, :heigt 4} {:key 2, :x 3, :y 1, :width 4, :heigt 4} {:key 3, :x 5, :y 5, :width 2, :heigt 2})
   출력 : 중복이 하나도 없는 좌표 정보의 key 값 ex) 3
   "
  [전체좌표계 중복여부-확인-좌표정보]
  ;; loop 조건이 맞는 경우 멈추는 법 https://stackoverflow.com/questions/27097978/implementing-break-in-clojure
  (loop [[target & rest] 중복여부-확인-좌표정보]
    (let [not-overlap-vector 
          (->> (map-to-locations target)
               ;; (map (fn [loc] (= (get 전체좌표계 loc) 1)))) 오 get 안써도 되다니..
               (map (fn [loc] (= (전체좌표계 loc) 1))))
          ]
      (if (every? true? not-overlap-vector)
        (:key target)
        (recur rest))
      )
    )
  )

(comment
  (make-usable-data "day3.sample copy.txt")
  (day3-part1 (make-usable-data "day3.sample.txt"))
  (day3-part2 (list-to-freq (make-usable-data "day3.sample copy.txt")) (make-usable-data "day3.sample copy.txt"))
  (day3-part2 (list-to-freq (make-usable-data "day3.sample.txt")) (make-usable-data "day3.sample.txt"))
  (:key (map-to-locations {:key 1, :x 1, :y 3, :width 4, :heigt 4}))
  )