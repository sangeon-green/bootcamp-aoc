(ns aoc2018-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.
(defn 데이터로드
  "입력: 파일 경로
   출력: 문자열 벡터 
      ex) [1518-11-01 00:00] Guard #10 begins shift"
  [filename]
  (->> (io/resource filename)
       slurp
       (str/split-lines)
       sort))

(defn 키-값-생성 [변환대상]
  (let [[year month day hour min type id] 변환대상]
    {:year year :month month :day day :hour hour :min (Integer/parseInt min) :type type :id id}))

(defn 근무기록-to-키값-쌍 [변환대상]
  "문자열을 필요한 값만 추출하여 key value 쌍으로 반환함
   입력: [1518-11-01 00:00] Guard #10 begins shift
   출력: {:year 1518, :month 11, :day 01, :hour 00, :min 00, :type Guard #10, :id 10}"
  (->> 변환대상
       (re-matcher #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (falls|wakes|Guard #(\d+))")
       re-find
       rest
       키-값-생성))

(defn 근무자별기록정리 [[모든근무자취침기록 현재근무자] 현재근무자취침기록원본]
  "입력된 키-값을 동일한 키로 묶어서 출력함
   입력: {:year 1518, :month 11, :day 01, :hour 00, :min 00, :type Guard #10, :id 10}
   출력: {10 [ 5 10 20 25]} ..."
  (if-let [다음근무자 (:id 현재근무자취침기록원본)]
    [(conj 모든근무자취침기록 {다음근무자 []}) 다음근무자]
    [(conj (pop 모든근무자취침기록)
           {현재근무자 (conj (first (vals (last 모든근무자취침기록))) (:min 현재근무자취침기록원본))})
     현재근무자]))


(defn 취침-기상-시간-seq [취침기록]
  (->> 취침기록
       (partition 2)
       (map #(range (first %) (second %)))
       (reduce concat)))

(defn 취침-기상-시간-freq [근무자 취침기록]
  "근무자와 취침기록을 가져와서 근무자별 총 취침횟수, 가장 많이 잠든시간, 빈도를 구하는 로직
   입력 : 근무자 아이디와 취침기록
      10 [5 25 30 55 24 29]
   출력 : { 
   근무자: 10
   총취침횟수: 20
   가장많이잠든시간: 45
   빈도: 3
   }"
  (let [취침기록-seq (취침-기상-시간-seq 취침기록)]
    {:근무자 (Integer/parseInt 근무자)
     :총취침횟수 (count 취침기록-seq)
     :가장많이잠든시간 (if (empty? (frequencies 취침기록-seq)) 0 (key (apply max-key val (frequencies 취침기록-seq))))
     :빈도 (if (empty? (frequencies 취침기록-seq)) 0 (val (apply max-key val (frequencies 취침기록-seq))))}))


;; 아래처럼 묶고 싶었는데..
;; (defn test11 [value]
;;   (->> value
;;        first
;;        (apply (partial merge-with into))
;;        (fn [[근무자 취침기록]] (취침-기상-시간-freq 근무자 취침기록))))

(defn day4-part1 [{:keys [근무자 가장많이잠든시간]}]
  (* 근무자 가장많이잠든시간))

;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.


(defn day4-part2 [{:keys [근무자 가장많이잠든시간]}]
  (* 근무자 가장많이잠든시간))

(defn day4-해답 [{:keys [근무자 가장많이잠든시간]}]
  (* 근무자 가장많이잠든시간))

(comment
   (->> (map 근무기록-to-키값-쌍 (데이터로드 "day4.sample2.txt"))
        (reduce 근무자별기록정리 [[] nil])
        first
        (apply (partial merge-with into))
        (map (fn [[근무자 취침기록]] (취침-기상-시간-freq 근무자 취침기록)))
        (apply max-key :총취침횟수)
        day4-part1)
   (->> (map 근무기록-to-키값-쌍 (데이터로드 "day4.sample.txt"))
        (reduce 근무자별기록정리 [[] nil])
        first
        (apply (partial merge-with into))
        (map (fn [[근무자 취침기록]] (취침-기상-시간-freq 근무자 취침기록)))
        (apply max-key :빈도)
        day4-해답))


;; 아래는 이거저거 테스트 하기 위한 comment
((comment
   (근무기록-to-키값-쌍 "[1518-11-01 00:00] Guard #10 begins shift")
   (map 근무기록-to-키값-쌍 (데이터로드 "day4.sample2.txt"))
      (->> (map 근무기록-to-키값-쌍 (데이터로드 "day4.sample2.txt"))
        (reduce 근무자별기록정리 [[] nil])
        first
        (apply (partial merge-with into)))
   (->> (map 근무기록-to-키값-쌍 (데이터로드 "day4.sample2.txt"))
        (reduce 근무자별기록정리 [[] nil])
        first
        (apply (partial merge-with into))
        (map (fn [[근무자 취침기록]] (취침-기상-시간-freq 근무자 (취침-기상-시간-seq 취침기록)))))
   (partition-by #(contains? (set ["[1518-11-01 00:00] Guard #10 begins shift"  "[1518-11-01 23:58] Guard #99 begins shift"]) %) (데이터로드 "day4.sample.txt"))
   (데이터로드 "day4.sample.txt"))
 (->> (map 근무기록-to-키값-쌍 (데이터로드 "day4.sample.txt"))
      (reduce 근무자별기록정리 [[] nil])
      first
      (apply (partial merge-with into))
      (map (fn [[근무자 취침기록]] (취침-기상-시간-freq 근무자 (취침-기상-시간-seq 취침기록))))
      (apply max-key :총취침횟수))
 (->> (map 근무기록-to-키값-쌍 (데이터로드 "day4.sample.txt"))
      (reduce 근무자별기록정리 [[] nil])
      first
      (apply (partial merge-with into))
      (map (fn [[근무자 취침기록]] (취침-기상-시간-freq 근무자 (취침-기상-시간-seq 취침기록))))
      (apply max-key :빈도))
  (->> (map 근무기록-to-키값-쌍 (데이터로드 "day4.sample2.txt"))
        (apply (merge-with into (first (reduce 근무자별기록정리 [[] nil])))))
   
)



;; 조금더 예쁘게 바꿔보는 중
;; (defn pattern []
;;   (let [data ((fn [[key x y width heigt]] {:key key :x x :y y :width width :heigt heigt}) (rest (re-find (re-matcher #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (wakes|falls|Guard) #(\d+)" "[1518-11-01 00:00] Guard #10 begins shift"))))]
;;     (println data)))

;; partition-by를 쓰고 싶었지만 실패한 흔적들..
;; (->> (make-usable-data "day4.sample.txt")
;;      (partition-by #(= re-find (re-matcher #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] Guard #(\d+) begins shift") %))
;; )
;; (
;;  (re-find (re-matcher #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] Guard #(\d+) begins shift" "[1518-11-01 00:00] Guard #10 begins shift"))
;; )

;; (defn filter [대상]
;;   (->> 대상
;;        (re-matcher #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] Guard #(\d+) begins shift")
;;        re-find
;; ))
;; (filter "[1518-11-01 00:00] Guard #10 begins shif")
;; (
;;  (let [a -> ((map #(filter) % (make-usable-data "day4.sample.txt")))]
;;    a)
;; )
;; (defn 가드-아이디-업데이트 [업데이트대상]
    ;;  (println 업데이트대상)
    ;;  (let [value (->> (for [index (range (count 업데이트대상))]
    ;;                     (if (nil? (:id (nth 업데이트대상 index)))
    ;;                       (-> (assoc (nth 업데이트대상 index) :id ((nth 업데이트대상 (dec index)) :id)))
    ;;                       (nth 업데이트대상 index))))]
    ;;    value))

;; {id: aa, value: [ time 00:00
;;                  type 슬립
;;                  time: 00:05
;;                  tye: wake] }

;; sort 


