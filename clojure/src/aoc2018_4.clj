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

(defn make-usable-data
  "입력: 파일 경로
   출력: (1 740 926 5 4)"
  [filename]
  (println filename)
  (->> (io/resource filename)
       slurp
       (str/split-lines)
       ))


(defn make-hashmap [변환대상]
  ((fn [[year month day hour min type id]] {:year year :month month :day day :hour hour :min min :type type :id id}) 변환대상))

(defn pattern []
  (let [data ((fn [[key x y width heigt]] {:key key :x x :y y :width width :heigt heigt}) (rest (re-find (re-matcher #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (wakes|falls|Guard) #(\d+)" "[1518-11-01 00:00] Guard #10 begins shift"))))]
    (println data)))

(defn pattern2 [변환대상]
  (println "변환대상 "  변환대상)
  (let [data (->> 변환대상
                  (re-matcher #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (wakes|falls|Guard) #(\d+)")
                  re-find
                  rest
                  make-hashmap)]
    (println data)))
(pattern2 "[1518-11-01 00:00] Guard #10 begins shift")
(make-hashmap "[1518-11-01 00:00] Guard #10 begins shift")
(map (pattern2 (make-usable-data "day4.sample.txt")))
(map(make-usable-data "day4.sample.txt"))
(first (make-usable-data "day4.sample.txt"))
(make-usable-data "day4.sample.txt")
;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

