(ns aoc2020-4
  (:require [clojure.core]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as cset]
            [clojure.spec.alpha :as spec]))

(defn 단락분리 [입력값]
  "입력된 데이터가 \n\n으로 구분 되어 있어서 단락 구분을 위해 split"
  (->> (str/split 입력값 #"\n\n")))

(def data-load
  (->>  (io/resource "2020_day4.sample-light.txt")
        slurp
        단락분리))

(def 값이숫자인필수키 #{:byr :iyr :eyr})
(def 단위가있는필수키 #{:hgt})
(def 조건이없는필수키 #{:ecl  :pid :hcl})
(def 필수키 (cset/union 값이숫자인필수키 단위가있는필수키 조건이없는필수키))

(defn 문자열->키값 [키 값]
  (cond
    (값이숫자인필수키 키) (Integer/parseInt 값)
    (단위가있는필수키 키) (let [[수치 단위] (rest (re-find (re-matcher #"([\d]+)?([a-z]+)?" 값)))]
                    {:value (Integer/parseInt 수치)
                     :unit 단위})
    (조건이없는필수키 키) (str 값)))

(defn 문자열-여권정보->키값-여권정보 [문자열-여권정보]
  "문자열로 된 여권 정보를 받아서 키값 형태의 여권정보로 변환
   입력: ecl:gry pid:860033327 eyr:2020 hcl:#fffffd" "byr:1937 iyr:2017 cid:147 hgt:183cm
   출력: {:ecl gry :pid 860033327 ...}"
  (->> 문자열-여권정보
       (re-seq #"(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([a-zA-Z0-9#]+)")
       (reduce (fn [결과값 [_키값 키 값]]
                 (assoc 결과값 (keyword 키) (문자열->키값 (keyword 키) 값)))
               {})))
;; spec/conform -> 맞는 passport일 경우 passport 자체가 반환, 아니면 에러 메시지 -> coercion
;; valid /conform 을 coercion 용도로 사용하지 마라!

;; ## 파트 1
;; 여권이 유효한지 판단하려고 한다. 여권에는 다음과 같은 필드가 있음.
;; - byr (Birth Year)
;; - iyr (Issue Year)
;; - eyr (Expiration Year)
;; - hgt (Height)
;; - hcl (Hair Color)
;; - ecl (Eye Color)
;; - pid (Passport ID)
;; - cid (Country ID)

;; 파트 1에서는 여권의 모든 필드가 존재하는지의 여부를 검사한다. 주어진 입력에서 '유효한' 여권의 숫자를 반환하여라.

;; ```
;; ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
;; byr:1937 iyr:2017 cid:147 hgt:183cm

;; iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
;; hcl:#cfa07d byr:1929

;; hcl:#ae17e1 iyr:2013
;; eyr:2024
;; ecl:brn pid:760753108 byr:1931
;; hgt:179cm

;; hcl:#cfa07d eyr:2025 pid:166559648
;; iyr:2011 ecl:brn hgt:59in
;; ```

;; - 첫번째는 유효한 여권이다. 8개의 필드가 전부 존재한다.
;; - 두번째는 유효하지 않다. hgt가 없기 때문.
;; - 세번째는 cid가 없지만, ** cid는 없어도 되는 ** 것으로 간주한다. 그래서 유효하다.
;; - 네번째는 cid와 byr이 없다. byr은 반드시 있어야하는 필드이므로 유효하지 않다.

(defn 모든필수키존재? [여권정보]
  "여권 정보에 모든 필수정보가 들어있는지 확인"
  (->> (cset/difference
        필수키
        (set (keys 여권정보)))
       empty?))

(defn 해결-part1 [data]
  (->> data
       (map 문자열-여권정보->키값-여권정보)
       (filter 모든필수키존재?)
       count))

(comment
  (해결-part1 data-load))


;; ## 파트 2
;; 파트1에서는 필드의 유무만을 검사했다면, 파트2에서는 구체적인 범위가 주어진다.
;; - byr (Birth Year) - 4 자리 숫자; 최소 1920 & 최대 2002.
;; - iyr (Issue Year) - 4 자리 숫자; 최소 2010 & 최대 2020.
;; - eyr (Expiration Year) - 4 자리 숫자; 최소 2020 & 최대 2030.
;; - hgt (Height) - 마지막에 cm 혹은 in이 오는 숫자:
;; - cm의 경우, 숫자는 최소 150 & 최대 193.
;; - in의 경우, 숫자는 최소 59 & 최대 76.
;; - hcl (Hair Color) - #뒤에 오는 정확히 6개의 캐릭터 0-9 혹은 a-f.
;; - ecl (Eye Color) - 정확히 amb blu brn gry grn hzl oth 중 하나.
;; - pid (Passport ID) - 처음 0을 포함하는 9자리 숫자.
;; - cid (Country ID) - 없어도 됨.

;; 아래는 예시들이다.
;; ```
;; byr valid:   2002
;; byr invalid: 2003

;; hgt valid:   60in
;; hgt valid:   190cm
;; hgt invalid: 190in
;; hgt invalid: 190

;; hcl valid:   #123abc
;; hcl invalid: #123abz
;; hcl invalid: 123abc

;; ecl valid:   brn
;; ecl invalid: wat

;; pid valid:   000000001
;; pid invalid: 0123456789
;; ```
;; 모든 필드의 기준에 맞는 여권의 수를 반환하여라.
(spec/def ::byr (spec/int-in 1920 2003))
(spec/def ::iyr (spec/int-in 2010 2021))
(spec/def ::eyr (spec/int-in 2020 2031))
(spec/def ::hgt (fn [{값 :value 단위 :unit}]
                  (or
                   (and (= 단위 "cm") ((spec/int-in 150 194) 값))
                   (and (= 단위 "in") (<= 59 값 76)))))
(spec/def ::hgt (fn [{값 :value 단위 :unit}]
                  (or
                   (and (= 단위 "cm") (<= 150 값 193))
                   (and (= 단위 "in") (<= 59 값 76)))))
(spec/def ::hcl #(re-matches #"#[0-9a-f]{6}" %))
(spec/def ::ecl #(re-matches #"(amb|blu|brn|gry|grn|hzl|oth)" %))
(spec/def ::pid #(re-matches #"\d{9}" %))
(spec/def ::passport (spec/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]))

(defn 여권정보값이모두유효인지? [passport]
  "여권 정보에 모든 필수정보가 모두 유효한 값인지 확인"
  (spec/valid? ::passport passport))

(defn 해결-part2 [input]
  (->> input
       (map 문자열-여권정보->키값-여권정보)
       (filter 여권정보값이모두유효인지?)
       count))

(comment
  (해결-part2 data-load))