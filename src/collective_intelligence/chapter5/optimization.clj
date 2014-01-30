(ns collective-intelligence.chapter5.optimization
  (:require [clojure.java.io :refer [as-file reader]]))

(def people [["Seymore" "BOS"]
             ["Franny" "DAL"]
             ["Zooey" "CAK"]
             ["Walt" "MIA"]
             ["Buddy" "ORD"]
             ["Les" "OMA"]])

(def destination "LGA")

(defn get-minutes
 [time-str]
 (let [hours (-> time-str (clojure.string/split #":") first Integer/parseInt)
       minutes (-> time-str (clojure.string/split #":") last Integer/parseInt)]
   (+ minutes (* hours 60))))

(defn read-data
  [resource]
  (line-seq (reader resource)))

(defn clean-data
  [data]
  (map #(clojure.string/split % #",") data))

(def data (-> "resources/schedule.txt" read-data clean-data))

(defn to-maps
  [data]
  (map #(hash-map [(first %) (second %)]
                  [[(nth % 2) (nth % 3) (Integer/parseInt (last %))]]) data))

(defn flights
  [data]
  (apply merge-with concat (to-maps data)))

(def flight-data (flights data))

(defn in-f
  [m o d i]
  (nth (get m [(nth o d) destination]) i))

(defn print-schedule
  [r]
  (let [people-names (map first people)
        origins (map last people)]
    (doseq [d (range 0 (count r) 2)]
      (println (format "%10s%10s %5s-%5s $%3s %5s-%5s $%3s"
                       (nth people-names (/ d 2))
                       (nth origins (/ d 2))
                       (nth (in-f flight-data origins (/ d 2) (nth r d)) 0)
                       (nth (in-f flight-data origins (/ d 2) (nth r d)) 1)
                       (nth (in-f flight-data origins (/ d 2) (nth r d)) 2)
                       (nth (in-f flight-data origins (/ d 2) (nth r (+ 1 d))) 0)
                       (nth (in-f flight-data origins (/ d 2) (nth r (+ 1 d))) 1)
                       (nth (in-f flight-data origins (/ d 2) (nth r (+ 1 d))) 2))))))

(def solution [1 4 3 2 7 3 6 3 2 4 5 3])

(comment
  (print-schedule [1 4 3 2 7 3 6 3 2 4 5 3])
  (println (flights data)))


(defn origin-for
  [i]
  (last (people i)))

(defn ob-flights-for
  [i]
  (vec (flight-data [(origin-for i) destination])))

(defn r-flights-for
  [i]
  (vec (flight-data [destination (origin-for i)])))

(defn outbound-cost
  [idx solution]
  (let [flight-number (solution idx)]
    (last ((ob-flights-for idx) flight-number))))

(defn return-cost
  [idx solution]
  (let [flight-number (solution (inc idx))]
    (last ((r-flights-for idx) flight-number))))

(defn total-cost
  [solution]
  (let [people-range (range (/ (count solution) 2))
        outbound-costs (map #(outbound-cost % solution) people-range)
        return-costs (map #(return-cost % solution) people-range)]
    (+ (reduce + outbound-costs) (reduce + return-costs))))

(comment
  (outbound-cost 0 solution)
  (return-cost 0 solution)
  (total-cost solution)
)

(defn latest-arrival
  [solution]
  (let [arrival-time #((r-flights-for %) (solution %))]
    (map (comp get-minutes second arrival-time) (range (/ (count solution) 2)))))

(defn earliest-departure
  [solution]
  (let [departure-time #((ob-flights-for %) (solution (inc %)))]
    (map (comp get-minutes second departure-time) (range (/ (count solution) 2)))))

(latest-arrival solution)
(earliest-departure solution)

(defn total-wait
  [solution]
  (let [people-range (range (/ (count solution) 2))
        outbound-wait (comp ; XXX BROKEN ob-flights-for
                       )
        outbound-waits (map outbound-wait people-range)
        return-wait (comp get-minutes first r-flights-for)
        return-waits (map return-wait people-range)]
    (println (outbound-wait 0))
    (+ (reduce + outbound-waits) (reduce + return-waits))))

(total-wait solution)

(defn schedule-cost
  [solution]
  (let [total-price 0
        latest-arrival 0
        earliest-departure (*24 60)]
    (doseq [d (range (/ (count solution) 2))]
      (let [origin (last (d people))
            outbound (get-in flights [[origin destination] (d solution)])
            return (get-in flights [[origin destination] ((inc d) solution)])]
        ))))
