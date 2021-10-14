(ns pairing-scheduler.core
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set])
  (:import
   (java.time Period DayOfWeek ZonedDateTime ZoneId LocalTime LocalDate)))

(defn overlapping-daytimes
  [guest-ids {:keys [availabilities]}]
  (->> guest-ids
       (map availabilities)
       (map (fn [availabilities]
              (set (map (fn [[day-of-week hour _]]
                          [day-of-week hour]) availabilities))))
       (apply set/intersection)))

(defn random-event
  [guest-ids {:keys [availabilities] :as context}]
  (let [possible-times (overlapping-daytimes guest-ids context)]
    (if (seq possible-times)
      (let [[day-of-week time-of-day] (rand-nth (vec possible-times))]
        {:guest-ids (set guest-ids)
         :day-of-week day-of-week
         :time-of-day time-of-day})
      {:guest-ids (set guest-ids)
       :day-of-week :impossible
       :time-of-day -1})))

(defn remove-from-vec
  [pos coll]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))


(defn ->zoned-date-time
  "inst should be a java.util.Date ex. #inst \"2021-01-01T09\"
   timezone should be a string, ex. \"America/Toronto\""
  [inst timezone]
  (ZonedDateTime/ofInstant (.toInstant inst) (ZoneId/of timezone)))

#_(let [at #inst "2021-01-01T09"
        tz "America/Toronto"
        day-of-week DayOfWeek/WEDNESDAY]
    (->zoned-date-time at tz))

(defn days-over-max
  [guest-id {:keys [timezones max-events-per-day schedule]}]
  (let [guest-events (->> schedule
                          (filter (fn [event]
                                    (contains? (event :guest-ids) guest-id))))]
   (if (and max-events-per-day (max-events-per-day guest-id))
     (->> [DayOfWeek/MONDAY
           DayOfWeek/TUESDAY
           DayOfWeek/WEDNESDAY
           DayOfWeek/THURSDAY
           DayOfWeek/FRIDAY
           DayOfWeek/SATURDAY
           DayOfWeek/SUNDAY]
          (map (fn [target-day-of-week]
                 (< (max-events-per-day guest-id)
                    (->> guest-events
                         (filter (fn [event]
                                   (= target-day-of-week
                                      (.getDayOfWeek (->zoned-date-time (:at event) (timezones guest-id))))))
                         count))))
          (filter true?)
          (count))
     0)))

#_(days-over-max "alice" {:timezones {"alice" "America/Toronto"}
                          :max-events-per-day {"alice" 1}
                          :schedule [{:at #inst "2021-01-01T09"
                                      :guest-ids #{"alice" "bob"}}
                                     {:at #inst "2021-01-01T10"
                                      :guest-ids #{"alice" "bob"}}]})


(defn individual-score
  [guest-id {:keys [schedule availabilities timezones max-events-per-day max-events-per-week topics] :as context}]
  (let [guest-events (->> schedule
                          (filter (fn [event]
                                    (contains? (event :guest-ids) guest-id))))
        guest-open-times  (->> (availabilities guest-id)
                               (map (fn [[inst _]]
                                      inst))
                               set)
        guest-event-times (->> guest-events
                               (map :at))]
    (+ ;; above max for day
      (* 50 (days-over-max guest-id context))

      ;; above max for week
      (if (and max-events-per-week
               (max-events-per-week guest-id)
               (< (max-events-per-week guest-id)
                  (count guest-events)))
       100
       0)

      ;; per event
      (->> guest-events
           (map (fn [event]
                  ;; using negatives for ok events, to promote more events rather than fewer
                  ;; b/c otherwise, an empty schedule would always be a perfect schedule
                  (cond
                    ;; double-scheduled
                    (< 1 (->> guest-event-times
                              (filter (partial = [(event :day-of-week) (event :time-of-day)]))
                              count))
                    200
                    ;; outside of any available times
                    (not (contains? guest-open-times [(event :day-of-week) (event :time-of-day)]))
                    100
                    ;; if it's not with someone with matching topics
                    (and topics ;; ignore this criterion ifno topics passed in
                         (->> (event :guest-ids)
                              (map topics)
                              (apply set/intersection)
                              empty?))
                    99
                    ;; at preferred time
                    (contains? (availabilities guest-id) [(event :day-of-week) (event :time-of-day) :preferred])
                    -5
                    ;; at available time
                    (contains? (availabilities guest-id) [(event :day-of-week) (event :time-of-day) :available])
                    -1)))
           (reduce +)))))

(defn schedule-score
  [{:keys [schedule availabilities] :as context}]
  (->> availabilities
       keys
       (map (fn [guest-id]
              ;; create a non-linearity, to prefer scheduling an event for someone with fewer events than someone with many
              (let [guest-events (->> schedule
                                      (filter (fn [event]
                                                (contains? (event :guest-ids) guest-id))))
                    other-guest-count (->> guest-events
                                           (mapcat :guest-ids)
                                           set
                                           count
                                           dec)]
                (- (* (individual-score guest-id context)
                      (/ (inc (count guest-events)))
                      (Math/pow (count guest-events) 0.5))
                   other-guest-count))))
       (reduce +)))

(defn tweak-schedule
  [{:keys [schedule availabilities] :as context}]
  (case (if (empty? schedule)
          :create-event
          (rand-nth [:move-event :drop-event :create-event]))
    :create-event
    (update context
            :schedule
            conj (let [guest-ids (take 2 (shuffle (keys availabilities)))]
                   (random-event guest-ids context)))

    :drop-event
    (update context
            :schedule
            (fn [schedule]
              (let [event-index (rand-int (count schedule))]
                (remove-from-vec event-index (vec schedule)))))

    :move-event
    (update context
            :schedule
            (fn [schedule]
              (let [event-index (rand-int (count schedule))]
                (update (vec schedule) event-index
                        (fn [event]
                          (random-event (event :guest-ids) context))))))))

(defn optimize-schedule
  [{:keys [schedule availabilities report-fn] :as context}]
  (let [max-iterations 5000
        max-tweaks-per-iteration 4]
    (loop [context context
           iteration-count 0]
      (when report-fn
        (report-fn iteration-count (schedule-score context)))
      (if (> iteration-count max-iterations)
        context
        (let [tweak-count (+ 1 (rand-int max-tweaks-per-iteration))
              tweak-n-times (apply comp (repeat tweak-count tweak-schedule))
              alt-context (tweak-n-times context)]
          (if (< (schedule-score alt-context)
                 (schedule-score context))
            (recur alt-context (inc iteration-count))
            (recur context (inc iteration-count))))))))

(defn generate-initial-schedule
  [times-to-pair {:keys [availabilities] :as context}]
  (let [guest-ids (keys availabilities)
        pairs (combo/combinations guest-ids 2)]
    (assoc context
           :schedule
           (->> pairs
                (repeat times-to-pair)
                (apply concat)
                (map (fn [guest-ids]
                       (random-event guest-ids context)))
                (remove nil?)))))

(defn schedule
  [{:keys [availabilities times-to-pair] :as context}]
  (if (<= (count availabilities) 1)
   (assoc context :schedule [])
   (-> (generate-initial-schedule times-to-pair context)
       optimize-schedule)))

(defn report
  [{:keys [schedule availabilities] :as context}]
  {:schedule schedule
   :event-count (count schedule)
   :score (double (schedule-score context))
   :guests (->> availabilities
                keys
                (map (fn [guest-id]
                       [guest-id (let [guest-events (->> schedule
                                                         (filter (fn [event]
                                                                   (contains? (event :guest-ids) guest-id))))]
                                   {:score (double (individual-score guest-id context))
                                    :count (count guest-events)
                                    :unique (count (disj (set (mapcat :guest-ids guest-events)) guest-id))})]))
                (into {}))})

(defn -main [])
