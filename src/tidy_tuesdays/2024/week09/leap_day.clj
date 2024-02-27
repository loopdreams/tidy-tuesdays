;; # Week 9 - Leap Day

(ns tidy-tuesdays.2024.week09.leap-day
  (:require
   [scicloj.kindly.v4.kind :as kind]
   [tablecloth.api :as tc]))


;; ## Introduction
;;
;; The challenge this week is to examine Wikipedia's information on Leap Year and
;; identify **underrepresented** leap years within the data.
;;
;; There are three associated datasets, Feb 29th **births**, **deaths**, and **events**.

(def births (tc/dataset "data/2024/week09/births.csv" {:key-fn keyword}))
(def deaths (tc/dataset "data/2024/week09/deaths.csv" {:key-fn keyword}))
(def events (tc/dataset "data/2024/week09/events.csv" {:key-fn keyword}))

;; ## Most eventful Leap Years

;; Let's start by combining the datasets and looking at the number of *any* events (birth, death, events) by year.
;;
;; ### Detour
;;
;; The **births** and **deaths** datasets contain both the birth year and death year each, I'm presuming that in the case of the **births**
;; dataset for example, the 'birth year' is the leap year. But, just to be sure, I will also use a function to check that:

(defn leap-year? [year]
  (when year
    (cond
      (zero? (mod year 400)) true
      (zero? (mod year 100)) false ;; apparently a leap year is not divisible by 100 (except multiples of 400)
      (zero? (mod year 4)) true
      :else false)))



(every? true? (map leap-year? (:year_birth births)))

;; All birth-years in the **births** dataset are leap years. However:

(count
 (filter true? (map leap-year? (:year_death births))))

;; There are also 14 deaths which occurred on leap years in the **births** dataset


(every? true? (map leap-year? (:year_death deaths)))

(count
 (filter true? (map leap-year? (:year_birth deaths))))

;; In the case of the **deaths** dataset, there were 9 rows which also included births that were leap years.

;; Let's have a quick look at see if these datasets overlap:

(kind/table
 (-> births
     (tc/select-rows #(leap-year? (% :year_death)))))

;; These are the people with both births and deaths on leap years in the **births** dataset, let's check if any of these are in the **deaths** dataset:

(some (into #{} (-> births
                    (tc/select-rows #(leap-year? (% :year_death)))
                    :person))
      (:person deaths))

;; Only one!
;;
;; Okay, so I've missed something pretty obvious here...these datasets (of course!) record people who were born or died on the **29th February**.
;; So, even though someone was born on 29th February, and died in a leap year, it doesn't mean they died on 29th of February too. However,
;; this moment of stupidity ended up leading to finding an interesting overlap with the datasets, **James Milne Wilson** was someone who was
;; born on, and who died on the 29th of February!!
;;
;; ### Return to the initial question
;;
;; Back to the initial challenge, and I was trying to merge the datasets...


(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values
         (filter #(< 1 (:count %))
                 (reduce (fn [merged ds]
                           (let [type (tc/dataset-name ds)
                                 rows (tc/rows ds :as-maps)]
                             (into merged
                                   (for [r rows]
                                     (assoc r :type type)))))
                         []
                         [
                          (-> births
                              (tc/group-by :year_birth)
                              (tc/aggregate {:count #(tc/row-count %)})
                              (tc/rename-columns {:$group-name :year})
                              (tc/set-dataset-name :births))
                          (-> deaths
                              (tc/group-by :year_death)
                              (tc/aggregate {:count #(tc/row-count %)})
                              (tc/rename-columns {:$group-name :year})
                              (tc/set-dataset-name :deaths))
                          (-> events
                              (tc/group-by :year)
                              (tc/aggregate {:count #(tc/row-count %)})
                              (tc/rename-columns {:$group-name :year})
                              (tc/set-dataset-name :events))]))}
  :title "Years With 2+ birth/death/event on Feb 29th (Wikipedia)"
  :mark :bar
  :height 400
  :width 600
  :encoding {:x {:field :year
                 :type :nominal
                 :sort "-y"
                 :title "Year"}
             :y {:field :count
                 :type :quantitative
                 :title "Count"}
             :color {:field :type
                     :type :nominal
                     :scale {:scheme "tableau20"}}}})

;; The most 'eventful' 29th of Februaries were in **1928**, **1980**, **1984**, and **1996**.

;; ## Underrepresented Years
;;
;; Let's try find the years where nothing was recorded by Wikipedia. First, lets find the earliest year from the datasets:

(-> births
    :year_birth
    sort
    first)

(-> deaths
    :year_death
    sort
    first)

(-> events
    :year
    sort
    first)

;; It appears to be **468**. As a side note, who died that year?
;;

(-> deaths
    (tc/select-rows #(= 468 (% :year_death)))
    :person
    first)

;; A Pope with a funny name apparently.
;;
;; Now, let's generate all leap years that would occur between that date and the current year (2024):

(def valid-leap-years
  (filter leap-year? (range 468 2021)))

;; Now, let's remove the leap years where something happened from the valid leap years, to get a count of the leap years where nothing
;; is recorded as having happened:

(count
 (remove
  (into #{}
        (concat
         (:year events)
         (:year_birth births)
         (:year_death deaths)))
  valid-leap-years))

;; Wow, a lot of 29ths of February where 'nothing' happened. Let's represent this as a proportion of all leap years since 468:

(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :title "What Happened on Feb 29th (468 - 2020)?"
  :data {:values [{:name "Nothing Happened"
                   :value 309}
                  {:name "Something Happened"
                   :value (- (count valid-leap-years) 309)}]}
  :encoding {:theta {:field :value, :type :quantitative :stack "normalize"}
             :color {:field :name :type :nominal}
             :order {:field :value :sort "ascending"}}
  :layer [{:mark {:type :arc
                  :outerRadius 90
                  :tooltip true}}
          {:mark {:type "text"
                  :radius 110
                  :fontSize 16}
           :encoding
           {:text {:field :value}}}]
  :config {:legend {:title ""
                    :direction :horizontal
                    :orient :top}}})


;; Based on the years 468 - 2020, there is only **18%** chance of something 'historical' happening this Thursday! :)
