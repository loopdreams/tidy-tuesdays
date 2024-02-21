;; # Week 7 - Valentine's Day Consumer Data

(ns tidy-tuesdays.2024.week07.valentines
  (:require
   [aerial.hanami.templates :as ht]
   [clojure.string :as str]
   [scicloj.noj.v1.vis.hanami :as hanami]
   [tablecloth.api :as tc]
   [scicloj.kindly.v4.kind :as kind]))


;; ## Introduction
;;
;; This week's datasets are related to consumer spending during valentines day from the National Retail Federation in the United States.
;; There are three datasets, loaded below.
;;
;; I discovered later that the csv files contain a BOM, so the `clean-keyword` function removes this.

(defn clean-keyword [keyword]
  (str/replace keyword #"[\"\" \p{C}]" ""))

(def historical_spending (->  "data/2024/week07/historical_spending.csv"
                              (tc/dataset {:key-fn (comp keyword clean-keyword)})))

(def gifts_age (-> "data/2024/week07/gifts_age.csv"
                   (tc/dataset {:key-fn (comp keyword clean-keyword)})))

(def gifts_gender (-> "data/2024/week07/gifts_gender.csv"
                      (tc/dataset {:key-fn (comp keyword clean-keyword)})))


;; ## Spending Trends
;;
;; Let's start with something very basic - the trend for average spending per year.

(-> historical_spending
    (hanami/plot ht/line-chart
                 {:TITLE "Avg Spend Per Year"
                  :Y :PerPerson :YTYPE :quantitative
                  :X :Year :XTYPE :temporal
                  :SIZE 3}))

;; A dip in 2021, presumably due to the pandemic.
;;
;; Next, the average spend per item type.
;;
;; First let's restructure the data from a format that looks like
;; `{:Year 2020 :Candy 23 :Flowers 56 ...}` to a format like this: `[{:year 2020 :type "Candy" :value 23} ...]`

(def restructured-historical
  (reduce (fn [acc row]
            (let [year (:Year row)
                  vals (dissoc row :Year)]
              (into acc
                    (for [v    vals
                          :let [[type val] v]]
                      {:year  year
                       :type  (name type)
                       :value val}))))
          []
          (-> historical_spending
              (tc/drop-columns [:PerPerson :PercentCelebrating])
              (tc/rows :as-maps))))

(-> restructured-historical
    (hanami/plot ht/line-chart
                 {:X :year :XTYPE :ordinal
                  :Y :value :YTYPE :quantitative :YTITLE "Avg Spend"
                  :COLOR {:field :type :type :nominal :sort "-y" :title "Item"}
                  :SIZE 3
                  :WIDTH 500}))

;; Let's rank the items by average spend over the period:

(def items-avg-spend
  (-> restructured-historical
      (tc/dataset)
      (tc/group-by :type)
      (tc/aggregate {:avg-spend #(/ (reduce + (% :value)) (count (% :value)))})
      (tc/rename-columns {:$group-name :Item})))

(-> items-avg-spend
    (hanami/plot ht/bar-chart
                    {:X :Item :XTYPE :nominal :XSORT "-y"
                     :Y :avg-spend :YTPE :quantitative}))
      


;; ## Gender
;;
;;
(kind/table
 gifts_gender)

(defn items-split [data color]
  (reduce (fn [result entry]
            (let [colr (color entry)
                  ks (keys (dissoc entry color))]
              (conj result
                    (for [k ks]
                      {color colr
                       :Percent (k entry)
                       :Item (name k)}))))
          []
          data))

(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values
         (reduce concat
                 (items-split
                  (-> gifts_gender
                      (tc/drop-columns :SpendingCelebrating)
                      (tc/rows :as-maps))
                  :Gender))}
  :mark :bar
  :width 500
  :height 400
  :encoding {:x {:field :Item :sort "-y"
                 :axis {:labelAngle -45}}
             :y {:field :Percent :type :quantitative}
             :xOffset {:field :Gender}
             :color {:field :Gender :scale {:scheme "category20"}}}
  :title {:text "Valentine's Day"
          :subtitle "Spending Trends By Gender"}})

;; ## Age

(kind/md
 (let [most-likely-to-celebrate (-> gifts_age
                                    (tc/order-by :SpendingCelebrating :desc)
                                    :Age
                                    first)
       least-likely-to-celebrate (-> gifts_age
                                     (tc/order-by :SpendingCelebrating)
                                     :Age
                                     first)]
   (str "According to the data, the **"
        most-likely-to-celebrate
        "** age group were the most likely to celebrate Valentine's day, while the **"
        least-likely-to-celebrate
        "** age group were the least likely.")))

(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values
         (reduce concat
                 (items-split
                  (-> gifts_age
                      (tc/drop-columns :SpendingCelebrating)
                      (tc/rows :as-maps))
                  :Age))}
  :mark :bar
  :width 500
  :height 400
  :encoding {:x {:field :Item :sort "-y"
                 :axis {:labelAngle -45}}
             :y {:field :Percent :type :quantitative}
             :xOffset {:field :Gender}
             :color {:field :Gender :scale {:scheme "blues"}}}
  :title {:text "Valentine's Day"
          :subtitle "Spending Trends By Age Group"}})

;; As we can see, the most popular item was Candy, most likely to be bought by an 18-24 year old.

;; ## Combining some of the data
;;
;; So far, we know that **Jewelry** is the best in terms of the level of spending, but **Candy** is the most popular choice (at a lower cost).
;; Let's imagine we are doing market research, which is the best item to invest in (combining both of these datapoints)?

;; First, let's get the average popularity of an item across the age groups. I will also multiply the items by the 'SpendingCelebrating' column,
;; to get a 'weighted' average. (I'm making this up as I go along, so not 100% sure on the methodology here)

(def valentines-items (-> gifts_age
                          (tc/drop-columns [:Age :SpendingCelebrating])
                          keys))

(defn average [coll]
  (float
   (/ (reduce + coll) (count coll))))


(def item-average-popularity
  (tc/dataset
   (for [item valentines-items]
     (let [weighted-avgs
           (-> gifts_age
               (tc/map-columns :target
                               [:SpendingCelebrating item]
                               #(* %1 %2))
               :target)]
       {:Item (name item)
        :Average-popularity
        (/
         (average weighted-avgs)
         100)}))))

;; Next, let's join with the spending dataset...



(kind/table
 (-> item-average-popularity
    (tc/inner-join items-avg-spend :Item)))

;; Now, let's calculate a 'score' (popularity * cost), and sort the items by the highest score:

(kind/table
 (-> item-average-popularity
     (tc/inner-join items-avg-spend :Item)
     (tc/map-columns :score [:Average-popularity :avg-spend]
                     #(int (* %1 %2)))  ;; Rounding these for aesthetic purposes in the table.
     (tc/order-by :score :desc)))

;; **Evening Out** is the winner!🎉
