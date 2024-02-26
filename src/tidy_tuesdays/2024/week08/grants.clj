;; # Week 8 - ISC Grants

(ns tidy-tuesdays.2024.week08.grants
  (:require
   [clojure.string :as str]
   [scicloj.kindly.v4.kind :as kind]
   [tablecloth.api :as tc]
   [scicloj.noj.v1.vis.hanami :as hanami]
   [aerial.hanami.templates :as ht]))

;; ## Introduction
;; This week's dataset contains information about the R Consortium Infrastructure Steering Committee (ISC) Grant Program.
;;
;; Grants have been provided since 2016

(def DS (tc/dataset "data/2024/week08/isc_grants.csv" {:key-fn keyword}))

(kind/table
 (-> DS (tc/info :columns)))

;; The 'group' category here refers to whether the grant was awarded in the spring cycle (1) or the fall cycle (2).
;;
;; ## Keywords
;; This week's entry contains the prompt "Are there any keywords that stand out in the titles or summaries of awarded grants? Have the funded amounts changed over time?"
;;
;; In that spirit, let's try look at the titles and summaries for keywords.
;;
;; First, the titles.

(def stopwords (str/split-lines (slurp "data/language/NLTK's list of english stopwords")))

(defn split-words [string]
  (->> (str/split string #" ")
       (map str/lower-case)
       (map #(str/replace % #"[():“”-]" ""))
       (map #(str/replace % #"[\,\.\n]" " "))
       (map str/trim)
       (remove (into #{} stopwords))
       (remove #{""})))

;; ### Top 10 Keywords in Titles
(kind/md
 (str/join "\n"
           (for [kw
                 (->>
                  (apply str (:title DS))
                  split-words
                  frequencies
                  (sort-by val)
                  reverse
                  (take 10))
                 :let [[word count] kw]]
             (str "* " word " (" count ")"))))

;; Unsurprisingly, 'R' is the top keyword. Let's look at the titles containing the word **spatial**:

(kind/table
 (-> DS
     (tc/select-rows #(re-find #"spatial" (str/lower-case (% :title))))
     (tc/select-columns [:title :year :funded])))

;; ### Most expensive 'keywords'
;; Let's try assign 'values' to the words using the funding amounts.

(def word-costs
  (let [all-costs
        (-> DS
            (tc/select-columns [:title :funded])
            (tc/map-columns :word-costs [:title :funded]
                            (fn [title funded]
                              (let [words (split-words title)]
                                (zipmap words (repeat [funded])))))
            :word-costs)]
    (reduce (fn [result entry]
              (merge-with into result entry))
            {}
            all-costs)))

;; Now we have something like this:

(take 10 word-costs)

;; Next, I'm not sure whether to 'average' the costs, or to sum them. I'll try both.

(def summed-costs
  (reduce (fn [new-m [k v]]
            (assoc new-m k
                   (reduce + v)))
          {}
          word-costs))

(def averaged-costs
  (reduce (fn [new-m [k v]]
            (assoc new-m k
                   (/ (reduce + v)
                      (count v))))
          {}
          word-costs))


;; Highest cost words (summed):

(take 10 (reverse (sort-by val summed-costs)))

;; Lowest cost words (summed):

(take 10 (sort-by val summed-costs))

;; Highest cost words (averaged):

(take 10 (reverse (sort-by val averaged-costs)))

;; Lowest cost words (averaged):

(take 10 (sort-by val averaged-costs))

;; As expected, when taking the sum of the funding amounts for projects with these words in the titles, the top words are similar
;; to those which occur most often.
;;
;; What is perhaps more interesting is when the averaged costs are taken.
;;
;; Out of interest, let's take a look at the projects with 'ongoing' in the title:

(kind/table
 (-> DS
     (tc/select-rows #(re-find #"ongoing" (str/lower-case (% :title))))
     (tc/select-columns [:title :year :funded])))

;; Just one entry! And an expensive project (containing the two most 'expensive' words)

;; ### Keywords in summaries
;;
;; Let's adopt the same methodology as above, but with the summary texts.

(kind/md
 (str/join "\n"
           (for [kw
                 (->>
                  (apply str (:summary DS))
                  split-words
                  frequencies
                  (sort-by val)
                  reverse
                  (take 20))
                 :let [[word count] kw]]
             (str "* " word " (" count ")"))))



;; Let's try make a word cloud with these words using Vega:

(kind/vega
 {:$schema "https://vega.github.io/schema/vega/v5.json"
  :width   800
  :height  400
  :padding 0
  :data    [{:name   "table"
             :values [(apply str (:summary DS))]
             :transform
             [{:type "countpattern"
               :field "data"
               :case "upper"
               :pattern "[\\w']{3,}"
               :stopwords (str "(" (str/join "|" stopwords) ")")}
              {:type "formula" :as "angle"
               :expr "[-45, 0, 45][~~(random() * 3)]"}]}]
  :scales  [{:name   "color"
             :type   "ordinal"
             :domain {:data "table" :field "text"}
             :range  ["#d5a928", "#652c90", "#939597"]}]
  :marks   [{:type   "text"
             :from   {:data "table"}
             :encode {:enter
                      {:text     {:field "text"}
                       :align    {:value "center"}
                       :baseline {:value "alphabetic"}
                       :fill     {:scale "color" :field "text"}}
                      :update {:fillOpacity {:value 1}}
                      :hover  {:fillOpacity {:value 0.5}}}
             :transform
             [{:type          "wordcloud"
               :size          [800 400]
               :text          {:field :text}
               :rotate        {:field "datum.angle"}
               :font          "Helvetica Neue, Arial"
               :fontSize      {:field "datum.count"}
               :fontWeight     600
               :fontSizeRange [12, 56]
               :padding       2}]}]})

;; Finally, let's look at the words in close proximity to 'r'

(def r-splits
  (->> (apply str (:summary DS))
       split-words
       (partition-by (fn [word] (= word "r")))
       (remove (fn [coll] (= (first coll) "r")))))



(def words-before-r (map last (drop-last r-splits)))
(def words-after-r (map first (rest r-splits)))

;; ### Words in Close Proximity to 'R' in summaries

(kind/vega
 {:$schema "https://vega.github.io/schema/vega/v5.json"
  :width   800
  :height  400
  :padding 0
  :data    [{:name   "table"
             :values [(str/join #" " (concat words-after-r words-before-r))]
             :transform
             [{:type "countpattern"
               :field "data"
               :case "upper"
               :pattern "[\\w']{3,}"
               :stopwords ""}
              {:type "formula" :as "angle"
               :expr "[-45, 0, 45][~~(random() * 3)]"}]}]
  :scales  [{:name   "color"
             :type   "ordinal"
             :domain {:data "table" :field "text"}
             :range  ["#d5a928", "#652c90", "#939597"]}]
  :marks   [{:type   "text"
             :from   {:data "table"}
             :encode {:enter
                      {:text     {:field "text"}
                       :align    {:value "center"}
                       :baseline {:value "alphabetic"}
                       :fill     {:scale "color" :field "text"}}
                      :update {:fillOpacity {:value 1}}
                      :hover  {:fillOpacity {:value 0.5}}}
             :transform
             [{:type          "wordcloud"
               :size          [800 400]
               :text          {:field :text}
               :rotate        {:field "datum.angle"}
               :font          "Helvetica Neue, Arial"
               :fontSize      {:field "datum.count"}
               :fontWeight     600
               :fontSizeRange [12, 56]
               :padding       2}]}]})

;; ## Funding Levels by year

(defn period-num->name [num]
  (case num
    1 "Spring Cycle"
    2 "Fall Cycle"))

(defn aggregate-by-year [agg-fn]
  (remove nil?
          (reduce (fn [result data]
                    (let [year (first (:year data))
                          agg (-> data
                                  (tc/group-by :group)
                                  (tc/aggregate {:amount agg-fn})
                                  (tc/rows :as-maps))]
                      (conj result
                            {:year year
                             :period (period-num->name (:$group-name (first agg)))
                             :amount (:amount (first agg))}
                            (when (= 2 (count agg))
                              {:year year
                               :period (period-num->name (:$group-name (second agg)))
                               :amount (:amount (second agg))}))))
                  []
                  (-> DS
                      (tc/group-by :year)
                      :data))))


(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values (aggregate-by-year #(reduce + (% :funded)))}
  :mark :bar
  :width 500
  :height 400
  :encoding {:x {:field :year :type :ordinal
                 :axis {:labelAngle -45}
                 :title "Year"}
             :y {:field :amount :type :quantitative
                 :title "Total Funding Amount"}
             :color {:field :period}}
  :title {:text "R ISC Grants"
          :subtitle "Funding Levels by Year"}})


;; ## Number of projects funded by year

(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values (aggregate-by-year #(count (% :title)))}
  :mark :bar
  :width 500
  :height 400
  :encoding {:x {:field :year :type :ordinal
                 :axis {:labelAngle -45}
                 :title "Year"}
             :y {:field :amount :type :quantitative
                 :title "Total Projects Funded"}
             :color {:field :period}}
  :title {:text "R ISC Grants"
          :subtitle "Projects Funded by Year"}})

;; While the levels of funding can vary quite a bit, the number of projects funded per year seems more constant.
;;
;; Let's look at the 'best value' years by dividing the total funding by projects.
;;
;; In this case, I won't look at whether they were spring/fall projects, and will also ignore 2023 (only a half-year of data).

(-> DS
    (tc/drop-rows #(= 2023 (% :year)))
    (tc/group-by :year)
    (tc/aggregate {:funding-per-project #(int (/ (reduce + (% :funded))
                                                 (count (% :title))))})
    (tc/rename-columns {:$group-name :year})
    (hanami/plot ht/line-chart
                 {:X :year :XTYPE :temporal
                  :Y :funding-per-project
                  :SIZE 4
                  :TITLE "Average Cost per Project"}))

;; 2021 had the best 'value' projects...

(-> DS
    (tc/drop-rows #(= 2023 (% :year)))
    (tc/group-by :year)
    (tc/aggregate {:funding #(reduce + (% :funded))
                   :projects #(count (% :title))})
    (tc/rename-columns {:$group-name :year})
    (hanami/plot ht/point-chart
                 {:X :funding :XTYPE :quantitative
                  :Y :projects :YTPYE :quantitative
                  :COLOR {:field :year}
                  :SIZE 150}))

;; Looks like 2021 was the best year in terms of the most number of projects at the lowest cost.
;;
;; ## Successful Developers

;; Finally, let's have a quick look at the most successful applicants. The third column 'percentage total' is the percentage of the total
;; grant funding provided throughout the lifetime of this program.


(kind/table
 (let [total-funding-provided (reduce + (:funded DS))]
   (-> DS
       (tc/group-by :proposed_by)
       (tc/aggregate {:funding-secured #(reduce + (% :funded))
                      :projects #(tc/row-count %)})
       (tc/map-columns :percentage-total [:funding-secured]
                       #(int (* 100 (/ % total-funding-provided))))
       (tc/rename-columns {:$group-name :Proposed-By})
       (tc/order-by [:percentage-total :funding-secured] [:desc :desc])
       (tc/select-rows (range 10)))))
