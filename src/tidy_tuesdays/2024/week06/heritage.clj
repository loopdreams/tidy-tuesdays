;; # Week 6 - World Heritage Sites
(ns tidy-tuesdays.2024.week06.heritage
  (:require
   [scicloj.kindly.v4.kind :as kind]))


;; ## Introduction
;; This week the challenge is to work with a small dataset and produce interesting visualisations.
;; This is a great exercise for me personally, because so far (in my work with clojure and data) I
;; have been more interested in processing and exploring the data, and I tent to be a bit 'lazy'
;; when it comes to details around the visualisations and aesthetics
;;
;; To start with, here is the simple dataset we are working with:


(kind/table
 {:column-names ["Country" "2004" "2022"]
  :row-maps [{"Country" "Norway" "2004" 5 "2022" 8}
             {"Country" "Denmark" "2004" 4 "2022" 10}
             {"Country" "Sweeden" "2004" 13 "2022" 15}]})

;; ## Basic Bar Chart
;; Let's start with a basic bar chart, but I want to try make it a bit 'tidier' and include a nice
;; title/subtitle (using thing repo as a reference point - https://github.com/aezarebski/vegacookbook.
;;
;; First, I will restucture the data a bit (to make it fit only way I know how to do 'grouped' bar charts...)

(def heritage-sites
  [{:country "Norway" :value 5 :year 2004}
   {:country "Norway" :value 8 :year 2022}
   {:country "Denmark" :value 4 :year 2004}
   {:country "Denmark" :value 10 :year 2022}
   {:country "Sweeden" :value 13 :year 2004}
   {:country "Sweeden" :value 15 :year 2022}])


(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values heritage-sites}
  :mark :bar

  :height 400
  :width 100

  :encoding {:column {:field :country
                      :type :ordinal
                      :title nil
                      :header {:labelOrient :bottom}}

             :x {:field :year
                 :type :ordinal
                 :title nil
                 :axis {:labels false
                        :ticks false}}

             :y {:field :value
                 :type :quantitative
                 :title nil
                 :axis {:grid true
                        :domain false}}

             :color {:field :year
                     :type :ordinal
                     :title nil
                     :scale {:range ["#1380A1" "#FAAB18"]}}}

  :config {:view {:stroke :transparent}
           :legend {:orient :top}}

  :title {:text "A Richer Heritage"
          :subtitle "Number of World Heritage Sites, 2004-2022"}})

;; ## An alternative bar chart
;;
;; The same chart as above, but with 'circle' marks instead of bars...
;;
;; First, expand the data:

(def heritage-sites-v2
  (reduce (fn [acc {:keys [country value year]}]
            (into acc
                  (for [offset (range value)]
                    {:country country
                     :year year
                     :offset (inc offset)})))
          []
          heritage-sites))

;; Then, the chart:

(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values heritage-sites-v2}
  :mark {:type "circle" :size 250}

  :height 280
  :width 100

  :encoding {:column {:field :country
                      :type :ordinal
                      :title nil
                      :header {:labelOrient :bottom}}

             :x {:field :year
                 :type :ordinal
                 :title nil
                 :axis {:labels false
                        :ticks false}}

             :y {:field :offset
                 :type :quantitative
                 :title nil
                 :axis {:grid true
                        :domain false}}

             :color {:field :year
                     :type :ordinal
                     :title nil
                     :scale {:range ["#a6639c" "#3a417c"]}}}


  :config {:view {:stroke :transparent}
           :legend {:orient :top}}

  :title {:text "A Richer Heritage"
          :subtitle "Number of World Heritage Sites, 2004-2022"}})


;; ## A donut chart
;;
;; Finally, I tried to make a pie/doughnut chart version of the data. This mostly involved lots of tweaking with vega lite properties
;; with no clear sense of where I was going! A lot of complexity and effort involved for something quite simple, and also something
;; which in the end is not very reusable. I don't know much about Vega Lite at all yet, so a better way to do this would be to automatically
;; calculate some of the angles, etc., instead of hard-coding the values like I've done.
;;
;; To make this chart I first made the 'circle' elements (half-circles):


(defn circle [year inner-radius outer-radius range-min range-max]
  {:data     {:values (->> heritage-sites-v2
                           (reduce (fn [acc m]
                                     (conj acc (assoc m :offset 1)))
                                   [])
                           (filter #(= year (:year %))))}
   :mark     {:type    :arc
              :radius  outer-radius
              :radius2 inner-radius
              :cornerRadius 10
              :padAngle 0.009}
   :encoding {:theta {:field :offset
                      :type  :quantitative
                      :scale {:type "linear" :rangeMax range-max :rangeMin range-min}}
              :color {:field :country
                      :type  :nominal
                      :scale {:range ["#efe6d5" "#9dbeb7" "#e73213"]}}}})

;; Then, the 'label' elements ("2004" and "2022")

(defn label [label radius theta angle]
  {:mark {:type :text
          :radius radius
          :theta theta
          :angle angle
          :color "gray"
          :fontSize 18}
   :encoding {:text {:value label}}})
  

;; Finally, putting it together as a chart:

(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data    {:values {}}                 ;; I'm not sure why, but this is needed for the 'labels' to appear...
  :height  320

  :layer [(circle 2004 60 100 -3.0708 0)
          (label "2004" 80 5.4 37)
          (circle 2022 110 150 -3.0708 0)
          (label "2022" 130 0.1 90)]

  :config {:legend {:title ""
                    :direction :horizontal
                    :orient :bottom}}

  :title {:text     "A Richer Heritage"
          :subtitle "Number of World Heritage Sites, 2004-2022"}})

;; The same chart, rotated:

(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data    {:values {}} ;; I'm not sure why, but this is needed for the 'labels' to appear...
  :height 320

  :layer [(circle 2004 60 100 1.55 -1.47)
          (label "2004" 80 5.65 64)
          (circle 2022 110 150 1.55 -1.47)
          (label "2022" 130 4.7 0)]

  :config {:legend {:title ""
                    :direction :horizontal
                    :orient :top}}

  :title {:text     "A Richer Heritage"
          :subtitle "Number of World Heritage Sites, 2004-2022"}})

