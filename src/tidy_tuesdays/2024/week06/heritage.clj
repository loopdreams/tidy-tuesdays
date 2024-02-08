;; # Week 6 - World Heritage Sites
(ns tidy-tuesdays.2024.week06.heritage
  (:require
   [aerial.hanami.templates :as ht]
   [scicloj.noj.v1.vis.hanami :as hanami]
   [scicloj.kindly.v4.kind :as kind]))


;; ## Introduction
;; This week the challenge is to work with a small dataset and produce interesting visualisations.
;; This is a great exercise for me personally, because so far (in my work with clojure and data) I
;; have been more interested in processing and exploring the data, and I tent to be a bit 'lazy'
;; when it comes to details around the visualisations.
;;
;; To start with, here is the simple dataset we are working with:

(def heritage-sites
  [{:country "Norway" :2004 5 :2022 8}
   {:country "Denmark" :2004 4 :2022 10}
   {:country "Sweeden" :2004 13 :2022 15}])

(kind/table
 {:column-names [:country :2004 :2022]
  :row-maps heritage-sites})

;; ## Basic Bar Chart
;; Let's start with a basic bar chart, but I want to try make it a bit 'tidier' and include a nice
;; title/subtitle (using thing repo as a reference point - https://github.com/aezarebski/vegacookbook.
;;
;; First, I will restucture the data a bit (to make it fit only way I know how to do 'grouped' bar charts...)

(def heritage-sites-v2
  [{:country "Norway" :value 5 :year 2004}
   {:country "Norway" :value 8 :year 2022}
   {:country "Denmark" :value 4 :year 2004}
   {:country "Denmark" :value 10 :year 2022}
   {:country "Sweeden" :value 13 :year 2004}
   {:country "Sweeden" :value 15 :year 2022}])


(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values heritage-sites-v2}
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

;; ## Something more expirimental
;;
;; The same chart as above, but with 'circle' marks instead of bars...
;;
;; First, expand the data:

(def heritage-sites-v3
  (reduce (fn [acc {:keys [country value year]}]
            (into acc
                  (for [offset (range value)]
                    {:country country
                     :year year
                     :offset (inc offset)})))
          []
          heritage-sites-v2))

;; Then, the chart:

(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values heritage-sites-v3}
  :mark {:type "circle" :size 160}

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
                     :scale {:range ["#1380A1" "#FAAB18"]}}}

  :config {:view {:stroke :transparent}
           :legend {:orient :top}}

  :title {:text "A Richer Heritage"
          :subtitle "Number of World Heritage Sites, 2004-2022"}})


;; ## A donut chart...

(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values heritage-sites-v2}
  :height 380
  :encoding {:theta {:field :value
                     :type :quantitative
                     :stack true
                     :aggregate "sum"}}
  :layer [{:mark {:type :arc
                  :radius2 60
                  :radius 100}
           :transform [{:filter {:field :year
                                 :equal "2004"}}]
           :encoding {:color {:field :country
                              :type :nominal
                              :scale {:range "category"}}}}

          {:mark {:type :text
                  :radius 80
                  :color "white"}
           :transform [{:filter {:field :year
                                 :equal "2004"}}]
           :encoding {:text {:field :value
                             :type :quantitative
                             :aggregate :sum}
                      :detail {:field :country
                               :type :nominal}}}
          {:mark {:type :text
                  :radius 38
                  :color "black"
                  :fontSize 18
                  :font "Arial"}
           :encoding {:text {:value "2004"}}}

          {:mark {:type :text
                  :radius 170
                  :color "black"
                  :fontSize 18
                  :font "Arial"}
           :encoding {:text {:value "2022"}}}

          {:mark {:type :arc
                  :radius2 120
                  :radius 160}
           :transform [{:filter {:field :year
                                 :equal "2022"}}]
           :encoding {:color {:field :country
                              :type :nominal
                              :scale {:range "category"}}}}
          
          {:mark {:type :text
                  :radius 140
                  :color "white"}
           :transform [{:filter {:field :year
                                 :equal "2022"}}]
           :encoding {:text {:field :value
                             :type :quantitative
                             :aggregate :sum}
                      :detail {:field :country
                               :type :nominal}}}]

  :resolve {:scale {:theta "independant"}}
  
  :config {:view {:stroke nil}
           
           :legend {:orient :top}}

  :title {:text "A Richer Heritage"
          :subtitle "Number of World Heritage Sites, 2004-2022"}})
