;; # Week 4: Groundhog Predictions
;;
(ns tidy-tuesdays.2024.week04.groundhog
  (:require
   [aerial.hanami.common :as hc]
   [aerial.hanami.templates :as ht]
   [scicloj.noj.v1.vis.hanami :as hanami]
   [tablecloth.api :as tc]
   [scicloj.kindly.v4.kind :as kind]
   [scicloj.noj.v1.stats :as stats]
   [scicloj.clay.v2.api :as clay]))


^:kindly/hide-code
(def groundhogs-source "data/2024/week04/groundhogs.csv")
^:kindly/hide-code
(def predictions-source "data/2024/week04/predictions.csv")

;; ## Context
;;
;; From Wikipedia:
;;
;; Groundhog Day is a tradition observed regionally in the United States and Canada
;; on February 2 of every year. It derives from the Pennsylvania Dutch superstition
;; that if a groundhog emerges from its burrow on this day and sees its shadow,
;; it will retreat to its den and winter will go on for six more weeks; if it does
;; not see its shadow, spring will arrive early. In 2024, an early spring was predicted.

;; ## Datasets
;; Defining the datasets...
(def ds-groundhogs
  (tc/dataset groundhogs-source {:key-fn keyword}))

(def ds-predictions
  (tc/dataset predictions-source {:key-fn keyword}))

;; ## Likihood of Spring
;; The 'predictions' dataset marks whether a groundhog sees their own shadow as
;; either "TRUE" or "FALSE" in the :shadow column. Let's write a function to aggregate those
;; values into an percentage likelihood that 'Spring' will come early (i.e., if the groundhog
;; doesn't see their shadow.)

(defn calculate-likelihood-spring [shadow-col]
  (if (empty? shadow-col) nil
    (let [valids (remove nil? shadow-col)
          total  (count valids)
          falses (count (filter #(= "FALSE" %) shadow-col))]
      (if (pos? falses)
        (float (/ falses total))
        0))))


(def consensus-predictions
  (-> ds-predictions
      (tc/group-by :year)
      (tc/aggregate {:spring-likelihood #(calculate-likelihood-spring (% :shadow))})
      (tc/map-columns :prediction [:spring-likelihood]
                      #(cond
                         (= % 0.5) "Tie"
                         (< % 0.5) "Winter"
                         :else     "Spring"))
      (tc/rename-columns {:$group-name :year})))

;; A first look at the likelihood of 'Spring' coming early over the time period:

(-> consensus-predictions
    (hanami/plot ht/line-chart
                 {:X :year
                  :XTYPE :temporal
                  :Y :spring-likelihood
                  :WIDTH 700
                  :SIZE 2}))

;; As we can see, since 1970, the likihood of spring coming early has increased. However, it has
;; also tended toward a 50-50 chance of the groundhog seeing their own shadow. In early times,
;; spring was very unlikely to come early, with only three instances where the groundhog didn't see
;; their own shadow from around 1986 to 1943 (57 years).
;;
;; Below are a few more attempts to visualise this phenomena:

(-> consensus-predictions
    (hanami/plot ht/point-chart
                 {:Y :prediction :YTYPE :nominal
                  :X :year :XTYPE :temporal
                  :MSIZE 100
                  :WIDTH 700
                  :HEIGHT 100
                  :COLOR {:field :spring-likelihood :type :quantitative}}))

;; Aggregate predictions since 1970:

(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values (-> consensus-predictions
                     (tc/drop-rows #(= "Tie" (% :prediction)))
                     (tc/drop-rows #(> 1970 (% :year)))
                     (tc/rows :as-maps))}
  :mark :rect
  :width 700
  :encoding {:y {:field :prediction :type :nominal}
             :x {:field :year :type :ordinal}
             :color {:field :spring-likelihood :type :quantitative
                     :scale {:scheme "blueorange"}}}
  :config {:axis {:grid true :tickBand :extent}}})


;; In the early years, there was only one groundhog. It seems that
;; as more and more groundhogs get added, there is more disagreement, tending
;; towards 50-50 predictions.
;;
;; Here is how the number of groundhogs have increased over time:

(-> ds-predictions
    (tc/group-by :year)
    (tc/aggregate {:groundhogs #(count (% :id))})
    (tc/rename-columns {:$group-name :year})
    (hanami/plot ht/line-chart
                 {:X :year :XTYPE :temporal
                  :Y :groundhogs
                  :MSIZE 5}))
    
;; ## Groundhogs
;; ### Locations

;; Let's see how the groundhogs are spread out geographically. The first map is the U.S.
;; groundhogs, and the second is the Canadian ones.

(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :width 500
  :height 300
  :layer [{:data {:values (slurp "resources/data/topo/states-10m.json")
                  :format {:type "topojson" :feature "states"}}
           :projection {:type "albersUsa"}
           :mark {:type :geoshape
                  :fill "lightgray"
                  :stroke "white"}}

          {:data {:values (-> ds-groundhogs
                              (tc/select-rows #(= "USA" (% :country)))
                              (tc/group-by :name)
                              (tc/aggregate {:lng #(first (% :longitude))
                                             :lat #(first (% :latitude))
                                             :type #(first (% :type))})
                              (tc/rename-columns {:$group-name :name})
                              (tc/rows :as-maps))}
           :projection {:type "albersUsa"}
           :mark :circle
           :encoding {:longitude {:field :lng}
                      :latitude {:field :lat}
                      :size {:value 80}
                      :color {:field :type :type :nominal}}}]})

(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :width 500
  :height 300
  :layer [{:data {:values (slurp "resources/data/topo/canadaprovtopo.json")
                  :format {:type "topojson" :feature "canadaprov"}}
           :projection {:type "albers"}
           :mark {:type :geoshape
                  :fill "lightgray"
                  :stroke "white"}}

          {:data {:values (-> ds-groundhogs
                              (tc/select-rows #(= "Canada" (% :country)))
                              (tc/group-by :name)
                              (tc/aggregate {:lng #(first (% :longitude))
                                             :lat #(first (% :latitude))
                                             :type #(first (% :type))})
                              (tc/rename-columns {:$group-name :name})
                              (tc/rows :as-maps))}
           :projection {:type "albers"}
           :mark :circle
           :encoding {:longitude {:field :lng}
                      :latitude {:field :lat}
                      :size {:value 80}
                      :color {:field :type :type :nominal}}}]})

;; ### Types
;; As we can see, there are lots of different kinds of 'groundhogs'. Let's look at the
;; most common types:


(-> ds-groundhogs
    (tc/group-by :type)
    (tc/aggregate {:count #(tc/row-count %)})
    (tc/rename-columns {:$group-name :type})
    (hanami/plot ht/bar-chart
                 {:Y :type :YTYPE :nominal :YSORT "-x"
                  :X :count
                  :HEIGHT 500}))

;; My personal favourite is the 'Atlantic Lobster' groundhog (Lucy the Lobster).

;; ### Spring Bias in 2023
;; Let's group the groundhogs by state, and see if there is a difference in likelihood
;; to predict a 'spring' by region for the year 2023.
;;
;; First, we have to join the two datasets:

(def joined-2023-regional-average
  (-> ds-predictions
      (tc/select-rows #(= 2023 (% :year)))
      (tc/inner-join ds-groundhogs :id)
      (tc/select-rows #(= "USA" (% :country)))
      (tc/group-by :region)
      (tc/aggregate {:regional-spring-likelihood
                     #(calculate-likelihood-spring (% :shadow))})
      (tc/rename-columns {:$group-name :state})))

;; Then, the map: (missing states are white)

(kind/vega
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :width 500
  :height 300
  :data {:values (slurp "resources/data/topo/states-10m.json")
         :format {:type "topojson" :feature "states"}}
  :transform [{:lookup "properties.name"
               :from {:data {:values (-> joined-2023-regional-average
                                         (tc/rows :as-maps))}
                      :fields [:regional-spring-likelihood]
                      :key :state}}]
  :mark "geoshape"
  :encoding {:color {:field :regional-spring-likelihood :type :quantitative
                     :scale {:scheme "blueorange"}}}
  :projection {:type "albersUsa"}})

;; As would be expected, the southern groundhogs were more likely to predict an early Spring
;; in 2023.


;; ### Groundhog 'Type' Bias?
;;
;; Are specific 'types' of groundhogs more likely to predict an early spring?
;;
;; First, let's write a function to aggregate the historical predictions given
;; a groundhog id.

(defn average-predictions-by-id [id]
  (-> ds-predictions
      (tc/select-rows #(= id (% :id)))
      :shadow
      calculate-likelihood-spring))

;; Here are all the individual groundhogs and their 'likelihood' of predicting
;; and early spring based on their historical predictions:

(-> ds-groundhogs
    (tc/select-columns [:id :name :region :predictions_count])
    (tc/map-columns :spring-likelihood [:id]
                    #(average-predictions-by-id %))
    (hanami/plot ht/bar-chart
                 {:X :spring-likelihood
                  :Y :name :YTYPE :nominal :YSORT "-x"
                  :COLOR {:field :predictions_count :type :quantitative}
                  :HEIGHT 1000}))

;; General Beauregard Lee, the groundhog from Georgia, seems quite optimistic
;; about early springs!
;;
;; Let's look at the likelihood of predicting Spring by type of groundhog:

(defn average-predictions-by-multiple-ids [ids]
  (->>
   (for [id ids]
     (-> ds-predictions (tc/select-rows #(= id (% :id))) :shadow))
   (reduce concat)
   calculate-likelihood-spring))

(-> ds-groundhogs
    (tc/select-columns [:id :type])
    (tc/group-by :type)
    (tc/aggregate {:agg-spring-likelihood #(average-predictions-by-multiple-ids (% :id))})
    (tc/order-by :agg-spring-likelihood :desc)
    (tc/rename-columns {:$group-name :type})
    (hanami/plot ht/bar-chart
                 {:Y :type :YTYPE :nominal :YSORT "-x"
                  :YTITLE "Groundhog Type"
                  :X :agg-spring-likelihood
                  :XTITLE "Likelihood of Predicting Spring %"
                  :HEIGHT 500}))

;; It's interesting to see that the actual groundhogs sit somewhere in the middle.
;;
;; ## Conclusion
;;
;; There are two possible conclusions here:
;;
;; (a) The original groundhog (Punxsutawney Phil) is a bit of a pessimist/recluse. For many years
;; he saw his shadow and predicted long Winters. As more and more groundhogs were added to the
;; tradition, a more balanced 'consensus' emerged, whereby there is now more chance of
;; an early Spring being predicted (c. 50%).
;;
;; (b) The tradition is somehow rooted in science, and the increased instances of early Springs are
;; reflective of global warming. Groundhogs are further strenghening the consensus of the scientific
;; community!
