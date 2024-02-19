(ns tidy-tuesdays.make-html
  (:gen-class)
  (:require [scicloj.clay.v2.api :as clay]))

(defn make-fn [_]
  (clay/make!
   {:format [:quarto :html]
    :book {:title "Clojure Tidy Tuesdays"}
    :base-source-path "src/tidy_tuesdays/2024"
    :show nil
    :base-target-path "book"
    :source-path ["index.clj"
                  "week05/groundhog.clj"
                  "week06/heritage.clj"
                  "week07/valentines.clj"]
    :clean-up-target-dir true}))

(comment
  (make-fn nil)
  (clay/browse!))
