(ns tidy-tuesdays.make-html
  (:gen-class)
  (:require [scicloj.clay.v2.api :as clay]))

(defn make-fn [_]
  (clay/make!
   {:format [:quarto :html]
    :book {:title "Clojure Tidy Tuesdays"}
    :base-source-path "src/tidy_tuesdays/2024"
    :base-target-path "book"
    :source-path ["index.clj"
                  "week04/groundhog.clj"]
    :clean-up-target-dir true}))

(comment
  (make-fn)
  (clay/browse!))
