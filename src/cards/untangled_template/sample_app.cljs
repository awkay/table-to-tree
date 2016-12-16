(ns untangled-template.sample-app
  (:require-macros [untangled.client.cards :refer [untangled-app]])
  (:require [devcards.core :as rc :refer-macros [defcard defcard-doc]]
            [om.next :as om :refer-macros [defui]]
            [untangled.client.core :as uc]
            [untangled-template.ui.components :as comp]
            [om.dom :as dom]
            [devcards.core :as dc]))

(defui Item
  Object
  (render [this]
    (let [{:keys [ui/editing? item/value]} (om/props this)]
      (dom/li nil
        (if editing?
          (dom/div nil
            (dom/input #js {:value value})
            (dom/button #js {} "X"))
          (dom/div nil value))))))

(def ui-item (om/factory Item))

(defcard item-card
  "# Item states

  This card shows an item in regular and edit mode."
  (dom/div nil
    (ui-item {:ui/editing? false
              :item/value  "not editing"})
    (ui-item {:ui/editing? true
              :item/value  "editing"})))

