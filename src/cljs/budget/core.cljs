(ns budget.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.events :as events])
  (:import [goog.net XhrIo]
           goog.net.EventType
           [goog.events EventType]))

(enable-console-print!)

(def ^:private meths
  {:get "GET"
   :put "PUT"
   :post "POST"
   :delete "DELETE"})

(defn json-xhr [{:keys [method url data on-complete]}]
  (let [xhr (XhrIo.)]
    (events/listen xhr goog.net.EventType.COMPLETE
      (fn [e]
        (on-complete (.parse js/JSON (.getResponseText xhr)))))
    (. xhr
       ;; TODO adjust to json
      (send url (meths method) (when data (pr-str data))
        #js {"Content-Type" "application/edn"}))))


;; TODO get to work outside of om/next
;; TODO move outer hull to om/next
;; TODO move breadscrumbs to om/next

(defn attr [n attributes]
  (reduce (fn [result [key value]]
            (condp = key
              :text (.text result value)
              :style (do (doseq [[key value] value]
                           (.style result (name key) value))
                         result)
              ;; default:
              (let [key (name key)]
                (if (.startsWith key "on-")
                  (let [event (.substring key 3)]
                    (.on result event value))
                  (.attr result (name key) value)))))
          n
          attributes))

(defn append
  ([n tag]
   (append n tag {}))
  ([n tag attributes]
   (attr (.append n tag) attributes)))

(defn translate [tx ty]
  (str "translate(" (int tx) "," (int ty) ")"))

(def PI (aget js/Math "PI"))

;; TODO parameters to component
;; Dimensions of sunburst.
(def width 750)
(def height 600)
(def radius (/ (min width height) 2))

;; TODO parameters to component
;; Breadcrumb dimensions: width, height, spacing, width of tip/tail.
(def bc-dimensions
  {:w 75
   :h 30
   :s 3
   :t 10})

;; Mapping of step names to colors.
(def colors
  {:home "#5687d1"
   :product "#7b615c"
   :search "#de783b"
   :account "#6ab975"
   :other "#a173d1"
   :end "#bbbbbb"})

(def total-size (atom 0)) ;; TODO put into om/next

(def vis
  (-> js/d3
      (.select "#chart")
      (append "svg:svg" {:width width :height height})
      (append "svg:g"   {:id "container" :transform (translate (/ width 2) (/ height 2))})))

(def part
  (-> js/d3
      .-layout
      .partition
      (.size (clj->js [(* 2 PI)
                       (* radius radius)]))
      (.value (fn [d] (.-size d)))))

(def arc
  (-> js/d3
      .-svg 
      .arc
      (.startAngle (fn [d] (.-x d)))
      (.endAngle (fn [d] (.-x d)))
      (.innerRadius (fn [d] (.sqrt js/Math (.-y d))))
      (.outerRadius (fn [d] (.sqrt js/Math (+ (.-y d) (.-dy d)))))))

;; Given a node in a partition layout, return a vector of all of its ancestor
;; nodes, highest first, but excluding the root.
(defn get-ancestors [node]
  (->> (iterate #(.-parent %)
                node)
       (take-while identity)
       reverse
       (into [])))

(defn initialize-breadcrumb-trail []
  ;; Add the svg area.
  (let [trail (-> js/d3
                  (.select "#sequence")
                  (append "svg:svg" {:width width :height height :id "trail"}))]
    ;; Add the label at the end, for the percentage
    (-> trail
        (append "svg:text" {:id "endLabel"
                            :style {:fill "#000"}}))))

;; Generate a string that describes the points of a breadcrumb polygon.
(defn breadcrumb-points [d i]
  (let [parts ["0,0"
               (str (:w bc-dimensions) ",0")
               (str (+ (:w bc-dimensions) (:t bc-dimensions)) "," (int (/ (:h bc-dimensions) 2)))
               (str (:w bc-dimensions) "," (:h bc-dimensions))
               (str "0," (:h bc-dimensions))]
        ;; Leftmost breadcrumb; don't include 6th vertex.
        parts (if (> i 0)
                (str (:t bc-dimensions) "," (int (/ (:h bc-dimensions) 2)))
                parts)]
    (apply str (interpose " " parts))))

;; Update the breadcrumb trail to show the current sequence and percentage.
(defn update-breadcrumbs [node-array percentage-string]
  (let [;; Data join; key function combines name and depth (= position in sequence).
        g (-> js/d3
              (.select "#trail")
              (.selectAll "g")
              (.data node-array (fn [d] (str (.-name d) (.-depth d)))))
        ;; Add breadcrumb and label for entering nodes.
        entering (-> g (.enter) (append "svg:g"))]
    
    (-> entering
        (append "svg:polygon" {:points breadcrumb-points
                               :style {:fill (fn [d] (aget colors (.-name d)))}}))
    
    (-> entering
        (append "svg:text" {:x (int (/ (+ (:w bc-dimensions) (:t bc-dimensions)) 2))
                            :y (int (/ (:h bc-dimensions) 2))
                            :dy "0.35em"
                            :text-anchor "middle"
                            :text (fn [d] (-.name d))}))
    
    ;; Set position for entering and updating nodes.
    (-> g
        (attr {:transform (fn [d i]
                            (translate (* i (+ (:w bc-dimensions) (:s bc-dimensions))) 0))}))

    ;; Remove exiting nodes.
    (-> g .exit .remove)

    ;; Now move and update the percentage at the end.
    (-> js/d3
        (.select "#trail")
        (.select "#endlabel")
        (attr {:x (* (+ (.-length node-array) 0.5)
                     (+ (:w bc-dimensions) (:s bc-dimensions)))
               :y (int (/ (:h bc-dimensions) 2))
               :dy "0.35em"
               :text-anchor "middle"
               :text percentage-string}))
    ;; Make the breadcrumb trail visible, if it's hidden.
    (-> js/d3
        (.select "#trail")
        (attr {:style {:visibility ""}}))))

(defn draw-legend []
  (let [w 75 ;; width of legend item
        h 30 ;; height of legend item
        s 3  ;; spacing of legend items
        r 3  ;; radius of rounded rect
        legend (-> js/d3
                   (.select "#legend")
                   (append "svg:svg" {:width w
                                      :height (* (-> js/d3
                                                     (.keys colors)
                                                     .-length)
                                                 (+ h s))}))
        g (-> legend
              (.selectAll "g")
              (.data (.data (.entries js/d3 colors) ))
              (.enter)
              (append "svg:g" {:transform (fn [d i]
                                            (translate 0 (+ h s)))}))]
    (-> g
        (append "svg:rect" {:rx r :ry r :width w :height h
                            :style {:fill (fn [d] (.-value d))}}))
    (-> g
        (append "svg:text" {:x (int (/ w 2))
                            :y (int (/ h 2))
                            :dy "0.35em"
                            :text-anchor "middle"
                            :text (fn [d] (.-key d))})))
)

(defn toggle-legend []
  (let [legend (-> js/d3 (.select "#legend"))
        visibility (-> legend (.style "visibility"))]
    (attr legend {:style {:visibility (if (= visibility "hidden") "" "hidden")}})))

;; Fade all but the current sequence, and show it in the breadcrumb trail.
(defn mouse-over [d]
  (let [percentage (-> d
                       -.value
                       (* 100)
                       (/ @total-size)
                       (.toPrecision 3))
        percentage-string (if (< percentage 0.1)
                            (str percentage "%")
                            "< 0.1%")]
    (-> js/d3
        (.select "#percentage")
        (attr {:text percentage-string}))
    (-> js/d3
        (.select "#explanation")
        (attr {:style {:visibility ""}}))
    (let [sequence-array (clj->js (get-ancestors d))]
      (update-breadcrumbs sequence-array percentage-string)
      
      ;; Fade all the segments.
      (-> js/d3
          (.selectAll "path")
          (:attr {:style {:opacity "0.3"}}))
      
      ;; Then highlight only those that are an ancestor of the current segment.
      (-> js/d3
          (.selectAll "path")
          (.filter (fn [node]
                     (>= (.indexOf sequence-array node) 0)))
          (attr {:style {:opacity 1}})))))

;; Restore everything to full opacity when moving off the visualization.
(defn mouse-leave [d]
  ;; Hide the breadcrumb trail
  (-> js/d3
      (.select "#trail")
      (attr {:style {:visiblity "hidden"}}))
  ;; Deactivate all segments during transition.
  (-> js/d3
      (.selectAll "path")
      (attr {:on-mouseover nil}))
  ;; Transition each segment to full opacity and then reactivate it.
  (-> js/d3
      (.selectAll "path")
      (.transition)
      (.duration 1000)
      (attr {:style {:opacity 1}})
      (.each "end" #(-> js/d3
                        (.select js/this)
                        (attr {:on-mouseover mouse-over}))))
  
  (-> js/d3
      (.select "#explanation")
      (attr {:stype {:visibility "hidden"}})))

(defn create-visualization [json]
  (initialize-breadcrumb-trail)
  ;;(drawLegend)
  (-> js/d3
      (.select "#togglelegend")
      (attr {:on-click toggle-legend}))
  (-> vis
      (append "svg:circle" {:r radius :style {:opacity 0}}))
  (let [nodes (-> part
                  (.nodes json)
                  (.filter (fn [d] (> (.dx d 0.005))))) ;; 0.005 radians = 0.29 degrees
        path (-> vis
                 (.data (clj->js [json]))
                 (.selectAll "path")
                 (.data nodes)
                 (.enter)
                 (append "svg:path" {:display (fn [d] (if (nil? (aget d "depth"))
                                                        nil
                                                        "none"))
                                     :d arc
                                     :fill-rule "evenodd"
                                     :style {:fill (fn [d] (or (aget colors (.-name d))
                                                               "#000"))
                                             :opacity 1}
                                     :on-mouseover mouse-over}))]
    ;; Add the mouseleave handler to the bounding circle.
    (-> js/d3
        (.select "#container")
        (attr {:on-mouseleave mouse-leave}))
    ;; Get total size of the tree = value of root node from partition.
    (reset! total-size (-> path .node .-__data__ .-value))))

(json-xhr {:method :get
           :url "generated/budget2016-depenses.json"
           :on-complete create-visualization})

(defonce app-state (atom {:text "Hello Chestnut!"}))

(defn root-component [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil (dom/h1 nil (:text app))))))

(om/root
 root-component
 app-state
 {:target (. js/document (getElementById "app"))})
