(ns game-clj.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :refer :all])
  (:import [com.badlogic.gdx.physics.box2d Filter]))

(def ^:const pixels-per-tile 32)
(def x0 (/ 100 pixels-per-tile))
(def y0 x0)

(defn create-part-body!
  [screen radius]
  (let [body (add-body! screen (body-def :static))]
    (let [part-fixture-def
          (->> (circle-shape :set-radius radius)
               (fixture-def :density 1 :friction 0 :restitution 1 :shape))]
      (do
        (set! (.groupIndex (.filter part-fixture-def)) (short -1))
        (body! body :create-fixture part-fixture-def))
      body)))

(defn create-ball-body!
  [screen radius]
  (let [body (add-body! screen (body-def :dynamic))]
    (let [ball-fixture-def
          (->> (circle-shape :set-radius radius
                             :set-position (vector-2 radius radius))
               (fixture-def :density 1 :friction 0 :restitution 1 :shape))]
      (do (set! (.groupIndex (.filter ball-fixture-def)) (short -1))
          (body! body :create-fixture ball-fixture-def))
     body)))

(defn create-rect-body!
  [screen width height]
  (let [body (add-body! screen (body-def :static))]
    (->> [0 0
          0 height
          width height
          width 0
          0 0]
         float-array
         (chain-shape :create-chain)
         (fixture-def :density 1 :shape)
         (body! body :create-fixture))
    body))

(defn create-part-entity!
  [screen]
  (let [part (texture "circle32.png")
        width (/ (texture! part :get-region-width) pixels-per-tile)
        height (/ (texture! part :get-region-height) pixels-per-tile)]
    (assoc part :body (create-part-body! screen (/ width 2))
                :width width
                :height height)))

(defn create-ball-entity!
  [screen]
  (let [ball (texture "head.png")
        width (/ (texture! ball :get-region-width) pixels-per-tile)
        height (/ (texture! ball :get-region-height) pixels-per-tile)]
    (assoc ball :body (create-ball-body! screen (/ width 2))
                :width width
                :height height)))

(defn create-rect-entity!
  [screen block width height]
  (assoc block
         :body (create-rect-body! screen width height)
         :width width :height height))

(defn move [entities x y]
  (when-let [entity (some #(if (:ball? %) %) entities)]
    (body! entity :set-linear-velocity x y)))


(defn interpolate-snake [part1 part2]
  (let [position (vector-2! (body! part2 :get-position) :lerp (body! part1 :get-position) 0.5)]
       (body-position! part2 (x position) (y position) 0))
  part2)


(defn follow-head [entities]
  (when-let [parts (filter #(if (:part? %) %) entities)]
    (when-let [head (some #(if (:ball? %) %) entities)]
      (reduce interpolate-snake head parts)))
  entities)

(defn update-screen!
  [screen entities]
  (follow-head entities))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (update! screen
                          :camera (orthographic)
                          :renderer (stage)
                          :world (box-2d 0 0))
          game-w (/ (game :width) pixels-per-tile)
          game-h (/ (game :height) pixels-per-tile)
          floor-h (/ 1 pixels-per-tile)
          block-w (/ 100 pixels-per-tile)
          block-h (/ 30 pixels-per-tile)
          block (shape :line
                       :set-color (color :yellow)
                       :rect 0 0 block-w block-h)
          block-cols (int (/ game-w block-w))
          block-rows (int (/ game-h 2 block-h))
          ball (doto (create-ball-entity! screen)
                 (body-position! x0 y0
                                 0)
                 (body! :set-linear-velocity 0 0))
          wall (doto {:body (create-rect-body! screen game-w game-h)}
                 (body-position! 0 0 0))
          floor (doto {:body (create-rect-body! screen game-w floor-h)}
                  (body-position! 0 0 0))]
      ; set the screen width in tiles
      (width! screen game-w)
      ; return the entities
      [
       (assoc wall :wall? true)
       (assoc floor :floor? true)
       (for [col (range block-cols)
             row (range block-rows)
             :let [x (* col block-w)
                   y (+ (* row block-h) (- game-h (* block-h block-rows)))]]
         (assoc (doto  (create-part-entity! screen)
                       (body-position! x y 0)
                       (body! :set-linear-velocity 0 0))
                :part? true))
       (for [col (range block-cols)
             row (range block-rows)
             :let [x (* col block-w)
                   y (+ (* row block-h) (- game-h (* block-h block-rows)))]]
         (assoc (doto (create-rect-entity! screen block block-w block-h)
                  (body-position! x y 0))
                :block? true))
       (assoc ball :ball? true)]))

  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
         (step! screen)
         (render! screen)
         (update-screen! screen)))


  :on-key-down
  (fn [screen entities]
    (let [x (cond (key-pressed? :a) -20 (key-pressed? :d) 20 :else 0)
          y (cond (key-pressed? :s) -20 (key-pressed? :w) 20 :else 0)]
     (move entities x y)))

  :on-begin-contact
  (fn [screen entities]
    (when-let [entity (first-entity screen entities)]
      (cond
        (:floor? entity) entities
        (:block? entity) (remove #(= entity %) entities)))))

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    (assoc (label "0" (color :white))
           :id :fps
           :x 5))

  :on-render
  (fn [screen entities]
    (->> (for [entity entities]
           (case (:id entity)
             :fps (doto entity (label! :set-text (str (game :fps))))
             entity))
         (render! screen)))

  :on-resize
  (fn [screen entities]
    (height! screen 300)))

(defgame game-clj-game
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))

;; (-> main-screen :entities deref)
;; (.groupIndex (.getFilterData (first (.getFixtureList (:body (first (filter :ball? @(-> main-screen :entities))))))))
