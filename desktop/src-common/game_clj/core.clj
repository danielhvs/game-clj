(ns game-clj.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :refer :all])
  (:import [com.badlogic.gdx.physics.box2d Filter]))

(def ^:const pixels-per-tile 32)

;; https://www.bountysource.com/teams/play-clj/issues
(defn collision-group [group]
  (let [collision-filter (new com.badlogic.gdx.physics.box2d.Filter)]
    (set! (.groupIndex collision-filter) (short group))
    collision-filter))

(defn create-part-body!
  [screen radius]
  (let [body (add-body! screen (body-def :static))]
    (->> (circle-shape :set-radius radius)
         (fixture-def :density 1 :friction 0 :restitution 1 :shape)
         (body! body :create-fixture))
    body))

(defn create-ball-body!
  [screen radius]
  (let [body (add-body! screen (body-def :dynamic))]
    (->> (circle-shape :set-radius radius)
         (fixture-def :density 1 :friction 0 :restitution 1 :shape)
         (body! body :create-fixture))
    body))

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
  (let [radius (/ 32 pixels-per-tile 2)
        part (shape :line
                    :set-color (color :blue)
                    :circle 0 0 radius 20)]
    (assoc part :body (create-part-body! screen radius))))

(defn create-ball-entity!
  [screen]
  (let [radius (/ 32 pixels-per-tile 2)
        ball (shape :filled
                    :set-color (color :blue)
                    :circle 0 0 radius 20)]
    (assoc ball :body (create-ball-body! screen radius))))

(defn create-rect-entity!
  [screen block width height]
  (assoc block
         :body (create-rect-body! screen width height)
         :width width :height height))

(defn move [entities x y]
  (when-let [entity (some #(if (:paddle? %) %) entities)]
    (body! entity :set-linear-velocity x y)))


(defn follow-head [entities]
  (when-let [part (some #(if (:part? %) %) entities)]
    (when-let [head (some #(if (:paddle? %) %) entities)]
      (let [position (body! head :get-position) ]
        (body-position! part (- (x position) 0.3) (- (y position) 0.3) 0)))))

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
                       :set-color (color :green)
                       :rect 0 0 block-w block-h)
          block-cols (int (/ game-w block-w))
          block-rows (int (/ game-h 2 block-h))
          part (doto (create-part-entity! screen)
                 (body-position! (/ 100 pixels-per-tile)
                                 (/ 100 pixels-per-tile)
                                 0)
                 (body! :set-linear-velocity 0 0))
          ball (doto (create-ball-entity! screen)
                 (body-position! (/ 100 pixels-per-tile)
                                 (/ 100 pixels-per-tile)
                                 0)
                 (body! :set-linear-velocity 0 0))
          paddle (doto (create-rect-entity! screen block block-w block-h)
                   (body-position! 0 0 0))
          wall (doto {:body (create-rect-body! screen game-w game-h)}
                 (body-position! 0 0 0))
          floor (doto {:body (create-rect-body! screen game-w floor-h)}
                  (body-position! 0 0 0))]
      ; set the screen width in tiles
      (width! screen game-w)
      ; return the entities
      [(assoc ball :ball? true :paddle? true :parts part)
       (assoc part :part? true)
       (assoc wall :wall? true)
       (assoc floor :floor? true)
       (for [col (range block-cols)
             row (range block-rows)
             :let [x (* col block-w)
                   y (+ (* row block-h) (- game-h (* block-h block-rows)))]]
         (assoc (doto (create-rect-entity! screen block block-w block-h)
                  (body-position! x y 0))
                :block? true))]))

  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
         (step! screen)
         (render! screen)
         (update-screen! screen)
         ))

  :on-key-down
  (fn [screen entities]
    (let [x (cond (key-pressed? :a) -10 (key-pressed? :d) 10 :else 0)
          y (cond (key-pressed? :s) -10 (key-pressed? :w) 10 :else 0)]
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
    (assoc (label "0" (color :blue))
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
