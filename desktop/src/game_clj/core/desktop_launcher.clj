(ns game-clj.core.desktop-launcher
  (:require [game-clj.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. game-clj-game "game-clj" 800 600)
  (Keyboard/enableRepeatEvents true))
