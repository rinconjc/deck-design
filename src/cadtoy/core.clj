(ns cadtoy.core
  (:gen-class)
  (:require [scad-clj
             [model :as m :refer [cube cylinder sphere union]]
             [scad :refer [write-scad]]]))

(def treated-pine-color #(m/color [(/ 153 255) (/ 113 255) (/ 87 255) 1] %))

(def decking-color #(m/color [(/ 140 255) (/ 70 255) (/ 23 255) 1] %))
(def concrete-color #(m/color [(/ 0xcc 255) (/ 0xcc 255) (/ 0xcc 255) 1] %))

(def depth 400)
(def board-size [8.6 1.9])
(def left-depth (+ depth 110))
(def right-depth (- depth 90))
(def width 632)
(def height (+ 100 9 1.9))
(def kitchen-lc (- width 132))
(def kitchen-rc (- kitchen-lc 330))

(defn balcony-floor []
  (->> (cube (- width kitchen-rc) 160 15 :center false)
       (m/translate [kitchen-rc (- depth 160) -15])
       concrete-color))

(defn post [x y z]
  (->> (cube 9 9 (+ height 10 (Math/abs z)) :center false)
       (m/translate [x y z])
       treated-pine-color))

(defn ledger [x y size]
  (->> (cube size 4.5 9 :center false)
       (m/translate [x (- y 4.5) 0])
       treated-pine-color))

(defn joist [x size]
  (->> (cube 4.5 size 9 :center false)
       (m/translate [x 0 0])
       treated-pine-color))

(def bearer
  (->> (cube 632 4.5 19 :center false)
       (m/translate [0 0 -10])
       treated-pine-color))

(defn hand-rail-x [x y size]
  (->> (cube size 4.5 4.5 :center false)
       (m/translate [x y (- height 4.5)])
       treated-pine-color))

(defn hand-rail-y [x y size]
  (->> (cube 4.5 size 4.5 :center false)
       (m/translate [x y (- height 4.5)])
       treated-pine-color))

(defn decking [x y len [w h]]
  (->>(cube len w h :center false)
      (m/translate [x (- y w) 9])
      (decking-color)))

(defn rhomboid [x y z dx]
  (m/polyhedron [[0 0 0] [x 0 0] [x y 0] [0 y 0]
                 [dx 0 z] [(+ dx x) 0 z] [(+ dx x) y z] [dx y z]]
                [[0 1 2 3] [4 5 1 0] [7 6 5 4] [5 6 2 1][6 7 3 2][7 4 0 3]]))

(defn stringer-x [x x1 y h w]
  (let [z 20
        len (Math/abs (- x1 x))
        za (Math/atan (/ h len))
        dx (* -1 (/ z (Math/tan za)))]
    (->> (rhomboid (Math/sqrt (+ (Math/pow len 2) (* h h))) 90 z dx)
         (m/rotate [0 (+ 45 (Math/toDegrees za)) 0])
         (m/translate [(+ x1 dx) y 0]))))

(defn decking-board []
  (let [[w h] [8.6 1.9]
        gap 0.4]
    (concat
     (for [y (range depth 0 (- 0 w gap))]
       (if (> y right-depth)
         (decking kitchen-rc y (- width kitchen-rc) [w h])
         (decking 0 y width [w h])))
     (for [y (range left-depth depth (- 0 w gap))]
       (decking kitchen-lc y 132 [w h])))))

(defn joists []
  (concat
   (for [x (range 4)] (joist (* 45 x) right-depth))
   (for [x (range 7)] (joist (+ kitchen-rc (* 45 x)) depth))
   (for [x (range 3)] (joist (+ kitchen-lc (* 45 x)) left-depth))))

(def deck
  (apply union
         (->> (m/polygon [[0 0] [width 0] [width left-depth] [(- width 132) left-depth]
                          [kitchen-lc depth] [kitchen-rc depth]
                          [kitchen-rc right-depth] [0 right-depth]])
              (m/color [0.9 0.9 0.9 0.3]))
         (balcony-floor)
         (post 0 0 -110)
         (post (- width 9) 0 -150)
         (post 210 0 -120)
         (post 420 0 -130)
         (post 0 (- right-depth 9) -120)
         (post kitchen-rc (- right-depth 9) -120)
         (post (- width 9) (- left-depth 90 9) -150)
         bearer
         (ledger 0 right-depth 170)
         (ledger kitchen-rc depth 330)
         (ledger kitchen-lc left-depth 132)
         (joist (- width 4.5) left-depth)
         (hand-rail-x 0 0 width)
         (hand-rail-x 0 (- right-depth 4.5) 170)
         (hand-rail-y 0 0 right-depth)
         (hand-rail-y (- width 4.5) 0 (- left-depth 90 9))
         (stringer-x 0 kitchen-rc right-depth 110 90)
         (joists)
         (decking-board)
         ))

(spit "post-demo.scad"
      (write-scad deck))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
