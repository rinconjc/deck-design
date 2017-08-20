(ns cadtoy.core
  (:gen-class)
  (:require [scad-clj
             [model :as m :refer [cube cylinder sphere union]]
             [scad :refer [write-scad]]]))

;; 17*16.05 + 17*5.15*4 + 17*6.15*3 = 272.85 + 350.2 + 313.65 = 936.38
;; 100 = 11*3 + 6.6*4 = 33 + 26.4 = 59.6
;; 138: 2x7.2 + 1x3.2 = 17.6
;; 110 = 3X5.4x3 + 1x5.4 = 10x5.4
;; 6.6 = 14x5.4
;; bunnings : 316.44 + 286 + 241.92 = 844.36
;; blackbutt : 75x6.98 = 524

(def treated-pine-color #(m/color [(/ 153 255) (/ 113 255) (/ 87 255) 1] %))

(def decking-color #(m/color [(/ 140 255) (/ 70 255) (/ 23 255) 1] %))
(def concrete-color #(m/color [(/ 0xcc 255) (/ 0xcc 255) (/ 0xcc 255) 1] %))
(def wall-color #(m/color [(/ 0xdd 255) (/ 0xcc 255) (/ 0xaf 255) 1] %))

(def paint-color #(m/color [(/ 122 255) (/ 88 255) (/ 78 255) 1] %))

(def left-width 132)
(def depth 400)
(def board-size [8.6 1.9])
(def left-depth (+ depth 110))
(def right-depth (- depth 90))
(def width 632)
(def height (+ 100 9 1.9))
(def kitchen-lc (- width left-width))
(def kitchen-rc (- kitchen-lc 330))
(def post-side 9)

(defn balcony-floor []
  (concrete-color
   (union
    (->> (cube (- width kitchen-rc) 160 15 :center false)
         (m/translate [kitchen-rc (- depth 160) -15]))
    (->> (cube left-width 110 15 :center false)
         (m/translate [kitchen-lc depth -15])))))

(defn post [x y z & {:keys [fence?] :or {fence? true}}]
  (->> (cube post-side post-side (+ (if fence? height 0) (Math/abs z)) :center false)
       (m/translate [x y z])
       paint-color))

(defn ledger [x y size]
  (->> (cube size 4.5 9 :center false)
       (m/translate [x (- y 4.5) 0])
       treated-pine-color))

(defn joist
  [x size & {:keys [h y z] :or {h 9 y 0 z 0}}]
  (->> (cube 4.5 size h :center false)
       (m/translate [x y z])
       treated-pine-color))

(defn bearer [& {:keys [x y z] :or {x 0 y 0 z -5}}]
  (->> (cube (+ width 9) 4.5 14 :center false)
       (m/translate [x y z])
       treated-pine-color))

(defn hand-rail-x [x y size]
  (->> (cube size 13.8 3 :center false)
       (m/translate [x (- y 2.4) (- height 3)])
       paint-color))

(defn hand-rail-y [x y size]
  (->> (cube 13.8 size 3 :center false)
       (m/translate [x y (- height 3)])
       paint-color))

(defn decking [x y len [w h]]
  (->>(cube (+ post-side len) w h :center false)
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
       (if (> y (- left-depth 90))
         (decking kitchen-lc y (+ 90 left-width) [w h])
         (decking kitchen-lc y left-width [w h]))))))

(defn joists []
  (apply union
         (joist (+ width 4.5) left-depth :h 14 :z -5)
         (joist (+ width 45) 90 :y (- left-depth 90))
         (joist (+ width 90) 90 :y (- left-depth 90))
         (for [x (range 4)] (joist (* 45 x) right-depth :h 9 :z 0))
         (for [x (range 7)] (joist (+ kitchen-rc (* 45 x)) depth :h 9 :z 0))
         (for [x (range 3)] (joist (+ kitchen-lc (* 45 x)) left-depth :h 9 :z 0))))

(defn stairs-south []
  (post width depth ))

(defn furniture []
  (let [chair (fn [] (m/difference
                      (cube 72 75 90 :center false)
                      (->> (cube 50 55 50 :center false)
                           (m/translate [30 10 50]))))]
    (union
     (->> (cube 184 72 90 :center false)
          (m/translate [220 60 10]))
     (->> (cube 150 83 63 :center false)
          (m/translate [230 150 10]))
     (m/translate [140 155 10] (chair))
     (->> (chair) (m/rotate [0 0 Math/PI])
          (m/translate [470 230 10])))))

(defn stairs [h & {:keys [step-height step-width width] :or {width 90}}]
  (let [nh (Math/ceil (/ h 19))
        wi (or step-width 24)
        hi (/ h nh)]
    (-> (for [i (range 1 (inc nh)) :let [y (* -1 i wi)
                                         z (* -1 i hi)]]
          (union (m/translate [0 y (+ z hi)] (cube width wi 2 :center false))
                 (m/translate [0 y z] (cube width 2 hi :center false))))
        (union (m/translate [0 (* -1 (inc nh) wi) (* -1 h)] (cube width wi 2 :center false)))
        decking-color)))

(def old-stairs
  (->> (stairs 96 :step-height 16 :step-width 28)
       (m/rotate [0 0 (* Math/PI 1.5)])
       (m/translate [kitchen-rc (+ 90 right-depth) 0])))

(def new-stairs
  (m/translate [width (- left-depth 90) -5] (stairs 150 :step-width 26)))

(defn wall [x y z h l]
  (->> (cube l 20 h :center false)
       (m/translate [x y z])))

(def walls
  (->> (union
        (wall 0 depth -110 320 kitchen-lc)
        (wall kitchen-lc left-depth -150 360 (+ 100 left-width))
        (->> (wall 0 0 -150 360 130)
             (m/rotate [0 0 (* Math/PI 0.5)] )
             (m/translate [kitchen-lc depth 0])))
       wall-color))

(defn slate-x [x y z len height]
  (->> (cube len 1.8 height :center false)
       (m/translate [x y (+ z 9)])
       paint-color))

(defn slate-y [x y z len height]
  (->> (cube 1.8 len height :center false)
       (m/translate [x y (+ z 9)])
       paint-color))

(def slates
  (union
   (for [z [6 42.5 82]]
     (slate-x 0 0 z 632 11))

   (for [z [21 32 59 70]]
     (slate-x 0 0 z 632 6.6))

   (for [z [6 42.5 82]]
     (slate-x 0 310 z 170 11))

   (for [z [21 32 59 70]]
     (slate-x 0 310 z 170 6.6))

   (for [z [6 42.5 82]]
     (slate-y 0 0 z 310 11))

   (for [z [21 32 59 70]]
     (slate-y 0 0 z 310 6.6))

   (for [z [6 42.5 82]]
     (slate-y 642 0 z 420 11))

   (for [z [21 32 59 70]]
     (slate-y 642 0 z 420 6.6))
   (for [z [6 42.5 82]]
     (slate-y 720 410 z 100 11))
   (for [z [21 32 59 70]]
     (slate-y 720 410 z 100 6.6))))

(def deck
  (union
   walls
   old-stairs
   new-stairs
   (balcony-floor)
   (post 0 0 -110)
   (post width 0 -150)
   (post 210 0 -120)
   (post 420 0 -130)
   (post 0 (- right-depth 9) -120)
   (post kitchen-rc (- right-depth 9) -120)
   (post width (- left-depth 90 9) -150)
   (post (+ 90 width) (- left-depth 90 9) -150) ;; south-stair-south-post
   (post (+ 90 width) (- left-depth 9) -150)
   ;;extra posts
   (post 0 130 -120 :fence? false)
   (post 210 130 -120 :fence? false)
   (post 420 130 -120 :fence? false)
   (post 632 130 -120 :fence? false)

   (bearer :z -14)
   (bearer :y 130 :z -14)
   ;;(bearer :y 122.5 :z -14)
   (ledger 0 right-depth 170)
   (ledger kitchen-rc depth 330)
   (ledger kitchen-lc left-depth (+ left-width 90))
   (ledger width (- left-depth 90) 90)
   (hand-rail-x 0 0 width)
   (hand-rail-x 0 (- right-depth 10) 180)
   (hand-rail-y 0 0 right-depth)
   (hand-rail-y (- width 4.5) 0 (- left-depth 90 9))
   (hand-rail-y (+ width 90) (- left-depth 90) 90)
   ;; (stringer-x 0 kitchen-rc right-depth 110 90)
   ;;(furniture)
   (joists)
   (decking-board)

   slates
   ))

(spit "post-demo.scad"
      (write-scad deck))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
