(ns goldberg.variations.canone-alla-quarta
  (:use
    [leipzig.scale :exclude [flat sharp]]
    [leipzig.melody]
    [leipzig.canon]
    [leipzig.live]
    [goldberg.instrument]
    [overtone.live :only [midi->hz now at ctl]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstractions                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run [[from & tos]]
  (if-let [to (first tos)]
    (let [up-or-down (if (<= from to)
                       (range from to)
                       (reverse (range (inc to) (inc from))))]
      (concat up-or-down (run tos)))
    [from]))

(def repeats (partial mapcat #(apply repeat %)))
(def runs (partial mapcat run))
(def triples (partial mapcat #(repeat 3 %)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part I                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def melody1 
  (let [call
        (phrase
          (repeats [[2 1/4] [1 1/2] [14 1/4] [1 3/2]]) 
          (runs [[0 -1 3 0] [4] [1 8]])) 
        response
        (phrase
          (repeats [[10 1/4] [1 1/2] [2 1/4] [1 9/4]]) 
          (runs [[7 -1 0] [0 -3]])) 
        development
        (phrase
          (repeats [[1 1] [11 1/4] [1 1/2] [1 1] [1 3/4]
                    [11 1/4] [1 13/4]])
          (runs [[4] [2 -3] [-1 -2] [0] [3 5] [1] [1 2]
                 [-1 1 -1] [5 0]]))
        interlude 
        (phrase (repeats [[15 1/4] [1 10/4]]) 
                (runs [[-1 4] [6 -3]]))
        buildup 
        (phrase
          (repeats [[1 3/4] [7 1/4] [1 1/2] [2 1/4] [1 5/4] [11 1/4] [1 6/4] [4 1/2]])
          (runs [[3 1 7] [0 -1 0] [2 -2 0 -1] [1 -2] [4 1]])) 
        finale
        (phrase
          (repeats [[1 1/2] [1 6/4] [1 1/2] [2 1/4] [1 1] [3 1/4] [1 1/2] [1 1/4] [1 1]])             
          (runs [[6] [0 -2] [1 -2 -1] [4 3 4]]))]
    (->> (after 1/2 call)
         (then response)
         (then development)
         (then interlude)
         (then buildup)
         (then finale)
         (where :part (is :dux)))))

(def bass1
  (let [crotchets-a
        (phrase (repeat 9 1)
                (triples (run [-7 -9]))) 
        twiddle 
        (phrase (repeats [[1 1/4] [1 5/4] [2 1/4] [2 1/2]])
                (runs [[-10] [-17] [-11 -13] [-11]])) 
        crotchets-b
        (phrase
          (repeat 9 1)
          (triples (run [-12 -10]))) 
        elaboration
        (phrase
          (repeats [[1 3/4] [9 1/4] [1 1/2] [1 1] [2 1/4] [3 1/2] [1 1]])
          (runs [[-7] [-12] [-9 -11] [-9 -13 -12] [-14] [-7 -8 -7] [-9 -8] [-5]])) 
        busy 
        (phrase
          (repeats [[2 1/4] [2 1/2] [4 1/4] [4 1/2] [4 1/4] [3 1/2] [1 7/4]])
          (runs [[-12 -10] [-12] [-9 -7 -9 -8 -11 -9 -11] [-9] [-11] [-13]])) 
        finale 
        (phrase
          (repeats [[7 1/4] [1 1/2] [1 3/4] [23 1/4] [2 1/2] [1 1]])
          (runs [[-10 -6 -8 -7] [-14] [-9 -6] [-8 -10] [-5] [-12] [-9 -11] [-13]
                 [-10] [-7 -6] [-9] [-11] [-13] [-10 -9 -11 -10] [-13] [-17]]))] 
    (->> crotchets-a (then twiddle) (then crotchets-b) (then elaboration) (then busy) (then finale)
         (where :part (is :bass)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part II                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def melody2
  (let [theme
        (phrase (repeats [[2 1/4] [1 1/2] [6 1/4] [1 5/4] [5 1/4] [1 1/2] [1 5/8] [3 1/8] [1 1/2] [1 1]])
                (runs [[-3 -2 -6 -3] [-10 -9] [-11 -9 -10] [-8 -9 -8 -10 -9] [-4]]))
        response
        (phrase (repeats [[1 1/2] [12 1/4]])
                (runs [[-9 -10 -9 -11 -2]]))
        complicated 
        (phrase (repeats [[1 7/2] [2 1/4] [3 1/2] [2 1/4] [1 2]])
                (runs [[-2] [-2 -3 -2] [-2] [-2 -4 -3]]))
        thence 
        (phrase (repeats [[1 5/4] [11 1/4] [1 13/4]])
                (runs [[-1 -3] [-1 -4] [-4 -8 -7]]))
        blah 
        (phrase (repeats [[11 1/4] [1 7/2] [3 1/2] [4 1/4] [4 1/2] [1 3/4] [1 1/4]])
                (runs [[1 -1 0 -3 -2 -5 -4] [0 -3] [-5 -4] [-6 -4 -8]])) 
        finale 
        (phrase (repeats [[1 5/4] [11 1/4] [1 1/2] [1 3/4] [1 1/4] [1 1/2] [1 3]])
                (runs [[-7] [-5 -12] [-10] [-7] [-5] [-3] [-7 -6] [-8 -7]]))] 
    (->> (after 1/2 theme) (then response) (then complicated) (then thence) (then blah) (then finale) (where :part (is :dux)))))

(def bass2
  (let [intro
        (phrase (repeats [[3 1] [5 1/2] [2 1/4] [6 1/2] [1 7/2]])
                (runs [[-10] [-10 -12 -11 -14 -11 -12 -11] [-9] [-13] [-11 -12]])) 
        development 
        (phrase (repeats [[5 1/2] [24 1/4]])
                (runs [[-9 -3 -5 -4 -7 -6] [-8 -4 -6 -5] [-8] [-10] [-8] [-12] [-10 -12]]))
        up-n-down 
        (phrase (repeats [[8 1/4] [3 1/2] [1 3/4] [7 1/4] [1 1/2] [1 3/4] [7 1/4] [1 1/2] [1 3/4]])
                (runs [[-9] [-11 -14] [-12] [-9 -10 -9 -11] [-4] [-9 -11 -10 -13 -12] [-5] [-10 -12 -11 -14 -13] [-6]]))
        down 
        (phrase (repeats [[27 1/4] [1 5/4] [3 1/4] [1 3/2]])
                (runs [[-5 -7 -6 -9 -8 -11 -10 -13 -12 -15 -14] [-6 -8 -7 -10 -9 -11] [-9 -10]])) 
        finale 
        (phrase (repeats [[1 2] [3 1/2] [1 5/4] [3 1/4] [1 1]])
                (runs [[-13] [-9] [-11 -10] [-14] [-12] [-10] [-8 -7]]))] 
    (->> intro (then development) (then up-n-down) (then down) (then finale)
         (where :part (is :bass)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canon                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn canone-alla-quarta [f notes]
  (canon
    (comp (interval -3)
          mirror
          (simple 3)
          (partial where :part (is :comes))
          f)
    notes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accidentals                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sharp (partial + 1/2))
(def flat (partial + -1/2))

(defn with-accidentals [accidentals notes]
  (map
    (fn [{p :pitch t :time :as note}]
      (if (accidentals [t p])
        (update-in note [:pitch] (accidentals [t p]))
        note))
    notes))

(def accidentals1 
  (let [leader
        {[(+ 3 3/4) 3] sharp, [(+ 7 1/2) 3] sharp, [14 -1] flat, [(+ 25 1/4) 3] sharp,
         [(+ 30 1/2) 3] sharp, [40 3] sharp, [(+ 46 3/4) 3] sharp}
        follower
        {[(+ 27 3/4) -4] sharp, [30 -4] sharp, [(+ 34 1/2) -4] sharp, [(+ 38 1/2) -4] sharp,
         [(+ 40 1/4) -4] sharp, [44 -4] sharp, [(+ 47 1/4) -4] sharp}
        bass
        {[8 -9] sharp, [(+ 28 3/4) -11] sharp, [33 -11] sharp, [43 -11] sharp,
         [(+ 45 3/4) -11] sharp}]
    (merge bass leader follower)))

(def accidentals2 
  (let [leader
        {[(+ 5 1/2) -8] flat, [(+ 6 5/8) -8] flat, [(+ 6 7/8) -10] sharp, [9 -10] sharp, [(+ 9 3/4) -11] sharp
         [(+ 11 1/2) -4] sharp, [(+ 11 3/4) -3] sharp
         [(+ 15 3/4) -3] sharp, [(+ 17 1/2) -3] flat, [(+ 17 3/4) -4] sharp, [18 -3] sharp
         [(+ 21 1/2) -3] sharp, [(+ 22 1/4) -3] flat, [(+ 22 1/2) -4] sharp, [(+ 22 3/4) -4] flat}
        follower
        {[(+ 9 1/2) 5] sharp, [(+ 11 1/2) -4] sharp, [(+ 11 3/4) -3] sharp, [(+ 25 1/2) 0] sharp,
         [(+ 27 1/2) 0] sharp, [(+ 28 1/4) -1] flat, [31 -2] flat}
        bass 
        {[(+ 19 1/2) -10] sharp, [(+ 20 1/2) -11] sharp, [(+ 22 3/4) -10] sharp
         [(+ 23 1/2) -10] flat, [(+ 29 3/4) -14] sharp, [(+ 33 3/4) -11] sharp, [(+ 34 1/4) -11] flat,
         [(+ 37 3/4) -10] sharp}]
    (merge leader follower bass)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arrangement                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod play-note :default [{midi :pitch}] (-> midi midi->hz harpsichord))

; Warning: Using the sampled-piano will download and cache 200MB of samples
(comment
  (use 'overtone.inst.sampled-piano) 
  (defmethod play-note :default [{midi :pitch, start :time, duration :duration}]
    (let [id (sampled-piano midi)]
      (at (+ start duration) (ctl id :gate 0))))
) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Play                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def piece 
  (->>
    (->> melody1
            (canone-alla-quarta #(drop-last 6 %)) 
            (with bass1)
            (with-accidentals accidentals1)
            (times 2)) 
       (then
         (->> melody2
                  (canone-alla-quarta #(drop-last 4 %))
                  (with bass2)
                  (with-accidentals accidentals2))) 
       (where :pitch (comp G major))
       (where :time (bpm 90)) 
       (where :duration (bpm 90)))) 

(defn demo
  [start finish melody]
  (->> melody
       (filter #(-> % :time (>= start)))
       (filter #(-> % :time (< finish)))
       (where :time (from (- start)))
       (where :pitch (comp G major))
       (where :time (bpm 80))
       (where :duration (bpm 80))
       play
       ))

(comment
  (play piece)
)
