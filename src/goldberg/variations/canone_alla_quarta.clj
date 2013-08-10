(ns goldberg.variations.canone-alla-quarta
  (:use
    [leipzig.scale]
    [leipzig.melody]
    [leipzig.canon]
    [leipzig.live]
    [goldberg.instrument]
    [overtone.live :only [midi->hz now stop at ctl]]))

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

(defn accumulate [series] (reductions + 0 series)) 
(def repeats (partial mapcat #(apply repeat %)))
(def runs (partial mapcat run))
(def triples (partial mapcat #(repeat 3 %)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Melody                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn note [timing pitch] {:time timing :pitch pitch}) 
(defn sum-n [series n] (reduce + (take n series)))
(defn accumulate [series] (map (partial sum-n series) (range (count series))))
(def repeats (partial mapcat #(apply repeat %)))
(def runs (partial mapcat run))

(def melody1 
  (let [call
         [(repeats [[2 1/4] [1 1/2] [14 1/4] [1 3/2]])
          (runs [[0 -1 3 0] [4] [1 8]])]
        response
         [(repeats [[10 1/4] [1 1/2] [2 1/4] [1 9/4]])
          (runs [[7 -1 0] [0 -3]])]
        development
         [(repeats [[1 3/4] [12 1/4] [1 1/2] [1 1] [1 1/2]
                   [12 1/4] [1 3]])
           (runs [[4] [4] [2 -3] [-1 -2] [0] [3 5] [1] [1] [1 2]
                 [-1 1 -1] [5 0]])] 
        [durations pitches]
                  (map concat call response development)
                times (map (from 1/2) (accumulate durations))]
              (map note times pitches)))

(def bass1
  (let [triples (partial mapcat #(repeat 3 %))]
    (map note
      (accumulate (repeats [[21 1] [13 1/4]]))
      (concat
        (triples (runs [[-7 -10] [-12 -10]]))
        (runs [[5 0] [6 0]])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canone alla quarta - Johann Sebastian Bach   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn canone-alla-quarta [notes]
  (canon
    (comp (interval -3) mirror (simple 3))
    notes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arrangement                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod play-note :default [{midi :pitch}] (-> midi midi->hz harpsichord))

; Warning: Using the sampled-piano will download and cache 200MB of samples
(use 'overtone.inst.sampled-piano)
(defmethod play-note :default [{midi :pitch, start :time, duration :duration}]
  (let [id (sampled-piano midi)]
    (at (+ start duration) (ctl id :gate 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Play                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (->> melody1
       canone-alla-quarta
       (with bass1) 
       (where :pitch (comp G major))
       (where :time (bpm 90)) 
       (where :duration (is 500)) 
       play)

)
