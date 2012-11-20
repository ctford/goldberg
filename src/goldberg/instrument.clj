(ns goldberg.instrument
  (:use
    [overtone.live]))

(definst harpsichord [freq 440]
  (let [duration 1]
    (*
      (line:kr 1 1 duration FREE)
      (pluck (* (white-noise) (env-gen (perc 0.001 5) :action FREE))
             1
             1
             (/ 1 freq) (* duration 2) 0.25))))
