(ns replforce.constructing-music (:use overtone.live))

;; soundwaves

(demo 4 (sin-osc 440))
(demo 4 (saw 440))
(demo 4 (square 440))

;; overtones

(definst chime [freq 440 dur 1]
  (let [fund freq
        over1 (* 2 fund)
        over2 (* 3.1 fund)
        over3 (* 6.1 fund)]
    (* (+ (* (sin-osc fund) 1)
          (* (sin-osc over1) 0.7)
          (* (sin-osc over2) 0.2)
          (* (sin-osc over3) 0.1))
       ;;(env-gen (lin 0 0.1 dur))
       )))

(demo (chime 440))

(definst bell [frequency 440 duration 10
               h0 1 h1 0.6 h2 0.4 h3 0.25 h4 0.2 h5 0.15]
  (let [harmonic-series [ 1  2  3  4  5  6]
                                        ;[ 1  2  3  4.2 5.4 6.8]
        proportions     [h0 h1 h2 h3 h4 h5]
        component
        (fn [harmonic proportion]
          (* 1/2
             proportion
             (env-gen (perc 0.01 (* proportion duration)))
             (sin-osc (* harmonic frequency))))
        whole
        (mix (map component harmonic-series proportions))]
    (detect-silence whole :action FREE)
    whole))

(do (bell 300) (bell 200) (bell 400))
(stop)

(definst kick [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(def metro (metronome 128))

(definst chime [freq 440 dur 1]
  (let [fund freq
        over1 (* 2 fund)
        over2 (* 3 fund)
        over3 (* 6 fund)]
    (pan2 (* (+ (* (sin-osc fund) 1)
                (* (sin-osc over1) 0.7)
                (* (sin-osc over2) 0.2)
                (* (sin-osc over3) 0.1))
             (env-gen (lin 0 0.1 dur))))))


(defn player [beat]
  (at (metro beat) (kick))
  ;;(at (metro (+ 0 beat))  (chime 222))
  (at (metro (+ 0.5 beat)) (c-hat))
  (apply-by (metro (inc beat)) #'player (inc beat) []))

(demo 5 (sin-osc))

(player (metro))
