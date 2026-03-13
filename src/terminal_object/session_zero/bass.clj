(ns terminal-object.session-zero.bass
  (:require [overtone.live :refer :all]
            [terminal-object.core :refer [metro]]
            [terminal-object.session-zero.master :refer [master-bus master-group]]))


;; Глобальный LFO, синхронизированный с BPM
(defonce lfo-bus (control-bus))
(defsynth global-lfo [rate 8]
  (out:kr lfo-bus (sin-osc:kr rate)))

;; Запускаем один раз
(global-lfo (/ (metro :bpm) 60 0.5))


(defsynth roll-bass
  [freq 110
   filtfreq 800
   res 0.3
   amp 0.6
   dur 0.1
   out-bus 0]
  (let [osc (+ (saw freq) (sin-osc (/ freq 1.0)))
        filtered (rlpf osc filtfreq res)
        distorted (distort (* filtered 1.5))
        ;; Используем perc вместо adsr для фиксированной длительности
        env (env-gen (perc 0.01 dur) :action FREE)]
    (out:ar out-bus (* amp env (pan2 distorted 0)))))


(defsynth rolling-bass-mono
  [freq 110
   filtfreq 400
   res 0.3
   lfo-rate 8
   lfo-depth 200
   lfo-delta 1
   amp 0.6
   detune 0.01
   gate 0
   out-bus 0]
  (let [;; LFO работает непрерывно в рамках одного инстанса
        lfo (sin-osc lfo-rate)
        filt-mod (+ filtfreq (* lfo lfo-depth lfo-delta))
        dt (* freq detune)
        osc (+ (saw (+ freq dt)) (saw (- freq dt)) #_(square (* freq (/ 3 2))))
        filtered (hpf (rlpf osc filt-mod res) 105)
        ;; ADSR с коротким decay, но gate управляет амплитудой
        env (env-gen (adsr 0.005 0.1 0.8 0.05) gate :action NO-ACTION)]
    (out:ar out-bus (pan2 (* lfo amp env filtered) 0))))

(defonce !bass-synth (atom nil))
(defn start-bass-synth!
  []
  (kill @!bass-synth)
  (reset! !bass-synth (rolling-bass-mono [:head master-group]
                                         :freq 110
                                         :amp 0.45
                                         :lfo-rate (/ (metro :bpm) 60 64)
                                         :lfo-depth 150
                                         :detune 0.01
                                         :out-bus master-bus)))



(comment
  (start-bass-synth!)
  
  (stop)
  )
