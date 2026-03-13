(ns terminal-object.session-zero.kick
  (:require [overtone.live :refer :all]))

(defsynth psy-kick
  [freq 60
   pitch-decay 0.05
   amp-decay 0.3
   click-level 0.7
   amp 1.0
   out-bus 0]
  (let [;; Pitch envelope (от высокого к низкому для "ударного" эффекта)
        pitch-env (env-gen (envelope [220 freq] [pitch-decay] :exponential))
        ;; Основной суб
        sub (sin-osc (+ pitch-env pitch-env))
        sub-env (env-gen (perc 0.001 amp-decay) :action FREE)
        ;; Click (транзиент)
        click (white-noise)
        click-filtered (lpf click 1500)
        click-env (env-gen (perc 0.0025 0.01))
        ;; Микс
        kick (+ (* sub sub-env)
                (* click-filtered click-env click-level))]
    (out:ar out-bus (pan2 (* amp kick) 0))))
