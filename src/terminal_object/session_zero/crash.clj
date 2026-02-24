(ns terminal-object.session-zero.crash
  (:require [overtone.live :refer :all]
            [hyperfiddle.rcf :refer [tests]]))

(definst soft-massive-crash [amp 0.4 out-bus 0 pan 0 attack 0.05 release 5.0 room 0.9 mix 0.7]
  (let [wn (white-noise:kr)
        lfo (* 0.7 (square (+ 0.3 (* 50 wn))))
        ;; Soft envelope with a long decay
        env (env-gen (perc attack release) :action FREE)
        
        ;; Pink noise is "warmer/softer" than white noise
        src (pink-noise)
        
        ;; Filter to isolate the high shimmer
        shimmer (hpf src 3000)
        
        ;; Add a bit of resonance at 10kHz for a metallic edge
        filtered (resonz shimmer 10000 0.2)
        
        sig (* amp lfo env (+ shimmer filtered))
        
        ;; The "Massive" effect
        verb (free-verb sig :mix mix :room room :damp 0.2)]
    
    (out out-bus (pan2 verb pan))))

(comment

  (stop)
)

(tests
 "soft-massive-crash is instrument"
 (str (type soft-massive-crash)) := "class overtone.studio.inst.Inst")
