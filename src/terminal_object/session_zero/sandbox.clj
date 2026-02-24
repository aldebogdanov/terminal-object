(ns terminal-object.session-zero.sandbox
  (:require [overtone.live :refer :all]
            [terminal-object.core :refer [note->hz setter]]))


;; The core four. Each has a radically different harmonic content.
(demo 3 (sin-osc 100))    ; pure tone, no harmonics — sub bass, test tones
(demo 3 (saw 100))        ; all harmonics — rich, buzzy, the workhorse
(demo 3 (pulse 100 0.5))  ; odd harmonics at 50% width — hollow, square
(demo 3 (pulse 100 0.1))  ; narrow pulse — nasal, thin, biting

;; White noise — all frequencies equally. Crucial for percussion.
(demo 3 (* 0.3 (white-noise)))


;; env-gen is how you give sound a life and death
(demo 2
      (pan2 (* (env-gen (envelope [0 1 0.6 0] [0.01 0.1 0.5]) :action FREE)
               (saw 100)) 0))


;; perc envelope — fast attack, variable decay. Your best friend.
(demo 1
      (pan2 (* (env-gen (perc 0.001 0.3) :action FREE)
               (sin-osc 60)) 0))

;; Low-pass filter: removes everything above cutoff
(demo 2
      (pan2 (rlpf (saw 80) 700 0.3) 0))


;; rq (reciprocal of Q): lower = more resonant/screamy
;; Try 0.1 for aggressive resonance, 0.5 for gentle
(demo 2
      (pan2 (rlpf (saw 80) 1200 0.01)))



;; High-pass: removes low end. Essential for cleaning up layers.
(demo 2
      (rhpf (white-noise) 3000 0.3))


(definst psy-bass [freq 60 cutoff 3000 amp 0.7]
  (let [src (+ (saw freq) (* 0.5 (pulse freq 0.3)))
        ;; Filter envelope: opens fast, closes on decay
        filt-env (env-gen (perc 0.001 0.12))
        ;; Map envelope to cutoff range
        filt (rlpf src (+ freq (* cutoff filt-env)) 0.15)
        dist (clip2 (* (+ filt (* 1.3 (sin-osc freq))) 3) 1.8)
        sig (+ dist (* 1.3 (sin-osc freq)))
        ;; Amplitude envelope: slightly longer than filter
        amp-env (env-gen (perc 0.001 0.1) :action FREE)]
    (* amp sig amp-env)))

(psy-bass :freq (note->hz :a1))



(definst techno-kick [freq 55 punch 5 decay 0.4 amp 0.8]
  (let [;; Pitch drops from freq*punch down to freq
        pitch-env (env-gen (perc 0.001 0.07))
        pitch (+ freq (* freq punch pitch-env))
        src (sin-osc pitch)
        ;; Click transient from noise burst
        click (* (env-gen (perc 0.001 0.01)) (white-noise))
        amp-env (env-gen (perc 0.001 decay) :action FREE)]
    (* amp (+ (* 0.9 src) (* 0.15 click)) amp-env)))


(techno-kick :punch 10 :decay 0.8)



;; Metallic hit — ring modulation + resonant filter
(definst metal-hit [freq 200 mod-freq 347 decay 0.5 amp 0.5]
  (let [;; Ring mod: multiply two signals = sum and difference frequencies
        src (* (sin-osc freq) (sin-osc mod-freq))
        ;; Resonant bandpass for metallic character
        filt (bpf src (* freq 2.7) 0.02)
        env (env-gen (perc 0.001 decay) :action FREE)]
    (* amp env (+ (* 0.7 filt) (* 0.3 src)))))

(metal-hit)


;; Noise drone — filtered noise with slow modulation
(definst industrial-drone [freq 100 amp 0.3]
  (let [;; LFO slowly moves the filter
        lfo (+ freq (* 200 (sin-osc 0.2)))
        src (+ (* 0.5 (saw freq #_white-noise))
               (* 0.5 (pulse freq (+ 0.5 (* 0.4 (sin-osc 0.1))))))
        filt (rlpf src lfo 0.1)]
    (* amp filt)))

(stop)

(industrial-drone)


;; FM: one oscillator modulates the frequency of another
(definst fn-eff [freq 440 lfo-bus 999]
  (let [pow-val (lin-exp:kr (in:kr lfo-bus) -1 1 1 4)
        modulator (* freq (sin-osc 10.5))  ; mod depth * mod osc
        carrier (pulse (+ 220 modulator))
        sig (* carrier (env-gen (perc 0.01 0.35 1)))
        cutoff (* freq (pow 2 pow-val))
        filtered (rlpf sig cutoff 0.1)]
    (pan2 (* 0.5 filtered) 0)))

;; Higher mod index (ratio of mod depth to mod freq) = more harmonics
;; Integer ratios = harmonic; non-integer = inharmonic/metallic
(fn-eff :freq 440 :cutoff 8300)


(def metro (metronome 210))  ; 140 BPM — psy territory


(defonce lfo-group (group :head))

(defonce !lfo-bus (atom nil))
(setter !lfo-bus
        (control-bus))

(defsynth lfo [out-bus 0]
  (out out-bus (brown-noise:kr) #_(sin-osc:kr 0.2)))

(defonce !lfo-synth (atom nil))
(setter !lfo-synth
  (lfo [:tail lfo-group] :out-bus @!lfo-bus))

(println @!lfo-synth)

(defn psy-pattern [beat]
  (at (metro beat) (techno-kick :freq 55 :punch 13 :decay 0.6))
  (doseq [i (range 3)]
    (at (metro (+ (/ i 3) beat)) (psy-bass :amp (case i 0 0.5 0.7)
                                           :cutoff (case i 0 450 500)
                                           :freq (note->hz :a1))))


  (when (zero? (mod beat 16))
    (doseq [i (range 30)]
      (at (metro (+ beat (/ i 2))) (fn-eff :freq 440 :lfo-bus @!lfo-bus))))
  
  ;; Schedule next iteration
  (apply-by (metro (inc beat)) psy-pattern [(inc beat)]))

(psy-pattern (metro))
(stop-all)


(def bm (bus-monitor @!lfo-bus))

(add-watch bm :lfo-watch
           (fn [_ _ old new]
             (when (not= old new)
                   (println (format "%s -> %s" old new)))))

(remove-watch bm :lfo-watch)

(node-tree-seq)
