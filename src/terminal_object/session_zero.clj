(ns terminal-object.session-zero
  (:require [overtone.live :refer :all]
            [hyperfiddle.rcf]
            [terminal-object.core :refer [metro start-player stop-player restart-player
                                          set-bpm-and-restart pset! pdrop! note->hz]]
            [terminal-object.session-zero.master :refer [master-bus master-group]]
            [terminal-object.session-zero.kick :refer [psy-kick]]
            [terminal-object.session-zero.bass :refer [roll-bass !bass-synth]]
            [terminal-object.session-zero.crash :refer [soft-massive-crash]]
            [overtone.inst.drum :as d]
            [overtone.inst.synth :as s]))

(defn kick-4-4 [beat]
  (when (zero? (mod beat 4))
    (doseq [i (range 4)]
      (at (metro (+ beat i)) (psy-kick [:head master-group] :freq 55 :amp 0.75 :out-bus master-bus)))))

(defn bass-triple [beat]
  (doseq [i (range 3)]
    (at (metro (+ beat (/ i 3))) (roll-bass [:head master-group] :freq 55
                                            :filtfreq (case i 0 500 800)
                                            :out-bus master-bus))))

(defn rolling-lead-pattern-mono [b]
  (when (zero? (mod b 4))
    (doseq [i (range 4)]
      (let [notes #_[:a3 nil :a3 :a3] (take 4 (repeatedly #(rand-nth [:a3 :a3 :c4 :e4])))]
        (doseq [[idx note] (map-indexed vector notes)]
          (let [beat-offset (+ b i (* idx (/ 1 4)))
                accent (if (zero? (mod idx 4)) 1.0 0.7)]
            (when note
              (at (metro (+ beat-offset 0.00))
                  (ctl @!bass-synth
                       :freq (note->hz note)
                       :gate 1    ;; открываем (retrigger)
                       :amp (* 0.6 accent)))
              (at (metro (+ beat-offset (/ 60 (metro :bpm) (if (or (not= 1 idx) #_(some? (nth notes idx))) 1 2))))
                  (ctl @!bass-synth
                       :gate 0)))))))))

(defn hats-8 [beat]
  (doseq [i (range 2)]
    (let [b (+ beat (/ i 2))
          a (case i
              0 0.04
              1 (rand-nth [0.03 0 0]))]
      (at (metro b) (d/hat3 :amp a :t 0.02)))))

(defn crash [b]
  (when (zero? (mod b 64))
    (at (metro b) (soft-massive-crash :amp 1.0 :release 4.0 :room 0.999 :mix 0.5))))

(defn claps [b]
  (when (= 1 (mod b 2))
    (free-verb (in:ar (d/clap :amp 0.1 :decay 0.5)) 0.8 0.2 0.1)))


(defn o-hats [b]
  (at (metro (+ 0.5 b)) (d/open-hat 0.015 0.3)))

(comment

  (ns-unmap *ns* 'lead)
  
  (s/supersaw 110)
  (stop)

  (metro)
  @terminal-object.core/!patterns

  (reset! terminal-object.core/!patterns {})

  (pset! :o 'o-hats)
  (pdrop! :o)

  (pset! :t 'toms)
  (pdrop! :t)
  (pdrop! :c)
  (pset! :c 'crash)
  (pset! :c 'crash :l 'lead-atonal)

  (pdrop! :b)
  
  (pset! :k 'kick-4-4
         :b 'bass-triple
         :l 'rolling-lead-pattern-mono
         :h 'hats-8)
  (pset! :h 'hats-8)
  (pset! :l 'rolling-lead-pattern-mono)
  (pdrop! :cl)
  (pdrop! :l)
  (pdrop! :h)
  (pdrop! :k)

  (pdrop! :k :h :b)

  (d/snare)
  ;; ===== CONTROL =====
  (start-player)   ; Start with current code
  (stop-player)    ; Stop completely
  (restart-player) ; Stop and start with latest code

  (metro :bpm)  ; Check current beat
  ;; ===== TEMPO =====
  (set-bpm-and-restart 165)  ; Change BPM, restart if playing
  (set-bpm-and-restart 210)
  (set-bpm-and-restart 0)
  @!bpm  ; Check current BPM
 
  ;; ===== DEBUG =====
  (server-info)
  (node-tree)
  
  ;; ===== EXPERIMENT =====
  ;; Modify the player function above, then:
  (restart-player)  ; Instantly hear changes
  
  ;; Try new patterns in isolation:
  (do
    (toggle-hats)
    (at (m (inc (m))) #(d/hat3 :amp 0.4)))
  
  ;; ===== CLEANUP =====
  (stop-player)
  (kill-server)
  (boot-server)
  (stop)

  (recording-start "~/Desktop/session-zero.wav")

  (recording-stop)
  )
