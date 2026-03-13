(ns terminal-object.session-zero.master
  (:require [overtone.live :refer :all]))


(def master-group (group))


;; ─────────────────────────────────────────
;; 1. Create an intermediate bus
;;    (don't process bus 0 directly — it's fragile)
;; ─────────────────────────────────────────
(defonce master-bus (audio-bus 2))  ;; stereo

;; ─────────────────────────────────────────
;; 2. EQ — gentle high-pass + presence boost
;; ─────────────────────────────────────────
(defsynth master-eq [in-bus 0 out-bus 0]
  (let [sig      (in:ar in-bus 2)
        ;; Remove sub-rumble below 30Hz
        sig      (hpf sig 30)
        ;; Subtle high shelf +1.5dB at 8kHz for "air"
        sig      (b-hi-shelf sig 8000 1.0 1.5)]
    (out:ar out-bus sig)))

;; ─────────────────────────────────────────
;; 3. Stereo bus compressor — glue
;; ─────────────────────────────────────────
(defsynth master-compressor [in-bus 0 out-bus 0
                             threshold -12 ratio 2.5
                             attack 0.03 release 0.15]
  (let [sig        (in:ar in-bus 2)
        compressed (compander sig sig
                              (dbamp threshold)
                              1 (/ 1 ratio)   ;; above threshold
                              attack release)]
    (out:ar out-bus compressed)))

;; ─────────────────────────────────────────
;; 4. Brickwall limiter — loudness & safety
;; ─────────────────────────────────────────
(defsynth master-limiter [in-bus 0 out-bus 0
                          ceiling -0.3          ;; dBFS ceiling
                          lookahead 0.005]
  (let [sig     (in:ar in-bus 2)
        limited (limiter sig (dbamp ceiling) lookahead)]
    (out:ar out-bus limited)))

;; ─────────────────────────────────────────
;; 5. Final output writer → bus 0
;; ─────────────────────────────────────────
(defsynth master-out [in-bus 0]
  (out:ar 0 (* 0.8 (in:ar in-bus 2))))


;; Intermediate buses for the chain
(defonce eq-bus   (audio-bus 2))
(defonce comp-bus (audio-bus 2))
(defonce lim-bus  (audio-bus 2))

(defonce !master-chain (atom {}))

(defn start-master-chain! []
  (reset! !master-chain
    {:eq   (master-eq         [:tail master-group] :in-bus master-bus :out-bus eq-bus)
     :comp (master-compressor [:tail master-group] :in-bus eq-bus     :out-bus comp-bus)
     :lim  (master-limiter    [:tail master-group] :in-bus comp-bus   :out-bus lim-bus)
     :out  (master-out        [:tail master-group] :in-bus lim-bus)}))

(defn stop-master-chain! []
  (doseq [[_ node] @!master-chain]
    (when (node-active? node)
      (kill node)))
  (reset! !master-chain {}))

(comment
  (start-master-chain!)
  (stop-master-chain!))
