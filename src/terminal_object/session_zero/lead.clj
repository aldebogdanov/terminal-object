(ns terminal-object.session-zero.lead
  (:require [overtone.live :refer :all]
            [terminal-object.core :refer [setter]]
            [hyperfiddle.rcf :refer [tests]]))

(defonce lead-group (group :tail))

(defsynth lfo2 [freq 0.1 out-bus 0]
  (let [sig (lin-exp:kr (sin-osc-fb:kr freq 0) -1 1 0.003 0.33)]
    (out out-bus sig)))

(defonce !freq-bus (atom nil))
(setter !freq-bus
        (control-bus))

(defonce !freq-lfo (atom nil))
(setter !freq-lfo
        (lfo2 [:tail lead-group] :freq 0.01 :out-bus @!freq-bus))

(defsynth lfo [out-bus 0 freq-bus 999]
  (let [sig (lin-lin:kr (sin-osc:kr (in:kr freq-bus) 0) -1 1 300 2800)]
    (out out-bus sig)))

(defonce !lead-cutoff-bus (atom nil))
(setter !lead-cutoff-bus
        (control-bus))

(defonce !cutoff-lfo (atom nil))
(setter !cutoff-lfo
        (lfo [:after @!freq-lfo] :freq-bus @!freq-bus :out-bus @!lead-cutoff-bus))

(defsynth lead [freq 220 amp 0.5 out-bus 0 pan 0 gate 1 cutoff-bus 999]
  (let [env (env-gen (adsr 0.01 0.1 0.7 0.5) :gate gate :action FREE)
        src (saw:ar (freq-spread:ar freq))
        final (-> src
                  (+ (* 0.5 (brown-noise:ar)))
                  (lpf (in:kr cutoff-bus))
                  (free-verb 0.2 0.6 0.4)
                  (freq-shift 22)
                  
                  (* 1.55))]
    (out out-bus (pan2 (* env final amp) pan))))

(comment

  (stop)
  
  (def monitors
    (let [fstr "Freq: %s\t Cutoff: %s"
          m1 (bus-monitor @!freq-bus)
          m2 (bus-monitor @!lead-cutoff-bus)]
      (add-watch m1 :m
                 (fn [_ _ old new]
                   (when (not= old new)
                     (println (format fstr new @m2)))))
      (add-watch m2 :m
                 (fn [_ _ old new]
                   (when (not= old new)
                     (println (format fstr @m1 new)))))
      [m1 m2]))

  monitors
  
  (doseq [m monitors]
    (remove-watch m :m))
  )

(tests
 "cutoff-lfo listen to freq-bus"
 (long (node-get-control @!cutoff-lfo :freq-bus)) := (:id @!freq-bus)

 "cutoff-lfo speaks to lead-cutoff-bus"
 (long (node-get-control @!cutoff-lfo :out-bus)) := (:id @!lead-cutoff-bus)

 "freq-lfo speaks to freq-bus"
 (long (node-get-control @!freq-lfo :out-bus)) := (:id @!freq-bus))

