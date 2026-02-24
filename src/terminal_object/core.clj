(ns terminal-object.core
  (:require [hyperfiddle.rcf]
            [overtone.live :refer :all]
            [overtone.at-at :as at]
            [overtone.sc.protocols]
            [overtone.sc.bus]))

(hyperfiddle.rcf/enable!)

(stop)

;; -- Pattens --
(defonce !patterns (atom {}))

(defn pset! [& args]
  (let [pairs (partition 2 args)]
    (doseq [[k pattern-fn] pairs]
      (let [v (resolve pattern-fn)]
        (swap! !patterns assoc k v)
        (println (format "Pattern set: %s -> %s" k (symbol v)))))))

(defn pdrop! [& ks]
  (doseq [k ks]
    (swap! !patterns dissoc k)
    (println "Pattern dropped:" k)))

;; -- Player --
(defonce ^:private !player-running? (atom false))

(defonce ^:private !bpm (atom 120))

(def metro (metronome @!bpm))

(defn- player
  "Runs in parallel all pattern vars from `!patterns` atom"
  [beat]
  (->> @!patterns
       vals
       (pmap #(% beat))
       dorun))

(def ^:private player-pool (at/mk-pool))

(defonce ^:private !scheduler-id (atom nil))  ; Track running scheduler

(defn start-player []
  (when @!scheduler-id
    (at/stop @!scheduler-id)
    (reset! !scheduler-id nil))
  
  (reset! !player-running? true)
  (reset! !scheduler-id
    (at/every (/ 60000 @!bpm)
              #(let [b (metro)]
                 (player b))
              player-pool
              :desc "Session Zero player")))

(defn stop-player []
  (reset! !player-running? false)
  (when @!scheduler-id
    (at/stop @!scheduler-id)
    (reset! !scheduler-id nil)))

(defn restart-player []
  (stop-player)
  (Thread/sleep 150)  ; Brief pause to ensure cleanup
  (start-player)
  (println "Player restarted at" @!bpm "BPM"))

(defn set-bpm-and-restart [bpm]
  (let [bpm (max 0.001 bpm)
        was-playing @!player-running?]
    (when was-playing (stop-player))
    (metro :bpm (reset! !bpm bpm))
    (when was-playing (start-player))
    (println "BPM set to" @!bpm (if was-playing "(player restarted)" ""))))

;; -- Helpers --

(defn note->hz [note]
  (-> note note-info :midi-note midi->hz))

(defmacro setter [atom & body]
  `(let [obj# (deref ~atom)]
     (when (overtone.sc.bus/bus? obj#)
       (free-bus obj#))
     (when (node-active? obj#)
       (kill obj#))
     (reset! ~atom (do ~@body))))
