(ns graphterglow.core
  (:require [afterglow.effects :as fx]
            [afterglow.effects.params :as params]
            [afterglow.show :as show]
            [afterglow.show-context :refer [with-show]]
            [afterglow.rhythm :as rhythm]
            [afterglow.transform :as tf]
            [afterglow.util :as util]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]
            [incanter.svg :refer [save-svg]])
  (:import [afterglow.rhythm MetronomeSnapshot]))

(def a-show (show/show))

(defn build-test-snapshot
  "Creates a metronome snapshot representing the specified number of
  milliseconds after the supplied metrome was started."
  [metro offset]
  (let [snap (rhythm/metro-snapshot metro)
        start (:start snap)
        instant (+ start offset)
        beat (rhythm/marker-number instant start (rhythm/metro-tick metro))
        bar (rhythm/marker-number instant start (rhythm/metro-tock metro))
        phrase (rhythm/marker-number instant start (rhythm/metro-ding metro))
        beat-phase (rhythm/marker-phase instant start (rhythm/metro-tick metro))
        bar-phase (rhythm/marker-phase instant start (rhythm/metro-tock metro))
        phrase-phase (rhythm/marker-phase instant start (rhythm/metro-ding metro))]
    (MetronomeSnapshot. start (:bpm snap) (:bpb snap) (:bpp snap)
                        instant beat bar phrase beat-phase bar-phase phrase-phase)))

(defn build-beat-snapshot
  "Create a snapshot that represents the specified number of beats after
  the creation of the supplied metronome."
  [metro beats]
  (build-test-snapshot metro (* beats (rhythm/metro-tick metro))))

(defn graph-step
  "Draw a graph of a step parameter"
  []
  (let [metro (:metronome a-show)
        basic-step (params/build-step-param :starting (build-beat-snapshot metro 1))
        f (fn [x] (params/evaluate basic-step a-show (build-beat-snapshot metro x)))
        plot (function-plot f 0 4 :x-label "beat (1 is closest to start point)" :y-label "step param"
                            :title "fade-fraction 0 (default)")]
    (view plot)))

(defn graph-fade
  "Draw a graph of a step parameter with maximum fade-fraction"
  []
  (let [metro (:metronome a-show)
        basic-step (params/build-step-param :fade-fraction 1 :starting (build-beat-snapshot metro 1))
        f (fn [x] (params/evaluate basic-step a-show (build-beat-snapshot metro x)))
        plot (function-plot f 0 4 :x-label "beat (1 is closest to start point)" :y-label "step param"
                            :title "fade-fraction 1 (maximum)")]
    (view plot)))

(defn graph-fraction
  "Draw a graph of a step parameter with specified fade-fraction"
  [fraction]
  (let [metro (:metronome a-show)
        basic-step (params/build-step-param :fade-fraction fraction :starting (build-beat-snapshot metro 1))
        f (fn [x] (params/evaluate basic-step a-show (build-beat-snapshot metro x)))
        plot (function-plot f 0 4 :x-label "beat (1 is closest to start point)" :y-label "step param"
                            :title (str "fade-fraction " fraction))]
    (view plot)
    ;;(save-svg plot "/Users/jim/Desktop/plot-fraction-0-5.svg")
    ))

(defn graph-sine-fade
  "Draw a graph of a step parameter with sine fade curve and maximum fade-fraction"
  []
  (let [metro (:metronome a-show)
        basic-step (params/build-step-param :fade-fraction 1 :fade-curve :sine
                                            :starting (build-beat-snapshot metro 1))
        f (fn [x] (params/evaluate basic-step a-show (build-beat-snapshot metro x)))
        plot (function-plot f 0 4 :x-label "beat (1 is closest to start point)" :y-label "step param"
                            :title "sine fade-fraction 1 (maximum)")]
    (view plot)))

(defn graph-sine-fraction
  "Draw a graph of a step parameter with sine fade curve and specified fade-fraction"
  [fraction]
  (let [metro (:metronome a-show)
        basic-step (params/build-step-param :fade-fraction fraction :fade-curve :sine
                                            :starting (build-beat-snapshot metro 1))
        f (fn [x] (params/evaluate basic-step a-show (build-beat-snapshot metro x)))
        plot (function-plot f 0 4 :x-label "beat (1 is closest to start point)" :y-label "step param"
                            :title (str "sine fade-fraction " fraction))]
    (view plot)))
