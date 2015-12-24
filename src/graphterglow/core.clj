(ns graphterglow.core
  (:require [afterglow.effects :as fx]
            [afterglow.effects.params :as params]
            [afterglow.effects.oscillators :as osc]
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
(with-show a-show
  (show/patch-fixture! :dimmer (afterglow.fixtures/generic-dimmmer) 1 1))

(defn build-test-snapshot
  "Creates a metronome snapshot representing the specified number of
  milliseconds after the supplied metronome was started."
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
        f (fn [x] (params/evaluate basic-step a-show (build-beat-snapshot metro x) nil))
        plot (function-plot f 0 4 :x-label "beat (1 is closest to start point)" :y-label "step param"
                            :title "fade-fraction 0 (default)")]
    (view plot)))

(defn graph-fade
  "Draw a graph of a step parameter with maximum fade-fraction"
  []
  (let [metro (:metronome a-show)
        basic-step (params/build-step-param :fade-fraction 1 :starting (build-beat-snapshot metro 1))
        f (fn [x] (params/evaluate basic-step a-show (build-beat-snapshot metro x) nil))
        plot (function-plot f 0 4 :x-label "beat (1 is closest to start point)" :y-label "step param"
                            :title "fade-fraction 1 (maximum)")]
    (view plot)))

(defn graph-fraction
  "Draw a graph of a step parameter with specified fade-fraction"
  [fraction]
  (let [metro (:metronome a-show)
        basic-step (params/build-step-param :fade-fraction fraction :starting (build-beat-snapshot metro 1))
        f (fn [x] (params/evaluate basic-step a-show (build-beat-snapshot metro x) nil))
        plot (function-plot f 0 4 :x-label "beat (1 is closest to start point)" :y-label "step param"
                            :title (str "fade-fraction " fraction))]
    (view plot)
    ;;(save-svg plot "/Users/james/Desktop/plot-fraction-0-5.svg")
    ))

(defn graph-sine-fade
  "Draw a graph of a step parameter with sine fade curve and maximum fade-fraction"
  []
  (let [metro (:metronome a-show)
        basic-step (params/build-step-param :fade-fraction 1 :fade-curve :sine
                                            :starting (build-beat-snapshot metro 1))
        f (fn [x] (params/evaluate basic-step a-show (build-beat-snapshot metro x) nil))
        plot (function-plot f 0 4 :x-label "beat (1 is closest to start point)" :y-label "step param"
                            :title "sine fade-fraction 1 (maximum)")]
    (view plot)))

(defn graph-sine-fraction
  "Draw a graph of a step parameter with sine fade curve and specified fade-fraction"
  [fraction]
  (let [metro (:metronome a-show)
        basic-step (params/build-step-param :fade-fraction fraction :fade-curve :sine
                                            :starting (build-beat-snapshot metro 1))
        f (fn [x] (params/evaluate basic-step a-show (build-beat-snapshot metro x) nil))
        plot (function-plot f 0 4 :x-label "beat (1 is closest to start point)" :y-label "step param"
                            :title (str "sine fade-fraction " fraction))]
    (view plot)))

(defn graph-sawtooth
  "Draw a graph of an oscillated parameter based on a sawtooth oscillator"
  [& {:keys [interval] :or {interval :beat}}]
  (let [metro (:metronome a-show)
        osc-param (with-show a-show (osc/build-oscillated-param (osc/sawtooth :interval interval) :max 1))
        f (fn [x] (params/evaluate osc-param a-show (build-beat-snapshot metro x) nil))
        max-beat (if (= interval :phrase) 64 4)
        plot (function-plot f 0 max-beat :x-label "beat" :y-label "oscillator value"
                            :title (if (= interval :beat)
                                     "default sawtooth"
                                     (str "sawtooth with :interval " interval)))]
    (view plot)))

(defn graph-sawtooth-down
  "Draw a graph of an oscillated parameter based on a sawtooth oscillator in down mode"
  []
  (let [metro (:metronome a-show)
        osc-param (with-show a-show (osc/build-oscillated-param
                                     (osc/sawtooth :down? true) :max 1))
        f (fn [x] (params/evaluate osc-param a-show (build-beat-snapshot metro x) nil))
        plot (function-plot f 0 4 :x-label "beat" :y-label "oscillator value"
                            :title "downward sawtooth")]
    (view plot)))

(defn graph-sawtooth-ratio
  "Draw a graph of an oscillated parameter based on a sawtooth oscillator with a beat ration"
  [r]
  (let [metro (:metronome a-show)
        osc-param (with-show a-show (osc/build-oscillated-param
                                     (osc/sawtooth :interval-ratio r) :max 1))
        f (fn [x] (params/evaluate osc-param a-show (build-beat-snapshot metro x) nil))
        plot (function-plot f 0 4 :x-label "beat" :y-label "oscillator value"
                            :title (str "sawtooth with :interval-ratio " r))]
    (view plot)))

(defn graph-sawtooth-phase
  "Draw a graph of an oscillated parameter based on a sawtooth oscillator with a phase"
  [phase]
  (let [metro (:metronome a-show)
        osc-param (with-show a-show (osc/build-oscillated-param
                                     (osc/sawtooth :phase phase) :max 1))
        f (fn [x] (params/evaluate osc-param a-show (build-beat-snapshot metro x) nil))
        plot (function-plot f 0 4 :x-label "beat" :y-label "oscillator value"
                            :title (str "sawtooth with :phase " phase))]
    (view plot)))

(defn graph-triangle
  "Draw a graph of an oscillated parameter based on a triangle oscillator"
  [& {:keys [interval] :or {interval :beat}}]
  (let [metro (:metronome a-show)
        osc-param (with-show a-show (osc/build-oscillated-param (osc/triangle :interval interval) :max 1))
        f (fn [x] (params/evaluate osc-param a-show (build-beat-snapshot metro x) nil))
        max-beat (if (= interval :phrase) 64 4)
        plot (function-plot f 0 max-beat :x-label "beat" :y-label "oscillator value"
                            :title (if (= interval :beat)
                                     "default triangle"
                                     (str "triangle with :interval " interval)))]
    (view plot)))

(defn graph-square
  "Draw a graph of an oscillated parameter based on a square wave oscillator"
  [& {:keys [interval] :or {interval :beat}}]
  (let [metro (:metronome a-show)
        osc-param (with-show a-show (osc/build-oscillated-param (osc/square :interval interval) :max 1))
        f (fn [x] (params/evaluate osc-param a-show (build-beat-snapshot metro x) nil))
        max-beat (if (= interval :phrase) 64 4)
        plot (function-plot f 0 max-beat :x-label "beat" :y-label "oscillator value"
                            :title (if (= interval :beat)
                                     "default square"
                                     (str "square with :interval " interval)))]
    (view plot)))

(defn graph-square-width
  "Draw a graph of an oscillated parameter based on a square oscillator with a width"
  [width]
  (let [metro (:metronome a-show)
        osc-param (with-show a-show (osc/build-oscillated-param
                                     (osc/square :width width) :max 1))
        f (fn [x] (params/evaluate osc-param a-show (build-beat-snapshot metro x) nil))
        plot (function-plot f 0 4 :x-label "beat" :y-label "oscillator value"
                            :title (str "square with width " width))]
    (view plot)))

(defn graph-sine
  "Draw a graph of an oscillated parameter based on a sine wave oscillator"
  [& {:keys [interval] :or {interval :beat}}]
  (let [metro (:metronome a-show)
        osc-param (with-show a-show (osc/build-oscillated-param (osc/sine :interval interval) :max 1))
        f (fn [x] (params/evaluate osc-param a-show (build-beat-snapshot metro x) nil))
        max-beat (if (= interval :phrase) 64 4)
        plot (function-plot f 0 max-beat :x-label "beat" :y-label "oscillator value"
                            :title (if (= interval :beat)
                                     "default sine"
                                     (str "sine with :interval " interval)))]
    (view plot)))

(defn graph-skip-sawtooth
  "Draw a graph of a chase which skips every other instance of a
  double-time sawtooth beat oscillator, as an example of how to
  compose oscillators with chases; a square wave oscillator is used to
  pick between the chase elements."
  []
  (with-show a-show
    (let [metro (:metronome a-show)
          position-param (osc/build-oscillated-param (osc/square) :min 1 :max 2)
          saw-param (osc/build-oscillated-param (osc/sawtooth :beat-ratio (/ 2)))
          chase (fx/chase "Gap Saw"
                          [(fx/blank)
                           (afterglow.effects.dimmer/dimmer-effect saw-param (show/all-fixtures))]
                          position-param)
          f (fn [x] (let [snapshot (build-beat-snapshot metro x)
                          assigners (fx/generate chase a-show snapshot)]
                      (if-let [assigner (first assigners)]
                        (params/resolve-param (fx/assign assigner a-show snapshot nil nil) a-show snapshot)
                        0)))
          plot (function-plot f 0 4 :x-label "beat" :y-label "dimmer value"
                              :title "Chase including every other sawtooth wave")]
      (view plot))))
