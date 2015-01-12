(ns my-proj.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.java.io :as io]))

(defn get-lines-from-file [file]
	(with-open [reader (io/reader file)]
    	(doall (line-seq reader))))

(defn get-euclidean-distance [point1 point2]
	(math/sqrt
		(reduce + 
			(map (fn [x1 x2] (* (- x1 x2) (- x1 x2))) point1 point2))))

(defn get-hamming-distance [point1 point2]
	(reduce + 
		(map (fn [x1, x2] (if (= x1 x2) 0 1)) point1, point2)))

(defn get-points [lines] 
	(let [points (map (fn [x] (str/split x #",")) lines)
		points (map (fn [x] (pop x)) points)
		points (map (fn [x] (map read-string x)) points)]
		points))

(defn get-potential [point1 point2 method]
	(let [d (method point1 point2)]
		(math/expt (Math/E) (- (* alpha d d)))))

(defn get-potentials [features method]	
	(map (fn [x1] 
		(reduce +
			(map (fn [x2] (get-potential x1 x2 method)) features)))
	features))

(defn get-max-potential [potentials]
	(apply max-key second (map-indexed vector potentials)))

(defn get-subtrahend-i [pk xi xk method]
	(let [d (get-hamming-distance xi xk)]
		(* pk (math/expt (Math/E) (- (* betta d d))))))

(defn get-new-potentials [potentials features pk xk method]	
	(map (fn [pi xi] (- pi (get-subtrahend-i pk xi xk method))) potentials features))

(defn get-clusters [potentials-ar points clusters-ar potentials-of-clusters-ar method]
	(loop [potentials potentials-of-clusters-ar
		clusters clusters-ar
        potentials-of-clusters potentials-of-clusters-ar]
        (let [new-potentials (get-new-potentials potentials points (last potentials-of-clusters) (points (last clusters)) method)
        	max-p (get-max-potential new-potentials)
			possible-potential (second max-p)
			possible-cluster (first max-p)]
			(if (= 11 10)
				clusters
				(recur new-potentials (conj clusters possible-cluster) (conj potentials-of-clusters possible-potential))))))

(defn output [clusters features]
	(map (fn [x]
		((print (str "{" (inc x) "} ["))
		(map (fn [y]
			((print (str y " "))))
			(features x))))
	clusters))

(defn -main [file method]
	(def ra 2)
	(def alpha (/ 4 (* ra ra)))
	(def rb (* 1.5 ra))
	(def betta (/ 4 (* rb rb)))
	(let [lines (get-lines-from-file file)
		points (get-points lines)
		points (into [] points)
		get-distance (if (= method "euclid") get-euclidean-distance get-hamming-distance)
		potentials (get-potentials points get-distance)
		max-p (get-max-potential potentials)
		potentials-of-clusters '[]
		potentials-of-clusters (conj potentials-of-clusters (second max-p))
		clusters '[]
		clusters (conj clusters (first max-p))]
		(get-clusters potentials points clusters potentials-of-clusters get-distance)
		;;(output clusters points)
		))