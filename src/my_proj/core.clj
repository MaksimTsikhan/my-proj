(ns my-proj.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.java.io :as io]))

(def ra 2)
(def alpha (/ 4 (* ra ra)))
(def rb (* 1.5 ra))
(def betta (/ 4 (* rb rb)))

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

(defn get-min-distance [xk clusters points method]
	(let [distances (map (fn [i] (method xk (points i))) clusters)]
		(second (apply min-key second (map-indexed vector distances)))))

(defn get-clusters [potentials-ar points clusters-ar potentials-of-clusters-ar method]
	(def eps1 0.5)
	(def eps2 0.15)
	(loop [potentials potentials-ar
		clusters clusters-ar
        potentials-of-clusters potentials-of-clusters-ar]
        (let [
        	p1 (first potentials-of-clusters)
        	pk (last potentials-of-clusters)
        	k-index (last clusters)
        	xk (points k-index)
        	new-potentials (get-new-potentials potentials points pk xk method)
        	max-p (get-max-potential new-potentials)
			possible-potential (second max-p)
			possible-cluster (first max-p)
			is-center (> possible-potential (* eps1 p1))
			to-continue is-center]

			(if (= is-center true)
			(clusters (conj clusters possible-cluster)
			(potentials-of-clusters (conj potentials-of-clusters possible-potential)))
			(let [dmin (get-min-distance xk clusters points method)]
				(if (and (>= possible-potential (* eps2 p1)) (>= (+ (/ dmin ra) (/ possible-potential p1))))
				(to-continue true)
				(new-potentials (assoc new-potentials possible-cluster 0)))))
			(if (to-continue)
				clusters
				(recur new-potentials clusters potentials-of-clusters)))))


(defn output [clusters features]
	(map (fn [x]
		((print (str "{" (inc x) "} ["))
		(map (fn [y]
			((print (str y " "))))
			(features x))))
	clusters))

(defn -main [file method]
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
		clusters (get-clusters potentials points clusters potentials-of-clusters get-distance)
		(output clusters points)
		))