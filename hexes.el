;; hexes.el --- A collection of utilities for working with hexes.
;; Author: Mattias <mattias1126@protonmail.com>
;; Maintainer: Mattias <mattias1126@protonmail.com>
;; Keywords: hexes mapping ttrpg
;; Version 0.1.0
;; URL: <todo: include url>
;; Package-Requires: ((emacs "29.0.50") (svg "1.1"))

;;; Commentary:

;; Provides a set of utilities for working with hexes, in particular we implement ideas from:
;; https://www.redblobgames.com/grids/hexagons/

;;; Code:

(defun hexes-flat-corner (x y size i)
  "Given X, Y, and SIZE of a hex, calculate the coordinates of the Ith corner."
       `(,(+ x (* size (cos (degrees-to-radians (* 60 i))))) .
	 ,(+ y (* size (sin (degrees-to-radians (* 60 i)))))))

(defun hexes-flat-corners (x y size)
  "Given X, Y, and SIZE of a flat hex, calculate all the corners."
(mapcar #'(lambda (i) (hexes-flat-corner x y size i))
	(number-sequence 0 5)))

(defun hexes-axial-s-component (q r)
  "Calculate the S component using Q and R for an axial coordinate system."
  (- (* -1 q) r))

(defun hexes-axial-to-cartesian (q r size &optional center)
  "Convert axial Q R coordinates to cartesian with a given SIZE and CENTER.
If CENTER is non-nil, use this value as the center of the 0,0 hex,
otherwise use 0."
  (let ((center (or center 0)))
  `(,(+ center (* size (* (/ 3.0 2) q))) .
    ,(+ center (* size (+ (* (/ (sqrt 3) 2) q) (* (sqrt 3) r)))))))

(defun hexes-flat-side-midpoint (x y size i)
  "Get the midpoint of the Ith hex side at X, Y with specified SIZE."
  (let ((corner-0 (hexes-flat-corner x y size (- i 2)))
	(corner-1 (hexes-flat-corner x y size (1+ (- i 2)))))
    `(,(/ (+ (car corner-0) (car corner-1)) 2) .
      ,(/ (+ (cdr corner-0) (cdr corner-1)) 2))))


(defun hex-road-coords (x y size start end)
  "Calculate the coordinates for a road with START and END.
The road is in the hex centered at X and Y with SIZE.
If START or END is a property list with :q :r :index :feature parameters
then get that point using the seeded random offset"
  (let ((midpoint-start (if (listp start)
			    (let ((start-offset (hex-feature-offset (plist-get start :q)
								    (plist-get start :r)
								    (plist-get start :feature)
								    (plist-get start :index))))
			      `(,(+ x (* (car start-offset) (/ size 1.5))) .
				,(+ y (* (cdr start-offset) (/ size 1.5)))))
			  (hexes-flat-side-midpoint x y size start)))
	(midpoint-end (if (listp end)
			  (let ((end-offset (hex-feature-offset (plist-get end :q)
								(plist-get end :r)
								(plist-get end :feature)
								(plist-get end :index))))
			    `(,(+ x (* (car end-offset) (/ size 1.5))) .
			      ,(+ y (* (cdr end-offset) (/ size 1.5)))))
			(hexes-flat-side-midpoint x y size end))))
    `((,(car midpoint-start) . ,(cdr midpoint-start))
      (,x ,y ,(car midpoint-end) ,(cdr midpoint-end)))))


(provide 'hexes)
;;; hexes.el ends here
