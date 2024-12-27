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


(provide 'hexes)
;;; hexes.el ends here
