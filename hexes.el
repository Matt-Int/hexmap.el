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

(require 'cl-lib)

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

(defun hex-feature-offset (q r &optional feature index)
  "Offset a FEATURE in hex Q, R based on its INDEX.
If INDEX is nil or 0, then it will be in the center of the hex.
This is only for instances where a single hex would have more than one FEATURE."
  (if (and index (> index 0))
      (let ((offset-direction (degrees-to-radians
			       (random-with-seed
				(format "%s%s%s%s" q r feature index)))))
	`(,(cos offset-direction) . ,(sin offset-direction)))
    '(0 . 0) ;; return offsets of 0 0 if the index is not provided or is 0
    ))

(defun hex-feature-axial-to-cartesian-coords (q r size index &optional feature center)
  "Convert a FEATURE's Q R coordinates into cartesian based on the SIZE and INDEX.
See `hex-feature-offset' for details on INDEX.
See `hex-axial-to-cartesian' for details on CENTER."
  (let ((coords (hexes-axial-to-cartesian q r size center))
	(offset (hex-feature-offset q r feature index)))
    (let ((offset-coords `(,(+ (car coords) (* (car offset) (/ size 1.5))) .
			   ,(+ (cdr coords) (* (cdr offset) (/ size 1.5))))))
      offset-coords)))

(defun hexes-boundaries (hexes-coordinates)
  "Return the max distance of all HEXES-COORDINATES provided to their average.
HEXES-COORDINATES should be a dot-pair list of \='((Q . R) (Q . R)) etc.."
  (let ((coords hexes-coordinates)
	(center (hexes-center hexes-coordinates))
	(results))
    (dolist (i coords)
      (add-to-list 'results (max (abs (- (car center) (car i)))
				 (abs (- (cdr center) (cdr i)))
				 (abs (- (hexes-axial-s-component (car i) (cdr i))
					 (hexes-axial-s-component (car center) (cdr center)))))))
    (cl-reduce #'max results)))

(defun hexes-center (hexes-coordinates)
  "Return the average coordinates of all HEXES-COORDINATES provided.
HEXES-COORDINATES should be a dot-pair list of \='((Q . R) (Q . R)) etc.."
  (let ((qs (mapcar #'car hexes-coordinates))
	(rs (mapcar #'cdr hexes-coordinates)))
    (message "qs: %s" qs)
    `(,(round (/ (apply #'+ qs) (float (length qs))))
      . ,(round (/ (apply #'+ rs) (float (length rs))))
      )))

(provide 'hexes)
;;; hexes.el ends here
