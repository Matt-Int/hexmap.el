;;; hex-drawing.el --- Draw hexes as SVGs, for use with hexmap-mode.el
;; Author: Mattias <mattias1126@protonmail.com>
;; Maintainer: Mattias <mattias1126@protonmail.com>
;; Keywords: hexes mapping ttrpg svg image
;; Version 0.1.0
;; URL: <todo: include url>
;; Package-Requires: ((emacs "29.0.50") (svg "1.1") (hexmap-mode "0.1.0"))

;;; Commentary:

;; Provides a set of functions to draw the hexes as represented in `.hexmap' files

;;; Code:
(require 'svg)
(require 'hexes)

(defun random-with-seed (seed)
  "Generate a random number with SEED and outputs a degree from 0 to 365.
Runs \(random t) at the end to avoid reproducible numbers elsewhere."
  (if (numberp seed) (setq seed (number-to-string seed)))
  (let ((seed-result (random seed))
	(result-degrees (random 365))
	(result-reseeded (random t)))
    result-degrees))

(defun hex-draw (svg x y size &optional fill stroke label)
  "Draw a hex in SVG with center of X, Y and specified SIZE.
Optionally provide the FILL, STROKE, and LABEL for the hex."
  (let ((fill (or fill "transparent"))
	(label (or label ""))
	(stroke (or stroke "black")))
    (svg-polygon svg (hexes-flat-corners x y size)
		 :stroke-color stroke :fill-color fill
		 :stroke-width (* size (/ 3.0 80.0)))
    (svg-text svg label :x x :y y :font-size (/ size 2) :text-anchor "middle")))


(defun hex-draw-axial (svg q r size &optional canvas fill stroke label offset)
  "Draw a hex on an SVG, accepts axial Q and R coordinates.
Needs to provide the SIZE for converting to cartesian coordinates.
CANVAS can be provided to calculate the center of 0,0.
OFFSET can be used to offset the final X,Y coordinates by the specified amount.
FILL, STROKE, and LABEL can also be provided, see `draw-hex' for details.
Example usage: \(let ((svg (svg-create 200 200)))
  (hex-draw-axial svg 0 0 20 200 \"green\")
  (hex-draw-axial svg 0 1 20 200 \"green\")
  (with-current-buffer (get-buffer-create \"*SVG Image: Testing*\")
    (erase-buffer)
    (insert-image (svg-image svg))
    (display-buffer \"*SVG Image: Testing*\")))"
  (let ((coords (hexes-axial-to-cartesian q r size))
	(center (or canvas 0))
	(fill (or fill "pink"))
	(offset (or offset '(0 . 0))))
    (hex-draw svg
	      (- (+ (/ center 2) (car coords)) (car offset))
	      (- (+ (/ center 2) (cdr coords)) (cdr offset))
	      size
	      fill stroke label)))

(defun hex-draw-road (svg x y size start end &optional colour width)
  "Draw a road segment within a hex centered at X, Y with SIZE on SVG.
Optionally, add in the COLOUR and WIDTH of the road segment."
  (let ((colour (or colour "brown"))
	(width (or width 1))
	(road-1 (hex-road-coords x y size start end)))
    (svg-path svg `((moveto (,(car road-1)))
		(smooth-curveto ,(cdr road-1)))
	  :stroke colour :fill "transparent" :stroke-width (* size (/ width 80.0)))))

(defun hex-draw-axial-road (svg q r size start end &optional center colour width offset)
  "Use Q and R axial coordinates to draw a road on an SVG hex with a given SIZE.
Road is specified using START and END which is 0->5 starting with 0 at top,
and incrementing by one going clockwise.
OFFSET is used when Q:0,R:0 is no longer the center hex."
  (let ((cartesian (hexes-axial-to-cartesian q r size center))
	(offset (or offset '(0 . 0))))
    (hex-draw-road svg (- (car cartesian) (car offset))
		   (- (cdr cartesian) (cdr offset)) size start end colour width)))


(defun hex-draw-feature--draw-unknown (svg x y size &optional feature)
  "Draw a generic feature icon on SVG at X, Y with specified SIZE."
  (if feature
      (message (format "Feature: %s does not have a draw-function" feature)))
  (svg-circle svg x y
	      (/ size 20) :stroke-color "darkred" :fill-color "white"
	      :stroke-width (* size (/ 3.0 80.0)))
  (svg-text svg label :x x :y (- y (/ size 20)) :font-size (/ size 6) :text-anchor "middle"))


(defun hex-draw-feature--draw-lair (svg x y size)
  "Draw a generic feature icon on SVG at X, Y with specified SIZE."
  (svg-circle svg x y
	      (/ size 20) :stroke-color "darkred" :fill-color "black"
	      :stroke-width (* size (/ 3.0 80.0)))
  (svg-text svg label :x x :y (- y (/ size 20)) :font-size (/ size 6) :text-anchor "middle"))

(defun hex-draw-feature--draw-village (svg x y size)
  "Draw a village icon on SVG at X, Y with specified SIZE."
  (svg-rectangle svg
		 (- x (/ (/ size 5) 2))
		 (- y (/ (/ size 5) 2))
		 (/ size 5) (/ size 5) :stroke-color "black" :fill-color "white"
		 :stroke-width (* size (/ 3.0 80.0)))
  (svg-text svg label :x x :y (- y (/ size 5)) :font-size (/ size 6) :text-anchor "middle"))

(defun hex-draw-feature--draw-city (svg x y size)
  "Draw a city icon on SVG at X, Y with specified SIZE."
  (svg-rectangle svg
		 (- x (/ (/ size 2) 2))
		 (- y (/ (/ size 2) 2))
		 (/ size 2) (/ size 2) :stroke-color "black" :fill-color "white"
		 :stroke-width (* size (/ 3.0 80.0)))
  (svg-rectangle svg
		 (- x (/ (/ size 3) 2))
		 (- y (/ (/ size 3) 2))
		 (/ size 3) (/ size 3) :stroke-color "black" :fill-color "white"
		 :stroke-width (* size (/ 3.0 80.0)))
  (svg-text svg label :x x :y (- y (/ size 2)) :font-size (/ size 6) :text-anchor "middle"))


(defcustom feature-draw-functions '((village . hex-draw-feature--draw-village)
				    (city . hex-draw-feature--draw-city)
				    (lair . hex-draw-feature--draw-lair)
				    (nil . hex-draw-feature--draw-unknown))
  "A list of functions for features and how they should be drawn."
  :group 'hexmapping)

(defun hex-draw-terrain--draw-blank (svg x y size &optional terrain)
  "Draw no TERRAIN on SVG at X, Y with given SIZE.
This function does nothing on purpose and just takes the appropriate input."
  (message "Terrain: %s does not have a draw-function" terrain))

(defun hex-draw-terrain--draw-hills (svg x y size &optional biome)
  "Draw a hill terrain symbol on SVG at X,Y at given SIZE.
Provide BIOME to get the matching `biome-highlight-colours'."
  (let ((xr (* x (/ 35.0 200)))
	(yr (* y (/ 100.0 200)))
	(colour (if biome (cdr (assoc biome biome-highlight-colours)) "white")))
    (svg-path svg `((moveto ((,x . ,y)))
		    (elliptical-arc ((,(* size (/ 35.0 80)) ,(* size (/ 100.0 80)) ,(* size (/ 40.0 80)) 0 :sweep t))))
	      :fill "transparent" :stroke colour :relative t :stroke-width (* size (/ 4.0 80)))
    (svg-path svg `((moveto ((,x . ,y)))
		    (moveto ((,(* size (/ -20.0 80)) . ,(* size (/ -20.0 80)))) :relative t)
		    (elliptical-arc ((,(* size (/ 35.0 80)) ,(* size (/ 100.0 80)) ,(* size (/ 40.0 80)) 0 :sweep t))))
	      :fill "transparent" :stroke colour :relative t :stroke-width (* size (/ 4.0 80)))
    (svg-path svg `((moveto ((,x . ,y)))
		    (moveto ((,(* size (/ -50.0 80)) . 0)) :relative t)
		    (elliptical-arc ((,(* size (/ 35.0 80)) ,(* size (/ 100.0 80)) ,(* size (/ 40.0 80))
				      ,(* size (/ -5.0 80)) :sweep t))))
	      :fill "transparent" :stroke colour :relative t :stroke-width (* size (/ 4.0 80)))))

(defun hex-draw-terrain--draw-plains (svg x y size &optional biome)
  "Draw a plains terrain symbol on SVG at X,Y at given SIZE.
Provide BIOME to get the matching `biome-highlight-colours'."
  (let ((x2 (+ x (* 0.55 size)))
	(x1 x)
	(x3 (- x (* 0.55 size)))
	(x4 (+ (- x (* 0.55 size)) (* 0.45 size)))
	(x5 (+ x (* 0.15 size)))
	(x6 (+ (+ x (* 0.15 size)) (* 0.25 size)))
	(colour (if biome (cdr (assoc biome biome-highlight-colours)) "white")))
    (svg-line svg x1 y x2 y :stroke-color colour :stroke-width (* size (/ 3.0 80.0)))
    (svg-line svg x3 (- y (* 0.25 size)) x4 (- y (* 0.25 size)) :stroke-color colour :stroke-width (* size
												      (/ 3.0 80.0)))
    (svg-line svg x5 (- y (* 0.25 size)) x6 (- y (* 0.25 size)) :stroke-color colour :stroke-width (* size
												      (/ 3.0 80.0)))
    ))

(defun hex-draw-terrain--draw-wave (svg x y size &optional biome)
  "Draw a wave terrain symbol on SVG at X,Y at given SIZE.
Provide BIOME to get the matching `biome-highlight-colours'."
  (let ((x1 x)
	(x2 (+ x (* size 0.2)))
	(x3 (+ x (* size 0.4)))
	(y2 (- y (* size 0.1)))
	(colour (if biome (cdr (assoc biome biome-highlight-colours)) "white")))
    (svg-line svg x2 y2 x3 y :stroke-color colour :stroke-width (* size (/ 3.0 80.0)))
    (svg-line svg x1 y x2 y2 :stroke-color colour :stroke-width (* size (/ 3.0 80.0)))
    (svg-line svg (- x2 (* size 0.05)) (- y2 (* size 0.05)) x3 (- y (* size 0.05))
	      :stroke-color colour
	      :stroke-width (* size (/ 3.0 80.0)))
    (svg-line svg x1 (- y (* size 0.05)) (- x2 (* size 0.05)) (- y2 (* size 0.05))
	      :stroke-color colour
	      :stroke-width (* size (/ 3.0 80.0)))
    ))

(defun hex-draw-terrain--draw-waves (svg x y size &optional biome)
  "Draw a waves terrain symbol on SVG at X,Y at given SIZE.
Provide BIOME to get the matching `biome-highlight-colours'."
  (hex-draw-terrain--draw-wave svg x y size biome)
  (hex-draw-terrain--draw-wave svg (- x (* size 0.5)) (- y (* size 0.5)) size biome)
  (hex-draw-terrain--draw-wave svg (- x (* size 0.25)) (+ y (* size 0.5)) size biome))

(defun hex-draw-terrain--draw-mountain (svg x y size &optional biome)
  "Draw a mountain terrain symbol on SVG at X,Y at given SIZE.
Provide BIOME to get the matching `biome-highlight-colours'."
  (let ((x1 x)
	(x2 (+ x (* size 0.2)))
	(x3 (+ x (* size 0.4)))
	(y2 (- y (* size 0.4)))
	(colour (if biome (cdr (assoc biome biome-highlight-colours)) "white")))
    (svg-line svg x2 y2 x3 y :stroke-color colour :stroke-width (* size (/ 4.0 80)))
    (svg-line svg x1 y x2 y2 :stroke-color colour :stroke-width (* size (/ 4.0 80)))
    ))

(defun hex-draw-terrain--draw-mountains (svg x y size &optional biome)
  "Draw a mountains terrain symbol on SVG at X,Y at given SIZE.
Provide BIOME to get the matching `biome-highlight-colours'."
  (hex-draw-terrain--draw-mountain svg x y size biome)
  (hex-draw-terrain--draw-mountain svg (- x (* size 0.5)) (- y (* size 0.2)) size biome)
  (hex-draw-terrain--draw-mountain svg (- x (* size 0.34)) (+ y (* size 0.15)) size biome))


(defcustom terrain-draw-functions '((hills . hex-draw-terrain--draw-hills)
				    (plains . hex-draw-terrain--draw-plains)
				    (waves . hex-draw-terrain--draw-waves)
				    (mountains . hex-draw-terrain--draw-mountains)
				    (nil . hex-draw-terrain--draw-blank))
  "A list of functions for terrains and how they should be drawn."
  :group 'hexmapping)

(defun hex-draw-terrain (svg x y size &optional terrain biome)
  "Draw a specified TERRAIN on the SVG at X and Y with SIZE."
  (let ((func (cdr (assoc terrain terrain-draw-functions)))
	(unknown-func (cdr (assoc 'nil terrain-draw-functions))))
    (if func
	(apply func `(,svg ,x, y, size, biome))
      (apply unknown-func `(,svg ,x ,y ,size ,terrain)))))

(defun hex-draw-terrain-axial (svg q r size &optional canvas terrain biome offset)
  "Draw a specified TERRAIN on the SVG at Q and R with SIZE.
CANVAS is the size of the svg image, TERRAIN is the terrain to paint,
and biome determines the highlight colour, see `hex-draw-terrain'.
If OFFSET is non-nil and a dotted pair \='(X . Y) these are applied to the final hex."
  (let ((hex-coords (hexes-axial-to-cartesian q r size (/ canvas 2)))
	(offset (or offset '(0 . 0))))
	(hex-draw-terrain svg (- (car hex-coords) (car offset)) (- (cdr hex-coords) (cdr offset)) size
			  terrain biome)))


(defun hex-draw-feature (svg x y size label &optional feature)
  "Draw a specified FEATURE on the SVG at X and Y with SIZE."
  (let ((func (cdr (assoc feature feature-draw-functions)))
	(unknown-func (cdr (assoc 'nil feature-draw-functions))))
    (if func
	(apply func `(,svg ,x, y, size))
      (apply unknown-func `(,svg ,x ,y ,size ,feature)))))

(defun hex-draw-feature-axial (svg q r size index &optional feature canvas offset)
  "Draw a specified FEATURE on the SVG at hex in Q, R with SIZE.
INDEX is the nth feature this is in the hex, starting at 0.
OFFSET shifts the final result by a specified amount of pixels."
  ;; convert axial to x y
  (let ((feature-coords (hex-feature-axial-to-cartesian-coords
			 q r size index feature (/ canvas 2)))
	(offset (or offset '(0 . 0))))
    (hex-draw-feature svg (- (car feature-coords) (car offset))
		      (- (cdr feature-coords) (cdr offset)) size
		      (number-to-string index) feature))
  )

(provide 'hex-drawing)
;;; hex-drawing.el ends here
