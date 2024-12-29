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

(defun hex-draw (svg x y size &optional fill stroke label)
  "Draw a hex in SVG with center of X, Y and specified SIZE.
Optionally provide the FILL, STROKE, and LABEL for the hex."
  (let ((fill (or fill "pink"))
	(label (or label ""))
	(stroke (or stroke "transparent")))
    (svg-polygon svg (hexes-flat-corners x y size)
		 :stroke-color stroke :fill-color fill)
    (svg-text svg label :x x :y y :font-size (/ size 2) :text-anchor "middle")))


(defun hex-draw-axial (svg q r size &optional canvas fill stroke label)
  "Draw a hex on an SVG, accepts axial Q and R coordinates.
Needs to provide the SIZE for converting to cartesian coordinates.
CANVAS can be provided to calculate the center of 0,0.
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
	(fill (or fill "pink")))
    (hex-draw svg
	      (+ (/ center 2) (car coords))
	      (+ (/ center 2) (cdr coords))
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
	  :stroke colour :fill "transparent" :stroke-width width)))

(defun hex-draw-axial-road (svg q r size start end &optional center colour width)
  "Use Q and R axial coordinates to draw a road on an SVG hex with a given SIZE.
Road is specified using START and END which is 0->5 starting with 0 at top,
and incrementing by one going clockwise."
  (let ((cartesian (hexes-axial-to-cartesian q r size center)))
    (hex-draw-road svg (car cartesian) (cdr cartesian) size start end colour width)))


(defun hex-draw-feature--draw-unknown (svg x y size &optional feature)
  "Draw a generic feature icon on SVG at X, Y with specified SIZE."
  (if feature
      (user-error (format "Feature: %s does not have a draw-function" feature)))
  (svg-circle svg x y
	      (/ size 20) :stroke-color "black" :stroke-width (* size (/ 3.0 80.0))))

(defun hex-draw-feature--draw-village (svg x y size)
  "Draw a village icon on SVG at X, Y with specified SIZE."
  (svg-rectangle svg
		 (- x (/ (/ size 5) 2))
		 (- y (/ (/ size 5) 2))
		 (/ size 5) (/ size 5) :stroke-color "black" :stroke-width (* size (/ 3.0 80.0))))


(defgroup hexmapping nil
  "Specifying hexmaps for ttrpgs.")

(defcustom feature-draw-functions '((village . hex-draw-feature--draw-village)
				    (nil . hex-draw-feature--draw-unknown))
  "A list of functions for features and how they should be drawn."
  :group 'hexmapping)

(defun hex-draw-feature (svg x y size &optional feature)
  "Draw a specified FEATURE on the SVG at X and Y with SIZE."
  (let ((func (cdr (assoc feature feature-draw-functions)))
	(unknown-func (cdr (assoc 'nil feature-draw-functions))))
    (if func
	(apply func `(,svg ,x, y, size))
      (apply unknown-func `(,svg ,x ,y ,size ,feature)))))

(provide 'hex-drawing)
;;; hex-drawing.el ends here
