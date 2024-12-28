(require 'svg)

(defun draw-hex (svg x y size fill &optional label)
  (svg-polygon svg (hex-corners x y size)
	       :stroke-color "none" :fill-color fill)
  (svg-text svg (or label "") :x x :y y :font-size (/ size 2) :text-anchor "middle"))

(defun draw-hex-axial (svg q r size &optional canvas fill label)
  (let ((coords (axial-to-cartesian q r size))
	(center (or canvas 0))
	(fill (or fill "pink")))
    (draw-hex svg
	      (+ (/ center 2) (car coords))
	      (+ (/ center 2) (cdr coords))
	      size
	      fill label)))
