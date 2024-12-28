(require 'hexes)
(require 'hex-drawing)
(require 'hexmap-mode)


(hexes-flat-corner 100 100 10 0)

(defun hexes-flat-side-midpoint (x y size i)
  (let ((corner-0 (hexes-flat-corner x y size (- i 2)))
	(corner-1 (hexes-flat-corner x y size (1+ (- i 2)))))
    `(,(/ (+ (car corner-0) (car corner-1)) 2) .
      ,(/ (+ (cdr corner-0) (cdr corner-1)) 2))))


(defun hex-road-coords (x y size start end)
  (let ((midpoint-start (hexes-flat-side-midpoint x y size start))
	(midpoint-end (hexes-flat-side-midpoint x y size end)))
    `((,(car midpoint-start) . ,(cdr midpoint-start))
      (,x ,y ,x ,y ,(car midpoint-end) ,(cdr midpoint-end)))))

(hex-road-coords 200 200 80 0 2)

(defun hex-draw-road (x y size start end &optional colour)
  (let ((colour (or colour "brown")))
    ))

(car (hex-road-coords 200 200 80 0 4))


;;; smooth-curveto
(let ((svg (svg-create 400 400))
      (midpoint-0 (hexes-flat-side-midpoint 200 200 80 0))
      (midpoint-1 (hexes-flat-side-midpoint 200 200 80 1))
      (midpoint-2 (hexes-flat-side-midpoint 200 200 80 2))
      (midpoint-3 (hexes-flat-side-midpoint 200 200 80 3))
      (midpoint-4 (hexes-flat-side-midpoint 200 200 80 4))
      (midpoint-5 (hexes-flat-side-midpoint 200 200 80 5)))
  (svg-circle svg (car midpoint-0) (cdr midpoint-0) 3 :fill "orange")
  (svg-circle svg (car midpoint-1) (cdr midpoint-1) 3 :fill "green")
  (svg-circle svg (car midpoint-2) (cdr midpoint-2) 3 :fill "purple")
  (svg-circle svg (car midpoint-3) (cdr midpoint-3) 3 :fill "green")
  (svg-circle svg (car midpoint-4) (cdr midpoint-4) 3 :fill "orange")
  (svg-circle svg (car midpoint-5) (cdr midpoint-5) 3 :fill "purple")

(svg-path svg `((moveto ((,(car midpoint-0) . ,(cdr midpoint-0))))
		(smooth-curveto ((
			   200 200
			   ,(car midpoint-4) ,(cdr midpoint-4)))))
	  :stroke "orange" :fill "transparent")
(svg-path svg `((moveto ((,(car midpoint-1) . ,(cdr midpoint-1))))
		(smooth-curveto ((
			   200 200
			   ,(car midpoint-3) ,(cdr midpoint-3)))))
	  :stroke "green" :fill "transparent")
(svg-path svg `((moveto ((,(car midpoint-2) . ,(cdr midpoint-2))))
		(smooth-curveto ((
			   200 200
			   ,(car midpoint-5) ,(cdr midpoint-5)))))
	  :stroke "purple" :fill "transparent")
(svg-path svg `((moveto ((,(car midpoint-0) . ,(cdr midpoint-0))))
		(smooth-curveto ((
			   200 200
			   ,(car midpoint-1) ,(cdr midpoint-1)))))
	  :stroke "blue" :fill "transparent")
(svg-path svg `((moveto (,(car (hex-road-coords 200 200 80 1 3))))
		(smooth-curveto ,(cdr (hex-road-coords 200 200 80 1 3))))
	  :stroke "brown")
(with-current-buffer (get-buffer-create "*SVG Image: Testing*")
    (erase-buffer)
    (insert-image (svg-image svg))
    (display-buffer "*SVG Image: Testing*")))
