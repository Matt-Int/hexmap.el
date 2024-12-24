(require 'svg)

(defun flat-hex-corner (x y size i)
       `(,(+ x (* size (cos (degrees-to-radians (* 60 i))))) . 
	 ,(+ y (* size (sin (degrees-to-radians (* 60 i)))))))

(defun hex-corners (x y size)
(mapcar #'(lambda (i) (flat-hex-corner x y size i))
	(number-sequence 0 5)))

(defun draw-hex (svg x y size fill &optional label)
  (svg-polygon svg (hex-corners x y size)
	       :stroke-color "none" :fill-color fill)
  (svg-text svg (or label "") :x x :y y :font-size (/ size 2) :text-anchor "middle"))

(defun axial-s-component (q r)
  (- (* -1 q) r))

(defun axial-to-cartesian (q r size &optional center)
  (let ((center (or center 0)))
  `(,(+ center (* size (* (/ 3.0 2) q))) .
    ,(+ center (* size (+ (* (/ (sqrt 3) 2) q) (* (sqrt 3) r)))))))
  

(axial-to-cartesian -2 0 100)

(defun draw-hex-axial (svg q r size &optional canvas fill label)
  (let ((coords (axial-to-cartesian q r size))
	(center (or canvas 0))
	(fill (or fill "pink")))
    (draw-hex svg
	      (+ (/ center 2) (car coords))
	      (+ (/ center 2) (cdr coords))
	      size
	      fill label)))


(defun parse-hexmap-file ()
  (with-temp-buffer
    (insert-file-contents "./example.hexmap")
    (let ((map-spec (buffer-substring-no-properties (point-min) (point-max))))
      (split-string map-spec "\n"))))

(parse-hexmap-file)

(mapcar #'parse-hexmap-line (parse-hexmap-file))

(defun parse-hexmap-line (tile-spec)
  (if (string-match "\\(-?[0-9]+\\),\\(-?[0-9]+\\) \\([A-z]+\\) \\([a-z]+\\)" tile-spec)
      `((,(string-to-number (match-string 1 tile-spec)) . ; q coordinate
	 ,(string-to-number (match-string 2 tile-spec)))  ; r coordinate
	,(match-string 3 tile-spec) ; label
	,(match-string 4 tile-spec)) ; colour
    nil))

(defun parse-hexmap-roads (tile-spec)
  (if (string-match-p "-?[0-9]+,-?[0-9]+->-?[0-9]+,-?[0-9]+" tile-spec)
      (mapcar #'(lambda (coord) (mapcar #'string-to-number (string-split coord ","))) (string-split tile-spec "->"))))


(defun draw-hexmap (hexmap-file)
  "Go through each line in the file and draw the corresponding hex."
  (let ((tiles (mapcar #'parse-hexmap-line (parse-hexmap-file))))
    ))

(defun draw-line-axial (svg q1 r1 q2 r2 size &optional canvas color)
  (let ((coords-start (axial-to-cartesian q1 r1 size))
	(coords-end (axial-to-cartesian q2 r2 size))
	(center (or canvas 0))
	(fill (or color "brown")))
    (svg-line svg
	      (+ (/ center 2) (car coords-start))
	      (+ (/ center 2) (cdr coords-start))
	      (+ (/ center 2) (car coords-end))
	      (+ (/ center 2) (cdr coords-end))
	      size
	      color)))


(let ((tiles (remove nil (mapcar #'parse-hexmap-line (parse-hexmap-file))))
      (roads (remove nil (mapcar #'parse-hexmap-roads (parse-hexmap-file))))
      (svg (svg-create 400 400)))
  (mapc #'(lambda (tile)
	    (let ((coords (car tile))
		  (color (car (last tile)))
		  (label (car (cdr tile))))
	      (draw-hex-axial svg (car coords) (cdr coords) 40 400 color label)))
	tiles)
  (with-current-buffer (get-buffer-create "*SVG Image: Testing*")
    (erase-buffer)
    (insert-image (svg-image svg))
    (display-buffer "*SVG Image: Testing*")))


(defun get-path-for-road (road)
  (let ((segment)
	(path))
    (while (cdr road)
      (setq segment (append (car road) (car (cdr road))))
      (if path
	  (setq path `(,path ,segment))
	(setq path segment))
      
      (setq path (delete nil path))
      (setq road (cdr road)))
    path))

(get-path-for-road '((0 0) (1 1)))

;; ((0 0) (1 1) (1 0)) -> ((0 0 1 1) (1 1 1 0))

(get-path-for-road (mapcar #'(lambda (road) `(,(car road) ,(cdr road)))
	(mapcar #'(lambda (coord)
		    (axial-to-cartesian
		     (car coord)
		     (car (cdr coord))
		     20))
		(parse-hexmap-roads "0,0->1,1->14,2"))))

(defun draw-path-for-road (svg road &optional canvas)
  (let ((path (get-path-for-road (mapcar #'(lambda (road) `(,(car road) ,(cdr road)))
					 (mapcar #'(lambda (coord)
						     (axial-to-cartesian
						      (car coord)
						      (car (cdr coord))
						      20))
						 road))))
	(center (or canvas 0)))
    (message "PATH: %s" path)
    (svg-path svg `((moveto ((100 . 100)))
                (smooth-curveto (,path)))
          :fill "green" :stroke "blue")))

(let ((svg (svg-create 400 400)))
  (draw-path-for-road svg (parse-hexmap-roads "0,0->1,0->1,1")))
  
(let ((svg (svg-create 200 200)))
  (draw-path-for-road svg (parse-hexmap-roads "0,0->1,1->2,2") 400)
  (with-current-buffer (get-buffer-create "*SVG Image: Testing*")
    (erase-buffer)
    (insert-image (svg-image svg))
    (display-buffer "*SVG Image: Testing*")))


(let ((tiles (remove nil (mapcar #'parse-hexmap-line (parse-hexmap-file))))
      (roads (remove nil (mapcar #'parse-hexmap-roads (parse-hexmap-file))))
      (svg (svg-create 400 400)))
  (mapc #'(lambda (tile)
	    (let ((coords (car tile))
		  (color (car (last tile)))
		  (label (car (cdr tile))))
	      (draw-hex-axial svg (car coords) (cdr coords) 40 400 color label)))
	tiles)
  (mapc #'(lambda (road)))
  (with-current-buffer (get-buffer-create "*SVG Image: Testing*")
    (erase-buffer)
    (insert-image (svg-image svg))
    (display-buffer "*SVG Image: Testing*")))



(let ((tiles (remove nil (mapcar #'parse-hexmap-line (parse-hexmap-file))))
      (roads (remove nil (mapcar #'parse-hexmap-roads (parse-hexmap-file))))
      (svg (svg-create 400 400)))
  (mapc #'(lambda (road) (draw-path-for-road svg road)) roads)
  (with-current-buffer (get-buffer-create "*SVG Image: Testing*")
    (erase-buffer)
    (insert-image (svg-image svg))
    (display-buffer "*SVG Image: Testing*")))
