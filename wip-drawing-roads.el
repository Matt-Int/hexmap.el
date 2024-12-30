(require 'hexes)
(require 'hex-drawing)
(require 'hexmap-mode)

(let ((svg (svg-create 400 400)))
  (hex-draw-axial svg 0 0 80 400 "green")
  (hex-draw-road svg 200 200 80 4 0 "blue" 4)
  (hex-draw-road svg 200 200 80 0 3 "#333333" 4)
  (hex-draw-feature-axial svg 0 0 80 0 'village 400)
  (dotimes (i 10)
    (hex-draw-feature-axial svg 0 0 80 (1+ i) nil 400))
(with-current-buffer (get-buffer-create "*SVG Image: Testing*")
    (erase-buffer)
    (insert-image (svg-image svg))
    (display-buffer "*SVG Image: Testing*")))



