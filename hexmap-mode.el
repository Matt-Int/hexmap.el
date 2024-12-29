;; hexmap-mode.el --- A major mode for working with `.hexmap' files
;; Author: Mattias <mattias1126@protonmail.com>
;; Maintainer: Mattias <mattias1126@protonmail.com>
;; Keywords: hexes mapping ttrpg
;; Version 0.1.0
;; URL: <todo: include url>
;; Package-Requires: ((emacs "29.0.50") (svg "1.1"))

;;; Commentary:

;; Provides a major mode for reading `.hexmap' files and additional functionality for parsing these programmatically.

;;; Code:

(defun hexmap-mark-hex-at-point ()
  "Mark the hex specification at point."
  (interactive)
  (deactivate-mark)
  (let ((start (save-excursion
		 (end-of-line)
		 (search-backward-regexp "^-?[0-9]+,-?[0-9]+" nil t)))
	(end (save-excursion
	       (search-forward "}" nil t))))
    (if (and start end)
	(progn
	  (set-mark start)
	  (goto-char end)))))

(defun hexmap-goto-next ()
  "Go to next hex entry."
  (interactive)
  (search-forward-regexp "-?\\([0-9]\\)+,-?\\([0-9]\\)+" nil t))

(defun hexmap-goto-previous ()
  "Go to previous hex entry."
  (interactive)
  (unless (search-backward-regexp "-?\\([0-9]\\)+,-?\\([0-9]\\)+" nil t)
    (user-error "No more hexes")))

(defun hexmap--extract-axial-coords (hex)
  "Extract the axial coords from a HEX string."
  (if (string-match "\\(-?[0-9]+\\)+,\\(-?[0-9]+\\)+" hex)
      (let ((axial-q (string-to-number (match-string 1 hex)))
	    (axial-r (string-to-number (match-string 2 hex))))
	`(,axial-q . ,axial-r))))

(defun hexmap--extract-keyword (hex keyword &optional extract-list)
  "Extract KEYWORD from HEX string.  If EXTRACT-LIST is nil, treat as single value."
  (if (string-search keyword hex)
      (if extract-list
	  (let ((hex (with-temp-buffer
		       (insert hex)
		       (goto-char (point-min))
		       (search-forward keyword)
		       (search-forward "[")
		       (forward-char)
		       (backward-up-list)
		       (mark-sexp)
		       (buffer-substring-no-properties (1- (mark)) (1+ (point))))))
	    (mapcar #'(lambda (item) (string-trim item))
		    (remove "" (split-string (replace-regexp-in-string "\\(\n\\|\t\\)" "" hex) ","))))
	(if (string-match (format "%s: ?\\(.*\\)," keyword) hex)
	    (match-string 1 hex)))))
  
(defun hexmap--extract-roads (hex &optional rivers)
  "Extract road specifications from HEX.
Optionally set RIVERS to non-nil to parse rivers instead."
  (let ((roads (hexmap--extract-keyword hex (if rivers "rivers" "roads") t)))
    (mapcar #'(lambda (road)
		(apply #'cons
		       (mapcar #'(lambda (item) (if (or (equal item "0") (> (string-to-number item) 0))
					       (string-to-number item)
					       (intern item)))
			       (string-split road "->"))))
	    roads)))

(defun hexmap--parse-hex (hex)
  "Parse a HEX specification to construct a Lisp data structure."
  (let ((hex (replace-regexp-in-string "//.*" "" hex)))
    (let ((axial-coords (hexmap--extract-axial-coords hex))
	  (terrain (hexmap--extract-keyword hex "terrain"))
	  (label (replace-regexp-in-string "\"" "" (hexmap--extract-keyword hex "label")))
	  (features (hexmap--extract-keyword hex "features" t))
	  (roads (hexmap--extract-roads hex))
	  (rivers (hexmap--extract-roads hex t)))
      `(:axial-coords ,axial-coords :terrain ,(intern terrain) :label ,label
		      :features ,(mapcar #'intern features)
		      :roads ,roads
		      :rivers ,rivers))))

(defun hexmap-parse-buffer ()
  "Parse the current buffer into a list."
  (interactive)
  (let ((start (point-min))
	(end (point-max))
	(current (point))
	(result))
    (goto-char start)
    (while (hexmap-goto-next)
      (hexmap-mark-hex-at-point)
      (let ((hex (buffer-substring-no-properties (mark) (point))))
	(setq result (append result `(,(hexmap--parse-hex hex))))))
    (deactivate-mark)
    (goto-char current)
    result))

(defun hexmap-visualise-buffer ()
  "Parse the current buffer and produces an SVG."
  (interactive)
  (let ((map (hexmap-parse-buffer))
	(svg (svg-create 800 800)))
    (mapc #'(lambda (hex)
	      ;; function to draw main hex here
	      (hex-draw-axial svg
			      (car (plist-get hex :axial-coords))
			      (cdr (plist-get hex :axial-coords))
			      30
			      800
			      "green"
			      "transparent")
	      ;; function to draw roads here
	      (mapc #'(lambda (road)
			(unless (or (symbolp (car road)) (symbolp (cdr road)))
			  (hex-draw-axial-road svg
					       (car (plist-get hex :axial-coords))
					       (cdr (plist-get hex :axial-coords))
					       30
					       (car road)
					       (cdr road) 400))
			)
		    (plist-get hex :roads)))
	  map)
    (with-current-buffer (get-buffer-create "*Hexmap: SVG*")
      (image-mode)
      (erase-buffer)
      (insert-image (svg-image svg))
      (display-buffer "*Hexmap: SVG*"))))

(defvar hexmap-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; use {} for encapsulating blocks.
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    ;; Only use "" for strings, not ''.
    (modify-syntax-entry ?\" "\"" st)
    ;; Comments are //
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\^m "> b" st)
    st))

(defun hexmap-mode-indent-line ()
  "Indent current line."
  (let (indent boi-p move-eol-p (point (point)))
    (save-excursion
      (back-to-indentation)
      (setq indent (car (syntax-ppss))
	    boi-p (= point (point)))
      ;; don't indent empty lines
      (when (and (eq (char-after) ?\n)
		 (not boi-p))
	(setq indent 0))
      ;; check if we want to move to end of line
      (when boi-p
	(setq move-eol-p t))
      ;; decrement the indent if the first character on the line is a
      ;; closer.
      (when (eq (char-after) ?\})
	(setq indent (1- indent)))
      ;; indent the line
      (delete-region (line-beginning-position)
		     (point))
      (indent-to (* tab-width indent)))
    (when move-eol-p
     (move-end-of-line nil))))


;;;###autoload
(define-derived-mode hexmap-mode fundamental-mode "Hexmap"
  "A major mode for working with hexmaps."
  :syntax-table hexmap-mode-syntax-table
  ;; Syntax highlighting
  (font-lock-add-keywords nil
			  '(("terrain:" . font-lock-doc-face)
			    ("rivers:" . font-lock-doc-face)
			    ("label:" . font-lock-string-face)
			    ("features:" . font-lock-type-face)
			    ("roads:" . font-lock-type-face)
			    ("[0-7]->[0-7]" . font-lock-constant-face)
			    ("[0-7]->[a-z]+" . font-lock-constant-face)
			    ("[a-z]+->[0-7]" . font-lock-constant-face)
			    ("[a-z]+->[a-z]+" . font-lock-constant-face)
			    ("-?\\([0-9]\\)+,-?\\([0-9]\\)+" . font-lock-type-face)))
  
  ;; Keybinds
  (local-set-key (kbd "C-c C-c") #'hexmap-visualise-buffer)
  (local-set-key (kbd "C-c C-n") #'hexmap-goto-next)
  (local-set-key (kbd "C-c C-p") #'hexmap-goto-previous)
  ;; defaults
  (setq-local indent-line-function #'hexmap-mode-indent-line)
  (setq-local tab-width 4)
  )

(add-to-list 'auto-mode-alist '("\\.hexmap" . hexmap-mode))
(provide 'hexmap-mode)
;;; hexmap-mode.el ends here

