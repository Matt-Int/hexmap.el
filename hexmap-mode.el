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
  (let ((start (save-excursion
		 (end-of-line)
		 (search-backward-regexp "-?\\([0-9]\\)+,-?\\([0-9]\\)+" nil t)))
	(end (save-excursion
	       (search-forward "}" nil t))))
    (if (and start end)
	(progn
	  (set-mark start)
	  (goto-char end)))))

(defun hexmap-goto-next ()
  "Go to next hex entry."
  (interactive)
  (unless (search-forward-regexp "-?\\([0-9]\\)+,-?\\([0-9]\\)+" nil t)
    (user-error "No more hexes")))

(defun hexmap-goto-previous ()
  "Go to previous hex entry."
  (interactive)
  (unless (search-backward-regexp "-?\\([0-9]\\)+,-?\\([0-9]\\)+" nil t)
    (user-error "No more hexes")))

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
			    ("-?\\([0-9]\\)+,-?\\([0-9]\\)+" . font-lock-type-face)))
  
  ;; Keybinds
  (local-set-key (kbd "C-c C-c") #'hexmap-mark-hex-at-point)
  ;; defaults
  (setq-local indent-line-function #'hexmap-mode-indent-line)
  (setq-local tab-width 4)
  )

(add-to-list 'auto-mode-alist '("\\.hexmap" . hexmap-mode))
(provide 'hexmap-mode)
;;; hexmap-mode.el ends here

