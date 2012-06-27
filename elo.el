;;; elo.el --- [PRE-ALPHA] a require that tangles

;; Copyright (C) 2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20120616
;; Version: 0.0.1 (pre-alpha)
;; Homepage: http://tarsius.github.com/elo
;; Keywords: compile, convenience, lisp

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; * Prelude

;; This is a pre-release.  Version numbers are inspired by how
;; Emacs is versioned - 1.1.0 will be the first stable version.
;; Actually before 1.0.0 you shouldn't be using this at all.

;; * Introduction

;; TODO

;;; Code:

(require 'auto-compile)
(require 'ob-tangle)

(defvar elo-ding t)

(defvar elo-load-suffixes '(".elo"))

(defvar elo-byte-compile t)

(defun elo-file-p (file)
  (string-match (concat (regexp-opt elo-load-suffixes) "$") file))

(defun elo-locate-elo (library &optional path)
  (locate-file (substitute-in-file-name library)
	       (or path load-path)
	       (let ((load-suffixes elo-load-suffixes))
		 (get-load-suffixes))))

(defun elo-locate-elo* (library &optional path)
  (let* ((load-suffixes (cons ".elo" load-suffixes))
	 (result (locate-library library)))
    (when (elo-file-p result)
      result)))

(defun elo-tangle-file (elo &optional el)
  (org-babel-tangle-file elo (or el (packed-source-file elo)) "emacs-lisp"))

(defun elo-tangle-file*** (elo &optional nomessage noerror el)
  (unless el
    (setq el (packed-source-file elo)))
  (when (or (not (file-exists-p el))
	    (file-newer-than-file-p elo el))
    (message "Loading %s (tangle)..." elo)
    (org-babel-tangle-file elo el "emacs-lisp")
    (message "Loading %s (tangle)...done" elo))
  el)



;;;###autoload
(define-minor-mode elo-auto-tangle-mode
  ""
  :lighter elo-auto-tangle-mode-lighter
  :group 'elo
  (if elo-auto-tangle-mode
      (add-hook  'after-save-hook 'elo-auto-tangle-file nil t)
    (remove-hook 'after-save-hook 'elo-auto-tangle-file t))
  ;; TODO mode-line
  )

;;;###autoload
(define-globalized-minor-mode elo-auto-tangle-global-mode
  elo-auto-tangle-mode turn-on-elo-auto-tangle-mode)

(defun turn-on-elo-auto-tangle-mode ()
  (and (eq major-mode 'org-mode)
       nil ; TODO .elo suffix
       (elo-auto-tangle-mode 1)))

(defvar elo-auto-tangle-mode-lighter " Elo" ; TODO none once done
  "Mode lighter for Elo-Auto-Tangle Mode.")

;; TODO all kinds of error handling
(defun elo-auto-tangle-file ()
  (let ((elo (buffer-file-name)))
    (elo-tangle-file elo)))



;; FIXME only suitable for require not load at the moment
(defun elo-tangle-on-load (file &optional nosuffix path)
  ;; To keep it simple we don't support files with no suffix.
  (unless nosuffix
    (let* ((elo (elo-locate-elo file path))
	   (el (and elo (packed-source-file elo))))
      ;; (message "elo-tangle-on-load")
      ;; (message "  file: %s" file)
      ;; (message "  el:   %s" el)
      ;; (message "  elo:  %s" elo)
      (when (and elo
		 (file-exists-p elo)
		 (or (not (file-exists-p el))
		     (file-newer-than-file-p elo el)))
	(condition-case nil
	    (progn
	      ;; TODO use a tempory file in case of failure?
	      (message "Retangling %s..." elo)
	      (elo-tangle-file elo el)
	      (message "Retangling %s...done" elo))
	  (error
	   (message "Retangling %s...failed" elo)
	   (elo-ding)))
	(auto-compile-byte-compile el elo-byte-compile)))))

(defun elo-load (file &optional noerror nomessage)
  (load (elo-tangle-on-load file) noerror nomessage))

(defun elo-require (feature &optional filename noerror path)
  (elo-tangle-on-load (or filename (symbol-name feature)))
  (require feature filename noerror))

(define-minor-mode elo-tangle-on-load-mode
  "Before loading a library retangle it if it needs retangling."
  :lighter elo-on-load-mode-lighter
  :group 'elo
  :global t
  (if elo-tangle-on-load-mode
      (progn
	(ad-enable-advice 'load    'before 'elo-tangle-on-load)
	(ad-enable-advice 'require 'before 'elo-tangle-on-load)
	(ad-activate 'load)
	(ad-activate 'require))
    (ad-disable-advice 'load    'before 'elo-tangle-on-load)
    (ad-disable-advice 'require 'before 'elo-tangle-on-load)))

(defvar elo-tangle-on-load-mode-lighter ""
  "Mode lighter for Elo-Tangle-On-Load Mode.")

(defadvice load (before elo-tangle-on-load disable)
  ;; (file &optional noerror nomessage nosuffix must-suffix)
  "Before loading the library retangle it if needs to be retangled."
  (elo-tangle-on-load file nosuffix))

(defadvice require (before elo-tangle-on-load disable)
  ;; (feature &optional FILENAME NOERROR)
  "Before loading the library retangle it if needs to be retangled."
  (elo-tangle-on-load (or filename (symbol-name feature))))

(defun elo-ding ()
  (and elo-ding (ding)))

(provide 'elo)
;;; elo.el ends here
