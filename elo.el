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

;; ??? Does it work with the released auto-compile
;; or do we need this? (require 'packed)
;; ??? Can we do without this?
(require 'auto-compile)

(defvar elo-byte-compile t)

(defvar elo-ding t)


;;; Locating Files.

(defconst elo-load-suffixes '(".elc" ".el" ".elo" ".org"))

;; TODO support the optional arguments

(defun elo-locate-el (file &optional nosuffix path interactive-call)
  )

(defun elo-locate-elo (file)
  )


;;; Basic Tangling.

(defun elo-tangle-file (elo &optional el)
  (org-babel-tangle-file elo (or el (elo-locate-el elo)) "emacs-lisp")
  ;; ??? Should we optionally byte-compile and generate autoloads
  ;; here or is it okay to just depend on `auto-compile'?
  )


;;; Manual Tangling.


;;; Automatic Tangling.

(defun elo-tangle-on-load (file &optional nosuffix)
  ;; ??? should we support nosuffix?
  ;; ??? are these checks needed, why?
  ;; ??? we doesn't `org-babel-load-file' use `file-newer-than-file-p'
  ;; but it's one implementation.  Do we have to do the same?
  (when (and (stringp file)
	     (not (equal file "")))
    (let ((el  (elo-locate-el  file))
	  (elo (elo-locate-elo file nosuffix)))
      (condition-case nil
	  (when (and (file-exists-p elo)
		     (or (not (file-exists-p el))
			 (file-newer-than-file-p elo el)))
	    (message "Retangling %s..." elo)
	    (elo-tangle-file elo el)
	    (message "Retangling %s...done" elo))
	(error
	 (message "Retangling %s...failed" elo)
	 (elo-ding))))))


;;; Elo Load Variants.

;; ??? Exactly like `load-file', do we need to change anything?
(defun elo-load-file (file)
  "Load the Lisp file named FILE."
  ;; This is a case where .elc makes a lot of sense.
  (interactive (list (let ((completion-ignored-extensions
			    (remove ".elc" completion-ignored-extensions)))
		       (read-file-name "Load file: "))))
  (load (expand-file-name file) nil nil t))

(defun elo-load (file &optional noerror nomessage nosuffix must-suffix)
  )

(defun elo-require (feature &optional filename noerror path)
  (elo-tangle-on-load (or filename (symbol-name feature)))
  (require feature filename noerror))


;;; Elo-Tangle-On-Load Mode.

(define-minor-mode elo-tangle-on-load-mode
  "Before loading a library retangle it if it needs retangling."
  :lighter elo-on-load-mode-lighter
  :group 'elo
  :global t
  (if elo-on-load-mode
      (progn
	(ad-enable-advice 'load    'before 'elo-tangle-on-load)
	(ad-enable-advice 'require 'before 'elo-tangle-on-load)
	(ad-activate 'load)
	(ad-activate 'require))
    (ad-disable-advice 'load    'before 'elo-tangle-on-load)
    (ad-disable-advice 'require 'before 'elo-tangle-on-load)))

(defvar elo-tangle-on-load-mode-lighter "elo"
  "Mode lighter for Elo-Tangle-On-Load Mode.")

(defadvice load (before elo-tangle-on-load disable)
  ;; (file &optional noerror nomessage nosuffix must-suffix)
  "Before loading the library retangle it if needs to be retangled."
  (elo-tangle-on-load file nosuffix))

(defadvice require (before elo-tangle-on-load disable)
  ;; (feature &optional FILENAME NOERROR)
  "Before loading the library retangle it if needs to be retangled."
  (elo-tangle-on-load (or filename (symbol-name feature))))


;;; Utilities.

(defun elo-ding ()
  (when elo-ding (ding)))


;;; Trash.

;; XXX tempory
(defun etc/require (feature)
  (let ((full-feature (intern (concat "etc/" (symbol-name feature)))))
    (unless (featurep full-feature)
      (org-babel-load-file
       (expand-file-name
	(convert-standard-filename
	 (concat (symbol-name feature) ".org"))
	epkg-etc-directory)))
    full-feature))

(provide 'elo)
;;; elo.el ends here
