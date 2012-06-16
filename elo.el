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

;;; Code:

(defun elo-require (feature &optional filename noerror path)
  )

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
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; tarsius.el ends here
