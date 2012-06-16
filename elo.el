;;; elo.el --- [proof of concept] a require that tangles

;; Copyright (C) 2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20120616
;; Version: 0.0.1
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

;; Elo makes literate elisp programming easier.  Basically it adds
;; support for a third kind of elisp library files besides .el and
;; .elc: .elo.

;;; Code:

;; Civil Disobedience
;; or we might just not use these: `mapcan'
(require 'cl)

(require 'ob-tangle)

(declare-function org-infile-export-plist "org-exp")

(add-to-list 'auto-mode-alist '("\\.elo\\'" . org-mode))

(defgroup elo nil
  "Support for Org Emacs Lisp files.
Org Emacs Lisp files (.elo) are a third kind of Emacs Lisp files
besides source files (.el) and byte compiled files (.elc)."
  :group 'convenience)

(defcustom elo-ding t
  "Whether to beep (or flash the screen) when an error occurs.

Elo-Auto-Tangle mode continues after an errors occurs because
aborting and therefor not processing the remaining files would be
confusing.  Instead it continues and beeps or flashes the screen
to get the users attention; set this variable to nil to disable
even that."
  :group 'elo
  :type 'boolean)

(defconst elo-load-suffixes '(".elo"))

(defun elo-load-suffixes ()
  (let ((load-suffixes elo-load-suffixes))
    (get-load-suffixes)))

(defun elo-load-suffix-match-p (file)
  (save-match-data
    (string-match (concat (regexp-opt (elo-load-suffixes)) "\\'") file)))

(defun elo-locate-elo (library &optional nosuffix path)
  "Show the precise file name of Org Emacs library LIBRARY.
If no such library exists or if it is shadowed by an .el or .elc
file in a directory earlier in `load-path' return nil."
  (when (or (not nosuffix)
            (elo-load-suffix-match-p library))
    (let* ((load-suffixes (append elo-load-suffixes load-suffixes))
           (file (locate-file library
                              (or path load-path)
                              (append (unless nosuffix (get-load-suffixes))
                                      load-file-rep-suffixes))))
      (and file
           (elo-load-suffix-match-p file)
           file))))

(defun elo-tangle-file (elo &optional el)
  (interactive "fFile to tangle: \nP")
  (let ((visited-p (get-file-buffer (expand-file-name elo)))
	to-be-removed)
    (save-window-excursion
      (find-file elo)
      (setq to-be-removed (current-buffer))
      (elo-tangle elo el))
    (unless visited-p
      (kill-buffer to-be-removed))))

(defun elo-tangle (&optional elo el)
  (require 'org-exp)
  (unless elo
    (setq elo (buffer-file-name)))
  (unless el
    (setq el (packed-source-file elo)))
  (let* ((org-export-inbuffer-options-extra '(("PROVIDE" :provide)))
         (feature (plist-get (org-infile-export-plist) :provide))
         (blocks
          (mapcan (lambda (b)
                    (when (string= (cdr (assoc :tangle (nth 4 b))) "yes")
                      (list b)))
                  (cdar (org-babel-tangle-collect-blocks "emacs-lisp")))))
    (if (not blocks)
        (ignore-errors (delete-file el))
      (with-temp-file el
        (emacs-lisp-mode)
        (insert (format "\
;;; %s ---

;; This file was tangled (generated) from ./%s.
;;
;; DO NOT EDIT.  Instead edit ./%s.
"
                        (file-name-nondirectory el)
                        (file-name-nondirectory elo)
                        (file-name-nondirectory elo)))
        (mapc 'org-babel-spec-to-string blocks)
        (when feature
          (insert (format "\n(provide '%s)\n" (read feature))))))))

;;;###autoload
(define-minor-mode elo-auto-tangle-mode
  "Tangle Org Emacs Lisp files after the visiting buffers are saved.

After a buffer visiting an Org Emacs Lisp file (.elo) is saved
update the respective source file (.el).

This mode should be enabled globally, using it's globalized
variant `elo-auto-tangle-global-mode'."
  :lighter elo-auto-tangle-mode-lighter
  :group 'elo
  (if elo-auto-tangle-mode
      (add-hook  'after-save-hook 'elo-tangle nil t)
    (remove-hook 'after-save-hook 'elo-tangle t)))

;;;###autoload
(define-globalized-minor-mode elo-auto-tangle-global-mode
  elo-auto-tangle-mode turn-on-elo-auto-tangle-mode)

(defun turn-on-elo-auto-tangle-mode ()
  (and (eq major-mode 'org-mode)
       buffer-file-name
       (string-match "\\.elo\\'" buffer-file-name)
       (elo-auto-tangle-mode 1)))

(defvar elo-auto-tangle-mode-lighter " Elo"
  "Mode lighter for Elo-Auto-Tangle Mode.")

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
  (elo-tangle-on-load file nosuffix must-suffix))

(defadvice require (before elo-tangle-on-load disable)
  ;; (feature &optional FILENAME NOERROR)
  "Before loading the library retangle it if needs to be retangled."
  (elo-tangle-on-load (or filename (symbol-name feature))))

(defun elo-tangle-on-load (file &optional nosuffix must-suffix)
  (when (or (and (not nosuffix)
                 (not must-suffix))
            (elo-load-suffix-match-p file))
    (let ((elo (elo-locate-elo file nosuffix))
          el)
      (when (and elo
                 (setq el (packed-source-file elo))
                 (or (not (file-exists-p el))
                     (file-newer-than-file-p elo el)))
        (condition-case nil
            (elo-tangle-file elo el)
          (error
           (message "Cannot tangle on load: %s" elo)
           (elo-ding)))))))

(defun elo-ding ()
  (and elo-ding (ding)))

(provide 'elo)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; elo.el ends here
