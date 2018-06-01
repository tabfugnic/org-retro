;;; org-retro.el --- Make retros easier to run

;; Author: Eric J. Collins <eric@tabfugni.cc>
;; Version: 0.1.0
;; Keywords: org, retro
;; URL: https://github.com/thoughtbot/org-retro/org-retro.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; 'org-retro' is software that helps you take full advantage of org
;; mode while giving you more control on how it is presented.  This is
;; heavily opinionated on how I personally run retrospectives.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

; Customizable variables
(defgroup org-retro nil
  "Settings for retrospectives."
  :version "0.1.0"
  :group 'applications)

(defcustom org-retro-presentation-scale 4
  "Set presentation height that allows better viewing on small screens."
  :type 'integer
  :group 'retro)

(define-derived-mode org-retro-mode org-mode "Retrospective"
  "Mode for making retrospectives easier to run")

(defun org-retro-increment-number-inline (&optional number)
  (interactive)
  (or number (setq number 1))
  (let ((current-point (point)))
    (end-of-line)
    (skip-chars-backward "+0-9")
    (backward-char)
    (if (looking-at " \\+[0-9]+$")
        (replace-match
         (format " +%d" (+ number (string-to-number (match-string 0)))))
      (org-retro-insert-number-at-end-of-line number))
    (goto-char current-point)))

(defun org-retro-increment-number-inline-by-amount ()
  (interactive)
  (org-retro-increment-number-inline
   (string-to-number (read-string "Enter amount: "))))

(defun org-retro-presentation-toggle ()
  (interactive)
  (if (zerop text-scale-mode-amount)
      (text-scale-adjust org-retro-presentation-scale)
    (text-scale-adjust 0)))

(defun org-retro-insert-number-at-end-of-line (number)
  (end-of-line)
  (insert (format " +%d" number)))

(setq auto-mode-alist (cons '("\\.retro$" . org-retro-mode) auto-mode-alist))

(define-key org-retro-mode-map (kbd "C-c u") 'org-retro-increment-number-inline)
(define-key org-retro-mode-map (kbd "C-c C-u") 'org-retro-increment-number-inline-by-amount)
(define-key org-retro-mode-map (kbd "C-c M-p") 'org-retro-presentation-toggle)

(provide 'org-retro)
