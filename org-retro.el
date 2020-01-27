;;; org-retro.el --- Make retros easier to run

;; Author: Eric J. Collins <eric@tabfugni.cc>
;; Version: 0.3.0
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

(require 'org)

; Customizable variables
(defgroup org-retro nil
  "Settings for retrospectives."
  :version "0.3.0"
  :group 'applications)

(defcustom org-retro-presentation-scale 4
  "Set presentation height that allows better viewing on small screens."
  :type 'integer
  :group 'retro)

(define-derived-mode org-retro-mode org-mode "Retrospective"
  "Mode for making retrospectives easier to run.")

(defun org-retro-increment-number-inline (&optional number)
  "Increment by NUMBER at the end of line.

When first attempting to increment, add NUMBER to the end of line
with the format ' +NUMBER'.

If optional NUMBER is not provided, default to 1."
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
  "Increment at the end of the line by input amount."
  (interactive)
  (org-retro-increment-number-inline
   (string-to-number (read-string "Enter amount: "))))

(defun org-retro-presentation-toggle ()
  "Toggle presentation mode.

Using text-scale-mode increases text size by configurable amount.

Checking this is done by looking at text-scale-mode-amount which
is not set until it has been manipulated.  Check that beforehand
to determine if it should go into presentation mode.  If it's set
to zero then this is no longer in presentation mode.

Default size increase is set to 4."
  (interactive)
  (if (or (not (boundp 'text-scale-mode-amount))(zerop text-scale-mode-amount))
      (text-scale-set org-retro-presentation-scale)
    (text-scale-set 0)))

(defun org-retro-next-subtree (&optional number)
  "Move point down to next subtree by NUMBER.

When NUMBER is negative, move point up instead.

Signal when unable to move point in the direction."
  (interactive)
  (let ((number (or number 1)))
    (widen)
    (when (and (org-retro-first-subtree-p) (< number 0))
      (org-narrow-to-subtree)
      (signal 'beginning-of-buffer nil))
    (when (and (org-retro-last-subtree-p) (> number 0))
      (org-narrow-to-subtree)
      (signal 'end-of-buffer nil))
    (outline-next-visible-heading number)
    (org-narrow-to-subtree)))

(defun org-retro-previous-subtree ()
  "Move point to previous subtree."
  (interactive)
  (org-retro-next-subtree -1))

(defun org-retro-insert-number-at-end-of-line (number)
  "Non interactive function to insert a NUMBER at the end of the line."
  (end-of-line)
  (insert (format " +%d" number)))

(defun org-retro-first-subtree-p ()
  "Check if current pointing to the first subtree."
  (save-excursion
    (org-back-to-heading)
    (let ((current-heading (org-current-line-string)))
      (outline-previous-heading)
      (string= (org-current-line-string) current-heading))))

(defun org-retro-last-subtree-p ()
  "Check if current pointing to the last subtree."
  (save-excursion
    (org-back-to-heading)
    (let ((current-heading (org-current-line-string)))
      (outline-next-heading)
      (and
       (not (string= (org-current-line-string) current-heading))
       (not (org-at-heading-p))))))


(setq auto-mode-alist (cons '("\\.retro$" . org-retro-mode) auto-mode-alist))

(define-key org-retro-mode-map (kbd "C-c u") 'org-retro-increment-number-inline)
(define-key org-retro-mode-map (kbd "C-c C-u") 'org-retro-increment-number-inline-by-amount)
(define-key org-retro-mode-map (kbd "C-c C-<return>") 'org-retro-presentation-toggle)
(define-key org-retro-mode-map (kbd "C-c M-n") 'org-retro-next-subtree)
(define-key org-retro-mode-map (kbd "C-c M-p") 'org-retro-previous-subtree)

(provide 'org-retro)

;;; org-retro.el ends here
