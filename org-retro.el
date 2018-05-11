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
  "Settings for retrospectives"
  :version "0.1.0"
  :group 'applications)

(define-derived-mode org-retro-mode org-mode "Retrospective"
  "Mode for making retrospectives easier to run")

(defun org-retro-increment-number-inline ()
 (goto-char (point-max))
 (skip-chars-backward "0-9")
 (if (looking-at "[0-9]+")
     (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))
   (insert " +1")))

(setq auto-mode-alist (cons '("\\.retro$" . org-retro-mode) auto-mode-alist))

(provide 'org-retro)
