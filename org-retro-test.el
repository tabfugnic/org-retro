;;; org-retro-test.el --- Test suite for org-retro

;; Author: Eric J. Collins <eric@tabfugni.cc>
;; Keywords: org, retro, test
;; URL: https://github.com/tabfugnic/org-retro/org-retro-test.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; simple unit test suite for the org-retro

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

(load-file "./org-retro.el")
(require 'org-retro)
(require 'ert)

(ert-deftest org-retro-create-plus-one-test ()
  (with-temp-buffer
    (insert "foo")
    (goto-char (point-min))
    (org-retro-increment-number-inline)
    (should
     (equal
      (thing-at-point 'line t)
      "foo +1"))))

(ert-deftest org-retro-increment-number-test ()
  (with-temp-buffer
    (insert "foo +1")
    (goto-char (point-min))
    (org-retro-increment-number-inline)
    (should
     (equal
      (thing-at-point 'line t)
      "foo +2"))))

(ert-deftest org-retro-increment-number-in-strange-string-test ()
  (with-temp-buffer
    (insert "foo +1 bar +5")
    (goto-char (point-min))
    (org-retro-increment-number-inline)
    (should
     (equal
      (thing-at-point 'line t)
      "foo +1 bar +6"))))
