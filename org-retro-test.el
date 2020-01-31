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
(require 'org)
(require 'cl)
(require 'org-retro)
(require 'ert)

(ert-deftest org-retro-create-plus-one-test ()
  (with-temp-buffer
    (insert "foo 495")
    (goto-char (point-min))
    (org-retro-increment-number-inline)
    (should
     (equal
      (thing-at-point 'line t)
      "foo 495 +1"))))

(ert-deftest org-retro-create-plus-one-test-with-additional-line ()
  (with-temp-buffer
    (insert "foo 495\nthing")
    (goto-char (point-min))
    (org-retro-increment-number-inline)
    (should
     (equal
      (thing-at-point 'line t)
      "foo 495 +1\n"))))

(ert-deftest org-retro-increment-number-test ()
  (with-temp-buffer
    (insert "foo +1")
    (goto-char (point-min))
    (org-retro-increment-number-inline)
    (should
     (equal
      (thing-at-point 'line t)
      "foo +2"))))

(ert-deftest org-retro-create-plus-one-with-plus-at-end-test ()
  (with-temp-buffer
    (insert "foo +")
    (goto-char (point-min))
    (org-retro-increment-number-inline)
    (should
     (equal
      (thing-at-point 'line t)
      "foo + +1"))))

(ert-deftest org-retro-create-plus-one-when-math-test ()
  (with-temp-buffer
    (insert "learned the value of x+1")
    (goto-char (point-min))
    (org-retro-increment-number-inline)
    (should
     (equal
      (thing-at-point 'line t)
      "learned the value of x+1 +1"))))

(ert-deftest org-retro-increment-number-in-strange-string-test ()
  (with-temp-buffer
    (insert "foo +1 bar +5")
    (goto-char (point-min))
    (org-retro-increment-number-inline)
    (should
     (equal
      (thing-at-point 'line t)
      "foo +1 bar +6"))))

(ert-deftest org-retro-increment-number-by-input-test ()
  (with-temp-buffer
    (insert "foo +4")
    (goto-char (point-min))
    (org-retro-increment-number-inline 10)
    (should
     (equal
      (thing-at-point 'line t)
      "foo +14"))))

(ert-deftest org-retro-increment-number-keeps-current-pointer-location-test ()
  (with-temp-buffer
    (let ((test-point (1+ (point-min))))
      (insert "foo +4")
      (goto-char test-point)
      (org-retro-increment-number-inline 10)
      (should
       (equal (point) test-point)))))

(ert-deftest org-retro-increment-number-goes-negative-test ()
  (with-temp-buffer
    (insert "foo +1")
    (goto-char (point-min))
    (org-retro-increment-number-inline -2)
    (should
     (equal
      (thing-at-point 'line t)
      "foo -1"))))

(ert-deftest org-retro-increment-by-negative-create-test ()
  (with-temp-buffer
    (insert "foo 495")
    (goto-char (point-min))
    (org-retro-increment-number-inline -1)
    (should
     (equal
      (thing-at-point 'line t)
      "foo 495 -1"))))

(ert-deftest org-retro-clear-number-test ()
  (with-temp-buffer
    (insert "foo 495 +1")
    (goto-char (point-min))
    (org-retro-clear-number)
    (should
     (equal
      (thing-at-point 'line t)
      "foo 495"))))

(ert-deftest org-retro-presentation-toggle-test ()
  (with-temp-buffer
    (org-retro-presentation-toggle)
    (should
     (> text-scale-mode-amount 0))
    (org-retro-presentation-toggle)
    (should
     (= text-scale-mode-amount 0))))

(ert-deftest org-retro-navigate-to-narrowed-sections-test ()
  (with-temp-buffer
    (insert "* Joys\n- thing\n* Deltas\n- stuff")
    (goto-char (point-min))
    (org-narrow-to-subtree)
    (org-retro-next-subtree)
    (should
     (equal
      (buffer-string)
      "* Deltas\n- stuff"))
    (org-retro-previous-subtree)
    (should
     (equal
      (buffer-string)
      "* Joys\n- thing"))))

(ert-deftest org-retro-navigating-beyond-narrowed-section-test ()
  (with-temp-buffer
    (insert "* Joys\n- thing\n")
    (goto-char (1+ (point-min)))
    (org-narrow-to-subtree)
    (should-error
     (org-retro-next-subtree)
      :type 'end-of-buffer)
    (goto-char (1+ (point-min)))
    (should-error
     (org-retro-previous-subtree)
      :type 'beginning-of-buffer)))

(ert-deftest org-retro-final-subtree-test ()
  (with-temp-buffer
    (insert "* Joys\n- thing\n")
    (should
     (org-retro-first-subtree-p))
    (should
     (org-retro-last-subtree-p))))

(ert-deftest org-retro-clear-test ()
  (with-temp-buffer
    (insert "* Joys\n- thing\n")
    (org-retro-mode)
    (org-retro-clear)
    (should
     (equal
      (buffer-string)
     "* Joys\n"))))

(ert-deftest org-retro-clear-persist-tag-test ()
  (with-temp-buffer
    (insert "* Joys\n- thing\n* I love content :persist:\n- I am persist\n")
    (setf org-tags-column 1)
    (org-retro-mode)
    (org-retro-clear)
    (should
     (equal
      (buffer-string)
      "* Joys\n* I love content :persist:\n- I am persist\n"))))

(ert-deftest org-retro-archive-test ()
  (cleanup-test-files
   (lambda()
     (cl-letf (((symbol-function 'buffer-file-name) (lambda(&optional buffer) "test.retro")))
     (with-temp-file "test.retro"
       (insert "* Joys\n- thing\n"))
     (with-temp-buffer
       (org-retro-mode)
       (insert-file-contents "test.retro" nil nil nil t)
       (org-retro-archive)
       (should
        (equal
         (buffer-string)
         "* Joys\n"))
       (insert-file-contents (concat "test-" (format-time-string "%Y%m%d") ".retro") nil nil nil t)
       (should
        (equal
         (buffer-string)
         "* Joys\n- thing\n")))))))

(defun cleanup-test-files(func)
  "Cleanup test files after calling FUNC."
  (unwind-protect
      (funcall func)
    (dolist (file (file-expand-wildcards "test*.retro"))
      (delete-file file))))

;;; org-retro-test.el ends here
