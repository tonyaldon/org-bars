;;; org-bars-tests.el --- Tests for org-bars.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Tony Aldon.

;; Author: Tony Aldon <tony.aldon.adm@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for org-bars.el

;;; Code:

(require 'ert)

;; (global-set-key (kbd "C-<f1>") (lambda () (interactive)(ert t)))

(comment ; manual test with different org-indent-indentation-per-level
 (setq org-indent-indentation-per-level 1)
 (setq org-indent-indentation-per-level 2)
 (setq org-indent-indentation-per-level 3)
 )

(ert-deftest org-bars-pixel-line-test ()
  (should (string= (org-bars-pixel-line 3 3 1) "\"010020030\","))
  (should (string= (org-bars-pixel-line 3 4 1) "\"001000200030\","))
  (should (string= (org-bars-pixel-line 2 6 1) "\"000100000200\","))
  (should (string= (org-bars-pixel-line 2 3 1) "\"010020\","))
  (should (string= (org-bars-pixel-line 2 3 2) "\"010000020000\","))
  (should (string= (org-bars-pixel-line 2 3 3) "\"010000000020000000\",")))

(ert-deftest org-bars-pixel-bar-test ()
  (should (string= (org-bars-pixel-bar 3 1) "3"))
  (should (string= (org-bars-pixel-bar 3 2) "03"))
  (should (string= (org-bars-pixel-bar 3 3) "030"))
  (should (string= (org-bars-pixel-bar 3 5) "00300"))
  (should (string= (org-bars-pixel-bar 3 4) "0030"))
  (should (string= (org-bars-pixel-bar 1 6) "000100")))

(ert-deftest org-bars-color-level-test ()
  (defface color-level '((t :foreground "#4dafc3")) "" :group 'org-faces)
  (should
   (string= (org-bars-color-level 'color-level 30 15)
            "#4eea6ed47558")))

(ert-deftest org-bars-xpm-color-spec-test ()
  (defface color-level-1 '((t :foreground "#4dafc3")) "" :group 'org-faces)
  (defface color-level-3 '((t :foreground "#d07391")) "" :group 'org-faces)
  (defface color-level-2 '((t :foreground "#c97260")) "" :group 'org-faces)
  (let ((org-level-faces '(color-level-1 color-level-2 color-level-3))
        (desaturate 30)
        (darken 15))
    (should
     (string= (org-bars-xpm-color-spec desaturate darken)
              (concat "\"1 c #4eea6ed47558\","
                      "\"2 c #840860a45952\","
                      "\"3 c #942563507310\","
                      "\"0 c None\",")))))

(comment ; for manual testing
 (custom-set-faces
  '(org-bars-star-empty ((t (:foreground "#00ff00"))))
  '(org-bars-star-invisible ((t (:foreground "#ff0000"))))
  '(org-bars-star-visible ((t (:foreground "#0000ff")))))

 (custom-set-faces
  '(org-bars-star-empty ((t nil)))
  '(org-bars-star-invisible ((t nil)))
  '(org-bars-star-visible ((t nil))))

 (setq org-level-color-stars-only t)
 (setq org-level-color-stars-only nil)
 )
