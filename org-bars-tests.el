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

(ert-deftest org-bars-xpm-data-test ()
  (let ((level 1)
        (width 3)
        (height 6)
        (indentation 1)
        (color-options '(:only-one-color t :bar-color "#00ff00")))
    (should (string=
             (org-bars-xpm-data level width height indentation color-options)
             (concat "/* XPM */\nstatic char *rule[] = {"
                     "\"3 6 2 1\","
                     "\"* c #00ff00\",\"0 c None\","
                     "\"0*0\","
                     "\"0*0\","
                     "\"0*0\","
                     "\"0*0\","
                     "\"0*0\","
                     "\"0*0\",};"))))
  (defface color-level-A '((t :foreground "#4dafc3")) "" :group 'org-faces)
  (defface color-level-B '((t :foreground "#c97260")) "" :group 'org-faces)
  (defface color-level-C '((t :foreground "#d07391")) "" :group 'org-faces)
  (let ((level 3)
        (width 5)
        (height 8)
        (indentation 2)
        (color-options '(:only-one-color nil
                         :desaturate-level-faces 30
                         :darken-level-faces 15))
        (org-n-level-faces 3)
        (org-level-faces '(color-level-A color-level-B color-level-C)))
    (should (string=
             (org-bars-xpm-data level width height indentation color-options)
             (concat "/* XPM */\nstatic char *rule[] = {"
                     "\"30 8 4 1\","
                     "\"1 c #4eea6ed47558\",\"2 c #840860a45952\",\"3 c #942563507310\",\"0 c None\","
                     ;;  bar  ind  bar  ind  bar  ind
                     "\"001000000000200000000030000000\","
                     "\"001000000000200000000030000000\","
                     "\"001000000000200000000030000000\","
                     "\"001000000000200000000030000000\","
                     "\"001000000000200000000030000000\","
                     "\"001000000000200000000030000000\","
                     "\"001000000000200000000030000000\","
                     "\"001000000000200000000030000000\",};")))))

(ert-deftest org-bars-xpm-dimensions-test ()
  (let ((level 1)
        (width 3)
        (indentation 1)
        (height 6)
        (colors 2))
    (should (string= (org-bars-xpm-dimensions
                      level width indentation height colors)
                     "\"3 6 2 1\",")))
  (let ((level 1)
        (width 3)
        (indentation 1)
        (height 6)
        (colors 2)
        (vpadding 2))
    (should (string= (org-bars-xpm-dimensions
                      level width indentation height colors vpadding)
                     "\"3 10 2 1\",")))
  (let ((level 1)
        (width 3)
        (indentation 3)
        (height 6)
        (colors 2))
    (should (string= (org-bars-xpm-dimensions
                      level width indentation height colors)
                     "\"9 6 2 1\",")))
  (let ((level 3)
        (width 3)
        (indentation 3)
        (height 12)
        (colors 4))
    (should (string= (org-bars-xpm-dimensions
                      level width indentation height colors)
                     "\"27 12 4 1\","))))

(ert-deftest org-bars-pixel-line-test ()
  (should (string= (org-bars-pixel-line 3 3 1) "\"010020030\","))
  (should (string= (org-bars-pixel-line 3 3 1 'only-one-color) "\"0*00*00*0\","))
  (should (string= (org-bars-pixel-line 3 4 1) "\"001000200030\","))
  (should (string= (org-bars-pixel-line 2 6 1) "\"000100000200\","))
  (should (string= (org-bars-pixel-line 2 3 1) "\"010020\","))
  (should (string= (org-bars-pixel-line 2 3 2) "\"010000020000\","))
  (should (string= (org-bars-pixel-line 2 3 3) "\"010000000020000000\",")))

(ert-deftest org-bars-cycle-level-test ()
  (should (= (org-bars-cycle-level
              1 '(:org-cycle-level-faces t :org-n-level-faces 3))
             1))
  (should (= (org-bars-cycle-level
              2 '(:org-cycle-level-faces t :org-n-level-faces 3))
             2))
  (should (= (org-bars-cycle-level
              3 '(:org-cycle-level-faces t :org-n-level-faces 3))
             3))
  (should (= (org-bars-cycle-level
              4 '(:org-cycle-level-faces t :org-n-level-faces 3))
             1))
  (should (= (org-bars-cycle-level
              5 '(:org-cycle-level-faces t :org-n-level-faces 3))
             2))
  (should (= (org-bars-cycle-level
              1 '(:org-cycle-level-faces nil :org-n-level-faces 3))
             1))
  (should (= (org-bars-cycle-level
              2 '(:org-cycle-level-faces nil :org-n-level-faces 3))
             2))
  (should (= (org-bars-cycle-level
              3 '(:org-cycle-level-faces nil :org-n-level-faces 3))
             3))
  (should (= (org-bars-cycle-level
              4 '(:org-cycle-level-faces nil :org-n-level-faces 3))
             3))
  (should (= (org-bars-cycle-level
              5 '(:org-cycle-level-faces nil :org-n-level-faces 3))
             3)))

(ert-deftest org-bars-pixel-bar-test ()
  (should (string= (org-bars-pixel-bar 3 1) "3"))
  (should (string= (org-bars-pixel-bar 3 2) "03"))
  (should (string= (org-bars-pixel-bar 3 3) "030"))
  (should (string= (org-bars-pixel-bar 3 5) "00300"))
  (should (string= (org-bars-pixel-bar 3 4) "0030"))
  (should (string= (org-bars-pixel-bar 1 6) "000100"))
  (should (string= (org-bars-pixel-bar 3 1 t) "*"))
  (should (string= (org-bars-pixel-bar 3 2 t) "0*"))
  (should (string= (org-bars-pixel-bar 3 3 t) "0*0"))
  (should (string= (org-bars-pixel-bar 3 5 t) "00*00"))
  (should (string= (org-bars-pixel-bar 3 5 'only-one-color) "00*00")))

(ert-deftest org-bars-color-level-test ()
  (defface color-level '((t :foreground "#4dafc3")) "" :group 'org-faces)
  (should
   (string= (org-bars-color-level 'color-level 30 15)
            "#4eea6ed47558")))

(ert-deftest org-bars-xpm-color-spec-with-level-faces-test ()
  (defface color-level-1 '((t :foreground "#4dafc3")) "" :group 'org-faces)
  (defface color-level-2 '((t :foreground "#c97260")) "" :group 'org-faces)
  (defface color-level-3 '((t :foreground "#d07391")) "" :group 'org-faces)
  (let ((org-level-faces '(color-level-1 color-level-2 color-level-3))
        (desaturate 30)
        (darken 15))
    (should
     (string= (org-bars-xpm-color-spec-with-level-faces desaturate darken)
              (concat "\"1 c #4eea6ed47558\","
                      "\"2 c #840860a45952\","
                      "\"3 c #942563507310\","
                      "\"0 c None\",")))))

(ert-deftest org-bars-xpm-color-spec-one-color-test ()
  (should
   (string= (org-bars-xpm-color-spec-one-color "#00ff00")
            (concat "\"* c #00ff00\","
                    "\"0 c None\","))))

(ert-deftest org-bars-xpm-color-spec-test ()
  (defface color-level-a '((t :foreground "#4dafc3")) "" :group 'org-faces)
  (defface color-level-b '((t :foreground "#c97260")) "" :group 'org-faces)
  (defface color-level-c '((t :foreground "#d07391")) "" :group 'org-faces)
  (let ((org-level-faces '(color-level-a color-level-b color-level-c)))
    (should
     (string= (org-bars-xpm-color-spec
               '(:only-one-color nil
                 :desaturate-level-faces 30
                 :darken-level-faces 15))
              (concat "\"1 c #4eea6ed47558\","
                      "\"2 c #840860a45952\","
                      "\"3 c #942563507310\","
                      "\"0 c None\",")))
    (should
     (string= (org-bars-xpm-color-spec
               '(:only-one-color t
                 :bar-color "#ff0000"))
              (concat "\"* c #ff0000\","
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
