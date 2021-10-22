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
(require 'org-bars)

;; (global-set-key (kbd "C-<f1>") (lambda () (interactive)(ert t)))
;; (global-set-key (kbd "C-<f1>") (lambda () (interactive)(ert "org-bars-xpm-data-test")))

(ert-deftest org-bars-xpm-data-test ()
  (should (string=
           (org-bars-xpm-data 1 3 6
                              '(:only-one-color t :bar-color "#00ff00")
                              '(:org-indent-indentation-per-level 1))
           (concat "/* XPM */\nstatic char *rule[] = {"
                   "\"3 6 2 1\","
                   "\"* c #00ff00\",\"0 c None\","
                   "\"0*0\","
                   "\"0*0\","
                   "\"0*0\","
                   "\"0*0\","
                   "\"0*0\","
                   "\"0*0\",};")))

  (defface color-level-A '((t :foreground "#4dafc3")) "" :group 'org-faces)
  (defface color-level-B '((t :foreground "#c97260")) "" :group 'org-faces)
  (defface color-level-C '((t :foreground "#d07391")) "" :group 'org-faces)
  (should (string=
           (org-bars-xpm-data 6 3 5
                              '(:only-one-color nil
                                :desaturate-level-faces 30
                                :darken-level-faces 15)
                              '(:org-indent-indentation-per-level 2
                                :org-cycle-level-faces t
                                :org-n-level-faces 3
                                :org-level-faces (color-level-A color-level-B color-level-C)))
           (concat "/* XPM */\nstatic char *rule[] = {"
                   "\"36 5 4 1\","
                   "\"1 c #4eea6ed47558\",\"2 c #840860a45952\",\"3 c #942563507310\",\"0 c None\","
                   ;;  | ind | ind | ind | ind | ind | ind
                   "\"010000020000030000010000020000030000\","
                   "\"010000020000030000010000020000030000\","
                   "\"010000020000030000010000020000030000\","
                   "\"010000020000030000010000020000030000\","
                   "\"010000020000030000010000020000030000\",};")))
  (should (string=
           (org-bars-xpm-data 6 3 5
                              '(:only-one-color nil
                                :desaturate-level-faces 30
                                :darken-level-faces 15)
                              '(:org-indent-indentation-per-level 2
                                :org-cycle-level-faces nil
                                :org-n-level-faces 3
                                :org-level-faces (color-level-A color-level-B color-level-C)))
           (concat "/* XPM */\nstatic char *rule[] = {"
                   "\"36 5 4 1\","
                   "\"1 c #4eea6ed47558\",\"2 c #840860a45952\",\"3 c #942563507310\",\"0 c None\","
                   ;;  | ind | ind | ind | ind | ind | ind
                   "\"010000020000030000030000030000030000\","
                   "\"010000020000030000030000030000030000\","
                   "\"010000020000030000030000030000030000\","
                   "\"010000020000030000030000030000030000\","
                   "\"010000020000030000030000030000030000\",};"))))

(ert-deftest org-bars-xpm-dimensions-test ()
  (let ((level 1)
        (width 3)
        (indentation 1)
        (height 6)
        (colors 2))
    (should (string= (org-bars-xpm-dimensions
                      level width height indentation colors)
                     "\"3 6 2 1\",")))
  (let ((level 1)
        (width 3)
        (indentation 3)
        (height 6)
        (colors 2))
    (should (string= (org-bars-xpm-dimensions
                      level width height indentation colors)
                     "\"9 6 2 1\",")))
  (let ((level 3)
        (width 3)
        (indentation 3)
        (height 12)
        (colors 4))
    (should (string= (org-bars-xpm-dimensions
                      level width height indentation colors)
                     "\"27 12 4 1\","))))

(ert-deftest org-bars-pixel-bar-test ()
  (should (string= (org-bars-pixel-bar 6 3 1 nil '(:org-cycle-level-faces t
                                                   :org-n-level-faces 3))
                   "\"010020030010020030\","))
  (should (string= (org-bars-pixel-bar 6 3 1 nil '(:org-cycle-level-faces nil
                                                   :org-n-level-faces 3))
                   "\"010020030030030030\","))
  (should (string= (org-bars-pixel-bar 6 3 1 'only-one-color) "\"0*00*00*00*00*00*0\","))
  (should (string= (org-bars-pixel-bar 2 3 1 'only-one-color) "\"0*00*0\","))
  (should (string= (org-bars-pixel-bar 2 3 2 'only-one-color) "\"0*00000*0000\","))
  (should (string= (org-bars-pixel-bar 2 3 3 'only-one-color) "\"0*00000000*0000000\",")))

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

(ert-deftest org-bars-pixel-hline-test ()
  (should (string= (org-bars-pixel-hline 3 1) "3"))
  (should (string= (org-bars-pixel-hline 3 2) "03"))
  (should (string= (org-bars-pixel-hline 3 3) "030"))
  (should (string= (org-bars-pixel-hline 3 5) "00300"))
  (should (string= (org-bars-pixel-hline 3 4) "0030"))
  (should (string= (org-bars-pixel-hline 1 6) "000100"))
  (should (string= (org-bars-pixel-hline 3 1 t) "*"))
  (should (string= (org-bars-pixel-hline 3 2 t) "0*"))
  (should (string= (org-bars-pixel-hline 3 3 t) "0*0"))
  (should (string= (org-bars-pixel-hline 3 5 t) "00*00"))
  (should (string= (org-bars-pixel-hline 3 5 'only-one-color) "00*00")))

(ert-deftest org-bars-color-level-test ()
  (defface color-level '((t :foreground "#4dafc3")) "" :group 'org-faces)
  (should
   (string= (org-bars-color-level 'color-level 30 15)
            "#4eea6ed47558")))

(ert-deftest org-bars-xpm-color-spec-with-level-faces-test ()
  (defface color-level-1 '((t :foreground "#4dafc3")) "" :group 'org-faces)
  (defface color-level-2 '((t :foreground "#c97260")) "" :group 'org-faces)
  (defface color-level-3 '((t :foreground "#d07391")) "" :group 'org-faces)
  (should
   (string= (org-bars-xpm-color-spec-with-level-faces
             30 15 '(:org-n-level-faces 8 ; default org-mode value
                     :org-level-faces   (color-level-1 color-level-2 color-level-3)))
            (concat "\"1 c #4eea6ed47558\","
                    "\"2 c #840860a45952\","
                    "\"3 c #942563507310\","
                    "\"0 c None\",")))
  (let ((org-options '(:org-n-level-faces 2 ; < 8
                       :org-level-faces   (color-level-1 color-level-2 color-level-3)))
        (desaturate 30)
        (darken 15))
    (should
     (string= (org-bars-xpm-color-spec-with-level-faces
               30 15 '(:org-n-level-faces 2 ; < 8
                       :org-level-faces   (color-level-1 color-level-2 color-level-3)))
              (concat "\"1 c #4eea6ed47558\","
                      "\"2 c #840860a45952\","
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
  (should
   (string= (org-bars-xpm-color-spec '(:only-one-color nil
                                       :desaturate-level-faces 30
                                       :darken-level-faces 15)
                                     '(:org-n-level-faces 8 ; default org-mode value
                                       :org-level-faces   (color-level-a color-level-b color-level-c)))
            (concat "\"1 c #4eea6ed47558\","
                    "\"2 c #840860a45952\","
                    "\"3 c #942563507310\","
                    "\"0 c None\",")))
  (should
   (string= (org-bars-xpm-color-spec '(:only-one-color t
                                       :bar-color "#ff0000")
                                     '(:org-n-level-faces 8 ; default org-mode value
                                       :org-level-faces   (color-level-a color-level-b color-level-c)))
            (concat "\"* c #ff0000\","
                    "\"0 c None\","))))

(ert-deftest org-bars-face-height-test ()
  (defface org-bars-height-1 '((t :height 1.5)) "" :group 'org-faces)
  (defface org-bars-height-2 '((t :height 0.8)) "" :group 'org-faces)
  (defface org-bars-height-3 '((t :height 200)) "" :group 'org-faces)
  (defface org-bars-height-4 '((t :foreground "#ff0000")) "" :group 'org-faces) ; we don't specify the height

  ;; test `org-bars-face-height' when we don't scale the text.
  (let ((line-spacing nil)
        face-height-1 face-height-2 face-height-3 face-height-4
        height-1 height-2 height-3 heights-4)
    (with-current-buffer (get-buffer-create "*org-bars-test*")
      (erase-buffer)
      (fundamental-mode)
      (insert (propertize "face-height-1\n" 'face 'org-bars-height-1))
      (insert (propertize "face-height-2\n" 'face 'org-bars-height-2))
      (insert (propertize "face-height-3\n" 'face 'org-bars-height-3))
      (insert (propertize "face-height-4\n" 'face 'org-bars-height-4)))
    ;; we must use `switch-to-buffer' to use `line-pixel-height', it doesn't
    ;; gives the expected result when used inside `with-current-buffer' macro.
    (switch-to-buffer "*org-bars-test*")
    (goto-line 1)
    (setq heights-1 (line-pixel-height))
    (goto-line 2)
    (setq heights-2 (line-pixel-height))
    (goto-line 3)
    (setq heights-3 (line-pixel-height))
    (goto-line 4)
    (setq heights-4 (line-pixel-height))
    (setq face-height-1 (org-bars-face-height 'org-bars-height-1 0))
    (setq face-height-2 (org-bars-face-height 'org-bars-height-2 0))
    (setq face-height-3 (org-bars-face-height 'org-bars-height-3 0))
    (setq face-height-4 (org-bars-face-height 'org-bars-height-4 0))
    (kill-buffer "*org-bars-test*")
    ;; should forms
    (should (= face-height-1 heights-1))
    (should (= face-height-2 heights-2))
    (should (= face-height-3 heights-3))
    (should (= face-height-4 heights-4)))

  ;; test `org-bars-face-height' when we scale the text with `text-scale-increase'
  (let ((text-scale-mode-step 1.2)
        (line-spacing nil)
        face-height-1 face-height-2 face-height-3 face-height-4
        heights-1 heights-2 heights-3 heights-4)
    (with-current-buffer (get-buffer-create "*org-bars-test*")
      (erase-buffer)
      (fundamental-mode)
      (insert (propertize "face-height-1\n" 'face 'org-bars-height-1))
      (insert (propertize "face-height-2\n" 'face 'org-bars-height-2))
      (insert (propertize "face-height-3\n" 'face 'org-bars-height-3))
      (insert (propertize "face-height-4\n" 'face 'org-bars-height-4))
      ;; we want to test `org-bars-face-height' after the call
      ;; of the following form
      (text-scale-increase 5))
    ;; we must use `switch-to-buffer' to use `line-pixel-height'. because
    ;; its returned value depends on the buffer being displayed.
    (switch-to-buffer "*org-bars-test*")
    (cl-flet ((heights-on-line (h) `(,(- h 2) ,(1- h) ,h ,(1+ h) ,(+ h 2))))
      (goto-line 1)
      (setq heights-1 (heights-on-line (line-pixel-height)))
      (goto-line 2)
      (setq heights-2 (heights-on-line (line-pixel-height)))
      (goto-line 3)
      (setq heights-3 (heights-on-line (line-pixel-height)))
      (goto-line 4)
      (setq heights-4 (heights-on-line (line-pixel-height))))
    (setq face-height-1 (org-bars-face-height 'org-bars-height-1 0))
    (setq face-height-2 (org-bars-face-height 'org-bars-height-2 0))
    (setq face-height-3 (org-bars-face-height 'org-bars-height-3 0))
    (setq face-height-4 (org-bars-face-height 'org-bars-height-4 0))
    (kill-buffer "*org-bars-test*")
    (should (member face-height-1 heights-1))
    (should (member face-height-2 heights-2))
    (should (member face-height-3 heights-3))
    (should (member face-height-4 heights-4))))

;;; manual tests

(defmacro comment (&rest body) "Ignores body and yield nil." nil)

(comment
 (let ((width 9)
       (height 25)
       (color-options '(:only-one-color nil
                        :desaturate-level-faces 30
                        :darken-level-faces 15))
       (org-options `(:org-indent-indentation-per-level ,org-indent-indentation-per-level
                      :org-n-level-faces ,org-n-level-faces
                      :org-level-faces ,org-level-faces)))
   (insert (propertize " " 'display (org-bars-xpm-image
                                     1 width height color-options org-options))))
 (let ((width 9)
       (height 18)
       (color-options '(:only-one-color nil
                        :desaturate-level-faces 30
                        :darken-level-faces 15))
       (org-options `(:org-indent-indentation-per-level ,org-indent-indentation-per-level
                      :org-n-level-faces ,org-n-level-faces
                      :org-level-faces ,org-level-faces)))
   (insert (propertize " " 'display (org-bars-xpm-image
                                     1 width height color-options org-options))))
 (let ((width 9)
       (height 18)
       (color-options '(:only-one-color nil
                        :desaturate-level-faces 30
                        :darken-level-faces 15))
       (org-options `(:org-indent-indentation-per-level ,org-indent-indentation-per-level
                      :org-n-level-faces ,org-n-level-faces
                      :org-level-faces ,org-level-faces)))
   (insert (propertize " " 'display (org-bars-xpm-image
                                     2 width height color-options org-options))))
 (let ((width 9)
       (height 18)
       (color-options '(:only-one-color nil
                        :desaturate-level-faces 30
                        :darken-level-faces 15))
       (org-options `(:org-indent-indentation-per-level ,org-indent-indentation-per-level
                      :org-n-level-faces ,org-n-level-faces
                      :org-level-faces ,org-level-faces)))
   (insert (propertize " " 'display (org-bars-xpm-image
                                     3 width height color-options org-options))))
 (let ((width 9)
       (height 18)
       (color-options '(:only-one-color nil
                        :desaturate-level-faces 30
                        :darken-level-faces 15))
       (org-options `(:org-indent-indentation-per-level ,org-indent-indentation-per-level
                      :org-n-level-faces ,org-n-level-faces
                      :org-level-faces ,org-level-faces)))
   (insert (propertize " " 'display (org-bars-xpm-image
                                     5 width height color-options org-options))))
 )

(comment
 (setq org-bars-color-options '(:only-one-color nil
                                :desaturate-level-faces 30
                                :darken-level-faces 15))
 (setq org-bars-color-options '(:only-one-color t
                                :bar-color "#ff0000"))

 (setq org-bars-extra-pixels-height 6) ; default
 (setq org-bars-extra-pixels-height 0)

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
 (setq org-n-level-faces 8) ; default
 (setq org-n-level-faces 3)
 (setq org-cycle-level-faces t) ; default
 (setq org-cycle-level-faces nil)
 (setq org-indent-indentation-per-level 2) ; default
 (setq org-indent-indentation-per-level 1)
 (setq org-indent-indentation-per-level 3)
 )

(comment
 (custom-set-faces
  '(org-level-1 ((t (:height 200))))
  '(org-level-2 ((t (:height 180))))
  '(org-level-3 ((t (:height 150))))
  '(org-level-4 ((t (:height 110))))
  '(org-level-5 ((t (:height 110))))
  '(org-level-6 ((t (:height 110))))
  '(org-level-7 ((t (:height 110))))
  '(org-level-8 ((t (:height 110)))))

 (custom-set-faces
  '(org-level-1 ((t (:height 1.8))))
  '(org-level-2 ((t (:height 1.4))))
  '(org-level-3 ((t (:height 1.2))))
  '(org-level-4 ((t (:height 1.0))))
  '(org-level-5 ((t (:height 1.0))))
  '(org-level-6 ((t (:height 1.0))))
  '(org-level-7 ((t (:height 1.0))))
  '(org-level-8 ((t (:height 1.0)))))

 (custom-set-faces
  '(org-level-1 ((t (:height 200))))
  '(org-level-2 ((t (:height 180))))
  '(org-level-3 ((t (:height 1.2))))
  '(org-level-4 ((t (:height 1.2))))
  '(org-level-5 ((t (:height 1.0))))
  '(org-level-6 ((t (:height 1.0))))
  '(org-level-7 ((t (:height 1.0))))
  '(org-level-8 ((t (:height 1.0)))))

 (custom-set-faces
  ;; we don't specify the height
  '(org-level-1 ((t :foreground "#ff0000")))
  '(org-level-2 ((t :foreground "#ff0000")))
  '(org-level-3 ((t :foreground "#ff0000")))
  '(org-level-4 ((t :foreground "#ff0000")))
  '(org-level-5 ((t :foreground "#ff0000")))
  '(org-level-6 ((t :foreground "#ff0000")))
  '(org-level-7 ((t :foreground "#ff0000")))
  '(org-level-8 ((t :foreground "#ff0000"))))
 )
