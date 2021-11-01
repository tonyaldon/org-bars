;;; org-bars.el --- Add bars to virtual indentation -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Tony Aldon

;; Author: Tony Aldon <tony.aldon.adm@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (dash "2.17.0") (s "1.12.0"))
;; Keywords: outlines
;; Homepage: https://github.com/tonyaldon/org-bars

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO:

;;; Code:

(require 'color)
(require 'face-remap)
(require 's)
(require 'dash)
(require 'org-indent)

;;; compute XPM image data (virtual bars)

(defvar org-bars-color-options '(:only-one-color nil)
  "Plist holding user options related to the colors of the level bars.

:only-one-color
    If t, the level bars have the color `:bar-color'.
    If nil, the level bars inherit the color of the foreground faces
    of the heading lines.  See `org-level-faces'.
:bar-color
    If `:only-one-color' is non-nil, use this color for the level bars.
:desaturate-level-faces
    If `:only-one-color' is nil, apply this number (0 to 100) to
    desaturate the colors from the foreground color of the faces
    `org-level-faces'.  See `org-bars-color-level'.
:darken-level-faces
    If `:only-one-color' is nil, apply this number (0 to 100) to
    darken the colors from the foreground color of the faces
    `org-level-faces'.  See `org-bars-color-level'.")

(defun org-bars-color-level (face desaturate darken)
  "Desaturate and darken foreground color of FACE.

0 <= DESATURATE <= 100 and 0 <= DARKEN <= 100.
See `color-desaturate-name' and `color-darken-name'."
  (-> (face-foreground face nil 'default)
      (color-desaturate-name desaturate)
      (color-darken-name darken)))

(defun org-bars-xpm-color-spec-with-level-faces (desaturate darken org-options)
  "Return xpm color specification calculated from `org-level-faces'.

ORG-OPTIONS is a plist:
:org-n-level-faces
    an integer between 1 to 8 included corresponding to the
    number of colors we use in the xpm image.  In practice, the
    value of `:org-n-level-faces' is `org-n-level-faces'.
:org-level-faces
    a list of 8 faces.  In practice, the value of `:org-level-faces'
    is `org-level-faces'.  In xpm color specification 1 to 8
    correspond to modified colors from the foreground colors
    of the 8 faces of `:org-level-faces'.  If `:org-n-level-faces' < 8,
    we only use the first `:org-n-level-faces' faces of `:org-level-faces'
    in the xpm specification."
  (let ((n-faces (plist-get org-options :org-n-level-faces))
        (faces (plist-get org-options :org-level-faces)))
    (concat
     (-reduce
      #'concat
      (--map-indexed (concat "\"" (number-to-string (1+ it-index)) " c "
                             (org-bars-color-level it desaturate darken)
                             "\",")
                     (-take n-faces faces)))
     "\"0 c None\",")))

(defun org-bars-xpm-color-spec-one-color (color)
  "Return xpm color specification with one color COLOR and None color.

In the xpm color specification, * corresponds to color COLOR
and 0 to None."
  (concat "\"* c " color "\","
          "\"0 c None\","))

(defun org-bars-xpm-color-spec (color-options org-options)
  "Return xpm color specification respecting options COLOR-OPTIONS.

COLOR-OPTIONS is a plist with the same specification as
`org-bars-color-options' variable.

ORG-OPTIONS is a plist with same specification as
in `org-bars-xpm-color-spec-with-level-faces' function signature."
  (let ((only-one-color-p (plist-get color-options :only-one-color))
        (color            (plist-get color-options :bar-color))
        (desaturate       (plist-get color-options :desaturate-level-faces))
        (darken           (plist-get color-options :darken-level-faces)))
    (if only-one-color-p
        (org-bars-xpm-color-spec-one-color (or color "#8c8c8c"))
      (org-bars-xpm-color-spec-with-level-faces (or desaturate 0)
                                                (or darken 0)
                                                org-options))))

(defun org-bars-xpm-dimensions (level width height indentation colors)
  "Return the xpm dimensions.

In practice, `org-bars-xpm-dimensions' is called with INDENTATION argument
value equal to `org-indent-indentation-per-level'."
  (let ((width-str (number-to-string (* level width indentation)))
        (heigth-str (number-to-string height))
        (character-per-pixel "1"))
    (concat "\"" width-str " " heigth-str " "
            (number-to-string colors) " " character-per-pixel "\",")))

(defun org-bars-cycle-level (level org-options)
  "Determine the level to pass to `org-bars-pixel-hline'.

ORG-OPTIONS is a plist:
:org-cycle-level-faces
    t if we want to cycle level bar color modulo `:org-n-level-faces'.
    In practice the value of `:org-cycle-level-faces' is
    `org-cycle-level-faces'.
:org-n-level-faces
    an integer between 1 to 8 included corresponding to the
    number of colors we use in the xpm image.  In practice, the
    value of `:org-n-level-faces' is `org-n-level-faces'."
  (let ((cycle-p (plist-get org-options :org-cycle-level-faces))
        (n-faces (plist-get org-options :org-n-level-faces)))
    (if cycle-p
        (if (= (mod level n-faces) 0) n-faces (mod level n-faces))
      (min level n-faces))))

(defun org-bars-pixel-hline (level width &optional only-one-color)
  "Return WIDTH pixels equal to 0 but one centered equal to LEVEL.

LEVEL must be an integer verifying, 1 < LEVEL < 8.
If ONLY-ONE-COLOR is non-nil, LEVEL pixel is replaced by a star *.
For instance:
    (org-bars-pixel-hline 3 9) -> \"000030000\".
    (org-bars-pixel-hline 3 9 t) -> \"0000*0000\"."
  (cond
   ((= width 1) (or (and only-one-color "*")
                    (number-to-string level)))
   ((= width 2) (concat "0" (or (and only-one-color "*")
                                (number-to-string level))))
   ((= (mod width 2) 1)
    (concat (s-repeat (floor (/ width 2.0)) "0")
            (or (and only-one-color "*")
                (number-to-string level))
            (s-repeat (floor (/ width 2.0)) "0")))
   (t
    (let* ((l-pixels (floor (/ width 2.0)))
           (r-pixels (1- l-pixels)))
      (concat (s-repeat l-pixels "0")
              (or (and only-one-color "*")
                  (number-to-string level))
              (s-repeat r-pixels "0"))))))

(defun org-bars-pixel-bar (level width indentation only-one-color &optional org-options)
  "Return the pixels bars for level LEVEL with WIDTH being the character's width.

`org-bars-pixel-bar' is used to construct the XPM image
used as `line-prefix' text property for each line for the level
LEVEL in the org tree.  See `org-bars-xpm-data'.

WIDTH * (INDENTATION - 1) corresponds to the number of None pixels we add
after each level bar.  In practice, `org-bars-pixel-bar' is called
with INDENTATION argument value equal to `org-indent-indentation-per-level'.

ORG-OPTIONS is a plist:
:org-cycle-level-faces
    t if we want to cycle level bar color modulo `:org-n-level-faces'.
    In practice the value of `:org-cycle-level-faces' is
    `org-cycle-level-faces'.
:org-n-level-faces
    an integer between 1 to 8 included corresponding to the
    number of colors we use in the xpm image.  In practice, the
    value of `:org-n-level-faces' is `org-n-level-faces'.

See `org-bars-cycle-level'."
  (let ((none-pixels (s-repeat (* (1- indentation) width) "0")))
    (concat "\""
            (s-join none-pixels
                    (--map (org-bars-pixel-hline
                            (or (and only-one-color 1)
                                (org-bars-cycle-level it org-options))
                            width only-one-color)
                           (number-sequence 1 level)))
            none-pixels
            "\",")))

(defun org-bars-xpm-data (level width height color-options org-options)
  "Return xpm data string.

COLOR-OPTIONS is a plist with the same specification as
`org-bars-color-options' variable.

ORG-OPTIONS is a plist:
:org-indent-indentation-per-level
    an integer >= 1 that represent the indentation per level in number
    of characters.  In practice the value of `:org-indent-indentation-per-level'
    is `org-indent-indentation-per-level'.
:org-cycle-level-faces
    t if we want to cycle level bar color modulo `:org-n-level-faces'.
    In practice the value of `:org-cycle-level-faces' is
    `org-cycle-level-faces'.
:org-n-level-faces
    an integer between 1 to 8 included corresponding to the
    number of colors we use in the xpm image.  In practice, the
    value of `:org-n-level-faces' is `org-n-level-faces'.
:org-level-faces
    a list of 8 faces.  In practice, the value of `:org-level-faces'
    is `org-level-faces'.  In xpm color specification 1 to 8
    correspond to modified colors from the foreground colors
    of the 8 faces of `:org-level-faces'.  If `:org-n-level-faces' < 8,
    we only use the first `:org-n-level-faces' faces of `:org-level-faces'
    in the xpm specification."
  (let* ((identifier "/* XPM */\nstatic char *rule[] = {")
         (only-one-color-p (plist-get color-options :only-one-color))
         (n-faces (plist-get org-options :org-n-level-faces))
         (colors (if only-one-color-p 2 (1+ n-faces)))
         (indentation (plist-get org-options :org-indent-indentation-per-level))
         (dimensions (org-bars-xpm-dimensions level width height indentation colors))
         (color-spec (org-bars-xpm-color-spec color-options org-options))
         (pixel-bar (org-bars-pixel-bar level width indentation only-one-color-p org-options))
         (raster (-reduce #'concat (-repeat height pixel-bar)))
         (end "};"))
    (concat identifier dimensions color-spec raster end)))

(defun org-bars-xpm-image (level width height color-options org-options)
  "Return an image descriptor for level LEVEL in the org tree.

When `org-bars-mode' is on, the `line-prefix' property
of each line at the level LEVEL (LEVEL > 0) in the org tree, is set to
the XPM image produced by `org-bars-xpm-image'.

LEVEL must be strickly superior to 0."
  `(image :type xpm
          :data ,(org-bars-xpm-data level width height color-options org-options)
          :mask heuristic
          :ascent center))

;;; manage stars and headlines faces

(defvar org-bars-with-dynamic-stars-p t
  "When t, heading stars are dynamicaly replaced by `org-bars-star'.

When nil, heading stars uses the standard \"*\" and are not updated
when the visibility of the buffer changes.")

(defface org-bars-star-empty nil
  "If non trivial, this is the face used for the heading star
when corresponding subtree is empty.  If trivial, faces in
`org-level-faces' will be used.

See `face-nontrivial-p' and `org-bars-subtree-is-empty-p'."
  :group 'org-faces)

(defface org-bars-star-invisible nil
  "If non trivial, this is the face used for the heading star
when corresponding subtree is invisible and not empty.  If trivial,
faces in `org-level-faces' will be used.

See `face-nontrivial-p', `org-bars-subtree-is-empty-p' and
`org-bars-subtree-is-invisible-p'."
  :group 'org-faces)

(defface org-bars-star-visible nil
  "If non trivial, this is the face used for the heading star
when corresponding subtree is not visible.  If trivial, faces
in `org-level-faces' will be used.

See `face-nontrivial-p', `org-bars-subtree-is-empty-p' and
`org-bars-subtree-is-invisible-p'."
  :group 'org-faces)

(defvar org-bars-stars
  '(:empty "◉"
    :invisible "▶"
    :visible "▼")
  "Plist of the strings used in place of the last star \"*\" in headlines.
The replacement star is choosen accordingly to the state of the subtree:
:empty
    if the subtree is empty (see `org-bars-subtree-is-empty-p'),
:invisible
    if the subtree is not empty and invisible
    (see `org-bars-subtree-is-invisible-p'),
:visible
    if the subtree is not empty and visible.")

(defun org-bars-subtree-is-empty-p ()
  "Return t if subtree at point is empty."
  (save-match-data
    (save-excursion
      (let* ((heading-end (progn (outline-back-to-heading)
                                 (outline-end-of-heading)
                                 (point)))
             (subtree-end (progn (outline-end-of-subtree)
                                 (point))))
        (= subtree-end heading-end)))))

(defun org-bars-subtree-is-invisible-p ()
  "Return t if subtree at point is invisible."
  (save-match-data (org-invisible-p (point-at-eol))))

(defun org-bars-star ()
  "Return plist of the string and face of the star to use on heading at point.
The plist has the keywords: :star and :face.

The string and face to use depend on the state of the subtree: empty,
invisible or visible.

You can customize the stars with the variable `org-bars-stars'."
  (let (star face)
    (cond ((org-bars-subtree-is-empty-p)
           (setq star (plist-get org-bars-stars :empty))
           (setq face 'org-bars-star-empty))
          ((org-bars-subtree-is-invisible-p)
           (setq star (plist-get org-bars-stars :invisible))
           (setq face 'org-bars-star-invisible))
          (t
           (setq star (plist-get org-bars-stars :visible))
           (setq face 'org-bars-star-visible)))
    `(:star ,star :face ,face)))

(defun org-bars-get-level-face (n)
  "Get the right face for match N in font-lock matching of headlines.

This function is meant to override `org-get-level-face' with an advice."
  ;; The font-lock matching of heading lines appears in the function
  ;; `org-set-font-lock-defaults' like this:
  ;;
  ;; `(,(if org-fontify-whole-heading-line
  ;;        "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
  ;;      "^\\(\\**\\)\\(\\* \\)\\(.*\\)")
  ;;   (1 (org-get-level-face 1))
  ;;   (2 (org-get-level-face 2))
  ;;   (3 (org-get-level-face 3)))
  ;;
  (let* ((beg-2 (match-beginning 2))
         (end-2 (match-end 2))
         (beg-1 (match-beginning 1))
         (org-l0 (- end-2 beg-1 1))
         (org-l (if org-odd-levels-only (1+ (/ org-l0 2)) org-l0))
         (org-f (if org-cycle-level-faces
                    (nth (% (1- org-l) org-n-level-faces) org-level-faces)
                  (nth (1- (min org-l org-n-level-faces)) org-level-faces))))
    (cond
     ;; we return 'default face because a face is expected as a result.
     ;; But this face will never be used because it is applied to
     ;; a part of the buffer we make invisible.
     ((eq n 1)
      (or (= org-l0 1)
          (add-text-properties
           beg-1 (- end-2 2) '(invisible org-bars)))
      'default)
     ((eq n 2)
      (if org-bars-with-dynamic-stars-p
          (let* ((star (org-bars-star))
                 (star-s (plist-get star :star))
                 (star-f (plist-get star :face)))
            (compose-region beg-2 (1- end-2) star-s)
            (if (face-nontrivial-p star-f) star-f org-f))
        org-f))
     (t (unless org-level-color-stars-only org-f)))))

(defun org-bars-refresh-stars (_state)
  "Refontify all visible heading stars.

_STATE is a dummy variable that is not used but mandatory because
`org-bars-refresh-stars' is meant to be added to `org-cycle-hook'."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-outline-regexp nil t)
      (with-silent-modifications
        (unless (car (get-char-property-and-overlay (point) 'invisible))
          (put-text-property (point-at-bol) (point-at-eol) 'fontified nil))))))

(defun org-bars-refresh-stars-after-change-function (beg end _)
  "Refontify heading stars on region that has changed.

This function is meant to be added to `after-change-functions'."
  (save-match-data
    (save-excursion
      ;; if `beg' is not on a heading, we must include the heading
      ;; `beg' belongs to in the refontification.
      (org-previous-visible-heading 2)
      (while (re-search-forward org-outline-regexp end t)
        (with-silent-modifications
          (unless (car (get-char-property-and-overlay (point) 'invisible))
            (put-text-property (point-at-bol) (point-at-eol) 'fontified nil)))))))

(defun org-bars-revert-heading-stars ()
  "Remove replacement stars `org-bars-stars' on every heading lines."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\**\\)\\(\\* \\)" nil t)
      (decompose-region (match-beginning 2) (match-end 2)))))

;;; compute prefixes text properties

(defvar org-bars-extra-pixels-height 0
  "Extra vertical pixel added headlines bars to get continuous bars.

If you don't use different font heights for headlines and
regular text, you can set `org-bars-extra-pixels-height' to 0.

See `org-bars-face-height', `org-bars-cycle-level-line-height' and
`org-bars-compute-prefixes'.")

(defun org-bars-face-height (face extra-pixels)
  "Return height in pixel of the face FACE.
This function takes care of the cases where the text has
been scaled up or down with `text-scale-increase' or `text-scale-decrease'."
  (let* ((face-font-height (aref (font-info (face-font face)) 3))
         (height-not-an-integer-p
          (not (integerp (face-attribute face :height nil t))))
         (scale (nth 2 (assq 'default text-scale-mode-remapping)))
         (height (cond ((and scale height-not-an-integer-p)
                        (ceiling (* scale face-font-height)))
                       (t face-font-height))))
    (+ height extra-pixels)))

(defun org-bars-cycle-level-line-height (level extra-pixel org-options)
  "Determine the line height for each heading line level.

ORG-OPTIONS is a plist:
:org-cycle-level-faces
    t if we want to cycle level bar color modulo `:org-n-level-faces'.
    In practice the value of `:org-cycle-level-faces' is
    `org-cycle-level-faces'.
:org-n-level-faces
    an integer between 1 to 8 included corresponding to the
    number of faces we take into account in the list `:org-level-faces'.
    In practice, the value of `:org-n-level-faces' is `org-n-level-faces'.
:org-level-faces
    a list of 8 faces.  In practice, the value of `:org-level-faces'
    is `org-level-faces'."
  (let ((cycle-p (plist-get org-options :org-cycle-level-faces))
        (n-faces (plist-get org-options :org-n-level-faces))
        (faces (plist-get org-options :org-level-faces)))
    (if cycle-p
        (org-bars-face-height (nth (% (1- level) n-faces) faces) extra-pixel)
      (org-bars-face-height (nth (1- (min level n-faces)) faces) extra-pixel))))

(defun org-bars-compute-prefixes ()
  "Compute prefix strings for regular text and headlines.

This function is meant to override `org-indent--compute-prefixes'
with an advice."

  (setq-local org-indent--heading-line-prefixes
              (make-vector org-indent--deepest-level nil))
  (setq-local org-indent--inlinetask-line-prefixes
              (make-vector org-indent--deepest-level nil))
  (setq-local org-indent--text-line-prefixes
              (make-vector org-indent--deepest-level nil))

  (let* ((gc-cons-threshold (max gc-cons-threshold (* 296 800000))) ; 800000 is the default value
         (width (window-font-width))
         (height (default-line-height))
         (color-options org-bars-color-options)
         (org-options `(:org-indent-indentation-per-level ,org-indent-indentation-per-level
                        :org-cycle-level-faces ,org-cycle-level-faces
                        :org-n-level-faces ,org-n-level-faces
                        :org-level-faces ,org-level-faces)))
    (dotimes (n org-indent--deepest-level)
      (let ((indentation (if (<= n 1) 0
                           (* (1- org-indent-indentation-per-level)
                              (1- n)))))
        ;; Headlines line prefixes.
        (let ((heading-prefix (make-string indentation ?*)))
          (aset org-indent--heading-line-prefixes
                n
                (if (<= n 1)
                    ""
                  (propertize
                   " " 'display (org-bars-xpm-image
                                 (1- n) width
                                 (org-bars-cycle-level-line-height
                                  n
                                  org-bars-extra-pixels-height
                                  org-options)
                                 color-options org-options))))

          ;; -------
          ;; BEG: part unchanged from `org-indent--compute-prefixes'
          ;; Inline tasks line prefixes
          (aset org-indent--inlinetask-line-prefixes
                n
                (cond ((<= n 1) "")
                      ((bound-and-true-p org-inlinetask-show-first-star)
                       (concat org-indent-inlinetask-first-star
                               (substring heading-prefix 1)))
                      (t (org-add-props heading-prefix nil 'face 'org-indent))))
          ;; END:
          ;; -------
          )

        ;; Text line prefixes.
        (aset org-indent--text-line-prefixes
              n
              (if (= n 0)
                  ""
                (propertize " " 'display (org-bars-xpm-image
                                          n width height
                                          color-options org-options))))))))

(defun org-bars-indent (&rest _r)
  "Indent current buffer with recomputed xpm image prefixes.

This is meant to be used as advice of `text-scale-increase'."
  (when org-bars-mode
    (org-bars-compute-prefixes)
    (org-with-wide-buffer (org-indent-indent-buffer))))

(defun org-bars-set-line-properties (level _indentation &optional heading)
  "Set prefix properties on current line an move to next one.

This function is meant to override `org-indent-set-line-properties'
with an advice.  Read it docstring for more details."
  (let* ((prefix
          (aref (pcase heading
                  (`nil org-indent--text-line-prefixes)
                  (`inlinetask org-indent--inlinetask-line-prefixes)
                  (_ org-indent--heading-line-prefixes))
                level)))
    ;; Add properties down to the next line to indent empty lines.
    (add-text-properties (line-beginning-position)
                         (line-beginning-position 2)
                         `(line-prefix ,prefix wrap-prefix ,prefix)))
  (forward-line))

;;; narrowing

(defvar-local org-bars-narrow-marker nil)

(defun org-bars-narrow ()
  "Remove bars that are not part of the buffer when narrowed.

Restore the bars when the buffer is widen.

This is meant to be used in `post-command-hook'."
  (cond
   ((buffer-narrowed-p)
    (when (member this-command
                  '(org-toggle-narrow-to-subtree
                    org-narrow-to-block org-narrow-to-element
                    org-narrow-to-subtree narrow-to-region narrow-to-defun
                    narrow-to-page narrow-to-defun-include-comments))
      (setq-local org-bars-narrow-marker (point-max-marker)))
    (let* ((pmax (point-max))
           (pmax+ (1+ (point-max))))
      (org-with-wide-buffer
       (when (< pmax (point-max))
         (org-indent-remove-properties pmax pmax+)))))
   ((member this-command '(widen org-toggle-narrow-to-subtree))
    (let ((marker+ (save-excursion
                     (goto-char org-bars-narrow-marker)
                     (line-beginning-position 3))))
      (org-indent-refresh-maybe org-bars-narrow-marker marker+ nil))
    (setq-local org-bars-narrow-marker nil))))

;;; org-bars-mode

(defvar-local org-bars-org-indent-mode nil
  "Hold the value of `org-indent-mode' before turning `org-bars-mode' on.")

(define-minor-mode org-bars-mode
  "Toggle `org-bars-mode' mode on or off."
  :global nil
  (cond
   (org-bars-mode
    (if (not (display-images-p))
        (progn
          (setq-local org-bars-mode nil)
          (message "org-bars-mode not turned on; your display can't display images."))
      (add-hook 'post-command-hook 'org-bars-narrow nil 'local)
      (advice-add 'text-scale-increase :after 'org-bars-indent)
      (advice-add 'org-indent-set-line-properties :override
                  'org-bars-set-line-properties)
      (advice-add 'org-indent--compute-prefixes :override
                  'org-bars-compute-prefixes)
      (advice-add 'org-get-level-face :override
                  'org-bars-get-level-face)
      (when org-bars-with-dynamic-stars-p
        (add-hook 'org-cycle-hook 'org-bars-refresh-stars nil t)
        (add-hook 'after-change-functions
                  'org-bars-refresh-stars-after-change-function nil t))
      (add-to-invisibility-spec '(org-bars))
      (setq-local org-bars-org-indent-mode (bound-and-true-p org-indent-mode))
      (org-indent-mode -1)
      (org-indent-mode 1)))
   (t
    (remove-hook 'post-command-hook 'org-bars-narrow 'local)
    (advice-remove 'text-scale-increase 'org-bars-indent)
    (advice-remove 'org-indent-set-line-properties
                   'org-bars-set-line-properties)
    (advice-remove 'org-indent--compute-prefixes
                   'org-bars-compute-prefixes)
    (advice-remove 'org-get-level-face
                   'org-bars-get-level-face)
    (when org-bars-with-dynamic-stars-p
      (remove-hook 'org-cycle-hook 'org-bars-refresh-stars t)
      (remove-hook 'after-change-functions
                   'org-bars-refresh-stars-after-change-function t)
      (org-bars-revert-heading-stars))
    (remove-from-invisibility-spec '(org-bars))
    (org-indent-mode -1)
    (if org-bars-org-indent-mode (org-indent-mode 1)))))

(provide 'org-bars)
;;; org-bars.el ends here
