;;; org-bars-xpm-image

(defun org-bars-xpm-image (level) ; maybe add arguments: width, heigth, vpadding
  "Return an image descriptor for level LEVEL in the org tree.

When `org-bars-mode' is on, the `line-prefix' property
of each line at the level LEVEL (LEVEL > 0) in the org tree, is set to
the XPM image produced by `org-bars-xpm-image'.

LEVEL must be strickly superior to 0."
  (let* ((identifier "/* XPM */\nstatic char *rule[] = {")
         (width 9) ; 9 calculated on my device with fill-column-indicator package
         (height 18) ; 18 calculated on my device with fill-column-indicator package
         (indentation org-indent-indentation-per-level)
         (colors 9) ; 8 org face levels + None color
         (dimensions (org-bars-xpm-dimensions level width indentation height colors))
         (color-spec
          (org-bars-xpm-color-spec '(:only-one-color nil
                                     :desaturate-level-faces 30
                                     :darken-level-faces 15)))
         (pixel-line (org-bars-pixel-line level width indentation))
         (raster (-reduce #'concat (-repeat height pixel-line)))
         (end "};")
         (data (concat identifier dimensions color-spec raster end)))
    `(image :type xpm
            :data ,data
            :mask heuristic
            :ascent center)))

(defun org-bars-xpm-dimensions (level width indentation height colors &optional vpadding)
  "Return the xpm dimensions.

In practice, `org-bars-xpm-dimensions' is called with INDENTATION argument
value equal to `org-indent-indentation-per-level'.

If VPADDING is non-nil, it corresponds to the number of None pixels
we add on the top and the bottom of the xpm images.  If the resulting
HEIGHT + (2*VPADDING) is superior of the line height (of the font)
where the image is inserted, this visually results in a padding arround
the line."
  (let ((width-str (number-to-string (* level width indentation)))
        (heigth-str (number-to-string
                     (if vpadding (+ (* 2 vpadding) height) height)))
        (character-per-pixel "1"))
    (concat "\"" width-str " " heigth-str " "
            (number-to-string colors) " " character-per-pixel "\",")))

;; if we want to add vertical padding to heading line we need
;; to add one extra left None pixel to every pixel-lines
(defun org-bars-pixel-line (level width indentation &optional only-one-color) ; maybe add argument: vpadding
  "Return the pixels line for level LEVEL with WIDTH being the character's width.

 (* WIDTH (- INDENTATION 1)) corresponds to the number of None pixels we add
after each level bar.  In practice, `org-bars-pixel-line' is called
with INDENTATION argument value equal to `org-indent-indentation-per-level'.

`org-bars-pixel-line' is used to construct the XPM image
used as `line-prefix' text property for each line for the level
LEVEL in the org tree."
  (let ((none-pixels (s-repeat (* (1- indentation) width) "0")))
    (concat "\""
            (s-join none-pixels
                    (--map (org-bars-pixel-bar it width only-one-color)
                           (number-sequence 1 level)))
            none-pixels
            "\",")))

(defun org-bars-pixel-bar (level width &optional only-one-color)
  "Return WIDTH pixels equal to 0 but one centered equal to LEVEL.

If ONLY-ONE-COLOR is non-nil, LEVEL pixel is replaced by a star *.
For instance:
    (org-bars-pixel-bar 3 9) -> \"000030000\".
    (org-bars-pixel-bar 3 9 t) -> \"0000*0000\"."
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

(defun org-bars-color-level (face desaturate darken)
  "Desaturate and darken foreground color of FACE.

0 <= DESATURATE <= 100 and 0 <= DARKEN <= 100.
See `color-desaturate-name' and `color-darken-name'."
  (-> (face-foreground face nil t)
      (color-desaturate-name desaturate)
      (color-darken-name darken)))

(defun org-bars-xpm-color-spec-with-level-faces (desaturate darken)
  "Return xpm color specification with color derivate from `org-level-faces'.

In the xpm color specification, 1 to 8 corresponds to derivate
colors from the foreground colors of the 8 faces in `org-level-faces'.
And 0 corresponds to the color None."
  (concat
   (-reduce
    #'concat
    (--map-indexed (concat "\"" (number-to-string (1+ it-index)) " c "
                           (org-bars-color-level it desaturate darken)
                           "\",")
                   org-level-faces))
   "\"0 c None\","))

(defun org-bars-xpm-color-spec-one-color (color)
  "Return xpm color specification with one color COLOR and None color.

In the xpm color specification, * corresponds to color COLOR
and 0 to None."
  (concat "\"* c " color "\","
          "\"0 c None\","))

(defun org-bars-xpm-color-spec (color-options)
  "Return xpm color specification respecting options COLOR-OPTIONS.

COLOR-OPTIONS is a plist with the same specification as
`org-bars-color-options' variable."
  (let ((only-one-color-p (plist-get color-options :only-one-color))
        (color            (plist-get color-options :bar-color))
        (desaturate       (plist-get color-options :desaturate-level-faces))
        (darken           (plist-get color-options :darken-level-faces)))
    (if only-one-color-p
        (org-bars-xpm-color-spec-one-color
         (or color (face-attribute 'default :foreground)))
      (org-bars-xpm-color-spec-with-level-faces (or desaturate 0)
                                                (or darken 0)))))

(defvar org-bars-color-options
  `(:only-one-color nil
    :bar-color ,(face-attribute 'default :foreground)
    :desaturate-level-faces 30
    :darken-level-faces 15)
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

;;;; comment

(comment ; dotimes, number-sequence, org-bars-xpm-image
 (insert (propertize " " 'display (org-bars-xpm-image 1)))
 (insert (propertize " " 'display (org-bars-xpm-image 2)))
 (insert (propertize " " 'display (org-bars-xpm-image 3)))
 (insert (propertize " " 'display (org-bars-xpm-image 4)))
 (insert (propertize " " 'display (org-bars-xpm-image 5)))
 (insert (propertize " " 'display (org-bars-xpm-image 17)))

 (dotimes (l 3) (message "%s" l))
 (number-sequence 1 5); (1 2 3 4 5)
 (number-sequence 1 1); (1)

 (s-join (s-repeat 2 "0") '("a" "b")) ; "a00b"
 (let ((none-pixels (s-repeat 2 "0")))
   (concat (s-join none-pixels '("a" "b")) none-pixels)) ; "a00b00"

 (color-desaturate-name "#ff0000" 0) ; "#ffff00000000"
 (color-darken-name "#ff0000" 0) ; "#ffff00000000"
 )

;;; font lock stuff for heading lines, org-get-level-face

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
  '(:empty "*"
    :invisible "+"
    :visible "-")
  "Plist of the strings used in place of the star \"* \" in heading lines.
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
           beg-1 (- end-2 2) '(invisible org-bars-invisible)))
      'default)
     ((eq n 2)
      (let* ((star (org-bars-star))
             (star-s (plist-get star :star))
             (star-f (plist-get star :face)))
        (compose-region beg-2 (1- end-2) star-s)
        (if (face-nontrivial-p star-f) star-f org-f)))
     (t (unless org-level-color-stars-only org-f)))))

(defun org-bars-refresh-stars (state)
  "Refontify all visible heading stars each time `org-cycle' command is used.

This function is meant to be added to `org-cycle-hook'."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-outline-regexp nil t)
      (with-silent-modifications
        (unless (car (get-char-property-and-overlay (point) 'invisible))
          (put-text-property (point-at-bol) (point-at-eol) 'fontified nil))))))

(defun org-bars-remove-replacement-stars ()
  "Remove replacement stars `org-bars-stars' on every heading lines."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\**\\)\\(\\* \\)" nil t)
      (decompose-region (match-beginning 2) (match-end 2)))))

;;; fix headings

;; the way bitmap are used to display leading stars in headings
;; (first stars but last one) works well when we use command
;; `org-promote-subtree' and `org-demote-subtree.'
;; But when we are after the last stars of the a heading and we
;; remove it with `backward-delete-char-untabify', the display
;; with bitmap of the heading is messed up.

(defun org-bars-fix-headings-before-change (start end)
  "Fix headings display.
It is meant to be used in `before-change-functions'."
  (when (org-at-heading-p)
    (add-text-properties (point-at-bol) (1+ (point-at-eol)) 'display nil)))

;;; compute prefixes

(defun org-bars-compute-prefixes ()
  "Compute prefix strings for regular text and headlines.

This function is meant to override `org-indent--compute-prefixes'
with an advice."
  ;; -------
  ;; BEG: part unchanged from `org-indent--compute-prefixes'
  (setq org-indent--heading-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--inlinetask-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))
  ;; END:
  ;; -------
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
                (propertize " " 'display (org-bars-xpm-image (1- n)))))

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
              (propertize " " 'display (org-bars-xpm-image n)))))))

;;; org-bars-mode

(define-minor-mode org-bars-mode
  "Toggle `org-bars-mode' mode on or off."
  :global nil
  (cond
   (org-bars-mode
    (push 'org-bars-fix-headings-before-change before-change-functions)
    (advice-add 'org-indent--compute-prefixes :override
                'org-bars-compute-prefixes)
    (advice-add 'org-get-level-face :override
                'org-bars-get-level-face)
    (add-hook 'org-cycle-hook 'org-bars-refresh-stars nil t)
    (add-to-invisibility-spec '(org-bars-invisible))
    (org-indent-mode -1)
    (org-indent-mode 1))
   (t
    (setq before-change-functions
          (delq 'org-bars-fix-headings-before-change before-change-functions))
    (advice-remove 'org-indent--compute-prefixes
                   'org-bars-compute-prefixes)
    (advice-remove 'org-get-level-face
                   'org-bars-get-level-face)
    (remove-hook 'org-cycle-hook 'org-bars-refresh-stars t)
    (org-bars-remove-replacement-stars)
    (remove-from-invisibility-spec '(org-bars-invisible))
    (org-indent-mode -1))))

(global-set-key (kbd "C-<f2>") 'org-bars-mode)
