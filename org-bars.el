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
         (width-str (number-to-string (* level width indentation)))
         (heigth-str (number-to-string height))
         (character-per-pixel "1")
         (colors "9") ; 8 org face levels + None color
         (dimensions
          (concat "\"" width-str " " heigth-str " " colors " " character-per-pixel "\","))
         (color-spec (org-bars-xpm-color-spec 30 15))
         (pixel-line (org-bars-pixel-line level width indentation))
         (raster (-reduce #'concat (-repeat height pixel-line)))
         (end "};")
         (data (concat identifier dimensions color-spec raster end)))
    `(image :type xpm
            :data ,data
            :mask heuristic
            :ascent center)))

;; if we want to add vertical padding to heading line we need
;; to add one extra left None pixel to every pixel-lines
(defun org-bars-pixel-line (level width indentation) ; maybe add argument: vpadding
  "Return the pixels line for level LEVEL with WIDTH being the character's width.

 (* WIDTH (- INDENTATION 1)) corresponds to the number of None pixels we add
after each level bar.  In practice, `org-bars-pixel-line' is called
with INDENTATION argument value equal to `org-indent-indentation-per-level'.

`org-bars-pixel-line' is used to construct the XPM image
used as `line-prefix' text property for each line for the level
LEVEL in the org tree."
  (let ((none-pixels (s-repeat (* (1- indentation) width) "0")))
    (concat "\""
            (s-join none-pixels (--map (org-bars-pixel-bar it width)
                                       (number-sequence 1 level)))
            none-pixels
            "\",")))

(defun org-bars-pixel-bar (level width)
  "Return WIDTH pixels equal to 0 but one centered equal to LEVEL.
For instance (org-bars-pixel-bar 3 9) -> \"000030000\"."
  (cond
   ((= width 1) (number-to-string level))
   ((= width 2) (concat "0" (number-to-string level)))
   ((= (mod width 2) 1)
    (concat (s-repeat (floor (/ width 2.0)) "0")
            (number-to-string level)
            (s-repeat (floor (/ width 2.0)) "0")))
   (t
    (let* ((l-pixels (floor (/ width 2.0)))
           (r-pixels (1- l-pixels)))
      (concat (s-repeat l-pixels "0")
              (number-to-string level)
              (s-repeat r-pixels "0"))))))

(defun org-bars-color-level (face desaturate darken)
  "Desaturate and darken foreground color of FACE."
  (-> (face-foreground face nil t)
      (color-desaturate-name desaturate)
      (color-darken-name darken)))

(defun org-bars-xpm-color-spec (desaturate darken)
  "Return color specification used in XPM format.
1 to 8 corresponds to a modified version of the
foreground colors of the 8 faces in `org-level-faces'.
0 corresponds to color None."
  (concat
   (-reduce
    #'concat
    (--map-indexed (concat "\"" (number-to-string (1+ it-index)) " c "
                           (org-bars-color-level it desaturate darken)
                           "\",")
                   org-level-faces))
   "\"0 c None\","))



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
 )

;;;; test

;; (global-set-key (kbd "C-<f1>") (lambda () (interactive)(ert t)))
;; (global-set-key (kbd "C-<f1>") (lambda () (interactive)(ert "org-bars-pixel-line-test")))

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

;;;; tests

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
