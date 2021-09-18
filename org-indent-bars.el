;;; org-indent-bars-xpm-image

(defun org-indent-bars-xpm-image (level)
  "Return an image descriptor for level LEVEL in the org tree.

When `org-indent-bars-mode' is on, the `line-prefix' property
of each line at the level LEVEL (LEVEL > 0) in the org tree, is set to
the XPM image produced by `org-indent-bars-xpm-image'.

LEVEL must be strickly superior to 0."
  (let* ((identifier "/* XPM */\nstatic char *rule[] = {")
         (width 9) ; 9 calculated on my device with fill-column-indicator package
         (height 18) ; 18 calculated on my device with fill-column-indicator package
         (width-str (number-to-string (* level width)))
         (heigth-str (number-to-string height))
         (character-per-pixel "1")
         (colors "9") ; 8 org face levels + None color
         (dimensions
          (concat "\"" width-str " " heigth-str " " colors " " character-per-pixel "\","))
         (color-spec (org-indent-bars-xpm-color-spec 30 15))
         (pixel-bar (org-indent-bars-pixel-bar
                     (or (and (= 0 (mod level org-n-level-faces))
                              org-n-level-faces)
                         (mod level org-n-level-faces))
                     width))
         (pixel-line (org-indent-bars-pixel-line level width))
         (raster (-reduce #'concat (-repeat height pixel-line)))
         (end "};")
         (data (concat identifier dimensions color-spec raster end)))
    `(image :type xpm
            :data ,data
            :mask heuristic
            :ascent center)))

(defun org-indent-bars-pixel-line (level width)
  "Return the pixels line for level LEVEL with WIDTH being the width character.

`org-indent-bars-pixel-line' is used to construct the XPM image
used as `line-prefix' text property for each line for the level
LEVEL in the org tree."
  (concat "\""
          (-reduce #'concat (--map (org-indent-bars-pixel-bar it width)
                                   (number-sequence 1 level)))
          "\","))

(defun org-indent-bars-pixel-bar (level width)
  "Return WIDTH pixels equal to 0 but one centered equal to LEVEL.
For instance (org-indent-bars-pixel-bar 3 9) -> \"000030000\"."
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

(defun org-indent-bars-color-level (face desaturate darken)
  "Desaturate and darken foreground color of FACE."
  (-> (face-foreground face nil t)
      (color-desaturate-name desaturate)
      (color-darken-name darken)))

(defun org-indent-bars-xpm-color-spec (desaturate darken)
  "Return color specification used in XPM format.
1 to 8 corresponds to a modified version of the
foreground colors of the 8 faces in `org-level-faces'.
0 corresponds to color None."
  (concat
   (-reduce
    #'concat
    (--map-indexed (concat "\"" (number-to-string (1+ it-index)) " c "
                           (org-indent-bars-color-level it desaturate darken)
                           "\",")
                   org-level-faces))
   "\"0 c None\","))



;;;; comment

(comment ; dotimes, number-sequence, org-indent-bars-xpm-image
 (insert (propertize " " 'display (org-indent-bars-xpm-image 1)))
 (insert (propertize " " 'display (org-indent-bars-xpm-image 3)))
 (insert (propertize " " 'display (org-indent-bars-xpm-image 9)))
 (insert (propertize " " 'display (org-indent-bars-xpm-image 17)))

 (dotimes (l 3) (message "%s" l))
 (number-sequence 1 5); (1 2 3 4 5)
 (number-sequence 1 1); (1)
 )

;;;; test

(ert-deftest org-indent-bars-pixel-line-test ()
  (should (string= (org-indent-bars-pixel-line 3 3) "\"010020030\","))
  (should (string= (org-indent-bars-pixel-line 3 4) "\"001000200030\","))
  (should (string= (org-indent-bars-pixel-line 2 6) "\"000100000200\",")))

(ert-deftest org-indent-bars-pixel-bar-test ()
  (should (string= (org-indent-bars-pixel-bar 3 1) "3"))
  (should (string= (org-indent-bars-pixel-bar 3 2) "03"))
  (should (string= (org-indent-bars-pixel-bar 3 3) "030"))
  (should (string= (org-indent-bars-pixel-bar 3 5) "00300"))
  (should (string= (org-indent-bars-pixel-bar 3 4) "0030"))
  (should (string= (org-indent-bars-pixel-bar 1 6) "000100")))

(ert-deftest org-indent-bars-color-level-test ()
  (defface color-level '((t :foreground "#4dafc3")) "")
  (should
   (string= (org-indent-bars-color-level 'color-level 30 15)
            "#4eea6ed47558")))

(ert-deftest org-indent-bars-xpm-color-spec-test ()
  (defface color-level-1 '((t :foreground "#4dafc3")) "")
  (defface color-level-3 '((t :foreground "#d07391")) "")
  (defface color-level-2 '((t :foreground "#c97260")) "")
  (let ((org-level-faces '(color-level-1 color-level-2 color-level-3))
        (desaturate 30)
        (darken 15))
    (should
     (string= (org-indent-bars-xpm-color-spec desaturate darken)
              (concat "\"1 c #4eea6ed47558\","
                      "\"2 c #840860a45952\","
                      "\"3 c #942563507310\","
                      "\"0 c None\",")))))

;;; font lock stuff for heading line, org-get-level-face

(defun org-indent-bars-get-level-face (n)
  "Get the right face for match N in font-lock matching of headlines.

This function is meant to override `org-get-level-face' with an advice."
  (let* ((org-l0 (- (match-end 2) (match-beginning 1) 1))
         (org-l (if org-odd-levels-only (1+ (/ org-l0 2)) org-l0))
         (org-f (if org-cycle-level-faces
                    (nth (% (1- org-l) org-n-level-faces) org-level-faces)
                  (nth (1- (min org-l org-n-level-faces)) org-level-faces))))
    (cond
     ;; -------
     ;; BEG: part changed from `org-get-level-face'
     ;; ((eq n 1) (if org-hide-leading-stars 'org-hide org-f)) ; default
     ;; ((eq n 1) '(face nil display "÷"))
     ((eq n 1) `(face nil display
                      ,(or (= org-l0 1)
                           (org-indent-bars-xpm-image (1- org-l0))))) ; works
     ;; END: part changed from `org-get-level-face'
     ;; -------
     ((eq n 2) org-f)
     (t (unless org-level-color-stars-only org-f)))))


;;; fix headings

;; the way bitmap are used to display leading stars in headings
;; (first stars but last one) works well when we use command
;; `org-promote-subtree' and `org-demote-subtree.'
;; But when we are after the last stars of the a heading and we
;; remove it with `backward-delete-char-untabify', the display
;; with bitmap of the heading is messed up.

(defun org-indent-bars-fix-headings-before-change (start end)
  "Fix headings display.
It is meant to be used in `before-change-functions'."
  (when (org-at-heading-p)
    (add-text-properties (point-at-bol) (1+ (point-at-eol)) 'display nil)))

;;; compute prefixes

(defun org-indent-bars-compute-prefixes ()
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
  (dotimes (n org-indent--deepest-level)
    (let ((indentation (if (<= n 1) 0
                         (* (1- org-indent-indentation-per-level)
                            (1- n)))))
      ;; Headlines line prefixes.
      (let ((heading-prefix (make-string indentation ?*)))
        (aset org-indent--heading-line-prefixes
              n
              (org-add-props heading-prefix nil 'face 'org-indent))
        ;; Inline tasks line prefixes
        (aset org-indent--inlinetask-line-prefixes
              n
              (cond ((<= n 1) "")
                    ((bound-and-true-p org-inlinetask-show-first-star)
                     (concat org-indent-inlinetask-first-star
                             (substring heading-prefix 1)))
                    (t (org-add-props heading-prefix nil 'face 'org-indent)))))
      ;; END:
      ;; -------

      ;; Text line prefixes.
      (aset org-indent--text-line-prefixes
            n
            (if (= n 0)
                ""
              (propertize " " 'display (org-indent-bars-xpm-image n)))))))

;;; org-indent-bars-mode

(define-minor-mode org-indent-bars-mode
  "Toggle `org-indent-bars-mode' mode on or off."
  :global nil
  (cond
   (org-indent-bars-mode
    (push 'org-indent-bars-fix-headings-before-change before-change-functions)
    (advice-add 'org-indent--compute-prefixes :override
                'org-indent-bars-compute-prefixes)
    (advice-add 'org-get-level-face :override
                'org-indent-bars-get-level-face)
    (org-indent-mode -1)
    (org-indent-mode 1))
   (t
    (setq before-change-functions
          (delq 'org-indent-bars-fix-headings-before-change before-change-functions))
    (advice-remove 'org-indent--compute-prefixes
                   'org-indent-bars-compute-prefixes)
    (advice-remove 'org-get-level-face
                   'org-indent-bars-get-level-face)
    (org-map-entries
     '(add-text-properties (point-at-bol) (1+ (point-at-eol)) 'display nil))
    (org-indent-mode -1))))

(global-set-key (kbd "C-<f2>") 'org-indent-bars-mode)
