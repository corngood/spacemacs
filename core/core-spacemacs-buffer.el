;;; core-spacemacs-buffer.el --- Spacemacs Core File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'core-dotspacemacs)
(eval-when-compile
  (defvar dotspacemacs-distribution)
  (defvar dotspacemacs-filepath)
  (defvar dotspacemacs-show-startup-list-numbers)
  (defvar dotspacemacs-startup-banner)
  (defvar dotspacemacs-startup-banner-scale)
  (defvar dotspacemacs-startup-buffer-show-icons)
  (defvar spacemacs-badge-official-png)
  (defvar spacemacs-banner-directory)
  (defvar spacemacs-banner-official-png)
  (defvar spacemacs-gplv3-official-png)
  (defvar spacemacs-version)
  (defvar configuration-layer-error-count))


(defconst spacemacs-buffer-version-info "0.999"
  "Current version used to display addition release information.")

(defconst spacemacs-buffer-name "*spacemacs*"
  "The name of the spacemacs buffer.")

(defconst spacemacs-buffer-logo-title "[S P A C E M A C S]"
  "The title displayed beneath the logo.")

(defconst spacemacs-buffer-buttons-startup-lists-offset 25
  "Relative position between the home buffer buttons and startup lists.")

(defconst spacemacs-buffer--window-width 80
  "Current width of the home buffer if responsive, 80 otherwise.
See `dotspacemacs-startup-buffer-responsive'.")

(defconst spacemacs-buffer--cache-file
  (expand-file-name (concat spacemacs-cache-directory "spacemacs-buffer.el"))
  "Cache file for various persistent data for the spacemacs startup buffer.")

(defvar spacemacs-buffer-startup-lists-length 20
  "Length used for startup lists with otherwise unspecified bounds.
Set to nil for unbounded.")

(defvar spacemacs-buffer-list-separator "\n\n")

(defvar spacemacs-buffer--release-note-version nil
  "If nil the release note is displayed.
If non nil it contains a version number, if the version number is lesser than
the current version the release note it displayed")

(defvar spacemacs-buffer--note-widgets nil
  "List of widgets used in currently inserted notes.
Allows to keep track of widgets to delete when removing them.")

(defvar spacemacs-buffer--current-note-type nil
  "Type of note currently displayed.")

(defvar spacemacs-buffer--fresh-install
  (not (file-exists-p dotspacemacs-filepath))
  "Non-nil if this Emacs instance if a fresh install.")

(defvar spacemacs-buffer--buttons-position nil
  "Horizontal position of the home buffer buttons.
Internal use, do not set this variable.")

(defvar spacemacs-buffer--random-banner nil
  "The random banner chosen.")

(defvar spacemacs-buffer-note-preview-lines 5
  "If it's a positive integer, show the notes first number of lines.
If nil, show the full note.")

(defvar spacemacs-buffer--note-preview-nr-of-removed-lines nil
  "Store the number of removed lines from the notes:
Quick Help and Release Notes.")

(defvar spacemacs-buffer--errors nil
  "List of errors during startup.")

(defvar spacemacs-buffer--idle-numbers-timer nil
  "This stores the idle numbers timer.")

(defvar spacemacs-buffer--startup-list-number nil
  "This accumulates the numbers that are typed in the home buffer.
It's cleared when the idle timer runs.")

(defvar spacemacs-buffer--last-width nil
  "Previous width of spacemacs-buffer.")

(defvar spacemacs-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (when dotspacemacs-show-startup-list-numbers
      (define-key map (kbd "0") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "1") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "2") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "3") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "4") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "5") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "6") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "7") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "8") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "9") 'spacemacs-buffer/jump-to-number-startup-list-line))

    (define-key map [down-mouse-1] 'spacemacs-buffer//mouse-1)
    (define-key map [mouse-1] 'ignore) ;; left button, avoid multiple clicks
    (define-key map [mouse-2] 'ignore) ;; mid button
    (define-key map [mouse-3] 'ignore) ;; right button
    (define-key map [drag-mouse-1] 'ignore)
    (define-key map [drag-mouse-2] 'ignore)
    (define-key map [drag-mouse-3] 'ignore)
    (define-key map (kbd "RET") 'spacemacs-buffer/return)

    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "J") 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)

    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "K") 'widget-backward)

    (define-key map (kbd "C-r") 'spacemacs-buffer/refresh)
    (define-key map "q" 'quit-window)
    map)
  "Keymap for spacemacs-buffer mode.")

(define-derived-mode spacemacs-buffer-mode special-mode "Spacemacs buffer"
  "Spacemacs major mode for startup screen.

\\{spacemacs-buffer-mode-map}"
  :group 'spacemacs
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (with-eval-after-load 'evil
    (progn
      (evil-set-initial-state 'spacemacs-buffer-mode 'motion)
      (evil-make-overriding-map spacemacs-buffer-mode-map 'motion)))
  (suppress-keymap spacemacs-buffer-mode-map t)
  (set-keymap-parent spacemacs-buffer-mode-map nil)
  (setq-local buffer-read-only t
              truncate-lines t))

(defun spacemacs-buffer//insert-ascii-banner-centered (file)
  "Insert the ascii banner contain in file and center it in the window.
FILE: the path to the file containing the banner."
  (insert
   (with-temp-buffer
     (insert-file-contents file)
     (let ((banner-width 0))
       (while (not (eobp))
         (let ((line-length (- (line-end-position) (line-beginning-position))))
           (when (< banner-width line-length)
             (setq banner-width line-length)))
         (forward-line 1))
       (goto-char 0)
       (let ((margin (max 0 (floor (/ (- spacemacs-buffer--window-width
                                         banner-width) 2)))))
         (while (not (eobp))
           (insert (make-string margin ?\s))
           (forward-line 1)))
       (insert "\n"))
     (buffer-string))))

(defun spacemacs-buffer/insert-banner-and-buttons ()
  "Choose a banner according to `dotspacemacs-startup-banner'and insert it.
in spacemacs buffer along with quick buttons underneath.
Easter egg:
Doge special text banner can be reachable via `999', `doge' or `random*'.
Doge special text banner for dark themes can be reachable via `997',
`doge-inverted' or `random*'.
Cate special text banner can de reachable via `998', `cat' or `random*'.
`random' ignore special banners whereas `random*' does not."
  (let ((banner (spacemacs-buffer//choose-banner))
        (buffer-read-only nil))
    (when banner
      (spacemacs-buffer/message (format "Banner: %s" banner))
      (if (image-type-available-p (intern (file-name-extension banner)))
          (spacemacs-buffer//insert-image-banner banner)
        (spacemacs-buffer//insert-ascii-banner-centered banner)))
    (spacemacs-buffer//insert-buttons)
    (spacemacs//redisplay)))

(defun spacemacs-buffer/display-startup-note ()
  "Decide of the startup note and display it if relevant."
  (when (file-exists-p spacemacs-buffer--cache-file)
    (load spacemacs-buffer--cache-file nil (not init-file-debug)))
  (cond
   (spacemacs-buffer--fresh-install
    ;; we assume the user is  new to spacemacs and open the quickhelp
    (spacemacs-buffer/toggle-note 'quickhelp)
    (setq spacemacs-buffer--release-note-version spacemacs-version)
    (spacemacs/dump-vars-to-file '(spacemacs-buffer--release-note-version)
                                 spacemacs-buffer--cache-file))
   ((or (not spacemacs-buffer--release-note-version)
        (version< spacemacs-buffer--release-note-version
                  spacemacs-version))
    ;; check the variable spacemacs-buffer--release-note-version
    ;; to decide whether we show the release note
    (spacemacs-buffer/toggle-note 'release-note)))
  (spacemacs//redisplay))

(defun spacemacs-buffer//choose-banner ()
  "Return the full path of a banner based on the dotfile value."
  (when dotspacemacs-startup-banner
    (cond ((eq 'official dotspacemacs-startup-banner)
           (if (and (display-graphic-p) (image-type-available-p 'png))
               spacemacs-banner-official-png
             (spacemacs-buffer//get-banner-path 1)))
          ((eq 'random dotspacemacs-startup-banner)
           (spacemacs-buffer//choose-random-text-banner))
          ((eq 'random* dotspacemacs-startup-banner)
           (spacemacs-buffer//choose-random-text-banner t))
          ((eq 'doge dotspacemacs-startup-banner)
           (spacemacs-buffer//get-banner-path 999))
          ((eq 'doge-inverted dotspacemacs-startup-banner)
           (spacemacs-buffer//get-banner-path 997))
          ((eq 'cat dotspacemacs-startup-banner)
           (spacemacs-buffer//get-banner-path 998))
          ((integerp dotspacemacs-startup-banner)
           (spacemacs-buffer//get-banner-path dotspacemacs-startup-banner))
          ((and dotspacemacs-startup-banner
                (image-type-available-p (intern (file-name-extension
                                                 dotspacemacs-startup-banner)))
                (display-graphic-p))
           (if (file-exists-p dotspacemacs-startup-banner)
               dotspacemacs-startup-banner
             (spacemacs-buffer/warning (format "could not find banner %s"
                                               dotspacemacs-startup-banner))
             (spacemacs-buffer//get-banner-path 1)))
          (t (spacemacs-buffer//get-banner-path 1)))))

(defun spacemacs-buffer//choose-random-text-banner (&optional all)
  "Return the full path of a banner chosen randomly.
If ALL is non-nil then truly all banners can be selected."
  (unless spacemacs-buffer--random-banner
    (let* ((files (directory-files spacemacs-banner-directory t ".*\.txt"))
           (count (length files))
           ;; -2 to remove the two last ones (easter eggs)
           (choice (random (- count (if all 0 2)))))
      (setq spacemacs-buffer--random-banner (nth choice files))))
  spacemacs-buffer--random-banner)

(defun spacemacs-buffer//get-banner-path (index)
  "Return the full path to banner with index INDEX."
  (concat spacemacs-banner-directory (format "%03d-banner.txt" index)))

(defun spacemacs-buffer//banner-fit-height-size ()
  "Calculate height of startup banner to fit buffer contents.
Returns height in units of line height with a minimum of 1."
  ;; first determine number of lines occupied by startup list
  (let* ((startup-list-line-height
          ;; the all-the-icons/nerd-icons package is not available here yet, but
          ;; require icons for just counting the lines in the
          ;; `dotspacemacs-startup-lists'
          (let ((dotspacemacs-startup-buffer-show-icons nil))
            (with-temp-buffer
              (spacemacs-buffer//do-insert-startupify-lists)
              (recentf-mode -1)
              (line-number-at-pos))))
         ;; We determine the maximum available banner height by subtracting the
         ;; number of lines in the home buffer contents (excl. logo and
         ;; startup-list), i.e. `26', and the number of lines in the startup
         ;; list from the total available text lines
         (image-height (- (window-text-height) 26 startup-list-line-height)))
    ;; return image-height with minimum of 3 line heights
    (max image-height 3)))

(defun spacemacs-buffer//insert-image-banner (banner)
  "Display an image banner.
BANNER: the path to an ascii banner file."
  (when (file-exists-p banner)
    (let* ((title spacemacs-buffer-logo-title)
           (spec (create-image banner))
           ;; we must use the scaled size for determining the correct
           ;; left-margin size
           (unscaled-size (image-size spec)) ;; size in 'canonical character units'
           (height (cdr unscaled-size)) ;; return size in units of line heights
           (scale (pcase dotspacemacs-startup-banner-scale
                    ('auto (let ((factor (/ (float (spacemacs-buffer//banner-fit-height-size))
                                            height)))
                             ;; return factor with maximum of 1
                             (min factor 1)))
                    (factor factor)))
           (size (cons (* scale (car unscaled-size)) (* scale (cdr unscaled-size))))
           (width (car size))
           (left-margin (max 0 (floor (- spacemacs-buffer--window-width width) 2))))
      ;; we scale the image by simply setting the scale property in the image-spec
      (plist-put (cdr spec) :scale scale)
      (insert (make-string left-margin ?\s))
      (insert-image spec)
      (insert "\n\n")
      (insert (make-string (max 0 (floor (/ (- spacemacs-buffer--window-width
                                               (+ (length title) 1)) 2))) ?\s))
      (insert (format "%s\n\n" title)))))

(defun spacemacs-buffer//insert-version ()
  "Insert the current version of Spacemacs and Emacs.
Right justified, based on the Spacemacs buffers window width."
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (let ((version (format "%s@%s (%s)"
                           spacemacs-version
                           emacs-version
                           dotspacemacs-distribution))
          (buffer-read-only nil))
      (insert (format (format "%%%ds"
                              (if (display-graphic-p)
                                  spacemacs-buffer--window-width
                                ;; terminal needs one less char
                                (1- spacemacs-buffer--window-width)))
                      version))
      (insert "\n\n"))))

(defvar spacemacs-buffer--icons-font nil
  "The icons font for spacemacs startup buffer. If nil means undetermined,
`none' means no icons-font should be applied, otherwise it's a icons-font.")

(defun spacemacs-buffer//determind-icons-font (&optional skip-require)
  "Return the icons font, `none' for non-applicatible."
  (unless spacemacs-buffer--icons-font
    (setq spacemacs-buffer--icons-font
          (or (pcase dotspacemacs-startup-buffer-show-icons
                ('t (when (or (eq dotspacemacs-default-icons-font 'nerd-icons)
                              (display-graphic-p))
                      dotspacemacs-default-icons-font))
                ('display-graphic-p (when (display-graphic-p)
                                      dotspacemacs-default-icons-font)))
              'none)))
  (unless (or skip-require
              (memq spacemacs-buffer--icons-font '(nil none))
              (fboundp (intern-soft (format "%s-octicon" spacemacs-buffer--icons-font))))
    (require spacemacs-buffer--icons-font))
  spacemacs-buffer--icons-font)

(defun spacemacs-buffer//font-icons-icon (str icon &rest params)
  "Apply the ICON if it available, otherwise return str, PARAMS for icon."
  (if-let* ((font (spacemacs-buffer//determind-icons-font))
            (icons
             (pcase font
               ('all-the-icons
                '((bookmark all-the-icons-octicon "bookmark" :face 'font-lock-keyword-face :v-adjust -0.05)
                  (calendar all-the-icons-octicon "calendar" :face 'font-lock-keyword-face :v-adjust -0.05)
                  (check all-the-icons-octicon "check" :face 'font-lock-keyword-face :v-adjust -0.05)
                  (dot all-the-icons-octicon "primitive-dot" :height 1.0 :v-adjust 0.01)
                  (error all-the-icons-material "error" :face 'font-lock-keyword-face)
                  (heart all-the-icons-faicon "heart" :height 0.8 :v-adjust -0.05)
                  (history all-the-icons-octicon "history" :face 'font-lock-keyword-face :v-adjust -0.05)
                  (radio-tower all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust -0.05)
                  (rocket all-the-icons-octicon "rocket" :face 'font-lock-keyword-face :v-adjust -0.05)
                  (warn all-the-icons-material "warning" :face 'font-lock-keyword-face)))
               ('nerd-icons
                '((bookmark nerd-icons-octicon "nf-oct-bookmark" :face 'font-lock-keyword-face :v-adjust -0.05)
                  (calendar nerd-icons-octicon "nf-oct-calendar" :face 'font-lock-keyword-face :v-adjust -0.05)
                  (check nerd-icons-octicon "nf-oct-check" :face 'font-lock-keyword-face :v-adjust -0.05)
                  (dot nerd-icons-octicon "nf-oct-dot" :height 1.0 :v-adjust 0.01)
                  (error nerd-icons-codicon "nf-cod-error" :face 'font-lock-keyword-face)
                  (heart nerd-icons-faicon "nf-fa-heart" :height 0.8 :v-adjust -0.05)
                  (history nerd-icons-octicon "nf-oct-history" :face 'font-lock-keyword-face :v-adjust -0.05)
                  (radio-tower nerd-icons-codicon "nf-cod-radio_tower" :height 0.8 :v-adjust -0.05)
                  (rotket nerd-icons-octicon "nf-oct-rocket" :face 'font-lock-keyword-face :v-adjust -0.05)
                  (warning nerd-icons-codicon "nf-cod-warning" :face 'font-lock-keyword-face)))))
            (res (alist-get icon icons)))
      (apply (car res) (cadr res) (or params (cddr res)))
    str))

(defun spacemacs-buffer//font-icons-icon-for-path (str path &rest args)
  "Apply the icon for PATH if it available, otherwise return str, PARAMS for icon."
  (if-let* ((font (spacemacs-buffer//determind-icons-font))
            ((not (memq font '(nil none)))))
      (if (file-remote-p path)
          (spacemacs-buffer//font-icons-icon str 'radio-tower)
        (if (file-directory-p path)
            (apply (intern (format "%s-icon-for-dir" font)) path args)
          (apply (intern (format "%s-icon-for-file" font)) (file-name-nondirectory path) (or args '(:height 0.8 :v-adjust -0.05)))))
    str))

(defun spacemacs-buffer//insert-footer ()
  "Insert the footer of the home buffer."
  (save-excursion
    (let* ((badge-path spacemacs-badge-official-png)
           (badge (when (and (display-graphic-p)
                             (image-type-available-p
                              (intern (file-name-extension badge-path))))
                    (create-image badge-path)))
           (badge-size (when badge (car (image-size badge))))
           (build-by (concat "Made with "
                             (spacemacs-buffer//font-icons-icon "heart" 'heart)
                             " by the community"))
           (proudly-free "Proudly free software")
           (gplv3-path spacemacs-gplv3-official-png)
           (gplv3 (when (and (display-graphic-p)
                             (image-type-available-p
                              (intern (file-name-extension gplv3-path))))
                    (create-image gplv3-path)))
           (gplv3-size (when gplv3 (car (image-size gplv3))))
           (buffer-read-only nil))
      (goto-char (point-max))
      (spacemacs-buffer/insert-page-break)
      (insert "\n")
      (when badge
        (insert-image badge)
        (spacemacs-buffer//center-line badge-size)
        (insert "\n\n"))
      (insert build-by)
      (spacemacs-buffer//center-line (length build-by))
      (insert "\n\n")
      (widget-create 'url-link
                     :tag proudly-free
                     :help-echo "What is free software?"
                     :mouse-face 'highlight
                     :follow-link "\C-m"
                     "https://www.gnu.org/philosophy/free-sw.en.html")
      (spacemacs-buffer//center-line (+ 2 (length proudly-free)))
      (when gplv3
        (insert "\n\n")
        (insert-image gplv3)
        (spacemacs-buffer//center-line gplv3-size)
        (insert "\n")))))

(defmacro spacemacs-buffer||notes-adapt-caption-to-width (caption
                                                          caption-length
                                                          width)
  "Adapt caption string's length to the note's frame current width.
For internal use in `spacemacs-buffer//notes-render-framed-text'.
CAPTION: string to be encrusted onto the note's frame
CAPTION-LENGTH: length of the caption
WIDTH: current external width of the note's frame."
  `(when (> ,caption-length (- ,width 6)) ; minimum frame width is 6
     (if (> ,width 8)
         (setq ,caption (concat (substring ,caption
                                           0
                                           (min -3 (- (- ,width 6 3)
                                                      ,caption-length)))
                                "..."))
       (setq ,caption nil
             ,caption-length 0))))

(defun spacemacs-buffer//if-note-preview-remove-rest-of-note ()
  "If `spacemacs-buffer-note-preview-lines' is a positive integer,
remove the rest of the note, after the variables line number."
  (when (and (integerp spacemacs-buffer-note-preview-lines)
             (> spacemacs-buffer-note-preview-lines 0))
    (goto-char (point-min))
    (forward-line spacemacs-buffer-note-preview-lines)
    (let* ((first-removed-line (line-number-at-pos (point)))
           (last-removed-line (line-number-at-pos (point-max))))
      (setq spacemacs-buffer--note-preview-nr-of-removed-lines
            (- last-removed-line first-removed-line))
      (delete-region (point) (point-max)))))

(defun spacemacs-buffer//notes-render-framed-text
    (content &optional topcaption botcaption hpadding max-width min-width)
  "Return a formatted string framed with curved lines.
The width of the created frame is the width of the content, unless it does not
satisfy max-width or min-width.  Note that max-width can be limited by the
window's width.
CONTENT can be a text or a filepath.
TOPCAPTION is a text to be encrusted at the top of the frame.
BOTCAPTION is a text to be encrusted at the bottom of the frame.
HPADDING is the horizontal spacing between the text and the frame.  The vertical
         spacing is always one line.
MAX-WIDTH is the maximum width of the frame,  frame included.  When
          `dotspacemacs-startup-buffer-responsive' is t, MAX-WIDTH will be
          limited to the window's width.  MAX-WIDTH takes precedence over
          MIN-WIDTH.
MIN-WIDTH is the minimal width of the frame, frame included.  The frame will not
          shrink any thinner than MIN-WIDTH characters unless MAX-WIDTH says
          otherwise."
  (with-temp-buffer
    (if (not (file-exists-p content))
        (insert content)
      (insert-file-contents content)
      (spacemacs-buffer//if-note-preview-remove-rest-of-note)
      (goto-char (point-max))
      (when (eq ?\n (char-before))    ;; remove additional newline at eof
        (delete-char -1)))
    (let* ((hpadding (if hpadding hpadding 1))
           (text-width (spacemacs-buffer//get-buffer-width))
           (width (+ 2 (* 2 hpadding) text-width))
           (fill-column text-width)
           (sentence-end-double-space nil)    ; needed by fill-region
           (paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] \\|[ \t]*[0-9]+[.)] ")
           (topcaption-length (if topcaption (length topcaption) 0))
           (botcaption-length (if botcaption (length botcaption) 0)))
      ;; min-width defaults to 1
      ;; max-width defaults to width, but truncated between min-width and window-width
      (setq min-width (or min-width 1)
            max-width (min (max (or max-width width) min-width)
                           spacemacs-buffer--window-width))
      (cond
       ((< width min-width)
        (setq width min-width
              fill-column (max 0 (- min-width 2 (* hpadding 2)))))
       ((> width max-width)
        (setq width max-width
              fill-column (max 0 (- max-width 2 (* hpadding 2))))))
      (spacemacs-buffer||notes-adapt-caption-to-width topcaption
                                                      topcaption-length
                                                      width)
      (spacemacs-buffer||notes-adapt-caption-to-width botcaption
                                                      botcaption-length
                                                      width)
      (fill-region (point-min) (point-max) nil nil)
      (concat
       "╭─" (when topcaption (propertize (concat " " topcaption " ")
                                         'face
                                         '(:weight bold)))
       (make-string (max 0 (- width (if topcaption 6 4) topcaption-length)) ?─) "─╮\n"
       (spacemacs-buffer//notes-render-framed-line "" width hpadding)
       (mapconcat (lambda (line)
                    (spacemacs-buffer//notes-render-framed-line line width hpadding))
                  (split-string (buffer-string) "\n" nil) "")
       (spacemacs-buffer//notes-render-framed-line "" width hpadding)
       "╰─" (when botcaption (propertize (concat " " botcaption " ")
                                         'face '(:weight bold)))
       (make-string (max 0 (- width (if botcaption 6 4) botcaption-length)) ?─)
       "─╯" (when botcaption "\n")))))

(defun spacemacs-buffer//notes-render-framed-line (line width hpadding)
  "Return a formatted LINE with borders of a frame on each side.
WIDTH: external width of the frame.  LINE should be shorter than WIDTH.
HPADDING: horizontal padding on both sides of the framed string."
  (let ((fill (max 0 (- width 2 hpadding (length line)))))
    (concat "│" (make-string hpadding ?\s) line (make-string fill ?\s)
            "│\n")))

(defun spacemacs-buffer//notes-insert-note
    (file topcaption botcaption &optional additional-widgets)
  "Insert the release note just under the banner.
FILE: the file that contains the content to show.
TOPCAPTION: the title of the note.
BOTCAPTION: a text to be encrusted at the bottom of the frame.
ADDITIONAL-WIDGETS: a function for inserting a widget under the frame."
  (save-excursion
    (goto-char (point-min))
    (search-forward "Search in Spacemacs\]" nil "move to limit") ; TODO: this is dirty
    (forward-line)
    (let* ((buffer-read-only nil)
           (note (concat "\n"
                         (spacemacs-buffer//notes-render-framed-text file
                                                                     topcaption
                                                                     botcaption
                                                                     2
                                                                     nil
                                                                     80))))
      (save-restriction
        (narrow-to-region (point) (point))
        (add-to-list 'spacemacs-buffer--note-widgets (widget-create 'text :format "%v" note))
        (let* ((width (spacemacs-buffer//get-buffer-width))
               (padding (max 0 (floor (/ (- spacemacs-buffer--window-width
                                            width) 2)))))
          (goto-char (point-min))
          (while (not (eobp))
            (beginning-of-line)
            (insert (make-string padding ?\s))
            (forward-line))))
      (save-excursion
        (while (re-search-backward "\\[\\[\\(.*\\)\\]\\]" nil t)
          (make-text-button (match-beginning 1)
                            (match-end 1)
                            'type 'help-url
                            'help-args (list (match-string 1)))))
      (when additional-widgets
        (funcall additional-widgets))
      (spacemacs-buffer//center-line)
      (delete-trailing-whitespace (line-beginning-position)
                                  (line-end-position)))))

(defun spacemacs-buffer//notes-insert-quickhelp ()
  "Insert quickhelp."
  (let ((widget-func
         (lambda ()
           (when spacemacs-buffer-note-preview-lines
             (widget-insert "\n")
             (let ((full-note-link-text
                    (format "Click to show the full note (%s more lines)"
                            spacemacs-buffer--note-preview-nr-of-removed-lines)))
               (add-to-list
                'spacemacs-buffer--note-widgets
                (widget-create
                 'push-button
                 :tag (propertize
                       full-note-link-text 'face 'font-lock-warning-face)
                 :help-echo "Open full note."
                 :action (lambda (&rest ignore)
                           (let ((cursor-pos-before-showing-full-note (point))
                                 (spacemacs-buffer-note-preview-lines nil))
                             ;; close note
                             (spacemacs-buffer/toggle-note 'quickhelp)
                             ;; open full note
                             (spacemacs-buffer/toggle-note 'quickhelp)
                             ;; cursor to beg of first line after preview
                             (goto-char cursor-pos-before-showing-full-note)
                             (progn (forward-line -2)
                                    (back-to-indentation)
                                    (forward-word)
                                    (backward-word))))
                 :mouse-face 'highlight
                 :follow-link "\C-m"))
               (spacemacs-buffer//center-line)
               (widget-insert "\n")))
           (add-to-list
            'spacemacs-buffer--note-widgets
            (widget-create 'push-button
                           :tag (propertize "Evil Tutorial"
                                            'face 'font-lock-keyword-face)
                           :help-echo
                           "Teach you how to use Vim basics."
                           :action (lambda (&rest ignore)
                                     (call-interactively #'evil-tutor-start))
                           :mouse-face 'highlight
                           :follow-link "\C-m"))
           (widget-insert " ")
           (add-to-list
            'spacemacs-buffer--note-widgets
            (widget-create 'push-button
                           :tag (propertize "Emacs Tutorial"
                                            'face 'font-lock-keyword-face)
                           :help-echo "Teach you how to use Emacs basics."
                           :action (lambda (&rest ignore)
                                     (call-interactively #'help-with-tutorial))
                           :mouse-face 'highlight
                           :follow-link "\C-m"))
           (widget-insert " ")
           (add-to-list
            'spacemacs-buffer--note-widgets
            (widget-create 'push-button
                           :tag (propertize "Vim Migration Guide"
                                            'face 'font-lock-keyword-face)
                           :help-echo "Documentation for former vim users."
                           :action (lambda (&rest ignore)
                                     (spacemacs/view-org-file
                                      (concat spacemacs-docs-directory
                                              "VIMUSERS.org") "^" 'all))
                           :mouse-face 'highlight
                           :follow-link "\C-m"))
           (widget-insert " ")
           (add-to-list
            'spacemacs-buffer--note-widgets
            (widget-create 'push-button
                           :tag (propertize "Close note"
                                            'face '(:foreground "orangeRed"))
                           :help-echo "Close note"
                           :action
                           (lambda (&rest ignore)
                             (spacemacs-buffer/toggle-note 'quickhelp)
                             (search-backward "[?"))
                           :mouse-face 'highlight
                           :follow-link "\C-m"))
           ;; center the buttons: Evil Tutorial, Emacs Tutorial, etc.
           (spacemacs-buffer//center-line)
           (widget-insert "\n"))))
    (spacemacs-buffer//notes-insert-note (concat spacemacs-info-directory
                                                 "quickhelp.txt")
                                         "Quick Help"
                                         nil
                                         widget-func)))

(defun spacemacs-buffer//notes-insert-release-note ()
  "Insert release note."
  (let ((widget-func
         (lambda ()
           (when spacemacs-buffer-note-preview-lines
             (let ((full-note-link-text
                    (format "Click to show the full note (%s more lines)"
                            spacemacs-buffer--note-preview-nr-of-removed-lines)))
               (add-to-list
                'spacemacs-buffer--note-widgets
                (widget-create
                 'push-button
                 :tag (propertize
                       full-note-link-text 'face 'font-lock-warning-face)
                 :help-echo "Open full note."
                 :action (lambda (&rest ignore)
                           (let ((cursor-pos-before-showing-full-note (point))
                                 (spacemacs-buffer-note-preview-lines nil))
                             ;; close note
                             (spacemacs-buffer/toggle-note 'release-note)
                             ;; open full note
                             (spacemacs-buffer/toggle-note 'release-note)
                             ;; cursor to beg of first line after preview
                             (goto-char cursor-pos-before-showing-full-note)
                             (progn
                               (back-to-indentation)
                               (forward-line -1)
                               (forward-char 3))))
                 :mouse-face 'highlight
                 :follow-link "\C-m"))
               (spacemacs-buffer//center-line)
               (widget-insert "\n")))
           (add-to-list
            'spacemacs-buffer--note-widgets
            (widget-create 'push-button
                           :tag (propertize "Click here for full change log"
                                            'face 'font-lock-warning-face)
                           :help-echo "Open the full change log."
                           :action
                           (lambda (&rest ignore)
                             (funcall 'spacemacs/view-org-file
                                      (concat spacemacs-start-directory
                                              "CHANGELOG.org")
                                      (format "Release %s.x"
                                              spacemacs-buffer-version-info)
                                      'subtree))
                           :mouse-face 'highlight
                           :follow-link "\C-m"))
           (widget-insert " ")
           (add-to-list
            'spacemacs-buffer--note-widgets
            (widget-create 'push-button
                           :tag (propertize "Close note"
                                            'face '(:foreground "orangeRed"))
                           :help-echo "Close note"
                           :action
                           (lambda (&rest ignore)
                             (spacemacs-buffer/toggle-note 'release-note)
                             (search-backward "[Release"))
                           :mouse-face 'highlight
                           :follow-link "\C-m"))
           ;; center the buttons: Click here for full change log and Close note
           (spacemacs-buffer//center-line)
           (widget-insert "\n"))))
    (spacemacs-buffer//notes-insert-note (concat spacemacs-release-notes-directory
                                                 spacemacs-buffer-version-info
                                                 ".txt")
                                         (format "Important Notes (Release %s.x)"
                                                 spacemacs-buffer-version-info)
                                         "Update your dotfile (SPC f e D) and\
 packages after every update"
                                         widget-func))
  (setq spacemacs-buffer--release-note-version nil)
  (spacemacs/dump-vars-to-file '(spacemacs-buffer--release-note-version)
                               spacemacs-buffer--cache-file))

(defun spacemacs-buffer//note-removal-cleanup ()
  "After removing a home buffer note.
Remove: additional empty lines (leaving only one),
and the trailing whitespace."
  (let ((inhibit-read-only t))
    (delete-blank-lines)
    (delete-region (line-beginning-position) (line-end-position))))

(defun spacemacs-buffer//widget-text-note-beg-pos ()
  (let (pos)
    (dolist (w spacemacs-buffer--note-widgets)
      (when (eq (car w) 'text)
        (setq pos (marker-position (widget-get w :from)))))
    pos))

(defun spacemacs-buffer//notes-clear-notes-and-widgets ()
  "Remove existing note widgets if exists."
  (when spacemacs-buffer--note-widgets
    (save-excursion
      (let ((note-beg-pos (spacemacs-buffer//widget-text-note-beg-pos)))
        (mapc 'widget-delete spacemacs-buffer--note-widgets)
        (goto-char note-beg-pos)
        (spacemacs-buffer//note-removal-cleanup)))
    (setq spacemacs-buffer--note-widgets nil)
    (setq spacemacs-buffer--release-note-version spacemacs-version)
    (spacemacs/dump-vars-to-file
     '(spacemacs-buffer--release-note-version) spacemacs-buffer--cache-file)))

(defun spacemacs-buffer//notes-redisplay-current-note ()
  "Delete and rediplay the currently displayed note."
  (spacemacs-buffer//notes-clear-notes-and-widgets)
  (let ((type spacemacs-buffer--current-note-type))
    (cond
     ((eq type 'quickhelp) (spacemacs-buffer//notes-insert-quickhelp))
     ((eq type 'release-note) (spacemacs-buffer//notes-insert-release-note))
     (t))))

(defun spacemacs-buffer/toggle-note (type)
  "Toggle the displayed note based on TYPE.
If TYPE is nil or unknown, just remove the currently displayed note.  Currently
allowed types are `quickhelp' and `release-note'"
  (spacemacs-buffer//notes-clear-notes-and-widgets)
  (if (or (eq spacemacs-buffer--current-note-type nil)
          (not (eq spacemacs-buffer--current-note-type type)))
      (progn
        (setq spacemacs-buffer--current-note-type type)
        (cond
         ((eq type 'quickhelp) (spacemacs-buffer//notes-insert-quickhelp))
         ((eq type 'release-note) (spacemacs-buffer//notes-insert-release-note))
         (t (setq spacemacs-buffer--current-note-type nil)
            (message "Unknown note type: %s" 'type))))
    (setq spacemacs-buffer--current-note-type nil)))

(defun spacemacs-buffer/set-mode-line (format &optional redisplay)
  "Set mode-line format for spacemacs buffer.
FORMAT: the `mode-line-format' variable Emacs will use to build the mode-line.
If REDISPLAY is non-nil then force a redisplay as well"
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (setq mode-line-format format))
  (when redisplay (spacemacs//redisplay)))

(defun spacemacs-buffer/message (msg &rest args)
  "Display MSG in *Messages* prepended with '(Spacemacs)'.
The message is displayed only if `init-file-debug' is non nil.
ARGS: format string arguments."
  (when init-file-debug
    (message "(Spacemacs) %s" (apply 'format msg args))))

(defun spacemacs-buffer/error (msg &rest args)
  "Display MSG as an Error message in `*Messages*' buffer.
ARGS: format string arguments."
  (let ((msg (apply 'format msg args)))
    (message "(Spacemacs) Error: %s" msg)
    (when message-log-max
      (add-to-list 'spacemacs-buffer--errors msg 'append))))

(defvar spacemacs-buffer--warnings nil
  "List of warnings during startup.")

(defun spacemacs-buffer/warning (msg &rest args)
  "Display MSG as a warning message but in buffer `*Messages*'.
ARGS: format string arguments."
  (let ((msg (apply 'format msg args)))
    (message "(Spacemacs) Warning: %s" msg)
    (when message-log-max
      (add-to-list 'spacemacs-buffer--warnings msg 'append))))

(defun spacemacs-buffer/insert-page-break ()
  "Insert a page break line in spacemacs buffer."
  (spacemacs-buffer/append "\n\n"))

(defun spacemacs-buffer/append (msg &optional messagebuf)
  "Append MSG to spacemacs buffer.
If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (when messagebuf
        (message "(Spacemacs) %s" msg)))))

(defun spacemacs-buffer/replace-last-line (msg &optional messagebuf)
  "Replace the last line of the spacemacs buffer with MSG.
If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (when messagebuf
        (message "(Spacemacs) %s" msg)))))

(eval-and-compile
  (defun spacemacs-buffer//startup-list-jump-func-name (str)
    "Given a string, return a spacemacs-buffer function name.

Given:           Return:
\"[?]\"            \"spacemacs-buffer/jump-to-[?]\"
\"Recent Files:\"  \"spacemacs-buffer/jump-to-recent-files\""
    (let ((s (downcase str)))
      ;; remove last char if it's a colon
      (when (string-match ":$" s)
        (setq s (substring s nil (1- (length s)))))
      ;; replace any spaces with a dash
      (setq s (replace-regexp-in-string " " "-" s))
      (concat "spacemacs-buffer/jump-to-" s))))

(defmacro spacemacs-buffer||add-shortcut
    (shortcut-char search-label &optional no-next-line)
  "Add a single-key keybinding for quick navigation in the home buffer.
Navigation is done by searching for a specific word in the buffer.
SHORTCUT-CHAR: the key that the user will have to press.
SEARCH-LABEL: the word the cursor will be brought under (or on).
NO-NEXT-LINE: if nil the cursor is brought under the searched word.

Define a named function: spacemacs-buffer/jump-to-...
for the shortcut. So that a descriptive name is shown,
in for example the `view-lossage' (C-h l) buffer:
 r                      ;; spacemacs-buffer/jump-to-recent-files
 p                      ;; spacemacs-buffer/jump-to-projects
instead of:
 r                      ;; anonymous-command
 p                      ;; anonymous-command"
  (let ((func-name-symbol
         (intern (spacemacs-buffer//startup-list-jump-func-name search-label))))
    `(progn (defun ,func-name-symbol ()
              (interactive)
              (unless (search-forward ,search-label (point-max) t)
                (search-backward ,search-label (point-min) t))
              ,@(unless no-next-line
                  '((forward-line 1)))
              (back-to-indentation))
            (define-key spacemacs-buffer-mode-map ,shortcut-char ',func-name-symbol))))

(defun spacemacs-buffer//center-line (&optional real-width)
  "When point is at the end of a line, center it.
REAL-WIDTH: the real width of the line.  If the line contains an image, the size
            of that image will be considered to be 1 by the calculation method
            used in this function.  As a consequence, the caller must calculate
            himself the correct length of the line taking into account the
            images he inserted in it."
  (let* ((width (or real-width (current-column)))
         (margin (max 0 (floor (/ (- spacemacs-buffer--window-width
                                     width)
                                  2)))))
    (beginning-of-line)
    (insert (make-string margin ?\s))
    (end-of-line)))

(defun spacemacs-buffer//insert-buttons ()
  "Create and insert the interactive buttons under Spacemacs banner."
  (goto-char (point-max))
  (spacemacs-buffer||add-shortcut "m" "[?]" t)
  (widget-create 'url-link
                 :tag (propertize "?" 'face 'font-lock-doc-face)
                 :help-echo "Open the quickhelp."
                 :action (lambda (&rest ignore)
                           (spacemacs-buffer/toggle-note 'quickhelp))
                 :mouse-face 'highlight
                 :follow-link "\C-m")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Homepage" 'face 'font-lock-keyword-face)
                 :help-echo "Open the Spacemacs GitHub page in your browser."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "https://spacemacs.org")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Documentation" 'face 'font-lock-keyword-face)
                 :help-echo "Open the Spacemacs documentation in your browser."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "https://spacemacs.org/doc/DOCUMENTATION.html")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Gitter Chat" 'face 'font-lock-keyword-face)
                 :help-echo
                 "Ask questions and chat with fellow users in our chat room."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "https://gitter.im/syl20bnr/spacemacs")
  (insert " ")
  (widget-create 'push-button
                 :help-echo "GPLv3 copying conditions."
                 :action (lambda (&rest ignore)
                           (find-file (concat spacemacs-start-directory "LICENSE"))
                           (read-only-mode))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Licensing" 'face 'font-lock-keyword-face))
  (let ((len (- (line-end-position)
                (line-beginning-position))))
    (spacemacs-buffer//center-line)
    (setq spacemacs-buffer--buttons-position (- (line-end-position)
                                                (line-beginning-position)
                                                len)))
  (insert "\n")
  (widget-create 'push-button
                 :help-echo "Update all ELPA packages to the latest versions."
                 :action (lambda (&rest ignore)
                           (configuration-layer/update-packages))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Update Packages" 'face 'font-lock-keyword-face))
  (insert " ")
  (widget-create 'push-button
                 :help-echo
                 "Rollback ELPA package updates if something got borked."
                 :action (lambda (&rest ignore)
                           (call-interactively 'configuration-layer/rollback))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Rollback Package Update"
                             'face 'font-lock-keyword-face))
  (spacemacs-buffer//center-line)
  (insert "\n")
  (widget-create 'push-button
                 :tag (propertize "Release Notes"
                                  'face 'font-lock-preprocessor-face)
                 :help-echo "Hide or show the Changelog"
                 :action (lambda (&rest ignore)
                           (spacemacs-buffer/toggle-note 'release-note))
                 :mouse-face 'highlight
                 :follow-link "\C-m")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Search in Spacemacs"
                                  'face 'font-lock-function-name-face)
                 :help-echo "Search Spacemacs contents."
                 :action
                 (lambda (&rest ignore)
                   (let ((comp-frontend
                          (cond
                           ((configuration-layer/layer-used-p 'helm)
                            'helm-spacemacs-help)
                           ((configuration-layer/layer-used-p 'ivy)
                            'ivy-spacemacs-help)
                           ((configuration-layer/layer-used-p 'compleseus)
                            'compleseus-spacemacs-help))))
                     (call-interactively comp-frontend)))
                 :mouse-face 'highlight
                 :follow-link "\C-m")
  (spacemacs-buffer//center-line)
  (insert "\n"))

(defun spacemacs-buffer//insert-string-list (list-display-name list)
  "Insert a non-interactive startup list in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: a list of strings displayed as entries."
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert
             "\n"
             (with-temp-buffer
               (insert el)
               (fill-paragraph)
               (goto-char (point-min))
               (insert "    - ")
               (while (= 0 (forward-line))
                 (insert "      "))
               (buffer-string))))
          list)))

(defun spacemacs-buffer//insert-file-list (list-display-name list)
  "Insert an interactive list of files in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: a list of string pathnames made interactive in this function.

If LIST-DISPLAY-NAME is \"Recent Files:\":
prepend each list item with a number starting at: 1
The numbers indicate that the file can be opened,
by pressing its number key."
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (let ((button-prefix
                   (concat
                    "\n    "
                    (when dotspacemacs-show-startup-list-numbers
                      (format "%2s " (number-to-string spacemacs-buffer--startup-list-nr)))
                    " "
                    (spacemacs-buffer//font-icons-icon-for-path "" el)
                    " "))
                  (button-text (abbreviate-file-name el)))
              (insert button-prefix)
              (widget-create 'push-button
                             :action `(lambda (&rest ignore)
                                        (find-file-existing ,el))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :button-face nil
                             :format "%[%t%]" button-text))
            (setq spacemacs-buffer--startup-list-nr
                  (1+ spacemacs-buffer--startup-list-nr)))
          list)))

(defun spacemacs-buffer//insert-files-by-dir-list
    (list-display-name grouped-list)
  "Insert an interactive grouped list of files in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
GROUPED-LIST: a list of string pathnames made interactive in this function."
  (when (car-safe grouped-list)
    (insert list-display-name)
    (mapc (lambda (group)
            (let* ((group-remote-p (file-remote-p (car group)))
                   (button-prefix
                    (concat
                     "\n    "
                     (when dotspacemacs-show-startup-list-numbers
                       (format "%2s " (number-to-string spacemacs-buffer--startup-list-nr)))
                     " "
                     (if group-remote-p
                         (spacemacs-buffer//font-icons-icon "" 'radio-tower)
                       (spacemacs-buffer//font-icons-icon-for-path "" (car group)))
                     " "))
                   (button-text-project (abbreviate-file-name (car group))))
              (insert button-prefix)
              (widget-create 'push-button
                             :action `(lambda (&rest ignore)
                                        (find-file-existing ,(car group)))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :format "%[%t%]" button-text-project)
              (setq spacemacs-buffer--startup-list-nr
                    (1+ spacemacs-buffer--startup-list-nr))
              (mapc (lambda (el)
                      (let* ((button-prefix
                              (concat
                               "\n        "
                               (when dotspacemacs-show-startup-list-numbers
                                 (format "%2s " (number-to-string spacemacs-buffer--startup-list-nr)))
                               " "
                               (if (or group-remote-p (file-remote-p (concat (car group) el)))
                                   (spacemacs-buffer//font-icons-icon "" 'radio-tower)
                                 (spacemacs-buffer//font-icons-icon-for-path "" (car group)))
                               " "))
                             (button-text-filename (string-trim-left (expand-file-name el)
                                                                     (regexp-quote (car group)))))
                        (insert button-prefix)
                        (widget-create 'push-button
                                       :action `(lambda (&rest ignore) (find-file-existing ,el))
                                       :mouse-face 'highlight
                                       :follow-link "\C-m"
                                       :button-prefix ""
                                       :button-suffix ""
                                       :format "%[%t%]" button-text-filename))
                      (setq spacemacs-buffer--startup-list-nr
                            (1+ spacemacs-buffer--startup-list-nr)))
                    (cdr group))))
          grouped-list)))

(defun spacemacs-buffer//insert-bookmark-list (list-display-name list)
  "Insert an interactive list of bookmarks entries (if any) in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: a list of string bookmark names made interactive in this function."
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (let* ((filename (bookmark-get-filename el))
                   (button-prefix
                    (concat
                     "\n    "
                     (when dotspacemacs-show-startup-list-numbers
                       (format "%2s " (number-to-string spacemacs-buffer--startup-list-nr)))
                     " "
                     (spacemacs-buffer//font-icons-icon-for-path "" filename)
                     " "))
                   (button-text
                    (if filename
                        (format "%s - %s"
                                el (abbreviate-file-name filename))
                      (format "%s" el))))
              (insert button-prefix)
              (widget-create 'push-button
                             :action `(lambda (&rest ignore) (bookmark-jump ,el))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :format "%[%t%]" button-text))
            (setq spacemacs-buffer--startup-list-nr
                  (1+ spacemacs-buffer--startup-list-nr)))
          list)))

(defun spacemacs-buffer//get-org-items (types)
  "Make a list of agenda file items for today of kind types.
TYPES: list of `org-mode' types to fetch."
  (require 'org-agenda)
  (let ((date (calendar-gregorian-from-absolute (org-today))))
    (cl-loop for file in (org-agenda-files nil 'ifmode)
             append (spacemacs-buffer//make-org-items
                     file
                     (apply 'org-agenda-get-day-entries file date
                            types)))))

(defun spacemacs-buffer//agenda-list ()
  "Return today's agenda."
  (require 'org-agenda)
  (spacemacs-buffer//get-org-items org-agenda-entry-types))

(defun spacemacs-buffer//todo-list ()
  "Return current todos."
  (require 'org-agenda)
  (spacemacs-buffer//get-org-items '(:todo)))

(defun spacemacs-buffer//make-org-items (file items)
  "Make a spacemacs-buffer org item list.
FILE: file name.
ITEMS:"
  (cl-loop for item in items
           collect (spacemacs-buffer//make-org-item file item)))

(defun spacemacs-buffer//make-org-item (file item)
  "Make a spacemacs-buffer version of an org item.
FILE: file name.
ITEM:"
  `(("text" . ,(get-text-property 0 'txt item))
    ("file" . ,file)
    ("pos"  . ,(marker-position (get-text-property 0 'org-marker item)))
    ("time" . ,(get-text-property 0 'time item))))

(defun spacemacs-buffer//org-jump (el)
  "Action executed when using an item in the home buffer's todo list.
EL: `org-agenda' element to jump to."
  (require 'org-agenda)
  (find-file-other-window (cdr (assoc "file" el)))
  (widen)
  (goto-char (cdr (assoc "pos" el)))
  (when (derived-mode-p 'org-mode)
    (org-show-context 'agenda)
    (save-excursion
      (and (outline-next-heading)
           (org-flag-heading nil)))    ; show the next heading
    (when (outline-invisible-p)
      (outline-show-entry))            ; display invisible text
    (recenter (/ (window-height) 2))
    (org-back-to-heading t)
    (if (re-search-forward org-complex-heading-regexp nil t)
        (goto-char (match-beginning 4))))
  (run-hooks 'org-agenda-after-show-hook))

(defun spacemacs-buffer//insert-todo-list (list-display-name list)
  "Insert an interactive todo list of `org-agenda' entries in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: list of `org-agenda' entries in the todo list."
  (when (car list)
    (insert list-display-name)
    (setq list (sort list
                     (lambda (a b)
                       (cond
                        ((eq "" (cdr (assoc "time" b)))
                         t)
                        ((eq "" (cdr (assoc "time" a)))
                         nil)
                        (t
                         (string< (cdr (assoc "time" a))
                                  (cdr (assoc "time" b))))))))
    (mapc (lambda (el)
            (let* ((button-prefix
                    (concat
                     "\n    "
                     (when dotspacemacs-show-startup-list-numbers
                       (format "%2s " (number-to-string spacemacs-buffer--startup-list-nr)))
                     " "
                     (spacemacs-buffer//font-icons-icon "" 'dot)
                     " "))
                   (button-text
                    (format "%s %s %s"
                            (let ((filename (cdr (assoc "file" el))))
                              (if dotspacemacs-home-shorten-agenda-source
                                  (file-name-nondirectory filename)
                                (abbreviate-file-name filename)))
                            (if (not (eq "" (cdr (assoc "time" el))))
                                (format "- %s -"
                                        (cdr (assoc "time" el)))
                              "-")
                            ;; Replace links in org style in todo entries
                            ;; "[[Link][Name]]" => "[Name]"
                            (replace-regexp-in-string
                             "\\[\\[[^][]+\\]\\[\\([^][]+\\)\\]\\]"
                             "[\\1]"
                             (cdr (assoc "text" el))))))
              (insert button-prefix)
              (widget-create 'push-button
                             :action `(lambda (&rest ignore)
                                        (spacemacs-buffer//org-jump ',el))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :format "%[%t%]" button-text))
            (setq spacemacs-buffer--startup-list-nr
                  (1+ spacemacs-buffer--startup-list-nr)))
          list)))

(defun spacemacs-buffer//associate-to-project (recent-file by-project)
  (dolist (x by-project)
    (when (string-prefix-p (car x) (expand-file-name recent-file))
      (setcdr x (cons (string-remove-prefix (car x) recent-file) (cdr x))))))

(autoload 'projectile-known-projects "projectile")
(defun spacemacs-buffer//recent-files-by-project ()
  (let ((by-project (mapcar (lambda (p) (cons (expand-file-name p) nil))
                            (projectile-known-projects))))
    (dolist (recent-file recentf-list by-project)
      (spacemacs-buffer//associate-to-project recent-file by-project))))

(defun spacemacs//subseq (seq start end)
  "Adapted version of `cl-subseq'.
Use `cl-subseq', but accounting for end points greater than the size of the
list.  Return entire list if end is omitted.
SEQ, START and END are the same arguments as for `cl-subseq'"
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end)
                              (min len end)))))

(defmacro spacemacs-buffer||propertize-heading (icon text shortcut-char)
  `(concat (when dotspacemacs-startup-buffer-show-icons
             (concat ,icon " "))
           (propertize ,text 'face 'font-lock-keyword-face)
           (propertize (concat " (" ,shortcut-char ")")
                       'face 'font-lock-comment-face)))

(defun spacemacs-buffer//insert-errors ()
  (when (spacemacs-buffer//insert-string-list
         (spacemacs-buffer||propertize-heading
          (spacemacs-buffer//font-icons-icon "" 'error)
          "Errors:" "e")
         spacemacs-buffer--errors)
    (spacemacs-buffer||add-shortcut "e" "Errors:")
    (insert spacemacs-buffer-list-separator)))

(defun spacemacs-buffer//insert-warnings ()
  (when (spacemacs-buffer//insert-string-list
         (spacemacs-buffer||propertize-heading
          (spacemacs-buffer//font-icons-icon "" 'warning)
          "Warnings:" "w")
         spacemacs-buffer--warnings)
    (spacemacs-buffer||add-shortcut "w" "Warnings:")
    (insert spacemacs-buffer-list-separator)))

(defun spacemacs-buffer//insert-recent-files (list-size)
  "Insert recent file entries to spacemacs-buffer.

LIST-SIZE is specified in `dotspacemacs-startup-lists' for recent entries."
  (unless recentf-mode (recentf-mode))
  (let (;; we need to remove `org-agenda-files' entries from recent files
        (agenda-files
         (when-let* ((default-directory
                      (or (bound-and-true-p org-directory) "~/org"))
                     (files
                      (when (bound-and-true-p org-agenda-files)
                        (if (listp org-agenda-files)
                            ;; if it's a list, we take that value directly
                            org-agenda-files
                          ;; but if it's a string, it must be file where the list
                          ;; of agenda files are stored in that file and we have
                          ;; to load `org-agenda' to process the list. If org is
                          ;; already loaded, then we assume that the user has
                          ;; already called org-agenda-files.
                          (when (not (featurep 'org))
                            (warn "`org-agenda-files' is a string and \
not a list. This requires us to load `org' to process the org agenda files in \
startup list.")
                            (require 'org)
                            (org-agenda-files))))))
           (mapcar #'expand-file-name files)))
        ;; we also need to skip sub-directories of `org-directory'
        (ignore-directory (when (bound-and-true-p org-directory)
                            (expand-file-name org-directory)))
        (recent-files-list))
    (cl-loop for rfile in recentf-list
             while (> list-size 0)
             do (let ((full-path (expand-file-name rfile)))
                  (unless (or (and ignore-directory
                                   (string-prefix-p ignore-directory full-path))
                              (member full-path agenda-files))
                    (cl-pushnew rfile recent-files-list)
                    (setq list-size (1- list-size))))
             finally do (setq recent-files-list (nreverse recent-files-list)))
    (when (spacemacs-buffer//insert-file-list
           (spacemacs-buffer||propertize-heading
            (spacemacs-buffer//font-icons-icon "" 'history)
            "Recent Files:" "r")
           recent-files-list)
      (spacemacs-buffer||add-shortcut "r" "Recent Files:")))
  (insert spacemacs-buffer-list-separator))

(defun spacemacs-buffer//insert-recent-files-by-project (list-size)
  (unless recentf-mode (recentf-mode))
  (when (spacemacs-buffer//insert-files-by-dir-list
         (spacemacs-buffer||propertize-heading
          (spacemacs-buffer//font-icons-icon "" 'rocket)
          "Recent Files by Project:" "R")
         (mapcar (lambda (group)
                   (cons (car group)
                         (spacemacs//subseq (reverse (cdr group))
                                            0
                                            (cdr list-size))))
                 (spacemacs//subseq (spacemacs-buffer//recent-files-by-project)
                                    0
                                    (car list-size))))
    (spacemacs-buffer||add-shortcut "R" "Recent Files by Project:")
    (insert spacemacs-buffer-list-separator)))

(defun spacemacs-buffer//insert-todos (list-size)
  (when (spacemacs-buffer//insert-todo-list
         (spacemacs-buffer||propertize-heading
          (spacemacs-buffer//font-icons-icon "" 'check)
          "To-Do:" "d")
         (spacemacs//subseq (spacemacs-buffer//todo-list)
                            0 list-size))
    (spacemacs-buffer||add-shortcut "d" "To-Do:")
    (insert spacemacs-buffer-list-separator)))

(defun spacemacs-buffer//insert-agenda (list-size)
  (when (spacemacs-buffer//insert-todo-list
         (spacemacs-buffer||propertize-heading
          (spacemacs-buffer//font-icons-icon "" 'calendar)
          "Agenda:" "c")
         (spacemacs//subseq (spacemacs-buffer//agenda-list)
                            0 list-size))
    (spacemacs-buffer||add-shortcut "c" "Agenda:")
    (insert spacemacs-buffer-list-separator)))

(defun spacemacs-buffer//insert-bookmarks (list-size)
  (when (configuration-layer/layer-used-p 'spacemacs-helm)
    (helm-mode))
  (require 'bookmark)
  (when (spacemacs-buffer//insert-bookmark-list
         (spacemacs-buffer||propertize-heading
          (spacemacs-buffer//font-icons-icon "" 'bookmark)
          "Bookmarks:" "b")
         (spacemacs//subseq (bookmark-all-names)
                            0 list-size))
    (spacemacs-buffer||add-shortcut "b" "Bookmarks:")
    (insert spacemacs-buffer-list-separator)))

(defun spacemacs-buffer//insert-projects (list-size)
  (when (spacemacs-buffer//insert-file-list
         (spacemacs-buffer||propertize-heading
          (spacemacs-buffer//font-icons-icon "" 'rocket)
          "Projects:" "p")
         (spacemacs//subseq (projectile-known-projects)
                            0 list-size))
    (spacemacs-buffer||add-shortcut "p" "Projects:")
    (insert spacemacs-buffer-list-separator)))

(defvar spacemacs-buffer--startup-list-nr 1)

(defun spacemacs-buffer//do-insert-startupify-lists ()
  "Insert the startup lists in the current buffer."
  (setq spacemacs-buffer--startup-list-nr 1)
  (let ((spacemacs-buffer--icons-font nil) ; need to be updated
        (is-org-loaded (bound-and-true-p spacemacs-initialized)))
    (when-let* (spacemacs-initialized
                (font (spacemacs-buffer//determind-icons-font 'skip-require))
                ((not (memq font '(nil none))))
                ((not (configuration-layer/package-used-p font))))
      (message "Package `%s' isn't installed" font)
      (setq spacemacs-buffer--icons-font 'none))
    (dolist (els (if is-org-loaded (append '(warnings) dotspacemacs-startup-lists) '(warnings)))
      (let ((el (or (car-safe els) els))
            (list-size (or (cdr-safe els)
                           spacemacs-buffer-startup-lists-length)))
        (cond
         ((eq el 'warnings)
          (spacemacs-buffer//insert-errors)
          (spacemacs-buffer//insert-warnings))
         ((eq el 'recents) (spacemacs-buffer//insert-recent-files list-size))
         ((and (eq el 'recents-by-project)
               (fboundp 'projectile-mode))
          (spacemacs-buffer//insert-recent-files-by-project list-size))
         ((eq el 'todos) (spacemacs-buffer//insert-todos list-size))
         ((eq el 'agenda) (spacemacs-buffer//insert-agenda list-size))
         ((eq el 'bookmarks) (spacemacs-buffer//insert-bookmarks list-size))
         ((and (eq el 'projects)
               (fboundp 'projectile-mode))
          (spacemacs-buffer//insert-projects list-size)))))))

(defun spacemacs-buffer//get-buffer-width ()
  "Return the length of longest line in the current buffer."
  (save-excursion
    (goto-char 0)
    (let ((current-max 0))
      (while (not (eobp))
        (let ((line-length (- (line-end-position) (line-beginning-position))))
          (setq current-max (max current-max line-length)))
        (forward-line 1))
      current-max)))

(defun spacemacs-buffer//center-startup-lists ()
  "Center startup lists after they were inserted."
  (let* ((lists-width (spacemacs-buffer//get-buffer-width))
         (margin (max 0 (- spacemacs-buffer--buttons-position
                           spacemacs-buffer-buttons-startup-lists-offset)))
         (width-diff (- spacemacs-buffer--window-width lists-width))
         (final-padding (cond
                         ((>= width-diff margin) margin)
                         ((< width-diff 0)       0)
                         (t                      (floor (/ width-diff 2))))))
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (insert (make-string final-padding ?\s))
      (forward-line))))

(defun spacemacs-buffer/insert-startup-lists ()
  "Insert startup lists in home buffer."
  (interactive)
  (with-current-buffer (get-buffer spacemacs-buffer-name)
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (spacemacs-buffer/insert-page-break)
      (insert "\n")
      (save-restriction
        (narrow-to-region (point) (point))
        (spacemacs-buffer//do-insert-startupify-lists)
        (spacemacs-buffer//center-startup-lists)))))

(defun spacemacs-buffer/goto-link-line ()
  "Set point to the beginning of the link line."
  (interactive)
  (with-current-buffer spacemacs-buffer-name
    (goto-char (point-min))
    (with-demoted-errors "spacemacs buffer error: %s"
      (search-forward "[")
      (left-char 2))))

(defun spacemacs-buffer//mouse-1 (event)
  "Action to open widget button at mouse click.

NOTE: This is reserved only to use in spacemacs-buffer. It is a slimmed down
version of `widget-button-press' since `widget-button-click' doesn't work."
  (interactive "e")
  (when (widget-event-point event)
    (let ((pos (widget-event-point event)))
      (goto-char pos)
      (when-let* ((button (get-char-property pos 'button)))
        (widget-apply-action button)))))

(defun spacemacs-buffer/jump-to-number-startup-list-line ()
  "Jump to the startup list line with the typed number.

The minimum delay in seconds between number key presses,
can be adjusted with the variable:
`dotspacemacs-startup-buffer-multi-digit-delay'."
  (interactive)
  (when spacemacs-buffer--idle-numbers-timer
    (cancel-timer spacemacs-buffer--idle-numbers-timer))
  (let* ((key-pressed-string (string-trim-left (if (characterp last-input-event)
                                                   (string last-input-event)
                                                 (format "%s" last-input-event))
                                               "kp-")))
    (setq spacemacs-buffer--startup-list-number
          (concat spacemacs-buffer--startup-list-number key-pressed-string))
    (let (message-log-max) ; only show in minibuffer
      (message "Jump to startup list: %s" spacemacs-buffer--startup-list-number))
    (setq spacemacs-buffer--idle-numbers-timer
          (run-with-idle-timer
           dotspacemacs-startup-buffer-multi-digit-delay nil
           'spacemacs-buffer/stop-waiting-for-additional-numbers))))

(defun spacemacs-buffer/jump-to-line-starting-with-nr-space (nr-string)
  "Jump to the line begins with NR-STRING, skipping non-digit prefix."
  (let ((prev-point (point)))
    (goto-char (window-start))
    (if (not (re-search-forward
              (concat "^ +" nr-string "[0-9]* +. ")
              ;; don't search past two lines above the window-end,
              ;; because they bottom two lines are hidden by the mode line
              (save-excursion (goto-char (window-end))
                              (forward-line -1)
                              (point))
              'noerror))
        (progn
          (goto-char prev-point)
          (let (message-log-max) ; only show in minibuffer
            (message "Couldn't find startup list number: %s"
                     spacemacs-buffer--startup-list-number)))
      (message "Opening file/dir: %s"
               (widget-value (widget-at (point))))
      (widget-button-press (point)))))

(defun spacemacs-buffer/stop-waiting-for-additional-numbers ()
  (spacemacs-buffer/jump-to-line-starting-with-nr-space
   spacemacs-buffer--startup-list-number)
  (setq spacemacs-buffer--startup-list-number nil))

(defun spacemacs-buffer//startup-hook ()
  "Code executed when Emacs has finished loading."
  (with-current-buffer (get-buffer spacemacs-buffer-name)
    (when dotspacemacs-startup-lists
      (spacemacs-buffer/insert-startup-lists))
    (spacemacs-buffer//insert-footer)
    (if configuration-layer-error-count
        (progn
          (spacemacs-buffer-mode)
          (face-remap-add-relative 'mode-line
                                   '((:background "red") mode-line))
          (spacemacs-buffer/set-mode-line
           (format
            (concat "%s error(s) at startup! "
                    "Spacemacs may not be able to operate properly.")
            configuration-layer-error-count) t))
      (spacemacs-buffer/set-mode-line spacemacs--default-mode-line)
      (spacemacs-buffer-mode))
    (force-mode-line-update)
    (spacemacs-buffer/goto-link-line)))

(defun spacemacs-buffer/goto-buffer (&optional refresh do-not-switch)
  "Create the special buffer for `spacemacs-buffer-mode'.
REFRESH if the buffer should be redrawn. This will automatically
switch to the buffer unless DO-NOT-SWITCH is non nil.

If a prefix argument is given, switch to it in an other, possibly new window."
  (interactive)
  (let ((buffer-exists (buffer-live-p (get-buffer spacemacs-buffer-name)))
        (save-line nil))
    (when (not buffer-exists)
      (setq spacemacs-buffer--note-widgets nil))
    (when (or (not (eq spacemacs-buffer--last-width (window-width)))
              (not buffer-exists)
              refresh)
      (setq spacemacs-buffer--window-width (if dotspacemacs-startup-buffer-responsive
                                               (window-width)
                                             80)
            spacemacs-buffer--last-width spacemacs-buffer--window-width)
      (with-current-buffer (get-buffer-create spacemacs-buffer-name)
        (save-excursion
          (when (> (buffer-size) 0)
            (setq save-line (line-number-at-pos))
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (spacemacs-buffer/set-mode-line "")
          (if dotspacemacs-startup-buffer-show-version
              (spacemacs-buffer//insert-version)
            (let ((inhibit-read-only t))
              (insert "\n")))
          (spacemacs-buffer/insert-banner-and-buttons)
          (when (bound-and-true-p spacemacs-initialized)
            (spacemacs-buffer//notes-redisplay-current-note)
            (when dotspacemacs-startup-lists
              (spacemacs-buffer/insert-startup-lists))
            (spacemacs-buffer//insert-footer)
            (configuration-layer/display-summary)
            (spacemacs-buffer/set-mode-line spacemacs--default-mode-line)
            (force-mode-line-update)
            (spacemacs-buffer-mode)))
        (if save-line
            (progn (goto-char (point-min))
                   (forward-line (1- save-line))
                   (forward-to-indentation 0))
          (spacemacs-buffer/goto-link-line)))
      (unless do-not-switch
        (if current-prefix-arg
            (switch-to-buffer-other-window spacemacs-buffer-name))
        (switch-to-buffer spacemacs-buffer-name))
      (spacemacs//redisplay))))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook
                      'spacemacs-buffer//resize-on-hook)
            (spacemacs-buffer//resize-on-hook)))

(defun spacemacs-buffer//resize-on-hook ()
  "Hook run on window resize events to redisplay the home buffer."
  ;; prevent spacemacs buffer redisplay in the filetree window
  (unless (memq this-command '(neotree-find-project-root
                               neotree-show
                               neotree-toggle
                               spacemacs/treemacs-project-toggle
                               treemacs
                               treemacs-bookmark
                               treemacs-find-file
                               treemacs-select-window))
    (let ((home-buffer (get-buffer-window spacemacs-buffer-name))
          (frame-win (frame-selected-window)))
      (when (and dotspacemacs-startup-buffer-responsive
                 home-buffer
                 (not (window-minibuffer-p frame-win)))
        (with-selected-window home-buffer
          (spacemacs-buffer/goto-buffer))))))

(defun spacemacs-buffer/refresh ()
  "Force recreation of the spacemacs buffer."
  (interactive)
  (setq spacemacs-buffer--last-width nil)
  (spacemacs-buffer/goto-buffer t))

(defalias 'spacemacs/home 'spacemacs-buffer/refresh
  "Go to Spacemacs home buffer.")

(defun spacemacs-buffer/return ()
  "Open the button or go to next line.

This function is intended to be used in `spacemacs-buffer-mode' only."
  (interactive)
  (if (get-char-property (point) 'button)
      ;; point on a button, press it
      (widget-button-press (point))
    ;; point on an entry, press it
    (if-let* ((button (save-excursion
                        (beginning-of-line-text)
                        (re-search-forward "[0-9]* +. " (point-at-eol) 'noerror))))
        (widget-button-press button)
      ;; go to next line
      (forward-line)
      (beginning-of-line-text))))

(defun spacemacs/home-delete-other-windows ()
  "Open home Spacemacs buffer and delete other windows.
Useful for making the home buffer the only visible buffer in the frame."
  (interactive)
  (spacemacs/home)
  (delete-other-windows))

(provide 'core-spacemacs-buffer)

;;; core-spacemacs-buffer ends here
