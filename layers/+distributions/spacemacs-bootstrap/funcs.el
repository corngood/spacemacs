;;; funcs.el --- Spacemacs Bootstrap Layer functions File  -*- lexical-binding: nil; -*-
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




(defun spacemacs/state-color-face (state)
  "Return the symbol of the face for the given STATE."
  (intern (format "spacemacs-%s-face" (symbol-name state))))

(defun spacemacs/state-color (state)
  "Return the color string associated to STATE."
  (face-background (spacemacs/state-color-face state)))

(defun spacemacs/current-state-color ()
  "Return the color string associated to the current state."
  (face-background (spacemacs/state-color-face evil-state)))

(defun spacemacs/state-face (state)
  "Return the face associated to the STATE."
  (spacemacs/state-color-face state))

(defun spacemacs/current-state-face ()
  "Return the face associated to the current state."
  (let ((state (if (eq evil-state 'operator)
                   evil-previous-state
                 evil-state)))
    (spacemacs/state-color-face state)))

(defun spacemacs/add-evil-cursor (state color shape)
  "Define a cursor and face for a new evil state.
An appropriate entry is added to `spacemacs-evil-cursors', as well.

For evil states that do not need an evil cursor use
`spacemacs/define-evil-state-face' instead."
  (add-to-list 'spacemacs-evil-cursors (list state color shape))
  (spacemacs/define-evil-state-face state color)
  (set (intern (format "evil-%s-state-cursor" state))
       (list (when dotspacemacs-colorize-cursor-according-to-state color)
             shape)))

(defun spacemacs/define-evil-state-face (state color)
  "Define a face for an evil state.
For evil states that also need an entry to `spacemacs-evil-cursors' use
`spacemacs/add-evil-cursor' instead."
  ;; this function and `spacemacs/add-evil-cursor' need to be separate because
  ;; some states must explicitly *not* have their own evil spacemacs cursor
  ;; for example treemacs: it needs no cursor since it solely uses hl-line-mode
  ;; and having an evil cursor defined anyway leads to the cursor sometimes
  ;; visibly flashing in treemacs buffers
  (eval `(defface ,(spacemacs/state-color-face (intern state))
           `((t (:background ,color :inherit 'mode-line)))
           (format "%s state face." state)
           :group 'spacemacs))
  ;; 'unspecified may not be used in defface, so set it via set-face-attribute.
  (set-face-attribute (spacemacs/state-color-face (intern state)) nil
                      :foreground (face-attribute 'mode-line :background)))

(defun spacemacs/set-state-faces ()
  (let ((ml-bg (face-attribute 'mode-line :background)))
    (cl-loop for (state color cursor) in spacemacs-evil-cursors
             do
             (set-face-attribute (spacemacs/state-color-face (intern state)) nil
                                 :foreground ml-bg))))

(defun evil-insert-state-cursor-hide ()
  (setq evil-insert-state-cursor '((hbar . 0))))

(defun spacemacs/set-evil-search-module (style)
  "Set the evil search module depending on STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             (bound-and-true-p hybrid-style-use-evil-search-module)))
    ;; if Evil is loaded already, just setting `evil-search-module' isn't
    ;; enough, we need to call `evil-select-search-module' as well (this is done
    ;; automatically when `evil-search-module' is changed via customize)
    (if (featurep 'evil-search)
        (evil-select-search-module 'evil-search-module 'evil-search)
      (setq-default evil-search-module 'evil-search)))
   (t
    (if (featurep 'evil-search)
        (evil-select-search-module 'evil-search-module 'isearch)
      (setq-default evil-search-module 'isearch)))))

(defun spacemacs/evil-search-clear-highlight ()
  "Clear evil-search or evil-ex-search persistent highlights."
  (interactive)
  (evil-ex-nohighlight)) ; `/' highlights

(defun spacemacs/evil-smart-doc-lookup ()
  "Run documentation lookup command specific to the major mode.
Use command bound to `SPC m h h` if defined, otherwise fall back
to `evil-lookup'"
  (interactive)
  (let ((binding (key-binding (kbd (concat dotspacemacs-leader-key " mhh")))))
    (if (commandp binding)
        (call-interactively binding)
      (evil-lookup))))

(defun spacemacs//set-evil-shift-width ()
  "Set the value of `evil-shift-width' based on the indentation settings of the
current major mode."
  (let ((shift-width
         (catch 'break
           (dolist (test spacemacs--indent-variable-alist)
             (let ((mode (car test))
                   (val (cdr test)))
               (when (or (and (symbolp mode) (derived-mode-p mode))
                         (and (listp mode) (apply 'derived-mode-p mode))
                         (eq 't mode))
                 (unless (listp val)
                   (setq val (list val)))
                 (dolist (v val)
                   (cond
                    ((integerp v) (throw 'break v))
                    ((and (symbolp v) (boundp v))
                     (throw 'break (symbol-value v))))))))
           (throw 'break (default-value 'evil-shift-width)))))
    (when (and (integerp shift-width)
               (< 0 shift-width))
      (setq-local evil-shift-width shift-width))))

(defmacro spacemacs|define-text-object (key name start end)
  "Define a text object and a surround pair.
START and END are strings (not regular expressions) that define
the boundaries of the text object."
  `(progn
     (spacemacs|define-text-object-regexp ,key ,name
                                          ,(regexp-quote start)
                                          ,(regexp-quote end))
     (with-eval-after-load 'evil-surround
       (add-to-list 'evil-surround-pairs-alist
                    (cons (string-to-char ,key)
                          (if ,end
                              (cons ,start ,end)
                            ,start))))))

(defmacro spacemacs|define-text-object-regexp (key name start-regexp end-regexp)
  "Define a text object.
START-REGEXP and END-REGEXP are the boundaries of the text object."
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-outer-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;; need to delay this macro since it relies on evil key maps to be defined
(with-eval-after-load 'evil
  (defmacro evil-map (state key seq)
    "Map for a given STATE a KEY to a sequence SEQ of keys.

Can handle recursive definition only if KEY is the first key of
SEQ, and if KEY's binding in STATE is defined as a symbol in
`evil-normal-state-map'.
Example: (evil-map visual \"<\" \"<gv\")"
    (let ((map (intern (format "evil-%S-state-map" state)))
          (key-cmd (lookup-key evil-normal-state-map key)))
      `(define-key ,map ,key
                   (lambda ()
                     (interactive)
                     ,(if (string-equal key (substring seq 0 1))
                          `(let ((orig-this-command this-command))
                             (setq this-command ',key-cmd)
                             (call-interactively ',key-cmd)
                             (run-hooks 'post-command-hook)
                             (setq this-command orig-this-command)
                             (execute-kbd-macro ,(substring seq 1)))
                        (execute-kbd-macro ,seq)))))))

(defun spacemacs/diminish-hook (_)
  "Display diminished lighter in vanilla Emacs mode-line."
  (let ((unicodep (dotspacemacs|symbol-value
                   dotspacemacs-mode-line-unicode-symbols)))
    (cl-loop for (mode uni nouni) in spacemacs--diminished-minor-modes
             do (diminish mode (if unicodep uni nouni)))))



(defun spacemacs//hydra-key-doc-function (key key-width doc doc-width)
  "Custom hint documentation format for keys."
  (format (format "[%%%ds] %%%ds" key-width (- -1 doc-width))
          key doc))



(defvar spacemacs--scroll-ts-full-hint-toggle t
  "Toggle the state of the scroll transient state documentation.

Initial value is t so full hint will be shown by default. This is
to preserve the behavior before hint toggle was implemented for
the scroll transient state.")

(defvar spacemacs--scroll-ts-full-hint nil
  "Display full scroll transient state documentation.")

(defun spacemacs//scroll-ts-toggle-hint ()
  "Toggle the full hint docstring for the scroll transient state."
  (interactive)
  (setq spacemacs--scroll-ts-full-hint-toggle
        (not spacemacs--scroll-ts-full-hint-toggle)))

(defun spacemacs//scroll-ts-hint ()
  "Return a condensed/full hint for the scroll transient state"
  (concat
   " "
   (if spacemacs--scroll-ts-full-hint-toggle
       spacemacs--scroll-ts-full-hint
     (concat "[" (propertize "?" 'face 'hydra-face-red) "] toggle help"))))



;; As suggested in the Emacs wiki https://www.emacswiki.org/emacs/HideShow#toc5
(defun spacemacs/toggle-selective-display (column)
  "Toggle selective display by column, a.k.a. folding by indentation.

Invokes `set-selective-display', but if a prefix argument is not supplied and
`selective-display' is not already true, source the prefix argument from the
column."
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))



(defun spacemacs/not-in-pdf-view-mode (orig-fun &rest args)
  "Disable bound function when in `pdf-view-mode'."
  (unless (eq major-mode 'pdf-view-mode)
    (apply orig-fun args)))
