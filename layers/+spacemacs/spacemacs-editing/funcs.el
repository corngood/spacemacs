;;; funcs.el --- Spacemacs editing Layer functions File  -*- lexical-binding: nil; -*-
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



;; smartparens

(defun spacemacs/smartparens-pair-newline (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun spacemacs/smartparens-pair-newline-and-indent (id action context)
  (spacemacs/smartparens-pair-newline id action context)
  (indent-according-to-mode))

(defun spacemacs/smart-closing-parenthesis ()
  "Insert a closing pair delimiter or move point past existing delimiter.

If the expression at point is already balanced and there is a
closing delimiter for that expression on the current line, move
point forward past the closing delimiter.

If the expression is balanced but there is no closing delimiter
on the current line, insert a literal ')' character.

If the expression is not balanced, insert a closing delimiter for
the current expression.

This command uses Smartparens navigation commands and therefore
recognizes pair delimiters that have been defined using `sp-pair'
or `sp-local-pair'."
  (interactive)
  (let* ((sp-navigate-close-if-unbalanced t)
         (current-pos (point))
         (current-line (line-number-at-pos current-pos))
         next-pos next-line)
    (save-excursion
      (let ((buffer-undo-list)
            (modified (buffer-modified-p)))
        (unwind-protect
            (progn
              (sp-up-sexp)
              (setq next-pos (point)
                    next-line (line-number-at-pos)))
          (primitive-undo (length buffer-undo-list)
                          buffer-undo-list)
          (set-buffer-modified-p modified))))
    (cond
     ((and (= current-line next-line)
           (not (= current-pos next-pos)))
      (sp-up-sexp))
     (t
      (insert-char ?\))))))

(defun spacemacs//activate-smartparens(&optional global)
  "Enable `smartparens-mode' or strict version.
This either activates `smartparens-mode' or `smartparens-strict-mode'
depending on the respective dotfile setting.

It is not necessary to activate `smartparens-mode' independently as it
is included in `smartparens-strict-mode'.

If `global' is non-nil activate the respective global mode."
  (if dotspacemacs-smartparens-strict-mode
      (if global
          (smartparens-global-strict-mode 1)
        (smartparens-strict-mode 1))
    (if global
        (smartparens-global-mode 1)
      (smartparens-mode 1))))

(defun spacemacs//deactivate-smartparens(&optional global)
  "Deactivate `smartparens-mode'.
This deactivates `smartparens-mode' and `smartparens-strict-mode'.

It is important to disable both to remove all advices.

If `global' is non-nil activate the respective global mode."
  (if global
      (progn
        (when smartparens-global-strict-mode
          (smartparens-global-strict-mode -1))
        (smartparens-global-mode -1))
    (when smartparens-strict-mode
      (smartparens-strict-mode -1))
    (smartparens-mode -1)))

(defun spacemacs//conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (or (eq this-command 'eval-expression)
          (eq this-command 'eldoc-eval-expression))
      (spacemacs//activate-smartparens)))

(defun spacemacs//adaptive-smartparent-pair-overlay-face ()
  (set-face-attribute 'sp-pair-overlay-face nil
                      :inherit 'lazy-highlight
                      :background 'unspecified
                      :foreground 'unspecified))

(defun spacemacs//put-clean-aindent-last ()
  "Put `clean-aindent--check-last-point` to end of `post-command-hook`.
This functions tries to ensure that clean-aindent checks for indent
operations after each indent operations have been done.

See issues #6520 and #13172"
  (when clean-aindent-mode
    (remove-hook 'post-command-hook 'clean-aindent--check-last-point)
    (add-hook 'post-command-hook 'clean-aindent--check-last-point t)))


;; uuidgen
;; TODO spacemacs/uuidgen-3 and spacemacs/uuidgen-5

(defun spacemacs/uuidgen-1 (arg)
  "Return a time based UUID (UUIDv1).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-1)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))

(defun spacemacs/uuidgen-4 (arg)
  "Return an UUID from random numbers (UUIDv4).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-4)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))


;;; wgrep

(defun spacemacs//grep-set-evil-state ()
  "Set the evil state for the read-only grep buffer given the current editing style."
  (if (eq dotspacemacs-editing-style 'emacs)
      (evil-emacs-state)
    (evil-motion-state)))

(defun spacemacs/wgrep-finish-edit ()
  "Set back the default evil state when finishing editing."
  (interactive)
  (wgrep-finish-edit)
  (spacemacs//grep-set-evil-state))

(defun spacemacs/wgrep-abort-changes ()
  "Set back the default evil state when aborting editing."
  (interactive)
  (wgrep-abort-changes)
  (spacemacs//grep-set-evil-state))

(defun spacemacs/wgrep-abort-changes-and-quit ()
  "Abort changes and quit."
  (interactive)
  (spacemacs/wgrep-abort-changes)
  (quit-window))

(defun spacemacs/wgrep-save-changes-and-quit ()
  "Save changes and quit."
  (interactive)
  (spacemacs/wgrep-finish-edit)
  (wgrep-save-all-buffers)
  (quit-window))

(defun spacemacs/grep-change-to-wgrep-mode ()
  (interactive)
  (require 'wgrep)
  (wgrep-change-to-wgrep-mode)
  (evil-normal-state))
