;;; packages.el --- Racket Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
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


(setq racket-packages
      '(
        company
        company-quickhelp
        ggtags
        evil-cleverparens
        racket-mode
        ))

(defun racket/post-init-company ()
  ;; this is the only thing to do to enable company in racket-mode
  ;; because racket-mode handle everything for us when company
  ;; is loaded.
  (add-hook 'racket-mode-hook 'company-mode))

(defun racket/post-init-company-quickhelp ()
  ;; Bug exists in Racket company backend that opens docs in new window when
  ;; company-quickhelp calls it. Note hook is appendended for proper ordering.
  (add-hook 'company-mode-hook
            (lambda ()
              (when (and (equal major-mode 'racket-mode)
                         (bound-and-true-p company-quickhelp-mode))
                (company-quickhelp-mode -1))) t))

(defun racket/post-init-ggtags ()
  (add-hook 'racket-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun racket/pre-init-evil-cleverparens ()
  (spacemacs|use-package-add-hook evil-cleverparens
    :pre-init
    (add-to-list 'evil-lisp-safe-structural-editing-modes 'racket-mode)))

(defun racket/init-racket-mode ()
  (use-package racket-mode
    :defer t
    :init
    (spacemacs/register-repl 'racket-mode 'racket-repl "racket")
    (add-hook 'racket-mode-hook 'racket-xp-mode)
    :config
    (add-to-list 'evil-evilified-state-modes 'racket-describe-mode)
    (evil-define-key 'evilified 'racket-describe-mode-map
      "o" 'link-hint-open-link)
    ;; smartparens configuration
    (with-eval-after-load 'smartparens
      (add-to-list 'sp--lisp-modes 'racket-mode)
      (when (fboundp 'sp-local-pair)
        (sp-local-pair 'racket-mode "'" nil :actions nil)
        (sp-local-pair 'racket-mode "`" nil :actions nil)))

    (defun spacemacs/racket-test-with-coverage ()
      "Call `racket-test' with universal argument."
      (interactive)
      (racket-test t))

    (defun spacemacs/racket-run-and-switch-to-repl ()
      "Call `racket-run-and-switch-to-repl' and enable
`insert state'."
      (interactive)
      (racket-run-and-switch-to-repl)
      (when (buffer-live-p (get-buffer racket-repl-buffer-name))
        ;; We don't need to worry about the first time the REPL is opened,
        ;; since the first time, insert state is automatically entered (since
        ;; it's registered as a REPL?).
        (with-current-buffer racket-repl-buffer-name
          (evil-insert-state))))

    (defun spacemacs/racket-send-last-sexp-focus ()
      "Call `racket-send-last-sexp' and switch to REPL buffer in
`insert state'."
      (interactive)
      (racket-send-last-sexp)
      (racket-repl)
      (evil-insert-state))

    (defun spacemacs/racket-send-definition-focus ()
      "Call `racket-send-definition' and switch to REPL buffer in
`insert state'."
      (interactive)
      (racket-send-definition)
      (racket-repl)
      (evil-insert-state))

    (defun spacemacs/racket-send-region-focus (start end)
      "Call `racket-send-region' and switch to REPL buffer in
`insert state'."
      (interactive "r")
      (racket-send-region start end)
      (racket-repl)
      (evil-insert-state))

    (dolist (prefix '(("mE" . "errors")
                      ("mg" . "navigation")
                      ("mh" . "doc")
                      ("mi" . "insert")
                      ("mr" . "refactor")
                      ("ms" . "repl")
                      ("mt" . "tests")))
      (spacemacs/declare-prefix-for-mode 'racket-mode (car prefix) (cdr prefix)))

    (spacemacs/set-leader-keys-for-major-mode 'racket-mode
      ;; errors
      "En" 'racket-xp-next-error
      "EN" 'racket-xp-previous-error
      ;; navigation
      "g`" 'racket-unvisit
      "gg" 'racket-xp-visit-definition
      "gn" 'racket-xp-next-definition
      "gN" 'racket-xp-previous-definition
      "gm" 'racket-visit-module
      "gr" 'racket-open-require-path
      "gu" 'racket-xp-next-use
      "gU" 'racket-xp-previous-use
      ;; doc
      "ha" 'racket-xp-annotate
      "hd" 'racket-xp-describe
      "hh" 'racket-xp-documentation
      ;; insert
      "il" 'racket-insert-lambda
      ;; refactor
      "mr" 'racket-xp-rename
      ;; REPL
      "'"  'racket-repl
      "sb" 'racket-run
      "sB" 'spacemacs/racket-run-and-switch-to-repl
      "se" 'racket-send-last-sexp
      "sE" 'spacemacs/racket-send-last-sexp-focus
      "sf" 'racket-send-definition
      "sF" 'spacemacs/racket-send-definition-focus
      "si" 'racket-repl
      "sr" 'racket-send-region
      "sR" 'spacemacs/racket-send-region-focus
      ;; Tests
      "tb" 'racket-test
      "tB" 'spacemacs/racket-test-with-coverage)
    (define-key racket-mode-map (kbd "H-r") 'racket-run)))
