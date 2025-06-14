;;; packages.el --- Elixir Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
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


(defconst elixir-packages
  '(
    (alchemist :toggle (eq elixir-backend 'alchemist))
    company
    dap-mode
    elixir-mode
    evil-matchit
    flycheck
    flycheck-credo
    ggtags
    ob-elixir
    popwin
    smartparens))

(defun elixir/init-alchemist ()
  (use-package alchemist
    :defer t
    :init
    (spacemacs/register-repl 'alchemist 'alchemist-iex-run "alchemist")
    (add-hook 'elixir-mode-hook 'alchemist-mode)
    (setq alchemist-project-compile-when-needed t
          alchemist-test-status-modeline nil)
    (add-to-list 'spacemacs-jump-handlers-elixir-mode
                 '(alchemist-goto-definition-at-point :async t))
    :config
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mX" "hex")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mc" "compile")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "me" "eval")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mh" "help")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mm" "mix")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mo" "macroexpand")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mp" "project")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "ms" "iex")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mt" "test")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mx" "execute")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "md" "debug")
    (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
      "el" 'alchemist-eval-current-line
      "eL" 'alchemist-eval-print-current-line
      "er" 'alchemist-eval-region
      "eR" 'alchemist-eval-print-region
      "eb" 'alchemist-eval-buffer
      "eB" 'alchemist-eval-print-buffer
      "ej" 'alchemist-eval-quoted-current-line
      "eJ" 'alchemist-eval-print-quoted-current-line
      "eu" 'alchemist-eval-quoted-region
      "eU" 'alchemist-eval-print-quoted-region
      "ev" 'alchemist-eval-quoted-buffer
      "eV" 'alchemist-eval-print-quoted-buffer

      "gt" 'alchemist-project-toggle-file-and-tests
      "gT" 'alchemist-project-toggle-file-and-tests-other-window

      "h:" 'alchemist-help
      "hH" 'alchemist-help-history
      "hh" 'alchemist-help-search-at-point
      "hr" 'alchemist-help--search-marked-region

      "m:" 'alchemist-mix
      "mc" 'alchemist-mix-compile
      "mx" 'alchemist-mix-run

      "'"  'alchemist-iex-run
      "sc" 'alchemist-iex-compile-this-buffer
      "si" 'alchemist-iex-run
      "sI" 'alchemist-iex-project-run
      "sl" 'alchemist-iex-send-current-line
      "sL" 'alchemist-iex-send-current-line-and-go
      "sm" 'alchemist-iex-reload-module
      "sr" 'alchemist-iex-send-region
      "sR" 'alchemist-iex-send-region-and-go

      "ta" 'alchemist-mix-test
      "tb" 'alchemist-mix-test-this-buffer
      "tB" 'alchemist-project-run-tests-for-current-file
      "tt" 'alchemist-mix-test-at-point
      "tF" 'alchemist-project-find-test
      "tf" 'alchemist-mix-test-file
      "tn" 'alchemist-test-mode-jump-to-next-test
      "tN" 'alchemist-test-mode-jump-to-previous-test
      "tr" 'alchemist-mix-rerun-last-test
      "ts" 'alchemist-mix-test-stale
      "tR" 'alchemist-test-toggle-test-report-display

      "xb" 'alchemist-execute-this-buffer
      "xf" 'alchemist-execute-file
      "x:" 'alchemist-execute

      "cb" 'alchemist-compile-this-buffer
      "cf" 'alchemist-compile-file
      "c:" 'alchemist-compile

      "gg" 'alchemist-goto-definition-at-point
      "." 'alchemist-goto-definition-at-point
      "gb" 'alchemist-goto-jump-back
      ","  'alchemist-goto-jump-back
      "gN" 'alchemist-goto-jump-to-previous-def-symbol
      "gn" 'alchemist-goto-jump-to-next-def-symbol
      "gj" 'alchemist-goto-list-symbol-definitions

      "Xi" 'alchemist-hex-info-at-point
      "Xr" 'alchemist-hex-releases-at-point
      "XR" 'alchemist-hex-releases
      "XI" 'alchemist-hex-info
      "Xs" 'alchemist-hex-search

      "ol" 'alchemist-macroexpand-once-current-line
      "oL" 'alchemist-macroexpand-once-print-current-line
      "ok" 'alchemist-macroexpand-current-line
      "oK" 'alchemist-macroexpand-print-current-line
      "oi" 'alchemist-macroexpand-once-region
      "oI" 'alchemist-macroexpand-once-print-region
      "or" 'alchemist-macroexpand-region
      "oR" 'alchemist-macroexpand-print-region

      "db" 'spacemacs/elixir-toggle-breakpoint)

    (dolist (mode (list alchemist-compile-mode-map
                        alchemist-eval-mode-map
                        alchemist-execute-mode-map
                        alchemist-message-mode-map
                        alchemist-help-minor-mode-map
                        alchemist-mix-mode-map
                        alchemist-macroexpand-mode-map
                        alchemist-refcard-mode-map
                        alchemist-test-report-mode-map))
      (evil-define-key 'normal mode
        (kbd "q") 'quit-window))))

(defun elixir/post-init-company ()
  ;; backend specific
  (add-hook 'elixir-mode-local-vars-hook #'spacemacs//elixir-setup-company))

(defun elixir/pre-init-dap-mode ()
  (when (eq elixir-backend 'lsp) (add-to-list 'spacemacs--dap-supported-modes 'elixir-mode))
  (add-hook 'elixir-mode-local-vars-hook #'spacemacs//elixir-setup-dap))

(defun elixir/post-init-evil-matchit ()
  (add-hook 'elixir-mode-hook `turn-on-evil-matchit-mode))

(defun elixir/init-elixir-mode ()
  (use-package elixir-mode
    :defer t
    :hook ((elixir-mode . spacemacs//elixir-default)
           (elixir-mode-local-vars . spacemacs//elixir-setup-backend))
    :config (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
              "=" 'elixir-format)))

(defun elixir/post-init-flycheck ()
  (spacemacs/enable-flycheck 'elixir-mode))

(defun elixir/init-flycheck-credo ()
  (use-package flycheck-credo
    :defer t
    :init (add-hook 'flycheck-mode-hook #'flycheck-credo-setup)))

(defun elixir/post-init-ggtags ()
  (add-hook 'elixir-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun elixir/pre-init-ob-elixir ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-elixir
      :init (add-to-list 'org-babel-load-languages '(elixir . t)))))
(defun elixir/init-ob-elixir ())

(defun elixir/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*mix*" :tail t :noselect t) popwin:special-display-config)))

(defun elixir/pre-init-smartparens ()
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (progn
      (sp-with-modes '(elixir-mode)
        (sp-local-pair
         "(" ")"
         :unless '(:add spacemacs//elixir-point-after-fn-p))
        (sp-local-pair
         "fn" "end"
         :when '(("SPC" "RET" "-" "("))
         :post-handlers '(:add spacemacs//elixir-do-end-close-action)
         :actions '(insert)))
      (sp-with-modes '(elixir-mode)
        (sp-local-pair
         "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(:add spacemacs//elixir-do-end-close-action)
         :actions '(insert))))))
