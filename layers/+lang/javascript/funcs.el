;;; funcs.el --- Javascript Layer functions File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
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



;; backend

(defun spacemacs//javascript-setup-backend ()
  "Conditionally setup javascript backend."
  (pcase javascript-backend
    ('tern (spacemacs//javascript-setup-tern))
    ('tide (spacemacs//tide-setup))
    ('lsp (spacemacs//javascript-setup-lsp))))

(defun spacemacs//javascript-setup-company ()
  "Conditionally setup company based on backend."
  (when (eq javascript-backend 'tide)
    (spacemacs//tide-setup-company 'js2-mode)))

(defun spacemacs//javascript-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (when (eq javascript-backend 'lsp)
    (spacemacs//javascript-setup-lsp-dap)))

(defun spacemacs//javascript-setup-next-error-fn ()
  "If the `syntax-checking' layer is enabled, then disable `js2-mode''s
`next-error-function', and let `flycheck' handle any errors."
  (when (configuration-layer/layer-used-p 'syntax-checking)
    (setq-local next-error-function nil)))

;; lsp

(defun spacemacs//javascript-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (unless javascript-lsp-linter
          (setq-local lsp-diagnostics-provider :none))
        (lsp-deferred))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dotfile."))))

(defun spacemacs//javascript-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-firefox)
  (require 'dap-chrome))


;; tern
(defun spacemacs//javascript-setup-tern ()
  (if (configuration-layer/layer-used-p 'tern)
      (when (locate-file "tern" exec-path)
        (spacemacs/tern-setup-tern))
    (message (concat "Tern was configured as the javascript backend but "
                     "the `tern' layer is not present in your `.spacemacs'!"))))


;; js-doc

(defun spacemacs/js-doc-require ()
  "Lazy load js-doc"
  (require 'js-doc))

(defun spacemacs/js-doc-set-key-bindings (mode)
  "Setup the key bindings for `js2-doc' for the given MODE."
  (spacemacs/declare-prefix-for-mode mode "mrd" "documentation")
  (spacemacs/set-leader-keys-for-major-mode mode
    "rdb" 'js-doc-insert-file-doc
    "rdf" (if (configuration-layer/package-used-p 'yasnippet)
              'js-doc-insert-function-doc-snippet
            'js-doc-insert-function-doc)
    "rdt" 'js-doc-insert-tag
    "rdh" 'js-doc-describe-tag))

;; js-refactor

(defun spacemacs/js2-refactor-require ()
  "Lazy load js2-refactor"
  (require 'js2-refactor))


;; skewer

(defun spacemacs/skewer-start-repl ()
  "Attach a browser to Emacs and start a skewer REPL."
  (interactive)
  (run-skewer)
  (skewer-repl))

(defun spacemacs/skewer-load-buffer-and-focus ()
  "Execute whole buffer in browser and switch to REPL in insert state."
  (interactive)
  (skewer-load-buffer)
  (skewer-repl)
  (evil-insert-state))

(defun spacemacs/skewer-eval-defun-and-focus ()
  "Execute function at point in browser and switch to REPL in insert state."
  (interactive)
  (skewer-eval-defun)
  (skewer-repl)
  (evil-insert-state))

(defun spacemacs/skewer-eval-region (beg end)
  "Execute the region as JavaScript code in the attached browser."
  (interactive "r")
  (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

(defun spacemacs/skewer-eval-region-and-focus (beg end)
  "Execute the region in browser and swith to REPL in insert state."
  (interactive "r")
  (spacemacs/skewer-eval-region beg end)
  (skewer-repl)
  (evil-insert-state))


;; Others

(defun spacemacs//javascript-setup-checkers ()
  (when-let* ((found (executable-find "eslint_d")))
    (setq-local flycheck-javascript-eslint-executable found)))

(defun spacemacs/javascript-format ()
  "Call formatting tool specified in `javascript-fmt-tool'."
  (interactive)
  (pcase javascript-fmt-tool
    ('prettier (call-interactively 'prettier-js))
    ('web-beautify (call-interactively 'web-beautify-js))
    (_ (error (concat "%s isn't valid javascript-fmt-tool value."
                      " It should be 'web-beutify or 'prettier.")
              (symbol-name javascript-fmt-tool)))))

(defun spacemacs/javascript-fmt-before-save-hook ()
  (add-hook 'before-save-hook 'spacemacs/javascript-format t t))
