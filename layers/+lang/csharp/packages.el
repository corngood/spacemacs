;;; packages.el --- C# Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
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


(setq csharp-packages
      '(
        company
        ;; (csharp-mode :toggle (version< emacs-version "29.0.60"))
        csharp-mode
        evil-matchit
        ggtags
        (omnisharp :location (recipe
                            :fetcher github
                            :repo "corngood/omnisharp-emacs"
                            :branch "master"
                            :files ("*.el"
                                    "src/*.el"
                                    "src/actions/*.el"))
                   :toggle (eq csharp-backend 'omnisharp))
        flycheck
        dap-mode))

(defun csharp/init-omnisharp ()
  (use-package omnisharp
    :defer t
    :init
    (spacemacs//csharp-setup-omnisharp)
    :config
    (spacemacs//csharp-configure-omnisharp)
    ))

(defun csharp/post-init-company ()
  (spacemacs//csharp-setup-company))

(defun csharp/init-csharp-mode ()
  (use-package csharp-mode
    :defer t
    :config
    (when (eq csharp-backend 'lsp)
      (spacemacs//csharp-setup-lsp))))

(defun csharp/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'csharp-mode
               '((evilmi-simple-get-tag evilmi-simple-jump)
                 (evilmi-c-get-tag evilmi-c-jump))))
  (add-hook 'csharp-mode-hook 'turn-on-evil-matchit-mode))

(defun csharp/post-init-flycheck ()
  (spacemacs/enable-flycheck 'csharp-mode))

(defun csharp/post-init-ggtags ()
  (add-hook 'csharp-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun csharp/pre-init-dap-mode ()
  (with-eval-after-load 'dap-mode
    (dap-register-debug-provider
     "unity"
     (lambda (conf)
       (plist-put conf :path
                  (concat
                   (projectile-project-root)
                   (file-name-as-directory "Library")
                   "EditorInstance.json"))
       (plist-put conf :cwd (projectile-project-root))
       (plist-put conf :dap-server-path '("UnityDebug"))))
    (dap-register-debug-template
     "Unity Editor"
     (list :type "unity"
           :request "launch"
           :name "Unity Editor")))
  (add-to-list 'spacemacs--dap-supported-modes 'csharp-mode)
  (add-hook 'csharp-mode-local-vars-hook (lambda () (require 'dap-mode))))
