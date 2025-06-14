;;; packages.el --- Lua Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
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


(defconst lua-packages
  '(
    company
    (company-lua :requires company :toggle (eq lua-backend 'lua-mode))
    flycheck
    ggtags
    lua-mode))

(defun lua/post-init-flycheck ()
  (spacemacs/enable-flycheck 'lua-mode))

(defun lua/init-lua-mode ()
  (use-package lua-mode
    :defer t
    :mode ("\\.lua\\'" . lua-mode)
    :interpreter ("lua" . lua-mode)
    :init
    (spacemacs/register-repl 'lua #'lua-show-process-buffer "lua")
    (add-hook 'lua-mode-local-vars-hook #'spacemacs//lua-setup-backend)

    ;; Set global settings
    (setq lua-indent-level 2
          lua-indent-string-contents t)

    ;; Set general bindings
    (spacemacs/declare-prefix-for-mode 'lua-mode "ms" "REPL")
    (spacemacs/set-leader-keys-for-major-mode 'lua-mode
      "hd" 'lua-search-documentation
      "sb" 'lua-send-buffer
      "sf" 'lua-send-defun
      "sl" 'lua-send-current-line
      "sr" 'lua-send-region
      "'" 'lua-show-process-buffer)

    ;; Set lua-mode specific bindings
    (when (eq lua-backend 'lua-mode)
      (spacemacs/declare-prefix-for-mode 'lua-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'lua-mode "mg" "goto"))))

(defun lua/post-init-company ()
  (add-hook 'lua-mode-local-vars-hook #'spacemacs//lua-setup-company))

(defun lua/init-company-lua ()
  (use-package company-lua
    :defer t))

(defun lua/post-init-ggtags ()
  (add-hook 'lua-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))
