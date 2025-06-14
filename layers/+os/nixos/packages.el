;;; packages.el --- NixOS Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2015-2025 Sylvain Benner & Contributors
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


(defconst nixos-packages
  '((company-nixos-options :requires (company nixos-options))
    flycheck
    (helm-nixos-options :requires (helm nixos-options))
    nix-mode
    nixos-options))

(defun nixos/post-init-company ()
  (let ((backends '(company-capf)))
    (when (configuration-layer/package-used-p 'company-nixos-options)
      (add-to-list 'backends 'company-nixos-options t))
    (eval `(spacemacs|add-company-backends
            :backends ,backends
            :modes nix-mode))))

(defun nixos/init-company-nixos-options ()
  (use-package company-nixos-options
    :defer t))

(defun nixos/init-helm-nixos-options ()
  (use-package helm-nixos-options
    :defer t
    :init
    (spacemacs/set-leader-keys
      "h>" 'helm-nixos-options)))

(defun nixos/init-nix-mode ()
  (use-package nix-mode
    :defer t
    :mode "\\.nix\\'"
    :init
    (add-hook 'nix-mode-hook #'spacemacs//nix-setup-backend)
    (add-to-list 'spacemacs-indent-sensitive-modes 'nix-mode)
    (spacemacs/set-leader-keys-for-major-mode 'nix-mode
      "==" 'nix-format-buffer
      "f"  'nix-flake)
    (when nixos-format-on-save
      (add-hook 'before-save-hook 'nix-format-before-save))
    :config
    (electric-indent-mode -1)))

(defun nixos/init-nixos-options ()
  (use-package nixos-options :defer t))

(defun nixos/post-init-flycheck ()
  (spacemacs/enable-flycheck 'nix-mode))
