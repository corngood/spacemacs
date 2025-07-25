;;; packages.el --- Spacemacs Purpose Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Bar Magal
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


(setq spacemacs-purpose-packages
      '(eyebrowse
        (helm-purpose :requires helm)
        (ivy-purpose :requires ivy)
        popwin
        (spacemacs-purpose-popwin
         :location (recipe :fetcher local)
         :requires popwin)
        window-purpose))

(defun spacemacs-purpose/pre-init-eyebrowse ()
  (spacemacs|use-package-add-hook eyebrowse
    :post-config
    (progn
      (add-hook 'purpose-mode-hook #'spacemacs/window-purpose-sync-eyebrowse)
      ;; sync with eyebrowse now
      (spacemacs/window-purpose-sync-eyebrowse))))

(defun spacemacs-purpose/init-helm-purpose ()
  (use-package helm-purpose
    :defer t
    :init
    (setq purpose-preferred-prompt 'helm)
    ;; remap bindings defined with `spacemacs/set-leader-keys'
    (global-set-key [remap purpose-switch-buffer-with-purpose]
                    #'helm-purpose-switch-buffer-with-purpose)
    (global-set-key [remap switch-buffer-without-purpose]
                    #'helm-purpose-mini-ignore-purpose)
    (global-set-key [remap purpose-switch-buffer-with-some-purpose]
                    #'helm-purpose-switch-buffer-with-some-purpose)))

(defun spacemacs-purpose/init-ivy-purpose ()
  ;; vanilla lets `ivy' take over
  (use-package ivy-purpose
    :defer t
    :init
    (setq purpose-preferred-prompt 'vanilla)
    (global-set-key [remap purpose-switch-buffer-with-purpose]
                    #'ivy-purpose-switch-buffer-with-purpose)
    (global-set-key [remap purpose-switch-buffer-without-purpose]
                    #'ivy-purpose-switch-buffer-without-purpose)
    (global-set-key [remap purpose-switch-buffer-with-some-purpose]
                    #'ivy-purpose-switch-buffer-with-some-purpose)))

(defun spacemacs-purpose/pre-init-popwin ()
  ;; when popwin creates a popup window, it removes the `purpose-dedicated'
  ;; window parameter from all windows, so we must save and restore it
  ;; ourselves. this works well as long as no buffer is displayed in more than
  ;; one window. if a buffer is displayed in several windows, and at least one
  ;; of these windows is purpose-dedicated, then all these windows will become
  ;; purpose-dedicated after popwin creates a popup window.
  ;; there is no problem if the local spacemacs-purpose-popwin package is used,
  ;; as long as the user doesn't call `popwin:create-popup-window' directly
  ;; (e.g. <f2> from `helm-mini')
  (spacemacs|use-package-add-hook popwin
    :post-config
    (progn

      (advice-add 'popwin:create-popup-window :before #'spacemacs/window-purpose-save-dedicated-windows)
      (advice-add 'popwin:create-popup-window :after #'spacemacs/window-purpose-restore-dedicated-windows)

      (add-hook 'purpose-mode-hook #'spacemacs/window-purpose-sync-popwin)
      (with-eval-after-load 'window-purpose
        (spacemacs/window-purpose-sync-popwin)
        ;; can't have both `purpose-mode' and `popwin-mode' active at the same
        ;; time (see https://github.com/syl20bnr/spacemacs/issues/9593), but we
        ;; use `popwin' for its configuration so we can't just exclude it, so
        ;; current solution is to disable `popwin-mode' (which is enabled in
        ;; popwin's :config block)
        (popwin-mode -1)))))

(defun spacemacs-purpose/pre-init-spacemacs-purpose-popwin ()
  (spacemacs|use-package-add-hook window-purpose
    :post-config
    (progn
      ;; override popwin commands with pupo commands
      (spacemacs/set-leader-keys
        "wpp" #'pupo/close-window
        "wpP" #'pupo/close-all-windows)
      (pupo-mode))))
(defun spacemacs-purpose/init-spacemacs-purpose-popwin ()
  (use-package spacemacs-purpose-popwin :commands pupo-mode))

(defun spacemacs-purpose/init-window-purpose ()
  (use-package window-purpose
    :defer t
    :init
    (add-hook 'emacs-startup-hook
              (lambda ()
                (spacemacs|add-transient-hook window-configuration-change-hook
                  (lambda () (require 'window-purpose))
                  lazy-load-window-purpose)))
    ;; 'r' is for "puRpose" ('w', 'p' are crowded, 'W', 'P' aren't
    ;; comfortable)
    (spacemacs/set-leader-keys
      "rb" 'purpose-switch-buffer-with-purpose
      "rB" 'switch-buffer-without-purpose
      "rdb" 'purpose-toggle-window-buffer-dedicated
      "rdw" 'purpose-toggle-window-purpose-dedicated
      "rD" 'purpose-delete-non-dedicated-windows
      "rp" 'purpose-switch-buffer-with-some-purpose
      "rP" 'purpose-set-window-purpose)
    :config
    (purpose-mode)
    ;; fix around window-purpose not respecting -other-window requirement
    ;; of clone-indirect-buffer-other-window
    ;; see https://github.com/bmag/emacs-purpose/issues/122
    (defalias 'clone-indirect-buffer-other-window-without-purpose
      (without-purpose-command #'clone-indirect-buffer-other-window))

    ;; change `switch-to-buffer' display preferences according to
    ;; `dotspacemacs-switch-to-buffer-prefers-purpose'. This affects actions
    ;; like `spacemacs/alternate-buffer', and opening buffers from Dired
    (setcdr (assq 'switch-to-buffer purpose-action-sequences)
            (if dotspacemacs-switch-to-buffer-prefers-purpose
                '(purpose-display-reuse-window-buffer
                  purpose-display-reuse-window-purpose
                  purpose-display-maybe-same-window
                  purpose-display-maybe-other-window
                  purpose-display-maybe-other-frame
                  purpose-display-maybe-pop-up-window
                  purpose-display-maybe-pop-up-frame)
              '(purpose-display-maybe-same-window
                purpose-display-reuse-window-buffer
                purpose-display-reuse-window-purpose
                purpose-display-maybe-other-window
                purpose-display-maybe-other-frame
                purpose-display-maybe-pop-up-window
                purpose-display-maybe-pop-up-frame)))
    ;; overriding `purpose-mode-map' with empty keymap, so it doesn't conflict
    ;; with original `C-x C-f', `C-x b', etc. and `semantic' key bindings.
    (setcdr purpose-mode-map nil)
    (spacemacs|diminish purpose-mode)
    (purpose-x-golden-ratio-setup)

    ;; Show magit-log-select and diff in two windows
    (purpose-x-magit-multi-on)))
