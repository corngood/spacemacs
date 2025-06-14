;;; packages.el --- mu4e Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
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


(defconst mu4e-packages
  '((mu4e :location site)
    (mu4e-alert :toggle (or mu4e-enable-notifications mu4e-enable-mode-line))
    (helm-mu :requires helm)
    org
    persp-mode
    window-purpose))

(defun mu4e/post-init-persp-mode ()
  (spacemacs|define-custom-layout mu4e-spacemacs-layout-name
    :binding mu4e-spacemacs-layout-binding
    :body
    (progn
      (defun spacemacs-layouts/add-mu4e-buffer-to-persp ()
        (persp-add-buffer (current-buffer)
                          (persp-get-by-name
                           mu4e-spacemacs-layout-name)))
      (spacemacs/add-to-hooks 'spacemacs-layouts/add-mu4e-buffer-to-persp
                              '(mu4e-main-mode-hook
                                mu4e-headers-mode-hook
                                mu4e-after-view-message-hook
                                mu4e-compose-mode-hook))
      (call-interactively 'mu4e)
      (define-advice mu4e~stop (:after nil kill-mu4e-layout-after-mu4e~stop)
        (when mu4e-spacemacs-kill-layout-on-exit
          (persp-kill mu4e-spacemacs-layout-name))))))

(defun mu4e/init-mu4e ()
  ;; Some distributions do not construct the initial Emacs load paths properly
  ;; so that packages are included in `package-directory-list' and thereby
  ;; activated at startup.  To avoid breakage, now that Spacemacs defers loading
  ;; mu4e, load `mu4e-autoloads' explicitly.
  ;;
  ;; See https://github.com/syl20bnr/spacemacs/issues/16931
  (condition-case _
      (require 'mu4e-autoloads)
    (error
     (spacemacs-buffer/error "Could not activate `mu4e'.  It is a site package and must be installed with your Emacs distribution")))
  (use-package mu4e
    :defer t
    :init
    (spacemacs/set-leader-keys "aem" 'mu4e)
    (setq mail-user-agent 'mu4e-user-agent
          read-mail-command 'mu4e
          mu4e-completing-read-function 'completing-read
          mu4e-use-fancy-chars t
          mu4e-view-show-images t
          message-kill-buffer-on-exit t
          mu4e-org-support nil)
    (let ((dir "~/Downloads/"))
      (when (file-directory-p dir)
        (setq mu4e-attachment-dir dir)))

    :config
    (evilified-state-evilify-map mu4e-main-mode-map
      :mode mu4e-main-mode
      :bindings
      (kbd "j") 'mu4e-search-maildir
      (kbd "C-j") 'next-line
      (kbd "C-k") 'previous-line)

    (evilified-state-evilify-map
      mu4e-headers-mode-map
      :mode mu4e-headers-mode
      :bindings
      (kbd "C-j") 'mu4e-headers-next
      (kbd "C-k") 'mu4e-headers-prev
      (kbd "J")   'mu4e/headers-mark-thread)

    (evilified-state-evilify-map
      mu4e-view-mode-map
      :mode mu4e-view-mode
      :bindings
      (kbd "C-j") 'mu4e-view-headers-next
      (kbd "C-k") 'mu4e-view-headers-prev
      (kbd "J")   'mu4e/view-mark-thread-read
      (kbd "gu")  'mu4e-view-go-to-url)

    (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
      dotspacemacs-major-mode-leader-key 'message-send-and-exit
      "c" 'message-send-and-exit
      "k" 'message-kill-buffer
      "a" 'message-kill-buffer
      "s" 'message-dont-send         ; saves as draft
      "f" 'mml-attach-file)

    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))

    (when mu4e-autorun-background-at-startup
      (mu4e t))

    (add-hook 'mu4e-compose-mode-hook
              (lambda () (use-hard-newlines t 'guess)))

    ;; from http://www.djcbsoftware.nl/code/mu/mu4e/Attaching-files-with-dired.html
    (require 'gnus-dired)
    ;; make the `gnus-dired-mail-buffers' function also work on
    ;; message-mode derived modes, such as mu4e-compose-mode
    (defun gnus-dired-mail-buffers ()
      "Return a list of active message buffers."
      (let (buffers)
        (save-current-buffer
          (dolist (buffer (buffer-list t))
            (set-buffer buffer)
            (when (and (derived-mode-p 'message-mode)
                       (null message-sent-message-via))
              (push (buffer-name buffer) buffers))))
        (nreverse buffers)))
    (setq gnus-dired-mail-mode 'mu4e-user-agent)
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)))

(defun mu4e/init-mu4e-alert ()
  (use-package mu4e-alert
    :defer t
    :init (with-eval-after-load 'mu4e
            (when mu4e-enable-notifications
              (mu4e-alert-enable-notifications))
            (when mu4e-enable-mode-line
              (mu4e-alert-enable-mode-line-display)))))

(defun mu4e/init-helm-mu ()
  (use-package helm-mu
    :defer t
    :init (dolist (m mu4e-modes)
            (spacemacs/set-leader-keys-for-major-mode m
              "S" 'helm-mu
              "/" 'helm-mu
              "C" 'helm-mu-contacts))))

(defun mu4e/pre-init-org ()
  (when mu4e-org-link-support
    (with-eval-after-load 'org
      (with-demoted-errors "(Spacemacs) Error loading a mu4e feature: %S"
        ;; This is a dirty hack due to mu(4e) 1.8.2 renaming mu4e-meta to
        ;; mu4e-config.  See also
        ;; https://github.com/djcb/mu/commit/cf0f72e4a48ac7029d7f6758b182d4bb559f8f49
        ;; and https://github.com/syl20bnr/spacemacs/issues/15618.  This code
        ;; used to simply read: (require 'mu4e-meta).  We now attempt to load
        ;; mu4e-config.  If this fails, load mu4e-meta.
        (unless (require 'mu4e-config nil t)
          (require 'mu4e-meta))
        (if (version<= mu4e-mu-version "1.3.5")
            (require 'org-mu4e)
          (require 'mu4e-org))
        ;; We require mu4e due to an existing bug https://github.com/djcb/mu/issues/1829
        ;; Note that this bug prevents lazy-loading.
        (when (version<= mu4e-mu-version "1.4.15")
          (require 'mu4e)))))
  (when mu4e-org-compose-support
    (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
      "o" 'org-mu4e-compose-org-mode)
    (autoload 'org-mu4e-compose-org-mode "org-mu4e")))

(defun mu4e/post-init-window-purpose ()
  (let ((modes))
    (dolist (mode mu4e-list-modes)
      (push (cons mode 'mail) modes))
    (dolist (mode mu4e-view-modes)
      (push (cons mode 'mail-view) modes))
    (purpose-set-extension-configuration
     :mu4e-layer
     (purpose-conf :mode-purposes modes))))
