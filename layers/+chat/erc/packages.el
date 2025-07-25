;;; packages.el --- erc Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
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


(defconst erc-packages
  '(
    company
    company-emoji
    emoji-cheat-sheet-plus
    erc
    erc-tweet
    (erc-gitter :location (recipe
                           :fetcher github
                           :repo "jleechpe/erc-gitter")
                :excluded t)
    erc-hl-nicks
    erc-image
    erc-social-graph
    (erc-terminal-notifier :toggle (spacemacs/system-is-mac))
    (erc-tex :location local)
    erc-view-log
    (erc-yank :location local :excluded t)
    erc-yt
    persp-mode
    window-purpose
    ))

(defun erc/post-init-company ()
  (spacemacs|add-company-backends :backends company-capf :modes erc-mode))

(defun erc/post-init-company-emoji ()
  (spacemacs|add-company-backends :backends company-emoji :modes erc-mode))

(defun erc/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'erc-mode-hook 'emoji-cheat-sheet-plus-display-mode))

(defun erc/init-erc ()
  "Initialize ERC"
  (use-package erc
    :defer t
    :init
    (spacemacs/set-leader-keys
      "acie" 'erc
      "aciE" 'erc-tls
      "acii" 'erc-track-switch-buffer
      "aciD" 'erc/default-servers)
    (spacemacs/declare-prefix "aci"  "irc")
    ;; utf-8 always and forever
    (setq erc-server-coding-system '(utf-8 . utf-8))
    :config
    (use-package erc-autoaway
      :defer t
      :init
      (setq erc-auto-discard-away t
            erc-autoaway-idle-seconds 600
            erc-autoaway-use-emacs-idle t))
    (erc-services-mode 1)
    (defun erc-list-command ()
      "execute the list command"
      (interactive)
      (insert "/list")
      (erc-send-current-line))
    (setq erc-kill-buffer-on-part t
          erc-kill-queries-on-quit t
          erc-kill-server-buffer-on-quit t)
    (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))
    (erc-track-mode t)
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
          erc-server-coding-system '(utf-8 . utf-8))
    (setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))
    (erc-spelling-mode 1)
    (setq erc-interpret-mirc-color t)
    (when erc-enable-sasl-auth
      (add-to-list 'erc-modules 'sasl))

    ;; Notifications are enabled if erc-enable-notifications is non-nil, and
    ;; D-BUS is available (i.e. Linux/BSD).
    (when (and erc-enable-notifications (boundp 'dbus-compiled-version))
      (require 'notifications)
      (defun erc-global-notify (match-type nick message)
        "Notify when a message is received."
        (notifications-notify
         :title nick
         :body message
         :app-icon (concat spacemacs-assets-directory "spacemacs.svg")
         :urgency 'low))
      (add-hook 'erc-text-matched-hook 'erc-global-notify))

    ;; keybindings
    (spacemacs/set-leader-keys-for-major-mode 'erc-mode
      "b" 'erc-switch-to-buffer
      "d" 'erc-input-action
      "j" 'erc-join-channel
      "n" 'erc-channel-names
      "l" 'erc-list-command
      "c" 'spacemacs/erc-find-channel-log
      "p" 'erc-part-from-channel
      "q" 'erc-quit-server)))

(defun erc/init-erc-gitter ()
  (use-package erc-gitter
    :config
    (add-to-list 'erc-modules 'gitter)))

(defun erc/pre-init-erc-hl-nicks ()
  (spacemacs|use-package-add-hook erc
    :post-config
    (use-package erc-hl-nicks)))
(defun erc/init-erc-hl-nicks ())

(defun erc/pre-init-erc-social-graph ()
  (spacemacs|use-package-add-hook erc
    :post-config
    (use-package erc-social-graph
      :config
      (erc-social-graph-enable)
      (setq erc-social-graph-dynamic-graph t)
      (spacemacs/set-leader-keys-for-major-mode 'erc-mode
        "D" 'erc-social-graph-draw))))
(defun erc/init-erc-social-graph ())

(defun erc/pre-init-erc-tex ()
  (spacemacs|use-package-add-hook erc
    :post-config
    (require 'erc-tex)))
(defun erc/init-erc-tex ())

(defun erc/pre-init-erc-yt ()
  (spacemacs|use-package-add-hook erc
    :post-config
    (use-package erc-yt
      :init (with-eval-after-load 'erc
              (add-to-list 'erc-modules 'youtube)))))
(defun erc/init-erc-yt ())

(defun erc/pre-init-erc-tweet ()
  (spacemacs|use-package-add-hook erc
    :post-config
    (use-package erc-tweet
      :init (with-eval-after-load 'erc
              (add-to-list 'erc-modules 'tweet)))))
(defun erc/init-erc-tweet ())

(defun erc/pre-init-erc-yank ()
  (spacemacs|use-package-add-hook erc
    :post-config
    (use-package erc-yank
      :if (configuration-layer/package-used-p 'gist)
      :init (evil-define-key 'normal erc-mode-map "p" 'erc-yank))))
(defun erc/init-erc-yank ())

(defun erc/init-erc-view-log ()
  (use-package erc-view-log
    :defer t
    :commands (spacemacs/erc-find-channel-log)
    :init
    (setq erc-log-channels-directory
          (expand-file-name
           (concat spacemacs-cache-directory
                   "erc-logs")))
    (unless (file-exists-p erc-log-channels-directory)
      (make-directory erc-log-channels-directory))
    (add-to-list 'auto-mode-alist
                 `(,(format "%s/.*\\.[log|txt]"
                            (regexp-quote
                             (expand-file-name
                              erc-log-channels-directory))) . erc-view-log-mode))
    (with-eval-after-load 'erc (add-to-list 'erc-modules 'log))
    :config
    ;; ERC Logging
    ;; Following https://raw.githubusercontent.com/Niluge-KiWi/erc-view-log/master/erc-view-log.el
    ;; installation instructions
    (add-hook 'erc-view-log-mode-hook 'turn-on-auto-revert-tail-mode)

    (spacemacs|define-transient-state erc-log
      :title "ERC Log Transient State"
      :doc "\n[_r_] reload the log file  [_>_/_<_] go to the next/prev mention"
      :evil-leader-for-mode (erc-view-log-mode . ".")
      :bindings
      ("r" erc-view-log-reload-file)
      (">" erc-view-log-next-mention)
      ("<" erc-view-log-previous-mention))

    (defun spacemacs/erc-find-channel-log ()
      "find current erc channel's log file in `erc-view-log-mode'"
      (interactive)
      (when (erc-logging-enabled)
        (find-file-existing (erc-current-logfile))))))

(defun erc/init-erc-image ()
  (use-package erc-image
    :defer t
    :init (with-eval-after-load 'erc
            (require 'erc-image)
            (add-to-list 'erc-modules 'image))))

(defun erc/init-erc-terminal-notifier ()
  (use-package erc-terminal-notifier
    :if (executable-find "terminal-notifier")))

(defun erc/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (progn
      (add-to-list 'persp-filter-save-buffers-functions
                   'spacemacs//erc-persp-filter-save-buffers-function)
      (spacemacs|define-custom-layout erc-spacemacs-layout-name
        :binding erc-spacemacs-layout-binding
        :body
        (progn
          (add-hook 'erc-mode-hook #'spacemacs//erc-buffer-to-persp)
          (if erc-server-list
              (erc/default-servers)
            (call-interactively 'erc)))))))

(defun erc/post-init-window-purpose ()
  (purpose-set-extension-configuration
   :erc-layer
   (purpose-conf :mode-purposes '((erc-mode . chat)))))
