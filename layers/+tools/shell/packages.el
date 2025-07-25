;;; packages.el --- shell packages File for Spacemacs  -*- lexical-binding: nil; -*-
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


(defconst shell-packages
  '(
    (comint :location built-in)
    company
    consult
    eat
    esh-help
    (eshell :location built-in)
    eshell-prompt-extras
    eshell-z
    evil-collection
    helm
    ivy
    magit
    multi-term
    org
    projectile
    (shell :location built-in)
    shell-pop
    (term :location built-in)
    (xterm-color :toggle (version< emacs-version "29.0.50"))
    terminal-here
    vi-tilde-fringe
    window-purpose
    (multi-vterm
     :toggle (and shell-enable-vterm-support
                  module-file-suffix
                  (not (spacemacs/system-is-mswindows))))
    (vterm
     :toggle (and shell-enable-vterm-support
                  module-file-suffix
                  (not (spacemacs/system-is-mswindows))))))


(defun shell/init-comint ()
  (setq comint-prompt-read-only t)
  (add-hook 'comint-mode-hook 'spacemacs/disable-hl-line-mode)
  (with-eval-after-load 'centered-cursor-mode
    (add-hook 'comint-mode-hook 'spacemacs//inhibit-global-centered-cursor-mode)))

(defun shell/pre-init-company ()
  ;; support in eshell
  (spacemacs|use-package-add-hook eshell
    :post-init
    (progn
      (spacemacs|add-company-backends :backends company-capf :modes eshell-mode)
      (add-hook 'eshell-directory-change-hook
                'spacemacs//toggle-shell-auto-completion-based-on-path)
      ;; The default frontend screws everything up in short windows like
      ;; terminal often are
      (add-hook 'eshell-mode-hook
                'spacemacs//eshell-switch-company-frontend))))

(defun shell/init-esh-help ()
  (use-package esh-help
    :defer t
    :init (add-hook 'eshell-mode-hook 'eldoc-mode)
    :config (setup-esh-help-eldoc)))

(defun shell/init-eshell ()
  (use-package eshell
    :defer t
    :init
    (spacemacs/register-repl 'eshell 'eshell)
    (setq eshell-cmpl-cycle-completions nil
          ;; auto truncate after 20k lines
          eshell-buffer-maximum-lines 20000
          ;; history size
          eshell-history-size 350
          ;; no duplicates in history
          eshell-hist-ignoredups t
          ;; my prompt is easy enough to see
          eshell-highlight-prompt nil
          ;; treat 'echo' like shell echo
          eshell-plain-echo-behavior t
          ;; cache directory
          eshell-directory-name (concat spacemacs-cache-directory "eshell/"))

    (when shell-protect-eshell-prompt
      (add-hook 'eshell-after-prompt-hook 'spacemacs//protect-eshell-prompt))

    (autoload 'eshell-delchar-or-maybe-eof "em-rebind")

    (add-hook 'eshell-mode-hook 'spacemacs//init-eshell)
    (add-hook 'eshell-mode-hook 'spacemacs/disable-hl-line-mode)
    (with-eval-after-load 'centered-cursor-mode
      (add-hook 'eshell-mode-hook 'spacemacs//inhibit-global-centered-cursor-mode))

    :config
    ;; quick commands
    (defalias 'eshell/e 'find-file-other-window)
    (defalias 'eshell/d 'dired)

    ;; support `em-smart'
    (when shell-enable-smart-eshell
      (require 'em-smart)
      (setq eshell-where-to-jump 'begin
            eshell-review-quick-commands nil
            eshell-smart-space-goes-to-end t)
      (add-hook 'eshell-mode-hook 'eshell-smart-initialize))

    ;; Visual commands
    (require 'em-term)
    (mapc (lambda (x) (add-to-list 'eshell-visual-commands x))
          '("el" "elinks" "htop" "less" "ssh" "tmux" "top"))

    ;; automatically truncate buffer after output
    (when (boundp 'eshell-output-filter-functions)
      (add-hook 'eshell-output-filter-functions #'eshell-truncate-buffer))))

(defun shell/pre-init-evil-collection ()
  (add-to-list 'spacemacs-evil-collection-allowed-list 'vterm))

(defun shell/init-eshell-prompt-extras ()
  (use-package eshell-prompt-extras
    :commands epe-theme-lambda
    :init
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

(defun shell/init-eshell-z ()
  (use-package eshell-z
    :after eshell
    :init
    (setq eshell-z-freq-dir-hash-table-file-name
          (concat spacemacs-cache-directory "eshell/.z"))))

(defun shell/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-init
    (progn
      ;; eshell
      (add-hook 'eshell-mode-hook 'spacemacs/init-helm-eshell)
      ;;shell
      (spacemacs/set-leader-keys-for-major-mode 'shell-mode
        "H" 'spacemacs/helm-shell-history))))

(defun shell/pre-init-ivy ()
  (spacemacs|use-package-add-hook ivy
    :post-init
    (add-hook 'eshell-mode-hook 'spacemacs/init-ivy-eshell))
  (spacemacs/set-leader-keys-for-major-mode 'shell-mode
    "H" 'counsel-shell-history))

(defun shell/pre-init-consult ()
  (spacemacs|use-package-add-hook consult
    :post-init
    (progn
      ;; eshell
      (add-hook 'eshell-mode-hook 'spacemacs/init-consult-eshell)
      ;;shell
      (spacemacs/set-leader-keys-for-major-mode 'shell-mode
        "H" 'spacemacs/consult-shell-history))))

(defun shell/pre-init-magit ()
  (spacemacs|use-package-add-hook magit
    :post-init
    (defalias 's 'magit-status)))

(defun shell/init-multi-term ()
  (use-package multi-term
    :defer t
    :init
    (spacemacs/register-repl 'multi-term 'multi-term)
    :config
    (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))
    ;; multi-term commands to create terminals and move through them.
    (spacemacs/set-leader-keys-for-major-mode 'term-mode
      "c" 'multi-term
      "C" 'term-char-mode
      "l" 'term-line-mode
      "n" 'multi-term-next
      "N" 'multi-term-prev
      "p" 'multi-term-prev)))

(defun shell/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(shell . t))))

(defun shell/post-init-projectile ()
  (spacemacs/set-leader-keys
    "p'" #'spacemacs/projectile-shell-pop
    "p$" #'spacemacs/projectile-shell))

(defun shell/init-shell ()
  (spacemacs/register-repl 'shell 'shell)
  (defun shell-comint-input-sender-hook ()
    "Check certain shell commands.
 Executes the appropriate behavior for certain commands."
    (setq comint-input-sender
          (lambda (proc command)
            (cond
             ;; Check for clear command and execute it.
             ((string-match "^[ \t]*clear[ \t]*$" command)
              (comint-send-string proc "\n")
              (let ((inhibit-read-only  t))
                (erase-buffer)))
             ;; Check for man command and execute it.
             ((string-match "^[ \t]*man[ \t]*" command)
              (comint-send-string proc "\n")
              (setq command (replace-regexp-in-string
                             "^[ \t]*man[ \t]*" "" command))
              (setq command (replace-regexp-in-string
                             "[ \t]+$" "" command))
              (funcall 'man command))
             ;; Send other commands to the default handler.
             (t (comint-simple-send proc command))))))
  (add-hook 'shell-mode-hook 'shell-comint-input-sender-hook)
  (add-hook 'shell-mode-hook 'spacemacs/disable-hl-line-mode)

  (with-eval-after-load 'centered-cursor-mode
    (add-hook 'shell-mode-hook 'spacemacs//inhibit-global-centered-cursor-mode)))

(defun shell/init-shell-pop ()
  (use-package shell-pop
    :defer t
    :init
    (setq shell-pop-window-position shell-default-position
          shell-pop-window-size     shell-default-height
          shell-pop-term-shell      shell-default-term-shell
          shell-pop-full-span       shell-default-full-span)

    ;; Make sure that eshell history is written before the window is closed
    ;; see https://github.com/kyagi/shell-pop-el/issues/66
    (advice-add 'shell-pop--kill-and-delete-window :around #'spacemacs/shell-pop-with-eshell-history-write)

    (make-shell-pop-command "eshell" eshell)
    (make-shell-pop-command "term" term shell-pop-term-shell)
    (make-shell-pop-command "ansi-term" ansi-term shell-pop-term-shell)
    (make-shell-pop-command "inferior-shell" inferior-shell)
    (make-shell-pop-command "multiterm" multiterm)

    (let* ((initial-shell-mode-name (format "%S-mode" shell-default-shell))
           (initial-shell-mode (intern initial-shell-mode-name)))
      (evil-set-initial-state initial-shell-mode 'insert))

    (when (fboundp 'spacemacs/make-variable-layout-local)
      (spacemacs/make-variable-layout-local 'shell-pop-last-shell-buffer-index 1
                                            'shell-pop-last-shell-buffer-name ""
                                            'shell-pop-last-buffer nil))

    (add-hook 'term-mode-hook 'ansi-term-handle-close)

    (spacemacs/set-leader-keys
      "'"   'spacemacs/default-pop-shell
      "atse" 'spacemacs/shell-pop-eshell
      "atsi" 'spacemacs/shell-pop-inferior-shell
      "atsm" 'spacemacs/shell-pop-multiterm
      "atst" 'spacemacs/shell-pop-ansi-term
      "atsT" 'spacemacs/shell-pop-term)
    (spacemacs/declare-prefix "'" "open shell")
    :config
    (add-hook 'shell-pop-out-hook #'spacemacs//shell-pop-restore-window)))

(defun shell/init-term ()
  (spacemacs/register-repl 'term 'term)
  (spacemacs/register-repl 'term 'ansi-term)

  (when (eq dotspacemacs-editing-style 'vim)
    (evil-define-key 'insert term-raw-map
      (kbd "C-k") 'term-send-up
      (kbd "C-j") 'term-send-down))

  (evil-define-key 'insert term-raw-map
    (kbd "<mouse-2>") 'term-mouse-paste
    (kbd "<mouse-3>") 'term-mouse-paste
    (kbd "C-c C-d") 'term-send-eof
    (kbd "C-c C-z") 'term-stop-subjob
    (kbd "<tab>") 'term-send-tab)

  (evil-define-key 'normal term-raw-map
    (kbd "<mouse-2>") 'term-mouse-paste
    (kbd "<mouse-3>") 'term-mouse-paste
    (kbd "C-k") 'term-send-up
    (kbd "C-j") 'term-send-down
    ;; hack to fix pasting issue, the paste transient-state won't work in term
    "p" 'term-paste)

  (add-hook 'term-mode-hook 'spacemacs/disable-hl-line-mode)
  (with-eval-after-load 'centered-cursor-mode
    (add-hook 'term-mode-hook 'spacemacs//inhibit-global-centered-cursor-mode)))

(defun shell/init-xterm-color ()
  (use-package xterm-color
    :init
    ;; Comint and Shell
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output comint-output-filter-functions))
    (add-hook 'eshell-mode-hook 'spacemacs/init-eshell-xterm-color)
    (with-eval-after-load 'eshell
      ;; Work around bug in eshell's preoutput-filter code.
      ;; Eshell doesn't call preoutput-filter functions in the context of the eshell
      ;; buffer. This breaks the xterm color filtering when the eshell buffer is updated
      ;; when it's not currently focused.
      ;; To remove if/when fixed upstream.
      (defun eshell-output-filter@spacemacs-with-buffer (fn process string)
        (let ((proc-buf (if process (process-buffer process)
                          (current-buffer))))
          (when proc-buf
            (with-current-buffer proc-buf
              (funcall fn process string)))))
      (advice-add
       #'eshell-output-filter
       :around
       #'eshell-output-filter@spacemacs-with-buffer))))

(defun shell/init-terminal-here ()
  (use-package terminal-here
    :defer t
    :commands (terminal-here-launch terminal-here-project-launch)
    :init
    (spacemacs/register-repl 'terminal-here 'terminal-here)
    (spacemacs/set-leader-keys
      "\"" 'terminal-here-launch
      "p \"" 'terminal-here-project-launch)))


(defun shell/post-init-vi-tilde-fringe ()
  (spacemacs/add-to-hooks 'spacemacs/disable-vi-tilde-fringe
                          '(comint-mode-hook
                            eshell-mode-hook
                            shell-mode-hook
                            term-mode-hook)))

(defun shell/init-eat ()
  (use-package eat
    :defer t
    :commands (eat eat-other-window eat-project eat-project-other-window)
    :init
    (make-shell-pop-command "eat" spacemacs//eat-for-shell-pop)
    (spacemacs/set-leader-keys "atsa" 'spacemacs/shell-pop-eat)
    (spacemacs/register-repl 'eat 'eat)
    :config
    (setq eat-shell shell-default-term-shell)
    (add-hook 'eat-mode-hook 'spacemacs/disable-hl-line-mode)))

(defun shell/init-vterm ()
  (use-package vterm
    :defer t
    :commands (vterm vterm-other-window)
    :init
    (make-shell-pop-command "vterm" vterm)
    (spacemacs/set-leader-keys "atsv" 'spacemacs/shell-pop-vterm)
    (spacemacs/register-repl 'vterm 'vterm)
    :config
    (setq vterm-shell shell-default-term-shell)
    (define-key vterm-mode-map (kbd "M-n") 'vterm-send-down)
    (define-key vterm-mode-map (kbd "M-p") 'vterm-send-up)
    (define-key vterm-mode-map (kbd "M-y") 'vterm-yank-pop)
    (define-key vterm-mode-map (kbd "M-/") 'vterm-send-tab)
    (when spacemacs-vterm-history-file-location
      (spacemacs//vterm-bind-m-r vterm-mode-map))
    (evil-define-key 'insert vterm-mode-map (kbd "C-y") 'vterm-yank)
    (evil-define-key 'insert vterm-mode-map (kbd "C-o") 'evil-execute-in-normal-state)
    (evil-define-key 'normal vterm-mode-map
      [escape] 'vterm-send-escape
      [return] 'vterm-send-return
      (kbd "p") 'vterm-yank
      (kbd "u") 'vterm-undo)
    (add-hook 'vterm-mode-hook 'spacemacs/disable-hl-line-mode)
    (with-eval-after-load 'centered-cursor-mode
      (add-hook 'vterm-mode-hook 'spacemacs//inhibit-global-centered-cursor-mode))))

(defun shell/init-multi-vterm ()
  (use-package multi-vterm
    :defer t
    :init
    (make-shell-pop-command "multivterm" multivterm)
    (spacemacs/set-leader-keys "atsM" 'spacemacs/shell-pop-multivterm)
    (spacemacs/register-repl 'multi-vterm 'multi-vterm)
    :config
    (setq vterm-shell shell-default-term-shell)
    ;; multi-term commands to create terminals and move through them.
    (spacemacs/set-leader-keys-for-major-mode 'vterm-mode
      "c" 'multi-vterm
      "n" 'multi-vterm-next
      "N" 'multi-vterm-prev
      "p" 'multi-vterm-prev
      "r" 'multi-vterm-rename-buffer)))

(defun shell/post-init-window-purpose ()
  (purpose-set-extension-configuration
   :shell-layer
   (purpose-conf :mode-purposes '((vterm-mode . terminal)
                                  (eshell-mode . terminal)
                                  (shell-mode . terminal)
                                  (term-mode . terminal)))))
