;;; core-spacemacs.el --- Spacemacs Core File -*- lexical-binding: t -*-
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

(setq message-log-max 16384)

(defgroup spacemacs nil
  "Spacemacs customizations."
  :group 'emacs
  :prefix 'spacemacs-)

(require 'subr-x nil 'noerror)
(require 'core-versions)
(require 'core-load-paths)
(require 'core-emacs-backports)
(require 'core-env)
(require 'core-hooks)
(require 'core-debug)
(require 'core-command-line)
(require 'core-configuration-layer)
(require 'core-dotspacemacs)
(require 'core-custom-settings)
(require 'core-release-management)
(require 'core-jump)
(require 'core-display-init)
(require 'core-themes-support)
(require 'core-fonts-support)
(require 'core-spacemacs-buffer)
(require 'core-keybindings)
(require 'core-toggle)
(require 'core-early-funcs)
(require 'core-funcs)
(require 'core-micro-state)
(require 'core-transient-state)
(require 'core-use-package-ext)
(require 'core-spacebind)
(require 'core-compilation)

(defvar spacemacs-post-user-config-hook nil
  "Hook run after dotspacemacs/user-config")
(defvar spacemacs-post-user-config-hook-run nil
  "Whether `spacemacs-post-user-config-hook' has been run")
(defvar spacemacs-scratch-mode-hook nil
  "Hook run on buffer *scratch* after `dotspacemacs-scratch-mode' is invoked.")

(defvar spacemacs--default-mode-line mode-line-format
  "Backup of default mode line format.")

(defvar spacemacs-initialized nil
  "Whether or not spacemacs has finished initializing by completing
the final step of executing code in `emacs-startup-hook'.")

(defun spacemacs//package-regenerate-autoloads (&optional path)
  "Regenerate the autoloads for installed packages."
  (interactive "P")
  (dolist (dir (or path (list package-user-dir)))
    (when (file-directory-p dir)
      (dolist (pkg-dir (directory-files dir t "\\`[^.]"))
        (when-let* (((file-directory-p pkg-dir))
                    (pkg-desc (package-load-descriptor pkg-dir)))
          (let ((default-directory pkg-dir))
            (mapc 'delete-file (file-expand-wildcards "*-autoloads.el" )))
          (package-generate-autoloads
           (package-desc-name pkg-desc) pkg-dir))))))

(defun spacemacs//lookup-load-hints (file)
  "Findout the `load-hints' item for the FILE."
  (unless (file-name-absolute-p file)
    (car-safe (seq-find (lambda (row) (member file (cdr row))) load-hints))))

(defun spacemacs//activate-load-hints ()
  "Enable the `load-hints' support for Spacemacs."
  (setq package-enable-load-hints dotspacemacs-enable-load-hints
        load-hints
        (mapcar
         (lambda (path)
           (when-let* (((file-directory-p path))
                       (files (seq-difference
                               (directory-files path) '("." "..")
                               #'string=))
                       ;; list of files basename, the load-suffixes was removed
                       (bases
                        (mapcar
                         (lambda (f)
                           (seq-some
                            (lambda (s)
                              (if-let* ((n (length s))
                                        ((length> f n))
                                        ((string= s (substring f (- n)))))
                                  (substring f 0 (- n))))
                            (get-load-suffixes)))
                         files)))
             (cons path (seq-uniq (remove nil bases) 'string-equal))))
         load-path))

  (defun require@LOAD-HINTS (args)
    "The advice for `require' function"
    (let ((feature (nth 0 args))
          (filename (nth 1 args))
          (noerror (nth 2 args)))
      (when-let* (((not filename))
                  (name (symbol-name feature))
                  (path (spacemacs//lookup-load-hints name)))
        (setq filename (expand-file-name name path)))
      (list feature filename noerror)))

  (advice-add #'require :filter-args #'require@LOAD-HINTS)

  (define-advice package-generate-autoloads (:after (name pkg-dir) LOAD-HINTS)
    ;; if package-enabled-load-hints is non-nil then collecting loadable
    ;; files in pkg-dir and generating the load-hints list.
    (when-let* (dotspacemacs-enable-load-hints
                (auto-name (format "%s-autoloads.el" name))
                (output-file (expand-file-name auto-name pkg-dir))
                (name (symbol-name name))
                (files (seq-difference
                        (directory-files pkg-dir)
                        `("." ".." ,(concat name "-pkg.el") ,auto-name)
                        #'string=))
                ;; list of files basename, the load-suffixes was removed
                (bases
                 (remove nil
                         (mapcar
                          (lambda (f)
                            (seq-some
                             (lambda (s)
                               (if-let* ((n (length s))
                                         ((length> f n))
                                         ((string= s (substring f (- n)))))
                                   (substring f 0 (- n))))
                             (get-load-suffixes)))
                          files))))
      (with-current-buffer (find-file-noselect output-file)
        (goto-char (point-min))
        (when (re-search-forward "add-to-list 'load-path" nil t)
          (forward-line 0)
          (insert (format "(add-to-list 'load-hints (cons %S '%S))\n"
                          '(or (and load-file-name
                                    (directory-file-name
                                     (file-name-directory load-file-name)))
                               (car load-path))
                          (seq-uniq bases 'string-equal))))
        (save-buffer)
        (kill-buffer)))))

(defun spacemacs/init ()
  "Perform startup initialization."
  (setq command-line-args (spacemacs//parse-command-line command-line-args))
  (when spacemacs-debugp (spacemacs/init-debug))
  ;; silence ad-handle-definition about advised functions getting redefined
  (setq ad-redefinition-action 'accept)
  ;; this is for a smoother UX at startup (i.e. less graphical glitches)
  (hidden-mode-line-mode)
  (spacemacs//toggle-gui-elements 0)
  (spacemacs//setup-ido-vertical-mode)
  ;; explicitly set the preferred coding systems to avoid annoying prompt
  ;; from emacs (especially on Microsoft Windows)
  (prefer-coding-system 'utf-8)
  ;; Extend use package if already installed
  (spacemacs/use-package-extend)
  ;; TODO move these variables when evil is removed from the bootstrapped
  ;; packages.
  (setq-default evil-want-C-u-scroll t
                ;; `evil-want-C-i-jump' is set to nil to avoid `TAB' being
                ;; overlapped in terminal mode. The GUI specific `<C-i>' is used
                ;; instead.
                evil-want-C-i-jump nil)
  (dotspacemacs/load-file)
  (dotspacemacs|call-func dotspacemacs/init "Calling dotfile init...")
  (when dotspacemacs-undecorated-at-startup
    ;; this should be called before toggle-frame-maximized
    (set-frame-parameter nil 'undecorated t)
    (set-frame-parameter nil 'internal-border-width 0)
    (add-to-list 'default-frame-alist '(undecorated . t))
    (add-to-list 'default-frame-alist '(internal-border-width . 0)))
  (when dotspacemacs-maximized-at-startup
    (unless (frame-parameter nil 'fullscreen)
      (toggle-frame-maximized))
    (add-to-list 'default-frame-alist '(fullscreen . maximized)))
  (dotspacemacs|call-func dotspacemacs/user-init "Calling dotfile user init...")
  ;; Given the loading process of Spacemacs we have no choice but to set the
  ;; custom settings twice:
  ;; - once at the very beginning of startup (here)
  ;; - once at the very end of loading (in `spacemacs/setup-startup-hook')
  ;; The first application of custom settings is to be sure that Emacs knows all
  ;; the defined settings before saving them to a file (otherwise we loose all
  ;; the settings that Emacs does not know of).
  ;; The second application is to override any setting set in dotfile functions
  ;; like `dotspacemacs/user-config`, users expect the custom settings to be the
  ;; effective ones.
  ;; Note: Loading custom-settings twice is not ideal since they can have side
  ;; effects! Maybe an inhibit variable in Emacs can suppress these side effects?
  (spacemacs/initialize-custom-file)
  ;; Commenting the first load although it is mentioned above that we must do it
  ;; I don't recall why we must load the custom settings twice and my experiment
  ;; seems to show that we don't need this double loading process anymore.
  ;; related issue: https://github.com/syl20bnr/spacemacs/issues/9736
  ;; (dotspacemacs|call-func dotspacemacs/emacs-custom-settings
  ;;                         "Calling dotfile Emacs custom settings...")
  (setq dotspacemacs-editing-style (dotspacemacs//read-editing-style-config
                                    dotspacemacs-editing-style))
  (configuration-layer/initialize)
  ;; frame title init
  (when dotspacemacs-frame-title-format
    (require 'format-spec)
    (setq frame-title-format '((:eval (spacemacs/title-prepare dotspacemacs-frame-title-format))))
    (if dotspacemacs-icon-title-format
        (setq icon-title-format '((:eval (spacemacs/title-prepare dotspacemacs-icon-title-format))))
      (setq icon-title-format frame-title-format)))

  (when (and dotspacemacs-enable-load-hints (not (boundp 'load-hints)))
    (spacemacs//activate-load-hints))

  (unless (boundp 'load-hints) ; make sure the `load-hints' avaliable for the
    (defvar load-hints '()))   ; *-autoloads.el after the feature was toggled.

  ;; theme
  (spacemacs/load-default-theme)
  ;; font
  (spacemacs|do-after-display-system-init
    (unless (spacemacs/set-default-font dotspacemacs-default-font)
      (spacemacs-buffer/warning
       "Cannot find any of the specified fonts (%s)! Font settings may not be correct."
       (if (listp (car dotspacemacs-default-font))
           (mapconcat 'car dotspacemacs-default-font ", ")
         (car dotspacemacs-default-font)))))
  ;; spacemacs init
  (setq inhibit-startup-screen t)

  ;; Draw the spacemacs buffer without lists and scalling to avoid having
  ;; to load build-in org which will conflict with elpa org
  (spacemacs-buffer/goto-buffer t)

  ;; This is set to nil during startup to allow Spacemacs to show buffers opened
  ;; as command line arguments.
  (setq initial-buffer-choice nil)
  (require 'core-keybindings)
  ;; for convenience and user support
  (unless (fboundp 'tool-bar-mode)
    (spacemacs-buffer/message (concat "No graphical support detected, "
                                      "you won't be able to launch a "
                                      "graphical instance of Emacs"
                                      "with this build.")))
  ;; check for new version
  (if dotspacemacs-mode-line-unicode-symbols
      (setq-default spacemacs-version-check-lighter "[⇪]"))
  ;; load environment variables
  (if (fboundp 'dotspacemacs/user-env)
      (dotspacemacs/call-user-env)
    (spacemacs/load-spacemacs-env))
  ;; install the dotfile if required
  (dotspacemacs/maybe-install-dotfile))

(defun spacemacs//setup-ido-vertical-mode ()
  "Setup `ido-vertical-mode' for the setup wizard.

Does nothing until `ido' is loaded, since most Spacemacs invocations
don't actually need ido (it is only used for the dotfile setup wizard
`dotspacemacs/maybe-install-dotfile')."
  (with-eval-after-load 'ido
    (require 'ido-vertical-mode)
    (ido-vertical-mode t)
    (add-hook
     'ido-setup-hook
     ;; think about hacking directly `ido-vertical-mode' source in libs instead.
     (defun spacemacs//ido-vertical-natural-navigation ()
       ;; more natural navigation keys: up, down to change current item
       ;; left to go up dir
       ;; right to open the selected item
       (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
       (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
       (define-key ido-completion-map (kbd "<left>") 'ido-delete-backward-updir)
       (define-key ido-completion-map (kbd "<right>") 'ido-exit-minibuffer)))))

(defun display-startup-echo-area-message ()
  "Change the default welcome message of minibuffer to another one."
  (message "Spacemacs is ready."))

(defun spacemacs/defer-until-after-user-config (func)
  "Call FUNC if dotspacemacs/user-config has been called. Otherwise,
defer call using `spacemacs-post-user-config-hook'."
  (if spacemacs-post-user-config-hook-run
      (funcall func)
    (add-hook 'spacemacs-post-user-config-hook func)))

(defun spacemacs//byte-compile-cleanup ()
  "Remove byte-compiled versions of `spacemacs-compiled-files'."
  (let ((default-directory spacemacs-start-directory))
    (spacemacs//remove-byte-compiled-files
     spacemacs-compiled-files)))

(defun spacemacs/setup-startup-hook ()
  "Add post init processing."
  (add-hook
   'emacs-startup-hook
   (defun spacemacs/startup-hook ()
     ;; This is set here so that emacsclient will show the startup buffer (and
     ;; so that it can be changed in user-config if necessary). It was set to
     ;; nil earlier in the startup process to properly handle command line
     ;; arguments.
     (setq initial-buffer-choice (lambda () (get-buffer spacemacs-buffer-name)))

     ;; Activate winner-mode for non dumped emacs sessions. Do this prior to
     ;; user-config to allow users to disable the feature and patch ediff
     ;; themselves. See issue 12582 for details.
     (winner-mode t)

     ;; Ultimate configuration decisions are given to the user who can defined
     ;; them in his/her ~/.spacemacs file
     (dotspacemacs|call-func dotspacemacs/user-config
                             "Calling dotfile user config...")
     (dotspacemacs|call-func dotspacemacs/emacs-custom-settings
                             "Calling dotfile Emacs custom settings...")
     ;; don't write custom settings into the dotfile before loading them,
     ;; otherwise https://github.com/syl20bnr/spacemacs/issues/10504 happens
     (spacemacs/initialize-custom-file-sync)
     (run-hooks 'spacemacs-post-user-config-hook)
     (setq spacemacs-post-user-config-hook-run t)
     (when (fboundp dotspacemacs-scratch-mode)
       (when (get-buffer "*scratch*")
         (with-current-buffer "*scratch*"
           (funcall dotspacemacs-scratch-mode)
           (run-hooks 'spacemacs-scratch-mode-hook))))
     (when spacemacs--delayed-user-theme
       (spacemacs/load-theme spacemacs--delayed-user-theme
                             spacemacs--fallback-theme t))
     (configuration-layer/display-summary)
     (spacemacs/check-for-new-version nil spacemacs-version-check-interval)
     (spacemacs-buffer/goto-link-line)
     (setq spacemacs-initialized t)
     (setq read-process-output-max dotspacemacs-read-process-output-max)
     ;; Redraw the spacemacs buffer with full org support
     ;; Before it must be drawn without org related features to
     ;; avoid loading build in org in emacs >= 29
     (spacemacs-buffer//startup-hook)
     ;; change gc settings at the last for it will take effect immediately
     (setq gc-cons-threshold (car dotspacemacs-gc-cons)
           gc-cons-percentage (cadr dotspacemacs-gc-cons))))

  (if dotspacemacs-byte-compile
      (when (> 1 (spacemacs//dir-byte-compile-state
                  (concat spacemacs-core-directory "libs/")))
        (byte-recompile-directory (concat spacemacs-core-directory "libs/") 0))
    (spacemacs//remove-byte-compiled-files-in-dir spacemacs-core-directory))

  ;; Check if revision has changed.
  (spacemacs//revision-check))

(provide 'core-spacemacs)
