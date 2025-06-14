;;; packages.el --- Treemacs Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Alexander Miller <alexanderm@web.de>
;;         Hong Xu <hong@topbug.net>
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


(defconst treemacs-packages
  '(
    golden-ratio
    treemacs
    (treemacs-evil :toggle (memq dotspacemacs-editing-style '(vim hybrid)))
    (treemacs-icons-dired :toggle treemacs-use-icons-dired)
    (treemacs-all-the-icons :toggle treemacs-use-all-the-icons-theme)
    (treemacs-magit :requires magit)
    (treemacs-persp :requires persp-mode)
    treemacs-projectile
    winum
    ))

(defun treemacs/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config
    (add-to-list 'golden-ratio-exclude-buffer-regexp
                 (rx "*Treemacs" (0+ any)))))

(defun treemacs/init-treemacs ()
  (use-package treemacs
    :commands (treemacs-select-window
               treemacs-select-scope-type
               treemacs--window-number-ten
               treemacs-current-visibility)
    :defer t
    :init
    (setq treemacs-follow-after-init t)
    (add-hook 'treemacs-mode-hook
              #'spacemacs/treemacs-setup-width-lock)
    (spacemacs|spacebind
     "Files manipulation."
     :global
     (("f" "Files"
       ("t" treemacs "File tree")
       ("B" treemacs-bookmark "Find bookmark in file tree")
       ("T" treemacs-find-file "Focus current file in file tree")
       ("M-t" treemacs-find-tag "Focus tag in file tree" ))
      ("p" "Project"
       ("t" spacemacs/treemacs-project-toggle "Open project in file tree"))))
    (which-key-add-major-mode-key-based-replacements 'treemacs-mode
      "c"         "treemacs-create"
      "o"         "treemacs-visit-node"
      "oa"        "treemacs-visit-node-ace"
      "t"         "treemacs-toggles"
      "y"         "treemacs-copy"
      "C-c C-p"   "treemacs-projects"
      "C-c C-p c" "treemacs-projects-collapse")
    :config
    (spacemacs/define-evil-state-face "treemacs" "MediumPurple1")
    ;; minor modes are enabled by default, so they must be explicitly
    ;; turned off
    (if (eq treemacs-use-follow-mode t)
        (treemacs-follow-mode t)
      (treemacs-follow-mode -1))
    (if (eq treemacs-use-follow-mode 'tag)
        (treemacs-tag-follow-mode t)
      (treemacs-tag-follow-mode -1))
    (if treemacs-use-filewatch-mode
        (treemacs-filewatch-mode t)
      (treemacs-filewatch-mode -1))
    (if (memq treemacs-use-git-mode '(simple extended deferred))
        (treemacs-git-mode treemacs-use-git-mode)
      (treemacs-git-mode -1))
    (add-to-list 'spacemacs-window-split-ignore-prefixes
                 treemacs--buffer-name-prefix)))

(defun treemacs/init-treemacs-evil ()
  (use-package treemacs-evil
    :after treemacs))

(defun treemacs/init-treemacs-projectile ()
  (use-package treemacs-projectile
    :after treemacs
    :defer t
    :init (require 'treemacs-projectile)))

(defun treemacs/init-treemacs-persp ()
  (use-package treemacs-persp
    :after treemacs persp-mode
    :config (when (eq treemacs-use-scope-type 'Perspectives)
              (treemacs-set-scope-type 'Perspectives))))

(defun treemacs/init-treemacs-icons-dired ()
  (use-package treemacs-icons-dired
    :hook (dired-mode . treemacs-icons-dired-mode)))

(defun treemacs/init-treemacs-all-the-icons ()
  (use-package treemacs-all-the-icons
    :hook ((treemacs-mode dired-mode) . (lambda () (treemacs-load-theme 'all-the-icons)))))

(defun treemacs/pre-init-winum ()
  (spacemacs|use-package-add-hook winum
    :post-config
    (progn
      (when (configuration-layer/package-used-p 'winum)
        ;; `0', `M-0' and `C-x w 0' are bound to `winum-select-window-0-or-10'
        (define-key winum-keymap
                    [remap winum-select-window-0-or-10] #'treemacs-select-window)
        ;; replace the which-key name
        (push '((nil . "winum-select-window-0-or-10") .
                (nil . "treemacs-select-window"))
              which-key-replacement-alist)
        (with-eval-after-load 'treemacs
          (dolist (n (number-sequence 1 5))
            (add-to-list 'winum-ignored-buffers
                         (format "%sFramebuffer-%s*"
                                 treemacs--buffer-name-prefix n))))))))

(defun treemacs/init-treemacs-magit ()
  (use-package treemacs-magit
    :after treemacs magit
    :defer t))
