;;; packages.el --- Helm Layer packages File  -*- lexical-binding: nil; -*-
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


(defconst helm-packages
  '((avy-jump-helm-line
     :location (recipe :fetcher github :repo "sunlin7/avy-jump-helm-line"))
    auto-highlight-symbol
    bookmark
    helm
    (helm-ag :location (recipe
                        :fetcher github
                        :repo "smile13241324/helm-ag"))
    helm-comint
    helm-descbinds
    (helm-ls-git :toggle (configuration-layer/layer-used-p 'git))
    helm-make
    helm-mode-manager
    helm-org
    (helm-posframe :toggle helm-use-posframe)
    helm-projectile
    ;; FIXME Remove obsolete packages helm-swoop, helm-themes,
    ;; helm-ag, helm-git-grep, etc. (see https://github.com/melpa/melpa/pull/9520)
    (helm-swoop :location (recipe
                           :fetcher github
                           :repo "emacsattic/helm-swoop"))
    (helm-themes :location (recipe
                           :fetcher github
                           :repo "emacsattic/helm-themes"))
    (helm-spacemacs-help :location local)
    helm-xref
    ido
    imenu
    persp-mode
    popwin
    projectile))


;; Initialization of packages
(defun helm/init-avy-jump-helm-line ()
  (use-package avy-jump-helm-line
    :defer t
    :init
    (with-eval-after-load 'helm
      (define-key helm-map (kbd "C-q") 'avy-jump-helm-line))))

(defun helm/pre-init-auto-highlight-symbol ()
  (spacemacs|use-package-add-hook auto-highlight-symbol
    :post-init
    ;; add some functions to ahs transient states
    (setq spacemacs--symbol-highlight-transient-state-doc
          (concat
           spacemacs--symbol-highlight-transient-state-doc
           "  Search: [_s_] swoop  [_b_] buffers  [_f_] files  [_/_] project"))
    (spacemacs/transient-state-register-add-bindings 'symbol-highlight
      '(("s" spacemacs/helm-swoop-region-or-symbol :exit t)
        ("b" spacemacs/helm-buffers-smart-do-search-region-or-symbol :exit t)
        ("f" spacemacs/helm-files-smart-do-search-region-or-symbol :exit t)
        ("/" spacemacs/helm-project-smart-do-search-region-or-symbol :exit t)))))

(defun helm/post-init-bookmark ()
  (spacemacs/set-leader-keys "fb" 'helm-filtered-bookmarks))

(defun helm/init-helm ()
  (use-package helm
    :defer t
    :init
    (spacemacs|diminish helm-ff-cache-mode)
    (spacemacs|add-transient-hook completing-read
      (lambda (&rest _args) (require 'helm))
      lazy-load-helm-for-completing-read)
    (spacemacs|add-transient-hook completion-at-point
      (lambda (&rest _args) (require 'helm))
      lazy-load-helm-for-completion-at-point)
    (spacemacs|add-transient-hook read-file-name
      (lambda (&rest _args) (require 'helm))
      lazy-load-helm-for-read-file-name)
    (add-hook 'helm-cleanup-hook #'spacemacs//helm-cleanup)
    ;; key bindings
    ;; Use helm to provide :ls, unless ibuffer is used
    (unless (configuration-layer/package-used-p 'ibuffer)
      (evil-ex-define-cmd "buffers" 'helm-buffers-list))
    ;; use helm by default for M-x, C-x C-f, and C-x b
    (unless (configuration-layer/layer-usedp 'amx)
      (global-set-key (kbd "M-x") 'spacemacs/helm-M-x-fuzzy-matching))
    (global-set-key (kbd "C-x C-f") 'spacemacs/helm-find-files)
    (global-set-key (kbd "C-x b") 'helm-buffers-list)
    ;; use helm to switch last(/previous) visited buffers with C(-S)-tab
    (evil-global-set-key 'motion (kbd "C-<tab>") 'helm-buffers-list)
    (evil-global-set-key 'motion (kbd "C-<iso-lefttab>") 'helm-buffers-list)
    ;; use helm everywhere
    (spacemacs||set-helm-key "<f1>" helm-apropos)
    (spacemacs||set-helm-key "a'"   helm-available-repls)
    (spacemacs||set-helm-key "bb"   helm-mini)
    (spacemacs||set-helm-key "bU"   spacemacs/helm-buffers-list-unfiltered)
    (spacemacs||set-helm-key "Cl"   helm-colors)
    (spacemacs||set-helm-key "ff"   spacemacs/helm-find-files)
    (spacemacs||set-helm-key "fF"   helm-find-files)
    (spacemacs||set-helm-key "fL"   helm-locate)
    (spacemacs||set-helm-key "fr"   helm-recentf)
    (spacemacs||set-helm-key "hda"  helm-apropos)
    (spacemacs||set-helm-key "hdF"  spacemacs/helm-faces)
    (spacemacs||set-helm-key "hi"   helm-info-at-point)
    (spacemacs||set-helm-key "hm"   helm-man-woman)
    (spacemacs||set-helm-key "iu"   helm-ucs)
    (spacemacs||set-helm-key "jI"   helm-imenu-in-all-buffers)
    (spacemacs||set-helm-key "rm"   helm-all-mark-rings)
    (spacemacs||set-helm-key "rl"   helm-resume)
    (spacemacs||set-helm-key "rr"   helm-register)
    (spacemacs||set-helm-key "rs"   spacemacs/resume-last-search-buffer)
    (spacemacs||set-helm-key "ry"   helm-show-kill-ring)
    (spacemacs||set-helm-key "sl"   spacemacs/resume-last-search-buffer)
    (spacemacs||set-helm-key "sj"   spacemacs/helm-jump-in-buffer)
    (evil-add-command-properties 'spacemacs/helm-jump-in-buffer :jump t)
    (evil-add-command-properties 'lazy-helm/spacemacs/helm-jump-in-buffer :jump t)
    ;; search with grep
    (spacemacs||set-helm-key "sgb"  spacemacs/helm-buffers-do-grep)
    (spacemacs||set-helm-key
     "sgB" ("grep-search buffers w/ input" .
            spacemacs/helm-buffers-do-grep-region-or-symbol))
    (spacemacs||set-helm-key "sgf"  spacemacs/helm-files-do-grep)
    (spacemacs||set-helm-key
     "sgF" ("grep-search files w/ input" .
            spacemacs/helm-files-do-grep-region-or-symbol))
    (spacemacs||set-helm-key "sgg"  spacemacs/helm-file-do-grep)
    (spacemacs||set-helm-key
     "sgG" ("grep-search file w/ input" .
            spacemacs/helm-file-do-grep-region-or-symbol))
    ;; various key bindings
    (spacemacs||set-helm-key "fel" helm-locate-library)
    (spacemacs||set-helm-key "hdx" spacemacs/describe-ex-command)
    (spacemacs||set-helm-key "swg" helm-google-suggest)
    (with-eval-after-load 'helm-files
      (define-key helm-find-files-map
                  (kbd "C-c C-e") 'spacemacs/helm-find-files-edit)
      (define-key helm-find-files-map
                  (kbd "S-<return>") 'helm-ff-run-switch-other-window)
      (defun spacemacs//add-action-helm-find-files-edit ()
        (helm-add-action-to-source
         "Edit files in dired `C-c C-e'" 'spacemacs//helm-find-files-edit
         helm-source-find-files))
      (add-hook 'helm-find-files-before-init-hook
                'spacemacs//add-action-helm-find-files-edit))
    (with-eval-after-load 'helm-buffers
      (define-key helm-buffer-map
                  (kbd "S-<return>") 'helm-buffer-switch-other-window))
    ;; Add minibuffer history with `helm-minibuffer-history'
    (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
    ;; Delay this key bindings to override the defaults
    (add-hook 'emacs-startup-hook
              (lambda ()
                (spacemacs||set-helm-key "hdb" describe-bindings)
                (spacemacs||set-helm-key "hdc" describe-char)
                (spacemacs||set-helm-key "hdf" describe-function)
                (spacemacs||set-helm-key "hdk" describe-key)
                (spacemacs||set-helm-key "hdl" spacemacs/describe-last-keys)
                (spacemacs||set-helm-key "hdp" describe-package)
                (spacemacs||set-helm-key "hdP" configuration-layer/describe-package)
                (spacemacs||set-helm-key "hds" spacemacs/describe-system-info)
                (spacemacs||set-helm-key "hdt" describe-theme)
                (spacemacs||set-helm-key "hdv" describe-variable)
                (spacemacs||set-helm-key "hI"  spacemacs/report-issue)
                (spacemacs||set-helm-key "hn"  view-emacs-news)
                (spacemacs||set-helm-key "hPs" profiler-start)
                (spacemacs||set-helm-key "hPk" profiler-stop)
                (spacemacs||set-helm-key "hPr" profiler-report)
                (spacemacs||set-helm-key "hPw" profiler-report-write-profile)
                ;; define the key binding at the very end in order to allow the user
                ;; to overwrite any key binding
                (unless (configuration-layer/layer-usedp 'amx)
                  (spacemacs/set-leader-keys
                    dotspacemacs-emacs-command-key 'spacemacs/helm-M-x-fuzzy-matching))))
    ;; avoid duplicates in `helm-M-x' history.
    (setq history-delete-duplicates t)
    :config
    (helm-mode)
    (spacemacs|hide-lighter helm-mode)
    (advice-add 'helm-grep-save-results-1 :after 'spacemacs//gne-init-helm-grep)
    ;; helm-locate uses es (from everything on windows which doesn't like fuzzy)
    (helm-locate-set-command)
    (setq helm-locate-fuzzy-match (and (bound-and-true-p helm-use-fuzzy)
                                       (string-match "locate" helm-locate-command)
                                       t))
    (setq helm-boring-buffer-regexp-list
          (append helm-boring-buffer-regexp-list
                  spacemacs-useless-buffers-regexp))
    (setq helm-white-buffer-regexp-list
          (append helm-white-buffer-regexp-list
                  spacemacs-useful-buffers-regexp))

    ;; allow to leave helm result groups with evil bindings
    (setq helm-move-to-line-cycle-in-source nil)
    ;; allow find file on non-exists file at point
    (setq helm-ff-allow-non-existing-file-at-point t)

    ;; use helm to switch last(/previous) visited buffers with C(-S)-tab
    (define-key helm-map (kbd "C-<tab>") 'helm-follow-action-forward)
    (define-key helm-map (kbd "C-<iso-lefttab>") 'helm-follow-action-backward)
    ;; alter helm-bookmark key bindings to be simpler
    (defun simpler-helm-bookmark-keybindings ()
      (define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
      (define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)
      (define-key helm-bookmark-map
                  (kbd "C-f") 'helm-bookmark-toggle-filename)
      (define-key helm-bookmark-map
                  (kbd "S-<return>") 'helm-bookmark-run-jump-other-window)
      (define-key helm-bookmark-map (kbd "C-/") 'helm-bookmark-help))
    (with-eval-after-load 'helm-bookmark
      (simpler-helm-bookmark-keybindings))
    (when (configuration-layer/package-used-p 'winum)
      (define-key helm-buffer-map
                  (kbd "RET") 'spacemacs/helm-find-buffers-windows)
      (define-key helm-generic-files-map
                  (kbd "RET") 'spacemacs/helm-find-files-windows)
      (define-key helm-find-files-map
                  (kbd "RET") 'spacemacs/helm-find-files-windows))))

(defun helm/init-helm-comint ()
  (use-package helm-comint
    :defer t
    :after helm))

(defun helm/init-helm-ag ()
  (use-package helm-ag
    :defer t
    :init
    (setq helm-ag-use-grep-ignore-list t)
    ;; This overrides the default C-s action in helm-projectile-switch-project
    ;; to search using rg/ag/pt/whatever instead of just grep
    (with-eval-after-load 'helm-projectile
      (define-key helm-projectile-projects-map
                  (kbd "C-s") 'spacemacs/helm-projectile-grep)
      ;; `spacemacs/helm-projectile-grep' calls:
      ;; `spacemacs/helm-project-smart-do-search-in-dir'
      ;; which needs to be an action.
      ;; Delete the current action.
      (helm-delete-action-from-source
       "Grep in projects `C-s'" helm-source-projectile-projects)
      (helm-add-action-to-source
       "Search in projects `C-s'"
       'spacemacs/helm-project-smart-do-search-in-dir
       helm-source-projectile-projects))

    (spacemacs/set-leader-keys
      ;; helm-ag marks
      "s`"  'helm-ag-pop-stack
      ;; opened buffers scope
      "sb"  'spacemacs/helm-buffers-smart-do-search
      "sB"  '("smart-search buffers w/ input" .
              spacemacs/helm-buffers-smart-do-search-region-or-symbol)
      "sab" 'helm-do-ag-buffers
      "saB" '("ag-search buffers w/ input" .
              spacemacs/helm-buffers-do-ag-region-or-symbol)
      "skb" 'spacemacs/helm-buffers-do-ack
      "skB" '("ack-search buffers w/ input" .
              spacemacs/helm-buffers-do-ack-region-or-symbol)
      "srb" 'spacemacs/helm-buffers-do-rg
      "srB" '("rg-search buffers w/ input" .
              spacemacs/helm-buffers-do-rg-region-or-symbol)
      "stb" 'spacemacs/helm-buffers-do-pt
      "stB" '("pt-search buffers w/ input" .
              spacemacs/helm-buffers-do-pt-region-or-symbol)
      ;; current file scope
      "ss"  'spacemacs/helm-file-smart-do-search
      "sS"  'spacemacs/helm-file-smart-do-search-region-or-symbol
      "saa" 'helm-ag-this-file
      "saA" 'spacemacs/helm-file-do-ag-region-or-symbol
      ;; files scope
      "sf"  'spacemacs/helm-files-smart-do-search
      "sF"  '("smart-search files w/ input" .
              spacemacs/helm-files-smart-do-search-region-or-symbol)
      "saf" 'helm-do-ag
      "saF" '("ag-search files w/ input" .
              spacemacs/helm-files-do-ag-region-or-symbol)
      "skf" 'spacemacs/helm-files-do-ack
      "skF" '("ack-search files w/ input" .
              spacemacs/helm-files-do-ack-region-or-symbol)
      "srf" 'spacemacs/helm-files-do-rg
      "srF" '("rg-search files w/ input" .
              spacemacs/helm-files-do-rg-region-or-symbol)
      "stf" 'spacemacs/helm-files-do-pt
      "stF" '("pt-search files w/ input" .
              spacemacs/helm-files-do-pt-region-or-symbol)
      ;; current dir scope
      "sd"  'spacemacs/helm-dir-smart-do-search
      "sD"  '("smart-search dir w/ input" .
              spacemacs/helm-dir-smart-do-search-region-or-symbol)
      "sad" 'spacemacs/helm-dir-do-ag
      "saD" 'spacemacs/helm-dir-do-ag-region-or-symbol
      "skd" 'spacemacs/helm-dir-do-ack
      "skD" 'spacemacs/helm-dir-do-ack-region-or-symbol
      "srd" 'spacemacs/helm-dir-do-rg
      "srD" 'spacemacs/helm-dir-do-rg-region-or-symbol
      "std" 'spacemacs/helm-dir-do-pt
      "stD" 'spacemacs/helm-dir-do-pt-region-or-symbol
      ;; current project scope
      "/"   'spacemacs/helm-project-smart-do-search
      "*"   'spacemacs/helm-project-smart-do-search-region-or-symbol
      "sp"  'spacemacs/helm-project-smart-do-search
      "sP"  '("smart-search project w/ input" .
              spacemacs/helm-project-smart-do-search-region-or-symbol)
      "sap" 'spacemacs/helm-project-do-ag
      "saP" '("ag-search project w/ input" .
              spacemacs/helm-project-do-ag-region-or-symbol)
      "skp" 'spacemacs/helm-project-do-ack
      "skP" '("ack-search project w/ input" .
              spacemacs/helm-project-do-ack-region-or-symbol)
      "srp" 'spacemacs/helm-project-do-rg
      "srP" '("rg-search project w/ input" .
              spacemacs/helm-project-do-rg-region-or-symbol)
      "stp" 'spacemacs/helm-project-do-pt
      "stP" '("pt-search project w/ input" .
              spacemacs/helm-project-do-pt-region-or-symbol))
    :config
    (advice-add 'helm-ag--save-results :after 'spacemacs//gne-init-helm-ag)
    (evil-define-key 'normal helm-ag-map
      (kbd dotspacemacs-leader-key) spacemacs-default-map)
    (evilified-state-evilify-map helm-grep-mode-map
      :mode helm-grep-mode
      :bindings
      (kbd "q") 'quit-window)
    (evilified-state-evilify-map helm-ag-mode-map
      :mode helm-ag-mode
      :bindings
      (kbd "gr") 'helm-ag--update-save-results
      (kbd "q") 'quit-window)))

(defun helm/init-helm-descbinds ()
  (use-package helm-descbinds
    :defer t
    :init
    (setq helm-descbinds-window-style 'split
          helm-descbinds-disable-which-key nil)
    (add-hook 'helm-mode-hook 'helm-descbinds-mode)
    (spacemacs/set-leader-keys "?" 'helm-descbinds)))

(defun helm/init-helm-ls-git ()
  (use-package helm-ls-git
    :defer t
    :init
    (spacemacs/set-leader-keys "gff" 'helm-ls-git)
    (when (configuration-layer/package-usedp 'magit)
      ;; Do not use helm-ls-git-rebase-todo-mode for git-rebase-todo,
      ;; instead let it be handled by magit
      (setq auto-mode-alist
            (delete '("/git-rebase-todo$" . helm-ls-git-rebase-todo-mode)
                    auto-mode-alist)))
    :config
    (add-hook 'helm-ls-git-commit-mode-hook 'display-fill-column-indicator-mode)
    (when (configuration-layer/package-usedp 'magit)
      ;; Undo the forced action of adding helm-ls-git-rebase-todo-mode to
      ;; auto-mode-alist by helm-ls-git.
      (setq auto-mode-alist
            (delete '("/git-rebase-todo$" . helm-ls-git-rebase-todo-mode)
                    auto-mode-alist))
      ;; Set `helm-ls-git-status-command' conditonally on `git' layer
      ;; If `git' is in use, use default `\'magit-status-setup-buffer'
      ;; Otherwise, use defaault `\'vc-dir'
      (setq helm-ls-git-status-command 'magit-status-setup-buffer))))

(defun helm/init-helm-make ()
  (use-package helm-make
    :defer t
    :init
    (spacemacs/set-leader-keys
      "cc" 'helm-make-projectile
      "cm" 'helm-make)))

(defun helm/init-helm-mode-manager ()
  (use-package helm-mode-manager
    :defer t
    :init
    (spacemacs/set-leader-keys
      "hM"    'helm-switch-major-mode
      ;; "hm"    'helm-disable-minor-mode
      "h C-m" 'helm-enable-minor-mode)))

(defun helm/init-helm-org ()
  (use-package helm-org
    :commands (helm-org-in-buffer-headings)
    :defer t))

(defun helm/init-helm-posframe ()
  (use-package helm-posframe
    :defer t
    :init
    (setq helm-posframe-poshandler 'posframe-poshandler-frame-center)
    (setq helm-posframe-width (round (* 0.618 (frame-width))))
    (setq helm-posframe-height (round (* 0.618 (frame-height))))
    (setq helm-posframe-parameters
          '((internal-border-width . 2)
            (left-fringe . 4)
            (right-fringe . 4)
            (undecorated . nil)))
    (helm-posframe-enable)))

(defun helm/pre-init-helm-projectile ()
  ;; overwrite projectile settings
  (spacemacs|use-package-add-hook projectile
    :post-init
    (progn
      (setq projectile-switch-project-action 'helm-projectile)
      (spacemacs/set-leader-keys
        "pb"  'helm-projectile-switch-to-buffer
        "pd"  'helm-projectile-find-dir
        "pf"  'helm-projectile-find-file
        "pF"  'helm-projectile-find-file-dwim
        "ph"  'helm-projectile
        "pp"  'helm-projectile-switch-project
        "pr"  'helm-projectile-recentf
        "sgp" 'helm-projectile-grep))))

(defun helm/init-helm-projectile ()
  (use-package helm-projectile
    :commands (helm-projectile-switch-to-buffer
               helm-projectile-find-dir
               helm-projectile-dired-find-dir
               helm-projectile-recentf
               helm-projectile-find-file
               helm-projectile-grep
               helm-projectile
               helm-projectile-switch-project)
    :init
    ;; needed for smart search if user's default tool is grep
    (defalias 'spacemacs/helm-project-do-grep 'helm-projectile-grep)
    (defalias
      'spacemacs/helm-project-do-grep-region-or-symbol
      'helm-projectile-grep)
    :config (when (configuration-layer/package-used-p 'winum)
              (define-key helm-projectile-find-file-map
                          (kbd "RET") 'spacemacs/helm-find-files-windows))))

(defun helm/init-helm-spacemacs-help ()
  (use-package helm-spacemacs-help
    :commands (helm-spacemacs-help-dotspacemacs
               helm-spacemacs-help
               helm-spacemacs-help-layers
               helm-spacemacs-help-packages
               helm-spacemacs-help-docs
               helm-spacemacs-help-toggles)
    :init
    (autoload 'helm-spacemacs-help-faq "helm-spacemacs-faq" nil t)
    (spacemacs/set-leader-keys
      "h ."   'helm-spacemacs-help-dotspacemacs
      "h SPC" 'helm-spacemacs-help
      "h f"   'helm-spacemacs-help-faq
      "h l"   'helm-spacemacs-help-layers
      "h p"   'helm-spacemacs-help-packages
      "h r"   'helm-spacemacs-help-docs
      "h t"   'helm-spacemacs-help-toggles)))

(defun helm/init-helm-swoop ()
  (use-package helm-swoop
    :defer t
    :init
    (setq helm-swoop-split-with-multiple-windows t
          helm-swoop-split-direction 'split-window-vertically
          helm-swoop-split-window-function 'spacemacs/helm-swoop-split-window-function)

    (defun spacemacs/helm-swoop-split-window-function (&rest args)
      "Override to make helm settings (like `helm-split-window-default-side') work"
      (let (;; current helm-swoop implemenatation prevents it from being used fullscreen
            (helm-full-frame nil)
            (pop-up-windows t))
        (apply 'helm-default-display-buffer args)))

    (defun spacemacs/helm-swoop-clear-cache ()
      "Call `helm-swoop--clear-cache' to clear the cache"
      (interactive)
      (helm-swoop--clear-cache)
      (message "helm-swoop cache cleaned."))

    (spacemacs/set-leader-keys
      "sC"    'spacemacs/helm-swoop-clear-cache
      "ss"    'helm-swoop
      "sS"    'helm-multi-swoop
      "s C-s" 'helm-multi-swoop-all)

    (evil-add-command-properties 'helm-swoop :jump t)))

(defun helm/init-helm-themes ()
  (use-package helm-themes
    :defer t
    :init
    (spacemacs/set-leader-keys
      "Ts" 'spacemacs/helm-themes)))

(defun helm/init-helm-xref ()
  (use-package helm-xref
    :commands (helm-xref-show-xrefs-27 helm-xref-show-xrefs)
    :init
    ;; This is required to make `xref-find-references' not give a prompt.
    ;; `xref-find-references' asks the identifier (which has no text property)
    ;; and then passes it to `lsp-mode', which requires the text property at
    ;; point to locate the references.
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29619
    (setq xref-prompt-for-identifier '(not xref-find-definitions
                                           xref-find-definitions-other-window
                                           xref-find-definitions-other-frame
                                           xref-find-references
                                           spacemacs/jump-to-definition))))

(defun helm/post-init-ido ()
  ;; helm is incompatible with ido, and spits an error when trying to enable
  ;; both modes (see https://github.com/syl20bnr/spacemacs/issues/11640 and
  ;; https://github.com/emacs-helm/helm/issues/2085)
  (when (bound-and-true-p ido-mode)
    (ido-mode -1)))

(defun helm/post-init-imenu ()
  (spacemacs/set-leader-keys "ji" 'spacemacs/helm-jump-in-buffer))

(defun helm/post-init-popwin ()
  ;; disable popwin-mode while Helm session is running
  (add-hook 'helm-after-initialize-hook #'spacemacs//helm-prepare-display)
  ;;  Restore popwin-mode after a Helm session finishes.
  (add-hook 'helm-cleanup-hook #'spacemacs//helm-restore-display))

(defun helm/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (setq
     spacemacs--persp-display-buffers-func 'spacemacs/persp-helm-mini
     spacemacs--persp-display-perspectives-func 'spacemacs/helm-perspectives)))

(defun helm/post-init-projectile ()
  (setq projectile-completion-system 'helm))
