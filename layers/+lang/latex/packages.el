;;; packages.el --- Latex Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
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


(defconst latex-packages
  '(
    auctex
    (auctex-latexmk :location (recipe :fetcher github :repo "smile13241324/auctex-latexmk")
                    :toggle (string= "LatexMk" latex-build-command))
    company
    math-symbol-lists
    (company-math :requires company math-symbol-lists)
    (company-auctex :requires company)
    (company-reftex :requires company)
    evil-matchit
    evil-tex
    flycheck
    flyspell
    ggtags
    (lsp-latex :requires lsp-mode)
    (magic-latex-buffer :toggle latex-enable-magic)
    smartparens
    (reftex :location built-in)
    typo
    which-key
    yasnippet))

(defun latex/post-init-company ()
  (spacemacs//latex-setup-company))

(defun latex/init-auctex ()
  (use-package tex
    :defer t
    :init
    (setq TeX-command-default latex-build-command
          TeX-engine latex-build-engine
          TeX-auto-save t
          TeX-parse-self t
          TeX-syntactic-comment t
          ;; Synctex support
          TeX-source-correlate-start-server nil
          ;; Don't insert line-break at inline math
          LaTeX-fill-break-at-separators nil)
    (when latex-enable-auto-fill
      (add-hook 'LaTeX-mode-hook 'latex/auto-fill-mode))
    (when latex-enable-folding
      (add-hook 'LaTeX-mode-hook 'TeX-fold-mode))
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
    (add-hook 'LaTeX-mode-hook #'spacemacs//latex-setup-backend)
    (when latex-refresh-preview
      (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))
    :config
    ;; otherwise `, p` preview commands doesn't work
    (require 'preview)
    ;; when `latex-view-with-pdf-tools' is non-nil, configure pdf-tools for

    ;; viewing output pdf's
    (spacemacs//latex-setup-pdf-tools)

    ;; Key bindings for plain TeX
    (dolist (mode '(tex-mode latex-mode context-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "\\"  'TeX-insert-macro                            ;; C-c C-m
        "-"   'TeX-recenter-output-buffer                  ;; C-c C-l
        "%"   'TeX-comment-or-uncomment-paragraph          ;; C-c %
        ";"   'comment-or-uncomment-region                 ;; C-c ; or C-c :
        ;; TeX-command-run-all runs compile and open the viewer
        "k"   'TeX-kill-job                                ;; C-c C-k
        "l"   'TeX-recenter-output-buffer                  ;; C-c C-l
        "m"   'TeX-insert-macro                            ;; C-c C-m
        "n"   'TeX-next-error                              ;; C-c `
        "N"   'TeX-previous-error                          ;; M-g p
        "v"   'TeX-view                                    ;; C-c C-v
        ;; TeX-doc is a very slow function
        "hd"  'TeX-doc
        "xb"  'latex/font-bold
        "xc"  'latex/font-code
        "xe"  'latex/font-emphasis
        "xi"  'latex/font-italic
        "xr"  'latex/font-clear
        "xo"  'latex/font-oblique
        "xfc" 'latex/font-small-caps
        "xff" 'latex/font-sans-serif
        "xfr" 'latex/font-serif)
      (spacemacs/declare-prefix-for-mode mode "mxf" "fonts")
      (unless (and (eq latex-backend 'lsp)
                   (eq mode 'latex-mode))
        (spacemacs/declare-prefix-for-mode mode "mh" "help")
        (spacemacs/declare-prefix-for-mode mode "mx" "text/fonts")
        (spacemacs/set-leader-keys-for-major-mode mode
          "a"   'TeX-command-run-all                         ;; C-c C-a
          "b"   'latex/build))
      (when dotspacemacs-major-mode-emacs-leader-key
        (spacemacs/set-leader-keys-for-major-mode mode
          dotspacemacs-major-mode-emacs-leader-key 'TeX-command-master))
      (when dotspacemacs-major-mode-leader-key
        (spacemacs/set-leader-keys-for-major-mode mode
          dotspacemacs-major-mode-leader-key 'TeX-command-master))
      (when latex-enable-folding
        (spacemacs/set-leader-keys-for-major-mode mode
          ;; the following commands are mostly not autoloaded, but that's fine
          ;; because `TeX-fold-mode' is added to `LaTeX-mode-hook'
          "z=" 'TeX-fold-math
          "zb" 'TeX-fold-buffer
          "zB" 'TeX-fold-clearout-buffer
          "ze" 'TeX-fold-env
          "zI" 'TeX-fold-clearout-item
          "zm" 'TeX-fold-macro
          "zp" 'TeX-fold-paragraph
          "zP" 'TeX-fold-clearout-paragraph
          "zr" 'TeX-fold-region
          "zR" 'TeX-fold-clearout-region
          "zz" 'TeX-fold-dwim)
        (spacemacs/declare-prefix-for-mode mode "mz" "fold")))

    ;; Key bindings specific to LaTeX
    (spacemacs/set-leader-keys-for-major-mode 'latex-mode
      "*"   'LaTeX-mark-section      ;; C-c *
      "."   'LaTeX-mark-environment  ;; C-c .
      "ii"   'LaTeX-insert-item       ;; C-c C-j
      "s"   'LaTeX-section           ;; C-c C-s
      "fe"  'LaTeX-fill-environment  ;; C-c C-q C-e
      "fp"  'LaTeX-fill-paragraph    ;; C-c C-q C-p
      "fr"  'LaTeX-fill-region       ;; C-c C-q C-r
      "fs"  'LaTeX-fill-section      ;; C-c C-q C-s
      "pb"  'preview-buffer
      "pB"  'preview-clearout-buffer
      "pc"  'preview-clearout
      "pd"  'preview-document
      "pD"  'preview-clearout-document
      "pe"  'preview-environment
      "pf"  'preview-cache-preamble
      "pp"  'preview-at-point
      "pP"  'preview-clearout-at-point
      "pr"  'preview-region
      "pR"  'preview-clearout
      "ps"  'preview-section
      "pS"  'preview-clearout-section
      "xB"  'latex/font-medium
      "xr"  'latex/font-clear
      "xfa" 'latex/font-calligraphic
      "xfn" 'latex/font-normal
      "xfu" 'latex/font-upright)

    ;; Rebind latex keys to avoid conflicts with lsp mode
    (if (eq latex-backend 'lsp)
        (spacemacs/set-leader-keys-for-major-mode 'latex-mode
          "au"   'TeX-command-run-all
          "c"    'latex/build
          "iC"   'org-ref-insert-cite-key
          "ic"   'LaTeX-close-environment ;; C-c ]
          "ie"   'LaTeX-environment)       ;; C-c C-e
      (spacemacs/set-leader-keys-for-major-mode 'latex-mode
        "c"   'LaTeX-close-environment ;; C-c ]
        "e"   'LaTeX-environment))       ;; C-c C-e

    ;; Declare prefixes
    (spacemacs/declare-prefix-for-mode 'latex-mode "mi" "insert")
    (spacemacs/declare-prefix-for-mode 'latex-mode "mp" "preview")
    (spacemacs/declare-prefix-for-mode 'latex-mode "mf" "fill")))

(defun latex/pre-init-auctex-latexmk ()
  (spacemacs|use-package-add-hook tex
    :post-config
    (auctex-latexmk-setup)))

(defun latex/init-auctex-latexmk ()
  (use-package auctex-latexmk
    :defer t
    :init (setq auctex-latexmk-inherit-TeX-PDF-mode t)))

(defun latex/post-init-evil-matchit ()
  (add-hook 'LaTeX-mode-hook 'evil-matchit-mode))

(defun latex/post-init-flycheck ()
  (spacemacs/enable-flycheck 'latex-mode))

(defun latex/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'LaTeX-mode-hook))

(defun latex/init-evil-tex ()
  (use-package evil-tex
    :defer t
    :init
    (add-hook 'LaTeX-mode-hook #'evil-tex-mode)
    (setq evil-tex-toggle-override-m nil)
    (setq evil-tex-toggle-override-t nil)
    :config
    (spacemacs/declare-prefix-for-mode 'latex-mode "mq"
      (cons "evil-tex toggles" evil-tex-toggle-map))))


(defun latex/init-reftex ()
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t)
  (let ((prefix (if (eq latex-backend 'lsp) "R" "r")))
    (spacemacs/declare-prefix-for-mode 'latex-mode (concat "m" prefix) "reftex")
    (spacemacs/set-leader-keys-for-major-mode 'latex-mode
      (concat prefix "c")    'reftex-citation
      (concat prefix "g")    'reftex-grep-document
      (concat prefix "i")    'reftex-index-selection-or-word
      (concat prefix "I")    'reftex-display-index
      (concat prefix " " "TAB") 'reftex-index
      (concat prefix "l")    'reftex-label
      (concat prefix "p")    'reftex-index-phrase-selection-or-word
      (concat prefix "P")    'reftex-index-visit-phrases-buffer
      (concat prefix "r")    'reftex-reference
      (concat prefix "s")    'reftex-search-document
      (concat prefix "t")    'reftex-toc
      (concat prefix "T")    'reftex-toc-recenter
      (concat prefix "v")    'reftex-view-crossref)))

(defun latex/post-init-ggtags ()
  (add-hook 'latex-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun latex/post-init-smartparens ()
  (add-hook 'LaTeX-mode-hook #'spacemacs//activate-smartparens))

(defun latex/post-init-typo ()
  ;; Typo mode isn't useful for LaTeX.
  (defun spacemacs//disable-typo-mode ()
    (typo-mode -1))
  (add-hook 'LaTeX-mode-hook 'spacemacs//disable-typo-mode))

(defun latex/post-init-yasnippet ()
  (add-hook 'LaTeX-mode-hook 'spacemacs/load-yasnippet))

(defun latex/post-init-which-key ()
  (push '((nil . "\\`latex/font-\\(.+\\)\\'") . (nil . "\\1"))
        which-key-replacement-alist))

(defun latex/init-magic-latex-buffer ()
  (use-package magic-latex-buffer
    :defer t
    :spacediminish (magic-latex-buffer " ✦" " mL")
    :init
    (add-hook 'TeX-update-style-hook 'magic-latex-buffer)
    (setq magic-latex-enable-block-highlight t
          magic-latex-enable-suscript t
          magic-latex-enable-pretty-symbols t
          magic-latex-enable-block-align nil
          magic-latex-enable-inline-image nil)))

(defun latex/init-lsp-latex ()
  (use-package lsp-latex
    :defer t))

(defun latex/init-math-symbol-lists ()
  (use-package math-symbol-lists
    :defer t))

(defun latex/init-company-math ()
  (use-package company-math
    :defer t))

(defun latex/init-company-auctex ()
  (use-package company-auctex
    :defer t))

(defun latex/init-company-reftex ()
  (use-package company-reftex
    :defer t))
