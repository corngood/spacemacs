;;; packages.el --- Source Control Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
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

(defconst version-control-packages
  '(
    browse-at-remote
    ;; Git-gutter+ is not longer maintained and will break with latest magit version
    ;; therefore we switch to diff-hl for users which have configered git-gutter+ to avoid
    ;; breaking there config.
    ;; (diff-hl            :toggle (or (eq 'diff-hl version-control-diff-tool)
    ;;                                 (eq 'git-gutter+ version-control-diff-tool)))
    (diff-hl :location (recipe
                        :fetcher github
                        :repo "smile13241324/diff-hl"
                        :branch "frame-local-diff-hl-margin-mode")
             :toggle (or (eq 'diff-hl version-control-diff-tool)
                         (eq 'git-gutter+ version-control-diff-tool)))
    diff-mode
    evil-unimpaired
    (git-gutter         :toggle (eq 'git-gutter version-control-diff-tool))
    (git-gutter-fringe  :toggle (eq 'git-gutter version-control-diff-tool))
    (smerge-mode :location built-in)
    (vc :location built-in)))

(defun version-control/init-vc ()
  (use-package vc
    :defer t
    :commands (vc-ignore)
    :init
    (spacemacs/set-leader-keys "gv" vc-prefix-map)
    (spacemacs/declare-prefix "gv" "version-control")
    :config
    (define-key vc-prefix-map "e" #'vc-ediff)
    (define-key vc-prefix-map "R" #'vc-resolve-conflicts)
    (with-eval-after-load 'vc-dir
      (evilified-state-evilify-map vc-dir-mode-map
        :mode vc-dir-mode
        :bindings
        "j" 'vc-dir-next-line
        (kbd "M-n") 'vc-dir-next-line
        "k" 'vc-dir-previous-line
        (kbd "M-p") 'vc-dir-previous-line
        "gj" 'vc-dir-next-directory
        (kbd "<tab>") 'vc-dir-next-directory
        "gk" 'vc-dir-previous-directory
        (kbd "<backtab>") 'vc-dir-previous-directory
        "l" 'vc-print-log
        "c" 'vc-next-action
        "a" 'vc-annotate
        "r" 'vc-dir-refresh
        "E" 'vc-dir-ignore))

    (with-eval-after-load 'log-view
      (evilified-state-evilify-map log-view-mode-map
        :mode log-view-mode
        :bindings
        (kbd "M-n") 'log-view-msg-next
        (kbd "M-p") 'log-view-msg-prev
        (kbd "C-j") 'log-view-msg-next
        (kbd "C-k") 'log-view-msg-prev
        "J" 'log-view-file-next
        (kbd "<tab>") 'log-view-file-next
        "gj" 'log-view-file-next
        "K" 'log-view-file-prev
        "gk" 'log-view-file-prev
        (kbd "<backtab>") 'log-view-file-prev
        (kbd "<return>") 'log-view-find-revision
        "H" 'log-view-toggle-entry-display
        "o" 'ace-link-woman)
      (evilified-state-evilify-map vc-svn-log-view-mode-map
        :mode vc-svn-log-view-mode
        :eval-after-load vc-svn)
      (evilified-state-evilify-map vc-git-log-view-mode-map
        :mode vc-git-log-view-mode
        :eval-after-load vc-git)
      (evilified-state-evilify-map vc-hg-log-view-mode-map
        :mode vc-hg-log-view-mode
        :eval-after-load vc-hg))
    (with-eval-after-load 'vc-annotate
      (evilified-state-evilify-map vc-annotate-mode-map
        :mode vc-annotate-mode
        :bindings
        "J" 'vc-annotate-next-revision
        "K" 'vc-annotate-prev-revision
        "L" 'vc-annotate-show-log-revision-at-line
        "H" 'vc-annotate-toggle-annotation-visibility
        "a" 'vc-annotate-revision-at-line
        "p" 'vc-annotate-revision-previous-to-line))))

(defun version-control/init-diff-mode ()
  (use-package diff-mode
    :defer t
    :config
    (spacemacs/declare-prefix-for-mode 'diff-mode "mf" "format")
    (spacemacs/set-leader-keys-for-major-mode 'diff-mode
      "a" 'diff-apply-hunk
      "d" 'diff-hunk-kill
      "D" 'diff-file-kill
      "e" 'diff-ediff-patch
      "fc" 'diff-unified->context
      "fr" 'diff-reverse-direction
      "fu" 'diff-context->unified
      "g" 'diff-goto-source
      "j" 'diff-hunk-next
      "J" 'diff-file-next
      "k" 'diff-hunk-prev
      "K" 'diff-file-prev
      "r" 'spacemacs/diff-mode-revert-hunk
      "s" 'diff-split-hunk
      "u" 'diff-undo
      "q" 'quit-window)
    (spacemacs|define-transient-state diff-mode
      :title "Diff-mode Transient State"
      :evil-leader-for-mode (diff-mode . ".")
      :bindings
      ("j" diff-hunk-next "next hunk")
      ("J" diff-file-next "next file")
      ("k" diff-hunk-prev "previous hunk")
      ("K" diff-file-prev "previous file")
      ("q" nil "quit" :exit t)
      ("<escape>" nil nil :exit t))))

(defun version-control/init-diff-hl ()
  (use-package diff-hl
    :defer t
    :init
    (spacemacs/set-leader-keys "gv=" 'diff-hl-diff-goto-hunk)
    (setq diff-hl-side (if (eq version-control-diff-side 'left)
                           'left 'right))
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (define-advice turn-on-diff-hl-mode (:after (&rest _) AUTOMARGIN)
      (and (memq version-control-margin '(t auto))
           (not diff-hl-margin-mode)    ; not global mode
           (not (display-graphic-p (window-frame (get-buffer-window))))
           (diff-hl-margin-local-mode)
           (diff-hl-update)))
    (when (eq version-control-margin 'global)
        (run-with-idle-timer 1 nil 'spacemacs/vcs-enable-margin-globally))
    ;; The diff-hl-margin mode requests the diff-hl-mode to be enabled, so
    ;; enable the diff-hl-mode anyway.
    (run-with-idle-timer 1 nil 'global-diff-hl-mode)))

(defun version-control/post-init-evil-unimpaired ()
  (define-key evil-normal-state-map (kbd "[ h") 'spacemacs/vcs-previous-hunk)
  (define-key evil-normal-state-map (kbd "] h") 'spacemacs/vcs-next-hunk))

(defun version-control/init-git-gutter ()
  (use-package git-gutter
    :defer t
    :init
    ;; If you enable global minor mode
    (when version-control-margin
      (run-with-idle-timer 1 nil 'global-git-gutter-mode))
    (setq git-gutter:update-interval 2
          git-gutter:modified-sign " "
          git-gutter:added-sign "+"
          git-gutter:deleted-sign "-"
          git-gutter:diff-option "-w"
          git-gutter:hide-gutter t
          git-gutter:ask-p nil
          git-gutter:verbosity 0
          git-gutter:handled-backends '(git hg bzr svn)
          git-gutter:hide-gutter t)
    :config
    (spacemacs|hide-lighter git-gutter-mode)
    ;; Do not activate git-gutter in pdf-view-mode, see #15106
    (when (configuration-layer/layer-used-p 'pdf)
      (add-to-list 'git-gutter:disabled-modes 'pdf-view-mode))))

(defun version-control/init-git-gutter-fringe ()
  (use-package git-gutter-fringe
    :defer t
    :init
    (spacemacs|do-after-display-system-init
      (with-eval-after-load 'git-gutter
        (require 'git-gutter-fringe)))
    (setq git-gutter-fr:side (if (eq version-control-diff-side 'left)
                                 'left-fringe 'right-fringe))))

(defun version-control/init-smerge-mode ()
  (use-package smerge-mode
    :defer t
    :diminish smerge-mode
    :init
    (spacemacs/set-leader-keys
      "gr" 'spacemacs/smerge-transient-state/body)
    (spacemacs|transient-state-format-hint smerge
      spacemacs--smerge-ts-full-hint
      "\n
 Movement^^^^             Merge Action^^      Diff^^            Other
 -------------------^^^^  ----------------^^  --------------^^  -------------------------------^^
 [_n_]^^   next conflict  [_u_] keep upper    [_<_] base/upper  [_C_] combine curr/next conflicts
 [_N_/_p_] prev conflict  [_b_] keep base     [_=_] upper/lower [_s_] swap upper/lower
 [_j_]^^   next line      [_l_] keep lower    [_>_] base/lower  [_U_] undo
 [_k_]^^   prev line      [_a_] keep all      [_r_] refine      [_q_] quit
 ^^^^                     [_c_] keep current  [_e_] ediff       [_?_] toggle help
 ^^^^                     [_K_] kill current")
    (spacemacs|define-transient-state smerge
      :title "Smerge Transient State"
      :hint-is-doc t
      :dynamic-hint (spacemacs//smerge-ts-hint)
      :on-enter (require 'smerge-mode)
      :bindings
      ;; move
      ("n" smerge-vc-next-conflict)
      ("N" smerge-prev)
      ("p" smerge-prev)
      ("j" evil-next-line)
      ("k" evil-previous-line)
      ("<down>" evil-next-line)
      ("<up>" evil-previous-line)
      ("<next>" scroll-up-command)
      ("<prior>" scroll-down-command)
      ;; merge action
      ("a" smerge-keep-all)
      ("b" smerge-keep-base)
      ("l" smerge-keep-lower)
      ("u" smerge-keep-upper)
      ("c" smerge-keep-current)
      ("K" smerge-kill-current)
      ("s" smerge-swap)
      ;; diff
      ("<" smerge-diff-base-mine)
      ("=" smerge-diff-mine-other)
      (">" smerge-diff-base-other)
      ("r" smerge-refine)
      ("e" smerge-ediff :exit t)
      ;; other
      ("C" smerge-combine-with-next)
      ("U" evil-undo)
      ("q" nil :exit t)
      ("?" spacemacs//smerge-ts-toggle-hint))))

(defun version-control/init-browse-at-remote ()
  (use-package browse-at-remote
    :defer t
    :init (spacemacs/set-leader-keys "go" 'browse-at-remote)))
