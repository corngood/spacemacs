;;; config.el --- Language Server Protocol Layer config file for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Fangrui Song <i@maskray.me>
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


(defvar lsp-remap-xref-keybindings nil "When non-nil, xref keybindings remapped to lsp-ui-peek-find-*")
(defvar lsp-navigation 'both
  "If `simple' binds lightweight navigation functions under `SPC m g'.
If `peek' binds lsp-ui navigation functions under `SPC m g'.
If `both', binds lightweight navigation functions under `SPC m g' and lsp-ui functions under `SPC m G'")

;; These are config variables exposed by the lsp-ui package
;; They all have toggles bound under 't' in spacemacs/lsp-define-keys-for-mode
(defvar lsp-ui-doc-enable t "Enable/disable lsp-ui-doc overlay")
(defvar lsp-ui-doc-include-signature nil "When non-nil, type signature included in the lsp-ui-doc overlay")
(defvar lsp-ui-sideline-enable t "Enable/disable lsp-ui-sideline overlay")
(defvar lsp-ui-sideline-show-symbol nil "When non-nil, sideline includes symbol info (largely redundant for c modes)")  ; don't show symbol on the right of info
(defvar lsp-ui-sideline-ignore-duplicate t "Ignore duplicates")

(defvar lsp-use-lsp-ui t "When non-nil, use `lsp-ui' package.")

(defvar lsp-use-upstream-bindings nil "When non-nil, map keys to `lsp-command-map'.")

(defvar lsp-sonarlint nil "When non-nil, use `lsp-sonarlint' package.")

(defvar lsp-manage-backends-manually nil "When non-nil lsp-mode does not insert `company-capf' as the ultimate first item of `company-backends'.
`lsp-manage-backends-manually' can either be `:all' or a list of major-modes that should be managed manually.")
