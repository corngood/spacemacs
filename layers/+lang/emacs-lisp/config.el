;;; config.el --- Emacs Lisp Layer configuration File for Spacemacs  -*- lexical-binding: nil; -*-
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


;; Variables

(spacemacs|define-jump-handlers emacs-lisp-mode)
(spacemacs|define-jump-handlers lisp-interaction-mode)
(spacemacs|define-jump-handlers inferior-emacs-lisp-mode)

(defvar emacs-lisp-hide-namespace-prefix nil
  "If non-nil, hide namespace prefixes using nameless-mode.")

(defvar emacs-lisp-format-on-save t
  "If non-nil, format elisp buffers before saving.")
