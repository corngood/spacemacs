;;; funcs.el --- Spacemacs Editing Visual Layer functions File  -*- lexical-binding: nil; -*-
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

(defun spacemacs/centered-buffer-transient-state ()
  "Center buffer and enable centering transient state."
  (interactive)
  (spacemacs/toggle-centered-buffer-on)
  (spacemacs/centered-buffer-transient-state/body))

(defun spacemacs//maybe-enable-term-cursor ()
  "Enable `global-term-cursor-mode' when a first terminal frame is created."
  (unless (or (display-graphic-p)
              (bound-and-true-p global-term-cursor-mode))
    (global-term-cursor-mode)
    (remove-hook 'server-after-make-frame-hook 'spacemacs//maybe-enable-term-cursor)))
