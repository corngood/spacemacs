;;; packages.el --- speed-reading Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
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


(setq speed-reading-packages
      '(
        spray
        which-key
        ))

(defun speed-reading/init-spray ()
  (use-package spray
    :commands spray-mode
    :init
    (defun speed-reading/start-spray ()
      "Start spray speed reading on current buffer at current point."
      (interactive)
      (evil-insert-state)
      (spray-mode t)
      (internal-show-cursor (selected-window) nil))
    (spacemacs/set-leader-keys "ars" 'speed-reading/start-spray)

    (define-advice spray-quit (:after (&rest _) speed-reading//quit-spray)
      "Correctly quit spray."
      (internal-show-cursor (selected-window) t)
      (evil-normal-state))
    :config
    (define-key spray-mode-map (kbd "h") 'spray-backward-word)
    (define-key spray-mode-map (kbd "l") 'spray-forward-word)
    (define-key spray-mode-map (kbd "q") 'spray-quit)))

(defun speed-reading/post-init-which-key ()
  (push '((nil . "\\`speed-reading/\\(.+\\)\\'") . (nil . "\\1"))
        which-key-replacement-alist))
