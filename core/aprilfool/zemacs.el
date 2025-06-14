;;; zemacs.el --- Spacemacs 2016 April Fools File  -*- lexical-binding: nil; -*-
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


(setq spacemacs-buffer-name "*zemacs*")
(setq spacemacs-buffer-logo-title "[Z E M A C S]")
(setq spacemacs-buffer-version-info "af-1.01")

(define-minor-mode zemacs-buffer-mode
  "Zemacs major mode for startup screen."
  :lighter "ToTheMAX"
  (if zemacs-buffer-mode
      (progn
        (spacemacs/set-leader-keys-for-major-mode 'spacemacs-buffer-mode
          (kbd "aprilfool") 'next-next-NEXT-millennium)
        (zemacs//insert-links))
    ;; restore the true one
    (ad-disable-advice 'configuration-layer/initialize
                       'before 'zemacs/initialize)
    (ad-activate 'configuration-layer/initialize)
    (ad-disable-advice 'spacemacs-buffer//inject-version
                       'around 'zemacs/inject-version)
    (ad-activate 'spacemacs-buffer//inject-version)
    (ad-disable-advice 'spacemacs-buffer/insert-banner-and-buttons
                       'after 'zemacs/insert-banner-and-buttons)
    (ad-activate 'spacemacs-buffer/insert-banner-and-buttons)
    (load-file (concat spacemacs-start-directory "core/core-spacemacs-buffer.el"))
    (setq dotspacemacs-startup-banner 'official)
    (kill-buffer)
    (insert "
───█──█─█▀▀█─█▀▀█─█▀▀█─█──█───
───█▀▀█─█▄▄█─█▀▀▀─█▀▀▀─▀▀▀█───
───█──█─█──█─█────█────█▄▄█───
──────────────────────────────
───█▀▀█─█▀▀█─█▀▀█─▀▀█▀▀─█─────
───█▄▄█─█▀▀▀─█▀▀▄───█───█─────
───█──█─█────█──█─▄▄█▄▄─█▄▄───
──────────────────────────────
──█▀▀▀─▄▀▀▀▄─▄▀▀▀▄─█────▄▀▀▀──
──█▀▀▀─█───█─█───█─█────▀▀▀▄──
──█────▀▄▄▄▀─▀▄▄▄▀─█▄▄▄─▄▄▄▀──
──────────────────────────────
──────────▄▄███████▄▄─────────
───────▄███████████████▄──────
────▄██████▀────────▀█████▄───
──▄█████▀──────────────▀████▄─
▄████▀────▄██▄────▄██▄───▀████
██▀───────████────████─────▀██
██────────████────████──────██
██────────▀██▀────▀██▀──────██
██──────────────────────────██
██──█▄──────────────────▄█──██
██──█▀██▄──────────────▄██──██
██──█──▀██▄──────────▄▄█─█──██
██──█────▀████████████▀──█──██
██──█─────█────█────█────█──██
██──█─────█────█────█────█──██
██──██▄───█────█────█──▄██──██
██──██████████████████████──██
██──█─────█────█────█────█──██
██──█─────█────█────█────█──██
██──██────█────█────█───█▀──██
██───▀██████████████████▀───██
██▄───────────────────────▄███
█████▄──────────────────▄█████
─▀███████▄───────────▄██████▀─
────▀█████████████████████▀─── ")
    (spacemacs-buffer/goto-buffer)
    (spacemacs-buffer//remove-existing-widget-if-exist)))

(defun next-next-NEXT-millennium ()
  "Time to rock on!"
  (interactive)
  (zemacs-buffer-mode -1))

(defvar zemacs--links '(("this one" "http://neovim.io")
                        ("link" "http://cdn.meme.am/instances/500x/67641307.jpg")
                        ("Atom" "http://atom.io/")
                        ("implementation tricks" "http://emacshorrors.com/")))

(defun zemacs//insert-links ()
  "Replace bracketed texts by their link counterparts."
  (with-current-buffer spacemacs-buffer-name
    (save-excursion
      (dolist (l zemacs--links)
        (re-search-backward (format "\\(\\[%s\\]\\)" (car l)) nil t)
        (make-text-button
         (match-beginning 1)
         (match-end 1)
         'type 'help-url
         'help-args (cdr l))))))

(define-advice configuration-layer/initialize (:before (&rest _) zemacs/initialize)
  (setq dotspacemacs-startup-banner (concat spacemacs-banner-directory "img/zemacs.png")))

(define-advice spacemacs-buffer//inject-version
    (:around (f &rest args) zemacs/inject-version)
  (let ((emacs-version "99.9999999")
        (dotspacemacs-distribution "zemacs")
        (spacemacs-version "af-1.01"))
    (apply f args)))

(define-advice spacemacs-buffer/insert-banner-and-buttons
    (:after (&rest _) zemacs/insert-banner-and-buttons)
  ;; always display the release note
  (spacemacs-buffer//insert-release-note-widget
   (concat spacemacs-release-notes-directory
           spacemacs-buffer-version-info ".txt")))

(add-hook 'emacs-startup-hook 'zemacs-buffer-mode t)

(provide 'zemacs)
