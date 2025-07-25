;;; config.el --- kotlin Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Maxi Wolff <smile13241324@gmail.com>
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


;; variables

(spacemacs|define-jump-handlers kotlin-mode)

(defvar kotlin-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'company-kotlin)
  "The backend to use for IDE features.
Possible values are `lsp' and `company-kotlin'.
If `nil' then 'company-kotlin` is the default backend unless `lsp' layer is used")

(defvar kotlin-lsp-jar-path "~/install/server/bin/kotlin-language-server"
  "The path to the lsp jar file")
