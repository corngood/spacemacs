;;; core-themes-support.el --- Spacemacs Core File -*- lexical-binding: t -*-
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

(require 'core-display-init)

(defconst emacs-built-in-themes (cons 'default (custom-available-themes))
  "List of emacs built-in themes.")

(defvar spacemacs--fallback-theme nil
  "Fallback theme if user theme cannot be applied.")

(defvar spacemacs--delayed-user-theme nil
  "Internal variable storing user theme to be installed.")

(defvar spacemacs--cur-theme nil
  "Internal variable storing currently loaded theme.")

(defface org-kbd
  '((t (:background "LemonChiffon1" :foreground "black" :box
                    (:line-width 2 :color nil :style released-button))))
  "Face for displaying key bindings in Spacemacs documents."
  :group 'org-faces)

;; Do NOT inflate this map, encourage users to use '(<theme> :package <pkg-name>)
;; in `dotspacemacs-themes'.
(defconst spacemacs-theme-name-to-package
  '(
    (alect-black                      . alect-themes)
    (alect-black-alt                  . alect-themes)
    (alect-dark                       . alect-themes)
    (alect-dark-alt                   . alect-themes)
    (alect-light                      . alect-themes)
    (alect-light-alt                  . alect-themes)
    (almost-mono-black                . almost-mono-themes)
    (almost-mono-cream                . almost-mono-themes)
    (almost-mono-gray                 . almost-mono-themes)
    (almost-mono-white                . almost-mono-themes)
    (ample-flat                       . ample-theme)
    (ample-light                      . ample-theme)
    (apropospriate-dark               . apropospriate-theme)
    (apropospriate-light              . apropospriate-theme)
    (base16-3024                      . base16-theme)
    (base16-apathy                    . base16-theme)
    (base16-ashes                     . base16-theme)
    (base16-atelier-cave              . base16-theme)
    (base16-atelier-cave-light        . base16-theme)
    (base16-atelier-dune              . base16-theme)
    (base16-atelier-dune-light        . base16-theme)
    (base16-atelier-estuary           . base16-theme)
    (base16-atelier-estuary-light     . base16-theme)
    (base16-atelier-forest            . base16-theme)
    (base16-atelier-forest-light      . base16-theme)
    (base16-atelier-heath             . base16-theme)
    (base16-atelier-heath-light       . base16-theme)
    (base16-atelier-lakeside          . base16-theme)
    (base16-atelier-lakeside-light    . base16-theme)
    (base16-atelier-plateau           . base16-theme)
    (base16-atelier-plateau-light     . base16-theme)
    (base16-atelier-savanna           . base16-theme)
    (base16-atelier-savanna-light     . base16-theme)
    (base16-atelier-seaside           . base16-theme)
    (base16-atelier-seaside-light     . base16-theme)
    (base16-atelier-sulphurpool       . base16-theme)
    (base16-atelier-sulphurpool-light . base16-theme)
    (base16-atlas                     . base16-theme)
    (base16-bespin                    . base16-theme)
    (base16-black-metal               . base16-theme)
    (base16-black-metal-bathory       . base16-theme)
    (base16-black-metal-burzum        . base16-theme)
    (base16-black-metal-dark-funeral  . base16-theme)
    (base16-black-metal-gorgoroth     . base16-theme)
    (base16-black-metal-immortal      . base16-theme)
    (base16-black-metal-khold         . base16-theme)
    (base16-black-metal-marduk        . base16-theme)
    (base16-black-metal-mayhem        . base16-theme)
    (base16-black-metal-nile          . base16-theme)
    (base16-black-metal-venom         . base16-theme)
    (base16-brewer                    . base16-theme)
    (base16-bright                    . base16-theme)
    (base16-brogrammer                . base16-theme)
    (base16-brushtrees                . base16-theme)
    (base16-brushtrees-dark           . base16-theme)
    (base16-chalk                     . base16-theme)
    (base16-circus                    . base16-theme)
    (base16-classic-dark              . base16-theme)
    (base16-classic-light             . base16-theme)
    (base16-codeschool                . base16-theme)
    (base16-cupcake                   . base16-theme)
    (base16-cupertino                 . base16-theme)
    (base16-darktooth                 . base16-theme)
    (base16-default-dark              . base16-theme)
    (base16-default-light             . base16-theme)
    (base16-dracula                   . base16-theme)
    (base16-eighties                  . base16-theme)
    (base16-embers                    . base16-theme)
    (base16-flat                      . base16-theme)
    (base16-fruit-soda                . base16-theme)
    (base16-github                    . base16-theme)
    (base16-google-dark               . base16-theme)
    (base16-google-light              . base16-theme)
    (base16-grayscale-dark            . base16-theme)
    (base16-grayscale-light           . base16-theme)
    (base16-greenscreen               . base16-theme)
    (base16-gruvbox-dark-hard         . base16-theme)
    (base16-gruvbox-dark-medium       . base16-theme)
    (base16-gruvbox-dark-pale         . base16-theme)
    (base16-gruvbox-dark-soft         . base16-theme)
    (base16-gruvbox-light-hard        . base16-theme)
    (base16-gruvbox-light-medium      . base16-theme)
    (base16-gruvbox-light-soft        . base16-theme)
    (base16-harmonic-dark             . base16-theme)
    (base16-harmonic-light            . base16-theme)
    (base16-heetch                    . base16-theme)
    (base16-heetch-light              . base16-theme)
    (base16-helios                    . base16-theme)
    (base16-hopscotch                 . base16-theme)
    (base16-horizon-dark              . base16-theme)
    (base16-ia-dark                   . base16-theme)
    (base16-ia-light                  . base16-theme)
    (base16-icy                       . base16-theme)
    (base16-irblack                   . base16-theme)
    (base16-isotope                   . base16-theme)
    (base16-macintosh                 . base16-theme)
    (base16-marrakesh                 . base16-theme)
    (base16-materia                   . base16-theme)
    (base16-material                  . base16-theme)
    (base16-material-darker           . base16-theme)
    (base16-material-lighter          . base16-theme)
    (base16-material-palenight        . base16-theme)
    (base16-mellow-purple             . base16-theme)
    (base16-mexico-light              . base16-theme)
    (base16-mocha                     . base16-theme)
    (base16-monokai                   . base16-theme)
    (base16-nord                      . base16-theme)
    (base16-ocean                     . base16-theme)
    (base16-oceanicnext               . base16-theme)
    (base16-one-light                 . base16-theme)
    (base16-one-light                 . base16-theme)
    (base16-onedark                   . base16-theme)
    (base16-outrun-dark               . base16-theme)
    (base16-papercolor-dark           . base16-theme)
    (base16-papercolor-light          . base16-theme)
    (base16-paraiso                   . base16-theme)
    (base16-paraiso                   . base16-theme)
    (base16-phd                       . base16-theme)
    (base16-pico                      . base16-theme)
    (base16-pop                       . base16-theme)
    (base16-porple                    . base16-theme)
    (base16-railscasts                . base16-theme)
    (base16-rebecca                   . base16-theme)
    (base16-seti                      . base16-theme)
    (base16-shapeshifter              . base16-theme)
    (base16-snazzy                    . base16-theme)
    (base16-solarflare                . base16-theme)
    (base16-solarized-dark            . base16-theme)
    (base16-solarized-light           . base16-theme)
    (base16-spacemacs                 . base16-theme)
    (base16-summerfruit-dark          . base16-theme)
    (base16-summerfruit-light         . base16-theme)
    (base16-synth-midnight-dark       . base16-theme)
    (base16-tomorrow                  . base16-theme)
    (base16-tomorrow-night            . base16-theme)
    (base16-tube                      . base16-theme)
    (base16-twilight                  . base16-theme)
    (base16-unikitty-dark             . base16-theme)
    (base16-unikitty-light            . base16-theme)
    (base16-woodland                  . base16-theme)
    (base16-xcode-dusk                . base16-theme)
    (base16-zenburn                   . base16-theme)
    (brin                             . sublime-themes)
    (doom-Iosvkem                     . doom-themes)
    (doom-acario-dark                 . doom-themes)
    (doom-acario-light                . doom-themes)
    (doom-challenger-deep             . doom-themes)
    (doom-city-lights                 . doom-themes)
    (doom-dark+                       . doom-themes)
    (doom-dracula                     . doom-themes)
    (doom-earl-grey                   . doom-themes)
    (doom-ephemeral                   . doom-themes)
    (doom-fairy-floss                 . doom-themes)
    (doom-gruvbox                     . doom-themes)
    (doom-gruvbox-light               . doom-themes)
    (doom-horizon                     . doom-themes)
    (doom-laserwave                   . doom-themes)
    (doom-manegarm                    . doom-themes)
    (doom-material                    . doom-themes)
    (doom-molokai                     . doom-themes)
    (doom-monokai-classic             . doom-themes)
    (doom-monokai-spectrum            . doom-themes)
    (doom-monokai-pro                 . doom-themes)
    (doom-moonlight                   . doom-themes)
    (doom-nord                        . doom-themes)
    (doom-nord-light                  . doom-themes)
    (doom-nova                        . doom-themes)
    (doom-oceanic-next                . doom-themes)
    (doom-one                         . doom-themes)
    (doom-one-light                   . doom-themes)
    (doom-opera                       . doom-themes)
    (doom-opera-light                 . doom-themes)
    (doom-outrun-electric             . doom-themes)
    (doom-palenight                   . doom-themes)
    (doom-peacock                     . doom-themes)
    (doom-snazzy                      . doom-themes)
    (doom-solarized-dark              . doom-themes)
    (doom-solarized-light             . doom-themes)
    (doom-sourcerer                   . doom-themes)
    (doom-spacegrey                   . doom-themes)
    (doom-tokyo-night                 . doom-themes)
    (doom-tomorrow-day                . doom-themes)
    (doom-tomorrow-night              . doom-themes)
    (doom-vibrant                     . doom-themes)
    (doom-wilmersdorf                 . doom-themes)
    (doom-xcode                       . doom-themes)
    (doom-zenburn                     . doom-themes)
    (dorsey                           . sublime-themes)
    (ef-arbutus                       . ef-themes)
    (ef-autumn                        . ef-themes)
    (ef-bio                           . ef-themes)
    (ef-cherie                        . ef-themes)
    (ef-cyprus                        . ef-themes)
    (ef-dark                          . ef-themes)
    (ef-day                           . ef-themes)
    (ef-deuteranopia-dark             . ef-themes)
    (ef-deuteranopia-light            . ef-themes)
    (ef-dream                         . ef-themes)
    (ef-duo-dark                      . ef-themes)
    (ef-duo-light                     . ef-themes)
    (ef-elea-dark                     . ef-themes)
    (ef-elea-light                    . ef-themes)
    (ef-frost                         . ef-themes)
    (ef-kassio                        . ef-themes)
    (ef-light                         . ef-themes)
    (ef-maris-dark                    . ef-themes)
    (ef-maris-light                   . ef-themes)
    (ef-melissa-dark                  . ef-themes)
    (ef-melissa-light                 . ef-themes)
    (ef-night                         . ef-themes)
    (ef-reverie                       . ef-themes)
    (ef-rosa                          . ef-themes)
    (ef-spring                        . ef-themes)
    (ef-summer                        . ef-themes)
    (ef-symbiosis                     . ef-themes)
    (ef-trio-dark                     . ef-themes)
    (ef-trio-light                    . ef-themes)
    (ef-tritanopia-dark               . ef-themes)
    (ef-tritanopia-light              . ef-themes)
    (ef-winter                        . ef-themes)
    (eziam-dark                       . eziam-themes)
    (eziam-dusk                       . eziam-themes)
    (eziam-light                      . eziam-themes)
    (farmhouse-dark                   . farmhouse-themes)
    (farmhouse-light                  . farmhouse-themes)
    (fogus                            . sublime-themes)
    (graham                           . sublime-themes)
    (granger                          . sublime-themes)
    (gruvbox-dark-hard                . gruvbox-theme)
    (gruvbox-dark-medium              . gruvbox-theme)
    (gruvbox-dark-soft                . gruvbox-theme)
    (gruvbox-light-hard               . gruvbox-theme)
    (gruvbox-light-medium             . gruvbox-theme)
    (gruvbox-light-soft               . gruvbox-theme)
    (hemisu-dark                      . hemisu-theme)
    (hemisu-light                     . hemisu-theme)
    (hickey                           . sublime-themes)
    (junio                            . sublime-themes)
    (kaolin-aurora                    . kaolin-themes)
    (kaolin-breeze                    . kaolin-themes)
    (kaolin-bubblegum                 . kaolin-themes)
    (kaolin-dark                      . kaolin-themes)
    (kaolin-eclipse                   . kaolin-themes)
    (kaolin-galaxy                    . kaolin-themes)
    (kaolin-light                     . kaolin-themes)
    (kaolin-mono-dark                 . kaolin-themes)
    (kaolin-ocean                     . kaolin-themes)
    (kaolin-temple                    . kaolin-themes)
    (kaolin-valley-dark               . kaolin-themes)
    (kaolin-valley-light              . kaolin-themes)
    (material-light                   . material-theme)
    (mccarthy                         . sublime-themes)
    (minimal-light                    . minimal-theme)
    (moe-dark                         . moe-theme)
    (moe-light                        . moe-theme)
    (odersky                          . sublime-themes)
    (omtose-darker                    . omtose-phellack-theme)
    (omtose-softer                    . omtose-phellack-theme)
    (poet-dark                        . poet-theme)
    (poet-dark-monochrome             . poet-theme)
    (poet-monochrome                  . poet-theme)
    (ritchie                          . sublime-themes)
    (sanityinc-solarized-dark         . color-theme-sanityinc-solarized)
    (sanityinc-solarized-light        . color-theme-sanityinc-solarized)
    (sanityinc-tomorrow-blue          . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-bright        . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-day           . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-eighties      . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-night         . color-theme-sanityinc-tomorrow)
    (solarized-dark                   . solarized-theme)
    (solarized-dark-high-contrast     . solarized-theme)
    (solarized-gruvbox-dark           . solarized-theme)
    (solarized-gruvbox-light          . solarized-theme)
    (solarized-light                  . solarized-theme)
    (solarized-light-high-contrast    . solarized-theme)
    (solarized-wombat-dark            . solarized-theme)
    (solarized-zenburn                . solarized-theme)
    (spacemacs-dark                   . spacemacs-theme)
    (spacemacs-light                  . spacemacs-theme)
    (spolsky                          . sublime-themes)
    (tao-yang                         . tao-theme)
    (tao-yin                          . tao-theme)
    (wilson                           . sublime-themes)
    )
  "Alist matching a theme name with its package name.

Required when package name does not match theme name + `-theme' suffix.")

(defvar spacemacs-post-theme-change-hook nil
  "Hook run after theme has changed.")

(defun spacemacs/get-theme-package-name (theme)
  "Return the package theme for the given THEME name."
  (if-let* (((listp theme))
            (pkg-name (plist-get (cdr theme) :package)))
      pkg-name
    (let ((theme-name (or (car-safe theme) theme)))
      (cond
       ;; built-in
       ((memq theme-name emacs-built-in-themes) nil)
       ;; from explicit alist
       ((assq theme-name spacemacs-theme-name-to-package)
        (cdr (assq theme-name spacemacs-theme-name-to-package)))
       ;; fallback to <name>-theme
       (t (intern (format "%S-theme" theme-name)))))))

(defun spacemacs//get-theme-name (theme)
  "Return the name of THEME."
  (if (listp theme)
      (car theme)
    theme))

(defun spacemacs//get-theme-package-directory (theme)
  "Return the THEME location on disk."
  (let* ((pkg-name (spacemacs/get-theme-package-name theme))
         (dir (when (listp theme)
                (configuration-layer/get-location-directory
                 pkg-name
                 (plist-get (cdr theme) :location)
                 'dotfile))))
    (unless dir
      ;; fallback to elpa directory
      (setq dir (configuration-layer/get-elpa-package-install-directory
                 pkg-name)))
    dir))

(defun spacemacs//guess-fallback-theme (theme)
  "Guess the fallback theme for a THEME."
  (when theme
    (or (and (listp theme)
             (plist-get (cdr theme) :fallback))
        (let ((name (spacemacs//get-theme-name theme)))
          (cond ((string-match-p "light" (symbol-name name))
                 'spacemacs-light)
                ((string-match-p "dark" (symbol-name name))
                 'spacemacs-dark))))))

(defun spacemacs/load-default-theme ()
  "Load default theme.
Default theme is the first element of `dotspacemacs-themes'.  If
loading the default theme fails, set
`spacemacs--delayed-user-theme' to postpone the action and try
again layer configuration."
  ;; This function is called before all packages are necessarily activated, so
  ;; if failed to load the theme we can try again after the packages activated.
  (if-let* ((default-theme (car dotspacemacs-themes))
            (theme-name (spacemacs//get-theme-name default-theme)))
      (progn
        ;; non-registered theme, assume the theme is from a package
        (when-let* (((not (memq theme-name (cons 'default (custom-available-themes)))))
                    (pkg-name (spacemacs/get-theme-package-name default-theme)))
          (when dotspacemacs-enable-package-quickstart
            (spacemacs-buffer/warning
             (format-message "Your default theme %s requires full package initialization, negating the benefit of `dotspacemacs-enable-package-quickstart'."
                             theme-name)))
          (unless package--initialized
            (package-initialize 'no-activate))
          (package-activate pkg-name)
          (spacemacs//activate-theme-packages (list default-theme)))
        (condition-case _
            (spacemacs//load-theme-internal theme-name)
          (error (setq spacemacs--delayed-user-theme theme-name)
                 (setq spacemacs--fallback-theme
                       (or (spacemacs//guess-fallback-theme default-theme)
                           'spacemacs-dark))
                 (spacemacs//load-theme-internal spacemacs--fallback-theme))))
    (spacemacs-buffer/warning
     (concat "Please check the `dotspacemacs-themes' in your dotfile\n"
             "to make sure it has valid themes. Invalid value: \"%s\"")
     theme-name)))

(defun spacemacs/load-theme (theme &optional fallback-theme disable)
  "Apply user theme.
If FALLBACK-THEME is non-nil it must be a package name which will be loaded if
THEME cannot be applied.
If DISABLE is non-nil then disable all previously applied themes before applying
THEME."
  (let ((theme-name (spacemacs//get-theme-name theme)))
    (condition-case err
        (if (eq 'default theme-name)
            (mapc 'disable-theme custom-enabled-themes)
          (spacemacs//load-theme-internal theme-name))
      (error
       (if (or (null fallback-theme) (eq theme fallback-theme))
           ;; no fallback theme was specified, so we log explicit warning
           (spacemacs-buffer/warning
            "An error occurred while applying the theme \"%s\", error was: %s"
            theme-name spacemacs--fallback-theme err)
         ;; apply the fallback-theme
         (spacemacs//load-theme-internal fallback-theme disable)
         ;; pop up fallback theme to the top of the list
         (setq dotspacemacs-themes
               (cons theme-name (delq theme-name dotspacemacs-themes)))
         (spacemacs-buffer/warning
          "Failed to apply theme \"%s\", fallback to theme \"%s\""
          theme-name fallback-theme))))))

(defun spacemacs//load-theme-internal (theme-name &optional disable)
  "Load and enable the theme with THEME-NAME to be the active theme.
If DISABLE is non-nil then disable all previously applied themes before applying
THEME."
  (when (load-theme theme-name 'no-confirm 'no-enable)
    (when disable
      (mapc 'disable-theme custom-enabled-themes))
    (enable-theme theme-name)
    (setq spacemacs--cur-theme theme-name)
    (unless (display-graphic-p)
      (spacemacs|do-after-display-system-init
        (load-theme theme-name 'no-confirm)))))

(defun spacemacs/cycle-spacemacs-theme (&optional backward)
  "Cycle through themes defined in `dotspacemacs-themes'.
When BACKWARD is non-nil, or with \\[universal-argument], cycle backwards."
  (interactive "P")
  (let* ((theme-names (mapcar 'spacemacs//get-theme-name dotspacemacs-themes))
         (themes (if backward (reverse theme-names) theme-names))
         (next-theme (car (or (cdr (memq spacemacs--cur-theme themes))
                              ;; if current theme isn't in cycleable themes, start
                              ;; over
                              themes))))
    (when spacemacs--cur-theme
      (disable-theme spacemacs--cur-theme))
    (let ((progress-reporter
           (make-progress-reporter
            (format "Loading theme %s..." next-theme))))
      (spacemacs/load-theme next-theme nil 'disable)
      (progress-reporter-done progress-reporter))))

(defun spacemacs/cycle-spacemacs-theme-backward ()
  "Cycle through themes defined in `dotspacemacs-themes' backward."
  (interactive)
  (spacemacs/cycle-spacemacs-theme t))

(define-advice enable-theme (:after (theme &rest _) spacemacs//run-post-theme-hooks)
  "Perform post load processing."
  (setq spacemacs--cur-theme theme)
  (run-hooks 'spacemacs-post-theme-change-hook))

(defun spacemacs/theme-loader ()
  "Call appropriate theme loader based on completion framework."
  (interactive)
  (cond
   ((configuration-layer/layer-used-p 'helm)
    (call-interactively 'spacemacs/helm-themes))
   ((configuration-layer/layer-used-p 'ivy)
    (call-interactively 'counsel-load-theme))
   ((configuration-layer/layer-used-p 'compleseus)
    (call-interactively 'consult-theme))
   (t (call-interactively 'load-theme))))

(defun spacemacs//add-theme-packages-to-additional-packages ()
  "Add all theme packages from `dotspacemacs-themes' to packages to install."
  (setq dotspacemacs--additional-theme-packages nil)
  (dolist (theme dotspacemacs-themes)
    (let* ((pkg-name (spacemacs/get-theme-package-name theme))
           (theme2 (copy-tree theme)))
      (when pkg-name
        (if (listp theme2)
            (setcar theme2 pkg-name)
          (setq theme2 pkg-name))
        (add-to-list 'dotspacemacs--additional-theme-packages theme2)))))
(add-hook 'configuration-layer-pre-load-hook
          'spacemacs//add-theme-packages-to-additional-packages)

(defun spacemacs//activate-theme-packages (&optional themes-list)
  "Activate all theme packages from THEMES-LIST.

If THEMES-LIST is nil, activate theme packages from
`dotspacemacs-themes' instead."
  ;; Not all themes add themselves to `custom-theme-load-path' in autoloads.
  ;; (for example, moe-theme).
  ;;
  ;; Also, if a theme is :location local, autoloads do not happen,
  ;; so this is needed for those packages.
  (dolist (theme (or themes-list dotspacemacs-themes))
    (when-let* ((name (spacemacs//get-theme-name theme))
                ((not (memq name (cons 'default (custom-available-themes)))))
                (pkg-dir (spacemacs//get-theme-package-directory theme)))
      (add-to-list 'custom-theme-load-path pkg-dir))))

(add-hook 'configuration-layer-post-load-hook #'spacemacs//activate-theme-packages)

(provide 'core-themes-support)
