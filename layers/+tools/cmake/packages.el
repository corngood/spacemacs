(setq cmake-packages
      '(
        gdb-mi
        (cpputils-cmake :location (recipe
                                   :fetcher github
                                   :repo "corngood/cpputils-cmake"
                                   :branch "semantic_includes"
                                   ))
        ))

(defun cmake/init-cpputils-cmake ()
  (use-package cpputils-cmake
    :commands (cppcm-reload-all)
    :defer t
    :init
    (progn
      (add-hook 'c-mode-hook 'cppcm-reload-all)
      (add-hook 'c++-mode-hook 'cppcm-reload-all))
    :config
    (progn
      (spacemacs/set-leader-keys "cR" 'cppcm-reload-all))))

(defun spacemacs/cmake-gdb ()
  (interactive)
  (let ((exe-path (cppcm-get-exe-path-current-buffer)))
    (if exe-path
        (gdb (concat "gdb -i=mi " exe-path))
      (error "buffer has no executable path"))))

(defun cmake/post-init-gdb-mi ()
  (spacemacs/set-leader-keys "d" 'spacemacs/cmake-gdb))
