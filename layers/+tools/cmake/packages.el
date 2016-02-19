(setq cmake-packages
      '(
        gdb-mi
        cpputils-cmake
        ))

(defun cmake/init-cpputils-cmake ()
  (use-package cpputils-cmake
    :defer t
    :config
    (spacemacs/set-leader-keys "cR" 'cppcm-reload-all)))

(defun spacemacs/cmake-gdb ()
  (interactive)
  (let ((exe-path (cppcm-get-exe-path-current-buffer)))
    (if exe-path
        (gdb (concat "gdb -i=mi " exe-path))
      (error "buffer has no executable path"))))

(defun cmake/post-init-gdb-mi ()
  (spacemacs/set-leader-keys "d" 'spacemacs/cmake-gdb))

