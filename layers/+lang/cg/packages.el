(setq cg-packages '((cg-mode :location local)))

(defun cg/init-cg-mode ()
  (use-package cg-mode
    :defer t
    :init (autoload 'cg-mode "cg-mode")))
