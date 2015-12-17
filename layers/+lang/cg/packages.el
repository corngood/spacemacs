(setq cg-packages
  '(
	cg
    ))

(defun cg/init-cg ()
  (use-package cg
    :defer t
    :init
	(add-to-list 'auto-mode-alist '("\\.compute\\'" . cg-mode))
	(add-to-list 'auto-mode-alist '("\\.cginc\\'" . cg-mode))
	(add-to-list 'auto-mode-alist '("\\.shader\\'" . cg-mode))))
