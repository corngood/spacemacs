(configuration-layer/declare-layers '(csharp cg yaml))

(add-to-list 'auto-mode-alist '("\\.compute\\'" . cg-mode))
(add-to-list 'auto-mode-alist '("\\.cginc\\'" . cg-mode))
(add-to-list 'auto-mode-alist '("\\.shader\\'" . cg-mode))
(add-to-list 'auto-mode-alist '("\\.asset\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.prefab\\'" . yaml-mode))
