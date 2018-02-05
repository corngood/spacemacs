(setq zig-packages
      '(
        zig-mode
        ))

(defun zig/init-zig-mode ()
  (use-package zig-mode
    :defer t))
