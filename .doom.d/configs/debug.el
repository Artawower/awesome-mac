
(use-package dap-go
  :defer t
  :after go-mode
  :init
  (require 'dap-ui)
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (set-fringe-style (quote (14 . 10))) ;; Left breakpoint sqr size ;)
  )
