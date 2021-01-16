(use-package all-the-icons-dired
  :defer t
  :after dired
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )
