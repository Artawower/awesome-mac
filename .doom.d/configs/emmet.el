(use-package emmet-mode
  :defer t
  :init
  (add-hook 'ng2-html-mode-hook 'ac-emmet-html-setup)
  (add-hook 'html-mode 'ac-emmet-html-setup)
  (add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
  (add-hook 'css-mode-hook 'ac-emmet-css-setup)
  )
