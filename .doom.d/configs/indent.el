;; (require 'highlight-indent-guides)
;; (setq highlight-indent-guides-method 'character)
;; (add-hook 'ng2-html-mode 'highlight-indent-guides-mode)
;; (add-hook 'yaml-mode 'highlight-indent-guides-mode)
;; (add-hook 'go-mode 'highlight-indent-guides-mode)

(use-package indent-guide
  :after org-mode
  :init
  (setq indent-guide-threshold 0)
  ;; (set-face-foreground 'indent-guide-face (face-foreground 'default))
  ;; (setq indent-guide-char "⎸")
  (setq indent-guide-char ".")

  (add-hook 'ng2-html-mode 'indent-guide-mode)
  (add-hook 'ng2-ts-mode 'indent-guide-mode)
  (add-hook 'yaml-mode 'indent-guide-mode)
  (add-hook 'html-mode 'indent-guide-mode)
  (add-hook 'python-mode 'indent-guide-mode)
  (add-hook 'web-mode 'indent-guide-mode)
  (add-hook 'scss-mode 'indent-guide-mode)
  (add-hook 'css-mode 'indent-guide-mode)
  (add-hook 'go-mode 'indent-guide-mode)
  ;; (indent-guide-global-mode)
  )
