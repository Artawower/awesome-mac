;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                      AUTOCOMPLETE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package lsp-mode
  :commands lsp)

(use-package company-lsp
  :after lsp-mode
  :config (push 'company-lsp company-backends)

  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (push 'company-lsp company-backends)
  (add-hook 'after-init-hook 'global-company-mode)
  )


(use-package company-posframe
  :after company-lsp
  :config
  (company-posframe-mode 1)
  (add-to-list 'company-backends 'company-restclient)
  )
