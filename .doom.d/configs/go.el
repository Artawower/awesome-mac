
  (setenv "GOPATH" (concat (getenv "HOME") "/go"))
  (setenv "PATH" (concat (getenv "HOME") "/go/bin"))
  (use-package lsp-mode
    :after go-mode
    :ensure t
    :commands (lsp lsp-deferred)
    :hook (go-mode . lsp-deferred)
    :init
    (defun lsp-go-install-save-hooks ()
      (add-hook 'before-save-hook #'lsp-format-buffer t t)
      (add-hook 'before-save-hook #'lsp-organize-imports t t))
    (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
    (add-hook 'go-mode-hook '(lambda () (setq lsp-diagnostic-package :none)))
    )

  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.

  ;; Optional - provides fancier overlays.
  (use-package lsp-ui
    :after go-mode
    :ensure t
    :init
    ;; (setq lsp-ui-doc-enable t)
    ;; (setq lsp-ui-doc-use-webkit t)
    (setq lsp-ui-doc-position 'top)
    (setq lsp-ui-doc-max-width 180)
    (setq lsp-ui-sideline-show-hover t)
    ;; (setq lsp-ui-sideline-diagnostic-max-line-length 200)
    (setq lsp-ui-sideline-diagnostic-max-lines 5)
    (setq lsp-ui-sideline-show-symbol t)
    ;; (setq lsp-ui-doc-alignment 'window)
    (setq lsp-diagnostic-clean-after-change t)
    (setq lsp-ui-doc-delay 0.8)
    ;; (setq lsp-ui-doc-use-webkit t)
    (setq lsp-ui-doc-use-childframe t)
    :commands lsp-ui-mode)

  ;; Company mode is a standard completion package that works well with lsp-mode.
  (use-package company
    :after go-mode
    :ensure t
    :config
    ;; Optionally enable completion-as-you-type behavior.
    (setq company-idle-delay 0.1)
    (setq company-minimum-prefix-length 1)
    :init
    (add-hook 'go-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-go))
                              (company-mode)))
    )
