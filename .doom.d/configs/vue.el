;; (require 'eglot)
;; (require 'web-mode)
;; (require 'flycheck)
;; (setq lsp-diagnostic-package :none)

(use-package web-mode
  :defer t
  :init
  (require 'flycheck)
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (add-hook 'web-mode-hook '(lambda () (setq lsp-diagnostic-package :none)))
  (add-hook 'web-mode-hook #'company-mode)
  (add-hook 'web-mode-hook #'flycheck-mode)
  (add-hook 'web-mode-hook #'lsp)
  (setq-default indent-tabs-mode nil)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq mmm-vue-html-mode-exit-hook (lambda ()
                                      (message "Run when leaving vue-html mode")
                                      (emmet-mode -1)))
  (setq mmm-vue-html-mode-enter-hook (lambda ()
                                       (message "Run when entering vue-html mode")
                                       (emmet-mode 1)))
  )


;; (define-derived-mode genehack-vue-mode web-mode "ghVue"
;;   "A major mode derived from web-mode, for editing .vue files with LSP support.")
;; ;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . genehack-vue-mode))
;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

;; (add-hook 'genehack-vue-mode-hook #'eglot-ensure)
;; (add-hook 'genehack-vue-mode-hook #'lsp)
;; (add-hook 'genehack-vue-mode-hook #'company-mode)
;; (add-hook 'genehack-vue-mode-hook #'flycheck-mode)


;; (flycheck-add-mode 'javascript-eslint 'vue-mode)
;; (flycheck-add-mode 'javascript-eslint 'genehack-vue-mode-hook)

;; ;; (add-hook 'web-mode-hook #'lsp-flycheck-disable)
;; ;; (setq web-mode-pug-indent-offset 2)
;; ;;
;; ;; (add-hook 'genehack-vue-mode-hook #'web-mode-pug-indentation)
;; ;;
;; ;; (setq web-mode-pug-indent-offset 2)
;; ;; (setq genehack-vue-mode-pug-indent-offset 2)

;; (add-to-list 'eglot-server-programs '(genehack-vue-mode "vls"))
