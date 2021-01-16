
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                      LSP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp
  ;; :after (vue-mode, python-mode)
  ;; :defer t
  ;; :config
  ;; (lsp-register-custom-settings
  ;;  '(("pyls.plugins.pyls_mypy.enabled" t t)
  ;;    ("pyls.plugins.pyls_mypy.live_mode" nil t)
  ;;    ("pyls.plugins.pyls_black.enabled" t t)
  ;;    ("pyls.plugins.flake8.enabled" t t)
  ;;    ("pyls.plugins.pyls_isort.enabled" t t)))
  :hook
  ((python-mode . lsp))
  :init
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-file-watch-threshold 2000)
  ;; (dolist (dir '(
  ;;                "[/\\\\]redis-data\\'"
  ;;                "[/\\\\]data\\'"
  ;;                "[/\\\\]static\\'"
  ;;                "[/\\\\]assets\\'"
  ;;                ))
  ;;   (push dir lsp-file-watch-ignored))
  ;; (add-hook 'vue-mode-hook #'lsp)
  )
