;; (use-package python
;;   :ensure t
;;   :mode
;;   ("\\.py\\'" . python-mode)
;;   :bind
;;   (:map
;;    python-mode-map
;;    ("C-c C-c" . compile))
;;   :config
;;   (setq python-shell-interpreter "python3"
;; 	    ;; python-shell-interpreter-args "-i")
;;         ))

(use-package python-mode
  ;; :defer t
  :init
  ;; (require 'virtualenvwrapper)
  ;; (require 'auto-virtualenvwrapper)
  (add-hook 'python-mode-hook
            (lambda ()
              ;; (add-to-list 'company-backends 'company-jedi)
              ;; (setq indent-tabs-mode 0)
              (setq tab-width 4)
              (setq python-indent-offset 4)
              (setq global-flycheck-mode 1)
              )
  ))


(use-package lsp-pyright
  :ensure t
  :config
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t)
  (setq lsp-pyright-log-level "trace")
  ;; (setq lsp-pyright-venv-path "/Users/arturarosenko/projects/atom-security/atom-security-back/venv")
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred
;; (after! lsp-python-ms
;;   (set-lsp-priority! 'pyright 1))
