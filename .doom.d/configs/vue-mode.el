(use-package vue-mode
  :ensure t
  :mode ("\\.vue\\'")
  :config
  ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
  (setq mmm-submode-decoration-level 1)
)
;; (add-hook 'vue-mode-hook (flycheck-select-checker 'javascript-eslint))
(setq mmm-vue-html-mode-exit-hook (lambda ()
                                (message "Run when leaving vue-html mode")
                                (emmet-mode -1)))
(setq mmm-vue-html-mode-enter-hook (lambda ()
                                (message "Run when entering vue-html mode")
                                (emmet-mode 1)))
;; (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;; (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(custom-set-faces '(mmm-default-submode-face ((t (:background nil)))))
(setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))

(setq 'web-mode-javascript-indentation 2)
(setq-default indent-tabs-mode nil)
;; (add-hook 'vue-mode-hook #'lsp!)


;; (use-package vue-mode
;;   :mode "\\.vue\\'"
;;   :hook (vue-mode . prettier-js-mode)
;;   :config
;;   (add-to-list 'vue-modes '(:type template :name nil :mode web-mode))
;;   (add-to-list 'vue-modes '(:type template :name html :mode web-mode))
;;   (add-to-list 'vue-modes '(:type template :name pug :mode pug-mode))
;;   (add-to-list 'vue-modes '(:type script :name nil :mode js2-mode))
;;   (add-hook 'vue-mode-hook #'lsp)
;;   (setq prettier-js-args '("--parser vue"))
;;   )


;; (use-package vue-mode
;;   :mode "\\.vue\\'"
;;   :hook (vue-mode . prettier-js-mode)
;;   :config
;;   (add-to-list 'vue-modes '(:type template :name nil :mode web-mode))
;;   (add-to-list 'vue-modes '(:type template :name html :mode web-mode))
;;   (add-to-list 'vue-modes '(:type template :name pug :mode pug-mode))
;;   (add-to-list 'vue-modes '(:type script :name nil :mode js2-mode))
;;   (add-hook 'vue-mode-hook #'lsp)
;;   (setq prettier-js-args '("--parser vue"))
;;   (setq mmm-submode-decoration-level 2)
;;   )
  ;; (use-package vue-mode
  ;;              :config
  ;;              ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
  ;;              (setq mmm-submode-decoration-level 0)))
  ;;
  ;;
  ;;
  ;;
  ;;
  ;;
  ;;
  ;;
;; (use-package lsp-mode                   ;
;;   :commands lsp
;;   :hook ((before-save . lsp-format-buffer)
;;          (before-save . lsp-organize-imports))
;;   :config
;;   (setq lsp-auto-guess-root t)
;;   (setq lsp-document-sync-method 'incremental))

;; (use-package company-lsp
;;   :after lsp-mode
;;   :config (push 'company-lsp company-backends))

;; (use-package lsp
;;   :ensure nil
;;   :mode "\\.vue\\'"
;;   :config
;;   (add-hook 'vue-mode-hook #'lsp)

;;   (setq prettier-js-args '("--parser vue"))

;;   (add-hook 'vue-mode-hook #'prettier-js))
;; (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;; (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;; (use-package vue-mode
;;   :mode "\\.vue\\'"
;;   :config
;;   (setq vue-html-tab-width 4)

;;   (add-hook 'vue-mode-hook #'lsp)
;;   (setq prettier-js-args '("--parser vue"))

;;   (setq mmm-submode-decoration-level 0)
;;   )




;; (use-package vue-mode
;;   :mode "\\.vue\\'"
;;   :config
;;   (add-hook 'vue-mode-hook #'lsp)
;;   (add-to-list 'vue-modes '(:type template :name nil :mode web-mode))
;;   (add-to-list 'vue-modes '(:type template :name html :mode web-mode))
;;   (add-to-list 'vue-modes '(:type template :name pug :mode pug-mode)))

;; (use-package lsp-mode
;;   :custom
;;   (lsp-vetur-format-default-formatter-css "none")
;;   (lsp-vetur-format-default-formatter-html "none")
;;   (lsp-vetur-format-default-formatter-js "none")
;;   (lsp-vetur-validation-template nil))

;; (use-package vue-mode
;;   :mode "\\.vue\\'"
;;   :hook (vue-mode . prettier-js-mode)
;;   :config
;;   (add-hook 'vue-mode-hook #'lsp)
;;   (setq prettier-js-args '("--parser vue")))

;; (custom-set-faces '(mmm-default-submode-face ((t (:background nil)))))





;; (require 'vue-mode)
;; (require 'flycheck)
;; (flycheck-add-mode 'javascript-eslint 'vue-mode)

;; (defun vuejs-custom ()
;;   (setq vue-html-tab-width 2)
;;   (flycheck-mode t)
;;   (rainbow-mode)
;;   (global-set-key (kbd "C-c C-l") 'vue-mode-reparse)
;;   (global-set-key (kbd "C-c C-e") 'vue-mode-edit-indirect-at-point)
;;   (add-to-list 'write-file-functions 'delete-trailing-whitespace)
;;   (turn-on-diff-hl-mode))

;; (add-hook 'vue-mode-hook 'vuejs-custom)

;; (setq mmm-submode-decoration-level 0)

;; (defun js-custom ()
;;   (flycheck-mode t)
;;   (company-mode)
;;   (set (make-local-variable 'tab-width) 2)
;;   (setq js-indent-level 2))

;; (add-hook 'js-mode-hook 'js-custom)

;; ;; For vue-mode with Emacs 26.3
;; (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;; (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))



;; (require 'lsp-mode)
;; (require 'lsp-ui)
;; (require 'company-lsp)


;; (setq lsp-prefer-flymake :none)

;; (add-to-list 'company-backends 'company-lsp)
;; (setq company-lsp-cache-candidates t)

;; (setq lsp-ui-doc-enable nil)
;; (setq lsp-ui-imenu-enable t)
;; (setq lsp-ui-peek-enable t)
;; (setq lsp-ui-sideline-enable t)
;; ;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map (kbd "M--") #'lsp-ui-peek-find-references)

;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (add-hook 'vue-mode-hook #'lsp!)

;; (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;; (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;; (add-hook 'js-mode-hook (lambda () (setq syntax-ppss-table nil)))
