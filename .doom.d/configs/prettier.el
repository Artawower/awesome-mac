;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                      PRETTIER
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prettier
;; (add-hook 'js2-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook 'prettier-js-mode)
;; (add-hook 'ng2-ts-mode 'prettier-js-mode)
;; (add-hook 'gehack-vue-mode-hook 'prettier-js-mode)

(use-package prettier
  :defer 5
  :init

  (global-prettier-mode)
  (add-hook 'ng2-mode 'prettier-js-mode)
  (add-hook 'ng2-html-mode 'prettier-js-mode)

  (add-hook 'before-init-hook #'global-prettier-mode)
  ;; (add-hook 'before-save-hook #'prettier-prettify)

  ;; (add-hook 'gehack-vue-mode-hook
  ;;           (lambda () (add-hook 'before-save-hook prettier-prettify nil 'local)))

  (add-hook 'before-save-hook #'format-all-buffer nil t)
  )
