;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

(use-package flycheck
  :defer t
  :init
  (require 'flycheck-pycheckers)
  (setq flycheck-python-pylint-executable "python3")
  (setq-default flycheck-disabled-checkers '(go-gofmt go-golint scss-stylelint))
  (add-to-list 'flycheck-checkers 'flycheck-checkers)

  ;; (add-hook 'python-mode-hook (lambda ()
  ;;                               #'global-flycheck-mode
  ;;                               (flycheck-select-checker 'python-pycheckers)
  ;;                               (setq-local flycheck-checker 'python-pycheckers)
  ;;                               ))
  (global-flycheck-mode 1))

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
