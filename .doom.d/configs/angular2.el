;; ANGULAR
;; (require 'ng2-mode)
;; (require 'prettier)
;; (autoload 'ng2-mode "ng-2-mode" t)
;; (autoload 'ng2-html-mode "ng2-html-mode" t)
(use-package ng2-mode
  :init
  (setq lsp-clients-angular-language-server-command
        '("node"
          "/usr/local/lib/node_modules/@angular/language-server"
          "--ngProbeLocations"
          "/usr/local/lib/node_modules"
          "--tsProbeLocations"
          "/usr/local/lib/node_modules"
          "--stdio"))
  (setq read-process-output-max (* 1024 1024))
  :config
  (with-eval-after-load 'typescript-mode (add-hook 'typescript-mode-hook #'lsp))

  (with-eval-after-load 'typescript-mode (add-hook 'ng2-html-mode-hook #'lsp))





  (with-eval-after-load 'tide
    (flycheck-add-mode 'typescript-tslint 'ng2-ts-mode)
    (flycheck-add-mode 'typescript-tide 'ng2-ts-mode)
    )

  (with-eval-after-load 'ng2-html-mode (add-hook 'ng2-html-mode-hook (lambda ()
                                                                       (set (make-local-variable 'company-backends) '(company-web-html))
                                                                       (lsp)
                                                                       (company-mode t)))
                        (add-hook 'ng2-html-mode-hook #'lsp)
                        (add-hook 'ng2-mode-hook #'lsp))
  )
