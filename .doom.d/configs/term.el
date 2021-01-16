(use-package vterm-toggle
  :defer t
  :after vterm
  :ensure t
  :init
  (setq vterm-toggle-scope 'project)

  )


;; (use-package vterm-toggle
;;   :after (projectile vterm evil)
;;   :preface
;;   (defun vterm-toggle-projectile (&optional arg)
;;     (interactive "P")
;;     (if (derived-mode-p 'vterm-mode)
;;         (delete-window)
;;       (let* ((project (projectile-ensure-project (projectile-project-root)))
;;              (buffer (projectile-generate-process-name "vterm" arg)))
;;         (unless (buffer-live-p (get-buffer buffer))
;;           (projectile-with-default-dir project
;;             (vterm buffer)))
;;         (progn
;;           (vterm-toggle)
;;           (switch-to-buffer buffer)))))
;;   :config
;;   (setq vterm-toggle-fullscreen-p nil)
;;   (with-eval-after-load 'evil
;;     (evil-set-initial-state 'vterm-mode 'emacs))
;;   ;; (global-set-key (kbd "C-`") #'vterm-toggle-cd)
;;   (global-set-key (kbd "C-`") #'vterm-toggle-projectile)
;;   (add-to-list 'display-buffer-alist
;;                '("^.?v?term.*"
;;                  (display-buffer-reuse-window display-buffer-at-bottom)
;;                  (reusable-frames . visible)
;;                  (window-height . 0.5))))
