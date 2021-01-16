(with-eval-after-load 'css-mode
  (defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

  (defun run-sass-auto-fix ()
    "Run sass auto fix if cli tool exist"
    (interactive)
    (let ((default-directory (file-name-directory buffer-file-name)))
      (shell-command "sass-lint-auto-fix")
      (revert-buffer-no-confirm)
      (message "SASS FORMATTED")
      ))
  (add-hook 'scss-mode-hook '(lambda () (add-hook 'after-save-hook #'run-sass-auto-fix t t)))
  )
