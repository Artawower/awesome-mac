(use-package evil-leader
  :defer t
  :init

  (global-evil-leader-mode)

  ;; Easy moution
  (evilem-default-keybindings "SPC")

  (defun save-buffer-without-dtw ()
    (interactive)
    (let ((b (current-buffer)))   ; memorize the buffer
      (with-temp-buffer ; new temp buffer to bind the global value of before-save-hook
        (let ((before-save-hook (remove 'delete-trailing-whitespace before-save-hook)))
          (with-current-buffer b  ; go back to the current buffer, before-save-hook is now buffer-local
            (let ((before-save-hook (remove 'delete-trailing-whitespace before-save-hook)))
              (save-buffer)))))))

  (evil-leader/set-key
    "f" 'evilem-motion-next-line
    "b" 'evilem-motion-previous-line
    "p" 'prettier-prettify
    "k" 'save-buffer-without-dtw
    "a" 'counsel-org-agenda-headlines
    "q" 'kill-current-buffer

    "c" 'dired-create-empty-file

    "t" 'yafolding-toggle-element
    "z" 'yafolding-toggle-all
    "h" 'yafolding-hide-parent-element

    "d" 'dup-debug

    "v" 'vterm
    "`" 'vterm-toggle-cd

    "i" 'git-messenger:popup-message

    "o" 'origami-toggle-node
    "]" 'origami-toggle-all-nodes
    "w" 'origami-close-node-recursively
    "s" 'origami-open-node-recursively
    "r" 'lsp-ui-doc-show)

  (add-hook 'evil-insert-state-entry-hook (lambda ()
                                            (prettify-symbols-mode -1)
                                            (org-bullets-mode -1)
                                            ))
  (add-hook 'evil-normal-state-entry-hook (lambda () (prettify-symbols-mode 1) (when (derived-mode-p 'org-mode) (org-bullets-mode 1))))
  ;; (add-hook 'evil-insert-state-entry-hook (lambda ()
  ;;                                           (prettify-symbols-mode -1)))
  ;; (add-hook 'evil-normal-state-entry-hook (lambda ()
  ;;                                           (prettify-symbols-mode 1)))
  )

(use-package evil-surround
  :after evil-leader
  :config
  (global-evil-surround-mode 1))
