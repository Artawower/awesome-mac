
(define-key global-map [f8] 'bookmark-jump)
(define-key global-map [f9] 'bookmark-set)
(use-package bm
  :defer t
  :init
  (global-set-key (kbd "C-M-b") 'bm-toggle)
  (global-set-key (kbd "C-M-n") 'bm-next)
  (global-set-key (kbd "C-M-p") 'bm-previous)

  (setq bookmark-default-file "~/.emacs.d/bookmarks")  ;;define file to use.
  (setq bookmark-save-flag 1)  ;save bookmarks to .emacs.bmk after each entry
  )
