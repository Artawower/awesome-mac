(use-package ivy-posframe
  :defer t
  :after ivy
  :diminish
  :custom-face
  ;; (ivy-posframe-border ((t (:background "#c95562"))))
  (ivy-posframe-border ((t (:background "#4FAAEA"))))
  :init
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-height-alist '((t . 20))
        ivy-posframe-min-width 70
        ivy-posframe-width 110
        ivy-posframe-height 20
        ivy-posframe-min-height 20
        ivy-posframe-parameters '((internal-border-width . 2) (left-fringe . 18) (right-fringe . 18) ))

  (setq ivy-posframe-width 96)
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (ivy-posframe-mode +1))



(use-package all-the-icons-ivy-rich
  :defer t
  :after ivy-posframe
  :init
  (all-the-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1)
  )
