
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                      GIT
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'ssh-agency)

(use-package git-gutter
  :defer t
  :init
  (global-git-gutter-mode)
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
  )

(use-package magit
  :defer t
  :ensure t
  :init
  ;; (require 'keychain-environment)
  ;; (keychain-refresh-environment)
  (setq magit-git-debug t)
  ;; Open remote repo
  (defun parse-url (url)
    "convert a git remote location as a HTTP URL"
    (if (string-match "^http" url)
        url
      (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                "https://\\2/\\3"
                                url)))
  (defun magit-open-repo ()
    "open remote repo URL"
    (interactive)
    (let ((url (magit-get "remote" "origin" "url")))
      (progn
        (browse-url (parse-url url))
        (message "opening repo %s" url))))


  (add-hook 'magit-mode-hook
            (lambda ()
              (local-set-key (kbd "o") 'magit-open-repo)))

  (defun forge-browse-buffer-file ()
    (interactive
     (browse-url
      (let
          ((rev (magit-get-current-branch))
           (repo (forge-get-repository 'stub))
           (file (file-relative-name buffer-file-name (projectile-project-root))))
        (forge--format repo "https://%h/%o/%n/blob/%r/%f"
                       `((?r . ,rev) (?f . ,file)))))))
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

  )


;; (use-package doom-themes
;;   :preface (defvar doom-nord-region-highlight t)
;;   :init (load-theme 'doom-one t))



(use-package forge
  :after magit
  )


(use-package pretty-hydra
  :after git-messenger
  :bind ("<f6>" . toggles-hydra/body)
  :init
  (setq centaur-icon t)                          ;
  (defun icons-displayable-p ()
    "Return non-nil if `all-the-icons' is displayable."
    (and centaur-icon
         (display-graphic-p)
         (require 'all-the-icons nil t)))
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))




(use-package git-messenger
  :defer t
  :bind (:map vc-prefix-map
         ("p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init (setq git-messenger:show-detail t
              git-messenger:use-magit-popup t)
  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra git-messenger-hydra (:color blue)
        ("s" git-messenger:popup-show "show")
        ("c" git-messenger:copy-commit-id "copy hash")
        ("m" git-messenger:copy-message "copy message")
        ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "go parent")
        ("q" git-messenger:popup-close "quit")))

    (defun my-git-messenger:format-detail (vcs commit-id author message)
      (if (eq vcs 'git)
          (let ((date (git-messenger:commit-date commit-id))
                (colon (propertize ":" 'face 'font-lock-comment-face)))
            (concat
             (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                     (propertize "Commit" 'face 'font-lock-keyword-face) colon
                     (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                     (propertize "Author" 'face 'font-lock-keyword-face) colon
                     (propertize author 'face 'font-lock-string-face)
                     (propertize "Date" 'face 'font-lock-keyword-face) colon
                     (propertize date 'face 'font-lock-string-face))
             (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
             message
             (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
        (git-messenger:format-detail vcs commit-id author message)))

    (defun my-git-messenger:popup-message ()
      "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
      (interactive)
      (let* ((vcs (git-messenger:find-vcs))
             (file (buffer-file-name (buffer-base-buffer)))
             (line (line-number-at-pos))
             (commit-info (git-messenger:commit-info-at-line vcs file line))
             (commit-id (car commit-info))
             (author (cdr commit-info))
             (msg (git-messenger:commit-message vcs commit-id))
             (popuped-message (if (git-messenger:show-detail-p commit-id)
                                  (my-git-messenger:format-detail vcs commit-id author msg)
                                (cl-case vcs
                                  (git msg)
                                  (svn (if (string= commit-id "-")
                                           msg
                                         (git-messenger:svn-message msg)))
                                  (hg msg)))))
        (setq git-messenger:vcs vcs
              git-messenger:last-message msg
              git-messenger:last-commit-id commit-id)
        (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
        (git-messenger-hydra/body)
        (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
               (let ((buffer-name "*git-messenger*"))
                 (posframe-show buffer-name
                                :string popuped-message
                                :left-fringe 8
                                :right-fringe 8
                                ;; :poshandler #'posframe-poshandler-window-top-right-corner
                                :poshandler #'posframe-poshandler-window-top-right-corner
                                ;; Position broken with xwidgets and emacs 28
                                ;; :position '(-1 . 0)
                                :y-pixel-offset 20
                                :x-pixel-offset -20
                                :internal-border-width 2
                                :lines-truncate t
                                :internal-border-color (face-foreground 'font-lock-comment-face)
                                :accept-focus nil)
                 (unwind-protect
                     (push (read-event) unread-command-events)
                   (posframe-delete buffer-name))))
              ((and (fboundp 'pos-tip-show) (display-graphic-p))
               (pos-tip-show popuped-message))
              ((fboundp 'lv-message)
               (lv-message popuped-message)
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (lv-delete-window)))
              (t (message "%s" popuped-message)))
        (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
    (advice-add #'git-messenger:popup-close :override #'ignore)
    ;; (advice-add #'git-messenger:popup-close :override #'(setq modal-opened 0))
    (advice-add #'git-messenger:popup-message :override #'my-git-messenger:popup-message)))

;; (require 'evil)
;; (defun idle-commit-message()
;;   (cond
;;    ;; ((eq evil-state 'normal) (funcall 'git-messenger:popup-message)))
;;    ;; ((and (eq evil-state 'normal) (eq modal-opened 0)) (funcall (lambda ()
;;    ;;                                                               ;; (message (int-to-string(modal-opened)))
;;    ;;                                                               (funcall 'git-messenger:popup-message)
;;    ;;                                                               (setq modal-opened 1)
;;    ;;                                                               ))))



;;    ;;                                                               modal-opened not work, now its just a stub
;;    ((and (eq evil-state 'normal) t) (funcall (lambda ()
;;                                                ;; (message (int-to-string(modal-opened)))
;;                                                (funcall 'git-messenger:popup-message)
;;                                                (setq modal-opened 1)
;;                                                ))))
;;   )
