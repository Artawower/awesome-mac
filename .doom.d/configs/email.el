;; (require 'mu4e)
;; (setq mail-user-agent 'mu4e-user-agent)
;; (setq message-send-mail-function   'smtpmail-send-it
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server         "smtp.example.com"
;;       smtpmail-local-domain        "example.com")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)
(require 'mu4e-alert)

(setq mu4e-alert-interesting-mail-query
      (concat
       "flag:unread"
       " AND NOT flag:trashed"
       " AND NOT maildir:"
       "\"/[Gmail].All Mail\""))
(setq mu4e-alert-email-notification-types '(count))
(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))


;; ask for account when composing mail
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(defvar my-mu4e-account-alist
  '(("artawower@gmail.com"
     (user-mail-address  "artawower@gmail.com")
     (user-full-name     "darkawower")
     (mu4e-sent-folder   "/artawower@gmail.com/Sent Items")
     (mu4e-drafts-folder "/artawower@gmail.com/Drafts")
     (mu4e-trash-folder  "/artawower@gmail.com/Trash")
     (mu4e-refile-folder "/artawower@gmail.com/Archive"))
    ("artur.yaroshenko@waveaccess.ru"
     (user-mail-address  "artur.yaroshenko@waveaccess.ru")
     (mu4e-sent-folder   "/artur.yaroshenko@waveaccess.ru/Sent Items")
     (mu4e-drafts-folder "/artur.yaroshenko@waveaccess.ru/Drafts")
     (mu4e-trash-folder  "/artur.yaroshenko@waveaccess.ru/Trash")
     (mu4e-refile-folder "/artur.yaroshenko@waveaccess.ru/Archives"))))

(setq mu4e-user-mail-address-list
      (mapcar (lambda (account) (cadr (assq 'user-mail-address account)))
              my-mu4e-account-alist))

(setq mu4e-maildir "~/.Mail")
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; shortcuts
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/[Gmail].Sent Mail"   . ?s)))

;; something about ourselves
;; (setq
;;  user-mail-address "artawower33@gmail.com"
;;  user-full-name  "Artur Yaroshenko"
;;  mu4e-compose-signature
;;  (concat
;;   "Cheers,\n"
;;   "Blah Man\n"))

;; show images
(setq mu4e-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; convert html emails properly
;; Possible options:
;;   - html2text -utf8 -width 72
;;   - textutil -stdin -format html -convert txt -stdout
;;   - html2markdown | grep -v '&nbsp_place_holder;' (Requires html2text pypi)
;;   - w3m -dump -cols 80 -T text/html
;;   - view in browser (provided below)
(setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")

;; spell check
(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 72)
            (flyspell-mode)))

;; add option to view html message in a browser
;; `aV` in view to activate
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; fetch mail every 10 mins
(setq mu4e-update-interval 600)

;; (use-package mu4e-alert
;;   :ensure t
;;   :after mu4e
;;   :init
;;   (setq mu4e-alert-interesting-mail-query
;;     (concat
;;      "flag:unread maildir:/Exchange/INBOX "
;;      "OR "
;;      "flag:unread maildir:/Gmail/INBOX"
;;      ))
;;   (mu4e-alert-enable-mode-line-display)
;;   (defun gjstein-refresh-mu4e-alert-mode-line ()
;;     (interactive)
;;     (mu4e~proc-kill)
;;     (mu4e-alert-enable-mode-line-display)
;;     )
;;   (run-with-timer 0 60 'gjstein-refresh-mu4e-alert-mode-line)
;; )
