(add-hook 'telega-load-hook
	(lambda ()
	  (define-key global-map (kbd "C-c t") telega-prefix-map)))

(with-eval-after-load 'telega
  (define-key telega-msg-button-map "k" nil)
  ;; (setq telega-use-tracking-for '(or unmuted mention))
  (telega-notifications-mode))
