;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Artur Yaroshenko"
      user-mail-address "artawower@mail.ru")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-palenight)
;; (setq doom-theme 'zerodark)
;; (load-theme 'doom-spacegrey t)
;; (setq doom-theme 'doom-one)
;; (load-theme 'atom-one-dark t)
(setq doom-theme 'atom-one-dark)
;; (use-package zerodark-theme
;;   :ensure t)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Yandex.Disk.localized/org/")
(setq org-agenda-files '("~/Yandex.Disk.localized/org/articles"))
(setq org-agenda-files (directory-files-recursively "~/Yandex.Disk.localized/org/" "\\.org$"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                      LOAD CONFIGS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'esup) ;; Profiling

;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

(load! "./configs/flycheck.el")
(load! "./configs/sass.el")
(load! "./configs/modeline.el")
(load! "./configs/bookmarks.el")
(load! "./configs/treemacs.el")
(load! "./configs/fonts.el")
(load! "./configs/angular2.el")
(load! "./configs/git.el")
(load! "./configs/javascript.el")
(load! "./configs/prettier.el")
(load! "./configs/snippets.el")
(load! "./configs/indent.el")
(load! "./configs/lsp.el")
(load! "./configs/evil.el")
(load! "./configs/emmet.el")
(load! "./configs/go.el")
(load! "./configs/devops.el")
(load! "./configs/debug.el")
(load! "./configs/folding.el")
(load! "./configs/plantuml.el")
(load! "./configs/ivy.el")
(load! "./configs/http.el")
;; ;; (load! "./configs/vue-mode.el") ;; Not work anyway ;(
(load! "./configs/term.el")
;; ;; (load! "./configs/color-picker.el")
;; ;; (load! "./configs/neotree.el")
;; ;; (load! "./configs/email.el")
(load! "./configs/ejira.el")
(load! "./configs/dired.el")
(load! "./configs/python.el")
(load! "./configs/telega.el")

(load! "./configs/vue.el")

(use-package wakatime-mode
  :defer t
  :init
  (global-wakatime-mode)
  )
(load! "./configs/orgs.el")

(use-package jenkinsfile-mode
  :defer t
  :init
  (jenkinsfile-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                      KEYMAPS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITOR NAVIGATION


(global-set-key (kbd "C-S-k") 'shrink-window)
(global-set-key (kbd "C-S-j") 'enlarge-window)
(global-set-key (kbd "C-S-l") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-h") 'shrink-window-horizontally)

;; Debug

(define-key global-map [f5] 'dap-breakpoint-toggle)
(define-key global-map [f10] 'dap-next)
(define-key global-map [f12] 'dap-start-debugging)


(global-set-key (kbd "s-e") 'emmet-expand-line)
(global-set-key (kbd "C-s") 'save-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                      Tempoerary test package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strange package for monitoring your keypress activity (useless)
;; (require 'keyfreq)
;; (keyfreq-mode 1)
;; (keyfreq-autosave-mode 1)

;; SCSS AUTO FORMAT
;; (defun revert-buffer-no-confirm ()
;;     "Revert buffer without confirmation."
;;     (interactive)
;;     (revert-buffer :ignore-auto :noconfirm))

;; (defun run-sass-auto-fix ()
;;   (let ((default-directory (file-name-directory buffer-file-name)))
;;   (shell-command "sass-lint-auto-fix")
;;   (revert-buffer-no-confirm)
;;   ))

;; (add-hook 'scss-mode-hook '(lambda () (add-hook 'after-save-hook #'run-sass-auto-fix t t)))
;; (counsel-org-agenda-headlines (kbd "C-S-h") 'shrink-window-horizontally)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                       VISUAL
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

;; (set-frame-parameter (selected-frame) 'alpha '(45 45))
;; (add-to-list 'default-frame-alist '(alpha 45 45))
;; (set-face-attribute 'default nil :background "black"
;;                     :foreground "white" :font "Courier" :height 180)
;; (set-frame-parameter (selected-frame) 'alpha '(45 . 45))
;; (add-to-list 'default-frame-alist '(alpha . (45 . 45)))
;; (set-face-attribute 'default nil :background "white"
;;     :foreground "black" :font "Ligamonacop Nerd Font" :height 180)
;; (set-window-margins nil 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                      UI
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setenv "PERL5LIB" (concat "~/perl5/lib/perl5" ":"
			   (getenv "PERL5LIB")))

(setenv "LC_ALL" "en_US.UTF-8")

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
