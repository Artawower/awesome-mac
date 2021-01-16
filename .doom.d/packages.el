;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

(package! nvm)

;; syntax/languages
(package! ng2-mode)
(package! lsp-mode)
(package! company-lsp)
(package! company-web)
(package! company-posframe)
(package! bm)
(package! prettier)
;; vue
(package! vue-mode)

;;devops
(package! nginx-mode)
(package! dockerfile-mode)
(package! docker-compose-mode)

;;typescript
(package! typescript-mode)
(package! ng2-mode)

;; navigation
(package! evil-leader)
(package! evil-easymotion)
(package! evil-surround)
;; (package! prettier-js)

;; Some macos fixed
(package! exec-path-from-shell)

;; Markup
(package! pug-mode)
(package! emmet-mode)
(package! ac-emmet)
;; Vue js
(package! vue-mode)

;; JS


(package! company-web)
(package! eglot)
(package! web-mode)


(package! wakatime-mode)


(package! pretty-hydra)


;; visual
(package! posframe)


;; Snippets
(package! yasnippet)


;; Golang
(package! company-go)
(package! go-eldoc)
(package! go-scratch)
(package! go-dlv)
(package! dap-mode)
(package! go-playground)

;; Python
(package! python-mode)
;; (package! jedi)
;; (package! company-jedi)
(package! lsp-pyright)
(package! virtualenvwrapper)
(package! auto-virtualenvwrapper)
(package! flycheck-pycheckers)          ;
(package! lsp-python-ms)
(package! elpy)
;; flycheck
(package! flycheck-posframe)
;; Git
(package! vc-msg)


;; Colloborations
(package! floobits)

;; Database
(package! edbi)

;; Common
(package! eros) ;For right tooltips
(package! origami) ; Package for good folding
(package! restclient)
;; (package! verb)
(package! ripgrep)
;; (package! highlight-indent-guides)
(package! indent-guide)
;; Ivy
(package! ivy-posframe)
(package! helm-posframe)
(package! all-the-icons-ivy-rich)
;; (package! all-the-icons-ivy)

;; Socials
(package! telega)

;; terminal
(package! vterm-toggle)
(package! all-the-icons-dired)

(package! plantuml-mode)
(package! company-restclient)
(package! atom-one-dark-theme)

;; Org mode
;; (package! org-bullets)
(package! org-superstar)
(package! org-pretty-tags)
(package! org-super-agenda :pin "dd0d104c26")
(package! doct)
(package! org-gcal)
;; Email
;; (package! mu4e)
;; (package! mu4e-alert)

;; Jira
;; (package! org-jira)

;; Git
(package! forge)
(package! jiralib2)
;; (package! ssh-agency)
;; (package! keychain-environment)

(package! ejira
  :recipe (:host github :repo "nyyManni/ejira" :files ("*.el") :branch "fix/preparsed-sprint"))
;; Icons for terminal version (not work with doom ;( )
;; (package! icons-in-terminal
;;   :recipe (:host nil :repo "https://github.com/sebastiencs/icons-in-terminal"))

;; Interresting dashboard, but not very pretty
;; (package! mu4e-dashboard
;;   :recipe (:host nil :repo "https://github.com/rougier/mu4e-dashboard"))

;; (package! esup)
(package! jenkinsfile-mode)
