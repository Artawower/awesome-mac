(use-package ejira
  :defer t
  :after org
  :init
  (setq jiralib2-url              "your api"
        jiralib2-auth             'token
        jiralib2-user-login-name  "login"
        jiralib2-token            "token"
        ejira-org-directory       "~/.org-jira"
        ejira-projects            '("sprint" "sprint2")
        ejira-done-states        '("Done")
        ejira-in-progress-states '("In Progress" "In Code Review" "In Retest")
        ejira-high-priorities    '("High" "Highest")
        ejira-low-priorities     '("Low" "None")
        ejira-priorities-alist    '(("Highest" . ?A)
                                    ("High"    . ?B)
                                    ("Medium"  . ?C)
                                    ("Low"     . ?D)
                                    ("Lowest"  . ?E)
                                    ("Low"     . ?E)
                                    ("None"    . ?E))
        ejira-todo-states-alist   '(("To Do"       . 1)
                                    ("In Progress" . 2)
                                    ("Done"        . 3))
        ;; ejira-hourlog-entries 1
        ejira-hourmarking-step 60
        ejira-update-jql-unresolved-fn #'ejira-jql-my-unresolved-project-tickets)

  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)



  (require 'ejira-agenda)


  (add-to-list 'org-agenda-files ejira-org-directory)

  ;; Add an agenda view to browse the issues that
  (org-add-agenda-custom-command
   '("j" "My JIRA issues"
     ((ejira-jql "resolution = unresolved and assignee = currentUser()"
                 ((org-agenda-overriding-header "Assigned to me")))))))
