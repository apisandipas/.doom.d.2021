;; -*- lexical-binding: t; -*-

(map! :map org-mode-map
      :localleader
      :desc "Org babel tangle"
      "B" #'org-babel-tangle)

(use-package! org-auto-tangle
  :defer t
  :config
  (org-auto-tangle-mode 1))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-trigger 'always
        org-appear-delay 1.0
        org-appear-autolinks t)

  (org-appear-mode 1))


(use-package! org-mobile-sync
  :config
  (setq org-mobile-inbox-for-pull "~/org/from-mobile.org")
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg/")
  (org-mobile-sync-mode 1))



;;; Customize org-mode font setup
;;; TODO: Move these to theme file
(defun bp/org-font-setup ()
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.25 ))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.2 ))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.1 ))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0 ))))))

(add-hook! org-mode #'bp/org-font-setup)

;;;;; Ligatures & Pretty Symbols
(defun bp/org-prettify-symbols ()
  "Beautify Org Checkbox Symbol"
  (setq prettify-symbols-alist
        (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                '(("#+begin_src" . ?)
                  ("#+end_src" . ?)
                  ("#+begin_example" . ?)
                  ("#+end_example" . ?)
                  ("#+begin_quote" . ?)
                  ("#+end_quote" . ?)
                  (":END:" . ?)
                  ("#+header:" . ?)
                  ("#+name:" . ?﮸)
                  ("#+results:" . ? )
                  ("#+call:" . ? )
                  (":properties:" . ? )
                  (":logbook:" . ? ))))
  (prettify-symbols-mode))

(add-hook! org-mode #'bp/org-prettify-symbols)

(use-package! org-protocol-capture-html
  :after org)

;;; NOTE: this is the only way in which i could get this working
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;;; Org Mode config
(after! org
  (require 'ob-typescript)
  (require 'org-protocol)
  (require 'org-protocol-capture-html)
  (setq org-directory "~/org/"
        org-agenda-files (list "inbox.org" "agenda.org" )
        org-ellipsis " ▼"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-log-done 'time
        org-tags-column -80
        org-hide-emphasis-markers t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((typescript . t)
     ))

  (setq org-agenda-custom-commands
        '(("c" . "📦 Contexts")
          ("ch" "🏥 Health" tags-todo "Health")
          ("cs" "🧔🏼‍♂️ Self" tags-todo "Self")
          ("cl" "💏 Love" tags-todo "Love")
          ("cH" "🏠 Home" tags-todo "Home")
          ("cC" "🏙 Community" tags-todo "Community")
          ("A" "📅 Appointments" tags-todo "appointment")
          ("B" "💸 Bills" tags-todo "bill")
          ("C" "💦 Chores" tags-todo "chore")
          ("g" "👷🏼‍ GTD"
           ((todo "Health")
            (todo "Self")
            (todo "Love")
            (todo "Home")
            (todo "Community")))
          ("f" "💎 FACETS"
           ((tags-todo "Health")
            (tags-todo "Self")
            (tags-todo "Love")
            (tags-todo "Home")
            (tags-todo "Community")))))


  (setq org-tag-alist '(("URGENT" . ?u)
                        ("Health" . ?h)
                        ("Self" . ?s)
                        ("Love" . ?l)
                        ("Home" . ?h)
                        ("Family" . ?f)
                        ("Community" . ?c)
                        ("project" . ?P)
                        ("task" . ?T)
                        ("chore" . ?C)
                        ("bill" . ?B)
                        ("appointment" . ?A)
                        ))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "BLOCKED(b)" "LOOP(l)" "|" "CANCELLED(C)" "|" "DONE(d)")
          (type "[ ](c)" "PROJ(p)" "SOMEDAY(s)" "LOOP(r)" "|" "[x](x)")))


  (setq org-capture-templates
        ;; Generic todo entry
        `(("t" "Todo" entry (file+headline "inbox.org" "Todos Inbox")
           ,(concat "* [ ] %?\n"
                    "/Entered on/ %U"))

          ;; User by Org protocol to capture a note to the inbox
          ("p" "Protocol" entry (file+headline "inbox.org" "Links Inbox")
           "* %a\nCaptured On: %U\nWebsite: %l\n\n%i\n%?")

          ;; One-off fleeting notes go here.
          ("n" "Note" entry  (file+headline "inbox.org" "Notes Inbox")
           ,(concat "* Note (%a)\n"
                    "/Entered on/ %U\n" "\n" "%?"))

          ;; Used by org protocol to capture a link to the inbox
          ("L" "Protocol Link" item (file+headline "inbox.org" "Links Inbox")
           "%? [[%:link][%:description]] \nCaptured On: %U")

          ;; These come in from Nyxt. It can probably be merged with the Protocol Link one above.
          ;; TODO: Consider merging these two use cases.
          ("w" "Web link" item (file+headline "inbox.org" "Links Inbox")
           "* %?%a\n:SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n %?")

          ;; Adds a new medical appointment to the Agenda
          ("a" "Appointment" item  (file+headline "agenda.org" "Future")
           ,(concat "* APPT %? :appointment:\n"
                    "<%<%Y-%m-%d %a %H:00>>"))

          ;; Use as part of email workflow to schedule replies
          ("@" "Inbox [mu4e]" entry (file+headline "inbox.org" "Emails To Process")
           ,(concat "* TODO Reply to \"%a\" %?\n"
                    "/Entered on/ %U")))))

(use-package org-superstar
  :config
  (setq org-superstar-item-bullet-alist '((?+ . ?➤) (?- . ?✦)))
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package! org-super-agenda
  :commands org-super-agenda-mode
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-super-agenda-header-separator "\n"
        org-agenda-start-day nil
        org-agenda-span 10
        org-super-agenda-hide-empty-groups nil
        org-agenda-start-on-weekday nil))

(use-package! org-gcal
  :commands (org-gcal-sync org-gcal--sync-unlock org-gcal-post-at-point)
  :init
  (setq! org-gcal-auto-archive nil)
  (setq org-gcal-client-id "1057193299633-eimgu3bm260jkisachubpf8oj1cah5nj.apps.googleusercontent.com"
        org-gcal-client-secret (lambda (&rest _) (+pass-get-secret "Google/Calendar/Secret"))
        org-gcal-fetch-file-alist '(
                                    ("bparonto@gmail.com" .  "~/org/agenda.org")
                                    ("4tc3t9c2hef41n7dc461idql8k@group.calendar.google.com". "~/org/agenda.org"))))

  ;; (set
  ;;  ;; Edit settings
  ;;  org-auto-align-tags nil
  ;;  org-tags-column 0
  ;;  ;; org-fold-catch-invisible-edits 'show-and-error
  ;;  ;; org-special-ctrl-a/e t
  ;;  ;; org-insert-heading-respect-content t

  ;;  ;; ;; Org styling, hide markup etc.
  ;;  ;; org-hide-emphasis-markers t
  ;;  ;; org-pretty-entities t
  ;;  org-ellipsis "…"

  ;;  ;; Agenda styling
  ;;  org-agenda-tags-column 0
  ;;  org-agenda-block-separator ?─
  ;;  org-agenda-time-grid
  ;;  '((daily today require-timed)
  ;;    (800 1000 1200 1400 1600 1800 2000)
  ;;    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
  ;;  org-agenda-current-time-string
  ;;  "⭠ now ─────────────────────────────────────────────────")




(provide 'bp-org)
