;; -*- lexical-binding: t; -*-
;; Customizations for org-roam

(defun bp/org-roam-goto-month ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("m" "month" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "temporal/monthly/%<%Y-%B>.org"
                                                      "#+title: %<%Y-%B>\n#+filetags: #temporal/monthly\n")
                                   :unnarrowed t))))

(defun bp/org-roam-goto-year ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("y" "year" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "temporal/yearly/%<%Y>.org"
                                                      "#+title: %<%Y>\n#+filetags: #temporal/yearly\n")
                                   :unnarrowed t))))


(defun bp/org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))


(map! :leader
      :prefix ("n" "notes")
      :desc "Insert note immediately"
      "rI" #'bp/org-roam-node-insert-immediate)

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam org-roam-show-graph)
  :init
  (require 'org-roam-protocol)
  (setq
   org-gcal-recurring-events-mode 'nested
   org-roam-v2-ack t
   bp/daily-note-filename "%<%Y-%m-%d>.org"
   bp/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n"
   org-roam-directory "~/org/brain"
   org-roam-dailies-directory "temporal/daily/"
   org-roam-completion-everywhere t
   org-roam-capture-ref-templates '(("r" "default" plain
                                     "#+title: ${title}\n#+filetags: #sources/??\n\nTopics::\nAuthor::\nRelated::\n\n* ${title}\n** Content\n\n #+begin_quote\n ${body}\n #+end_quote \n\n** Summary\n\n %?\n\n"
                                     :if-new (file "sources/%<%Y%m%d%H%M%S>-${slug}.org") :unnarrowed t))
   org-roam-capture-templates `(("w" "zettlekasten" plain "#+title: ${title}\n#+filetags: \n\nTopics::  \n\n"
                                 :if-new (file "umami/%<%Y%m%d%H%M%S>-${slug}.org"))

                                ("t" "topic-node" plain "#+title: ${title}\n#+filetags: #topic-node \n\nTopics:: %? \n\n"
                                 :if-new (file "umami/%<%Y%m%d%H%M%S>-topic-${slug}.org"))

                                ("s" "source" plain
                                 "#+title: ${title}\n#+filetags: #source/%? \n\nTopics:: \nAuthor::  \nRelated::\n\n"
                                 :if-new (file "sources/%<%Y%m%d%H%M%S>-${slug}.org"))

                                ("m" "map of content" plain "#+title: ${title}\n#+filetags: #moc \n\nTopics:: %? \n\n"
                                 :if-new (file "maps/${slug}-moc.org")))
   org-roam-dailies-capture-templates `(("t" "task" entry
                                         "* TODO %?\n  %U\n  %a\n  %i"
                                         :if-new (file+head+olp ,bp/daily-note-filename
                                                                ,bp/daily-note-header
                                                                ("Tasks"))
                                         :empty-lines 1)

                                        ("l" "log entry" entry
                                         "* %<%I:%M %p> - %?\n\n"
                                         :if-new (file+head+olp ,bp/daily-note-filename
                                                                ,bp/daily-note-header
                                                                ("Log")))
                                        ("j" "journal" entry
                                         "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
                                         :if-new (file+head+olp ,bp/daily-note-filename
                                                                ,bp/daily-note-header
                                                                ("Log"))
                                         :empty-lines 1)

                                        ("a" "appointment" entry
                                         "* %<%I:%M %p> - %^{Meeting Title}  :appointment:\n\n%?\n\n"
                                         :if-new (file+head+olp ,bp/daily-note-filename
                                                                ,bp/daily-note-header
                                                                ("Log"))))))

(defhydra bp/org-roam-jump-menu (:hint nil)
  "
^Dailies^        ^Capture^       ^Jump^
^^^^^^^^-------------------------------------------------
_t_: today       _T_: today       _m_: current month
_r_: tomorrow    _R_: tomorrow    _e_: current year
_y_: yesterday   _Y_: yesterday   ^ ^
_d_: date        ^ ^              ^ ^
"
  ("t" org-roam-dailies-goto-today)
  ("r" org-roam-dailies-goto-tomorrow)
  ("y" org-roam-dailies-goto-yesterday)
  ("d" org-roam-dailies-goto-date)
  ("T" org-roam-dailies-capture-today)
  ("R" org-roam-dailies-capture-tomorrow)
  ("Y" org-roam-dailies-capture-yesterday)
  ("m" bp/org-roam-goto-month)
  ("e" bp/org-roam-goto-year)
  ;; ("z" bp/new-zettlekasten-entry)
  ("q" nil "cancel"))

(map! :leader
      :prefix ("n" "notes")
      :desc "Journal Manager"
      "j" #'bp/org-roam-jump-menu/body)

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :commands (org-roam-ui-mode)
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(provide 'bp-roam)
