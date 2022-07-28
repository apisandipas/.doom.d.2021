;; -*- lexical-binding: t; -*-
;; Customizations for org-roam

(defun bp/org-roam-goto-today ()
  "Go to the current days temporal note"
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%m-%d")) '(4))
                     :node (org-roam-node-create)
                     :templates `(("d" "daily" plain "\n\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "temporal/daily/%<%Y-%B>.org"
                                                      ,(bp/org-roam/daily-note-header))
                                   :unnarrowed t))))


(defun bp/org-roam-goto-month ()
  "Go to the current months temporal note"
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("m" "month" plain "\n\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "temporal/monthly/%<%Y-%B>.org"
                                                      "#+title: %<%Y-%B>\n#+filetags: #temporal/monthly\n\n")
                                   :unnarrowed t))))

(defun bp/org-roam-goto-year ()
  "Go to the current years temporal note"
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("y" "year" plain "\n\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "temporal/yearly/%<%Y>.org"
                                                      "#+title: %<%Y>\n#+filetags: #temporal/yearly\n\n")
                                   :unnarrowed t))))


(defun bp/org-roam-node-insert-immediate (arg &rest args)
  "Create a node via the prompt and then insert the link without visiting it."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun bp/org-roam-topic-node-insert-immediate (arg &rest args)
  "Create a topic node via the prompt and then insert the link without visiting it."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (second org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun bp/org-roam/get-yearly-note-link ()
  "Get the link for the current years temporal note, otherwise create it and return its link"
  (let* ((formatted-date (format-time-string "%Y"))
         (node (org-roam-node-from-title-or-alias (format-time-string formatted-date))))
    (if node
        (org-link-make-string
         (concat
          "id:" (org-roam-node-id node))
         (org-roam-node-title node))
      (progn
        (org-roam-capture- :goto nil
                           :node (org-roam-node-create)
                           :templates '(("y" "year" plain "\n\n* Goals\n\n%?* Summary\n\n"
                                         :if-new (file+head "temporal/yearly/%<%Y>.org"
                                                            "#+title: %<%Y>\n#+filetags: #temporal/yearly\n\n")
                                         :immediate-finish t
                                         :unnarrowed t)))
        (bp/org-roam/get-yearly-note-link)))))

(defun bp/org-roam/get-monthly-note-link ()
  "Get the link for the current months temporal note, otherwise create it and return its link"
  (let* ((formatted-date (format-time-string "%Y-%B"))
         (node (org-roam-node-from-title-or-alias (format-time-string formatted-date))))
    (if node
        (org-link-make-string
         (concat
          "id:" (org-roam-node-id node))
         (org-roam-node-title node))
      (progn
        (org-roam-capture- :goto nil
                           :node (org-roam-node-create)
                           :templates `(("m" "month" plain "\n\n* Goals\n\n%?* Summary\n\n"
                                         :if-new (file+head "temporal/monthly/%<%Y-%B>.org"
                                                            ,(bp/org-roam/monthly-note-header))
                                         :immediate-finish t
                                         :unnarrowed t)))
        (bp/org-roam/get-monthly-note-link)))))

(defun bp/org-roam/daily-note-header ()
  "Geneate the monthly note link for daily note header"
  (concat "#+title: %<%Y-%m-%d %a>\n\n"
          "Parent :: "
          (bp/org-roam/get-monthly-note-link)
          "\n\n"))

(defun bp/org-roam/monthly-note-header ()
  "Geneate the monthly note header"
  (concat "#+title: %<%Y-%B>\n"
          "#+filetags: #temploral/monthly\n\n"
          "Parent :: "
          (bp/org-roam/get-yearly-note-link)
          "\n\n"))

(map! :leader
      :prefix ("nr" "notes")
      :desc "Insert node immediately"
      "I" #'bp/org-roam-node-insert-immediate)

(map! :leader
      :prefix ("nr" "notes")
      :desc "Insert topic node immediately"
      "T" #'bp/org-roam-topic-node-insert-immediate)

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam org-roam-show-graph)
  :init
  (require 'org-roam-protocol)
  (setq
   org-gcal-recurring-events-mode 'nested
   org-roam-v2-ack t
   bp/daily-note-filename "%<%Y-%m-%d>.org"
   org-roam-directory "~/org/brain"
   org-roam-dailies-directory "temporal/daily/"
   org-roam-completion-everywhere t
   ;; TODO: Templates?
   org-roam-capture-ref-templates `(("r" "default" plain
                                     ,(concat
                                       "#+title: ${title}\n"
                                       "#+filetags: #sources/??\n\n"
                                       "Topics::\n"
                                       "Author::\n"
                                       "Related::\n\n"
                                       "* ${title}\n"
                                       "** Content\n\n"
                                       "#+begin_quote\n ${body}\n #+end_quote \n\n "
                                       "** Summary\n\n "
                                       "%?\n\n")
                                     :if-new (file "sources/%<%Y%m%d%H%M%S>-${slug}.org") :unnarrowed t))
   org-roam-capture-templates `(("w" "zettlekasten" plain
                                 "#+title: ${title}\n#+filetags: \n\nTopics ::  \n\n"
                                 :if-new (file "umami/%<%Y%m%d%H%M%S>-${slug}.org"))

                                ("t" "topic-node" plain
                                 "#+title: ${title}\n#+filetags: #topic-node \n\nRelated Topics :: %? \n\n"
                                 :if-new (file "topics/%<%Y%m%d%H%M%S>-topic-${slug}.org"))

                                ("s" "source" plain
                                 "#+title: ${title}\n#+filetags: #source/%? \n\nTopics:: \nAuthor::  \nRelated ::\n\n"
                                 :if-new (file "sources/%<%Y%m%d%H%M%S>-${slug}.org"))

                                ("m" "map of content" plain "#+title: ${title}\n#+filetags: #moc \n\nTopics :: %? \n\n"
                                 :if-new (file "maps/${slug}-moc.org")))
   org-roam-dailies-capture-templates `(("t" "task" entry
                                         "\n\n* TODO %?\n  %U\n  %a\n  %i"
                                         :if-new (file+head+olp ,bp/daily-note-filename
                                                                ,(bp/org-roam/daily-note-header)
                                                                ("Tasks"))
                                         :empty-lines 1)

                                        ("l" "log entry" entry
                                         "\n\n* %<%I:%M %p> - %?\n\n"
                                         :if-new (file+head+olp ,bp/daily-note-filename
                                                                ,(bp/org-roam/daily-note-header)
                                                                ("Log"))
                                         :empty-lines 1)
                                        ("j" "journal" entry
                                         "\n\n* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
                                         :if-new (file+head+olp ,bp/daily-note-filename
                                                                ,(bp/org-roam/daily-note-header)
                                                                ("Log"))
                                         :empty-lines 1)

                                        ("a" "appointment" entry
                                         "\n\n* %<%I:%M %p> - %^{Meeting Title}  :appointment:\n\n%?\n\n"
                                         :if-new (file+head+olp ,bp/daily-note-filename
                                                                ,(bp/org-roam/daily-note-header)
                                                                ("Log"))
                                         :empty-lines 1))))

(defhydra bp/org-roam-jump-menu (:exit t)
  "
^Jump To Day^    ^Capture^       ^Periodic^
^^^^^^^^-------------------------------------------------
_t_: today       _T_: today       _m_: current month
_r_: tomorrow    _R_: tomorrow    _e_: current year
_y_: yesterday   _Y_: yesterday   ^ ^
_d_: date        ^ ^              ^ ^
"
  ("t" bp/org-roam-goto-today)
  ("r" org-roam-dailies-goto-tomorrow)
  ("y" org-roam-dailies-goto-yesterday)
  ("d" org-roam-dailies-goto-date)
  ("T" org-roam-dailies-capture-today)
  ("R" org-roam-dailies-capture-tomorrow)
  ("Y" org-roam-dailies-capture-yesterday)
  ("m" bp/org-roam-goto-month)
  ("e" bp/org-roam-goto-year)
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
