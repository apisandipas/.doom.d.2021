;; -*- lexical-binding: t; -*-
;; Customizations for org-roam

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
        (org-roam-capture-templates (list (append (cdr org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))



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
  (use-package! emacsql-sqlite-builtin)
  (require 'org-roam-protocol)
  (setq org-roam-database-connector 'sqlite-builtin)
  (setq
   org-gcal-recurring-events-mode 'nested
   org-roam-v2-ack t
   bp/daily-note-filename "%<%Y-%m-%d>.org"
   org-roam-directory "~/Dropbox/org/brain"
   org-roam-dailies-directory "temporal/daily/"
   org-roam-completion-everywhere t
   )
  ;;  ;; TODO: Templates?
   (setq org-roam-capture-ref-templates `(("r" "default" plain
                                           ,(concat
                                             "#+title: ${title}\n"
                                             "#+filetags: sources/??\n\n"
                                             "Topics::\n"
                                             "Author::\n"
                                             "Related::\n\n"
                                             "* ${title}\n"
                                             "** Content\n\n"
                                             "#+begin_quote\n ${body}\n #+end_quote \n\n "
                                             "** Summary\n\n "
                                             "%?\n\n")
                                           :if-new (file "sources/%<%Y%m%d%H%M%S>-${slug}.org")
                                           :unnarrowed t)))

   (setq org-roam-capture-templates `(("w" "zettlekasten" plain
                                       "#+title: ${title}\n#+filetags: \n\nTopics ::  \n\n"
                                       :if-new (file "umami/%<%Y%m%d%H%M%S>-${slug}.org"))

                                      ("t" "topic-node" plain
                                       ,(concat "#+title: ${title}\n#+filetags: topic-node \n\n"
                                               "Related Topics:: [[id:d00d3b40-5891-4761-978b-9aef8a6471a2][Topics MOC]] "
                                               " %? %? \n\n")
                                               :if-new (file "topics/topic-${slug}.org"))

                                      ("c" "class notes" plain
                                       "#+title: ${title}\n#+filetags: course-notes \n\nClass :: %? \nWebsite ::  \nRelated ::\n\n"
                                       :if-new (file "umami/%<%Y%m%d%H%M%S>-${slug}.org"))

                                      ("s" "source" plain
                                       "#+title: ${title}\n#+filetags: source/%? \n\nTopics :: \nAuthor :: \nPublisher :: \nPublished :: \nURL :: \nRelated ::\n\n"
                                       :if-new (file "sources/%<%Y%m%d%H%M%S>-${slug}.org"))

                                      ("m" "map of content" plain "#+title: ${title}\n#+filetags: moc \n\nTopics :: %? \n\n"
                                       :if-new (file "maps/${slug}-moc.org"))))
     )

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


(use-package org-ref
  :config
  (setq
   bibtex-completion-bibliography '("~/Dropbox/org/bibtex.bib")
   bibtex-completion-notes-path "~/Dropbox/org/brain/umami"
   bibtex-completion-pdf-field "file"
   bibtex-completion-pdf-open-function
   (lambda (fpath)
     (call-process "firefox" nil 0 nil fpath))))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))


(provide 'bp-roam)
