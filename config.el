;; -*- lexical-binding: t; -*-
(add-to-list 'load-path "~/.doom.d/lisp/")

(setq user-full-name "Bryan Paronto"
      user-mail-address "bryan@cablecar.digital")

(setq doom-modeline-height 36)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-height 36)

(setq doom-font
      (font-spec
       :family "VictorMono Nerd Font" :size 20)
      doom-variable-pitch-font
      (font-spec
       :family  "VictorMono Nerd Font" :size 20)
      doom-big-font
      (font-spec
       :family "VictorMono Nerd Font" :size 20))

(setq standard-indent 2)

(after! doom-themes
    (setq doom-themes-enable-bold t
            doom-themes-enable-italic t))
    (custom-set-faces!
            '(font-lock-comment-face :slant italic)
            '(font-lock-keyword-face :slant italic))

(add-hook 'org-mode-hook (lambda ()
   "Beautify Org Checkbox Symbol"
    (setq prettify-symbols-alist
    (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
            '(("#+begin_src" . ?)
                ("#+end_src" . ?ﰵ)
                ("#+begin_example" . ?)
                ("#+end_example" . ?)
                ("#+header:" . ?)
                ("#+name:" . ?﮸)
                ("#+results:" . ?)
                ("#+call:" . ?)
                (":properties:" . ?)
                (":logbook:" . ?))))
   (prettify-symbols-mode)))

(use-package ligature
  :config
  (ligature-set-ligatures
        'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
        ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
        "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
        "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
        "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
        "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
        "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
        "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
        ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
        "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
        "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
        "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
        "\\\\" "://"))
  (global-ligature-mode t))

(setq split-height-threshold nil)
(setq split-width-threshold 160)

(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

(use-package ivy-posframe
  :after (ivy)
  :config
  (setq ivy-posframe-display-functions-alist
       '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

(setq doom-theme 'doom-vibrant)
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'one-dark)
;; (setq doom-theme 'doom-gruvbox-light)
;; (setq doom-theme 'doom-ephemeral)
;; (setq doom-theme 'leuven)
;; (setq doom-theme 'leuven-dark)

(defun bp/fill-visual-column ()
  (setq visual-fill-column-center-text t
        visual-fill-column-width 120)
  (visual-fill-column-mode 1))

(use-package dashboard
  :defer t
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-week-agenda nil)
  (setq dashboard-startup-banner "~/.doom.d/banners/e.png")
  ;; (add-hook! 'window-setup-hook #'treemacs 'append)
  (add-hook! 'dashboard-mode-hook #'bp/fill-visual-column)
  :config
  (dashboard-setup-startup-hook))

(set-frame-parameter (selected-frame)'alpha '(75 . 80))
(set-frame-parameter (selected-frame)'internal-border-width 24)
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(add-to-list 'default-frame-alist '(alpha . (75 . 80)))
(add-to-list 'default-frame-alist '(internal-border-width . 24))

(setq display-line-numbers-type 'relative)

(map! :map org-mode-map
      :localleader
      :desc "Org babel tangle"
      "B" #'org-babel-tangle)

(use-package org-superstar
  ;; :commands (org-mode)
  :config
  (setq org-superstar-item-bullet-alist '((?+ . ?➤) (?- . ?✦)))
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(server-start)
(require 'org-protocol)
(after! org
  (setq
   org-directory "~/org/"
   org-agenda-files (list "inbox.org" "agenda.org")
   org-ellipsis "▼"
   org-default-notes-file (expand-file-name
                           "notes.org" org-directory)
   org-log-done 'time
   org-hide-emphasis-markers t
   org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo   . " ")
     (tags   . " %i %-12:c")
     (search . " %i %-12:c"))
   org-capture-templates
   `(("i" "Inbox" entry  (file+headline "inbox.org" "Inbox")
      ,(concat "* TODO %?\n"
               "/Entered on/ %U"))

     ;;
     ("z" "zettle" plain (file+head "brain/umami/%<%Y%m%d%H%M%S>-${slug}.org"
                                        "#+title: ${title}\n#+date: %U\n%i\n"))

     ("p" "Protocol" entry (file+headline "inbox.org" "Inbox")
       "* %a\nCaptured On: %U\nWebsite: %l\n\n%i\n%?"
        )

	 ("L" "Protocol Link" entry (file+headline "inbox.org" "Links to Checkout")
        "* %? [[%:link][%:description]] \nCaptured On: %U")

     ;; Adds a new medical appointment to the Agenda
     ("d" "Drs Appointment" entry  (file+headline "agenda.org" "Future")
      ,(concat "* TODO %? :Health:appointment:\n"
               "<%<%Y-%m-%d %a %H:00>>"))

     ;; One-off fleeting notes and reminders go here.
     ("n" "Note" entry  (file "notes.org")
      ,(concat "* Note (%a)\n"
               "/Entered on/ %U\n" "\n" "%?"))
     ;; Use as part of email workflow to schedule replies
     ("@" "Inbox [mu4e]" entry (file "inbox.org")
      ,(concat "* TODO Reply to \"%a\" %?\n"
               "/Entered on/ %U")))
   ))

(use-package! org-super-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-super-agenda-header-separator "\n"
        ;; org-agenda-block-separator nil
        ;; org-agenda-compact-blocks nil
        org-agenda-start-day nil
        org-agenda-span 10
        org-super-agenda-hide-empty-groups nil
        org-agenda-start-on-weekday nil)
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
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "BLOCKED(b)" "|" "DONE(d)")
          (type "[ ](c)" "PROJ(p)" "SOMEDAY(s)" "LOOP(r)" "|" "[x](x)")
          (sequence "|" "CANCELLED")))
  :config
  (org-super-agenda-mode t))

(defun bp/org-roam-goto-month ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("m" "month" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "temporal/monthly/%<%Y-%B>.org"
                                                      "#+title: %<%Y-%B>\n#+filetags: monthly\n")
                                   :unnarrowed t))))

(defun bp/org-roam-goto-year ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("y" "year" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "temporal/yearly/%<%Y>.org"
                                                      "#+title: %<%Y>\n#+filetags: yearly\n")
                                   :unnarrowed t))))
(after! org
  (setq
   org-roam-v2-ack t
   bp/daily-note-filename "%<%Y-%m-%d>.org"
   bp/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n"
   org-roam-directory "~/org/brain"
   org-roam-dailies-directory "temporal/daily/"
   org-roam-completion-everywhere t
   org-roam-capture-ref-templates '(("l" "Web site" plain (function org-roam-capture--get-point)
                                         "${body}\n%?"
                                         :file-name "sources/${slug}"
                                         :head "#+title: ${title}\n#+CREATED: %U\n#+roam_key: ${ref}\n\n"
                                         :unnarrowed t))
   org-roam-capture-templates
   `(

     ("w" "default" entry ""
      :if-new (file+head "umami/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: \n\nTags::  "))


     ("s" "source" entry ""
      :if-new (file+head "sources/${slug}.org" "#+title: ${title}\n#+filetags: #📚 #sources/???a\n\nTags::\n\nAuthor:: \nSource:  "))

     ("m" "map of content" entry
      "* %?"
      :if-new (file+head "maps/${slug}-moc.org"
                         "#+title: ${title}\n#+filetags: #🗺️ #moc \n\n"))
     )
   org-roam-dailies-capture-templates
   `(

     ("t" "task" entry
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
                             ("Log")))

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
  ("c" nil "cancel"))

(map! :leader
      :prefix ("n" "notes")
      :desc "Journal Manager"
      "j" #'bp/org-roam-jump-menu/body)

(add-hook! org-mode #'bp/fill-visual-column)

(defun bp/org-font-setup ()
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.4 ))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.3 ))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.2 ))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.1 ))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0 ))))
   ))

(add-hook! 'org-mode-hook #'bp/org-font-setup)

(defun bp/presentation-mode ()
    (setq-local face-remapping-alist '(
    (header-line (:height 2.5) variable-pitch)
    (org-document-title (:height 1.75) org-document-title)
    (org-code (:height 1.55) org-code)
    (org-verbatim (:height 1.55) org-verbatim)
    (org-block (:height 1.25) org-block)
    (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-appear-mode -1)
  (org-image-actual-width nil)
  (org-display-inline-images)
  (org-tree-slide-header nil)
  (display-line-numbers-mode nil))

(use-package org-tree-slide
  :custom
  (add-hook! org-tree-slide-mode 'bp/presentation-mode)
  )

(map! :leader
      :prefix ("t" . "toggle")
      :desc "Toggle file tree"
      "t" #'treemacs)

(map! :leader
      :desc "Toggle comment"
      "j" #'comment-line)

(map! :leader
      :prefix ("b" . "buffers")
      :desc "Switch Buffers"
      "w" #'ivy-switch-buffer-other-window)

(map! :leader
      :prefix ("h". "help")
      :desc "Switch Themes"
      "t" #'counsel-load-theme)

(use-package! elfeed-goodies
  :init
  (setq elfeed-goodies/entry-pane-size 0.5)
  (evil-define-key 'normal elfeed-show-mode-map
    (kbd "J") 'elfeed-goodies/split-show-next
    (kbd "K") 'elfeed-goodies/split-show-prev)
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "J") 'elfeed-goodies/split-show-next
    (kbd "K") 'elfeed-goodies/split-show-prev)
  (setq elfeed-feeds (quote
                      (("https://www.reddit.com/r/linux.rss" reddit linux)
                       ("https://www.reddit.com/r/unixporn.rss" reddit unixporn)
                       ("https://www.reddit.com/r/commandline.rss" reddit commandline)
                       ("https://www.reddit.com/r/neovim.rss" reddit neovim)
                       ("https://www.reddit.com/r/vim.rss" reddit vim)
                       ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
                       ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                       ("https://www.reddit.com/r/orgmode.rss" reddi torgmode)
                       ("https://www.reddit.com/r/planetemacs.rss" reddit emacs)
                       ("https://www.reddit.com/r/doomemacs.rss" reddit doomemacs)
                       ("https://www.reddit.com/r/archlinux.rss" reddit emacs)
                       )))
  :config
  (add-hook 'elfeed-show-mode-hook 'visual-line-mode)
  (elfeed-goodies/setup))

(defun bp/prefer-horizontal-split ()
 (set-variable 'split-height-threshold nil t)
 (set-variable 'split-width-threshold 40 t))
(add-hook! 'markdown-mode-hook #'bp/prefer-horizontal-split)
(map! :leader
     :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)

(defvar bp/mu4e-account-alist
 '(
   ("bparonto@gmail.com"
    (user-mail-address  "bparonto@gmail.com")
    (user-full-name     "Bryan Paronto")
    (mu4e-sent-folder   "~/Mail/bparonto@gmail.com/Sent Items")
    (mu4e-drafts-folder "~/Mail/bparonto@gmail.com/Drafts")
    (mu4e-trash-folder  "~/Mail/bparonto@gmail.com/Deleted Items")
    (mu4e-refile-folder "~/Mail/bparonto@gmail.com/Archive"))

  ;; TODO Edit Gsuite admin config to allow
  ("bryan@cablecar.digital"
    (user-mail-address  "bryan@cablecar.digital")
    (user-full-name     "Bryan Paronto")
    (mu4e-sent-folder   "~/Mail/bryan@cablecar.digital/Sent Items")
    (mu4e-drafts-folder "~/Mail/bryan@cablecar.digital/Drafts")
    (mu4e-trash-folder  "~/Mail/bryan@cablecar.digital/Deleted Items")
    (mu4e-refile-folder "~/Mail/bryan@cablecar.digital/Archive"))
   ))

(setq mu4e-user-mail-address-list
      (mapcar (lambda (account) (cadr (assq 'user-mail-address account)))
            bp/mu4e-account-alist))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil))

;; (use-package! svg-tag-mode
  ;; ;; :commands (svg-tag-mrde)
  ;; :config
  ;; (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  ;; (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  ;; (defconst day-re "[A-Za-z]\\{3\\}")

  ;; (defun svg-progress-percent (value)
  ;;   (svg-image (svg-lib-concat
  ;;               (svg-lib-progress-bar (/ (string-to-number value) 100.0)
  ;;                                     nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
  ;;               (svg-lib-tag (concat value "%")
  ;;                            nil :stroke 0 :margin 0)) :ascent 'center))

  ;; (defun svg-progress-count (value)
  ;;   (let* ((seq (mapcar #'string-to-number (split-string value "/")))
  ;;          (count (float (car seq)))
  ;;          (total (float (cadr seq))))
  ;;     (svg-image (svg-lib-concat
  ;;                 (svg-lib-progress-bar (/ count total) nil
  ;;                                       :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
  ;;                 (svg-lib-tag value nil
  ;;                              :stroke 0 :margin 0)) :ascent 'center)))

  ;; (setq svg-tag-tags
  ;;       `(
  ;;         ;; Org tags
  ;;         (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))

  ;;         (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

  ;;         ;; Task priority
  ;;         ("\\[#[A-Z]\\]" . ( (lambda (tag)
  ;;                               (svg-tag-make tag :face 'org-priority
  ;;                                             :beg 2 :end -1 :margin 0))))

  ;;         ;; Progress
  ;;         ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
  ;;                                             (svg-progress-percent (substring tag 1 -2)))))
  ;;         ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
  ;;                                           (svg-progress-count (substring tag 1 -1)))))

  ;;         ;; TODO / DONE
  ;;         ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
  ;;         ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


  ;;         ;; Citation of the form [cite:@Knuth:1984]
  ;;         ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
  ;;                                           (svg-tag-make tag
  ;;                                                         :inverse t
  ;;                                                         :beg 7 :end -1
  ;;                                                         :crop-right t))))
  ;;         ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
  ;;                                                    (svg-tag-make tag
  ;;                                                                  :end -1
  ;;                                                                  :crop-left t))))


  ;; )

;; (use-package doct
;;   :commands (doct))

(add-hook! 'after-init-hook
  (lambda ()  (setq doom-modeline-height 36
                    doom-modeline-bar-width 6
                    doom-modeline-lsp t
                    doom-modeline-github t
                    doom-modeline-mu4e t
                    doom-modeline-irc t
                    doom-modeline-minor-modes nil
                    doom-modeline-persp-name nil
                    doom-modeline-buffer-file-name-style 'truncate-except-project
                    doom-modeline-major-mode-icon nil)
    (custom-set-faces '(mode-line ((t (:height 0.85))))
                      '(mode-line-inactive ((t (:height 0.85)))))))

(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))
