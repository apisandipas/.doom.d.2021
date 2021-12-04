(setq user-full-name "Bryan Paronto"
      user-mail-address "bryan@cablcar.digital")

(setq doom-font
      (font-spec
            :family "VictorMono Nerd Font" :size 18 :weight 'medium)
      doom-variable-pitch-font
      (font-spec
            :family  "VictorMono Nerd Font" :size 18)
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
(use-package ligature
  :load-path "path-to-ligature-repo"
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
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(setq doom-theme 'doom-opera)

(use-package dashboard
        :init
        (setq dashboard-set-heading-icons t)
        (setq dashboard-set-file-icons t)
        (setq dashboard-week-agenda nil)
        (setq dashboard-startup-banner "~/.doom.d/banners/gnu.png")
        :config
        (dashboard-setup-startup-hook))

(after! visual-fill-column
  (add-hook! dashboard-mode
    (setq visual-fill-column-center-text t
        visual-fill-column-width 200)
    (visual-fill-column-mode 1)))

(set-frame-parameter (selected-frame)'alpha '(90 . 90))
(add-to-list 'default-frame-alist'(alpha . (90 . 90)))

(setq display-line-numbers-type 'relative)

(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)
;; Enable superstarmode
(use-package org-superstar
  :config
  (setq org-superstar-item-bullet-alist '((?+ . ?➤) (?- . ?✦))))
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(after! org
  (setq org-agenda-files '("~/org")
        org-directory "~/org/"
        org-ellipsis " ▼ "
        org-default-notes-file (expand-file-name
                                "Notes.org" org-directory)
        org-log-done 'time
        org-hide-emphasis-markers t))

(use-package org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-super-agenda-header-separator "\n"
        org-agenda-block-separator nil
        org-agenda-compact-blocks nil
        org-agenda-start-day nil
        org-agenda-span 10
        org-super-agenda-hide-empty-groups nil
        org-agenda-start-on-weekday nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRSS(i)" "BLOCKED(b)" "|" "DONE(d)")
          (type "[ ](c)" "PROJ(p)" "SOMEDAY(s)" "LOOP(r)" "|" "[x](x)")
          (sequence "|" "CANCELED")))
  (setq org-agenda-custom-commands
        '(
          ("c" "💎 FACETS"
           ((alltodo ""
                     ((org-agenda-overriding-header "\n 💎 FACETS ")
                      (org-super-agenda-groups
                       '((:log t)
                         (:name "🏥 Health"
                          :tag "@Health"
                          :order 3)
                         (:name "🕹 Self"
                          :tag "@Self"
                          :order 4)
                         (:name "🫂 Love"
                          :tag "@Love"
                          :order 5)
                         (:name "🏠 Home"
                          :tag "@Home"
                          :order 6)
                         (:name "🤝 Community"
                          :tag "@Community"
                          :order 7)

                         (:discard (:not (:todo "TODO")))))))))

          ("g" " GTD"
           ((alltodo "" ((org-agenda-overriding-header "\n GTD ")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "In Progress"
                             :todo "IN-PROGRESS"
                             :order 1)
                            (:name "Next Up"
                             :todo "NEXT"
                             :scheduled nil
                             :order 2)
                            (:name "Scheduled"
                             :scheduled t
                             :todo t
                             :order 3)
                            (:name "Backlog"
                             :todo "TODO"
                             :scheduled nil
                             :order 4)
                            (:name "Blocked"
                             :todo "BLOCKED"
                             :order 5)
                            (:name "Someday"
                             :todo "SOMEDAY"
                             :order 6)
                            (:name "Repeating Tasks"
                             :todo "LOOP"
                             :scheduled t
                             :order 7)
                            ))))))


          ))
  :config
  (org-super-agenda-mode))

(after! org
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-prefix ""
        org-journal-time-prefix ""
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-file-format "%Y-%m-%d.org"))

(use-package visual-fill-column)
(after! visual-fill-column
  (add-hook! org-mode
    (setq visual-fill-column-center-text t)
    (visual-fill-column-mode 1)))

(defun bp/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 190)
                  (org-level-2 . 180)
                  (org-level-3 . 160)
                  (org-level-4 . 140)
                  (org-level-5 . 120)
                  (org-level-6 . 100)
                  (org-level-7 . 90)
                  (org-level-8 . 80)))
    (set-face-attribute (car face)
        nil :font "VictorMono Nerd Font"
        :weight 'bold
        :height (cdr face))))


(if 'org-mode
    (bp/org-font-setup))

(map! :leader
      :desc "Toggle file tree"
      "t t" #'treemacs)

(map! :leader
      :desc "Toggle comment"
      "j" #'comment-line)

(use-package! elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.5)
(add-hook 'elfeed-show-mode-hook 'visual-line-mode)
(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
(evil-define-key 'normal elfeed-search-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
(setq elfeed-feeds (quote
    (("https://www.reddit.com/r/linux.rss" reddit linux)
    ("https://www.reddit.com/r/unixporn.rss" reddit unixporn)   ("https://www.reddit.com/r/commandline.rss" reddit commandline)
    ("https://www.reddit.com/r/vim.rss" reddit vim)
    ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
    ("https://www.reddit.com/r/emacs.rss" reddit emacs)
    ("https://hackaday.com/blog/feed/" hackaday linux)
    ("https://opensource.com/feed" opensource linux)
    ("https://linux.softpedia.com/backend.xml" softpedia linux)
    ("https://itsfoss.com/feed/" itsfoss linux)
    ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
    ("https://distrowatch.com/news/dwd.xml" distrowatch linux))))

(use-package prettier-js
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (setq prettier-js-args '(
    "--single-quote" "true"
    "--jsx-single-quote" "true"
 )))

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)
