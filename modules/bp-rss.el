;; -*- lexical-binding: t; -*-
;; Customizations for elfeed

(after! elfeed
  ;; (elfeed-dashboard-mode)
  (setq rmh-elfeed-org-files '("~/org/elfeed.org"))
  (setq elfeed-search-filter "@1-month-ago +unread"))

(use-package elfeed-dashboard
  :init
  (evil-define-key 'normal elfeed-dashboard-mode-map
    (kbd "U") 'elfeed-dashboard-update
    (kbd "E") 'elfeed-dashboard-edit)
  :config
  (setq elfeed-dashboard-file "~/org/elfeed-dashboard.org")
  ;; update feed counts on elfeed-quit
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

(use-package! elfeed-goodies
  :init
  (setq elfeed-goodies/entry-pane-size 0.5
        elfeed-goodies/entry-pane-position 'right)
  (evil-define-key 'normal elfeed-show-mode-map
    (kbd "J") 'elfeed-goodies/split-show-next
    (kbd "K") 'elfeed-goodies/split-show-prev)
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "J") 'elfeed-goodies/split-show-next
    (kbd "K") 'elfeed-goodies/split-show-prev)
  ;;; NOTE Feeds now configured in ~/org/elfeed.org
  :config
  (add-hook 'elfeed-show-mode-hook 'visual-line-mode)
  (elfeed-goodies/setup))

(provide 'bp-rss)
