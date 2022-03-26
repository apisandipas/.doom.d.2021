;; -*- lexical-binding: t; -*-
;; Customizations for elfeed

(use-package! elfeed-goodies
  :init
  (setq elfeed-goodies/entry-pane-size 0.5)
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
