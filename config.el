;; -*- lexical-binding: t; -*-
(setq user-full-name "Bryan Paronto"
      user-mail-address "bryan@cablcar.digital")

(setq doom-font (font-spec :family "VictorMono Nerd Font" :size 18 :weight 'medium)
      doom-variable-pitch-font (font-spec :family  "VictorMono Nerd Font" :size 18)
      doom-big-font (font-spec :family "VictorMono Nerd Font" :size 20))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq doom-theme 'doom-opera)

(use-package dashboard
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-week-agenda t)
  (setq dashboard-startup-banner "~/.doom.d/banners/gnu.png")
  :config
  (dashboard-setup-startup-hook))

;; (after! visual-fill-column
;;   (add-hook! dashboard-mode
;;     (setq visual-fill-column-center-text t)
;;     (visual-fill-column-mode 1)))

(set-frame-parameter (selected-frame)'alpha '(90 . 90))
(add-to-list 'default-frame-alist'(alpha . (90 . 90)))

(setq display-line-numbers-type 'relative)

(setq org-agenda-files '("~/org"))
(setq org-directory "~/org/")

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
    (set-face-attribute (car face) nil :font "VictorMono Nerd Font" :weight 'bold :height (cdr face))))

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(bp/org-font-setup)

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
                     ("https://www.reddit.com/r/commandline.rss" reddit commandline)
                     ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
                     ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                     ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
                     ("https://hackaday.com/blog/feed/" hackaday linux)
                     ("https://opensource.com/feed" opensource linux)
                     ("https://linux.softpedia.com/backend.xml" softpedia linux)
                     ("https://itsfoss.com/feed/" itsfoss linux)
                     ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
                     ("https://www.phoronix.com/rss.php" phoronix linux)
                     ("http://feeds.feedburner.com/d0od" omgubuntu linux)
                     ("https://www.computerworld.com/index.rss" computerworld linux)
                     ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
                     ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
                     ("https://betanews.com/feed" betanews linux)
                     ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
                     ("https://distrowatch.com/news/dwd.xml" distrowatch linux))))
