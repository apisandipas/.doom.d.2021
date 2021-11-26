;; -*- lexical-binding: t; -*-
(setq user-full-name "Bryan Paronto"
      user-mail-address "bryan@cablcar.digital")

(setq doom-font (font-spec :family "Victor Mono" :size 18 :weight 'medium)
      doom-variable-pitch-font (font-spec :family  "Victor Mono" :size 18)
      doom-big-font (font-spec :family "Victor Mono" :size 20))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq doom-theme 'one-dark)

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq doom-fallback-buffer "*dashboard*")
  (when (file-exists-p "~/.doom.d.default/banners")
    (setq +doom-dashboard-banner-padding '(0 . 2)
          +doom-dashboard-banner-file "gnu.png"
          +doom-dashboard-banner-dir "~/.doom.d/banners")))

(set-frame-parameter (selected-frame)'alpha '(90 . 90))
(add-to-list 'default-frame-alist'(alpha . (90 . 90)))

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/")

(use-package visual-fill-column)
(after! visual-fill-column
  (add-hook! org-mode
    (setq visual-fill-column-center-text t)
    (visual-fill-column-mode 1)))

(defun bp/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 200)
                  (org-level-2 . 180)
                  (org-level-3 . 200)
                  (org-level-4 . 180)
                  (org-level-5 . 180)
                  (org-level-6 . 180)
                  (org-level-7 . 160)
                  (org-level-8 . 160)))
    (set-face-attribute (car face) nil :font "Victor Mono" :weight 'bold :height (cdr face))))

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(bp/org-font-setup)

(after! treemacs
  (map! :leader
      :desc "Toggle file tree"
      "t t" #'treemacs))

(map! :leader
      :desc "Toggle comment"
      "j" #'comment-line)
