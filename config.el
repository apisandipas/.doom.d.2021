;; -*- lexical-binding: t; -*-

(load "~/.doom.d/lisp/keybinds" nil t)
(load "~/.doom.d/lisp/ui" nil t)
(load "~/.doom.d/lisp/org" nil t)
(load "~/.doom.d/lisp/roam" nil t)
(load "~/.doom.d/lisp/rss" nil t)
(load "~/.doom.d/lisp/email" nil t)

;;; User Information
(setq user-full-name "Bryan Paronto"
      user-mail-address "bryan@cablecar.digital")

;; Open treemacs on launch
(add-hook! 'window-setup-hook #'treemacs 'append)

;;; Regenerate Agenda HTML after TODO changes
(add-hook 'org-after-todo-state-change-hook (lambda ()
          (run-with-idle-timer 20 nil (lambda () (load-file "~/Code/Repos/agenda-html/agenda-html.el")))))
