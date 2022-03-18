;; -*- lexical-binding: t; -*-
;;
;; (load (expand-file-name "~/.quicklisp/slime-helper.el") nil t)
(setq inferior-lisp-program "sbcl")

(load "~/.doom.d/lisp/keybinds" nil t)
(load "~/.doom.d/lisp/ui" nil t)
(load "~/.doom.d/lisp/org" nil t)
(load "~/.doom.d/lisp/roam" nil t)
(load "~/.doom.d/lisp/rss" nil t)
(load "~/.doom.d/lisp/email" nil t)
;; (add-to-list 'load-path "~/Code/Repos/doom-nano-testing")
;; (require 'load-nano)

;; User Information
(setq user-full-name "Bryan Paronto"
      user-mail-address "bryan@cablecar.digital")

;; Open treemacs on launch
;; Removed to form better file browsing habits...
;; (add-hook! 'window-setup-hook #'treemacs 'append)

;;; Regenerate Agenda HTML after TODO changes
(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (run-with-idle-timer 20 nil (lambda ()
                                          (load-file "~/Code/Repos/agenda-html/agenda-html.el")))))

;; (add-hook! 'typescript-mode (lambda ()
;;                               (setq lsp-eslint-enable t
;;                                     lsp-eslint-auto-fix-on-save t)))

(use-package auto-dim-other-buffers
  :config
  ;; (set-face-attribute auto-dim-other-buffers-face '( background "#bada55"))

  (add-hook 'after-init-hook
            (lambda ()
              (when (fboundp 'auto-dim-other-buffers-mode)
                (auto-dim-other-buffers-mode t)))))



(after! eshell
  (setq eshell-rc-script "~/.doom.d/eshell/profile"
        eshell-aliases-file "~/.doom.d/eshell/aliases"
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t))


;; (setf typescript-indent-level 2)
;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
;;

(after! circe
  (set-irc-server! "irc.libera.chat"
   '(:tls t
        :port 6697
        :sasl-username "apis_and_ipas"
        :sasl-password "qwerty1234"
        :channels ("#emacs" "#chat" "#javascript" "#guix" "#react" "#chicago"))))
