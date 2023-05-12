;; -*- lexical-binding: t; -*-
;;
;;
(defun bp/kill-non-project-buffers (&optional kill-special)
  "Kill buffers that do not belong to a `projectile' project or is an EXWM window

With prefix argument (`C-u'), also kill the special buffers."
  (interactive "P")
  (let ((bufs (buffer-list (selected-frame))))
    (dolist (buf bufs)
      (with-current-buffer buf
        (let ((buf-name (buffer-name buf)))
          (when (or (null (projectile-project-p))
                    (and kill-special
                         (string-match "^\*" buf-name)))
            ;; Preserve buffers with names starting with *scratch or *Messages
            (unless (or (string-match "^\\*\\(\\scratch\\|Messages\\)" buf-name)
                        ;; Preserve EXWM buffers
                        (eq (buffer-local-value 'major-mode buf) 'exwm-mode))
              (message "Killing buffer %s" buf-name)
              (kill-buffer buf))))))))

(map! :leader
      :prefix ("b". "buffers")
      :desc "Kill non-Project buffers"
      "D" #'bp/kill-non-project-buffers)

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
      "w" #'ivy-switch-buffer)

(map! :leader
      :prefix ("h". "help")
      :desc "Switch Themes"
      "t" #'counsel-load-theme)

(map! :leader
      :prefix ("b". "buffer")
      :desc "Clone indirect buffer other window"
      "c" #'clone-indirect-buffer-other-window)


(map! :leader
      :prefix ("w" "window")
      :desc "Undo Last Window Change"
      "u" #'winner-undo)

(map! :leader
      :prefix ("w" "window")
      :desc "Redo Last Window Change"
      "U" #'winner-redo)

(global-set-key (kbd "M-=") 'er/expand-region)


;; (defvar my-themes '(doom-palenight doom-nano-light))
;; (defvar my-current-theme 0)

;; (defun my-toggle-theme ()
;;   "Toggle between two themes."
;;   (interactive)
;;   (disable-theme (nth my-current-theme my-themes))
;;   (setq my-current-theme (mod (1+ my-current-theme) (length my-themes)))
;;   (load-theme (nth my-current-theme my-themes) t))

;; (global-set-key (kbd "<f5>") 'my-toggle-theme)

(provide 'bp-keybinds)
