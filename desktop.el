(defun bp/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun bp/set-wallpaper ()
  (interactive)
  ;; NOTE: You will need to update this to a valid background path!
  (start-process-shell-command
   "feh" nil  "~/.fehbg"))

(defvar bp/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun bp/kill-panel ()
  (interactive)
  (when bp/polybar-process
    (ignore-errors
      (kill-process bp/polybar-process)))
  (setq bp/polybar-process nil))

(defun bp/start-panel ()
  (interactive)
  (bp/kill-panel)
  (setq bp/polybar-process (start-process-shell-command "polybar" nil "sh ~/.config/polybar/launch-exwm.sh")))

(defun bp/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun bp/send-polybar-exwm-workspace ()
  (bp/send-polybar-hook "exwm-workspace" 1))

(defun bp/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (1 "dev")
    (2 "term")
    (3 "comms")
    (4 "mail")
    (5 "mt1")
    (6 "web")
    (7 "git")
    (8 "mus")
    (9 "mt2")
    (0 "vid")
    ))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'bp/send-polybar-exwm-workspace)

(defun bp/exwm-init-hook ()
  (doom-mark-buffer-as-real-h)
  (with-eval-after-load 'perspective
    ;; Set up perspective names on initial workspaces
    (exwm-workspace-switch-create 0)
    (persp-switch "Video")
    (persp-kill "main")

    (exwm-workspace-switch-create 1)

    (exwm-workspace-switch-create 2)
    (persp-switch "Term")
    (persp-kill "main")

    (exwm-workspace-switch-create 3)
    (persp-switch "Comms")
    (persp-kill "main")

    (exwm-workspace-switch-create 4)
    (persp-switch "Mail")
    (persp-kill "main")

    (exwm-workspace-switch-create 5)

    (exwm-workspace-switch-create 6)
    (persp-switch "Web")
    (persp-kill "main")

    (exwm-workspace-switch-create 7)
    (persp-switch "Git")
    (persp-kill "main")

    (exwm-workspace-switch-create 8)
    (persp-switch "Music")
    (persp-kill "main")

    (exwm-workspace-switch-create 9)
    )
  ;; Open eshell by default
  ;; (eshell)

  (exwm-outer-gaps-mode +1)

  ;; Show the time and date in modeline
  (setq display-time-day-and-date t)
  (display-time-mode 1)
  ;; Also take a look at display-time-format and format-time-string

  ;; Start the Polybar panel
  (bp/start-panel)

  ;; Launch apps that will run in the background
  (bp/run-in-background "dunst")
  (bp/run-in-background "nm-applet")
  (bp/run-in-background "pasystray")
  (bp/run-in-background "blueman-applet")
  (bp/run-in-background "blueman-tray"))


(defun bp/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun bp/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))


(defun bp/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-move-window 6))
    ("spotify" (exwm-floating-toggle-floating)
     (exwm-workspace-move-window 8)
     (exwm-layout-toggle-mode-line))))

;; This function should be used only after configuring autorandr!
;; (defun bp/update-displays ()
;;   (bp/run-in-background "autorandr --change --force")
;;   (bp/set-wallpaper)
;;   (message "Display config: %s"
;;            (string-trim (shell-command-to-string "autorandr --current"))))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 10)
  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'bp/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'bp/exwm-update-title)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'bp/configure-window-by-class)

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'bp/exwm-init-hook)

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --mode 1920x1080  --auto\
        --output DVI-I-1-1 --rotate right --left-of eDP-1 --mode 1920x1080 --auto \
        --output DVI-I-2-2 --mode 1920x1080 --left-of DVI-I-1-1  --auto")

  ;; This will need to be updated to the name of a display!  You can find
  ;; the names of your displays by looking at arandr or the output of xrandr
  (setq exwm-randr-workspace-monitor-plist '(
                                             1 "DVI-I-2-2"
                                             2 "DVI-I-2-2"
                                             3 "DVI-I-2-2"
                                             4 "DVI-I-2-2"
                                             5 "DVI-I-1-1"
                                             6 "DVI-I-1-1"
                                             7 "DVI-I-1-1"
                                             8 "eDP-1"
                                             9 "eDP-1"
                                             0 "eDP-1"
                                             ))

  ;; NOTE: Uncomment these lines after setting up autorandr!
  ;; React to display connectivity changes, do initial display update
  ;; (add-hook 'exwm-randr-screen-change-hook #'bp/update-displays)
  ;; (bp/update-displays)

  ;; Set the wallpaper after changing the resolution
  (bp/set-wallpaper)

  ;; Automatically send the mouse cursor to the selected workspace's display
  (setq exwm-workspace-warp-cursor t)

  ;; Window focus should follow the mouse pointer
  (setq mouse-autoselect-window nil
        focus-follows-mouse nil)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\s-o                         ;;Allow org-capture to passthru in Xwindows
          ?\s-i                         ;; Toggles char-mode/line-mode
          ?\C-\M-j                      ;; Buffer list
          ?\C-\ ))                      ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ;;
          ;; move window workspace with SUPER+SHIFT+{0-9}
          ([?\s-\)] . (lambda () (interactive) (exwm-workspace-move-window 0)))
          ([?\s-!] . (lambda () (interactive) (exwm-workspace-move-window 1)))
          ([?\s-\@] . (lambda () (interactive) (exwm-workspace-move-window 2)))
          ([?\s-#] . (lambda () (interactive) (exwm-workspace-move-window 3)))
          ([?\s-$] . (lambda () (interactive) (exwm-workspace-move-window 4)))
          ([?\s-%] . (lambda () (interactive) (exwm-workspace-move-window 5)))
          ([?\s-^] . (lambda () (interactive) (exwm-workspace-move-window 6)))
          ([?\s-&] . (lambda () (interactive) (exwm-workspace-move-window 7)))
          ([?\s-*] . (lambda () (interactive) (exwm-workspace-move-window 8)))
          ([?\s-\(] . (lambda () (interactive) (exwm-workspace-move-window 9)))
          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  ;; (perspective-exwm-mode)
  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config
  (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

(server-start)


(defun bp/disable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_PAUSE\""))

(defun bp/enable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_RESUME\""))

(defun bp/toggle-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_TOGGLE\""))


;; (setq perspective-exwm-override-initial-name
;;       '((0 . "video")
;;         (1 . "dev")
;;         (2 . "term")
;;         (3 . "comms")
;;         (4 . "mail")
;;         (5 . "empty")
;;         (6 . "web")
;;         (7 . "vc")
;;         (8 . "music")
;;         (9 . "misc")))

;; (defun my/exwm-configure-window ()
;;   (interactive)
;;   (pcase exwm-class-name
;;     ((or "Firefox" "Nightly")
;;      (perspective-exwm-assign-window
;;       :workspace-index 6
;;       :persp-name "web"))
;;     ("Kitty"
;;      (perspective-exwm-assign-window
;;       :workspace-index 6
;;       :persp-name "term"))
;;     ((or "Slack" "Discord" "TelegramDesktop")
;;      (perspective-exwm-assign-window
;;       :workspace-index 3
;;       :persp-name "comms"))))

;; (add-hook 'exwm-manage-finish-hook #'my/exwm-configure-window)

(defun exwm-input-line-mode ()
  "Set exwm window to line-mode and show mode line"
  (call-interactively #'exwm-input-grab-keyboard)
  ;; (exwm-layout-show-mode-line)
  )

(defun exwm-input-char-mode ()
  "Set Exwm window to char-mode and hide mode line"
  (call-interactively #'exwm-input-release-keyboard)
  ;; (exwm-layout-hide-mode-line)
  )

(defun exwm-input-toggle-mode ()
  "Toggle between line- and char-mode"
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (if (equal (nth 1 (nth 1 mode-line-process)) "line")
          (exwm-input-char-mode)
        (exwm-input-line-mode)))))

(exwm-input-set-key (kbd "s-i")
                    (lambda () (interactive)
                      (exwm-input-toggle-mode)))

(exwm-input-set-key (kbd "s-o")
                    (lambda ()
                      (interactive)
                      (exwm-input-toggle-mode)
                      (org-capture)))
