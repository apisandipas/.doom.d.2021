(defun bp/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun bp/set-wallpaper ()
  (interactive)
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
  (setq bp/polybar-process (start-process-shell-command "polybar" nil "polybar -c ~/.doom.d/exwm/polybar.config.ini --reload main")))

(defun bp/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun bp/send-polybar-exwm-workspace ()
  (bp/send-polybar-hook "exwm-workspace" 1))

(defun bp/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (1 "  Dev")
    (2 "  Term")
    (3 "  Chat")
    (4 "  Mail")
    (5 "  Data")
    (6 "爵  Web")
    (7 "  VCS")
    (8 "  Music")
    (9 "  Files")
    (0 "  Video")
    ))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'bp/send-polybar-exwm-workspace)

(defun bp/exwm-init-hook ()
  (doom-mark-buffer-as-real-h)
  ;; Open eshell by default
  ;; (eshell)

  (exwm-outer-gaps-mode +1)

  ;; Show the time and date in modeline
  (setq display-time-day-and-date nil)
  (display-time-mode -1)

  ;; Start the Polybar panel
  (bp/start-panel)

  ;; Launch apps that will run in the background
  (bp/run-in-background "nm-applet")
  (bp/run-in-background "pasystray")
  (bp/run-in-background "blueman-applet")
  (bp/run-in-background "blueman-tray"))
  ;; (bp/run-in-background "serve ~/org/brain/bins/agenda -p 8989")
  (bp/run-in-background "dunst -geom \"380x50-10+38\" -frame_width \"1\" -font \"Victor Mono Medium 14\"")

(defun bp/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun bp/exwm-update-title ()
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))


;; (defun bp/configure-window-by-class ()
;;   (interactive)
;;   (pcase exwm-class-name
;;     ("firefox" (exwm-workspace-move-window 6))))

;; This function should be used only after configuring autorandr!
(defun bp/update-displays ()
  (bp/run-in-background "autorandr --change --force")
  (bp/set-wallpaper)
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

(use-package edwina
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys)
  ;; (edwina-mode 1)
  )


(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 10)
  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'bp/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'bp/exwm-update-title)

  ;; Configure windows as they're created
  ;; (add-hook 'exwm-manage-finish-hook #'bp/configure-window-by-class)

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
  (add-hook 'exwm-randr-screen-change-hook #'bp/update-displays)
  (bp/update-displays)



  ;; Set the wallpaper after changing the resolution
  (bp/set-wallpaper)

  ;; Automatically send the mouse cursor to the selected workspace's display
  (setq exwm-workspace-warp-cursor t)

  ;; Window focus should follow the mouse pointer
  (setq mouse-autoselect-window nil
        focus-follows-mouse nil)

;;;  Play nice with firefox, enables modal interactions
  (require 'exwm-firefox-evil)
  (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          escape
          ?\M-`
          ?\M-&
          ?\M-:
          ?\s-o    ;;Allow org-capture to passthru in Xwindows
          ?\s-i    ;; Toggles char-mode/line-mode
          ?\C-\M-j ;; Buffer list
          ?\C-\ )) ;; Ctrl+Space

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
          ;; ([?\s-w] . exwm-workspace-switch)
          ([?\s-w] . switch-to-buffer-other-window)
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

          ;; Switch to window workspace with SUPER+SHIFT+{0-9}
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


(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))
;; (defun bp/disable-desktop-notifications ()
;;   (interactive)
;;   (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_PAUSE\""))

;; (defun bp/enable-desktop-notifications ()
;;   (interactive)
;;   (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_RESUME\""))

;; (defun bp/toggle-desktop-notifications ()
;;   (interactive)
;;   (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_TOGGLE\""))

(defun exwm-input-line-mode ()
  "Set exwm window to line-mode and show mode line"
  (call-interactively #'exwm-input-grab-keyboard))

(defun exwm-input-char-mode ()
  "Set Exwm window to char-mode and hide mode line"
  (call-interactively #'exwm-input-release-keyboard))

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
