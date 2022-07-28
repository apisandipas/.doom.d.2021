;; -*- lexical-binding: t; -*-
;; Configuration for EXWM and Desktop Environment

(defun bp/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun exwm-async-run (name)
  "Run a process asynchronously"
  (interactive)
  (start-process name nil name))

(defun run-or-raise-or-dismiss (program program-buffer-name)
  "If no instance of the program is running, launch the program.
If an instance already exists, and its corresponding buffer is
displayed on the screen, move to the buffer. If the buffer is not
visible, switch to the buffer in the current window. Finally, if
the current buffer is already that of the program, bury the
buffer (=minimizing in other WM/DE)"
  ;; check current buffer
  (if (string= (buffer-name) program-buffer-name)
      (bury-buffer)
    ;; either switch to or launch program
    (progn
      (if (get-buffer program-buffer-name)
          (progn
            (if (get-buffer-window program-buffer-name)
                (select-window (display-buffer program-buffer-name) nil)
              (exwm-workspace-switch-to-buffer program-buffer-name)))
        ;; start program
        (exwm-async-run program)))))

(defun bp/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil  "~/.fehbg"))

(defvar bp/polybar-processes nil
  "A list of running polybar processes. So that we can kill them later. 👿")

(defun bp/get-monitors-list ()
  "Get a list of the currently connected monitors.

Requires polybar, instead of relying on xrandr,
 though you probably want it installed too."
  (split-string
   (substring (shell-command-to-string "polybar -m | cut -d: -f 1") 0 -1) "\n"))


(defun bp/kill-panel ()
  "Stop any running polybar processes. and reset the process list variable"
  (interactive)
  (let ((process-list bp/polybar-processes))
    (dolist (p process-list)
      (if (process-live-p p)
          (kill-process p))))
  (setq bp/polybar-processes nil))

(defvar bp/polybar-config-location "~/.doom.d/exwm/polybar.config.ini"
  "The customized location of your polybar config.ini ")

(defun bp/start-panel ()
  "Start polybar on each connected monitor"
  (interactive)
  (bp/kill-panel)
  (setq bp/polybar-processes
        (mapcar (lambda (monitor)
                  (start-process-shell-command "polybar" nil
                                               (format "MONITOR=%s polybar -c %s --reload main" monitor bp/polybar-config-location)))
                (bp/get-monitors-list))))


(defun bp/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun bp/send-polybar-exwm-workspace ()
  (bp/send-polybar-hook "exwm-workspace" 1))

(defun bp/polybar-exwm-workspace ()
  (interactive)
  (pcase exwm-workspace-current-index
    (0 "ﳜ Video")
    (1 "  Term")
    (2 "  Chat")
    (3 "  Dev")
    (4 "  Mail")
    (5 "  Web")
    (6 "  VCS")
    (7 "  Music")
    (8 "  Files")
    (9 "  Streaming")))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'bp/send-polybar-exwm-workspace)

(defun bp/send-polybar-exwm-ws-indicator ()
  (bp/send-polybar-hook "exwm-ws-indicator" 1))

(defun bp/polybar-exwm-ws-indicator ()
  (pcase exwm-workspace-current-index
     (0 "                   ")
     (1 "                   ")
     (2 "                   ")
     (3 "                   ")
     (4 "                   ")
     (5 "                   ")
     (6 "                   ")
     (7 "                   ")
     (8 "                   ")
     (9 "                   ")))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'bp/send-polybar-exwm-ws-indicator)

(defun bp/exwm-init-hook ()

  (doom-mark-buffer-as-real-h)

  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 0)

  ;; Useless gaps == eye candy
  (exwm-outer-gaps-mode +1)

  ;; Start the Polybar panel
  (bp/start-panel)

  ;; Launch apps that will run in the background
  (bp/run-in-background "dropbox")
  (bp/run-in-background "nm-applet")
  (bp/run-in-background "pasystray")
  (bp/run-in-background "blueman-applet")
  ;; (bp/run-in-background "blueman-tray")
  (bp/run-in-background "~/Apps/KeylightControl.AppImage")
  (bp/run-in-background "dunst")
  (bp/run-in-background "firefox")

  (bp/run-in-background "companion")
  (bp/run-in-background "kitty")
  (bp/run-in-background "obs")
  (bp/run-in-background "spotify"))


(defun bp/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun bp/exwm-update-title ()
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun bp/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("kitty" (exwm-workspace-move-window 1))
    ("obs" (exwm-workspace-move-window 0))
    ("firefox" (exwm-workspace-move-window 5))
    ("Spotify" (exwm-workspace-move-window 7))
    ))

;; This function should be used only after configuring autorandr!
(defun bp/update-displays ()
  (interactive)
  (bp/run-in-background "autorandr --change --force")
  (bp/set-wallpaper)
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

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
  (start-process-shell-command "xrandr" nil "xrandr --output DP-2 --mode 1920x1080 --auto\
        --output DP-1 --rotate right --left-of DP-2 --mode 1920x1080 --auto\
        --output HDMI-1 --rotate left --right-of DP-2 --mode 1920x1080 --auto")

  ;; This will need to be updated to the name of a display!  You can find
  ;; the names of your displays by looking at ar andr or the output of xrandr
  (setq exwm-randr-workspace-monitor-plist '(
        0 "DP-1"
        1 "DP-1"
        2 "DP-1"
        3 "DP-2"
        4 "DP-2"
        5 "DP-2"
        6 "DP-2"
        7 "HDMI-1"
        8 "HDMI-1"
        9 "HDMI-1"))

  ;; NOTE: Uncomment these lines after setting up autorandr!
  ;; React to display connectivity changes, do initial display update
  ;; (add-hook 'exwm-randr-screen-change-hook #'bp/update-displays)
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

;;; Start these app in char mode so as to avoid.
  (setq exwm-manage-configurations
        '(((member exwm-class-name '("Emacs" "kitty" "Nyxt"))
	   char-mode t)))

  ;; these keys should always pass through to Emacs
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

;;;  TODO: Thisis may have larger implications
;;;  TODO: Look into methods to move cursor to other frames, like i have set up in Stumpwm
  (general-def :keymaps 'override
    "s-h" 'windmove-left
    "s-l" 'windmove-right
    "s-j" 'windmove-down
    "s-k" 'windmove-up
    )

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; ;; Launch applications via shell command
          ([?\s-\\] . (lambda (command)
                       (interactive (list (read-shell-command " ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ;; ([?\s-w] . exwm-workspace-switch)
          ([?\s-w] . counsel-switch-buffer)
          ;;
          ;; move window workspace with SUPER+SHIFT+{0-9}
          ,@(cl-mapcar (lambda (c n)
                         `(,(kbd (format "s-%c" c)) .
                           (lambda ()
                             (interactive)
                             (exwm-workspace-move-window ,n)
                             ;; (exwm-workspace-switch ,n)
                             )))
                       '(?! ?@ ?# ?$ ?% ?^ ?& ?* ?\( ?\))
                       ;; '(?\) ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\()
                       (number-sequence 0 9))

          ;; Switch to window workspace with SUPER+{0-9}
          ([?\s-1] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
          ([?\s-2] . (lambda () (interactive) (exwm-workspace-switch-create 1)))
          ([?\s-3] . (lambda () (interactive) (exwm-workspace-switch-create 2)))
          ([?\s-4] . (lambda () (interactive) (exwm-workspace-switch-create 3)))
          ([?\s-5] . (lambda () (interactive) (exwm-workspace-switch-create 4)))
          ([?\s-6] . (lambda () (interactive) (exwm-workspace-switch-create 5)))
          ([?\s-7] . (lambda () (interactive) (exwm-workspace-switch-create 6)))
          ([?\s-8] . (lambda () (interactive) (exwm-workspace-switch-create 7)))
          ([?\s-9] . (lambda () (interactive) (exwm-workspace-switch-create 8)))
          ([?\s-0] . (lambda () (interactive) (exwm-workspace-switch-create 9)))))

  (setq window-divider-default-bottom-width 2
        window-divider-default-right-width 2)
  (window-divider-mode)

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

(defun bp/exwm-jump-to-buffer ()
  "Switch to workspace of chosen EXWM buffer."
  (interactive)
  (let ((buf (completing-read "Switch to X buffer: "
                          (mapcar #'buffer-name
                                  (seq-filter
                                   (lambda (b) (eq (buffer-local-value 'major-mode b) 'exwm-mode))
                                   (buffer-list))))))
(exwm-workspace-switch-to-buffer buf)))

(exwm-input-set-key (kbd "s-f")
                    (lambda ()
                      (interactive)
                      (bp/exwm-jump-to-buffer)))

(defun bp/take-screenshot ()
  "Takes a fullscreen screenshot of the current workspace"
  (interactive)
  (require 'cl-lib)
  (require 'seq)
  (when window-system
        (cl-loop for i downfrom 3 to 1 do
                (progn
                (message (concat (number-to-string i) "..."))
                (sit-for 1)))
        (message "Cheese!")
        (sit-for 1)
        (start-process "screenshot" nil "import" "-window" "root"
                (concat (getenv "HOME") "/Pictures/" (seq-subseq (number-to-string (float-time)) 0 10) ".png"))
        (message "Screenshot taken!")))
(global-set-key (kbd "<Print>") 'bp/take-screenshot)

(defun bp/take-screenshot-region ()
  "Takes a screenshot of a region selected by the user."
  (interactive)
  (when window-system
        (call-process "import" nil nil nil ".newScreen.png")
        (call-process "convert" nil nil nil ".newScreen.png" "-shave" "1x1"
                        (concat (getenv "HOME") "/Pictures/" (seq-subseq (number-to-string (float-time)) 0 10) ".png"))
        (call-process "rm" nil nil nil ".newScreen.png")))
(global-set-key (kbd "<Scroll_Lock>") 'bp/take-screenshot-region)

(defun bp/open-spotify ()
  (interactive)
  (run-or-raise-or-dismiss "spotify" "Spotify"))
