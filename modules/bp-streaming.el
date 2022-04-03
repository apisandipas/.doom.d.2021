;; -*- lexical-binding: t; -*-
;; Helpers for OBS and Twitch.tv

(defun bp/twitch-message (text)
  (interactive "MText: ")
  (with-current-buffer
      (get-buffer-create "Twitch message")
    (erase-buffer)
    (insert text)
    (goto-char (point-min))))

(use-package obs-websocket
  :config
  (defhydra bp/obs-websocket (:exit t)
    "
^Stream^            ^Scenes^          ^Actions^
^^^^^^^^-------------------------------------------------
_sb_: Stream begin  _C_: Small Screen _o_: Open Twitch
_se_: Stream end    _u_: Close Up     _m_: Macbook
_c_: Connect        _b_: BRB          _o_: Starting Soon
                                  _t_: Technical Difficulties
"
    ("t" org-roam-dailies-goto-today)
    ("c" (obs-websocket-connect) "Connect")
    ("C" (obs-websocket-send "SetCurrentScene" :scene-name "Small Screen") )
    ("u" (obs-websocket-send "SetCurrentScene" :scene-name "Close Up") )
    ("b" (obs-websocket-send "SetCurrentScene" :scene-name "Card - BRB") )
    ("o" (obs-websocket-send "SetCurrentScene" :scene-name "Card -Starting Soon"))
    ("t" (obs-websocket-send "SetCurrentScene" :scene-name "Card- Technical Difficulties") )
    ("m" (obs-websocket-send "SetCurrentScene" :scene-name "Macbook") )
    ("<f7>" bp/twitch-message "Message") ;; Then I can just f7 f7
    ("sb" (obs-websocket-send "StartStreaming") )
    ("se" (obs-websocket-send "StopStreaming") ))
  (global-set-key (kbd "<f7>") #'bp/obs-websocket/body))

(provide 'bp-streaming)
