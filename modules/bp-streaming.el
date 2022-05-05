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
_sb_: Stream begin  _m_: Main         _O_: Open Twitch
_se_: Stream end    _u_: Close Up     _x_: Thanks
_c_: Connect        _b_: BRB          _o_: Starting Soon
"
    ("c" (obs-websocket-connect) "Connect")
    ("m" (obs-websocket-send "SetCurrentScene" :scene-name "Main") )
    ("u" (obs-websocket-send "SetCurrentScene" :scene-name "CloseUp") )
    ("o" (obs-websocket-send "SetCurrentScene" :scene-name "Card-StartingSoon") )
    ("t" (obs-websocket-send "SetCurrentScene" :scene-name "Card-TechnicalDifficulties"))
    ("b" (obs-websocket-send "SetCurrentScene" :scene-name "Card-BRB"))
    ("x" (obs-websocket-send "SetCurrentScene" :scene-name "Card-Thanks"))
    ("O" (browse-url "https://twitch.tv/facetious_coding"))
    ("sb" (obs-websocket-send "StartStreaming") )
    ("se" (obs-websocket-send "StopStreaming") ))
  (global-set-key (kbd "<f7>") #'bp/obs-websocket/body))

(provide 'bp-streaming)
