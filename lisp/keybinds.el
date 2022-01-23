
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
      "w" #'ivy-switch-buffer-other-window)

(map! :leader
      :prefix ("h". "help")
      :desc "Switch Themes"
      "t" #'counsel-load-theme)

(map! :leader
      :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)
