;; -*- lexical-binding: t; -*-
;;

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

(provide 'bp-keybinds)
