;;; ../repos/dotfiles/doom-emacs/.doom.d/ext/keymap.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix-map ("\\" . "words")
       :desc "Check word spelling" "\\" #'flyspell-word
       :desc "Find word synonym" "s" #'synonym))

(map! :leader
      :desc "pdf-view-mode" "v" #'pdf-view-mode)

(map! :leader
      (:prefix-map ("e" . "eshell")
       :desc "Open eshell" "e" #'eshell
       :desc "Open vterm" "v" #'vterm
       :desc "Run command" "c" #'eshell-command
       :desc "Run python interpreter" "p" #'run-python))

(map! :leader
      :desc "Dired" "D" #'dired)

(map! :leader
      (:prefix-map ("d" . "ediff")
       :desc "Quit ediff session" "q" #'ediff-quit
       :desc "Run ediff on two files" "d" #'ediff
       :desc "Run ediff on three files" "3" #'ediff3))

(map! :leader
      :desc "kill-buffer-and-window" "k" #'kill-buffer-and-window)

(map! :leader
      :desc "Pull from upstream branch" "g p" #'magit-pull-from-upstream)

(map! :leader
      :desc "Man" "m" #'man)

(map! :leader
      :desc "Save Tex file and export to PDF" "l" #'TeX-command-master)

(map! :leader
      :desc "Run shell command" "!" #'shell-command)

(map! :leader
      :desc "Run shell command asynchronously" "&" #'async-shell-command)
