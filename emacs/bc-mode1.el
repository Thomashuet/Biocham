      ("%.*" . font-lock-comment-face)
      ("\\<\\(where\\|in\\|and\\|parts_of\\|not\\|for\\|if\\|then\\|else\\)\\>"
         . font-lock-keyword-face)
      ("\\<\\(present\\|absent\\|undefined\\|parameter\\|macro\\|list_\\(parameter\\|macro\\)s\\|make\\(pre\\|ab\\)sent_not_\\(pre\\|ab\\)sent\\|\\(clear_\\)\\?initial_state\\)\\>"
         . font-lock-builtin-face)
      ("\\<[0-9.]+\\>" . font-lock-constant-face)
   ))

(defvar bc-mode-map
   (let 
      ((bc-mode-map (make-sparse-keymap)))
      ; (define-key bc-mode-map "\C-j" 'newline-and-indent)
      bc-mode-map)
   "Keymap for Biocham mode")

(defvar bc-mode-syntax-table
   (let ((bc-mode-syntax-table (make-syntax-table)))
      ; _ is a word constituent
      (modify-syntax-entry ?_ "w" bc-mode-syntax-table)
      bc-mode-syntax-table)
  "Syntax table for Biocham mode")

(defun bc-mode ()
   "Major mode for editing Biocham files."
   (interactive)
   (kill-all-local-variables)
   (use-local-map bc-mode-map)
   (set-syntax-table bc-mode-syntax-table)
   (set (make-local-variable 'comment-start) "% ")
   (set (make-local-variable 'font-lock-defaults) '(biocham-mode-keywords))
   (setq major-mode 'bc-mode)
   (setq mode-name "Biocham Mode")
   (run-hooks 'bc-mode-hook))

(provide 'bc-mode)
