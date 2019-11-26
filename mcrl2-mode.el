;;; mcrl2-mode.el --- Major mode for mCRL2 specifications -*- lexical-binding: t; -*-

;;; Package --- summary:

;;; Commentary:

;;; Code:
(require 'transient)

(defgroup mcrl2-mode nil
  "Major mode for editing mCRL2 files."
  :group 'languages)

(defcustom mcrl2-tools-path ""
  "The location of the mCRL2 tools."
  :type 'string
  :group 'mcrl2)

(defconst mcrl2-output-buffer "*mCRL2 Tools Output*")
(defconst mcrl2tools-artifacts-folder "artifacts/")

(defvar mcrl2-mode-hook nil)

(defvar mcrl2-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") #'mcrl2-check-syntax-buffer)
    (define-key keymap (kbd "C-c C-?") #'mcrl2-tools)
    keymap)
  "Keymap for mCRL2 major mode.")

(defconst mcrl2-font-lock-keywords
  (let ((proc (regexp-opt '("sum" "block" "allow" "hide" "rename" "comm") 'symbols))
        (spec (regexp-opt '("sort" "cons" "map" "var" "eqn" "act" "proc" "init" "struct") 'symbols))
        (proc-c (regexp-opt '("delta" "tau" "true" "false") 'symbols))
        (sort (regexp-opt '("List" "Set" "Bag" "Bool" "Pos" "Nat" "Int" "Real" "List" "Set" "Bag") 'symbols))
        (data (regexp-opt '("whr" "end" "lambda" "forall" "exists" "div" "mod" "in") 'symbols))
        (operator (regexp-opt '("min" "max" "succ" "pred" "abs" "floor" "ceil" "round" "exp" "A2B" "head" "tail" "rhead" "rtail" "count" "Set2Bag" "Bag2Set") 'symbols))
        (operator2 (regexp-opt '("." "+" "|" "&" "<" ">" ":" ";" "=" "#" "@" "(" ")" "{" "}" "||" "_" "->") t)))

    `( (,spec . font-lock-keyword-face)
       (,sort . font-lock-type-face)
       (,proc-c . font-lock-constant-face)
       (,operator . font-lock-constant-face)
       (,operator2 . font-lock-constant-face)
       (,proc . font-lock-builtin-face)))
  "The mCRL2 highlighting keywords.")

(defun mcrl2-indent-line ()
  "Indent current line."
  (interactive)
  (beginning-of-line)
  ;; TODO
  )

(defun mcrl2-start-process (buf bin args)
  "Run BIN as an Emacs process and puts the output into the given buffer (as BUF)."
  (apply #'start-process
         "mCRL2" buf (concat mcrl2-tools-path bin) args))

(defun mcrl2-clear-buffer ()
  "Kill buffer if exists."
  (let ((b (get-buffer mcrl2-output-buffer)))
    (when b
      (kill-buffer b))))

(defun mcrl2-check-syntax-string (string)
  "Check the syntax of the STRING."
  (mcrl2-clear-buffer)
  (let* ((process-connection-type nil) ;; Use a pipe
         (buf (get-buffer-create mcrl2-output-buffer))
         (ps (mcrl2-start-process buf "mcrl22lps" `("--check-only"))))
    (process-send-string ps string)
    (process-send-eof ps)
    (set-process-sentinel ps
                          (lambda (_ps event)
                            (unless (equal event "finished\n")
                              (switch-to-buffer-other-window mcrl2-output-buffer)
                              (error "The mCRL2 parsing failed: %s" event))
                            (let ((output (with-current-buffer mcrl2-output-buffer
                                            (buffer-string))))
                              (if (string-match-p (regexp-quote "contains a well-formed mCRL2 specification") output)
                                  (message output)
                                (switch-to-buffer-other-window mcrl2-output-buffer)))))))

(defun mcrl2-check-syntax-buffer ()
  "Check the syntax of the current buffer."
  (interactive)
  (mcrl2-check-syntax-string (buffer-string)))

(defvar mcrl2-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?% "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\r ">" st)
    st)
  "Syntax table for mCRL2-mode.")

(defun mcrl2-mcrl22lps (&optional args)
  "Call mcrl22lps with arguments ARGS."
  (interactive (list (transient-args 'mcrl2-tools)))
  (mcrl2-tools-shell "mcrl22lps" args "_lps" ".lps"))

(defun mcrl2-tools-shell (bin args sep ext)
  "Call tool BIN with arguments ARGS, seperator SEP and extension EXT."
  (unless (file-exists-p mcrl2tools-artifacts-folder)
    (make-directory mcrl2tools-artifacts-folder))
  (let* ((buf (get-buffer-create mcrl2-output-buffer)))
    (async-shell-command (concat bin
                                 " " (mapconcat #'identity args " ") " "
                                 (shell-quote-argument buffer-file-name)
                                 " > "
                                 (shell-quote-argument (concat default-directory
                                                               mcrl2tools-artifacts-folder
                                                               (file-name-sans-extension (buffer-name))
                                                               sep
                                                               ext)))
                         buf)))

(define-transient-command mcrl2-tools ()
  "mcrl2 tools."
  ["mcrl2lps"
   [("-e" "check syntax and static semantics; do not linearise"  "--check-only")]
   [("-v" "display short intermediate messages"     "--verbose")]
   [("-d" "display detailed intermediate messages"  "--debug")]
   ]
  ["Actions"
   [("d" "mcrl2lps"          mcrl2-mcrl22lps)]])

(defun mcrl2-mode ()
  "Mode for editing mCRL2 specifications."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mcrl2-mode-map)
  (set-syntax-table mcrl2-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(mcrl2-font-lock-keywords))
  ;; Register indentation function
  (set (make-local-variable 'indent-line-function) 'mcrl2-indent-line)

  (setq major-mode 'mcrl2-mode)
  (setq mode-name "mCRL2")
  (run-hooks 'mcrl2-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mcrl2\\'" . mcrl2-mode))

(provide 'mcrl2-mode)

;;; mcrl2-mode.el ends here
