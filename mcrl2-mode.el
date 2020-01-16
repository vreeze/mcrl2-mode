;;; mcrl2-mode.el --- Major mode for mCRL2 specifications -*- lexical-binding: t; -*-

;;; eboy.el ---  Emulator  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Pieter de Vreeze <mail@de-vreeze.com>

;; Author: Pieter de Vreeze <mail@de-vreeze.com>
;; URL: https://github.com/vreeze/mcrl2-mode
;; Version: 0.0.1
;; Package-Requires:
;; Keywords:

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; This is a major mode to edit and analyse mCRL2 specifications using the mCRL2 toolset.
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
(defconst mcrl2tools-properties-folder "properties/")

(defvar mcrl2-mode-hook nil)

(defvar mcrl2-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") #'mcrl2-check-syntax-buffer)
    (define-key keymap (kbd "C-c C-t") #'mcrl2-tools)
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
  (interactive (list (transient-args 'mcrl2-mcrl22lps-map)))
  (mcrl2-tools-shell "mcrl22lps" args "_lps" ".lps"))

(defun mcrl2-transient-store-and-back()
  (interactive)
  (transient-set)
  (mcrl2-tools))

(defun mcrl2-lps2pbes (&optional args)
  "Call lps2pbes with arguments ARGS."
  (interactive (list (transient-args 'mcrl2-lps2pbes-map)))
  (transient-set)
  (mcrl2-tools-shell "lps2pbes" args "" ".pbes"))

(defun mcrl2-tools-shell (bin args sep ext)
  "Call tool BIN with arguments ARGS, seperator SEP and extension EXT."
  (unless (file-exists-p mcrl2tools-artifacts-folder)
    (make-directory mcrl2tools-artifacts-folder))
  (let* ((buf (get-buffer-create mcrl2-output-buffer)))
    (async-shell-command (concat bin
                                 " " (mapconcat #'identity args " ") " "
                                 (shell-quote-argument buffer-file-name) " "
                                 (shell-quote-argument (concat default-directory
                                                               mcrl2tools-artifacts-folder
                                                               (file-name-sans-extension (buffer-name))
                                                               sep
                                                               ext)))
                         buf)))

(defun mcrl2-tools-shell-noartefacts (bin args sep ext)
  "Call tool BIN with arguments ARGS, seperator SEP and extension EXT."
  (let* ((buf (get-buffer-create mcrl2-output-buffer)))
    (async-shell-command (concat bin
                                 " " (mapconcat #'identity args " ") " "
                                 (concat default-directory
                                         mcrl2tools-artifacts-folder
                                         (file-name-sans-extension (buffer-name))
                                         sep
                                         ext))
                         buf)))

(define-transient-command mcrl2-tools ()
  "mCRL2 Tools."
  ["mCRL2 Tools."
   [("L" "to LPS" mcrl2-mcrl22lps-map)
    ("S" "to Simulate" mcrl2-lpssim-map)
    ("v" "to Verify" mcrl2-verify-map)]
   [("p" "lps2pbes" mcrl2-lps2pbes-map)
    ("s" "pbessolve" mcrl2-pbessolve-map)]])

(define-transient-command mcrl2-mcrl22lps-map ()
  "mCRL2 to LPS."
  :value '("--verbose")
  ["Options:"
   [("-e" "check syntax and static semantics; do not linearise"  "--check-only")
    ]]
  ["Standard options:"
   [("-v" "display short intermediate messages"     "--verbose")
    ("-d" "display detailed intermediate messages"  "--debug")
    ("-h" "display help information"  "--help")]]
  ["Actions"
   [("L" "mCRL22LPS"          mcrl2-mcrl22lps)]])

(define-transient-command mcrl2-lps2pbes-map ()
  "LPS to PBES."
  :value '("--verbose")
  ["lps2pbes:"
   [("-c" "add counter example equations to the generated PBES" "--counter-example")
    ("-f" "use the state formula from FILE"                 "--formula=" mcrl2-read-file)
    ("-m" "insert dummy fixpoints in modal operators, which may lead to smaller PBESs" "--preprocess-modal-operators")
    (mcrl2-verify:--out)
    ("-s" "generate equations such that no mixed conjunctions and disjunctions occur" "--structured")
    ("-t" "--timed" "use the timed version of the algorithm, even for untimed LPS's")
    ("-u" "--unoptimized" "do not simplify boolean expressions")]]
  ["Standard options:"
   [("-v" "display short intermediate messages"     "--verbose")
    ("-d" "display detailed intermediate messages"  "--debug")
    ("-h" "display help information"  "--help")]]
  ["Actions"
   [("p" "lps2pbes"          mcrl2-lps2pbes)]
   [("s" "store"             mcrl2-transient-store-and-back)]
   ]
  )

(define-transient-command mcrl2-pbessolve-map ()
  "PBES solve."
  :value '("--verbose")
  ["Standard options:"
   [("-v" "display short intermediate messages"     "--verbose")
    ("-d" "display detailed intermediate messages"  "--debug")
    ("-h" "display help information"  "--help")]]
  )


(defconst mcrl2-standard-options ["Standard options:"
                                  [("-v" "display short intermediate messages"     "--verbose")
                                   ("-d" "display detailed intermediate messages"  "--debug")
                                   ("-h" "display help information"  "--help")]])
(define-transient-command mcrl2-lpssim-map ()
  "Simulate the LPS in INFILE via a text-based interface."
  ["Options:"
   [("-y" "do not replace global variables in the LPS with dummy values"  "--nodummy")
    ]]
  ["Standard options:"
   [("-v" "display short intermediate messages"     "--verbose")
    ("-d" "display detailed intermediate messages"  "--debug")
    ("-h" "display help information"  "--help")]]
  ["Actions"
   [("s" "lpssim"          mcrl2-lpssim)]])

(define-infix-argument mcrl2-verify:--out ()
  :description "output format"
  :class 'transient-option
  :key "-o"
  :argument "--out="
  :reader 'mcrl2-lps2pbes-select-out-format)

(defun mcrl2-lps2pbes-select-out-format (&rest _ignore)
  (magit-read-char-case nil t
    (?b "[b]es"       "BES in internal format")
    (?p "[p]bes"      "PBES in internal format (default)")
    (?g "p[g]solver"  "BES in PGSolver format")
    (?t "[t]ext"      "PBES in textual (mCRL2) format")))

(define-transient-command mcrl2-verify-map ()
  "Verify."
  :value '("--verbose")
  ["Standard options:"
   [("-v" "display short intermediate messages"     "--verbose")
    ("-d" "display detailed intermediate messages"  "--debug")
    ("-h" "display help information"                "--help")]]
  ["Actions"
   [("v" "verify"          mcrl2-verify)]])

(define-infix-argument mcrl2:-f ()
  :description "File"
  :class 'transient-files
  :argument "-f"
  :reader 'mcrl2-read-file)

(defun mcrl2-read-file (&rest _)
  "Prompt for formula."
  (expand-file-name (read-file-name "Formula: " (concat default-directory mcrl2tools-properties-folder))))

(defun mcrl2-lpssim (&optional args)
  "Call lpssim with arguments ARGS."
  (interactive (list (transient-args 'mcrl2-lpssim-map)))
  (mcrl2-tools-shell-noartefacts "lpssim" args "_lps" ".lps")
  (switch-to-buffer mcrl2-output-buffer))

(defun mcrl2-verify (&optional args)
  "Call verify with arguments ARGS."
  (interactive (list (transient-args 'mcrl2-verify-map)))
                                        ;(mcrl2-tools-shell-noartefacts "lpssim" args "_lps" ".lps")
  (message "echo verify %s" args)
  (message "echo lps2pbes %s" (transient-args 'mcrl2-lps2pbes-map))
  (message "echo pbessolve %s" (transient-args 'mcrl2-pbessolve-map))
  (switch-to-buffer mcrl2-output-buffer))


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
