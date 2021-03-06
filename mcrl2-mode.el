;;; mcrl2-mode.el --- Major mode for mCRL2 specifications -*- lexical-binding: t; -*-

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
                                ;;(switch-to-buffer-other-window mcrl2-output-buffer)
                                ))))))

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
  (mcrl2-tools-shell "mcrl22lps" args (mcrl2-get-file-name nil ".mcrl2") (mcrl2-get-file-name 't ".lps")))

(defun mcrl2-lps2lts (&optional args)
  "Call lps2lts with arguments ARGS."
  (interactive (list (transient-args 'mcrl2-lps2lts-map)))
  (mcrl2-tools-shell "lps2lts" args (mcrl2-get-file-name 't ".lps") (mcrl2-get-file-name 't ".lts")))

(defun mcrl2-transient-store-and-back()
  (interactive)
  (transient-set)
  (mcrl2-tools))

(defun mcrl2-lps2pbes (&optional args)
  "Call lps2pbes with arguments ARGS."
  (interactive (list (transient-args 'mcrl2-lps2pbes-map)))
  (transient-set)
  (mcrl2-tools-shell "lps2pbes" args (mcrl2-get-file-name 't ".lps") (mcrl2-get-file-name 't ".pbes")))

(defun mcrl2-get-file-name (is-artifact extension)
  "Construct input file, prepend with artifact folder if IS-ARTIFACT is t, concat with EXTENSION."
  (concat (if is-artifact
              mcrl2tools-artifacts-folder
            "")
          (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
          extension))

(defun mcrl2-tools-shell (bin args in-file out-file)
  "Call tool BIN with arguments ARGS and input file IN-FILE and output file OUT-FILE."
  (unless (file-exists-p mcrl2tools-artifacts-folder)
    (make-directory mcrl2tools-artifacts-folder))
  (let* ((buf (get-buffer-create mcrl2-output-buffer)))
    (async-shell-command (concat bin
                                 " " (mapconcat #'identity args " ") " "
                                 (shell-quote-argument in-file) " "
                                 (shell-quote-argument out-file))
                         buf)))

(defun mcrl2-tools-shell-noartefacts (bin args in-file)
  "Call tool BIN with arguments ARGS and input file IN-FILE."
  (let* ((buf (get-buffer-create mcrl2-output-buffer)))
    (async-shell-command (concat bin
                                 " " (mapconcat #'identity args " ") " "
                                 in-file)
                         buf)))

(define-transient-command mcrl2-tools ()
  "mCRL2 Tools."
  ["mCRL2 Tools - configure/execute"
   [("l" "mcrl22lps" mcrl2-mcrl22lps-map)
    ("t" "lps2lts" mcrl2-lps2lts-map)
    ("p" "lps2pbes" mcrl2-lps2pbes-map)
    ("s" "pbessolve" mcrl2-pbessolve-map)]]
  ["Actions:"
   [;;("P" "Parse Specification" mcrl2-lpssim-map)
    ("I" "LTS Info" mcrl2-ltsinfo)
    ("S" "Simulate" mcrl2-lpssim-map)
    ("V" "Verify" mcrl2-verify-map)]])

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
   [("l" "mCRL22LPS"          mcrl2-mcrl22lps)]
   [("s" "store and back"     mcrl2-transient-store-and-back)]])

(define-transient-command mcrl2-lps2lts-map ()
  "LPS to LTS."
  :value '("--verbose")
  ["Options:"
   [("-D" "detect deadlocks (i.e. for every deadlock a message is printed)"  "--deadlock")
    ]]
  ["Standard options:"
   [("-v" "display short intermediate messages"     "--verbose")
    ("-d" "display detailed intermediate messages"  "--debug")
    ("-h" "display help information"  "--help")]]
  ["Actions"
   [("t" "LPS2LTS"            mcrl2-lps2lts)]
   [("s" "store and back"     mcrl2-transient-store-and-back)]])

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
   [("s" "store and back"    mcrl2-transient-store-and-back)]
   ]
  )

(define-transient-command mcrl2-pbessolve-map ()
  "PBES solve."
  :value '("--verbose")
  ["pbessolve:"
   [("-e" "The file to which the evidence is written. If not set, a default name will be chosen." "--evidence-file")
    ("-f" "The file containing the LPS or LTS that was used to generate the PBES using lps2pbes -c. If this option
is set, a counter example or witness for the encoded property will be generated. The extension of the file
should be .lps in case of an LPS file, in all other cases it is assumed to be an LTS." "--file")]]
  ["Standard options:"
   [("-v" "display short intermediate messages"     "--verbose")
    ("-d" "display detailed intermediate messages"  "--debug")
    ("-h" "display help information"  "--help")]]
  ["Actions"
   ;;[("p" "pbessolve"          mcrl2-pbessolve)]
   [("s" "store and back"     mcrl2-transient-store-and-back)]])


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
  (file-relative-name (expand-file-name (read-file-name "Formula: " (concat default-directory mcrl2tools-properties-folder))) default-directory))

(defun mcrl2-lpssim (&optional args)
  "Call lpssim with arguments ARGS."
  (interactive (list (transient-args 'mcrl2-lpssim-map)))
  ;; If mcrl2 file is newer than lps file, suggest to run mcrl22lps
  (when (file-newer-than-file-p (mcrl2-get-file-name nil ".mcrl2") (mcrl2-get-file-name 't ".lps"))
    (mcrl2-mcrl22lps))
  ;; TODO: If mcrl22lps returns an error, do not start lpssim
  (mcrl2-tools-shell-noartefacts "lpssim" args (mcrl2-get-file-name 't ".lps"))
  (switch-to-buffer mcrl2-output-buffer))

(defun mcrl2-ltsinfo (&optional args)
  "Call ltsinfo."
  (interactive (list (transient-args 'mcrl2-tools)))
  (mcrl2-tools-shell-noartefacts "ltsinfo" "" (mcrl2-get-file-name 't ".lts"))
  (switch-to-buffer mcrl2-output-buffer))

(defun mcrl2-verify (&optional args)
  "Call verify with arguments ARGS."
  (interactive (list (transient-args 'mcrl2-verify-map)))
  (message "echo %s, %s, %s, %s" args (transient-args 'mcrl2-mcrl22lps-map) (transient-args 'mcrl2-lps2pbes-map) (transient-args 'mcrl2-pbessolve-map))
  (unless (file-exists-p mcrl2tools-artifacts-folder)
    (make-directory mcrl2tools-artifacts-folder))
  (let* ((buf (get-buffer-create mcrl2-output-buffer))
         (specname (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
         (lpsname (concat mcrl2tools-artifacts-folder specname ".lps"))
         (pbesname (concat mcrl2tools-artifacts-folder specname ".pbes"))
         (mcrl22lps-args (mapconcat #'identity (transient-args 'mcrl2-mcrl22lps-map) " "))
         (lps2pbes-args (mapconcat #'identity (transient-args 'mcrl2-lps2pbes-map) " "))
         (pbessolve-args (mapconcat #'identity (transient-args 'mcrl2-pbessolve-map) " "))
         (mcrl22lps-cmd (concat "mcrl22lps"
                                " " mcrl22lps-args
                                " " (shell-quote-argument (file-name-nondirectory (buffer-file-name)))
                                " " (shell-quote-argument lpsname)))
         (lps2pbes-cmd (concat "lps2pbes"
                               " " lps2pbes-args
                               " " (shell-quote-argument lpsname)
                               " " (shell-quote-argument pbesname)))
         (pbessolve-cmd (concat "pbessolve"
                                " " pbessolve-args
                                " " (shell-quote-argument pbesname))))

    (async-shell-command (concat mcrl22lps-cmd
                                 " && " lps2pbes-cmd
                                 " && " pbessolve-cmd)
                         buf)
    ))


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
