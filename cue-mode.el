;;; cue-mode.el --- Major mode for editing CUE files.

;; Copyright (C) 2022-2022 Detlev Vandaele

;; Author: Detlev Vandaele
;; URL: https://github.com/detvdl/cue-mode.el
;; Version: 0.0.1
;; Package-Requires: ((json-mode "1.8.0") (emacs "24.4") (reformatter "0.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; extend json-mode's syntax highlighting and provide basic flychecker

;;; Code:

(require 'json-mode)
(require 'reformatter)
(require 'rx)

(defgroup cue-mode '()
  "Major mode for editing CUE files."
  :group 'json-mode)

(defcustom cue-command "cue"
  "Command used to interact with CUE files.
Should be `cue' or the complete, absolute path to the `cue' executable on your system."
  :type 'file
  :group 'cue-mode
  :safe 'stringp)

(defcustom cue-fmt-at-save t
  "If non-nil, CUE buffers will be formatted after each buffer save."
  :type 'boolean
  :group 'cue-mode
  :safe 'booleanp)

(reformatter-define cue-format
  :program cue-command
  :args '("fmt" "-E" "--strict")
  :stdin nil
  :stdout nil
  :input-file (reformatter-temp-file-in-current-directory)
  :group 'cue-mode
  :lighter "CUEfmt")

(defvar cue-mode-keywords (regexp-opt '("import" "package" "len" "close" "and"
                                        "or" "div" "mod" "quo" "rem") 'symbols))

(defvar cue-mode-types (regexp-opt '("null" "bool" "int" "float" "string" "bytes"
                                     "number" "uint" "uint8" "int8" "uint16" "int16"
                                     "rune" "uint32" "int32" "uint64" "int64" "uint128"
                                     "int128" "float32" "float64") 'symbols))

(defvar cue-mode-comprehension (regexp-opt '("for" "in" "if" "let") 'symbols))

(defconst cue-mode-operators (regexp-opt '("+" "&&" "==" "<" "=" "(" ")" "-" "||"
                                           "!=" ">" ":" "{" "}" "*" "&" "=~"
                                           "<=" "?" "[" "]" "," "/" "|" "!~"
                                           ">=" "!" "_|_" "..." ".")))

(defconst cue-mode-identifiers "\\(_?#?[a-zA-Z]+[a-zA-Z0-9]*\\??\\)[[:space:]]*\\(:|=\\)")
(defconst cue-mode-quoted-string-re json-mode-quoted-string-re)
(defconst cue-mode-quoted-key-re json-mode-quoted-key-re)
(defconst cue-mode-number-re json-mode-number-re)
(defconst cue-mode-keyword-re json-mode-keyword-re)

(defconst cue-font-lock-keywords
  `((,cue-mode-keywords . font-lock-keyword-face)
    (,cue-mode-types . font-lock-type-face)
    (,cue-mode-comprehension . font-lock-keyword-face)
    (,cue-mode-operators . font-lock-builtin-face)
    (,cue-mode-identifiers . (1 font-lock-variable-name-face))
    (,cue-mode-keyword-re . (1 font-lock-constant-face))
    (,cue-mode-number-re . (1 font-lock-constant-face))))

;;;###autoload
(defconst cue-mode-standard-file-ext '(".cue")
  "List of CUE file extensions.")

;; This is to be sure the customization is loaded.  Otherwise,
;; autoload discards any defun or defcustom.
;;;###autoload
(defsubst cue-mode--update-auto-mode (filenames)
  "Update the `cue-mode' entry of `auto-mode-alist'.

FILENAMES should be a list of file as string.
Return the new `auto-mode-alist' entry"
  (let* ((new-regexp
          (rx-to-string
           `(seq (eval
                  (cons 'or
                        (append cue-mode-standard-file-ext
                                ',filenames))) eot)))
         (new-entry (cons new-regexp 'cue-mode))
         (old-entry (when (boundp 'cue-mode--auto-mode-entry)
                      cue-mode--auto-mode-entry)))
    (setq auto-mode-alist (delete old-entry auto-mode-alist))
    (add-to-list 'auto-mode-alist new-entry)
    new-entry))

;;; make byte-compiler happy
(defvar cue-mode--auto-mode-entry)

;;;###autoload
(defcustom cue-mode-auto-mode-list '(".cue")
  "List of filenames for the CUE entry of `auto-mode-alist'."
  :group 'cue-mode
  :type '(repeat string)
  :set (lambda (symbol value)
         "Update SYMBOL with a new regexp made from VALUE.

This function calls `cue-mode--update-auto-mode' to change the
`cue-mode--auto-mode-entry' entry in `auto-mode-alist'."
         (set-default symbol value)
         (setq cue-mode--auto-mode-entry (cue-mode--update-auto-mode value))))

;; Autoload needed to initalize the the `auto-list-mode' entry.
;;;###autoload
(defvar cue-mode--auto-mode-entry (cue-mode--update-auto-mode cue-mode-auto-mode-list)
  "Regexp generated from the `cue-mode-auto-mode-list'.")

(defvar cue-mode-syntax-table
  (let ((st (make-syntax-table json-mode-syntax-table)))
    ;; C-style comments
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23"   st)
    (modify-syntax-entry ?\n "> b"  st)
    ;; Give CR the same syntax as newline, for selective-display
    (modify-syntax-entry ?\^m "> b" st)
    st))

(defun cue-mode--syntactic-face (state)
  "Return syntactic face function for the position represented by STATE.
STATE is a `parse-partial-sexp' state, and the returned function is the
cue font lock syntactic face function."
  (cond
   ((nth 3 state)
    ;; This might be a string or a name
    (let ((startpos (nth 8 state)))
      (save-excursion
        (goto-char startpos)
        (if (looking-at-p cue-mode-quoted-key-re)
            font-lock-keyword-face
          font-lock-string-face))))
   ((nth 4 state) font-lock-comment-face)))

;; Flycheck checker
(flycheck-def-executable-var cue-checker cue-command)
(flycheck-define-command-checker 'cue-checker
  "A CUE syntax checker using the CUE cli command.

See URL `https://cuelang.org/'."
  :command `(,cue-command "vet" source)
  :error-patterns
  '((error line-start (opt (or "#" "_#") (zero-or-more (not ":")) ": ") (message) ":" "\n"
           (zero-or-more (any blank)) (file-name) ":" line ":" column "\n"
           (opt (zero-or-more (any blank)) (file-name) ":" end-line ":" end-column line-end)))
  :modes 'cue-mode
  :predicate (lambda () (executable-find cue-command))
  :next-checkers '())

(add-to-list 'flycheck-checkers 'cue-checker)

;;;###autoload
(define-derived-mode cue-mode json-mode "CUE"
  "Major mode for editing CUE files."
  :syntax-table cue-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
	   '(cue-font-lock-keywords
         nil nil nil nil
		 (cue-lock-syntactic-face-function . cue-mode--syntactic-face)))
  (setq-local indent-tabs-mode 'only)
  (setq-local tab-width 4)
  (setq-local indent-line-function 'indent-relative)
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (when cue-fmt-at-save
    (cue-format-on-save-mode)))

(define-key cue-mode-map (kbd "C-c C-f") 'cue-format-buffer)

(provide 'cue-mode)
;;; cue-mode.el ends here
