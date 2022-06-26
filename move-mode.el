(require 'cc-mode)

(eval-when-compile
  (and (= emacs-major-version 24)
       (>= emacs-minor-version 4)
       (require 'cl))
  (require 'cc-langs)
  (require 'cc-fonts))

;; This mode does not inherit properties from other modes. So, we do not use
;; the usual `c-add-language' function.
(eval-and-compile
  (put 'move-mode 'c-mode-prefix "move-"))

;; The following code uses of the `c-lang-defconst' macro define syntactic
;; features of Move language.  Refer to the documentation in the
;; cc-langs.el file for information about the meaning of the -kwds variables.

(c-lang-defconst c-primitive-type-kwds
  move '("u8" "u64" "u128"))


(c-lang-defconst c-modifier-kwds
  move '("public" "acquires" "fun" "let" "mut" "drop" "public(friend)" "public(script)" "const" "has" "friend" "aborts_if" "ensures"))

(c-lang-defconst c-class-decl-kwds
  move '("address" "module" "struct" "script" "spec"))

(c-lang-defconst c-constant-kwds
  move '("true" "false"))

(c-lang-defconst c-other-decl-kwds
  move '("use" "import"))

(c-lang-defconst c-other-kwds
  move '("as" "abort"))

(c-lang-defconst c-identifier-ops
  ;; Handle extended identifiers
  move '((left-assoc "." "::" ":")
         (right-assoc "@" "&" "&mut" "*")))

;; The following keywords do not fit well in keyword classes defined by
;; cc-mode.  So, we approximate as best we can.

(c-lang-defconst c-type-list-kwds
  move '("vector" "signer" "address"))

(c-lang-defconst c-typeless-decl-kwds
  move '("copy" "move_to" "move_from" "borrow_global" "borrow_global_mut" "assert!" "exists" "global"))


(c-lang-defconst c-block-stmt-1-kwds
  move '("loop" "while" "if" "else"))

;; Here we remove default syntax for loops, if-statements and other C
;; syntactic features that are not supported by the Move language.

(c-lang-defconst c-brace-list-decl-kwds
  ;; Remove syntax for C-style enumerations.
  move nil)

(c-lang-defconst c-block-stmt-2-kwds
  move '("loop" "while" "if" "else"))

(c-lang-defconst c-simple-stmt-kwds
  move '("break" "continue"))

(c-lang-defconst c-paren-stmt-kwds
  ;; Remove special case for the "(;;)" in for-loops.
  move nil)

(c-lang-defconst c-label-kwds
  ;; Remove case label syntax for the "case" and "default" keywords.
  move nil)

(c-lang-defconst c-before-label-kwds
  ;; Remove special case for the label in a goto statement.
  move nil)

(c-lang-defconst c-cpp-matchers
  ;; Disable all the C preprocessor syntax.
  move nil)


;; Add support for variable levels of syntax highlighting.
(defconst move-font-lock-keywords-1 (c-lang-const c-matchers-1 move)
  "Minimal highlighting for move-mode.")

(defconst move-font-lock-keywords-2 (c-lang-const c-matchers-2 move)
  "Fast normal highlighting for move-mode.")

(defconst move-font-lock-keywords-3 (c-lang-const c-matchers-3 move)
  "Accurate normal highlighting for move-mode.")

(defvar move-font-lock-keywords move-font-lock-keywords-3
  "Default expressions to highlight in move-mode.")

;; Our syntax table is auto-generated from the keyword classes we defined
;; previously with the `c-lang-const' macro.
(defvar move-mode-syntax-table nil
  "Syntax table used in move-mode buffers.")
(or move-mode-syntax-table
    (setq move-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table move))))

(defvar move-mode-abbrev-table nil
  "Abbreviation table used in move-mode buffers.")

(defvar move-mode-map nil
  "Keymap used in move-mode buffers.")
(or move-mode-map
    (setq move-mode-map (c-make-inherited-keymap)))

(easy-menu-define move-menu move-mode-map
  "Move Mode Commands"
  (cons "Move" (c-lang-const c-mode-menu move)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.move\\'" . move-mode))

;;;###autoload
(defun move-mode ()
  "Major mode for editing Move description language.

The hook `c-mode-common-hook' is run with no argument at mode
initialization, then `move-mode-hook'.

Key bindings:
\\{move-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table move-mode-syntax-table)
  (setq major-mode 'move-mode
        mode-name "Move"
        local-abbrev-table move-mode-abbrev-table
        abbrev-mode t
        c-basic-offset 4
        tab-width 4)
  (use-local-map move-mode-map)
  (c-initialize-cc-mode t)
  (if (fboundp 'c-make-emacs-variables-local)
      (c-make-emacs-variables-local))
  (c-init-language-vars move-mode)
  (c-common-init 'move-mode)
  (easy-menu-add move-menu)
  (c-run-mode-hooks 'c-mode-common-hook 'move-mode-hook)
  (c-update-modeline)
  (setq imenu-generic-expression
	      '(("Struct" "^[[:space:]]*struct[[:space:]]+\\([[:alnum:]]+\\)" 1)
          ("Module" "^[[:space:]]*module[[:space:]]+\\([[:alnum:]]+\\)" 1)
          ("Spec" "^[[:space:]]*spec[[:space:]]+\\([[:alnum:]]+\\)" 1))))

(provide 'move-mode)
;;; move-mode.el ends here
