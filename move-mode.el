(require 'cc-mode)

(eval-when-compile
  (and (= emacs-major-version 24)
       (>= emacs-minor-version 4)
       (require 'cl))
  (require 'cc-align)
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
  move '("public fun" "fun" "let" "let mut" "public(friend) fun" "public(script) fun" "const" "friend"))


(c-lang-defconst c-decl-prefix-re
  ;; Same as for C, except it does not match "(". This is needed for disabling
  ;; the syntax for casts.
  move"\\([\{\};,]+\\)")

(c-lang-defconst c-type-prefix-kws
  move '("struct"))

(c-lang-defconst c-class-decl-kwds
  move '("struct"))

(c-lang-defconst c-constant-kwds
  move '("true" "false"))

(c-lang-defconst c-other-decl-kwds
  move '( "module" "script" "spec"))

(c-lang-defconst c-typeof-kwds
  move '("as"))

(c-lang-defconst c-other-kwds
  move '("abort"))

(c-lang-defconst c-use-kwds
  move '("use"))

(c-lang-defconst c-identifier-ops
  ;; Handle extended identifiers
  move '((left-assoc "." "::" ":")
         (right-assoc "@" "&" "&mut" "*")))

;; The following keywords do not fit well in keyword classes defined by
;; cc-mode.  So, we approximate as best we can.

(c-lang-defconst c-type-list-kwds
  move '("vector" "signer" "address"))

(c-lang-defconst c-typeless-decl-kwds
  move '("acquires" "has" "aborts_if" "ensures") )


(c-lang-defconst c-block-stmt-1-kwds
  move '("loop" "while" "if" "else" "address"))

;; Here we remove default syntax for loops, if-statements and other C
;; syntactic features that are not supported by the Move language.

(c-lang-defconst c-brace-list-decl-kwds
  ;; Remove syntax for C-style enumerations.
  move nil)

(c-lang-defconst c-block-stmt-2-kwds
  move '("loop" "while" "if" "else"))

(c-lang-defconst c-simple-stmt-kwds
  move '("break" "continue" "return"))

(c-lang-defconst c-paren-stmt-kwds
  ;; Remove special case for the "(;;)" in for-loops.
  move nil)

(c-lang-defconst c-label-kwds
  ;; Remove case label syntax for the "case" and "default" keywords.
  move nil)

(c-lang-defconst c-before-label-kwds
  ;; Remove special case for the label in a goto statement.
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

  ;; Arglist
  (c-set-offset 'arglist-cont
                '(c-lineup-arglist-operators 0))
  (c-set-offset 'arglist-cont-nonempty
                '(c-lineup-arglist-operators c-lineup-arglist))
  (c-set-offset 'arglist-close
                '(c-lineup-under-anchor))

  ;; CC mode recognized struct fields as a statement.
  ;; Here we made the offset to 0 to fix its indentation.
  (c-set-offset 'statement-cont 0)

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
