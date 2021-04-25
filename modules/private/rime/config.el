;;; private/rime/config.el -*- lexical-binding: t; -*-

(use-package! rime
  :after-call after-find-file pre-command-hook
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-librime-root "~/.local/share/librime/dist")
  (rime-user-data-dir "~/Library/Rime")
  (rime-disable-predicates '(rime-predicate-hydra-p
                             rime-predicate-evil-mode-p
                             rime-predicate-ace-window-p
                             rime-predicate-prog-in-code-p
                             rime-predicate-space-after-cc-p
                             rime-predicate-org-latex-mode-p
                             rime-predicate-org-in-src-block-p
                             rime-predicate-after-ascii-char-p
                             rime-predicate-tex-math-or-command-p
                             rime-predicate-punctuation-line-begin-p
                             rime-predicate-punctuation-after-ascii-p
                             rime-predicate-current-uppercase-letter-p
                             rime-predicate-punctuation-after-space-cc-p))
  :config
  (defadvice! +rime--posframe-display-result-a (args)
    "给 `rime--posframe-display-result' 传入的字符串加一个全角空
格，以解决 `posframe' 偶尔吃字的问题。"
    :filter-args #'rime--posframe-display-result
    (cl-destructuring-bind (result) args
      (let ((newresult (if (string-blank-p result)
                           result
                         (concat result "　"))))
        (list newresult))))

  (load! "+rime-probe-english")
  ;; (load! "+rime-with-evil-escape")
  (load! "+rime-fix-last-candidate")
  )
