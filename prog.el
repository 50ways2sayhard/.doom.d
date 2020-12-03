;;; prog.el -*- lexical-binding: t; -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPANY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! company
  (setq company-idle-delay 0
        company-tooltip-align-annotations t
        company-tooltip-limit 9
        company-minimum-prefix-length 1
        company-show-numbers t
        company-echo-delay (if (display-graphic-p) nil 0)
        company-require-match 'never
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode)
        )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLYCHECK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! flycheck
  (setq-default flycheck-disabled-checkers
                '(javascript-jshint python-pylint python-pycompile))
  (setq-default flycheck-temp-prefix ".flycheck")
  (after! tide
    (flycheck-add-next-checker 'javascript-eslint '(t . javascript-tide) 'append)
    (flycheck-add-next-checker 'javascript-eslint '(t . jsx-tide) 'append)
    (flycheck-add-next-checker 'typescript-tslint '(t .  typescript-tide) 'append)
    (flycheck-add-next-checker 'javascript-eslint '(t . tsx-tide) 'append))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! sphinx-doc
  :after python
  :config
  (setq sphinx-doc-python-indent 4))

(use-package! lsp-pyright
  ;; :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3") (setq lsp-pyright-python-executable-cmd "python3"))
  :config
  (setq lsp-pyright-venv-path ".venv")
  (setq lsp-pyright-multi-root nil)
  (setq lsp-pyright-use-library-code-for-types t)
  )

(after! python
  (setq python-indent-offset 4
        importmagic-python-interpreter "python"
        flycheck-python-flake8-executable "flake8"))


(use-package! py-isort
  :defer t
  :init
  (setq python-sort-imports-on-save t)
  (defun +python/python-sort-imports ()
    (interactive)
    (when (and python-sort-imports-on-save
               (derived-mode-p 'python-mode))
      (py-isort-before-save)))
  (add-hook! 'python-mode-hook
    (add-hook 'before-save-hook #'+python/python-sort-imports nil t))
  )

(after! lsp
  (add-hook! 'pyvenv-post-activate-hooks #'lsp-restart-workspace)
  (add-hook! 'pyvenv-post-deactivate-hooks #'lsp-restart-workspace)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS, WEB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(after! js2-mode (setq js2-basic-offset 2))
(after! css-mode (setq css-indent-offset 2))

(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))

(use-package! css-in-js
  :config
  (setq css-in-js-enable-indentation nil)
  )
