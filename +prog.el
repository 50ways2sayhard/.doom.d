;;; ~/.doom.d/+prog.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPANY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company//sort-by-tabnine (candidates)
  (if (or (functionp company-backend)
          (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      candidates
    (let ((candidates-table (make-hash-table :test #'equal))
          candidates-1
          candidates-2)
      (dolist (candidate candidates)
        (if (eq (get-text-property 0 'company-backend candidate)
                'company-tabnine)
            (unless (gethash candidate candidates-table)
              (push candidate candidates-2))
          (push candidate candidates-1)
          (puthash candidate t candidates-table)))
      (setq candidates-1 (nreverse candidates-1))
      (setq candidates-2 (nreverse candidates-2))
      (nconc (seq-take candidates-1 2)
             (seq-take candidates-2 2)
             (seq-drop candidates-1 2)
             (seq-drop candidates-2 2)))))


(after! company
  (setq company-idle-delay 0
        company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-minimum-prefix-length 1
        company-show-numbers t
        company-echo-delay (if (display-graphic-p) nil 0)
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-backends '(company-capf)
        company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode)
        company-frontends '(company-pseudo-tooltip-frontend
                            ;; company-preview-frontend
                            company-echo-metadata-frontend
                            ))
  ;; (add-to-list 'company-transformers 'company//sort-by-tabnine t)
  )

(def-package! company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))

(after! company-lsp
  (setq +lsp-company-backend '(company-lsp :with company-tabnine :separate))
  )

(use-package! company-tabnine
  :defer 1
  :custom
  (company-tabnine-max-num-results 9)
  :hook
  (lsp-after-open . (lambda ()
                      (setq company-tabnine-max-num-results 3)
                      (add-to-list 'company-transformers 'company//sort-by-tabnine t)
                      ;; (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))
                      (setq company-backends '((company-lsp :with company-tabnine :separate) company-files company-dabbrev))
                      ))
  (kill-emacs . company-tabnine-kill-process)
  :config
  ;; Enable TabNine on default
  (set-company-backend! 'prog-mode 'company-tabnine 'company-capf 'company-yasnippet)
  )


(def-package! counsel-tramp
  :commands (counsel-tramp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLYCHECK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cspell-base-program "cspell")
(defvar cspell-config-file-path (concat "'" (expand-file-name  "~/Dotfiles/misc/apps/.cspell.json") "'"))
(defvar cspell-args (string-join `("--config" ,cspell-config-file-path) " "))
(defun cspell-check-buffer ()
  (interactive)
  (if cspell-base-program
      (let* ((file-name (concat "'" (file-name-nondirectory (buffer-file-name)) "'"))
             (command (string-join `(,cspell-base-program ,cspell-args ,file-name) " ")))
        (compilation-start command 'grep-mode))
    (message "Cannot find cspell, please install with `npm install -g csepll`")
    ))

(defun cspell-check-directory ()
  (interactive)
  (if cspell-base-program
      (let* ((project-root (doom-project-root))
             (default-directory
               (if (string-match-p "av/detection" project-root)
                   (expand-file-name "~/av")
                 project-root))
             (command (string-join `("git diff --name-only origin/develop | xargs -I{}" ,cspell-base-program ,cspell-args "'{}'") " ")))
        (compilation-start command 'grep-mode))
    (message "Cannot find cspell, please install with `npm install -g csepll`")))

;; (use-package! wucuo
;;   :defer t
;;   :init
;;   (add-hook! (js2-mode rjsx-mode go-mode c-mode c++-mode) #'wucuo-start))


(after! flycheck
  (setq-default flycheck-disabled-checkers
                '(
                  javascript-jshint handlebars
                  json-jsonlist json-python-json
                  c/c++-clang c/c++-cppcheck c/c++-gcc
                  python-pylint python-pycompile
                  ))

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; ======================== JS & TS ========================
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (after! tide
    (flycheck-add-next-checker 'javascript-eslint '(t . javascript-tide) 'append)
    (flycheck-add-next-checker 'javascript-eslint '(t . jsx-tide) 'append)
    (flycheck-add-next-checker 'typescript-tslint '(t .  typescript-tide) 'append)
    (flycheck-add-next-checker 'javascript-eslint '(t . tsx-tide) 'append))

  ;; ======================== Python ========================
  ;; (require 'flycheck-mypy)

  )

(defun disable-flycheck-mode ()
  (flycheck-mode -1))
;; (add-hook! (emacs-lisp-mode) 'disable-flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :after lsp-mode python
  :custom
  (lsp-python-ms-dir "~/.local/mspyls/")
  (lsp-python-executable-cmd "python3")
  )
;; (define-derived-mode asl-mode
;;   python-mode "ARGO Schema Language Mode"
;;   "Major mode for asl file."
;;   (flycheck-mode -1))
;; (add-to-list 'auto-mode-alist '("\\.asl\\'" . asl-mode))

(after! python
  (setq python-indent-offset 4
        importmagic-python-interpreter "python"
        flycheck-python-pylint-executable "pylint"
        flycheck-python-flake8-executable "flake8"))

;; if you use pyton2, then you could comment the following 2 lines
;; (setq python-shell-interpreter "python2"
;;       python-shell-interpreter-args "-i")

;; ignore some linting info
;; (if (featurep! :tools lsp)
;;     (setq lsp-pyls-plugins-pycodestyle-ignore '("E501")
;;           lsp-pyls-plugins-pylint-args [ "--errors-only" ]))
;; )


(after! lsp-python-ms
  (setq lsp-python-ms-python-executable-cmd "python"))


(add-hook 'pyvenv-post-activate-hooks (lambda () (lsp-restart-workspace)))

(add-hook 'python-mode-hook
          (lambda ()
            (setq company-backends '(company-files
                                     company-dabbrev
                                     ))))

(setq +lsp-company-backend '(company-lsp company-tabnine :with company-yasnippet))


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


(use-package! importmagic
  :defer t
  :hook (python-mode . importmagic-mode)
  :commands (importmagic-fix-imports importmagic-fix-symbol-at-point)
  :config
  (dolist (func '(importmagic-fix-imports importmagic-fix-symbol-at-point))
    (advice-add func :before #'revert-buffer-no-confirm)))

;; (use-package! poetry
;;   :ensure t)


(after! pipenv
  (setq pipenv-with-projectile t)
  ;; Override pipenv--clean-response to trim color codes
  (defun pipenv--clean-response (response)
    "Clean up RESPONSE from shell command."
    (replace-regexp-in-string "\n\\[0m$" "" (s-chomp response)))

  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(pipenv-activate pipenv-deactivate))
    (advice-add func :after #'reset-flycheck)))


;; For pytest-mode
(set-evil-initial-state! '(comint-mode) 'normal)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS, WEB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package! import-js
  :defer t
  :init
  (add-hook! (js2-mode rjsx-mode) (run-import-js))
  ;; (add-hook! (js2-mode rjsx-mode)
  ;;   (add-hook 'after-save-hook #'import-js-fix nil t)))
  )
(advice-add '+javascript|cleanup-tide-processes :after 'kill-import-js)
(after! js2-mode (setq js2-basic-offset 2))
(after! css-mode (setq css-indent-offset 2))


(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (after! go-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP & DAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! lsp-treemacs
  :defer t
  :config
  (lsp-treemacs-sync-mode 1))

(after! lsp-mode
  (setq lsp-use-native-json t
        lsp-log-io nil)
  (dolist (dir '("[/\\\\]\\.ccls-cache$"
                 "[/\\\\]\\.mypy_cache$"
                 "[/\\\\]\\.pytest_cache$"
                 "[/\\\\]\\.cache$"
                 "[/\\\\]\\.clwb$"
                 "[/\\\\]_build$"
                 "[/\\\\]__pycache__$"
                 "[/\\\\]bazel-bin$"
                 "[/\\\\]bazel-code$"
                 "[/\\\\]bazel-genfiles$"
                 "[/\\\\]bazel-out$"
                 "[/\\\\]bazel-testlogs$"
                 "[/\\\\]third_party$"
                 "[/\\\\]third-party$"
                 ))
    (push dir lsp-file-watch-ignored))
  )


;; (add-hook 'lsp-ui-mode-hook (λ!! #'lsp-ui-doc-mode -1))

(after! lsp-ui
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable t
        ;; lsp-ui-doc-use-childframe t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 30
        lsp-ui-doc-max-width 100
        lsp-ui-sideline-enable nil
        lsp-ui-peek-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-code-actions nil
        )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG & RUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! quickrun
  (quickrun-add-command "c++/c1z"
    '((:command . "clang++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++"))


(after! realgud (advice-remove #'realgud:terminate #'+debugger--cleanup-after-realgud-a))


(defun +my/dap-start ()
  (interactive)
  (dap-mode 1)
  (call-interactively #'dap-debug))

(add-hook! dap-mode-hook ((dap-tooltip-mode 1) (tooltip-mode 1)))

(after! dap-mode
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))
