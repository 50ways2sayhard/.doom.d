;;; edit.el -*- lexical-binding: t; -*-


(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)
;; -SmoothScroll

(use-package! smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  )

(use-package! color-rg)

;; (use-package! elec-pair
;;   :hook (after-init . electric-pair-mode)
;;   :init
;;   (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
;;   )


(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.

The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))

(add-hook 'before-save-hook #'delete-trailing-whitespace-except-current-line)

(use-package! smartparens
  :hook (prog-mode . smartparens-mode)
  :diminish smartparens-mode
  :custom
  (sp-escape-quotes-after-insert nil)
  )

;; Indent
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 4)
(setq-default js-switch-indent-offset 2)
(add-hook 'after-change-major-mode-hook
          (lambda () (if (equal electric-indent-mode 't)
                         (when (derived-mode-p 'text-mode)
                           (electric-indent-mode -1))
                       (electric-indent-mode 1))))

(use-package! delete-block)

(use-package! awesome-pair
  :hook (prog-mode . awesome-pair-mode)
  ;; :init
  ;; (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
  ;; (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
  ;; (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
  ;; (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
  ;; (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
  ;; (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
  ;; (define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

  ;; (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
  ;; (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

  ;; (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)

  ;; (define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
  ;; (define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
  ;; (define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

  ;; (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
  ;; (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
  ;; (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
  ;; (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
  ;; (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

  ;; (define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
  ;; (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
  ;; (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)
  )
