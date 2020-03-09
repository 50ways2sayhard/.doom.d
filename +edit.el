;;; ~/.doom.d/+edit.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BETTER EDIT EXP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +scroll-to-center ()
  (evil-scroll-line-to-center (line-number-at-pos))
  )

(advice-add #'save-buffer :after #'+scroll-to-center)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  )

(use-package delete-block
  :load-path (lambda () (expand-file-name ".local/straight/repos/delete-block/" user-emacs-directory))
  :bind
  (("M-d" . delete-block-forward)
   ("C-<backspace>" . delete-block-backward)
   ("M-<backspace>" . delete-block-backward)
   ("M-DEL" . delete-block-backward)))

(use-package highlight-indent-guides
  :if (display-graphic-p)
  :diminish
  :hook ((prog-mode web-mode nxml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-auto-character-face-perc 7))

(use-package pyim
  :demand t
  :config
  (use-package pyim-greatdict
    :config
    (pyim-greatdict-enable)
    )
  :init
  (setq pyim-default-scheme 'pyim-shuangpin)
  (setq pyim-page-tooltip 'posframe)
  (setq pyim-page-length 5)
  )

;; smart kill parens
(use-package! elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))
