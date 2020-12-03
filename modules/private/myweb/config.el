;;; private/myweb/config.el -*- lexical-binding: t; -*-

(after! js2-mode
  (setq js2-basic-offset 2)
  (add-hook! 'before-save-hook #'format-all-buffer))
(after! css-mode (setq css-indent-offset 2))

(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))

(use-package! css-in-js
  :config
  (setq css-in-js-enable-indentation nil)
  )

(after! flycheck
  (after! tide
    (flycheck-add-next-checker 'javascript-eslint '(t . javascript-tide) 'append)
    (flycheck-add-next-checker 'javascript-eslint '(t . jsx-tide) 'append)
    (flycheck-add-next-checker 'typescript-tslint '(t .  typescript-tide) 'append)
    (flycheck-add-next-checker 'javascript-eslint '(t . tsx-tide) 'append))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  )
