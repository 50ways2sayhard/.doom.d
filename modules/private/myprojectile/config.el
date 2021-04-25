;;; private/myprojectile/config.el -*- lexical-binding: t; -*-


(use-package! projectile
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  )
