;;; private/rime/+rime-fix-last-candidate.el -*- lexical-binding: t; -*-


(defun +rime--posframe-display-content-a (args)
  "给 `rime--posframe-display-content' 传入的字符串加一个全角空
格，以解决 `posframe' 偶尔吃字的问题。"
  (cl-destructuring-bind (content) args
    (let ((newresult (if (string-blank-p content)
                         content
                       (concat content "　"))))
      (list newresult))))

(if (fboundp 'rime--posframe-display-content)
    (advice-add 'rime--posframe-display-content
                :filter-args
                #'+rime--posframe-display-content-a)
  (error "Function `rime--posframe-display-content' is not available."))
