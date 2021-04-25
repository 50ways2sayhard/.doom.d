;;; private/rime/+rime-with-evil-escape.el -*- lexical-binding: t; -*-


(defun rime-evil-escape-advice (orig-fun key)
  "advice for `rime-input-method' to make it work together with `evil-escape'.
	Mainly modified from `evil-escape-pre-command-hook'"
  (if rime--preedit-overlay
      ;; if `rime--preedit-overlay' is non-nil, then we are editing something, do not abort
      (apply orig-fun (list key))
    (when (featurep 'evil-escape)
      (let* (
	     (fkey (elt evil-escape-key-sequence 0))
	     (skey (elt evil-escape-key-sequence 1))
	     (evt (read-event nil nil evil-escape-delay))
	     )
	(cond
	 ((and (characterp evt)
	       (or (and (char-equal key fkey) (char-equal evt skey))
		   (and evil-escape-unordered-key-sequence
			(char-equal key skey) (char-equal evt fkey))))
	  (evil-repeat-stop)
	  (evil-normal-state))
	 ((null evt) (apply orig-fun (list key)))
	 (t
	  (apply orig-fun (list key))
	  (if (numberp evt)
	      (apply orig-fun (list evt))
	    (setq unread-command-events (append unread-command-events (list evt))))))))))

(advice-add 'rime-input-method :around #'rime-evil-escape-advice)
