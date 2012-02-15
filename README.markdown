Mistilteinn for Emacs
======================

Install
------------------------------

Copy `mistilteinn.el` to `load-path`, and add your .emacs:

    (add-to-list 'load-path "~/workspaces/mistilteinn/")
    (require 'mistilteinn)

    ;; for minor mode
    (global-mistilteinn-mode t)

    ;; for anything
    (defun anything-for-mistiltein ()
      (interactive)
      (anything-other-buffer
       '(anything-c-source-git-ticket)
       "*mistiltein*"))
    (define-key global-map (kbd "C-t") 'anything-for-mistiltein))

Authors
----------------

 * @mzp
 * @suer
 * @mallowlabs

Acknowledge
----------------

 * @bleis (original mistilteinn author)