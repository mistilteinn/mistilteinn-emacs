Mistilteinn for Emacs
======================

Requirement
----------------

 * Ruby 1.8.7

Setup
------------------------------

Copy subcommand to PATH:

    $ cp git-* /path/to/bin

Add your .emacs:

    (add-to-list 'load-path "~/workspaces/mistilteinn/")
    (require 'mistilteinn)

    (defun anything-for-mistiltein ()
      (interactive)
      (anything-other-buffer
       '(anything-c-source-git-ticket)
       "*mistiltein*"))
    (define-key global-map (kbd "C-t") 'anything-for-mistiltein))

Copy hooks to project:

    $ cp hooks/* /path/to/project/.git/hooks

Authors
----------------

 * @mzp
 * @suer
 * @mallowlabs


Acknowledge
----------------

 * @bleis (original mistilteinn author)
 * @tosikawa (git-now author)