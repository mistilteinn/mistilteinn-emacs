;; -*- coding: utf-8 -*-
;;; mistilteinn-test.el ---

(require 'mistilteinn)
(require 'el-expectations)
(require 'el-mock)

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "mistilteinn inside")
      (expect t
        (mi:inside-p "."))
      (expect nil
        (mi:inside-p "/"))
      (desc "command: git-now")
      (expect (mock (shell-command "git now --compact"))
        (mistilteinn-git-now))
      (desc "command: git-master")
      (expect (mock (shell-command "git master master"))
        (stub completing-read => "master")
        (mistilteinn-git-master))
      (desc "command: mistilteinn create")
      (expect (mock (shell-command "mistilteinn create \"ticket title\""))
        (mistilteinn-git-ticket-create "ticket title"))
      (desc "command: git-now --fixup")
      (expect (mock (shell-command "git now --fixup \"ticket\""))
         (mi:git-fixup "ticket"))
      (desc "command: git-now --diff")
      (expect (mock (shell-command "git now --diff" "buf"))
        (mi:git-diff "buf"))
      (desc "message buffer: close-message-buffer should kill current buffer")
      (expect nil
        (let ((buffer (generate-new-buffer "test")))
          (with-current-buffer buffer
            (mi:close-message-buffer)
            (buffer-live-p buffer))))
      (desc "message buffer: strip comment")
      (expect "bar\n"
        (mi:strip-comment "# foo
bar
# baz
")
        )
      (desc "message buffer: keymap")
      (expect 'mi:close-message-buffer
        (lookup-key mi:message-keymap (kbd "C-c C-q")))
      (expect 'mi:commit-message-buffer
        (lookup-key mi:message-keymap (kbd "C-c C-c")))
      (desc "message buffer: callback after commit messeage")
      (expect (mock (callback "foobar
"))
        (mi:show-message-buffer 'callback)
        (insert "foobar")
        (mi:commit-message-buffer))
      (desc "message buffer: buffer")
      (expect mistilteinn-message-buffer
        (mi:show-message-buffer '(lambda (s)))
        (prog1
            (buffer-name)
            (mi:close-message-buffer)))
      (expect nil
        (mi:show-message-buffer '(lambda (s)))
        (let ((buffer (current-buffer)))
          (mi:close-message-buffer)
          (buffer-live-p buffer)))
      (expect nil
        (mi:show-message-buffer '(lambda (s)))
        (let ((buffer (current-buffer)))
          (mi:commit-message-buffer)
          (buffer-live-p buffer)))

      (desc "git util")
      (expect '("foo" "bar" "baz" "master")
        (stub shell-command-to-string => "  foo
* bar
  baz
  master
")
        (mi:branch-list))

      (desc "mi:close-message-buffer")
      (expect (mock (kill-buffer *))
        (mi:close-message-buffer))
      (desc "mi:switch-topic-branch")
      (expect (mock (shell-command "git branch id/100 2>/dev/null; git checkout id/100"))
        (mi:switch-topic-branch "100"))
      (desc "highlight a resolved ticket inactive")
      (expect 'mistilteinn-inactive-ticket-face
        (get-text-property 1 'face (car (mi:highlight-ticket '("[解決] id/100")))))
      (desc "highlight a unresolved ticket active")
      (expect 'mistilteinn-active-ticket-face
        (get-text-property 1 'face (car (mi:highlight-ticket '("id/100")))))
      )))

(provide 'mistilteinn-test)
;;; mistilteinn-test.el ends here
