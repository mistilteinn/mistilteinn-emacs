;;; mistilteinn-test.el ---

(require 'mistilteinn)
(require 'el-expectations)
(require 'el-mock)

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "command: git-now")
      (expect (mock (shell-command "git now --compact"))
        (mistilteinn-git-now))
      (desc "command: git-master")
      (expect (mock (shell-command "git master master"))
        (stub completing-read => "master")
        (mistilteinn-git-master))
      (desc "command: git-ticket")
      (expect (mock (shell-command "git ticket info"))
        (mistilteinn-git-info))
      (desc "command: git-ticket create")
      (expect (mock (shell-command "git ticket create \"ticket title\""))
        (mistilteinn-git-ticket-create "ticket title"))
      (desc "command: git-now --fixup")
      (expect (mock (shell-command "git now --fixup \"ticket\""))
         (mi:git-fixup "ticket"))

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
        (lookup-key mi:message-keymap (kbd "C-g")))
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
      )))

(provide 'mistilteinn-test)
;;; mistilteinn-test.el ends here
