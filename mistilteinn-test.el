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
         (stub buffer-string => "ticket")
         (mi:git-fixup))

      (desc "message buffer: close-message-buffer should kill current buffer")
      (expect nil
        (let ((buffer (generate-new-buffer "test")))
          (with-current-buffer buffer
            (mi:close-message-buffer)
            (buffer-live-p buffer))))
      (desc "message buffer: keymap")
      (expect 'mi:close-message-buffer
        (lookup-key mi:message-keymap (kbd "C-g")))
      (expect 'mi:commit-message-buffer
        (lookup-key mi:message-keymap (kbd "C-c C-c")))
      (desc "message buffer: callback after commit messeage")
      (expect (mock (callback))
        (mi:show-message-buffer 'callback)
        (mi:commit-message-buffer))
      (desc "message buffer: buffer")
      (expect mistilteinn-message-buffer
        (mi:show-message-buffer '(lambda ()))
        (prog1
            (buffer-name)
            (mi:close-message-buffer)))
      (expect nil
        (mi:show-message-buffer '(lambda ()))
        (let ((buffer (current-buffer)))
          (mi:close-message-buffer)
          (buffer-live-p buffer)))
      (expect nil
        (mi:show-message-buffer '(lambda ()))
        (let ((buffer (current-buffer)))
          (mi:commit-message-buffer)
          (buffer-live-p buffer)))
      )))

(provide 'mistilteinn-test)
;;; mistilteinn-test.el ends here
