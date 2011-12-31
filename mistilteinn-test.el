;;; mistilteinn-test.el ---

(require 'mistilteinn)
(require 'el-expectations)
(require 'el-mock)

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "call create command with subject by mistilteinn-git-ticket-create")
      (expect (mock (shell-command "git ticket create \"ticket title\""))
        (mistilteinn-git-ticket-create "ticket title"))
      (desc "call master command")
      (expect (mock (shell-command "git master master"))
        (stub completing-read => "master")
        (mistilteinn-git-master))
      (desc "call now command")
      (expect (mock (shell-command "git now --compact"))
        (mistilteinn-git-now))
      (desc "call fixup command")
      (expect (mock (shell-command "git now --fixup \"ticket\""))
        (stub mi:close-message-buffer)
        (stub buffer-string => "ticket")
        (mi:git-fixup))
      (desc "message buffer")
      (expect nil
        (let ((buffer (generate-new-buffer "test")))
          (with-current-buffer buffer
            (mi:close-message-buffer)
            (buffer-live-p buffer))))
      )))

(provide 'mistilteinn-test)
;;; mistilteinn-test.el ends here
