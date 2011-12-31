;;; -*- coding: utf-8 -*-
;;; mistilteinn.el ---bleis workflow
;; Copyright (C) 2011  mzp


;;; Code:
(require 'cl)

;;;; configure
;;; The definition of mistilteinn.
(defgroup mistilteinn nil
  "New style development with git and emacs"
  :group 'tools)

(defcustom mistilteinn-exclude-modes  '(dummy1-mode dummy2-mode)
  "Major modes `mistilteinn-minor-mode' cannot run."
  :type '(list symbol)
  :group 'mistilteinn)

(defcustom mistilteinn-inactive-ticket-regexp  "\\[解決\\]"
  "A regexp for inactive ticket regexp"
  :type 'string
  :group 'mistilteinn)

(defcustom mistilteinn-info-buffer "*mistilteinn-info*"
  "Buffer name for information"
  :type 'string
  :group 'mistilteinn)

(defcustom mistilteinn-message-buffer "*mistilteinn-message*"
  "Buffer name for message"
  :type 'string
  :group 'mistilteinn)

(defface mistilteinn-inactive-ticket-face
  '((t (:foreground "blue")))
  "*Face used for inactive ticket"
  :group 'mistilteinn)

(defface mistilteinn-active-ticket-face
  '()
  "*Face used for active ticket"
  :group 'mistilteinn)

;;;; message buffer function
;;; Create message buffer and popup for user to input commit message etc.
(defun mi:close-message-buffer ()
  "Close current message buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun mi:strip-comment (s)
  "strip comment heading #"
  (replace-regexp-in-string "^#.*\n" "" s))

(defvar mi:commit (lambda () nil))
(make-variable-buffer-local 'mi:commit)
(defun mi:commit-message-buffer ()
  "Commit current message buffer."
  (interactive)
  (funcall mi:commit (mi:strip-comment (buffer-string)))
  (mi:close-message-buffer))

(defvar mi:message-keymap
  (make-sparse-keymap)
  "A keymap for message buffer. ")

(define-key mi:message-keymap (kbd "C-c C-c") 'mi:commit-message-buffer)
(define-key mi:message-keymap (kbd "C-c C-q") 'mi:close-message-buffer)

(defconst mi:message-help
"
# Please enter the commit message for your changes. Lines starting
# with '#' will be ignored, and an empty message aborts the commit.
")

(defconst mi:message-font-locks
  '(("^\\(#.*\\)$" (1 font-lock-comment-face t))   ;; highlight comment
    ))

(defun mi:show-message-buffer (f)
  "Show message buffer and callback `f' when user input is completed."
  (let ((buffer (generate-new-buffer mistilteinn-message-buffer)))
    (with-current-buffer buffer
      ;; restore window configure at kill buffer
      (add-hook 'kill-buffer-hook
                (lexical-let ((wc (current-window-configuration)))
                  #'(lambda ()
                      (set-window-configuration wc))) nil t)
      (setq mi:commit f)
      ;; set keybind
      (use-local-map mi:message-keymap)
      ;; set fontlock
      (font-lock-add-keywords nil mi:message-font-locks)
      (when global-font-lock-mode (font-lock-mode t))
      ;; add help message
      (insert "# C-c C-c: commit; C-c C-q: close buffer\n")
      (save-excursion (insert mi:message-help)))
    (pop-to-buffer buffer)))

;;;; git command
(defun mistilteinn-git-now ()
  "run git-now to create temporary commit"
  (interactive)
  (shell-command "git now --compact"))

(defun mi:branch-list ()
  "Get branch list for current repository."
  (remove-if '(lambda (s) (string= "" s))
             (mapcar '(lambda (s) (replace-regexp-in-string "^*? *" "" s))
                     (split-string (shell-command-to-string "git branch")
                                   "\n"))))

(defun mistilteinn-git-master ()
  "run git-master to masterize current topic branch"
  (interactive)
  (let* ((branch (completing-read "git-master (default master): " (mi:branch-list) nil nil "master" nil "master"))
         (cmd    (format "git master %s" branch)))
    (shell-command cmd)))

(defun mistilteinn-git-info ()
  "run git-ticket summary to show ticket info"
  (interactive)
  (shell-command "git ticket info" mistilteinn-info-buffer))

(defun mistilteinn-git-ticket-create (subject)
  "Create ticket."
  (interactive "sSubject: ")
  (shell-command (format "git ticket create \"%s\"" subject)))

(defun mi:git-fixup (s)
  (shell-command (format "git now --fixup \"%s\"" s)))

(defun mistilteinn-git-fixup ()
  "run git-now --fixup to fixup now commit"
  (interactive)
  (mi:show-message-buffer 'mi:git-fixup))


(defmacro mi:with-cd (path &rest body)
  (let ((var (make-symbol "path")))
    `(let ((,var default-directory))
       (unwind-protect
           (progn
             (cd ,path)
             ,@body)
         (cd ,var)))))

(defun mi:git-dir-p (path)
  "Check `path' is git repository"
  (save-excursion (mi:with-cd path (eq 0 (shell-command "git rev-parse")))))

;; ------------------------------
;; anything
;; ------------------------------
(defun mi:switch-topic-branch (str)
  (shell-command (format "git ticket switch %s" (car (split-string str " ")))
                 "*git-ticket*"))

(defun mi:highlight-ticket (tickets)
  (loop for ticket in tickets
      collect
      (cond
       ((string-match mistilteinn-inactive-ticket-regexp ticket) (propertize ticket 'face 'mistilteinn-inactive-ticket-face))
       (t (propertize ticket 'face 'mistilteinn-active-ticket-face)))))

(defvar anything-c-source-git-ticket
  '((name . "Tickets")
    (candidates-in-buffer)
    (candidate-transformer mi:highlight-ticket)
    (init . (lambda () (call-process-shell-command "git ticket list" nil (anything-candidate-buffer 'git-ticket))))
    (action ("Switch topic branch" . mi:switch-topic-branch))))

;;;; minor mode
(defconst mistilteinn-minor-mode-map (make-sparse-keymap)
  "Keymap for the mistilteinn minor mode.")

(define-key mistilteinn-minor-mode-map (kbd "C-c # c") 'mistilteinn-git-ticket-create)
(define-key mistilteinn-minor-mode-map (kbd "C-c # m") 'mistilteinn-git-master)
(define-key mistilteinn-minor-mode-map (kbd "C-c # n") 'mistilteinn-git-now)
(define-key mistilteinn-minor-mode-map (kbd "C-c # i") 'mistilteinn-git-info)
(define-key mistilteinn-minor-mode-map (kbd "C-c # f") 'mistilteinn-git-fixup)

(define-minor-mode mistilteinn-minor-mode
  "mistilteinn"
  :lighter " mi"
  :keymap mistilteinn-minor-mode-map
  :group mistilteinn-mode
  (funcall (if mistilteinn-minor-mode 'add-hook 'remove-hook)
           'after-save-hook
           'mistilteinn-git-now))

(defun mi:mode-switch ()
  "Return t and enable mistilteinn-minor-mode if `mistilteinn-minor-mode' can called on current buffer."
  (when (and (not (minibufferp (current-buffer)))
             (not (memq major-mode mistilteinn-exclude-modes))
             (mi:git-dir-p "."))
    (mistilteinn-minor-mode t)))

(define-global-minor-mode global-mistilteinn-mode
  mistilteinn-minor-mode mi:mode-switch)

(provide 'mistilteinn)
;;; mistilteinn.el ends here
