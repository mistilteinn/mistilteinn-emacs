;;; mistilteinn.el ---bleis workflow

;; Copyright (C) 2011  mzp

;; Author: mzp <mzp@mzpAir.local>
;; Keywords:processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl)

;; ------------------------------
;; config
;; ------------------------------
(defgroup mistilteinn nil
  "New style development with git and emacs"
  :group 'tools)

(defcustom mistilteinn-exclude-modes  '(dummy1-mode dummy2-mode)
  "Major modes `mistilteinn-minor-mode' cannot run."
  :type '(list symbol)
  :group 'mistilteinn)

(defcustom mistilteinn-inactive-ticket-regexp  "\\[解決\\]"
  "Inactive ticket regexp"
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

;; ------------------------------
;; git command
;; ------------------------------
(defun mistilteinn-git-now ()
  "run git-now to create temporary commit"
  (interactive)
  (shell-command "git now --compact"))

(defun mi:branch-list ()
  (remove-if
   '(lambda (s) (string= "" s))
   (split-string (shell-command-to-string "git branch | sed 's/^. *//g'")
                 "\n")))

(defun mistilteinn-git-master ()
  "run git-master to masterize current topic branch"
  (interactive)
  (let* ((branch (completing-read "git-master (default master): " (mi:branch-list) nil nil "master" nil "master"))
         (cmd    (format "git master %s" branch)))
    (shell-command cmd)))

(defun mistilteinn-git-info ()
  "run git-ticket summary to show ticket info"
  (interactive)
  (shell-command "git ticket info" "*mistilteinn-info*"))

(defun mistilteinn-git-ticket-create (subject)
  "run git ticket create to create ticket"
  (interactive "sSubject: ")
  (shell-command (format "git ticket create \"%s\"" subject)))

;; message
(defvar mi:message-buffer "*mistilteinn-message*")

(defun mi:close-message-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun mi:make-message-keymap (f)
  "create keymap for message buffer"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") f)
    (define-key map (kbd "C-g") 'mi:close-message-buffer)
    map))

(defvar mi:message-help
"
# Please enter the commit message for your changes. Lines starting
# with '#' will be ignored, and an empty message aborts the commit.
")

(defvar mi:message-font-locks
  '(("^\\(#.*\\)$"
     (1 font-lock-comment-face t))))

(defun mi:show-message-buffer (f)
  "create commit message buffer"
  (let ((buffer (generate-new-buffer mi:message-buffer)))
    (with-current-buffer buffer
      ;; restore window configure at kill buffer
      (add-hook 'kill-buffer-hook
                (lexical-let ((wc (current-window-configuration)))
                  #'(lambda ()
                      (set-window-configuration wc))) nil t)
      ;; set keybind
      (use-local-map (mi:make-message-keymap f))
      ;; set fontlock
      (font-lock-add-keywords nil mi:message-font-locks)
      (when global-font-lock-mode (font-lock-mode t))
      ;; add help message
      (insert "# C-c C-c: commit; C-g: close buffer\n")
      (save-excursion (insert mi:message-help)))
    (pop-to-buffer buffer)))

(defun mi:git-fixup ()
  (interactive)
  (shell-command (format "git now --fixup \"%s\"" (replace-regexp-in-string "^#.*$" "" (buffer-string))))
  (mi:close-message-buffer))

(defun mistilteinn-git-fixup ()
  "run git-now --fixup to fixup now commit"
  (interactive)
  (mi:show-message-buffer 'mi:git-fixup))

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

;; ------------------------------
;; minor mode
;; ------------------------------
(defvar mistilteinn-minor-mode-map (make-sparse-keymap)
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
  "Return t and enable mistilteinn-mode if `widen-current-window' can called on current buffer."
  (when (and (not (minibufferp (current-buffer)))
             (not (memq major-mode mistilteinn-exclude-modes)))
    (mistilteinn-minor-mode t)))

(define-global-minor-mode global-mistilteinn-mode
  mistilteinn-minor-mode mi:mode-switch)

(provide 'mistilteinn)

;; ------------------------------
;; tests
;; ------------------------------
(dont-compile
  (when (fboundp 'expectations)
    (expectations
     (desc "call create command with subject by mistilteinn-git-ticket-create")
     (expect (mock (shell-command "git ticket create \"ticket title\""))
       (mistilteinn-git-ticket-create "ticket title"))
)))

;;; mistilteinn.el ends here
