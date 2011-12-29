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

;; ------------------------------
;; git command
;; ------------------------------
(defun mistilteinn-git-now ()
  (interactive)
  (shell-command "git now"))

;; ------------------------------
;; anything
;; ------------------------------
(defun mi:switch-topic-branch (str)
  (shell-command (format "git ticket switch %s" (car (split-string str " ")))
                 "*git-ticket*"))

(defvar anything-c-source-git-ticket
  '((name . "Tickets")
    (candidates-in-buffer)
    (init . (lambda () (call-process-shell-command "git ticket list" nil (anything-candidate-buffer 'git-ticket))))
    (action ("Switch topic branch" . mi:switch-topic-branch))))

;; ------------------------------
;; minor mode
;; ------------------------------
(defvar *mistilteinn-exclude-mode* '())

(defvar mistilteinn-mode-map (make-sparse-keymap)
  "Keymap for the mistilteinn minor mode.")

(define-minor-mode mistilteinn-minor-mode
  "mistilteinn"
  :lighter " mi"
  :group mistilteinn-mode
  (funcall (if mistilteinn-minor-mode 'add-hook 'remove-hook)
           'after-save-hook
           'mistilteinn-git-now))

(defun mi:mode-switch ()
  "Return t and enable mistilteinn-mode if `widen-current-window' can called on current buffer."
  (when (and (not (minibufferp (current-buffer)))
             (not (memq major-mode *mistilteinn-exclude-mode*)))
    (mistilteinn-minor-mode t)))

(define-global-minor-mode global-mistilteinn-mode
  mistilteinn-minor-mode mi:mode-switch)

(provide 'mistilteinn)
;;; mistilteinn.el ends here
