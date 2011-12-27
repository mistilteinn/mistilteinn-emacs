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
(defun mi:switch-topic-branch (str)
  (shell-command (format "git ticket %s" (car (split-string str " ")))
                 "*git-ticket*"))

(defvar anything-c-source-git-ticket
  '((name . "Tickets")
    (candidates-in-buffer)
    (init . (lambda () (call-process-shell-command "git-ticket" nil (anything-candidate-buffer 'git-ticket))))
    (action ("Switch topic branch" . mi:switch-topic-branch))))

(provide 'mistilteinn)
;;; mistilteinn.el ends here
