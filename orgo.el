;;; test.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Ben Postlethwaite

;; Author: Ben Postlethwaite
;; Keywords: lisp
;; Version: 0.0.1

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

;; Put a description of the package here

;;; Code:

(require 'json)
(require 'cl-lib)

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun extract-issue-title (pair) (car (cdr pair)))
(defun extract-values (vec) (mapcar 'cdr vec))

(defun get-issues (state)
  "return an issue which is tuple like structure (url title)"
  (let ((json-array-type 'list)
        (json-object-type 'alist)
        (cmd (format "/home/ben/programming/go/bin/orgo %s" state)))
    (mapcar 'extract-values (json-read-from-string
                             (shell-command-to-string cmd)))))

(defun insertion-point ()
  "Header insertion points. Find the first headline with :sync: true tag"
  (car
   (cl-remove nil
              (org-map-entries
               '(if (string=(org-entry-get (point) "sync") "true")
                    (progn
                      (show-children)
                      (point))
                  nil)))))

(defun get-todo-titles ()
  "Get all titles of headline todos.
This will only find headline todos (not other org-mode elements)
Must implement other search criteria with org-element-map.
Note that org-element-property :title dismisses the URL but we need to trim
the extra white space from the end of the title and the start of the URL"
  (mapcar (lambda (hl) (s-trim-right (car (org-element-property :title hl))))
          (org-element-map
              (org-element-parse-buffer)
              'headline
            (lambda (hl) (if (org-element-property :todo-keyword hl) hl nil )))))

(defun get-issues-to-create ()
  "for each issue in open github issues check if the issue isn't in the current todos
and it isn't return it"
  (let ((open-issues (get-issues "open"))
        (todo-titles (get-todo-titles)))
    (cl-remove nil
               (mapcar (lambda (issue) (if (member (extract-issue-title issue) todo-titles) nil issue))
                       open-issues))))

(defun get-todos-to-close ()
  "for each issue in closed github issues check if the issue is in the current todos
and if it is return it"
  (let ((closed-issues (get-issues "closed"))
        (todo-titles (get-todo-titles)))
    (cl-remove nil
               (mapcar (lambda (issue) (if (member (extract-issue-title issue) todo-titles) issue nil))
                       closed-issues))))

(defun close-todos-by-title (title)
  (progn
    (goto-char 1)
    (while (search-forward title nil t)
      (org-todo 'done))))

(defun insert-issues-as-todos (issues pos)
  (mapc
   (lambda (issue)
     (progn
       (goto-char pos)
       (org-insert-heading-after-current)
       (org-do-demote)
       (insert (format "TODO %s %s" (extract-issue-title issue) (car issue)))))
   issues))


(defun sync-todos ()
  (interactive)
  (progn
    (mapcar 'close-todos-by-title (mapcar 'extract-issue-title (get-todos-to-close)))
    (insert-issues-as-todos (get-issues-to-create) (insertion-point))))


(provide 'orgo)
;;; orgo.el ends here
