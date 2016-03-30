;; -*- mode: Lisp; lexical-binding: t; -*-

(require 'json)
(require 'cl-lib)

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun get-value (item key)
  (cond ((string= key "URL") (nth 0 item))
        ((string= key "Title") (nth 1 item))
        ((string= key "Description") (nth 2 item))
        ((string= key "Date") (nth 3 item))))

(defun get-item-title (item) (get-value item "Title"))
(defun get-item-URL (item) (get-value item "URL"))
(defun get-item-date (item) (get-value item "Date"))

(defun nth-elem (element xs)
  "Return zero-indexed position of ELEMENT in list XS, or nil if absent."
  (let ((idx  0))
    (catch 'nth-elt
      (dolist (x  xs)
        (when (equal element x) (throw 'nth-elt idx))
        (setq idx  (1+ idx)))
      nil)))

(defun format-json-resp (vec)
  (let ((values (mapcar 'cdr vec))
        (keys (mapcar 'car vec)))
    (list (nth (nth-elem "URL" keys) values)
          (nth (nth-elem "Title" keys) values)
          (nth (nth-elem "Description" keys) values)
          (nth (nth-elem "Date" keys) values))))

(defun get-issues (service &rest args)
  "return an issue which is tuple structure (url title)"
  (let ((json-array-type 'list)
        (json-object-type 'alist)
        (json-key-type 'string)
        (cmd (format "/home/ben/programming/go/bin/orgo %s %s" service (mapconcat 'identity args " "))))
    (mapcar 'format-json-resp (json-read-from-string
                               (shell-command-to-string cmd)))))

(defun insertion-point ()
  "Header insertion points. Find the first headline with :sync: true tag"
  (car
   (cl-remove nil
              (org-map-entries
               '(if (string=(org-entry-get (point) "orgo-insert") "github")
                    (progn
                      (show-children)
                      (point))
                  nil)))))

(defun get-todo-items ()
  "Get (URL title) pairs of all todos in document."
  (mapcar
   (lambda (comps) (list (nth 6 comps) (nth 4 comps)))
   (remove-if-not
    (lambda (comps) (and (nth 2 comps) (nth 6 comps)) )
    (org-map-entries
     (lambda () (append (org-heading-components) (list (org-entry-get (point) "URL"))))))))


(defun get-issues-not-matching-todos (service &rest args)
  "for each issue in open github issues check if the issue isn't in the current todos
and it isn't return it"
  (let ((open-issues (apply 'get-issues service args))
        (todo-urls (mapcar 'get-item-URL (get-todo-items))))
    (cl-remove nil
               (mapcar (lambda (issue) (if (member (get-item-URL issue) todo-urls) nil issue))
                       open-issues))))

(defun close-todo-matching-item (item)
  (org-map-entries
   (lambda () (if (string= (org-entry-get (point) "URL") (get-item-URL item))
                  (org-todo "done")))))

(defun insert-issues-as-todos (issues pos)
  (progn
    (mapc
     (lambda (issue)
       (let ((title (get-item-title issue))
             (URL (get-item-URL issue))
             (date (get-item-date issue)))
         (progn
           (goto-char pos)
           (org-insert-heading-after-current)
           (insert (format "TODO %s" title))
           (org-demote)
           (org-insert-property-drawer)
           (org-entry-put (point) "URL" URL)
           (if (not (string= "" date)) (org-schedule t date)))))
     issues)
    nil))

(defun sync-todos ()
  (interactive)
  (progn
    (mapc 'close-todo-matching-item (get-issues-not-matching-todos "github" "--state closed" "bpostlethwaite" "Plotly" "streambed"))
    (insert-issues-as-todos (get-issues-not-matching-todos "github" "--state open" "bpostlethwaite" "Plotly" "streambed") (insertion-point))
    (insert-issues-as-todos (get-issues-not-matching-todos "gcal") (insertion-point))))
