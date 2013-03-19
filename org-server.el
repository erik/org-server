;;; org-server.el --- Org Server application

;; Copyright 2013 Erik Price

;; Author: Erik Price <erik@erikprice.net>
;; URL: http://github.com/boredomist/org-server
;; Version: 0.0.1
;; Created: 13th March 2013
;; Keywords: http, org
;; Package-Requires: ((elnode "0.9.9.6.7") (s "1.3.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; elnode handler to automatically htmlize a given directory of org-mode files
;;
;; Source available on github at https://github.com/boredomist/org-server

(require 'elnode)
(require 's)

(defgroup org-server nil
  "A server for a directory of org-files"
  :group 'applications)

(defcustom org-server-port 8765
  "Default port to server from"
  :group 'org-server)

(defcustom org-server-host "localhost"
  "Default host for the server"
  :group 'org-server)

(defvar org-server--org-directory nil
  "Directory where we're looking for Org files")

(defvar org-server--navlinks-html ""
  "HTML for navigation bar, contains hierarchical list of files/directories
generated from list of files and directories")

(defvar org-server--org-file-names '()
  "List of org-mode files that we'll be serving up.

This is an alist of file-path -> mtime (So generated content can be cached)")

(defvar org-server--html-template "
<!doctype html>
<html>
  <head>
    <title>org-server</title>
  </head>
  <body>
    <div style=\"display:inline-block; float:left; max-width:20%;\" id=\"navigation\">
      {{{navigation}}}
    </div>

    <div style=\"display:inline-block; float:left; padding-left: 20px; max-width: 75%;\" id=\"org-file\">
      {{{body}}}
    </div>
  </body>
</html>
"

  "Template for pages sent to the client")

(defun org-server--html-templater (navbar-html body-html)
  "Build the final HTML for the page using the `org-server--html-template
template."
  (let* ((html (s-replace "{{{navigation}}}" navbar-html
                          org-server--html-template))
         (html (s-replace "{{{body}}}" body-html html)))
    html))

(defun org-server--send-index (httpcon)
  (elnode-send-html httpcon (org-server--html-templater
                             org-server--navlinks-html
                             "<h1>org-server.el</h1>")))

(defun org-server--html-for-file (path)
  "Build the HTML for a given org file

TODO: This should cache already generated files, maybe."
  (with-current-buffer (find-file-noselect (car path))
    (condition-case err
        (org-export-as-html 3 nil nil 'string t)
      (error (format "<h1>Error while generating HTML:</h1><p>%s</p>"
                     (error-message-string err))))))

(defun org-server--find-file (path)
  (let ((expanded (expand-file-name path org-server--org-directory)))
    (print expanded)

    (assoc expanded org-server--org-file-names)))

(defun org-server--org-handler (httpcon)
  "Main handler for org-server"

  ;; We take the substring to chop off the leading /
  (let* ((path (substring
                (elnode-http-pathinfo httpcon) 1)))

    (if (equal path "")
        (org-server--send-index httpcon)

      (let* ((file (org-server--find-file path))
             (html (and file (org-server--html-for-file file))))
        (if file
            (elnode-send-html httpcon (org-server--html-templater
                                       org-server--navlinks-html
                                       html))
            (elnode-send-404 httpcon path))))))

(defun org-server--build-file-list (directory)
  "Build up a list of org-mode files in the given relative directory.

Returns an HTML string of the files/directory structure"

  (let ((html-nav-string "")
        (absolute-dir (expand-file-name directory org-server--org-directory)))
    (dolist (file/attr (directory-files-and-attributes absolute-dir))
      (let* ((file  (first file/attr))
             (mtime  (nth 5 file/attr))
             (ext    (file-name-extension file))
             (path   (expand-file-name file absolute-dir))
             (rel-dir (format "%s/%s" directory file)))

        ;; Ignore dotfiles, ., .., and other such nonsense.
        ;; TODO: This behavior should definitely be configurable.
        (unless (s-starts-with? "." file)
          (if (file-directory-p path)
              (progn
                (setq html-nav-string
                      (concat html-nav-string
                              (org-server--build-file-list rel-dir))))

            (when (string= ext "org")
              (setq html-nav-string
                    (concat html-nav-string
                            (format "<li><a href=\"/%s\">%s</a></li>"
                                    rel-dir file)))
              (add-to-list 'org-server--org-file-names (cons path mtime)))))))

    ;; Return HTML nav string

    (let ((basename (file-name-nondirectory directory)))
      (cond
       ((equal html-nav-string "") "")
       ((equal basename ".") html-nav-string)
       (t (concat "<li>" basename "</li>"
                  "<ul style=\"margin-left: 0; padding-left: 1.5em;\">"
                  html-nav-string "</ul>"))))))

(defun org-server-init (directory)
  "Starts up a server for the given directory of org files"
  (interactive "D")

  (setq org-server--org-directory directory)
  (setq org-server--org-file-names '())
  (setq org-server--navlinks-html "")

  (setq org-server--navlinks-html
        (concat "<ul style=\"margin-left: 0; padding-left: 0;\">"
                (org-server--build-file-list ".")
                "</ul>"))

  (elnode-start 'org-server--org-handler :port org-server-port
                :host org-server-host))

(provide 'org-server)

;;; org-server.el ends here
