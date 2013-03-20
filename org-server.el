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
  "Default port to serve from"
  :group 'org-server)

(defcustom org-server-host "localhost"
  "Default host for the server"
  :group 'org-server)

(defcustom org-server-css "
.left { display: inline-block; float: left; }
#navigation { max-width: 20%; }
#org-file { padding-left: 20px; max-width: 75%; }
"
  "CSS to apply to each returned page.

Default is very minimal, feel free to go crazy here. It's dumped directly into
a <style> element in the header, so keep that in mind.")

(defvar org-server--org-directory nil
  "Directory where we're looking for Org files.")

(defvar org-server--navlinks-html ""
  "HTML string for navigation bar, contains hierarchical list of files/directories
generated from list of files and directories")

(defvar org-server--org-file-names '()
  "List of org-mode files that we'll be serving up.

This is an alist of file-path -> mtime (So generated content can be cached)")

(defvar org-server--active? nil
  "Whether or not we have an instance of org-server running already.")

(defvar org-server--html-template "
<!doctype html>
<html>
  <head>
    <title>org-server</title>
    <style type=\"text/css\">
       {{{css}}}
    </style>
  </head>
  <body>
    <div class=\"left\" id=\"navigation\">
      {{{navigation}}}
    </div>

    <div class=\"left\" id=\"org-file\">
      {{{body}}}
    </div>
  </body>
</html>
"

  "Template string for pages sent to the client.")

(defun org-server--html-templater (navbar-html body-html)
  "Build the final HTML for the page using the `org-server--html-template'
template."

  (s-replace "{{{css}}}" org-server-css
             (s-replace "{{{body}}}" body-html
                        (s-replace "{{{navigation}}}" navbar-html
                                   org-server--html-template))))

(defun org-server--send-index (httpcon)
  (elnode-send-html httpcon (org-server--html-templater
                             org-server--navlinks-html
                             "<h1>org-server.el</h1>")))

(defun org-server--html-for-file (path)
  "Build the HTML for a given org file

TODO: This should cache already generated files, maybe."
  (with-temp-buffer
    (insert-file-contents (car path))
    (condition-case err
        (org-export-as-html 3 nil nil 'string t)

      ;; On error, report the error, and return plaintext representation
      ;; of buffer.
      (error (format
              "<h3>Error while generating HTML:</h3>
               <p>%s</p>
               <pre>%s</pre>"

              ;; HTML-escape error message
              (with-temp-buffer
                (insert (error-message-string err))
                (sgml-quote (point-min) (point-max))
                (buffer-string))

              ;; Return an HTML-escaped version of the org plaintext
              (let ((plain-text (buffer-string)))
                (with-temp-buffer
                  (insert plain-text)
                  (sgml-quote (point-min) (point-max))
                  (buffer-string))))))))

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

(defun org-server-refresh ()
  "Refresh listing of directories for the currently running server."
  (interactive)
  (if (and (not org-server--active?)
             (y-or-n-p "Server not running. Start one now?"))
      (call-interactively 'org-server-start))

  (progn
    (setq org-server--org-file-names '())
    (setq org-server--navlinks-html
          (concat "<ul style=\"margin-left: 0; padding-left: 0;\">"
                  (org-server--build-file-list ".")
                  "</ul>"))

    (message "File listing updated")))

(defun org-server-start (directory)
  "Starts up a server for the given directory of org files"
  (interactive "D")

  ;; Either kill the active server or bail out if something's running
  (if (not (org-server-stop))
    (message "Cannot have multiple servers at once"))

  (progn
    (setq org-server--org-directory directory)
    (setq org-server--org-file-names '())
    (setq org-server--active? t)

    (setq org-server--navlinks-html
          (concat "<ul style=\"margin-left: 0; padding-left: 0;\">"
                  (org-server--build-file-list ".")
                  "</ul>"))

    (elnode-start 'org-server--org-handler :port org-server-port
                  :host org-server-host)

    (message "org-server started on port %d" org-server-port)))

(defun org-server-stop ()
  "Stops the running org-server."
  (interactive)

  (if (not org-server--active?)
      (message "org-server is not running")

    (when (y-or-n-p "Stop running org-server?")
      (elnode-stop org-server-port)
      (setq org-server--active? nil)))

  org-server--active?)

(provide 'org-server)

;;; org-server.el ends here
