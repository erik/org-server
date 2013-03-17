;;; org-server.el --- Org Server application

;; Copyright 2013 Erik Price

;; Author: Erik Price <erik@erikprice.net>
;; URL: http://github.com/boredomist/org-server
;; Version: 0.0.1
;; Created: 13th March 2013
;; Keywords: http, org
;; Package-Requires: ((elnode "0.9.9.6.7"))

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

(require 'elnode)

;; Some helper functions

;; Ripped from http://emacswiki.org/emacs/ElispCookbook#toc4
(defun string/--starts-with (s arg)
  "returns non-nil if string S starts with ARG.  Else nil."
  (cond ((>= (length s) (length arg))
         (string-equal (substring s 0 (length arg)) arg))
        (t nil)))

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

(defun org-server--send-index (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon org-server--navlinks-html))

(defun org-server--send-file (httpcon path)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon "<html><b>PRETEND THIS IS THE PATH</b></html>"))

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

      (let ((file (org-server--find-file path)))
        (if file
            (org-server--send-file httpcon file)
            (elnode-send-404 httpcon (substring path 1)))))))

(defun org-server--build-file-list (directory)
  "Build up a list of org-mode files in the given directory.

Returns an HTML string of the files/directory structure"

  (let ((html-nav-string ""))
    (dolist (file/attr (directory-files-and-attributes directory))
      (let* ((file  (first file/attr))
             (mtime  (nth 5 file/attr))
             (ext    (file-name-extension file))
             (path   (expand-file-name file directory)))

        ;; Ignore dotfiles, ., .., and other such nonsense.
        ;; TODO: This behavior should definitely be configurable.
        (unless (string/--starts-with file ".")

          (if (file-directory-p path)
              (progn
                (setq html-nav-string
                      (concat html-nav-string
                              (org-server--build-file-list path))))

            (when (string= ext "org")
              (setq html-nav-string (concat html-nav-string
                                            "<li>" file "</li>"))
              (add-to-list 'org-server--org-file-names (cons path mtime)))))))

    ;; Return HTML nav string
    (if (equal html-nav-string "")
        ""
      (let ((basename (file-name-nondirectory directory)))
        (concat "<li>" (if (equal basename "")
                           "Files"
                         basename)
                "</li>"

                "<ul>" html-nav-string "</ul>")))))

(defun org-server-init (directory)
  "Starts up a server for the given directory of org files"
  (interactive "D")

  (setq org-server--org-directory directory)
  (setq org-server--org-file-names '())
  (setq org-server--navlinks-html "")

  (setq org-server--navlinks-html
        (concat  "<ul>"
                 (org-server--build-file-list directory)
                 "</ul>"))

  (elnode-start 'org-server--org-handler :port org-server-port
                :host org-server-host))

(provide 'org-server)

;;; org-server.el ends here
