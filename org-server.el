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

(defcustom org-server-layout-file "org-server-template.html"
  "Template to use for the generated HTML.

Files should be simple HTML, with two mustache style variables,
{{{navigation}}} and {{{content}}} where the generated navigation bar and HTML
generated from the Org file should go, respectively."
  :group 'org-server)

(defvar org-server--org-directory nil
  "Directory where we're looking for Org files")

(defvar org-server--org-files '()
  "Them files we're interested in.

This is an alist of file -> mtime (So generated content can be cached)")

(defun org-server--org-handler (httpcon)
  (elnode-docroot-for org-server--org-directory
    with org-file
    on httpcon
    do (with-current-buffer (find-file-noselect org-file)
         (let ((org-html (org-export-as-html 3 nil nil 'string)))
           (elnode-send-html httpcon org-html)))))

(defun org-server--build-file-list (directory)
  "Build up a list of org-mode files in the given directory."
  (dolist (file/attr (directory-files-and-attributes directory))
    (let* ((file  (first file/attr))
           (mtime  (nth 5 file/attr))
           (ext    (file-name-extension file))
           (path   (expand-file-name file directory)))

      ;; Ignore dotfiles, ., .., and other such nonsense.
      ;; TODO: This behavior should definitely be configurable.
      (unless (string/--starts-with file ".")

        (if (file-directory-p path)
            (org-server--build-file-list path)

          (when (string= ext "org")
            (add-to-list 'org-server--org-files (cons path mtime))))))))

(defun org-server-init (directory)
  "Starts up a server for the given directory of org files"
  (interactive "D")

  (setq org-server--org-directory directory)
  (setq org-server--org-files '())

  (org-server--build-file-list directory)

  (elnode-start 'org-server--org-handler :port org-server-port
                :host org-server-host))

;;; org-server.el ends here
