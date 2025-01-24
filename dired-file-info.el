;;; dired-file-info.el --- Dired File Information    -*- lexical-binding: t; -*-

;; Copyright (C) 2022 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Show more detailed file information than y(dired-show-file-type) in
;; dired-mode.

;;; Usage

;; (autoload #'dired-file-info "dired-file-info" "" t)
;; (with-eval-after-load "dired"
;;   (define-key dired-mode-map "y" #'dired-file-info))

;;; Code:

;; TODO:
;; - Support marked files

(require 'dired)
(require 'cl-lib)

;;;; Settings

(defgroup dired-file-info nil
  "Dired File Info"
  :prefix "dired-file-info-"
  :group 'dired
  :group 'files)

(defcustom dired-file-info-message-language nil
  "Language of displayed messages."
  :group 'dired-file-info
  :type '(choice (const :tag "Auto" nil)
                 (string :tag "Language"))
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'dired-file-info-update-message-language)
           (dired-file-info-update-message-language))))

(defcustom dired-file-info-timestamp-format "%Y-%m-%d %H:%M:%S"
  "Timestamp format."
  :group 'dired-file-info
  :type '(choice (string :tag "Use format-time-string")
                 (function :tag "Function (time)")))

(defcustom dired-file-info-details-local-file-command
  (if (executable-find "exiftool")
      "exiftool -c \"%+.06f\""
    "stat")
  "Command to output local file details."
  :group 'dired-file-info
  :type '(choice (string :tag "command line to pass to dired-do-shell-command")
                 (function :tag "Function (file)")))

(defcustom dired-file-info-details-remote-file-command "stat"
  "Command to output remote file details."
  :group 'dired-file-info
  :type '(choice (string :tag "command line to pass to dired-do-shell-command")
                 (function :tag "Function (file)")))

(defcustom dired-file-info-details-local-dir-command "stat"
  "Command to output local directory details."
  :group 'dired-file-info
  :type '(choice (string :tag "command line to pass to dired-do-shell-command")
                 (function :tag "Function (file)")))

(defcustom dired-file-info-details-remote-dir-command "stat"
  "Command to output remote directory details."
  :group 'dired-file-info
  :type '(choice (string :tag "command line to pass to dired-do-shell-command")
                 (function :tag "Function (file)")))

(defcustom dired-file-info-overview-items
  (list #'dired-file-info--file-type
        #'dired-file-info--directory-size
        #'dired-file-info--file-size
        #'dired-file-info--file-timestamps)
  "List of overview items."
  :group 'dired-file-info
  :type '(repeat (function :tag "Function (file deref-symlinks)")))

;;;; Message Catalog

(defconst dired-file-info--messages
  '(("Japanese" .
     #s(hash-table
        size 30
        test equal
        data (
              "Scanning directory %s" "スキャン中 %s"
              "Size" "サイズ"
              "B" "B"
              "bytes" "バイト"
              "Files" "ファイル数"
              "Directories" "ディレクトリ数"
              "Accessed" "アクセス"
              "Modified" "内容更新"
              "Status Changed" "状態更新"
              "More details" "詳細"
              )))))

(defvar dired-file-info--current-messages nil)

(defun dired-file-info-set-message-language (lang)
  (setq dired-file-info--current-messages
        (alist-get (or lang current-language-environment)
                   dired-file-info--messages nil nil #'equal)))

(defun dired-file-info-update-message-language ()
  (dired-file-info-set-message-language dired-file-info-message-language))

(dired-file-info-update-message-language)

(defun dired-file-info--msg (str)
  (or (and dired-file-info--current-messages
           (gethash str dired-file-info--current-messages))
      str))

;;;; Debug

(defun dired-file-info--warn (format &rest args)
  (display-warning 'dired-file-info
                   (apply #'format-message format args)))

;;;; Directory Scan

(defun dired-file-info--accumulate-entry (path
                                          fun-file
                                          &optional
                                          fun-enter
                                          fun-leave
                                          deref-symlinks
                                          only-one-filesystem
                                          depth
                                          attr)
  (when (null depth) (setq depth 0))
  (let* ((attr (or attr (file-attributes path)))
         (type (file-attribute-type attr))
         (symlink (and (stringp type) type)))
    ;; Note: On MS-Windows, (file-attributes "Long filename...") seems
    ;; to return nil.
    (unless attr
      (dired-file-info--warn "Ignore file `%s' (`file-attributes' returns nil)"
                             path))
    (when (and attr
               (not (and only-one-filesystem
                         (/= only-one-filesystem
                             (file-attribute-device-number attr)))))
      (cond
       ;; Symbolic Link
       (symlink
        (if deref-symlinks
            (dired-file-info--accumulate-entry
             symlink fun-file fun-enter fun-leave
             deref-symlinks only-one-filesystem depth)
          (funcall fun-file path attr)))

       ;; Directory
       ((eq type t)
        (dired-file-info--accumulate-directory
         path fun-file fun-enter fun-leave
         deref-symlinks only-one-filesystem (1+ depth)))

       ;; Normal File
       (t
        (funcall fun-file path attr))))))

(defun dired-file-info--files-and-attributes (dir)
  (condition-case err
      ;; On MS-Windows, a long DIR-PATH will cause a file-missing
      ;; ("No such file or directory") error.
      (directory-files-and-attributes dir)
    (error
     (dired-file-info--warn
      "Ignore dir `%s' (`directory-files-and-attributes' signals error `%s')"
      dir err)
     nil)))

(defun dired-file-info--accumulate-directory (dir-path
                                              fun-file
                                              &optional
                                              fun-enter
                                              fun-leave
                                              deref-symlinks
                                              only-one-filesystem
                                              depth)
  (when (null depth) (setq depth 0))
  (when fun-enter (funcall fun-enter dir-path))
  (cl-loop
   for (entry-name . attr) in (dired-file-info--files-and-attributes dir-path)
   unless (string-match "\\`\\.\\.?\\'" entry-name)
   do (dired-file-info--accumulate-entry
       (concat dir-path "/" entry-name)
       fun-file fun-enter fun-leave
       deref-symlinks only-one-filesystem depth
       ;; By reusing already obtained attributes, not only performance
       ;; is improved but also errors in `file-attributes' for long
       ;; path names can be avoided.
       ;; While (file-attributes "long file name..") returns nil, the
       ;; attributes obtained from the `directory-files-and-attributes'
       ;; function do not seem to be nil.
       attr))
  (when fun-leave (funcall fun-leave dir-path)))

(defun dired-file-info--summarize-directory (dir-path
                                             &optional
                                             deref-symlinks
                                             only-one-filesystem)
  (let ((size 0.0)
        (num-files 0)
        (num-dirs 0))
    (dired-file-info--accumulate-entry ;;-directory? deref first symlink?
     dir-path
     (lambda (_path attr)
       (cl-incf num-files)
       (cl-incf size (file-attribute-size attr)))
     (lambda (path)
       (cl-incf num-dirs)
       (let ((message-log-max nil))
         (message (dired-file-info--msg "Scanning directory %s") path)))
     nil
     deref-symlinks
     only-one-filesystem
     0)
    (list size num-files num-dirs)))

;;;; Dereference Symlinks

(defun dired-file-info--deref-symlink (path deref-symlinks)
  (when deref-symlinks
    (let (target)
      (while (setq target (file-symlink-p path))
        (setq path target)))) ;;@todo infinite loop
  path)

(defun dired-file-info--directory-p (path deref-symlinks)
  (and (file-directory-p path)
       ;; Exclude symlink when not deref-symlinks
       (or (not (file-symlink-p path))
           deref-symlinks)))

;;;; Format Text

(defun dired-file-info--format-size (size)
  (when (stringp size)
    (setq size
          (and (string-match "\\`\\([0-9]+\\)" size)
               (string-to-number (match-string 1 size)))))

  (when (numberp size)
    (concat
     (dired-file-info--msg "Size")
     ": " (file-size-human-readable size) (dired-file-info--msg "B")
     " (" (format "%d" size) (dired-file-info--msg "bytes") ")")))

(defun dired-file-info--format-time (time)
  (cond
   ((stringp dired-file-info-timestamp-format)
    (format-time-string dired-file-info-timestamp-format time))
   ((functionp dired-file-info-timestamp-format)
    (funcall dired-file-info-timestamp-format time))
   (t "")))

;;;; Retrieve Property

(defun dired-file-info--file-type (file &optional deref-symlinks)
  (let (process-file-side-effects)
    (with-temp-buffer
      (if deref-symlinks
          (process-file "file" nil t t "-L" "--" file)
        (process-file "file" nil t t "--" file))
      (when (bolp)
        (backward-delete-char 1))
      (buffer-string))))

;; (defun dired-file-info--directory-size-du (file)
;;   (and (file-directory-p file)
;;        (not (file-remote-p file))
;;        (let ((du (progn
;;                    (message "Executing du...")
;;                    (shell-command-to-string (mapconcat #'shell-quote-argument (list "du" "-bs" file) " ")))))
;;          (message "Executing du...done")
;;          (concat "\n" (dired-file-info--format-size du)))))

(defun dired-file-info--directory-size (file &optional deref-symlinks)
  (when (dired-file-info--directory-p file deref-symlinks)
    (let* ((summary (dired-file-info--summarize-directory
                     file deref-symlinks nil))
           (size (nth 0 summary))
           (num-files (nth 1 summary))
           (num-dirs (nth 2 summary)))
      (concat "\n" (dired-file-info--format-size size)
              "\n" (dired-file-info--msg "Files") ": " (format "%d" num-files)
              "\n" (dired-file-info--msg "Directories") ": " (format "%d" num-dirs)))))

(defun dired-file-info--file-size (file &optional deref-symlinks)
  (and (not (dired-file-info--directory-p file deref-symlinks))
       (concat "\n" (dired-file-info--format-size
                     (file-attribute-size
                      (file-attributes
                       (dired-file-info--deref-symlink file deref-symlinks)))))))

(defun dired-file-info--file-timestamps (file &optional deref-symlinks)
  (let ((attr (file-attributes (dired-file-info--deref-symlink file deref-symlinks))))
    (concat
     (concat "\n" (dired-file-info--msg "Accessed") ": "
             (dired-file-info--format-time
              (file-attribute-access-time attr)))
     (concat "\n" (dired-file-info--msg "Modified") ": "
             (dired-file-info--format-time
              (file-attribute-modification-time attr)))
     (concat "\n" (dired-file-info--msg "Status Changed") ": "
             (dired-file-info--format-time
              (file-attribute-status-change-time attr))))))

;;;; Info List

(defun dired-file-info--show-overview (file &optional deref-symlinks)
  "Display overview FILE information."
  (message
   "%s"
   (concat
    (mapconcat (lambda (fun)
                 (funcall fun file deref-symlinks))
               dired-file-info-overview-items
               "")
    "\n"
    "("
    (if deref-symlinks "C-u ")
    (substitute-command-keys "\\[dired-file-info]:")
    (dired-file-info--msg "More details")
    ")"
    )))

(defconst dired-file-info-buffer-name "*Dired File Info*")

(defun dired-file-info--show-details (file &optional deref-symlinks)
  "Display detailed FILE information."
  (setq file (dired-file-info--deref-symlink file deref-symlinks))

  (let ((command (if (file-remote-p (expand-file-name file))
                     (if (file-directory-p file)
                         dired-file-info-details-remote-dir-command
                       dired-file-info-details-remote-file-command)
                   (if (file-directory-p file)
                       dired-file-info-details-local-dir-command
                     dired-file-info-details-local-file-command))))
    (cond
     ((stringp command)
      (let ((shell-command-buffer-name dired-file-info-buffer-name))
        (dired-do-shell-command command nil (list file))
        (when-let ((buffer (get-buffer dired-file-info-buffer-name)))
          (with-current-buffer buffer
            (view-mode)))))
     ((functionp command)
      (funcall command file)))))

;;;; Command

(defvar dired-file-info--last-args nil)

;;;###autoload
(defun dired-file-info (file &optional deref-symlinks details)
  "Display FILE information.

Outputs detailed information when executed twice in a row."
  (interactive
   (let* ((file (dired-get-filename t))
          (deref-symlinks current-prefix-arg)
          (details (and (eq last-command 'dired-file-info)
                        (equal (nth 0 dired-file-info--last-args) file)
                        (equal (nth 1 dired-file-info--last-args) deref-symlinks))))
     (list file deref-symlinks details)))

  (if details
      ;; Details
      (progn
        (dired-file-info--show-details file deref-symlinks)
        (setq dired-file-info--last-args nil))
    ;; Overview
    (dired-file-info--show-overview file deref-symlinks)
    (setq dired-file-info--last-args (list file deref-symlinks details))))

(provide 'dired-file-info)
;;; dired-file-info.el ends here
