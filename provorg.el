;;; provorg.el --- a simple provisioner                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Suhail Shergill

;; Author: Suhail Shergill
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

;; A simple provisioning system built on top of org-mode.

;;; Code:

;; code goes here

(require 'ob)

(eval-after-load 'ob
  '(progn

     (add-to-list 'org-babel-common-header-args-w-values '(sudo . ((no yes))))
     (add-to-list 'org-babel-common-header-args-w-values '(dir-dyn . ((yes no))))

     (add-to-list 'org-babel-common-header-args-w-values '(async . ((yes no))))

     (setq org-babel-header-arg-names (mapcar #'car org-babel-common-header-args-w-values))

     (let ((dir (file-name-directory (or load-file-name
                                         buffer-file-name))))
       (dolist (bol '(
                      "./provorg.org"
                      "./recipes.org"
                      )) (org-babel-lob-ingest (expand-file-name bol dir))))

     ))


;;{{{ lob

(defun provorg/call-lob/mkstr (var-list)
  "convert lob-call args to string"
  (mapcar #'(lambda (var)
              (mapconcat 'prin1-to-string var " "))
          var-list))
(defun provorg/call-lob (func &rest args)
  "call lob functions"
  (save-excursion
    (with-temp-buffer
      (let ((inside-header (plist-get args :inside-header))
            (arg (plist-get args :args))
            (end-header (plist-get args :end-header)))
        (org-babel-lob-execute (list
                                (concat func
                                        "[" (mapconcat 'identity (provorg/call-lob/mkstr inside-header) " ") "]"
                                        "(" (mapconcat 'identity (provorg/call-lob/mkstr arg) ", ") ")")
                                (mapconcat 'identity (provorg/call-lob/mkstr
                                                      end-header) " ")
                                ;; indentation. in most normal cases i've
                                ;; encountered this has been 2, but it's possible in
                                ;; certain circumstances this is different

                                ;; TODO:
                                ;; investigate. [[help:org-babel-get-src-block-info]]
                                ;; and [[help:org-babel-parse-src-block-match]]
                                2))
        ))))

;;}}}

;;{{{ utils

(defun provorg/utils/yes-or-no-to-boolean (arg)
  "get boolean from org-babel header arguments in yes/no format"
  (let ((val (provorg/utils/not-nil arg)))
    (if (stringp val)
        (string= val "yes")
      nil)))

(defun provorg/utils/not-nil (arg)
  "similar to `org-not-nil' except collapses empty strings to `nil' as well"
  (let ((arg (org-not-nil arg)))
    (cond
     ((and (stringp arg) (string= arg ""))
      nil)
     (arg))))

(defun provorg/utils/get-var (var params-list)
  "extract value of variable if defined by header arguments"
  (cdr (assoc var (mapcar #'cdr (org-babel-get-header params-list :var)))))

;;}}}

;;{{{ host

(defvar provorg/host/default "localhost"
  "default value for the current host")
(defvar provorg/host provorg/host/default
  "variable to store the current host")

(defun provorg/host/get ()
  "get the current host which we're provisioning. use provorg/host to cache
  the result"
  (if (stringp (org-not-nil provorg/host))
      provorg/host
    provorg/host/default))

(defun provorg/host/set (host)
  "set the current host, and unset `provorg/host/stack'"
  (let ((host (provorg/utils/not-nil host)))
    (setq provorg/host host
          provorg/host/stack nil)
    (provorg/host/get)))


(defvar provorg/host/stack '()
  "stack to hold the values of current hosts")

(defun provorg/host/push (host)
  "set `provorg/host' and push its current value on to stack"
  (let ((host (provorg/utils/not-nil host)))
    (push provorg/host provorg/host/stack)
    (setq provorg/host host)
    (provorg/host/get)))

(defun provorg/host/pop ()
  "pop the host from `provorg/host/stack' and set `provorg/host'"
  (provorg/host/set (pop provorg/host/stack))
  (provorg/host/get))

;;}}}

;;{{{ dir-dyn

(defun provorg/get-path (host dir &optional sudop)
  "get the path based on the value of `host'"
  (let* ((prefix (if sudop
                     (concat "/sudo:" (if (string= host "localhost")
                                          ":"
                                        (concat "root@" host ":")))
                   (if (string= host "localhost")
                       ""
                     (concat "/ssh:" host ":")))))
    (concat prefix (or (file-remote-p dir 'localname) dir))))

(defadvice org-babel-execute-src-block (around
                                        su/advice/ob/org-babel-execute-src-block/around/set-dir
                                        first
                                        a c pre)
  "Set `:dir' parameter for code blocks based on `:dir-dyn' and `:sudo' header
  arguments.
  NOTE: always specify `:dir' (it's ok to set it to ~) when using `:dir-dyn'"
  (let* ((info (or (ad-get-arg 1) (org-babel-get-src-block-info)))
         (params (ad-get-arg 2))
         (new-params (copy-alist params))
         (all-params (if params
                         (org-babel-process-params
                          (org-babel-merge-params (nth 2 info)
                                                  params))
                       (nth 2 info)))
         (dir-dynp (provorg/utils/yes-or-no-to-boolean (alist-get all-params
                                                             :dir-dyn t)))
         (host (or (if dir-dynp
                       (provorg/utils/get-var 'provorg/host all-params))
                   ;; either dir-dynp is false or 'provorg/host var not set via
                   ;; header
                   (provorg/host/get)))
         (sudop (provorg/utils/yes-or-no-to-boolean (alist-get all-params :sudo t)))
         (dir (or (alist-get all-params :dir t) (if dir-dynp
                                               "~"
                                             default-directory)))
         (path (provorg/get-path host dir sudop))
         )

    (when dir-dynp
      (add-to-list 'new-params (cons :dir path) t))
    (ad-set-arg 2 new-params)
    ad-do-it
    ;; so `org-babel-library-of-babel' seems to cache the arguments passed on to
    ;; lob functions across calls. attempting to unset it here fails. the place
    ;; where this affects us is when `:dir' from a previous `:dir-dyn yes'
    ;; invocation is remembered. the fix for now is to ensure that :dir is
    ;; always provided when setting `:dir-dyn' to `yes'
    (ad-set-arg 2 params)
    ))
;; (ad-disable-advice 'org-babel-execute-src-block 'around 'su/advice/ob/org-babel-execute-src-block/around/set-dir)

;;}}}

;;{{{ async

(require 'async)

(defvar provorg/ob/execute-src-block/arg nil
  "temporary scratchpad used to store the `arg' parameter for
  `org-babel-execute-src-block'")
(defvar provorg/ob/execute-src-block/info nil
  "temporary scratchpad used to store the `info' parameter for
  `org-babel-execute-src-block'")
(defvar provorg/ob/execute-src-block/params nil
  "temporary scratchpad used to store the `params' parameter for
  `org-babel-execute-src-block'")

(defadvice org-babel-execute-src-block (around
                                        su/advice/ob/org-babel-execute-src-block/around/async
                                        last
                                        a c pre)
  "Execute src-block with async"
  (let* ((arg (ad-get-arg 0))
         (info (or (ad-get-arg 1) (org-babel-get-src-block-info)))
         (params (ad-get-arg 2))
         (all-params (if params
                         (org-babel-process-params
                          (org-babel-merge-params (nth 2 info)
                                                  params))
                       (nth 2 info)))
         (asyncp (provorg/utils/yes-or-no-to-boolean (alist-get all-params :async t)))
         )
    (setq provorg/ob/execute-src-block/arg arg
          provorg/ob/execute-src-block/info info
          provorg/ob/execute-src-block/params params)
    (defvar su/async/?)
    (if (and asyncp su/async/?)
        (async-start
         `(lambda ()
            ;; load config
            ;; TODO: modularize the config and only load the 'relevant' bits?
            (load-file "~/.emacs")
            ;; 'disable' spawning off new async sessions
            (setq su/async/? nil)
            ;; disable confirmation of src-code execution
            (setq org-confirm-babel-evaluate nil)
            ;; inject args
            ,(async-inject-variables "provorg/ob/execute-src-block/")
            (org-babel-execute-src-block provorg/ob/execute-src-block/arg
                                         provorg/ob/execute-src-block/info
                                         provorg/ob/execute-src-block/params))
         (lambda (result)
           (message "async::%s" result)))
      ad-do-it
      )
    )
)

;;}}}

;; (ad-deactivate 'org-babel-execute-src-block)
;; (setq provorg/host "chaos")

(provide 'provorg)
;;; provorg.el ends here

