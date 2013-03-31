(require 'ob)

(eval-after-load 'ob
  '(progn

     (add-to-list 'org-babel-common-header-args-w-values '(sudo . ((no yes))))
     (add-to-list 'org-babel-common-header-args-w-values '(dir-dyn . ((yes no))))

     (setq org-babel-header-arg-names (mapcar #'car org-babel-common-header-args-w-values))
     ))

(defun su/provorg/utils/yes-or-no-to-boolean (arg)
  "get boolean from org-babel header arguments in yes/no format"
  (let ((val (org-not-nil arg)))
    (if (stringp val)
        (string= val "yes")
      nil)))

(defvar su/provorg/host "localhost"
  "variable to store the current host")

(defun su/provorg/get-host ()
  "get the current host which we're provisioning. use su/provorg/host to cache
  the result"
  (if (stringp (org-not-nil su/provorg/host))
      su/provorg/host
    "localhost"))

(defvar su/provorg/ob/execute-src-block/params nil
  "temporary scratchpad used to store the updated set of params as being passed
  to `org-babel-execute-src-block'")

(defadvice org-babel-execute-src-block (before
                                        su/advice/ob/org-babel-execute-src-block/before/set-dir
                                        a c pre)
  "Set `:dir' parameter for code blocks based on `:dir-dyn' and `:sudo' header
  arguments"
  (let* ((info (or (ad-get-arg 1) (org-babel-get-src-block-info)))
         (params (ad-get-arg 2))
         (all-params (if params
                         (org-babel-process-params
                          (org-babel-merge-params (nth 2 info)
                                                  params))
                       (nth 2 info)))
         (dir-dynp (su/provorg/utils/yes-or-no-to-boolean (aget all-params :dir-dyn t)))
         (sudop (su/provorg/utils/yes-or-no-to-boolean (aget all-params :sudo t)))
         (dir (or (aget all-params :dir t) default-directory))
         (host (su/provorg/get-host))
         (prefix (if sudop
                     (concat "/sudo:" (if (string= host "localhost")
                                          ":"
                                        (concat "root@" host ":")))
                   (if (string= host "localhost")
                       ""
                     (concat "/ssh:" host ":"))))
         (path (concat prefix dir))
         )
    (when dir-dynp
      (add-to-list 'params (cons :dir path)))
    ;; cache the final value of `params'
    (setq su/provorg/ob/execute-src-block/params params))
  ;; for some reason calling ad-set-arg from within `let*' fails. it succeeds if
  ;; i'm trying to set it to some string or some native datatypes, but fails for
  ;; the params alist. caching the value in another variable and calling
  ;; `ad-set-arg' from outside the `let*' works
  (ad-set-arg 2 su/provorg/ob/execute-src-block/params))

;; (ad-deactivate 'org-babel-execute-src-block)
;; (setq su/provorg/host "chaos")
