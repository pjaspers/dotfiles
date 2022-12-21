(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:adopt :cl-ppcre) :silent t))

;; rip-it --eject --min-length 3000 --disc 0 --reference tt1446192 --message "Finito" title
;;
;; This should do:
;; - transform title to a sanitized name
;; - mkdir <title>-<reference>
;; - makemkvcon --minlength=3000 mkv disc:0 all title
;; - when done rename the mkv inside the dir (if any)
;; - notify-ios  -m "Alles " (good or bad)
;; - drutil eject
;;
;; Also be able to just dump the whole script as a thing I can paste (--dry-run)

(defpackage :rip-it
  (:use :cl)
  (:export :toplevel :*ui*))
(in-package :rip-it)

(defparameter *min-length* 3000)
(defparameter *dry-run* nil)

(defparameter *help*
  (adopt:make-option 'help
    :help "display help and exit"
    :long "help"
    :short #\h
    :reduce (constantly t)))

(adopt:defparameters (*option-dry-run* *option-no-dry-run*)
  (adopt:make-boolean-options 'dry-run
    :long "dry-run"
    :help "Dry run"
    :help-no "Not dry run"))

(defparameter *disc-number*
  (adopt:make-option 'disc-number
    :help (format nil "specify disc number (default ~A)" "0")
    :long "disc-number"
    :short #\d
    :parameter "DISC-NUMBER"
    :initial-value "0"
    :reduce #'adopt:last))

(defparameter *min-length*
  (adopt:make-option 'min-length
    :help (format nil "min-length to keep, in minutes (default ~A)" "50")
    :long "min-length"
    :short #\l
    :parameter "MIN-LENGTH"
    :initial-value "50"
    :reduce #'adopt:last))

(defparameter *reference*
  (adopt:make-option 'reference
    :help "imdb or tvdb ref"
    :long "reference"
    :short #\r
    :parameter "REFERENCE"
    :reduce #'adopt:last))

(defparameter *message*
  (adopt:make-option 'message
    :help (format nil "message")
    :long "message"
    :short #\m
    :parameter "MESSAGE"
    :reduce #'adopt:last))

(defparameter *ui*
  (adopt:make-interface
    :name "rip-it"
    :usage "[OPTIONS] TITLE"
    :summary "print rip command"
    :help "bladiebla"
    :contents (list *help* *message* *min-length* *reference* *disc-number*)))

(defun sanitize-name(name)
  (string-downcase (cl-ppcre:regex-replace-all "\\W" (cl-ppcre:regex-replace-all "\\s+" name "_") "")))

(defun makemkv-command(&key disc min-length dir)
  (format nil "makemkvcon --minlength=~a mkv disc:~a all ~a" (* min-length 60) disc dir))

(defun notify-ios-command(&key title message)
  (format nil "notify-ios \"~A\" -m \"~A\"" title message))

(defun make-title(title &optional reference)
  (let ((title (sanitize-name title)))
    (if reference
        (format nil "~A-{~A}" title reference)
        title)
    ))

(defun toplevel()
  (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
    (cond
      ((gethash 'help options) (adopt:print-help-and-exit *ui*))
      (t (let* ((title (car arguments))
                (sanitized-title (make-title title (gethash 'reference options)))
                (path (format nil "~~/Movies/~a/original" sanitized-title))
                (makekmv (makemkv-command :disc (gethash 'disc-number options)
                                          :min-length (parse-integer (gethash 'min-length options))
                                          :dir path))
                (notify-ios (notify-ios-command :title title
                                                :message "Done"))
                )
           (format t "mkdir -p ~A && ~A && ~A && drutil eject" path makekmv notify-ios)
           )))
    ))

;; export TITLE="wreck-it-ralph-{tt1772341}"; mkdir ~/Movies/${TITLE} && makemkvcon --minlength=3000 mkv disc:0 all ~/Movies/${TITLE} && notify-ios "${TITLE}" -m "Alles is klaar" && drutil eject
