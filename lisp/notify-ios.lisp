(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload
    '(:uiop :drakma :st-json :with-user-abort :adopt)
    :silent t)
  )

(defpackage :notify-ios
  (:use :cl)
  (:export :toplevel :*ui*))

(in-package :notify-ios)

(defparameter *debug-is-on* nil)

;;;; Errors ------------------------------------------------------
(define-condition user-error (error) ())

(define-condition missing-foo (user-error) ()
  (:report "A foo is required, but none was supplied."))

(defun hass-ip()
  (uiop:getenv "HASS_IP")
  )

(defun hass-token()
  (uiop:getenv "HASS_TOKEN")
  )

(defun json-message (title message)
  (st-json:write-json-to-string
   (st-json:jso "message" message "title" title)))

(defun ensure-response-string (response)
  (etypecase response
    (string response)
    (stream (alexandria:read-stream-content-into-string response))
    (vector (flexi-streams:octets-to-string response))))

(defparameter *help*
  (adopt:make-option 'help
    :help "display help and exit"
    :long "help"
    :short #\h
    :reduce (constantly t)))

(defparameter *message*
  (adopt:make-option 'message
    :help (format nil "message")
    :long "message"
    :short #\m
    :parameter "MESSAGE"
    :reduce #'adopt:last))

(adopt:defparameters (*option-debug* *option-no-debug*)
  (adopt:make-boolean-options 'debug
    :long "debug"
    :short #\d
    :help "Enable the Lisp debugger."
    :help-no "Disable the Lisp debugger (the default)."))

(defparameter *ui*
  (adopt:make-interface
    :name "notify-ios"
    :usage "[OPTIONS] TITLE"
    :summary "send message to iOS"
    :help "Notify your iPhone through hass"
    :contents (list *help* *message* *option-debug* *option-no-debug*)
    :examples '(("Send 'ohai':" . "notify-ios ohai")
                ("Send 'ohai' with longer message:" . "notify-ios --message='thing and other thing' ohai"))))

(defun send-command(&key title message)
  (unless title
    (multiple-value-bind (seconds minutes hours day month) (get-decoded-time)
      (setf title (format nil "PING ~A:~A:~A ~A/~A" hours minutes seconds day month))))
  (unless message (setf message title))
  (if *debug-is-on*
      (format t "JSON: ~A" (json-message title message)))
  (multiple-value-bind (body status headers uri stream needs-close reason)
      (drakma:http-request (concatenate 'string "http://" (hass-ip) ":8123/api/services/notify/mobile_app_oranje")
                           :content (json-message title message)
                           :content-type "application/json"
                           :method :post
                           :additional-headers (list (cons "Authorization" (format nil "Bearer ~a" (hass-token)))))
    (setf body (ensure-response-string body))
    (if (<= 200 status 299)
        (st-json:read-json body)
        (error "Received ~D ~A from ~A: ~A" status reason uri body))
    )
  )

(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (adopt:exit 130))))

(defun toplevel()
(sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
      (when (gethash 'debug options)
        (sb-ext:enable-debugger)
        (setf drakma:*header-stream* *standard-output*)
        (setf *debug-is-on* t))
      (handler-case
          (cond
            ((gethash 'help options) (adopt:print-help-and-exit *ui*))
            (t (send-command
                :title (car arguments)
                :message (gethash 'message options))))
        (user-error (e) (adopt:print-error-and-exit e))))))

(defun build ()
  (sb-ext:save-lisp-and-die "notify-ios" :executable t :toplevel #'toplevel))
