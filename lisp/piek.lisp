(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:st-json :adopt :drakma :alexandria :with-user-abort :cl-ppcre) :silent t))

(defpackage :piek
  (:use :cl)
  (:export :toplevel :*ui*))

(in-package :piek)

(defvar *api-token* (uiop:getenv "HASS_TOKEN"))
(defvar *hass-url* (uiop:getenv "HASS_IP"))
(defparameter *debug-is-on* nil)

(defvar *harmony-conf* (with-open-file (stream "harmony.conf")
                         (st-json:read-json stream)))


(define-condition user-error (error) ())

(define-condition unknown-command(user-error)
  ((used-commands :initarg :used-commands)
   (matched-commands :initarg :matched-commands)
   (available :initarg :available))
  (:report (lambda (c s)
             (format s "Unsupported command: ~A (matched to ~A), should be one of ~A" (slot-value c 'used-commands) (slot-value c 'matched-commands) (slot-value c 'available)))))

(define-condition unknown-device(user-error)
  ((used-device :initarg :used-device)
   (available :initarg :available))
  (:report (lambda (c s)
             (format s "Unsupported device: ~A, should be one of ~A" (slot-value c 'used-device) (slot-value c 'available)))))

(define-condition unknown-activity(user-error)
  ((used-activity :initarg :used-activity)
   (available :initarg :available))
  (:report (lambda (c s)
             (format s "Unsupported device: ~A, should be one of ~A" (slot-value c 'used-activity) (slot-value c 'available)))))

(defun ensure-response-string (response)
  (etypecase response
    (string response)
    (stream (alexandria:read-stream-content-into-string response))
    (vector (flexi-streams:octets-to-string response))))

(defun mapcar-jso (thunk jso &aux list)
  (st-json:mapjso (lambda (key val)
            (push (funcall thunk key val) list))
          jso)
  list)

(defun jso-keys (jso) (mapcar-jso (lambda (k v) (declare (ignore v)) k) jso))

(defun list-device-names(conf)
  (jso-keys (st-json:getjso "Devices" conf)))

(defun list-activities(conf)
  (jso-keys (st-json:getjso "Activities" conf)))

(defstruct device name id commands)
(defun json->device(name jso)
  (let ((commands (st-json:getjso "commands" jso))
         (id (st-json:getjso "id" jso)))
    (make-device :name name :id id :commands commands)
    ))

(defstruct activity name id)

(defun activities-from-conf(conf)
  (let* ((thing nil)
        (mapper (lambda(k v) (push (make-activity :name v :id k) thing))))

        (st-json:mapjso mapper (st-json:getjso "Activities" conf))
    thing))

(defun devices-from-conf(conf)
  (let ((thing nil))
    (st-json:mapjso #'(lambda (k v)
                        (push (json->device k v) thing)
                        ) (st-json:getjso "Devices" conf))
    thing))

(defun activities()
  (activities-from-conf *harmony-conf*))

(defun devices()
  (devices-from-conf *harmony-conf*)
  )

(defmacro with-key(kind key value)
  (let* ((plural (cond((equal kind "device") "devices") ((equal kind "activity") "activities")))
         (lookup (intern (string-upcase (concatenate 'string (format nil "~A" plural) "-from-conf"))))
        (accessor (intern (string-upcase (concatenate 'string kind "-" key)))))
        ;; `(find value (,lookup ,*harmony-conf*) :key (intern (concatenate 'string type "-" key)) :test 'equal))
        `(find ,value (,lookup *harmony-conf*) :key (quote ,accessor) :test 'equal)
    ))

(defun collection(kind)
  (let* ((plural (cond((equal kind "device") "devices") ((equal kind "activity") "activities"))))
    (funcall (intern (string-upcase plural) "PIEK"))
    )
  )

(defun with-name-matches(kind name)
  (let ((scanner (cl-ppcre:create-scanner name :case-insensitive-mode t))
        (objects (collection kind))
        (accessor (intern (string-upcase (concatenate 'string kind "-name")) "PIEK")))
    (find name objects :test #'(lambda (a b) (declare (ignore a)) (cl-ppcre:scan scanner (funcall accessor b))))
    ))


(defun device-with-id (id) (with-key "device" "id" id))
(defun device-with-name (name) (with-key "device" "name" name))


(defun device-matches-name (name)
  (with-name-matches "device" name)
  )

(defun debug-info-send-command(hass-url entity-id device-id commands)
  (format t "
/api/service/remote/send_command

  [hass-url] ~A
  [entity]   ~A
  [device]   ~A
  [commands] ~A

" hass-url entity-id device-id commands))

(defun find-match(collection value)
  (let ((scanner (cl-ppcre:create-scanner value :case-insensitive-mode t)))
    (find value collection :test #'(lambda (a b) (declare (ignore a)) (cl-ppcre:scan scanner b)))))

(defun match-commands(commands available-commands)
  (mapcar #'(lambda(command) (find-match available-commands command)) commands))


;; http://100.74.48.103:8123/api/services/remote/send_command
(defun send-command(&key (entity-id "remote.piek") (device) (commands ()))
  (let* ((available-commands (device-commands device))
         (matched-commands (match-commands commands available-commands)))
    (unless (notany #'null matched-commands)
      (error 'unknown-command :available (device-commands device) :used-commands commands :matched-commands matched-commands))

    (if *debug-is-on*
        (debug-info-send-command *hass-url* entity-id (device-id device) matched-commands))

    (multiple-value-bind (body status uri reason)
        (drakma:http-request (concatenate 'string "http://" *hass-url* ":8123/api/services/remote/send_command")
                             :content (st-json:write-json-to-string
                                       (st-json:jso
                                        "entity_id" entity-id
                                        "device" (device-id device)
                                        "command" matched-commands))
                             :content-type "application/json"
                             :method :post
                             :additional-headers (list (cons "Authorization" (format nil "Bearer ~a" *api-token*))))
      (setf body (ensure-response-string body))
      (if (<= 200 status 299)
          (st-json:read-json body)
          (error "Received ~D ~A from ~A: ~A" status reason uri body))
      )
    )
  )

(defun debug-info-send-activity(hass-url entity-id activity-id)
  (format t "
/api/service/remote/turn_on

  [hass-url] ~A
  [entity]   ~A
  [activity] ~A

" hass-url entity-id activity-id))

(defun send-activity(&key (entity-id "remote.piek") (activity))
  (if *debug-is-on*
      (debug-info-send-activity *hass-url* entity-id (activity-id activity)))

  (multiple-value-bind (body status uri reason)
      (drakma:http-request (concatenate 'string "http://" *hass-url* ":8123/api/services/remote/turn_on")
                           :content (st-json:write-json-to-string
                                     (st-json:jso
                                      "entity_id" entity-id
                                      "activity" (activity-id activity)))
                           :content-type "application/json"
                           :method :post
                           :additional-headers (list (cons "Authorization" (format nil "Bearer ~a" *api-token*))))
    (setf body (ensure-response-string body))
    (if (<= 200 status 299)
        (st-json:read-json body)
        (error "Received ~D ~A from ~A: ~A" status reason uri body))
    )
  )

(defparameter *help*
  (adopt:make-option 'help
    :help "display help and exit"
    :long "help"
    :short #\h
    :reduce (constantly t)))

(adopt:defparameters (*option-debug* *option-no-debug*)
  (adopt:make-boolean-options 'debug
    :long "debug"
    :help "Enable the Lisp debugger."
    :help-no "Disable the Lisp debugger (the default)."))

(defparameter *device*
  (adopt:make-option 'device
    :help (format nil "send command to device(default ~A)" "lg")
    :long "device"
    :short #\d
    :parameter "DEVICE"
    :initial-value "lg"
    :reduce #'adopt:last))

(defparameter *activity*
  (adopt:make-option 'activity
    :help (format nil "send activity to harmony")
    :long "activity"
    :short #\a
    :parameter "ACTIVITY"
    :reduce #'adopt:last))

(defparameter *ui*
  (adopt:make-interface
    :name "piek"
    :usage "[-d DEVICE COMMAND]"
    :summary "control tv through harmony through hass"
    :help "Control the TV, or Apple TV, or Telenet"
    :contents (list *help* *device* *option-debug* *option-no-debug* *activity*)
    :examples '(("Volume Down TV':" . "piek -d lg volumedown")
                ("Pause Apple TV:" . "piek -d apple pause")
                ("Pause Telenet:" . "piek -d telenet pause")
                ("Show all commands of a device:" . "piek -d lg commands"))))

(defun print-available-commands(device)
  (format t "~A" (device-commands device)))

(defun print-available-activities()
  (format t "~A" (mapcar #'activity-name (activities))))

(defun handle-activity(arguments activity-name)
  (let ((activity (with-name-matches "activity" activity-name)))
    (cond
      ((equal "list" (car arguments)) (print-available-activities))
      ((null activity) (error 'unknown-activity :available (mapcar #'activity-name (activities)) :used-activity activity-name))
      (t (send-activity :activity activity)))))

(defun handle-device(arguments device-name)
  (let ((device (device-matches-name device-name)))
    (cond
      ((equal "list" (car arguments)) (print-available-commands device))
      ((null device) (error 'unknown-device :available (mapcar #'device-name (devices)) :used-device device-name))
      (t (send-command :device device :commands arguments)))))

;; todo: also support direct id for the device
(defun toplevel ()
  ;; (sb-ext:disable-debugger)
  (handler-case
      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
        (when (gethash 'debug options)
          (sb-ext:enable-debugger)
          (setf drakma:*header-stream* *standard-output*)
          (setf *debug-is-on* t))
        (when (gethash 'help options)
          (adopt:print-help-and-exit *ui*))
        (when (gethash 'activity options)
          (handle-activity arguments (gethash 'activity options))
          (adopt:exit))
        (if (null arguments)
            (adopt:print-help-and-exit *ui*))
        (when (gethash 'device options)
          (handle-device arguments (gethash 'device options))
          (adopt:exit))
        )
    (error (c) (adopt:print-error-and-exit c))))
