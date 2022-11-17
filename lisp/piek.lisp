(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:st-json :adopt :drakma :alexandria :with-user-abort :cl-ppcre) :silent t))

(defpackage :piek
  (:use :cl)
  (:export :toplevel :*ui*))

(in-package :piek)
(setf drakma:*header-stream* *standard-output*)
(defvar *api-token* (uiop:getenv "HASS_TOKEN"))
(defvar *hass-url* (uiop:getenv "HASS_IP"))

(defvar *harmony-conf* (with-open-file (stream "harmony.conf")
                         (st-json:read-json stream)))

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

;; (assoc "-1" (slot-value (st-json:getjso "Activities" *harmony-conf*) 'st-json::alist) :test #'string=)
;; (mapcar #'cdr (slot-value (st-json:getjso "Activities" *harmony-conf*) 'st-json::alist))
;; (mapcar #'car (slot-value (st-json:getjso "Activities" *harmony-conf*) 'st-json::alist))

(defstruct device name id commands)
(defstruct activity name id)
(defun json->device(name jso)
  (let ((commands (st-json:getjso "commands" jso))
         (id (st-json:getjso "id" jso)))
    (make-device :name name :id id :commands commands)
    ))

(defun devices-from-conf(conf)
  (let ((thing nil))
    (st-json:mapjso #'(lambda (k v)
                        (push (json->device k v) thing)
                        ) (st-json:getjso "Devices" *harmony-conf*))
    thing))

(defun devices()
  (devices-from-conf *harmony-conf*)
  )

(defun device-with-id (id)
  (find id (devices-from-conf *harmony-conf*) :key 'device-id :test 'equal))

(defun device-with-name (name)
  (find name (devices-from-conf *harmony-conf*) :key 'device-name :test 'equal))

(defun device-matches-name (name)
  (let ((scanner (cl-ppcre:create-scanner name :case-insensitive-mode t)))
    (find name (devices) :test #'(lambda (a b) (cl-ppcre:scan scanner (device-name b))))
    ))

(defun device-name-from-id(identifier jso)
  (cl-ppcre:scan "lg" (string-downcase "LG"))
  ;; (mapcar #'(lambda(k) (st:json:getjso* (format nil "Devices.~a.id" k) *harmony-conf*)) names)


  (let ((names (jso-keys (st-json:getjso "Devices" *harmony-conf*))))
    (mapcar #'(lambda(k)
                ;; (st-json:getjso* (format nil "Devices.~a.id" k) *harmony-conf*)
                ;; (cons k (st-json:getjso "id" (st-json:getjso k (st-json:getjso "Devices" *harmony-conf*))))
                (let ((path (format nil "Devices.~s.id" k)))
                  (format t "Path: |~a|" path)
                  (type-of "Devices.Apple TV.id")
                  (type-of (string-downcase path))
                  (type-of path)
                  (st-json:getjso* (string-downcase path) *harmony-conf*)
                  (type-of "Devices.Apple TV.id")
                  (st-json:getjso* `(,@(path)) *harmony-conf*)
                  )
                ) names))




  (st-json:getjso* "Devices.LG TV (2).id" *harmony-conf*)
  (st-json:getjso* "Devices.Telenet DVR.id" *harmony-conf*)
  (st-json:getjso* "Devices.Apple TV.id" *harmony-conf*)
  (st-json:getjso* "Devices.LG TV.id" *harmony-conf*)
  (st-json:mapjso #'(lambda (k v) (format t "K: ~a V: ~a" k v)) *harmony-conf*)
  (type-of (st-json:getjso "Devices" *harmony-conf*))
  )
;; http://100.74.48.103:8123/api/services/remote/send_command
(defun send-command(&key (entity-id "remote.piek") (device "50973046") (commands ()))
  (format t "entity: ~a device: ~a commands: ~a" entity-id device commands)
  (multiple-value-bind (body status headers uri stream needs-close reason)
      (drakma:http-request (concatenate 'string "http://" *hass-url* ":8123/api/services/remote/send_command")
                           :content (st-json:write-json-to-string
                                     (st-json:jso
                                      "entity_id" entity-id
                                      "device" device
                                      "command" commands))
                           :content-type "application/json"
                           :method :post
                           :additional-headers (list (cons "Authorization" (format nil "Bearer ~a" *api-token*))))
    (setf body (ensure-response-string body))
    (if (<= 200 status 299)
        (st-json:read-json body)
        (error "Received ~D ~A from ~A: ~A" status reason uri body))
    ))


;; (st-json:write-json-to-string (st-json:jso "bob" "stinkt" "so" "beit"))
;; (st-json:write-json-to-string (apply #'st-json:jso (alexandria:flatten '(("bob" "stinkt") ("so" "beit")))))



(defparameter *help*
  (adopt:make-option 'help
    :help "display help and exit"
    :long "help"
    :short #\h
    :reduce (constantly t)))

(defparameter *device*
  (adopt:make-option 'device
    :help (format nil "send command to device(default ~A)" "lg")
    :long "device"
    :short #\d
    :parameter "DEVICE"
    :initial-value "lg"
    :reduce #'adopt:last))

(defparameter *ui*
  (adopt:make-interface
    :name "piek"
    :usage "[-d DEVICE COMMAND]"
    :summary "control tv through harmony through hass"
    :help "Control the TV, or Apple TV, or Telenet"
    :contents (list *help* *device*)
    :examples '(("Volume Down TV':" . "piek -d lg volumedown")
                ("Pause Apple TV:" . "piek -d apple pause")
                ("Pause Telenet:" . "piek -d telenet pause")
                ("Show all commands of a device:" . "piek -d lg commands"))))

(defun print-available-commands(device)
  (format t "~A" (device-commands device)))

;; todo: also support direct id for the device
(defun toplevel ()
  (handler-case
      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
        (when (gethash 'help options)
          (adopt:print-help-and-exit *ui*))
        (if (null arguments)
          (adopt:print-help-and-exit *ui*))
        ;; (run (gethash 'device options))
        ;; (format t "args: ~a options: ~a" arguments options)
        (let ((device (device-matches-name (gethash 'device options))))
          (cond
            ((equal "commands" (car arguments)) (print-available-commands device))
            (t (send-command :device (device-id device) :commands arguments)))))
    (error (c) (adopt:print-error-and-exit c))))
