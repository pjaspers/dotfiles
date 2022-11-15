(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort :adopt :drakma :lquery) :silent t))

(defpackage :imdb-quote-fetcher
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :imdb-quote-fetcher)

;;;; Configuration -----------------------------------------------
(defparameter *debug-is-on* nil)

;;;; Errors ------------------------------------------------------
(define-condition user-error (error) ())

(define-condition missing-foo (user-error) ()
  (:report "A foo is required, but none was supplied."))

;;;; Functionality -----------------------------------------------
(defun ensure-response-string (response)
  (etypecase response
    (string response)
    (stream (alexandria:read-stream-content-into-string response))
    (vector (flexi-streams:octets-to-string response))))

(defun do-imdb-request(imdb-number)
  (multiple-value-bind (body status headers uri stream needs-close reason)
      (drakma:http-request (format nil "https://www.imdb.com/title/~A/quotes" imdb-number))
    (setf body (ensure-response-string body))
    (if (<= 200 status 299)
        body
        (error "Received ~D ~A from ~A: ~A" status reason uri body))
    ))

(defun parse(html)
  (let ((doc (lquery:$ (initialize html))))
    (lquery:$ doc ".did-you-know-actions" (remove))
    (lquery:$ doc "#quotes_content .quote" (map #'(lambda (node) (format nil "~A" (ppcre:regex-replace-all ":\\n" (string-trim '(#\Space #\Tab #\Newline) (plump:text node)) ": ")))))))


;;;; Run ---------------------------------------------------------
(defun run (arguments)
  (let* ((imdb-number (car arguments))
         (lines (parse (do-imdb-request imdb-number))))
    ;; This would work on a list byt not on a vector
    ;; (format t "~{~A~#[~:;~%--~% ~] ~}" lines)
    (format t "~{~A~#[~:;~%--~% ~] ~}" (coerce lines 'list))
    ))

;;;; User Interface ----------------------------------------------
(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (sb-ext:exit :code 130))))

(defparameter *help*
  (adopt:make-option 'help
    :help "display help and exit"
    :long "help"
    :short #\h
    :reduce (constantly t)))

(adopt:defparameters (*option-debug* *option-no-debug*)
  (adopt:make-boolean-options 'debug
    :long "debug"
    :short #\d
    :help "Enable the Lisp debugger."
    :help-no "Disable the Lisp debugger (the default)."))

(defparameter *ui*
  (adopt:make-interface
   :name "imdb-quote-fetcher"
   :usage "tt0113277"
   :summary "quotes from imdb"
   :help "List all quotes from the provided imdb page"
   :contents (list *help* *option-debug* *option-no-debug*)
   ))

(defun toplevel ()
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
            (t (run arguments)))
        (user-error (e) (adopt:print-error-and-exit e))))))
