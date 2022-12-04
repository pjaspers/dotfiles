(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:cl-ppcre) :silent t))

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


"mary_poppins_returns-{tt1446192}.mkv"
(cl-ppcre:regex-replace-all "([a-zA-Z])"(string-downcase (cl-ppcre:regex-replace-all "\\s+" "Mary Poppin's Returns" "_":preserve-case t)) "")

(sanitized "Mary Poppin's Returns")
