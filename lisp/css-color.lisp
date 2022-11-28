(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort :adopt :cl-ppcre) :silent t))

(defpackage :css-color
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :css-color)
(define-condition user-error (error) ())

(defparameter *use-shell-format* nil)
(defparameter *use-extended-description* nil)

(defun css-colors()
  (let ((result '()))
    (with-input-from-string (s (raw-css-colors))
      (do ((line (read-line s nil) ;; var init-form
                 (read-line s nil))) ;; step=form
          ((null line) (format nil "termination condition - line: ~s~% " line))
        (let ((color (parse-line line)))
          (unless (null color)
            (push color result)))))
    result
    ))

(defun random-css-color()
  (let (
        (*random-state* (make-random-state t))
        (colors (css-colors)))
    (nth (random (length colors)) colors)
    ))

(defstruct css-color name hex rgb ansi)


(defun ansi-from(r g b)
  (when (and (eq r g) (eq g b))
      (if (< r 8) (return-from ansi-from 16))
      (if (> r 248) (return-from ansi-from 231))

      (return-from ansi-from (+ (round (* (/ (- r 8) 247) 24)) 232))
      )

  (+ 16
     (* (round (* (/ r 255) 5)) 36)
     (* (round (* (/ g 255) 5)) 6)
     (round (* (/ b 255) 5)))
  )

(defun parse-decimals(decimal)
  (ppcre:register-groups-bind ((#'parse-integer r g b))
      ("^([0-9]+) ([0-9]+) ([0-9]+)" decimal)
    (list r g b)
    ))

(defun parse-hex(hex)
  (ppcre:register-groups-bind ((#'(lambda(a) (parse-integer a :radix 16)) r g b))
      ("^#([a-zA-Z0-9]{2})([a-zA-Z0-9]{2})([a-zA-Z0-9]{2})" hex)
    (list r g b)
    ))

(defun parse-line(line)
  (let ((regex "([a-z]+)\\s(#[a-zA-Z0-9]+)\\s+(\\d+\\s\\d+\\s\\d+)"))
    (cl-ppcre:register-groups-bind (name hex decimal-s)
        (regex line :sharedp t)
      (let ((rgb (parse-decimals decimal-s)))
        (make-css-color :name name :hex hex :rgb rgb :ansi (apply #'ansi-from rgb ))))
        ))

  (defun raw-css-colors()
        "
aliceblue #F0F8FF	240 248 255
antiquewhite	#FAEBD7	250 235 215
aqua	#00FFFF	0 255 255
aquamarine	#7FFFD4	127 255 212
azure	#F0FFFF	240 255 255
beige	#F5F5DC	245 245 220
bisque	#FFE4C4	255 228 196
black	#000000	0 0 0
blanchedalmond	#FFEBCD	255 235 205
blue	#0000FF	0 0 255
blueviolet	#8A2BE2	138 43 226
brown	#A52A2A	165 42 42
burlywood	#DEB887	222 184 135
cadetblue	#5F9EA0	95 158 160
chartreuse	#7FFF00	127 255 0
chocolate	#D2691E	210 105 30
coral	#FF7F50	255 127 80
cornflowerblue	#6495ED	100 149 237
cornsilk	#FFF8DC	255 248 220
crimson	#DC143C	220 20 60
cyan	#00FFFF	0 255 255
darkblue	#00008B	0 0 139
darkcyan	#008B8B	0 139 139
darkgoldenrod	#B8860B	184 134 11
darkgray	#A9A9A9	169 169 169
darkgreen	#006400	0 100 0
darkgrey	#A9A9A9	169 169 169
darkkhaki	#BDB76B	189 183 107
darkmagenta	#8B008B	139 0 139
darkolivegreen	#556B2F	85 107 47
darkorange	#FF8C00	255 140 0
darkorchid	#9932CC	153 50 204
darkred	#8B0000	139 0 0
darksalmon	#E9967A	233 150 122
darkseagreen	#8FBC8F	143 188 143
darkslateblue	#483D8B	72 61 139
darkslategray	#2F4F4F	47 79 79
darkslategrey	#2F4F4F	47 79 79
darkturquoise	#00CED1	0 206 209
darkviolet	#9400D3	148 0 211
deeppink	#FF1493	255 20 147
deepskyblue	#00BFFF	0 191 255
dimgray	#696969	105 105 105
dimgrey	#696969	105 105 105
dodgerblue	#1E90FF	30 144 255
firebrick	#B22222	178 34 34
floralwhite	#FFFAF0	255 250 240
forestgreen	#228B22	34 139 34
fuchsia	#FF00FF	255 0 255
gainsboro	#DCDCDC	220 220 220
ghostwhite	#F8F8FF	248 248 255
gold	#FFD700	255 215 0
goldenrod	#DAA520	218 165 32
gray	#808080	128 128 128
green	#008000	0 128 0
greenyellow	#ADFF2F	173 255 47
grey	#808080	128 128 128
honeydew	#F0FFF0	240 255 240
hotpink	#FF69B4	255 105 180
indianred	#CD5C5C	205 92 92
indigo	#4B0082	75 0 130
ivory	#FFFFF0	255 255 240
khaki	#F0E68C	240 230 140
lavender	#E6E6FA	230 230 250
lavenderblush	#FFF0F5	255 240 245
lawngreen	#7CFC00	124 252 0
lemonchiffon	#FFFACD	255 250 205
lightblue	#ADD8E6	173 216 230
lightcoral	#F08080	240 128 128
lightcyan	#E0FFFF	224 255 255
lightgoldenrodyellow	#FAFAD2	250 250 210
lightgray	#D3D3D3	211 211 211
lightgreen	#90EE90	144 238 144
lightgrey	#D3D3D3	211 211 211
lightpink	#FFB6C1	255 182 193
lightsalmon	#FFA07A	255 160 122
lightseagreen	#20B2AA	32 178 170
lightskyblue	#87CEFA	135 206 250
lightslategray	#778899	119 136 153
lightslategrey	#778899	119 136 153
lightsteelblue	#B0C4DE	176 196 222
lightyellow	#FFFFE0	255 255 224
lime	#00FF00	0 255 0
limegreen	#32CD32	50 205 50
linen	#FAF0E6	250 240 230
magenta	#FF00FF	255 0 255
maroon	#800000	128 0 0
mediumaquamarine	#66CDAA	102 205 170
mediumblue	#0000CD	0 0 205
mediumorchid	#BA55D3	186 85 211
mediumpurple	#9370DB	147 112 219
mediumseagreen	#3CB371	60 179 113
mediumslateblue	#7B68EE	123 104 238
mediumspringgreen	#00FA9A	0 250 154
mediumturquoise	#48D1CC	72 209 204
mediumvioletred	#C71585	199 21 133
midnightblue	#191970	25 25 112
mintcream	#F5FFFA	245 255 250
mistyrose	#FFE4E1	255 228 225
moccasin	#FFE4B5	255 228 181
navajowhite	#FFDEAD	255 222 173
navy	#000080	0 0 128
oldlace	#FDF5E6	253 245 230
olive	#808000	128 128 0
olivedrab	#6B8E23	107 142 35
orange	#FFA500	255 165 0
orangered	#FF4500	255 69 0
orchid	#DA70D6	218 112 214
palegoldenrod	#EEE8AA	238 232 170
palegreen	#98FB98	152 251 152
paleturquoise	#AFEEEE	175 238 238
palevioletred	#DB7093	219 112 147
papayawhip	#FFEFD5	255 239 213
peachpuff	#FFDAB9	255 218 185
peru	#CD853F	205 133 63
pink	#FFC0CB	255 192 203
plum	#DDA0DD	221 160 221
powderblue	#B0E0E6	176 224 230
purple	#800080	128 0 128
rebeccapurple	#663399	102 51 153
red	#FF0000	255 0 0
rosybrown	#BC8F8F	188 143 143
royalblue	#4169E1	65 105 225
saddlebrown	#8B4513	139 69 19
salmon	#FA8072	250 128 114
sandybrown	#F4A460	244 164 96
seagreen	#2E8B57	46 139 87
seashell	#FFF5EE	255 245 238
sienna	#A0522D	160 82 45
silver	#C0C0C0	192 192 192
skyblue	#87CEEB	135 206 235
slateblue	#6A5ACD	106 90 205
slategray	#708090	112 128 144
slategrey	#708090	112 128 144
snow	#FFFAFA	255 250 250
springgreen	#00FF7F	0 255 127
steelblue	#4682B4	70 130 180
tan	#D2B48C	210 180 140
teal	#008080	0 128 128
thistle	#D8BFD8	216 191 216
tomato	#FF6347	255 99 71
turquoise	#40E0D0	64 224 208
violet	#EE82EE	238 130 238
wheat	#F5DEB3	245 222 179
white	#FFFFFF	255 255 255
whitesmoke	#F5F5F5	245 245 245
yellow	#FFFF00	255 255 0
yellowgreen	#9ACD32	154 205 50
")


(defparameter *help*
  (adopt:make-option 'help
                     :help "display help and exit"
                     :long "help"
                     :short #\h
                     :reduce (constantly t)))

(defparameter *shell*
  (adopt:make-option 'shell
                     :long "shell"
                     :short #\s
                     :help "output in shell format"
                     :initial-value nil
                     :reduce (constantly t)))

(defparameter *full*
  (adopt:make-option 'full
                     :long "full"
                     :short #\f
                     :help "show name, hex and rgb"
                     :initial-value nil
                     :reduce (constantly t)))

(defparameter *random*
  (adopt:make-option 'random
                     :long "random"
                     :short #\r
                     :help "Return a random color."
                     :initial-value nil
                     :reduce (constantly t)))

(defparameter *ui*
  (adopt:make-interface
   :name "css-color"
   :usage "[COLOR OR NONE]"
   :summary "get css colors"
   :help "List all named css colors"
   :contents (list *help* *random* *shell* *full*)))

(defun ansi-color-start (color)
  ;; 38 for foreground
  (let ((background (css-color-ansi color))
        (foreground (css-color-ansi (contrasting-color color))))
    (format nil "~C[38;5;~Dm~C[48;5;~Dm" #\Escape foreground #\Escape background)))

(defun ansi-color-end ()
  (format nil "~C[0m" #\Escape))

(defun rgb-code (r g b)
  ;; The 256 color mode color values are essentially r/g/b in base 6, but
  ;; shifted 16 higher to account for the intiial 8+8 colors.
  (+ (* r 36)
     (* g 6)
     (* b 1)
     16))

(defun brightness(color)
  (apply #'brightness-index (css-color-rgb color)))

(defun brightness-index(r g b)
  (/ ( + (* 299 r) (* 587 g) (* 114 b)) 1000))

(defun hue-difference(r1 g1 b1 r2 g2 b2)
  (+
   (abs (- r1 r2))
   (abs (- g1 g2))
   (abs (- b1 b2))))

(defun color-difference(a b)
  (let ((rgbs (reduce #'cons (css-color-rgb a) :initial-value (css-color-rgb b) :from-end t)))
    (apply #'hue-difference rgbs)
    ))

(defun is-contrasting(a b)
  (let ((brightness-diff (abs (- (brightness a) (brightness b))))
        (color-diff (color-difference a b)))
    (and
     (> brightness-diff 110)
     (> color-diff 500))
    ))

;;  W3 deems that text and background colors have sufficient contrast when the brightness difference is over 125 AND the hue difference is over 500.
;; https://www.had2know.org/technology/color-contrast-calculator-web-design.html
(defun contrasting-color(color)
  (let ((contrasting-color (find color (css-colors) :test #'is-contrasting)))
    (if (null contrasting-color)
        (if (> (brightness color) 120)
            (color-by-name "black")
            (color-by-name "white"))

        contrasting-color)))

(defun print-css-color(color)
  (let* ((full-description (format nil "~A ~A ~A" (css-color-name color) (css-color-hex color) (css-color-rgb color)))
        (description (if *use-extended-description* full-description (css-color-name color))))
    (if *use-shell-format*
        (format t "~A~%" description)
        (format t "~A~A~A~%" (ansi-color-start color) description (ansi-color-end)))))

(defun colors-matching-name(name)
  (let ((scanner (cl-ppcre:create-scanner name :case-insensitive-mode t)))
    (remove-if-not
     #'(lambda (a) (cl-ppcre:scan scanner (css-color-name a)))
     (css-colors)
     )
    ))

(defun color-by-name(name)
  (find name (css-colors) :test #'(lambda (a b) (equal (css-color-name b) a))))

(defun handle-arguments(arguments)
  (let ((input (car arguments)))
    (cond
      ((cl-ppcre:scan "#[a-zA-Z0-9]{6}" input)
       (let ((rgb (parse-hex input)))
         (print-css-color (make-css-color :name input :hex input :rgb rgb :ansi (apply #'ansi-from rgb )))))
      ((null input) (mapcar #'print-css-color (css-colors)))
      (t (mapcar #'print-css-color (colors-matching-name (car arguments)))))))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (handler-case
      (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
        (when (gethash 'help options)
          (adopt:print-help-and-exit *ui*))
        (if (gethash 'shell options)
            (setf *use-shell-format* t))
        (if (gethash 'full options)
            (setf *use-extended-description* t))
        (when (gethash 'random options)
          (print-css-color (random-css-color))
          (adopt:exit))
        (handle-arguments arguments)
        )

    (user-error (e) (adopt:print-error-and-exit e))))
;; todo
;; - multiple arguments (so you can print a pallette)
;; - support the full option
;; - random names for all colors?
