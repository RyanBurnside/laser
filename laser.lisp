;;;; GRBL Spec and Implementation
;;;; https://github.com/grbl/grbl
;;;; Example "Scripts" here: https://github.com/grbl/grbl/tree/master/doc/script

;;;; src/laser.lisp
(in-package :laser)

;; The connection
(defparameter *conn* nil)
(defparameter *conn-desc* nil)

(defun trim-whitespace (str)
  (string-trim '(#\Newline #\Linefeed #\Return #\Space #\Tab) str))

(defun find-string-equal (substr str)
  "A case insensitive search for substr's position in str"
  (search substr str :test #'string-equal))

(defun prompt-port-desc ()
  "Returns SERIAL-PORT-DESCRIPTION object."
  (let ((ports (list-serial-ports))
        (choice nil))
    (format *standard-output* "Please Enter a Port Number:~%")
    (loop for i from 0
          for port in ports
          do (format *standard-output*
                     "Choice ~a -> ~a.~%"
                     i
                     (serial-port-description-name port)))

    (loop :while (or (null (integerp choice))
                     (null (< -1 choice (length ports))))
          :do (format *standard-output* "Enter port number: ~%")
              (setf choice (read)))
    (nth choice ports)))

(defun send-command (conn string)
  (libserialport:serial-write-data conn string :line-end :lf))

(defun read-response (conn &key (timeout-ms 1000) (max-octets 65536))
  ;; read a line (string), converting octets to chars using Babel
  (multiple-value-bind (str finished-line-p decoding-error timed-out-p)
      (libserialport:serial-read-line
       conn
       :blocking NIL
       :timeout timeout-ms
       :max-length max-octets
       :line-termination-char #\Newline
       ;; babel encoding...
       :encoding :latin-1)

    ;; Check the return values properly:
    ;; https://github.com/jetmonk/cl-libserialport/blob/main/libserialport-io.lisp#L319
    str))


(defparameter *test-set*
  '("G21"
    "G90"
    "G17"
    "M5"
    "$H"
    "G54"
    "G0 X100 Y100"
    "G0 X300 Y100"
    "G0 X300 Y300"
    "G0 X100 Y300"
    "G0 X100 Y100"
    "M5"
    "G0 X0 Y0"))

(defun main (commands)
  ;; Get SERIAL-PORT-DESCRIPTION
  (setf *conn-desc* (prompt-port-desc))
  ;; Establish connection based on serial port description
  (setf *conn* (libserialport:open-serial-port
                (serial-port-description-name *conn-desc*)
                :baud 115200
                :bits 8
                :stopbits 1
                :parity :sp-parity-none
                :XONXOFF  :SP-XONXOFF-DISABLED ;; Pete
                :MODE     :SP-MODE-READ-WRITE ;; Pete
                :flowcontrol :sp-flowcontrol-none))

  (loop :with RX-BUFFER-SIZE := 128
        :with g-count := 0 ; sent gcode line counter
        :with c-line = nil ; queue (list) of input line lengths
        :for line :in commands ; Raw line from input
        :for l-count :from 0 ; line counter
        :for l-block := (trim-whitespace line) ; Clean line from input
        :for grbl-out := ""
        ;; Track number of characters in grbl buffer
        :do
           (setf c-line (append c-line (list (1+ (length l-block)))))

           (loop :with out-temp := ""
                 :for octets-waiting := (libserialport:serial-input-waiting *conn*)
                 :while (or (>= (apply #'+ c-line)
                                (1- RX-BUFFER-SIZE))
                            (and (integerp octets-waiting)
                                 (plusp octets-waiting)))

                 :do
                    (setf out-temp (read-response *conn*))
                    (cond ((and (null (find-string-equal "ok" out-temp))
                                (null (find-string-equal "error" out-temp)))
                           (format t "DEBUG: ~a~%" out-temp))
                          (t (setf grbl-out (concatenate 'string grbl-out out-temp))
                             (incf g-count) ;
                             (pop c-line))))
           (send-command *conn* (format nil "~a~%" l-block))))
