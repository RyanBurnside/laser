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

;; Graham Queue model "On Lisp"
;; Car encodes contents, cdr encodes last ref
;; a nil CAR pops off NIL
(defun make-graham-queue () (cons nil nil))

(defun graham-enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q)))))

(defun graham-dequeue (q)
  (pop (car q)))

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
  '("G21"          ; Millimeters
    "G90"          ; Absolute positioning
    "G17"          ; XY Plane
    "M5"           ; Laser Off (spindle stop)
    "$H"           ; GRBR Home
    "G0 X100 Y100" ; Rapid Movement
    "G0 X200"
    "G0 Y200"
    "G0 X100"
    "G0 Y100"
    "M5"           ; Laser Off
    "G0 X0 Y0"     ; Move 0 0
    ))

(defun main (commands)
  ;; Get SERIAL-PORT-DESCRIPTION
  ;; (setf *conn-desc* (prompt-port-desc))
  (setf *conn-desc* (create-port-dialog
                     (list-serial-ports)
                     :key #'serial-port-description-name))

  ;; Establish connection based on serial port description
  (when (and (serial-port-p *conn*) (serial-port-alive *conn*))
    (format t "Port is already bound, shutting it down first.")
    (shutdown-serial-port *conn*))

  (setf *conn* (libserialport:open-serial-port
                (serial-port-description-name *conn-desc*)
                :baud 115200
                :bits 8
                :stopbits 1
                :parity :sp-parity-none
                :XONXOFF  :SP-XONXOFF-DISABLED
                :MODE     :SP-MODE-READ-WRITE
                :flowcontrol :sp-flowcontrol-none))

  (libserialport:serial-flush-buffer *conn*)

  (loop :with RX-BUFFER-SIZE := 65535 ; Your machine here
        :with g-count := 0 ; sent gcode line counter
        :with char-per-gcode-queue = (make-graham-queue) ; Graham queue RTFM
        :for line :in commands ; Raw line from input
        :for l-count :from 0 ; line counter
        :for current-gcode := (trim-whitespace line) ; Clean line from input
        :for grbl-out := ""
        :do ;; Track number of characters in grbl buffer
            (graham-enqueue (1+ (length current-gcode)) char-per-gcode-queue)

            ;; Drain all messages
            ;; popping off the associated char count from queue for each response
            (loop :with out-temp := ""
                  :for octets-waiting := (libserialport:serial-input-waiting *conn*)
                  :while (or (>= (apply '+ (car char-per-gcode-queue))
                                 (1- RX-BUFFER-SIZE))
                             (and (integerp octets-waiting)
                                  (plusp octets-waiting)))

                  :do
                     (setf out-temp (read-response *conn*))
                     ;; Handle non OK/Err return values, otherwise pop char counts
                     (cond ((and (null (find-string-equal "ok" out-temp))
                                 (null (find-string-equal "error" out-temp)))
                            (format t "DEBUG: ~a~%" out-temp))
                           (t (setf grbl-out (concatenate 'string grbl-out out-temp))
                              (incf g-count)
                              (graham-dequeue char-per-gcode-queue))))
            (send-command *conn* (format nil "~a~%" current-gcode)))

  (shutdown-all-serial-ports)
  )
