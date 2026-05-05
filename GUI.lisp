;;;; This is the GUI portion of the code
;;; GUI is generally a messy kind of thing so we seperate it here to
;; ensure that it'll last needing to be rewritten


(in-package :laser)

(defun create-port-dialog (connection-name-list &key key)
  "This is a simple dialog to prompt for port. It maintains the list of real items and uses the idex of the selection to map back. If a key is provided it calls that to make the passed list objects into something else."
  (let ((selection "not working"))
     (with-nodgui (:debug-tcl nil)
       (let* ((port-selector
                (make-instance 
                 'scrolled-listbox
                 :text "Select Port"))
              (OK
                (make-instance
                 'button
                 :text "Confirm Port"
                 :command (lambda ()
                            (setf selection (nth (car (listbox-get-selection-index port-selector))
                                                 connection-name-list))
                            (exit-nodgui)))))
         (pack port-selector)
         (pack OK)
         (listbox-insert (listbox port-selector) 0
                         (if key
                             (mapcar key connection-name-list)
                             connection-name-list))
         (listbox-select (listbox port-selector) 0)))
   selection))


  
