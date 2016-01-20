;;;; newhope.lisp

(in-package #:newhope)

;;; "newhope" goes here. Hacks and glory await!
(setf sb-impl::*default-external-format* :UTF-8)

;; make drakma fetch as utf-8
(setf drakma:*drakma-default-external-format* :utf-8)

;; for some GBK page, we use Python's requests instead of drakma
(burgled-batteries:startup-python)
(burgled-batteries:import "requests")

;; connect database
(connect-toplevel "whereisjob" "whereisjob" "123456" "localhost")

(defun trim(input)
  (string-trim 
   '(#\Space #\Newline #\Backspace #\Tab 
     #\Linefeed #\Page #\Return #\Rubout)
   input))

(defun css->htmls(selector html)
           (map 'list
                #'(lambda(node) (plump:serialize node nil))
                (clss:select selector (plump:parse html))))

(defun css->text(selector html &key attribute contains)
  (let* ((dom-vector (clss:select selector (plump:parse html)))
         (dom
          (and (> (length dom-vector) 0)
               (if contains
                   (find-if-not #'null (map 'list
                             #'(lambda(node)
                                 (if (cl-ppcre:scan contains (plump:serialize node nil)) node))
                             dom-vector))
                   (vector-pop dom-vector)))))
    (if dom
        (trim (if attribute
                  (plump:get-attribute dom attribute)
                  (plump:text dom)))
        "")))


(defun start-single-request(name)
  (funcall (intern (string-upcase (concatenate 'string name "-start-request")) 'newhope)))

(defun start-all-requests()
  (progn
    (format t "Starting 51job requests...... ~%")
    (51job-start-request)
    (format t "Starting zhaopin requests...... ~%")
    (zhaopin-start-request)
    (format t "Starting neitui requests...... ~%")
    (neitui-start-request)
    (format t "Starting lagou requests...... ~%")
    (lagou-start-request)))
