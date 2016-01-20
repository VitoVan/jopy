;;;; map.lisp

(in-package #:newhope)

(defvar api_key "yourkeyhere")

(defun get-geo-url(address city)
  (let* ((uri (concatenate 'string
                           "http://api.map.baidu.com/geocoder/v2/?address="
                           (cl-ppcre:regex-replace-all " " address "%20")
                           "&city="
                           city
                           "&output=json&ak="
                           api_key)))
    (progn
      ;;(format t "~A~%" uri)
      uri)))

(defun get-lng-lat(address city)
  (if (equal address "专业培训")
      (progn
        (format t "Empty: special ? ~%")
        (return-from get-lng-lat (values -1 -1))))
  (if (equal address "查看职位地图")
      (progn
        (format t "Empty: check map ~%")
        (return-from get-lng-lat (values -1 -1))))
  (if (equal address "远程办公")
      (progn
        (format t "Empty: remote ~%")
        (return-from get-lng-lat (values 0 0))))
  (if (> (length (trim address)) 0)
      (let* ((json-returned (decode-json-from-string
                             (flexi-streams:octets-to-string
                              (http-request (get-geo-url address city)
                                            :connection-timeout 5
                                            :force-binary t))))
             (status (cdr (assoc :status json-returned))))
        (cond
          ((= status 0)
           (let* ((geo-json (cdr (cadr (assoc :result json-returned))))
                  (lng (or (cdr (assoc :lng geo-json)) -2))
                  (lat (or (cdr (assoc :lat geo-json)) -2)))
             (progn
               (format t " ~A : ~A~%" lng lat)
               (values lng lat))))
          ((or (= status 4) (= status 302)) (progn
                          (format t "~%OUT OF USAGE: ~A ~%" status)
                          (sb-ext:exit)))
          (t (progn
               (format t "~%SOME THING WRONG: ~A ~%" status)
               nil))))))

(defun fill-db-geo()
  ;;(format t "Running..~%")
  (let* ((empty-results (query "select id, city,address from job where lng is null and lat is null and address <> '' limit 10")))
    (if (> (length empty-results) 0)
        (progn
          (dolist (x empty-results)
            (let* ((id (nth 0 x))
                   (address (nth 2 x))
                   (city (nth 1 x)))
              (progn
                (format t "~% ~A , ~A , ~A " (trim id) address city)
                (multiple-value-bind
                      (lng lat)
                    (handler-case
                        (get-lng-lat address city)
                      (error () (values -3 -3)))
                  (and lng lat
                       (execute
                        (concatenate
                         'string
                         "update job set lng = " (write-to-string lng)
                         ", lat = " (write-to-string lat) " where id = '" id "'")))))))
          (fill-db-geo)))))
