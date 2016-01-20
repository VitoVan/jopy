;;;; neitui_spider.lisp

(in-package #:newhope)

;; some pages contain characters not encoded in utf-8, which will stuck in drakma.
(defun neitui-http-request(url)
  (progn
    (burgled-batteries:run (concatenate 'string "r = requests.get('" url "')"))
    (burgled-batteries:run "r.encoding = 'utf-8'")
    (cl-ppcre:regex-replace-all #\Backspace (burgled-batteries:run "r.text")  "")))

(defvar neitui_base_url "http://www.neitui.me/?name=neitui&handle=lists&kcity=")
;;北京 郑州
(defvar neitui_city_list '("北京" "郑州"))
(defvar neitui_year (write-to-string (nth 5 (multiple-value-list (get-decoded-time)))))
(defvar neitui_today (multiple-value-bind
      (second minute hour date month year day-of-week dst-p tz)
    (get-decoded-time)
  (declare (ignorable second minute hour year day-of-week dst-p tz))
  (format nil "~2,'0d月~2,'0d日"
          month
          date)))
(defvar neitui_full_today
  (concatenate 'string neitui_year "-"
               (multiple-value-bind
                     (second minute hour date month year day-of-week dst-p tz)
                   (get-decoded-time)
                 (declare (ignorable second minute hour year day-of-week dst-p tz))
                 (format nil "~2,'0d-~2,'0d"
                         month
                         date))))

(defun neitui-parse-job(job-page &key id company url city)
  "Parse HTML of job page, for table job, insert directly"
  (let* ((title (css->text "div.jobnote> strong.padding-r10" job-page))
         (salaries (or (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "[0-9]+" (css->text "div.jobnote>span.pay" job-page))) '(0 0)))
         (min_salary (* (car salaries) 1000))
         (max_salary (* (or (cadr salaries) 0) 1000))
         (work_years (or (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "[0-9]+" (css->text "div.jobnote>span.experience" job-page))) '(0 0)))
         (min_work_year (car work_years))
         (max_work_year (or (cadr work_years) 0))
         (address
          (cl-ppcre:regex-replace-all
           "地点："
           (css->text "div.jobtitle>span.jobtitle-r" job-page) ""))
         (description (css->text "div.jobdetail" job-page))
         (raw_data job-page))
    (execute "INSERT INTO job VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)"
             id title min_salary max_salary min_work_year
             max_work_year city address description company raw_data url 0 neitui_full_today)))

(defun neitui-parse-company(job-page)
  "Parse HTML of job page, for table company, insert directly"
  (let* ((short_name (css->text "div.c_name>a" job-page))
         (company-url (concatenate 'string "http://www.neitui.me" (css->text "div.c_name>a" job-page :attribute "href")))
         (name short_name)
         (slogan (css->text "div.plate.company_information > dl:nth-child(6) > dd" job-page))
         (description
          (let* ((company-page (neitui-http-request company-url)))
            (css->text "#J_Company_Textarea" company-page)))
         (industry (css->text "div.plate.company_information > dl:nth-child(4) > dd:nth-child(4)" job-page))
         (address (css->text "div.plate.company_information > dl:nth-child(2) > dd:nth-child(2)" job-page))
         (website (css->text "dl.ci_body>dd>a[rel='nofollow']" job-page :attribute "href"))
         (raw_data job-page)
         (raw_url company-url))
    (let* ((real-name (caar (query "select * from company where short_name = $1 or name = $2" name name))))
      (if real-name
          (format t "C.. ")
          (execute "INSERT INTO company VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)"
                   name short_name slogan description industry address website raw_data raw_url neitui_full_today))
      (or real-name name))))

(defun neitui-parse-html(html-response)
  "Parse and Store the HTML data, return -1 if some of the data is out of date (not today)"
  (let* ((job-list (css->htmls ".jobinfo" html-response))
         (next-page (css->htmls "div > p > a.next" html-response)))
    (progn
      (dolist (job job-list)
        (let* ((job-note (css->text "div.jobnote:first-of-type" job))
               (job-url (concatenate 'string "http://www.neitui.me"
                                     (css->text "div.jobnote>div.jobnote-l>a" job :attribute "href")))
               (create-date (cl-ppcre:scan-to-strings "[0-9]{2}月[0-9]{2}日" job-note))
               (job-id (cl-ppcre:scan-to-strings "[0-9]+" job-url))
               (id (concatenate 'string "neitui_" job-id))
               (position-name (css->text "div.jobnote>div.jobnote-l>a" job))
               (company-name (css->text "div.jobnote-r>a" job))
               (city (cl-ppcre:regex-replace-all "\\[|\\]" (css->text "div.jobnote>div.jobnote-l>span" job) "")))
          (if (equal neitui_today create-date)
              (progn
                (format t "~A , ~A , ~A , ~A ~%"
                        position-name company-name city
                        create-date)
                (if (> (length (query "select * from job where id = $1" id)) 0)
                    ;; If data exists, then update_*
                    (progn
                      (format t "S.. ")
                      (execute "update job set update_date = $1, update_count = update_count + 1 where id = $2" neitui_full_today id))
                    ;;If not exists, scrape the HTML of job page
                    (let* ((job-page (neitui-http-request job-url)))
                      (neitui-parse-job job-page :id id :url job-url :company (neitui-parse-company job-page) :city city))))
              (progn
                (format t "~% OUT-OF-DATE ~% POSITION: ~A , DATE: ~A ~% ~%" position-name create-date)
                (return-from neitui-parse-html  -1)))))
      ;;check if there is next page, return -1 if not
      (if (< (length next-page) 1)
          (progn
            (format t "~% NO FURTHER PAGE ~% ")
            -1)))))

(defun neitui-parse-url(url &key (pn 1) (recursive t))
  (if (> pn 30)
      (format t "===== PAGE > 30, QUIT~%")
      (let* ((page-url (concatenate 'string url "&page=" (write-to-string pn)))
         (drakma-response (neitui-http-request page-url)))
    (if (and recursive (not (equal (neitui-parse-html drakma-response) -1)))
        (progn
          (format t "========== NEW PAGE ========== ~A ~%" (write-to-string (+ pn 1)))
          (neitui-parse-url url :pn (incf pn)))
        drakma-response))))

(defun neitui-start-request(&key city_list)
  (dolist (city (or city_list neitui_city_list))
    (progn
      (format t "========= PROCESSING CITY: ~A =========~%" city)
      (neitui-parse-url (concatenate 'string neitui_base_url city)))))
