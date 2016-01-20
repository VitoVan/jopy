;;;; lagou_spider.lisp

(in-package #:newhope)

(defvar lagou_agent "Mozilla/5.0 (Linux; U; Android 4.3; en-us; SM-N900T Build/JSS15J) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30")
(defvar lagou_cookie_jar (make-instance 'drakma:cookie-jar))
(defvar lagou_base_url "http://www.lagou.com/jobs/positionAjax.json?px=new&city=")
(defvar lagou_job_url "http://www.lagou.com/jobs/#ID#.html")
(defvar lagou_company_url "http://www.lagou.com/gongsi/#ID#.html")
(defvar lagou_city_list '("北京" "郑州"))
(defvar lagou_today (multiple-value-bind
      (second minute hour date month year day-of-week dst-p tz)
    (get-decoded-time)
  (declare (ignorable second minute hour day-of-week dst-p tz))
  (format nil "~d-~2,'0d-~2,'0d"
          year
          month
          date)))

(defun make-counter (initial-value) ; initial-value is the variable we will capture in our anonymous function
  (lambda ()			; We'll make a closure that takes no parameters
	(incf initial-value)))	 ; This is equivalent to '++initial-value' in C

(defvar *lagou-http-counter* (make-counter 0))

(defun http-count()
  (if (> (funcall *lagou-http-counter*) 2)
      (progn
        (format t "sleeping......")
        (sleep (random 3))
        (format t "up!~%")
        (setf *lagou-http-counter* (make-counter 0)))))

(defmacro lagou-http-request(uri &rest rest)
  `(progn
     (http-count)
     (format t "Requesting: ~A~%" ,uri)
     (handler-case
         (sb-ext:with-timeout 5
           (http-request ,uri ,@rest
                         :connection-timeout 5
                         :cookie-jar lagou_cookie_jar
                         :user-agent lagou_agent))
       (error (condition)
         (progn
           (format t "Error happening: ~A , restarting request. ~%" condition)
           (sleep 10)
           (handler-case
               (sb-ext:with-timeout 5
                 (http-request ,uri ,@rest
                               :connection-timeout 5
                               :cookie-jar lagou_cookie_jar
                               :user-agent lagou_agent))
             (error (condition) nil)))))))

(defun lagou-scrape-job(job-id)
  "Extract [address, description, raw_data, raw_url] from job page"
  (let* ((job-url (cl-ppcre:regex-replace "#ID#" lagou_job_url job-id))
         (drakma-response (lagou-http-request job-url))
         (address (css->text "#container > div.content_r > dl > dd > div:nth-child(5)" drakma-response))
         (description (cl-ppcre:regex-replace "职位描述"
                                               (css->text "#container > div.content_l.fl > dl.job_detail > dd.job_bt" drakma-response) ""))
         (raw_data drakma-response)
         (raw_url job-url))
    (values address description raw_data raw_url)))

(defun lagou-parse-job(job-json)
  "Parse JSON and extract [address, description, raw_data, raw_url] from job page, only update_* if exists"
  (let* ((job-id (write-to-string (cdr (assoc :position-id job-json))))
         (id (concatenate 'string "lagou_" job-id))
         (title (cdr (assoc :position-name job-json)))
         (salaries (or (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "[0-9]+" (cdr (assoc :salary job-json)))) '(0 0)))
         (min_salary (* (car salaries) 1000))
         (max_salary (* (or (cadr salaries) 0) 1000))
         (work_years (or (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "[0-9]+" (cdr (assoc :work-year job-json)))) '(0 0)))
         (min_work_year (car work_years))
         (max_work_year (or (cadr work_years) 0))
         (city (cdr (assoc :city job-json)))
         (company (cdr (assoc :company-short-name job-json))))
    (if (> (length (query "select * from job where id = $1" id)) 0)
        ;; If data exists, then update_*
        (progn
          (format t "J.. ")
          (execute "update job set update_date = $1, update_count = update_count + 1 where id = $2" lagou_today id))
        ;; New data, scrape and store it.
        (multiple-value-bind (address description raw_data raw_url)
            (lagou-scrape-job job-id)
          (execute "INSERT INTO job VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)"
                   id title min_salary max_salary min_work_year
                   max_work_year city address description company raw_data raw_url 0 lagou_today)))))

(defun lagou-scrape-company(company-id)
  "Extract [slogan, description, address, website, raw_data, raw_url] from company page"
  (let* ((company-url (cl-ppcre:regex-replace "#ID#" lagou_company_url company-id))
         (drakma-response (lagou-http-request company-url))
         (slogan
          (css->text "body > div.top_info > div > div > div.company_main > div" drakma-response))
         (description
          (css->text "#company_intro > div.item_content > div.company_intro_text > span.company_content" drakma-response))
         (address
          (css->text "ul > li > p.mlist_li_desc" drakma-response))
         (website
          (css->text "body > div.top_info > div > div > div.company_main > a.icon-wrap" drakma-response :attribute "href"))
         (raw_data drakma-response)
         (raw_url company-url))
    (values slogan description address website raw_data raw_url)))

(defun lagou-parse-company(job-json)
  "Parse JSON and extract [...] from company page, skip if exists"
  (let* ((company-id (write-to-string (cdr (assoc :company-id job-json))))
         (name (cdr (assoc :company-short-name job-json)))
         (short_name (cdr (assoc :company-name job-json)))
         (industry (cdr (assoc :industry-field job-json))))
    (if (< (length (query "select * from company where name = $1" name)) 1)
        ;; Data not exists, scrape and store it.
        (multiple-value-bind (slogan description address website raw_data raw_url)
            (lagou-scrape-company company-id)
          (execute "INSERT INTO company VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)"
                   name short_name slogan description industry address website raw_data raw_url lagou_today))
        (format t "C.."))))

(defun lagou-fix-company-address(url-list)
  (dolist (url url-list)
    (multiple-value-bind (slogan description address website raw_data raw_url)
        (lagou-scrape-company (cl-ppcre:regex-replace-all "http://www.lagou.com/gongsi/|.html" url ""))
      (declare (ignorable slogan description website raw_data raw_url))
      (progn
        (format t "ADDRESS: ~A ~%" address)
        (execute "update company set address = $1 where raw_url = $2"
               address url)))))

(defun lagou-parse-json(json-response)
  "Parse and Store the JSON data, return -1 if some of the data is out of date (not today)"
  (if (cdr (assoc :success json-response))
      (let* ((job-list (cdr (assoc :result (cdr (assoc :content json-response))))))
        (if (> (length job-list) 0)
            (dolist (job job-list)
              (let* ((create-date (subseq (cdr (assoc :create-time job)) 0 10))
                     (position-name (cdr (assoc :position-name job)))
                     (company-name (cdr (assoc :company-short-name job)))
                     (city (cdr (assoc :city job))))
                (if (equal lagou_today create-date)
                    (progn
                      (format t "~A , ~A , ~A , ~A , ~A ~%"
                              (cdr (assoc :position-id job))
                              position-name company-name city
                              (cdr (assoc :create-time job)))
                      (progn
                        (lagou-parse-job job)
                        (lagou-parse-company job)))
                    (progn
                      (format t "~% OUT-OF-DATE ~% POSITION: ~A , DATE: ~A ~% ~%" position-name create-date)
                      (return -1)))))
            -1))))

(defun lagou-parse-url(url &key (pn 1) (recursive t))
  (let* ((drakma-response (lagou-http-request url
                                         :method :post
                                         :parameters `(("pn" . ,(write-to-string pn))
                                                       ("first" . "false")
                                                       ("kd" . ""))
                                         :force-binary t))
         (string-response (flexi-streams:octets-to-string
                           drakma-response
                           :external-format :utf-8))
         (json-response (decode-json-from-string string-response)))
    (if (and recursive (not (equal (lagou-parse-json json-response) -1)))
        (progn
          (format t "========== NEW PAGE ========== ~A ~%" (write-to-string (+ pn 1)))
          (lagou-parse-url url :pn (incf pn)))
        json-response)))

(defun lagou-start-request(&key city_list)
  (dolist (city (or city_list lagou_city_list))
    (progn
      (format t "========= PROCESSING CITY: ~A =========~%" city)
      (lagou-parse-url (concatenate 'string lagou_base_url city)))))
