;;;; zhaopin_spider.lisp

(in-package #:newhope)

(defvar zhaopin_base_url "http://sou.zhaopin.com/jobs/searchresult.ashx?bj=160000&sm=1&jl=")
(defvar zhaopin_job_url "http://jobs.zhaopin.com/#ID#.htm")
(defvar zhaopin_city_list '("北京" "郑州"))
(defvar zhaopin_year (write-to-string (nth 5 (multiple-value-list (get-decoded-time)))))
(defvar zhaopin_today (multiple-value-bind
      (second minute hour date month year day-of-week dst-p tz)
    (get-decoded-time)
  (declare (ignorable second minute hour year day-of-week dst-p tz))
  (format nil "~2,'0d-~2,'0d"
          month
          date)))
(defvar zhaopin_full_today (concatenate 'string zhaopin_year "-" zhaopin_today))

(defun zhaopin-parse-job(job-page)
  "Parse HTML of job page, for table job, insert directly"
  (let* ((job-id (cl-ppcre:regex-replace-all
                  "http://jobs.zhaopin.com/|.htm|.*_"
                  (css->text "link[href^='http://jobs.zhaopin.com']" job-page :attribute "href") ""))
         (id (concatenate 'string "zhaopin_" job-id))
         (title (css->text "div.fixed-inner-box > div.inner-left.fl > h1" job-page))
         (salaries (or (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "[0-9]+" (css->text "div.terminalpage.clearfix > div.terminalpage-left > ul > li:nth-child(1) > strong" job-page))) '(0 0)))
         (min_salary (car salaries))
         (max_salary (or (cadr salaries) 0))
         (work_years (or (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "[0-9]+" (css->text "div.terminalpage.clearfix > div.terminalpage-left > ul > li:nth-child(5) > strong" job-page))) '(0 0)))
         (min_work_year (car work_years))
         (max_work_year (or (cadr work_years) 0))
         (city (css->text "div.terminalpage.clearfix > div.terminalpage-left > ul > li:nth-child(2) > strong > a" job-page))
         (address (trim (cl-ppcre:scan-to-strings "^.*" (css->text "div.terminalpage.clearfix > div.terminalpage-left > div.terminalpage-main.clearfix > div > div:nth-child(1) > h2" job-page))))
         (raw_des (css->text "div.terminalpage.clearfix > div.terminalpage-left > div.terminalpage-main.clearfix > div > div:nth-child(1)" job-page))
         (des_position (cl-ppcre:all-matches "SWSStringCutStart|SWSStringCutEnd" raw_des))
         (description (and des_position (trim (subseq raw_des (nth 1 des_position) (nth 2 des_position)))))
         (com_short_name (css->text "body > div.terminalpage.clearfix > div.terminalpage-right > div.company-box > p > a" job-page))
         (com_name (cl-ppcre:regex-replace "※发布本招聘广告企业的营业执照名称：" (css->text "body > div.terminalpage.clearfix > div.terminalpage-left > div.terminalpage-main.clearfix > div > div:nth-child(2) > div" job-page) ""))
         (company (if (> (length (trim com_name)) 0) com_name com_short_name))
         (raw_data job-page)
         (raw_url (css->text "link[href^='http://jobs.zhaopin.com']" job-page :attribute "href")))
    (execute "INSERT INTO job VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)"
             id title min_salary max_salary min_work_year
             max_work_year city address description company raw_data raw_url 0 zhaopin_full_today)))

(defun zhaopin-parse-company(job-page)
  "Parse HTML of job page, for table company, insert directly"
  (let* ((short_name (css->text "body > div.terminalpage.clearfix > div.terminalpage-right > div.company-box > p > a" job-page))
         (name (cl-ppcre:regex-replace "※发布本招聘广告企业的营业执照名称：" (css->text "body > div.terminalpage.clearfix > div.terminalpage-left > div.terminalpage-main.clearfix > div > div:nth-child(2) > div" job-page) ""))
         (name (if (> (length (trim name)) 0) name short_name))
         (slogan "")
         (description (trim (cl-ppcre:regex-replace-all "^.*
|※发布本招聘广告企业的营业执照名称.*$" (css->text "div.terminalpage.clearfix > div.terminalpage-left > div.terminalpage-main.clearfix > div > div:nth-child(2)" job-page) "")))
         (industry (css->text "div.terminalpage.clearfix > div.terminalpage-right > div.company-box > ul > li:nth-child(3) > strong > a" job-page))
         (address (css->text "div.terminalpage.clearfix > div.terminalpage-right > div.company-box > ul> li > strong" job-page))
         (website (css->text "div.terminalpage.clearfix > div.terminalpage-right > div.company-box > ul > li:nth-child(4) > strong > a" job-page))
         (raw_data job-page)
         (raw_url (css->text "link[rel='canonical']" job-page :attribute "href")))
    (if (< (length (query "select * from company where name = $1" name)) 1)
        (execute "INSERT INTO company VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)"
                 name short_name slogan description industry address website raw_data raw_url zhaopin_full_today)
        (format t "C.. "))))

(defun zhaopin-parse-html(html-response)
  "Parse and Store the HTML data, return -1 if some of the data is out of date (not today)"
  (let* ((job-list (css->htmls "#newlist_list_content_table > div" html-response))
         (next-page (css->htmls "a.next-page[href]" html-response)))
    (progn
      (dolist (job job-list)
        (let* ((create-date (css->text "div > dl > p" job))
               (job-id (cl-ppcre:regex-replace-all
                        "http://jobs.zhaopin.com/|.htm|.*_"
                        (css->text "a[href^='http://jobs.zhaopin.com']" job :attribute "href") ""))
               (id (concatenate 'string "zhaopin_" job-id))
               (position-name (css->text "a[href^='http://jobs.zhaopin.com/']" job))
               (company-name (css->text "div > ul > li.newlist_deatil_three.gsmc > a" job))
               (city (cl-ppcre:regex-replace-all "地点：| - .*"
                                                 (css->text "div > ul > li.newlist_deatil_two > span:nth-child(1)" job)
                                                 "")))
          (if (equal zhaopin_today create-date)
              (progn
                (format t "~A , ~A , ~A , ~A ~%"
                        position-name company-name city
                        create-date)
                
                (if (> (length (query "select * from job where id = $1" id)) 0)
                    ;; If data exists, then update_*
                    (progn
                      (format t "S.. ")
                      (execute "update job set update_date = $1, update_count = update_count + 1 where id = $2" zhaopin_full_today id))
                    ;;If not exists, scrape the HTML of job page
                    (let* ((job-url (cl-ppcre:regex-replace "#ID#" zhaopin_job_url job-id))
                           (job-page (http-request job-url)))
                      (progn
                        (zhaopin-parse-job job-page)
                        (zhaopin-parse-company job-page)))))
              (progn
                (format t "~% OUT-OF-DATE ~% POSITION: ~A , DATE: ~A ~% ~%" position-name create-date)
                (return-from zhaopin-parse-html  -1)))))
      ;;check if there is next page, return -1 if not
      (if (< (length next-page) 1)
          (progn
            (format t "~% NO FURTHER PAGE ~% ")
            -1)))))

(defun zhaopin-parse-url(url &key (pn 1) (recursive t))
  (let* ((page-url (concatenate 'string url "&p=" (write-to-string pn)))
         (drakma-response (http-request page-url)))
    (if (and recursive (not (equal (zhaopin-parse-html drakma-response) -1)))
        (progn
          (format t "========== NEW PAGE ========== ~A ~%" (write-to-string (+ pn 1)))
          (zhaopin-parse-url url :pn (incf pn)))
        drakma-response)))

(defun zhaopin-start-request(&key city_list)
  (dolist (city (or city_list zhaopin_city_list))
    (progn
      (format t "========= PROCESSING CITY: ~A =========~%" city)
      (zhaopin-parse-url (concatenate 'string zhaopin_base_url city)))))
