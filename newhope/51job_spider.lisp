;;;; 51job_spider.lisp

(in-package #:newhope)

;; use Python's requests instead of drakma for GBK pages
(defun 51job-http-request(url)
  (progn
    (burgled-batteries:run "cookies = dict(guide = '1')")
    (burgled-batteries:run (concatenate 'string "r = requests.get('" url "', cookies=cookies)"))
    (burgled-batteries:run "r.encoding = 'GB2312'")
    (burgled-batteries:run "r.text")))

(defvar 51job_base_url "http://search.51job.com/jobsearch/search_result.php?funtype=0100,2400,2500,2600,2700&ord_field=1&jobarea=")
;;北京 郑州
(defvar 51job_city_list '("010000" "170200"))
(defvar 51job_year (write-to-string (nth 5 (multiple-value-list (get-decoded-time)))))
(defvar 51job_today (multiple-value-bind
      (second minute hour date month year day-of-week dst-p tz)
    (get-decoded-time)
  (declare (ignorable second minute hour year day-of-week dst-p tz))
  (format nil "~2,'0d-~2,'0d"
          month
          date)))
(defvar 51job_full_today (concatenate 'string 51job_year "-" 51job_today))

(defun 51job-parse-job(job-page &key id company url)
  "Parse HTML of job page, for table job, insert directly"
  (let* ((title (css->text "div.tCompany_main_title > ul.tCompany_main_l > li > h1" job-page))
         (salaries (or (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "[0-9]+" (css->text "div.tCompany_basic > div > dl:nth-child(2) > dd > span.f-col-red" job-page :contains "月"))) '(0 0)))
         (min_salary (car salaries))
         (max_salary (or (cadr salaries) 0))
         (work_years (or (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "[0-9]+" (css->text "div.tCompany_introduction > div.tCompany_basic > div > dl:nth-child(2) > dd:nth-child(2)" job-page))) '(0 0)))
         (min_work_year (car work_years))
         (max_work_year (or (cadr work_years) 0))
         (city
          (cl-ppcre:regex-replace-all
           "-.*"
           (css->text "div.tCompany_introduction > div.tCompany_basic > div > dl:nth-child(1) > dd:nth-child(4)" job-page) ""))
         (address (trim (cl-ppcre:regex-replace "上班地址：" (css->text "div.tCompany_introduction > div.tCompany_basic > div > dl.lineDl" job-page :contains "上班地址") "")))
         (description (css->text "div.tCompany_introduction > div.tCompany_text > ul > p" job-page))
         (raw_data job-page))
    (execute "INSERT INTO job VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)"
             id title min_salary max_salary min_work_year
             max_work_year city address description company raw_data url 0 51job_full_today)))

(defun 51job-parse-company(job-page)
  "Parse HTML of job page, for table company, insert directly"
  (let* ((short_name (css->text "div.tCompany_sidebar > div.tBorderTop_box.job_page_company > h2" job-page))
         (company-url (css->text "div.tCompany_sidebar > div.tBorderTop_box.job_page_company > div > ul > li > a" job-page
                                 :contains "查看公司简介"
                                 :attribute "href"))
         (name
          (if (cl-ppcre:scan "公司$|集团$" short_name)
              short_name
              (let* ((company-page (51job-http-request company-url))
                     (company-name-string (css->text "div.tCompanyPage > div.tCompany_center.clearfix > div.tCompany_main.f14 > div.tCompany_center_bottomText" company-page))
                     (company-name (cl-ppcre:regex-replace "※ 发布本招聘广告企业的营业执照名称：" company-name-string "")))
                (if (> (length company-name) 0) company-name short_name))))
         (slogan "")
         (description (css->text "div.tCompanyPage > div.tCompany_center.clearfix > div.tCompany_main.f14 > div:nth-child(4) > div > p" job-page))
         (industry (css->text "div.tCompanyPage > div.tCompany_center.clearfix > div.tCompany_sidebar > div.tBorderTop_box.job_page_company > dl:nth-child(3) > dd.text_dd174" job-page))
         (address (css->text "div.tCompany_sidebar > div.tBorderTop_box.job_page_company > dl > dd > p.job_company_text" job-page))
         (website (css->text "div.tCompany_sidebar > div.tBorderTop_box.job_page_company > dl > dd > p.job_company_text > a" job-page))
         (raw_data job-page)
         (raw_url company-url))
    (if (< (length (query "select * from company where name = $1" name)) 1)
        (execute "INSERT INTO company VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)"
                 name short_name slogan description industry address website raw_data raw_url 51job_full_today)
        (format t "C.. "))
    name))

(defun 51job-parse-html(html-response)
  "Parse and Store the HTML data, return -1 if some of the data is out of date (not today)"
  (let* ((job-list (css->htmls "#resultList>div[class='el']" html-response))
         (next-page (css->htmls "li.bk:last-of-type>a[href]" html-response)))
    (progn
      (dolist (job job-list)
        (let* ((create-date (css->text "span.t5" job))
               (job-id (css->text "p.t1>input" job :attribute "value"))
               (id (concatenate 'string "51job_" job-id))
               (position-name (css->text "p.t1>a[href^='http://jobs.51job.com/']" job))
               (company-name (css->text "span.t2>a" job))
               (city (cl-ppcre:regex-replace-all "-.*" (css->text "span.t3" job) ""))
               (job-url (css->text "p.t1>a" job :attribute "href")))
          (if (equal 51job_today create-date)
              (progn
                (format t "~A , ~A , ~A , ~A ~%"
                        position-name company-name city
                        create-date)
                (if (> (length (query "select * from job where id = $1" id)) 0)
                    ;; If data exists, then update_*
                    (progn
                      (format t "S.. ")
                      (execute "update job set update_date = $1, update_count = update_count + 1 where id = $2" 51job_full_today id))
                    ;;If not exists, scrape the HTML of job page
                    ;;Skip special like: http://cofco.51job.com/sc/show_job_detail.php?jobid=72524839
                    (if (cl-ppcre:scan "jobs.51job.com" job-url)
                        (let* ((job-page (51job-http-request job-url)))
                          (51job-parse-job job-page
                                           :id id
                                           :company (51job-parse-company job-page)
                                           :url job-url)))))
              (progn
                (format t "~% OUT-OF-DATE ~% POSITION: ~A , DATE: ~A ~% ~%" position-name create-date)
                (return-from 51job-parse-html  -1)))))
      ;;check if there is next page, return -1 if not
      (if (< (length next-page) 1)
          (progn
            (format t "~% NO FURTHER PAGE ~% ")
            -1)))))

(defun 51job-parse-url(url &key (pn 1) (recursive t))
  (let* ((page-url (concatenate 'string url "&curr_page=" (write-to-string pn)))
         (drakma-response (51job-http-request page-url)))
    (if (and recursive (not (equal (51job-parse-html drakma-response) -1)))
        (progn
          (format t "========== NEW PAGE ========== ~A ~%" (write-to-string (+ pn 1)))
          (51job-parse-url url :pn (incf pn)))
        (format t "~A ~%" page-url))))

(defun 51job-start-request(&key city_list)
  (dolist (city (or city_list 51job_city_list))
    (progn
      (format t "========= PROCESSING CITY: ~A =========~%" city)
      (51job-parse-url (concatenate 'string 51job_base_url city)))))
