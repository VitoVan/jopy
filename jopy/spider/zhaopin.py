import datetime, requests, json, psycopg2, psycopg2.extras, re
from collections import defaultdict
from utils import *

class Spider:
    base_url = 'http://sou.zhaopin.com/jobs/searchresult.ashx?bj=160000&sm=1&jl='
    job_url = 'http://jobs.zhaopin.com/#ID#.htm'
    prefix = 'zhaopin'
    skipped = False

    def __init__(self, city, max_page = 10):
        self.city = city
        self.max_page = max_page

    def start_request(self):
        url = self.base_url + self.city
        self.parse_list(url)

    def parse_list(self, url, pn = 1):
        if pn > self.max_page and self.skipped:
            print('Out of pn: ' + str(pn) + ', exit')
            return -2
        try:
            r = requests.get(url + '&p=' + str(pn), timeout=5)
        except requests.exceptions.Timeout:
            print('Read list timeout')
            return -1
        print(self.prefix + '------------------- NOW PAGE: ' + str(pn))
        if self.parse_list_html(r.text) == 0:
            self.parse_list(url, pn = pn + 1)

    def parse_list_html(self, list_html):
        for single_html in html_split(list_html,'#newlist_list_content_table > div'):
            if self.parse_single_html(single_html) == -1:
                print('Out of date, stop.')
                return -1
        return 0
        
    def parse_single_html(self, single_html):
        info_dict = html_to_dict(single_html,
                                 [
                                     {
                                         'name': 'job_url',
                                         'selector': 'a[href^="http://jobs.zhaopin.com"]',
                                         'attribute': 'href'
                                     },
                                     {
                                         'name': 'create_date',
                                         'selector': 'div > dl > p'
                                     },
                                     {
                                         'name': 'position_name',
                                         'selector': 'a[href^="http://jobs.zhaopin.com/"]'
                                     },
                                     {
                                         'name': 'company_name',
                                         'selector': 'div > ul > li.newlist_deatil_three.gsmc > a'
                                     }
                                 ])
        if info_dict['create_date'] != datetime.datetime.now().strftime("%m-%d"):
            return -1
        job_id = re.sub('http://jobs.zhaopin.com/|.htm|.*_','',info_dict['job_url'])
        print(self.prefix + ' - // ' + info_dict['position_name'] + ' - ' + info_dict['company_name'] + ' - ' + self.city + ' - ' + info_dict['create_date'])
        records = find_job(self.prefix + '_' + job_id)
        if len(records) > 0:
            update_job([datetime.datetime.now(), self.prefix + '_' + job_id,])
            self.skipped = True
            print('J-*')
        elif info_dict['job_url'] != '':
            try:
                r = requests.get(info_dict['job_url'], timeout=5)
            except requests.exceptions.Timeout:
                print('Read job timeout')
                return -1
            # find the full company name(instead of brand, such as: 滴滴打车 -> 北京小桔科技有限公司)
            company_name = self.parse_company(info_dict['job_url'], r.text)
            self.parse_job(job_id, company_name, info_dict['job_url'], r.text)

    def parse_job(self, job_id, company_name, job_url, job_html):
        info_dict = html_to_dict(job_html,
                     [
                         {
                             'name': 'title',
                             'selector': 'div.fixed-inner-box > div.inner-left.fl > h1'
                         },
                         {
                             'name': 'salary',
                             'selector': 'div.terminalpage.clearfix > div.terminalpage-left > ul > li:nth-of-type(1) > strong'
                         },
                         {
                             'name': 'work_year',
                             'selector': 'div.terminalpage.clearfix > div.terminalpage-left > ul > li:nth-of-type(5) > strong'
                         },
                         {
                             'name': 'address',
                             'selector': 'div.terminalpage-left > div.terminalpage-main.clearfix > div > div:nth-of-type(1) > h2',
                             'lambda': lambda rr,rd: re.sub('查看职位地图','',rr).strip()
                         },
                         {
                             'name': 'location',
                             'lambda': lambda rr,rd: get_gps(self.city, rd['address'])
                         },
                         {
                             'name': 'description',
                             'selector': 'div.terminalpage.clearfix > div.terminalpage-left > div.terminalpage-main.clearfix > div > div:nth-of-type(1)',
                             'lambda': lambda rr,rd: re.sub('工作地址(.|\s)*','',rr).strip()
                         }
                     ])
        salary_range = defaultdict(lambda: 0, (enumerate(re.findall('\d+', info_dict['salary']))))
        work_year_range = defaultdict(lambda: 0, (enumerate(re.findall('\d+', info_dict['work_year']))))
        # get numbers
        info_dict['min_salary'] = int(salary_range[0])
        info_dict['max_salary'] = int(salary_range[1])
        info_dict['min_work_year'] = int(work_year_range[0])
        info_dict['max_work_year'] = int(work_year_range[1])
        # del raw data
        del info_dict['salary']
        del info_dict['work_year']
        # fill extra data
        info_dict['id'] = self.prefix + '_' + job_id
        info_dict['city'] = self.city
        info_dict['company'] = company_name
        info_dict['raw_data'] = job_html
        info_dict['raw_url'] = job_url
        info_dict['update_date'] = datetime.datetime.now()
        # insert it
        insert_data('job', info_dict)
        print('Inserting Job')
        
    def parse_company(self, job_url, job_html):
        info_dict = html_to_dict(job_html,
                     [
                         {
                             'name': 'short_name',
                             'selector': 'body > div.terminalpage.clearfix > div.terminalpage-right > div.company-box > p > a[rel="nofollow"]'
                         },
                         {
                             'name': 'name',
                             'selector': 'div.terminalpage-left > div.terminalpage-main.clearfix > div > div > div.announcement-icon.color-red',
                             'lambda': lambda rr,rd: re.sub('※发布本招聘广告企业的营业执照名称：','',rr).strip() if len(rr)>0 else rd['short_name']
                         },
                         {
                             'name': 'description',
                             'selector': 'div.terminalpage.clearfix > div.terminalpage-left > div.terminalpage-main.clearfix > div > div:nth-of-type(2)',
                             'lambda': lambda rr,rd: re.sub('.*该公司其他职位|※发布本招聘广告企业的营业执照名称.*','',rr).strip()
                         },
                         {
                             'name': 'industry',
                             'selector': 'div.terminalpage.clearfix > div.terminalpage-right > div.company-box > ul > li:nth-of-type(3) > strong > a'
                         },
                         {
                             'name': 'address',
                             'selector': 'div.terminalpage.clearfix > div.terminalpage-right > div.company-box > ul > li',
                             'contains': '公司地址',
                             'lambda': lambda rr,rd: re.sub('公司地址：','',rr).strip()
                         },
                         {
                             'name': 'location',
                             'lambda': lambda rr,rd: get_gps(self.city, rd['address'])
                         },
                         {
                             'name': 'website',
                             'selector': 'div.terminalpage.clearfix > div.terminalpage-right > div.company-box > ul > li:nth-of-type(4) > strong > a'
                         }
                     ])
        # check if exists
        records = find_company(info_dict['name'])
        if len(records) > 0:
            print('C-S')
        else:
            # fill extra data
            info_dict['raw_data'] = 'as job page'
            info_dict['raw_url'] = job_url
            info_dict['update_date'] = datetime.datetime.now()
            # insert it
            insert_data('company', info_dict)
            print('Inserting Company')
        return info_dict['name']
