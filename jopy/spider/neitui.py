import datetime, requests, json, psycopg2, psycopg2.extras, re
from collections import defaultdict
from utils import *

class Neitui:
    base_url = 'http://www.neitui.me/?name=neitui&handle=lists&kcity='
    prefix = 'neitui'

    def __init__(self, city):
        self.city = city

    def start_request(self):
        url = self.base_url + self.city
        self.parse_list(url)

    def parse_list(self, url, pn = 1):
        if pn > 30:
            print('Out of pn: 30, exit')
            return -2
        try:
            r = requests.get(url + '&page=' + str(pn), timeout=5)
        except requests.exceptions.Timeout:
            print('Read list timeout')
            return -1
        print('------------------- NOW PAGE: ' + str(pn))
        if self.parse_list_html(r.text) == 0:
            self.parse_list(url, pn = pn + 1)

    def parse_list_html(self, list_html):
        for single_html in html_split(list_html,'.jobinfo'):
            if self.parse_single_html(single_html) == -1:
                print('Out of date, stop.')
                return -1
        return 0
        
    def parse_single_html(self, single_html):
        info_dict = html_to_dict(single_html,
                                 [
                                     {
                                         'name': 'job_url',
                                         'selector': 'div.jobnote > div.jobnote-l > a[href^="/j"]',
                                         'attribute': 'href',
                                         'lambda': lambda rr,rd: 'http://www.neitui.me' + rr
                                     },
                                     {
                                         'name': 'company_url',
                                         'selector': 'div.jobnote > div.jobnote-r > a[href^="/c"]',
                                         'attribute': 'href',
                                         'lambda': lambda rr,rd: 'http://www.neitui.me' + rr
                                     },
                                     {
                                         'name': 'job_note',
                                         'selector': 'div.jobnote',
                                         'contains': '发布'
                                     },
                                     {
                                         'name': 'position_name',
                                         'selector': 'div.jobnote > div.jobnote-l > a'
                                     },
                                     {
                                         'name': 'company_name',
                                         'selector': 'div.jobnote-r > a'
                                     }
                                 ])
        info_dict['create_date'] = re.findall('[0-9]{2}月[0-9]{2}日',info_dict['job_note'])[0]
        if info_dict['create_date'] != datetime.datetime.now().strftime("%m月%d日"):
            return -1
        job_id = re.findall('[0-9]+',info_dict['job_url'])[0]
        print(info_dict['position_name'] + ' - ' + info_dict['company_name'] + ' - ' + self.city + ' - ' + info_dict['create_date'])
        records = find_job(self.prefix + '_' + job_id)
        if len(records) > 0:
            cursor.execute('update job set update_date = %s, update_count = update_count + 1 where id = %s',
                           [datetime.datetime.now(), self.prefix + '_' + job_id,])
            print('J-*')
        elif info_dict['job_url'] != '':
            try:
                r = requests.get(info_dict['job_url'], timeout=5)
            except requests.exceptions.Timeout:
                print('Read job timeout')
                return -1
            # find the full company name(instead of brand, such as: 滴滴打车 -> 北京小桔科技有限公司)
            company_name = self.parse_company(info_dict['company_url'], info_dict['company_name'])
            self.parse_job(job_id, company_name, info_dict['job_url'], r.text)

    def parse_company(self, company_url, company_name):
        # check if exists
        records = find_company(company_name)
        if len(records) > 0:
            print('C-S')
            return records[0]['name']
        else:
            try:
                r = requests.get(company_url, timeout=5)
            except requests.exceptions.Timeout:
                print('Read company timeout')
                return -1
            info_dict = html_to_dict(r.text,[
                {
                    'name': 'slogan',
                    'selector': '#J_Company_Default > div.company_vision > div > span:nth-of-type(2)'
                },
                {
                    'name': 'description',
                    'selector': '#J_Company_Textarea'
                },
                {
                    'name': 'industry',
                    'selector': '#J_Company_Default > div.company_description.clearfix > div.company_description_l > div:nth-of-type(2) > span:nth-of-type(2)'
                },
                {
                    'name': 'address',
                    'selector': '#J_Company_Default > div.company_description.clearfix > div.company_description_l > div:nth-of-type(1) > span:nth-of-type(2)'
                },
                {
                    'name': 'website',
                    'selector': '#J_Company_Default > h3 > span > em > a'
                }])
            info_dict['name'] = company_name
            info_dict['short_name'] = company_name
            # fill extra data
            info_dict['raw_data'] = r.text
            info_dict['raw_url'] = company_url
            info_dict['update_date'] = datetime.datetime.now()
            # insert it
            insert_data('company', info_dict)
            print('Inserting Company')
            return info_dict['name']
            
    def parse_job(self, job_id, company_name, job_url, job_html):
        info_dict = html_to_dict(job_html,
                     [
                         {
                             'name': 'title',
                             'selector': 'div.jobnote> strong.padding-r10'
                         },
                         {
                             'name': 'salary',
                             'selector': 'div.jobnote>span.pay'
                         },
                         {
                             'name': 'work_year',
                             'selector': 'div.jobnote>span.experience'
                         },
                         {
                             'name': 'address',
                             'selector': 'div.jobtitle>span.jobtitle-r',
                             'lambda': lambda rr,rd: re.sub('地点：','',rr).strip()
                         },
                         {
                             'name': 'description',
                             'selector': 'div.jobdetail'
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
        
