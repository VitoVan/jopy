import datetime, requests, json, psycopg2, psycopg2.extras, re
from bs4 import BeautifulSoup
from collections import defaultdict
from utils import insert_data, html_to_dict, find_job, find_company

class Lagou:
    base_url = 'http://www.lagou.com/jobs/positionAjax.json?px=new&city='
    job_url = 'http://www.lagou.com/jobs/#ID#.html'
    company_url = 'http://www.lagou.com/gongsi/#ID#.html'
    prefix = 'lagou'

    def __init__(self, city):
        self.city = city

    def start_request(self):
        url = self.base_url + self.city
        self.parse_list(url)

    def parse_list(self, url, pn = 1):
        params = {'first': 'false', 'pn': str(pn), 'kd':''}
        try:
            r = requests.post(url, data=params, timeout=5)
        except requests.exceptions.Timeout:
            print('Read list timeout')
            return -1
        print('------------------- NOW PAGE: ' + str(pn))
        if self.parse_list_json(r.json()) == 0:
            self.parse_list(url, pn = pn + 1)

    def parse_list_json(self, list_json):
        for single_json in list_json['content']['result']:
            #print('This is JSON: \n' + json.dumps(single_json, ensure_ascii=False))
            if self.parse_single_json(single_json) == -1:
                print('Out of date, stop.')
                return -1
        return 0
        
    def parse_single_json(self, single_json):
        create_date = single_json['createTime'][:10]
        if create_date != datetime.datetime.now().strftime("%Y-%m-%d"):
            return -1
        position_name = single_json['positionName']
        company_name = single_json['companyShortName']
        company_industry = single_json['industryField']
        company_id = str(single_json['companyId'])
        job_id = str(single_json['positionId'])
        print(position_name + ' - ' + company_name + ' - ' + self.city + ' - ' + create_date)
        self.parse_job(job_id, company_name)
        self.parse_company(company_name, company_id, company_industry)

    def parse_job(self, job_id, company_name):
        # check if exists
        records = find_job(self.prefix + '_' + job_id)
        if len(records) > 0:
            cursor.execute('update job set update_date = %s, update_count = update_count + 1 where id = %s',
                           [datetime.datetime.now(), self.prefix + '_' + job_id,])
            print('J-S')
        else:
            r_url = self.job_url.replace('#ID#',job_id)
            try:
                r = requests.get(r_url, timeout=5)
            except requests.exceptions.Timeout:
                print('Read job timeout')
                return -1
            info_dict = html_to_dict(r.text,
                         [
                             {
                                 'name': 'title',
                                 'selector': '#container > div.content_l.fl > dl.job_detail > dt > h1',
                                 'attribute': 'title'
                             },
                             {
                                 'name': 'salary',
                                 'selector': '#container > div.content_l.fl > dl.job_detail > dd.job_request > p > span.red'
                             },
                             {
                                 'name': 'work_year',
                                 'selector': '#container > div.content_l.fl > dl.job_detail > dd.job_request > p > span',
                                 'contains': '经验'
                             },
                             {
                                 'name': 'address',
                                 'selector': '#container > div.content_r > dl > dd > div:nth-of-type(1)'
                             },
                             {
                                 'name': 'description',
                                 'selector': '#container > div.content_l.fl > dl.job_detail > dd.job_bt'
                             }
                         ])
            salary_range = defaultdict(lambda: 0, (enumerate(re.findall('\d+', info_dict['salary']))))
            work_year_range = defaultdict(lambda: 0, (enumerate(re.findall('\d+', info_dict['work_year']))))
            # get numbers
            info_dict['min_salary'] = salary_range[0]
            info_dict['max_salary'] = salary_range[1]
            info_dict['min_work_year'] = work_year_range[0]
            info_dict['max_work_year'] = work_year_range[1]
            # del raw data
            del info_dict['salary']
            del info_dict['work_year']
            # fill extra data
            info_dict['id'] = self.prefix + '_' + job_id
            info_dict['city'] = self.city
            info_dict['company'] = company_name
            info_dict['raw_data'] = r.text
            info_dict['raw_url'] = r_url
            info_dict['update_date'] = datetime.datetime.now()
            # insert it
            insert_data('job', info_dict)
            print('Inserting Job')
        
    def parse_company(self, company_name, company_id, company_industry):
        # check if exists
        records = find_company(company_name)
        if len(records) > 0:
            print('C-S')
        else:
            r_url = self.company_url.replace('#ID#',company_id)
            try:
                r = requests.get(r_url, timeout=5)
            except requests.exceptions.Timeout:
                print('Read company timeout')
                return -1
            info_dict = html_to_dict(r.text,
                         [
                             {
                                 'name': 'short_name',
                                 'selector': 'body > div.top_info > div > div > div.company_main > h1 > a'
                             },
                             {
                                 'name': 'slogan',
                                 'selector': 'body > div.top_info > div > div > div.company_main > div.company_word'
                             },
                             {
                                 'name': 'description',
                                 'selector': '#company_intro > div.item_content > div.company_intro_text > span.company_content'
                             },
                             {
                                 'name': 'address',
                                 'selector': 'ul > li > p.mlist_li_desc'
                             },
                             {
                                 'name': 'website',
                                 'selector': 'body > div.top_info > div > div > div.company_main > a.icon-wrap',
                                 'attribute': 'href'
                             }
                         ])
            # fill extra data
            info_dict['name'] = company_name
            info_dict['industry'] = company_industry
            info_dict['raw_data'] = r.text
            info_dict['raw_url'] = r_url
            info_dict['update_date'] = datetime.datetime.now()
            # insert it
            insert_data('company', info_dict)
            print('Inserting Company')
