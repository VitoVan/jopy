import datetime, requests, json, psycopg2, psycopg2.extras, re, ast
from collections import defaultdict
from utils import *

class Woou:
    base_url = 'http://search.51job.com/jobsearch/search_result.php?funtype=0100,2400,2500,2600,2700&ord_field=1&jobarea='
    prefix = 'woou'

    def __init__(self, city):
        r = requests.get('http://js.51jobcdn.com/in/js/2009/jobarea_array_c.js')
        self.city_dict = {y:x for x,y in ast.literal_eval('{' + re.sub('\]=', ':', re.sub(';', ',', re.sub('var ja=\[\];|ja\[|\r\n', '', r.text))) + '}').items()}
        self.city_code = self.city_dict[city]
        self.city = city

    def start_request(self):
        url = self.base_url + self.city_code
        self.parse_list(url)

    def parse_list(self, url, pn = 1):
        try:
            cookies = dict(guide = '1')
            r = requests.get(url + '&curr_page=' + str(pn), timeout=5, cookies=cookies)
            r.encoding = 'gb2312'
        except requests.exceptions.Timeout:
            print('Read list timeout')
            return -1
        print('------------------- NOW PAGE: ' + str(pn))
        if self.parse_list_html(r.text) == 0 or pn == 1:
            self.parse_list(url, pn = pn + 1)

    def parse_list_html(self, list_html):
        for single_html in html_split(list_html,'#resultList > div.el:not(.mk)'):
            if self.parse_single_html(single_html) == -1:
                print('Out of date, stop.')
                return -1
        if len(html_split(list_html, 'li.bk > a[href]')) == 2:
            return 0
        else:
            print('No further page, stop.')
            return -2
        
    def parse_single_html(self, single_html):
        global fuck_html
        fuck_html = single_html
        info_dict = html_to_dict(single_html,
                                 [
                                     {
                                         'name': 'job_url',
                                         'selector': 'p.t1 > a[href^="http://jobs.51job.com/"]',
                                         'attribute': 'href'
                                     },
                                     {
                                         'name': 'job_id',
                                         'selector': 'p.t1 > input',
                                         'attribute': 'value'
                                     },
                                     {
                                         'name': 'create_date',
                                         'selector': 'span.t5'
                                     },
                                     {
                                         'name': 'position_name',
                                         'selector': 'p.t1 > a[href^="http://jobs.51job.com/"]'
                                     },
                                     {
                                         'name': 'company_name',
                                         'selector': 'span.t2 > a'
                                     },
                                     {
                                         'name': 'company_url',
                                         'selector': 'span.t2 > a',
                                         'attribute': 'href'
                                     }
                                 ])
        global fuck_dict
        fuck_dict = info_dict
        if info_dict['create_date'] != datetime.datetime.now().strftime("%m-%d"):
            return -1
        print(info_dict['position_name'] + ' - ' + info_dict['company_name'] + ' - ' + self.city + ' - ' + info_dict['create_date'])
        records = find_job(self.prefix + '_' + info_dict['job_id'])
        if len(records) > 0:
            cursor.execute('update job set update_date = %s, update_count = update_count + 1 where id = %s',
                           [datetime.datetime.now(), self.prefix + '_' + info_dict['job_id'],])
            print('J-*')
        elif info_dict['job_url'] != '':
            try:
                r = requests.get(info_dict['job_url'], timeout=5)
                r.encoding = 'gb2312'
            except requests.exceptions.Timeout:
                print('Read job timeout')
                return -1
            # find the full company name(instead of brand, such as: 滴滴打车 -> 北京小桔科技有限公司)
            company_name = self.parse_company(info_dict['company_url'])
            self.parse_job(info_dict['job_id'], company_name, info_dict['job_url'], r.text)

    def parse_job(self, job_id, company_name, job_url, job_html):
        info_dict = html_to_dict(job_html,
                     [
                         {
                             'name': 'title',
                             'selector': 'div.tCompany_center.clearfix > div.tHeader.tHjob > div > div.cn > h1'
                         },
                         {
                             'name': 'salary',
                             'selector': 'div.tCompany_center.clearfix > div.tHeader.tHjob > div > div.cn > strong'
                         },
                         {
                             'name': 'work_year',
                             'selector': 'div.tCompany_center.clearfix > div.tCompany_main > div.tBorderTop_box.bt > div > div > span',
                             'contains': '<em class="i1"></em>'
                         },
                         {
                             'name': 'address',
                             'selector': 'div.tCompany_center.clearfix > div.tCompany_main > div > div > p.fp',
                             'lambda': lambda rr,rd: re.sub('上班地址：','',rr).strip()
                         },
                         {
                             'name': 'description',
                             'selector': 'div.tCompany_center.clearfix > div.tCompany_main > div > div.job_msg',
                             'lambda': lambda rr,rd: re.sub('职位描述：|举报|分享','',rr).strip()
                         }
                     ])
        salary_range = defaultdict(lambda: 0, (enumerate(re.findall('\d+', info_dict['salary']))))
        work_year_range = defaultdict(lambda: 0, (enumerate(re.findall('\d+', info_dict['work_year']))))
        # get numbers
        salary_factor = 1
        if '年' in info_dict['salary']:
            salary_factor = 10000/12
        info_dict['min_salary'] = int(salary_range[0]) * salary_factor
        info_dict['max_salary'] = int(salary_range[1]) * salary_factor
        
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

    def parse_company(self, company_url):
        try:
            r = requests.get(company_url, timeout=5)
            r.encoding = 'gb2312'
        except requests.exceptions.Timeout:
            print('Read company timeout')
            return -1

        info_dict = html_to_dict(r.text,[
            {
                'name': 'short_name',
                'selector': 'div.tCompanyPage > div.tCompany_center.clearfix > div.tHeader.tHCop > div > h1'
            },
            {
                'name': 'description',
                'selector': 'div.tCompany_center.clearfix > div.tCompany_full > div.tBorderTop_box.bt > div > div.con_msg'
            },
            {
                'name': 'industry',
                'selector': 'div.tCompanyPage > div.tCompany_center.clearfix > div.tHeader.tHCop > div > p.ltype',
                'lambda': lambda rr,rd: re.sub('.*\|.*\|','', rr).strip()
            },
            {
                'name': 'address',
                'selector': 'div.tCompanyPage > div.tCompany_center.clearfix > div.tCompany_full > div > div > p.fp',
                'lambda': lambda rr,rd: re.sub('公司地址：','', rr).strip()
            },
            {
                'name': 'website',
                'selector': 'div.tCompany_center.clearfix > div.tCompany_full > div > div > p > a',
                'attribute': 'href'
            },
            {
                'name': 'name_string',
                'selector': 'div.tCompany_full > p.tPosition_center_bottomText'
            }])

        if info_dict['name_string']:
            info_dict['name'] = re.sub('\*发布本招聘广告企业的营业执照名称：','',info_dict['name_string']).strip()
        else:
            info_dict['name'] = info_dict['short_name']
        # check if exists
        records = find_company(info_dict['name'])
        if len(records) > 0:
            print('C-S')
        else:
            # fill extra data
            info_dict['raw_data'] = r.text
            info_dict['raw_url'] = company_url
            info_dict['update_date'] = datetime.datetime.now()
            del info_dict['name_string']
            # insert it
            insert_data('company', info_dict)
            print('Inserting Company')
        return info_dict['name']
