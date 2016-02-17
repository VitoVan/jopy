import psycopg2, psycopg2.extras
from bs4 import BeautifulSoup
from collections import defaultdict

conn = psycopg2.connect("dbname=whereisjob user=whereisjob password=123456")
conn.autocommit = True

def find_job(job_id):
    cursor = conn.cursor(cursor_factory=psycopg2.extras.DictCursor)
    cursor.execute('select  * from job where id = %s', (job_id,))
    return cursor.fetchall()

def find_company(company_name):
    cursor = conn.cursor(cursor_factory=psycopg2.extras.DictCursor)
    cursor.execute('select  * from company where name = %s or short_name = %s', (company_name, company_name))
    return cursor.fetchall()

def update_job(job_id):
    cursor = conn.cursor(cursor_factory=psycopg2.extras.DictCursor)
    cursor.execute('update job set update_date = %s, update_count = update_count + 1 where id = %s', job_id)

def insert_data(table, data):
    sql_str = 'insert into ' + table + ' (' + \
              ','.join(data.keys()) + ') values (' + \
              ','.join('%s' for x in data.keys()) + ')'
    cursor = conn.cursor(cursor_factory=psycopg2.extras.DictCursor)
    cursor.execute(sql_str, [x for x in data.values()])
    
def select_to_text(soup, selector, attribute=None, contains=None):
    results = soup.select(selector)
    if len(results) > 0:
        if contains: 
            final_result = None
            for result in results:
                if contains in str(result):
                    final_result = result
                    break
            if final_result:
                if attribute:
                    return final_result[attribute]
                else:
                    return final_result.get_text()
            else:
                return ''
        else:
            result = results[0]
            if attribute:
                return result[attribute]
            else:
                return result.get_text()
    else:
        return ''


def html_split(html, selector):
    soup = BeautifulSoup(html)
    elements = soup.select(selector)
    return list(map(str, elements))
    
def html_to_dict(html,selector_dict):
    result_dict = {}
    soup = BeautifulSoup(html)
    for selector in selector_dict:
        selector = defaultdict(lambda: None, selector)
        name = selector['name']
        selector_str = selector['selector']
        attribute = selector['attribute']
        contains = selector['contains']
        func = selector['lambda']
        raw_result = select_to_text(soup, selector_str, attribute, contains).strip()
        if func:
            result_dict[name] = func(raw_result, result_dict)
        else:
            result_dict[name] = raw_result
    return result_dict
