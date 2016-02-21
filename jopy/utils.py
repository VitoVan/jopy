import psycopg2, psycopg2.extras, requests, os, time
from bs4 import BeautifulSoup
from collections import defaultdict

conn = psycopg2.connect("dbname=whereisjob user=whereisjob password=123456")
conn.autocommit = True
AMAP_KEY = os.environ['AMAP_KEY']
AMAP_JOB_TABLEID = os.environ['AMAP_JOB_TABLEID']
AMAP_COMPANY_TABLEID = os.environ['AMAP_COMPANY_TABLEID']

def get_gps(city, address):
    print('getting location: ' + city + ' - ' + address + '\n')
    r = requests.get('http://restapi.amap.com/v3/geocode/geo?key='+ AMAP_KEY +'&address=' + address + '&city=' + city)
    result = r.json()
    try:
        return result['geocodes'][0]['location']
    except:
        return '-1'

def insert_map_data(data):
    if data['location'] == '-1' or data['location'] == ',':
        return -1
    final_data = {
        '_name' : data['title'] if 'title' in data else data['name'],
        '_location' : data['location'],
        '_address' : data['address'],
        'coordtype' : 'autonavi',
        'timestamp': int(time.time())
    }
    del data['raw_data']
    del data['location']
    del data['address']
    del data['update_date']
    final_data.update(data)
    payload = {
        'key' : AMAP_KEY,
        'tableid': AMAP_JOB_TABLEID if 'title' in data else AMAP_COMPANY_TABLEID,
        'loctype': '1',
        'data': str(final_data)
    }
    r = requests.post('http://yuntuapi.amap.com/datamanage/data/create', data = payload)
    if r.json()['status'] == 0:
        print('POST RESULT: ' + r.text + '\n')
        print(payload)

def update_map_job(job_id):
    r = requests.get('http://yuntuapi.amap.com/datamanage/data/list?tableid='
                     + AMAP_JOB_TABLEID + '&filter=id:' + job_id + '&limit=1&page=1&key=' + AMAP_KEY)
    job_json = r.json()
    try:
        job_map_id = job_json['datas'][0]['_id']
    except:
        print(r.text)
        return -1
    data = {
        "_id": job_map_id,
        "timestamp": int(time.time())
    }
    payload = {
        'key' : AMAP_KEY,
        'tableid': AMAP_JOB_TABLEID,
        'data': str(data).replace('\'','\"')
    }
    r = requests.post('http://yuntuapi.amap.com/datamanage/data/update', data = payload)
    if r.json()['status'] == 0:
        print('POST RESULT:' + r.text)
        print(payload)
        
    
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
    update_map_job(job_id[1])

def insert_data(table, data):
    sql_str = 'insert into ' + table + ' (' + \
              ','.join(data.keys()) + ') values (' + \
              ','.join('%s' for x in data.keys()) + ')'
    cursor = conn.cursor(cursor_factory=psycopg2.extras.DictCursor)
    cursor.execute(sql_str, [x for x in data.values()])
    insert_map_data(data)
    
def select_to_text(soup, selector, attribute=None, contains=None):
    if not selector:
        return ''
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
