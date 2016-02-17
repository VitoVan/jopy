from spider import zhaopin, neitui, lagou, woou
from threading import Thread

def spider_thread(spider):
    spider.start_request()

zhaopin_thread = Thread(target = spider_thread, args = (zhaopin.Spider('北京'),))
lagou_thread = Thread(target = spider_thread, args = (lagou.Spider('北京'),))
neitui_thread = Thread(target = spider_thread, args = (neitui.Spider('北京'),))
woou_thread = Thread(target = spider_thread, args = (woou.Spider('北京'),))

zhaopin_thread.start()
lagou_thread.start()
neitui_thread.start()
woou_thread.start()

zhaopin_thread.join()
lagou_thread.join()
neitui_thread.join()
woou_thread.join()

print('All systems done.')


