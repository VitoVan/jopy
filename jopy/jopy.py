from spider import zhaopin, neitui, lagou, woou
from threading import Thread
import sys

def spider_thread(spider):
    spider.start_request()

def main(city):
    zhaopin_thread = Thread(target = spider_thread, args = (zhaopin.Spider(city),))
    lagou_thread = Thread(target = spider_thread, args = (lagou.Spider(city),))
    neitui_thread = Thread(target = spider_thread, args = (neitui.Spider(city),))
    woou_thread = Thread(target = spider_thread, args = (woou.Spider(city),))

    zhaopin_thread.start()
    lagou_thread.start()
    neitui_thread.start()
    woou_thread.start()

    zhaopin_thread.join()
    lagou_thread.join()
    neitui_thread.join()
    woou_thread.join()

    print('All systems done.')


if __name__ == "__main__":
    main(sys.argv[1])
