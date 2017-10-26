#for my project 


from janetproject.items import JanetprojectItem
from scrapy import Spider, Request


import pandas
urls=pandas.read_csv('website.csv',header=None)
urls=urls[0]
urls=urls.tolist()
urls=urls[1:]

def reformat_url(url):
	url=url[27:]
	return url



class JanetSpider(Spider):
	name='janet_spider'
	allowed_urls=['https://www.producthunt.com/']
	start_urls=urls

	def start_requests(self):
		for url in self.start_urls:
			new_url=reformat_url(url)
			yield self.make_requests_from_url(new_url)


#  links=response.xpath('//a[@class="link_523b9"]/@href').extract()
#  totalreviews=response.xpath('//div[@class="item_54fdd"]')
#  for index, link in enumerate(links):
#   url='https://www.producthunt.com'+link
#   totalreview=totalreviews[index].xpath('.//div[@class="buttonContainer_b6eb3"]/text()[2]').extract()[1]
#    yield Request(url,callback=self.parse_detail, meta={'totalreview':totalreview})

	def parse(self,response):
		tags=response.xpath('//span[@class="topic_ca358 button_53e93 secondaryText_97b90"]/a/text()').extract()
		name=response.xpath('//h1[@class="headline_7b490 default_029ca base_e2db5"]/text()').extract_first()
		description=response.xpath('//h2[@class="tagline_5158b text_47b37 subtle_8ea23 base_e2db5"]/text()').extract_first()
		vote=response.xpath('//span[@class="count_1c8d9"]/text()').extract_first()
		huntername=response.xpath('//div[@class="hunter_ca3c8"]/div[1]/a[2]/text()').extract_first()
		makersname='/'.join(response.xpath('//div[@class="makers_25a84"]//a[@class="userName_e6768 text_47b37 default_029ca base_e2db5"]/text()').extract())
		makernum=len(response.xpath('//div[@class="makers_25a84"]//a[@class="userName_e6768 text_47b37 default_029ca base_e2db5"]/text()').extract())
		time=response.xpath('//div[@class="timestamp_86c05"]//time/text()').extract_first()
		reviewpeople='/'.join(response.xpath('//span[@class="text_47b37 subtle_8ea23 base_e2db5"]/span/span/text()[3]').extract())
		mainreviewcontent='//'.join(response.xpath('//div[@class="comment_4b2f3"]/span/text()').extract())
		subreviewcontent='//'.join(response.xpath('//div[@class="childComment_32bd0 comment_4b2f3"]/span/text()').extract())
		reviewcontent=mainreviewcontent+subreviewcontent
		reviewvotelist=response.xpath('//span[@class="actionButton_7c814"]/a/text()').extract()
		relatedproducts=response.xpath('//div[@class="content_ae6a1"]/h2/text()').extract()
		datetime=response.xpath('//a[@class="timestamp_85f53"]/time/@datetime').extract_first()
		a=len(tags)
		if a==0:
			tag1=tag2=tag3=''
		elif a==1:
			tag1=tags[0]
			tag2=tag3=''
		elif a==2:
			tag1=tags[0]
			tag2=tags[1]
			tag3=''
		elif a==3:
			tag1=tags[0]
			tag2=tags[1]
			tag3=tags[2]
		else:
			tag1='//'.join(tags)
			tag2=tag3='wrong'

		total=0
		time=len(reviewvotelist)

		for num in reviewvotelist:
			if num=='Upvote':
				num=0
			else:
				num=int(num)
			total=total+num

		avgreviewvote=total/time
		totalreviewvote=total


		for product in relatedproducts:
			relatedproduct=product


			item=JanetprojectItem()
			item['tag1']=tag1
			item['tag2']=tag2
			item['tag3']=tag3
			item['name']=name
			item['description']=description
			item['vote']=vote
			item['huntername']=huntername
			item['time']=time
			item['makersname']=makersname
			item['reviewpeople']=reviewpeople
			item['reviewcontent']=reviewcontent
			item['avgreviewvote']=avgreviewvote
			item['totalreviewvote']=totalreviewvote
			item['relatedproduct']=relatedproduct
			item['datetime']=datetime
			item['makernum']=makernum

			yield item 


