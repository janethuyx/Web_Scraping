#for product hutn
library(memoise)
library(shiny)
library(zoo)
library(xts)
library(dygraphs)
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(portfolio)
library(treemap)
library(wordcloud)
library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(stringr)
library(textreg)
library(googleVis)

producttags<-read.csv('producttags.csv')
productlistall<-read.csv('productlistafter.csv')
productreview<-read.csv('productreview.csv')
positive<-read.table('positivewords.txt')
negative<-read.table('negativewords.txt')
positive=as.character(positive$V1)
negative=as.character(negative$V1)
productlistall$targetcustomer<-NULL
productlistall$datetime=gsub('T',' ',productlistall$datetime)
productlistall$datetime=gsub('Z',' ',productlistall$datetime)
productlistall$datetime=as.Date(productlistall$datetime)
productlistall=productlistall[order(productlistall$datetime),]
productlistall=productlistall[c(-1,-2),]

othertag=c('Funny','Kids','Tech')
product_platform=c('Windows','Android','iPad','Linux','Mac','Website')
product_sub_type=c('Startup Books','Custom iPhone Keyboards','Standing Desks','Weather Apps','Indie Games',
                   'Spreadsheets','Cryptocurrencies','Design Tools','Design Books','3D Printing','CSM Tools','Biohacking',
                   'Developer Tools','Virtual Reality','Augmented Reality','Wearables','Drones','Robots','Text Editors',
                   'Email Marketing','Mac Menu Bar Apps','Artificial Intelligence','Fintech','Touch Bar Apps',
                   'Social Media Tools','Free Games','SEO Tools','Surfing','Wallpaper','Board Games','Action Games',
                   'Wi-Fi','History Books','Strategy games','Gear VR','Sci-fi Games','Dogs','Adult Coloring Books',
                   'Cats','SoundCloud','Basketball','PlayStation VR','Card Games','Isometric games','Phone Cases',
                   'Funny Games','Puzzle Games','Crafting games')
product_function=c('Internet of Things','Transportation','Cooking','Party','Public Relations','Parenting','Education',
                   'Nomad Lifestyle','Open Source','Fashion','Investing','Privacy','Freelance','Crowdfunding',
                   'Productivity','Home','Task Management','Politics','Backpacks','Marketing','Anonymous','Payment',
                   'Outdoors','Events','Meetings','Travel','User Experience','Growth Hacking','Prototyping',
                   'Calendar and Scheduling','Dating','Sales','Health and Fitness','Customer communication',
                   'Branding','Art','Venture Capital','Space','Software Engineering','Hiring and Recruiting','Advertising',
                   'Analytics','Legal','Virtual Assistants','Charity and Giving','Moving and Storage','On-Demand',
                   'A/B Testing','Delivery & Shipping','Learn a Language','Time Tracking')
product_type=c('Books','News','TV','Email','Music','Photoshop','Email Newsletters',
               'Sketch','Messaging','Writing Tools','Typography','APIs','GIFs','Maps','Emoji','Games','Hardware',
               'Movies','E-Commerce','Bots','Icons','Photography','Drinking','Video Streaming','Pets','Cars','Jewelry',
               'Sneakers and Shoes','Biking','Alarm Clocks','Skateboarding','Beauty','Coffee','Note','Sports',
               'Meditation')
Brand_related=c('Snapchat','Medium','Amazon','Instagram','Slack','Facebook','iPhone','Uber','SaaS','Product Hunt',
                'Facebook Messenger','Apple','Donald Trump','GitHub','Safari Extensions','Apple TV','Twitter','Spotify',
                'Apple Watch','Firefox Extensions','Google','Chrome Extensions','Wordpress','YouTube','HTC Vive',
                'Netflix','Oculus Rift','Telegram','Alexa Skills','iMessage Apps','Google Home','LinkedIn','Dropbox',
                'Kindle')
producttags$tagtype=ifelse(producttags$tag %in% product_function,'function','Not sure')
producttags$tagtype=ifelse(producttags$tag %in% Brand_related,'BrandRelated',producttags$tagtype)
producttags$tagtype=ifelse(producttags$tag %in% product_type,'Type',producttags$tagtype)
producttags$tagtype=ifelse(producttags$tag %in% product_sub_type,'SubType',producttags$tagtype)
producttags$tagtype=ifelse(producttags$tag %in% product_platform,'Platform', producttags$tagtype)
producttags$tagtype=ifelse(producttags$tag %in% othertag,'othertag',producttags$tagtype)

###################review###########
reviewall<- productreview %>%
  select(reviewcontent) %>%
  mutate(reviewcontent=as.character(reviewcontent))

reviewall=reviewall$reviewcontent

rvcorpus1<- Corpus(VectorSource(reviewall))

rv1clean<-tm_map(rvcorpus1,removePunctuation)
rv1clean<-tm_map(rv1clean,content_transformer(tolower))
rv1clean<-tm_map(rv1clean,removeWords,stopwords("english"))
rv1clean<-tm_map(rv1clean,removeNumbers)
rv1clean<-tm_map(rv1clean,stripWhitespace)
rv1clean<-tm_map(rv1clean,removeWords,c('and','the','our','that','for','are',
                                        'also','more','has','must','have','should',
                                        'this','with','many','hunt','url','yet','still',
                                        'without','can','will','get'))


textrv1=sapply(rv1clean, identity)
strrv1=str_split(textrv1,pattern='\\s+')
strrv1<-unlist(strrv1)
positive<-unlist(positive)
positive=positive[-1]
negative<-unlist(negative)
negative=negative[-1]

score_positive=sum(!is.na(match(strrv1,positive)))
score_negative=sum(!is.na(match(strrv1,negative)))

pstrv1=strrv1[!is.na(match(strrv1,positive))]
ngtv1=strrv1[!is.na(match(strrv1,negative))]


all=list(all1=reviewall,positive1=pstrv1,negative1=ngtv1)


reviewtypes <<- list("All" = 'all1',
               "Positive Review" = 'positive1',
               "Negative Review" = 'negative1')

timetypes<<-list('Product amount'='name',
                 'Platform active'='vote',
                 'User active'='totalreviewvote') 


getTermMatrix <- memoise(function(reviewtype) {

  if (!(reviewtype %in% reviewtypes))
    stop("Unknown reviewtype")
  
  text=all[reviewtype]
  text=unlist(text)

  myCorpus<- Corpus(VectorSource(text))
 
  myCorpus<-tm_map(myCorpus,removePunctuation)
  myCorpus<-tm_map(myCorpus,content_transformer(tolower))
  myCorpus<-tm_map(myCorpus,removeWords,stopwords("english"))
  myCorpus<-tm_map(myCorpus,removeNumbers)
  myCorpus<-tm_map(myCorpus,stripWhitespace)
  myCorpus<-tm_map(myCorpus,removeWords,c('and','the','our','that','for','are',
                                          'also','more','has','must','have','should',
                                          'this','with','many','hunt','url','yet','still',
                                          'without','can','will','get'))
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)

})


#################hypthosis#########
h1<- productlistall %>%
  select(name,makernum,vote) 
h1$makernum=as.factor(h1$makernum)


h2<- productlistall %>%
  select(description,vote) %>%
  mutate(description=as.character(description))
description=h2$description
dscorpus1<- Corpus(VectorSource(description))

ds1clean<-tm_map(dscorpus1,removePunctuation)
ds1clean<-tm_map(ds1clean,content_transformer(tolower))
ds1clean<-tm_map(ds1clean,removeWords,stopwords("english"))
ds1clean<-tm_map(ds1clean,removeNumbers)

ds1clean<-tm_map(ds1clean,stripWhitespace)
ds1clean<-tm_map(ds1clean,removeWords,c('and','the','our','that','for','are',
                                        'also','more','has','must','have','should',
                                        'this','with','many','hunt','url','yet','still',
                                        'without','can','will','get','by','between','a','one'))


ds1match=sapply(ds1clean, identity)
ds1match=str_split(ds1match,pattern='\\s+')
ds1match<-unlist(ds1match)


test1=str_split(h2$description,pattern='\\s+')[1]
test1<-unlist(test1)
test1=test1[!is.na(match(test1,ds1match))]
testdf1=data.frame(word=test1,vote=rep(h2$vote[1],length(test1)))
dsworddf=testdf1
i=2
while (i <= 1817){
  test=str_split(h2$description,pattern='\\s+')[i]
  test<-unlist(test)
  test=test[!is.na(match(test,ds1match))]
  testdf=data.frame(word=test,vote=rep(h2$vote[i],length(test)))
  dsworddf=rbind(testdf,dsworddf)
  i=i+1
}


h2a<- dsworddf %>%
  group_by(word) %>%
  summarise(averagevote=mean(vote))

h2a=h2a[order(h2a$averagevote,decreasing = T),]
h2a$ordernum=c(1:2119)

h2b$word=as.character(h2b$word)
dsworddf$word=as.character(dsworddf$word)
h2b<-merge(x=h2a,y=dsworddf,by='word',all = TRUE)

testh2b<-h2b
testh2b$ordernum=as.factor(testh2b$ordernum)


h2c<-h2a
h2c$word=as.character(h2c$word)
wordlis=rep(h2c$word[1],h2c$averagevote[1])
i=2
while (i<=2119){
  wordlis= append(rep(h2c$word[i],h2c$averagevote[i]),wordlis)
  i=i+1
}
wordchar<-unlist(wordlis)
dscorpus2<- Corpus(VectorSource(wordchar))