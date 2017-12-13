#################### global.R ######################
####################################################

library(shiny)

library(dplyr,warn.conflicts = FALSE)

library(dygraphs)

library(ggplot2)

library(RColorBrewer)

library(wordcloud)
library(NLP,warn.conflicts = FALSE)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(stringr)
library(textreg)

library(plotly,warn.conflicts = FALSE)
library(zoo,warn.conflicts = FALSE)
library(xts,warn.conflicts = FALSE)

library(memoise)





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
anum=dim(h2a)[1]
h2a$ordernum=c(1:anum)
h2b<-merge(x=h2a,y=dsworddf,by='word',all = TRUE)
h2b$word=as.character(h2b$word)
dsworddf$word=as.character(dsworddf$word)


testh2b<-h2b
testh2b$ordernum=as.factor(testh2b$ordernum)


h2c<-h2a
h2c$word=as.character(h2c$word)
wordlis=rep(h2c$word[1],h2c$averagevote[1])
i=2
bnum=dim(h2c)[1]
while (i<=bnum){
  wordlis= append(rep(h2c$word[i],h2c$averagevote[i]),wordlis)
  i=i+1
}
wordchar<-unlist(wordlis)
dscorpus2<- Corpus(VectorSource(wordchar))



h3=merge(x=producttags,y=productlistall[,c('name','vote')],by='name',all.x=T)
h31<- h3 %>%
  filter(tagtype=='BrandRelated')

h31$tag=as.factor(h31$tag)

h32<- h3 %>%
  filter(tagtype=='function')

h32$tag=as.factor(h32$tag)


h33<- h3 %>%
  filter(tagtype=='Platform')

h33$tag=as.factor(h33$tag)
summary(aov(h33$vote~h33$tag))

h34<- h3 %>%
  filter(tagtype=='SubType')

h34$tag=as.factor(h34$tag)

h35<- h3 %>%
  filter(tagtype=='Type')

h35$tag=as.factor(h35$tag)



#################### server.R ######################
####################################################

library(shiny)
library(dplyr,warn.conflicts = FALSE)

library(dygraphs)

library(ggplot2)

library(RColorBrewer)

library(wordcloud)
library(NLP,warn.conflicts = FALSE)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(stringr)
library(textreg)

library(plotly,warn.conflicts = FALSE)
library(zoo,warn.conflicts = FALSE)
library(xts,warn.conflicts = FALSE)

producttags<-read.csv('producttags.csv')
productlistall<-read.csv('productlistafter.csv')
productreview<-read.csv('productreview.csv')

productlistall$targetcustomer<-NULL
productlistall$datetime=gsub('T',' ',productlistall$datetime)
productlistall$datetime=gsub('Z',' ',productlistall$datetime)
productlistall$datetime=as.Date(productlistall$datetime)
productlistall=productlistall[order(productlistall$datetime),]
productlistall=productlistall[c(-1,-2),]

function(input, output, session) {
  
   
  terms <- reactive({
    

    input$update

    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })

  output$o1<- renderDygraph({
    input$update
    productlistall$name=ifelse(input$timetype=='name',1,productlistall$name)
    p1<-productlistall[,c(input$timetype,"datetime")]
    p1$datetime=as.character(p1$datetime)
    p1$datetime=as.Date(p1$datetime)
    p1a<-p1 %>%
      filter(datetime>=input$datetime[1] & datetime<=input$datetime[2]) 
    w=as.name(input$timetype)
    p1a<-p1a %>%
      mutate(datetime=as.factor(p1a$datetime)) %>%
      group_by(datetime) %>%
      summarise(totalproduct=sum(w)) 
    p1a$datetime=as.Date(p1a$datetime)
    zoo1<-as.xts(p1a,order.by=p1a$datetime)
    o1<-dygraph(zoo1) %>%
      dyRangeSelector() 
    
    return(o1)
  })
  output$o2<- renderPlotly({
    input$update
    
    p4<-producttags %>%
      group_by(tag) %>%
      summarise(tagnum=n())
    p4=p4[order(p4$tagnum,na.last = TRUE, decreasing = TRUE),]
    p4=p4[1:20,]
    g4<-ggplot(p4,aes(x=tag,y=tagnum,fill=tag))+
      geom_bar(stat="identity")+
      scale_x_discrete(limits =p4$tag)+
      theme_bw()+
      xlab("")+
      ylab('')+
      theme(axis.text.x = element_text(angle=45))
    
    
    ggplotly(g4)
    
  })
 
  wordcloud_rep <- repeatable(wordcloud)

  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,2),
                  min.freq = input$freq, max.words=input$max,random.order=F,
                  colors=brewer.pal(8, "Spectral"))
    
  })
  
  output$summary1 <- renderPrint({
    summary(aov(h1$vote ~ h1$makernum))
  })
  
  output$h1<- renderPlotly({
    plot_ly(productlistall, x = ~makernum, y = ~vote, color = ~makernum,
            size = ~makernum, text = ~paste("Name ", name))
  })
  
  output$h2<- renderPlotly({
    
    plot_ly(h1, y= ~vote,color=~makernum,type = "box")
  })
  
  output$summary2 <- renderPrint({
    summary(aov(testh2b$vote~testh2b$ordernum))
  })
  
  output$h3<- renderPlot({
    wordcloud(ds1clean,random.order=F,scale = c(4, 0.2),max.words=200,colors=brewer.pal(8, "Spectral"))
  })
  output$h4<- renderPlotly({
  plot_ly(h2b, x = ~ordernum, y = ~vote, color = ~ordernum,text = ~paste("Word: ", word))
  })
  output$h5<- renderPlot({
  wordcloud(dscorpus2,random.order=F,scale = c(2, 2),max.words=200,colors=brewer.pal(8, "Spectral"))
  })
  
  output$summary3 <- renderPrint({
    summary(aov(h32$vote~h32$tag)) 
  })
  output$summary4 <- renderPrint({
    summary(aov(h33$vote~h33$tag)) 
  })
  output$summary5 <- renderPrint({
    summary(aov(h34$vote~h34$tag))
  })
  output$summary6 <- renderPrint({
    summary(aov(h35$vote~h35$tag))
  })
  
}

###################### ui.R ########################
####################################################

library(shiny)
library(dplyr,warn.conflicts = FALSE)

library(dygraphs)

library(ggplot2)

library(RColorBrewer)

library(wordcloud)
library(NLP,warn.conflicts = FALSE)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(stringr)
library(textreg)

library(plotly,warn.conflicts = FALSE)
library(zoo,warn.conflicts = FALSE)
library(xts,warn.conflicts = FALSE)


shinyUI(fluidPage(
  

  titlePanel("Product Hunt Web Scrapying Project"),
  sidebarLayout(
  sidebarPanel(
    dateRangeInput("datetime", label = h3("Date range"), start = "2016-10-20", end = "2017-07-29"),
    selectInput("timetype","Overview Choose:",
                choices=timetypes),
    selectInput("selection", "Review Choose:",
                choices =reviewtypes
                  ),
    actionButton("update", "Change"),
    hr(),
    sliderInput("freq",
                "Minimum Frequency:",
                min = 1,  max = 100, value = 5),
    sliderInput("max",
                "Maximum Number of Words:",
                min = 1,  max = 500,  value = 200)
  ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 
                 h4('Tag'),
                 plotlyOutput("o2"),
                 h4("Time"),
                 dygraphOutput("o1")



        ),
        tabPanel("Review",
                 plotOutput("plot")
      ),
     tabPanel("Hypothesis-Team Member",
              h4("Summary"),
              verbatimTextOutput("summary1"),
              h4('ScatterPlot'),
              plotlyOutput("h1"),
              h4("Boxplot"),
              plotlyOutput("h2")
  ),
  tabPanel("Hypothesis-Description",
           h4("Summary"),
           verbatimTextOutput("summary2"),
           h3('Description'),
           plotOutput("h3"),
           h3("Word"),
           plotlyOutput("h4"),
           h4('Wordcloud'), 
           plotOutput("h5")

),
tabPanel("Hypothesis-Other",
         h4("Function"),
         verbatimTextOutput("summary3"),
         h4('Platform'),
         verbatimTextOutput("summary4"),
         h4("SubType"),
         verbatimTextOutput("summary5"),
         h4('Type'), 
         verbatimTextOutput("summary6")
         
)
))
)
))






