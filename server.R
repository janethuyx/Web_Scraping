
#product hunt

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
    
    g4<-ggplot(p4,aes(x=tag,y=tagnum,fill=tag))+
      geom_bar(stat="identity")+
      scale_x_discrete(limits =p4$tag)+
      theme_bw()
    
    
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
    wordcloud(ds1clean,random.order=F,scale = c(4, 0.2),max.words=200,colors='orange')
  })
  output$h4<- renderPlotly({
  plot_ly(h2b, x = ~ordernum, y = ~vote, color = ~ordernum,text = ~paste("Word: ", word))
  })
  output$h5<- renderPlot({
  wordcloud(dscorpus2,random.order=F,scale = c(2, 2),max.words=200,colors='orange')
  })
  
}

  

