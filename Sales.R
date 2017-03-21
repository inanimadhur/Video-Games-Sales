games <- read.csv("C:\\Users\\madhu\\Downloads\\Video_Games_Sales_as_at_22_Dec_2016.csv")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
head(games)

Pub_count <- games%>%
  select(Publisher,Genre,Year_of_Release)%>%
  group_by(Publisher)%>%
  summarise(Count=n())%>%
  filter(Count>100)%>%
  arrange(desc(Count))

Pub_count

#graph for publisher count
ggplot(data=Pub_count,aes(Publisher,Count))+
  geom_bar(stat="identity")  +coord_flip() + 
  geom_text(aes(label=Count),size=3,color="white",hjust=1.5,vjust=0.3)+
  theme_get()

#graph of game Genre by Publisher
games%>%
  group_by(Genre,Publisher)%>%
  summarise(count=n())%>%
  filter(count>50)%>%
  arrange((count))%>%
ggplot(aes(Publisher,count,fill=Genre)) +
  geom_bar(stat="identity") +coord_flip()  +
  theme(legend.position = "top") 


#total NA_sales of all publishers

 sale1 <- games%>%
   select(Publisher,NA_Sales)%>%
   group_by(Publisher)%>%
   summarise_each(funs(sum))%>%
   filter(NA_Sales>100)%>%
   ggplot(aes(Publisher,NA_Sales,fill=NA_Sales)) +geom_bar(stat="identity") + coord_flip()
 
 #total EU_sales of all publishers
 sale2 <- games%>%
   select(Publisher,EU_Sales)%>%
   group_by(Publisher)%>%
   summarise_each(funs(sum))%>%
   filter(EU_Sales>100)%>%
   ggplot(aes(Publisher,EU_Sales,fill=EU_Sales)) +geom_bar(stat="identity") + coord_flip()
 
 
 #total JP_sales of all publishers
 sale3 <- games%>%
   select(Publisher,JP_Sales)%>%
   group_by(Publisher)%>%
   summarise_each(funs(sum))%>%
   filter(JP_Sales>50)%>%
   ggplot(aes(Publisher,JP_Sales,fill=JP_Sales)) +geom_bar(stat="identity") + coord_flip()
 
 
 #total Other_sales of all publishers
 sale4 <- games%>%
   select(Publisher,Other_Sales)%>%
   group_by(Publisher)%>%
   summarise_each(funs(sum))%>%
   filter(Other_Sales>50)%>%
   ggplot(aes(Publisher,Other_Sales,fill=Other_Sales)) +geom_bar(stat="identity") + coord_flip()
 

 #total Global_sales of all publishers
 library("plotly")
sale5 <- games%>%
  select(Publisher,Global_Sales)%>%
  group_by(Publisher)%>%
  summarise_each(funs(sum))%>%
  filter(Global_Sales>50)%>%
  plot_ly(labels = ~Publisher, values = ~Global_Sales) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "Global Sales of Publishers",  showlegend = F,
         xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
         yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
  

  

  
  #platform wars
  games%>%
    group_by(Platform)%>%
    summarise(number=n())%>%
    arrange(-number)%>%
    filter(number>50)%>%
    mutate(Platform=factor(Platform,Platform))%>%
    ggplot(aes(Platform,number))+ geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

install.packages("devtools")
library(devtools)
install_github("easyGgplot2")
library(easyGgplot2)



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(sale1,sale2,sale3,sale4,cols=2) 
