#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
B2010 <- read.csv("BP Apprehensions 2010.csv")
B2017 <- read.csv("PB Apprehensions 2017.csv")

#add setor sums and smonth sums in 2010
rownames(B2010) <- B2010[,1]
B2010 <- subset(B2010, select = -c(Sector))
#rearrange the columns by moving Oct, Nov, and Dec to the end 
B2010 <- cbind(subset.data.frame(B2010)[,4:12],subset.data.frame(B2010)[,1:3])
monthSum2010 <- colSums((B2010))
B2010 <- rbind(B2010,monthSum2010)
rownames(B2010)[10] <- "Sector Total"

sectorSum2010 <- rowSums(B2010)
B2010 <- cbind(B2010,sectorSum2010)
colnames(B2010)[13] <- "Annual Total" 

#add setor sums and smonth sums in 2017
rownames(B2017) <- B2017[,1]
B2017 <- subset(B2017, select = -c(Sector))
#rearrange the columns by moving Oct, Nov, and Dec to the end 
B2017 <- cbind(subset.data.frame(B2017)[,4:12],subset.data.frame(B2017)[,1:3])
monthSum2017 <- colSums((B2017))
B2017 <- rbind(B2017,monthSum2017)
rownames(B2017)[10] <- "Sector Total"

sectorSum2017 <- rowSums(B2017)
B2017 <- cbind(B2017,sectorSum2017)
colnames(B2017)[13] <- "Annual Total" 

#compare apprehensions by months
sector <- rownames(B2010)
month <- colnames(B2010)

#input is the index of the sector you want to look up
graph_months <- function(i){ 
  year_comp <- rbind(B2010[i,1:12],B2017[i,1:12])
  barplot(as.matrix(year_comp), beside = T, col = c("green", "yellow"), bty="n",las=2)
  legend("topleft", 
         c(paste(sector[i] ,"2010"),paste(sector[i],"2017")), 
         pch=15,  
         col=c("green","yellow"),  
         bty="n"
  )
}

#compare apprehensions by sectors
#input is the month you want to look up
graph_sectors <- function(i){
  B2010<-t(B2010)
  B2017<-t(B2017)
  year_comp <- rbind(B2010[i,1:9],B2017[i,1:9])
  barplot(year_comp, 
          beside = TRUE, 
          col = c("blue", "orange"), 
          bty="n",
          las=2
  )
  legend("topleft", 
         c(paste(month[i] ,"2010"),
           paste(month[i],"2017")),
         pch=15,  
         col=c("blue", "orange"),  
         bty="n")
}





#shiny

library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Compare Apprehensions in 2010 & 2017 by Months and Sectors"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  fluidRow(
    column(width=12,
         sidebarPanel(
           selectInput("sector", "choose a region:", choices = sector))
    ),
    column(width=12,
           plotOutput('sector'))
  ),
  
  fluidRow(
    column(12,
           sidebarPanel(
             selectInput("month", "Choose a month:", choices = month))
    ),
    column(12,
           plotOutput('month'))
  ))
  
)

