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
  barplot(as.matrix(year_comp), beside = T, col = c("green", "yellow"), bty="n",las=2,
          main = "Compare 2010&2017 Apprehensions by Months")
  legend("topleft", 
         c(paste(sector[i] ,"2010"),paste(sector[i],"2017")), 
         pch=15,  
         col=c("green","yellow"),  
         bty="n"
         )
}
#say if we want to compare apprehension in Del Rio by months
graph_months(2)

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
          las=2,
          main = "Compare 2010&2017 Apprehensions by Sectors"
          )
  legend("topleft", 
         c(paste(month[i] ,"2010"),
           paste(month[i],"2017")),
         pch=15,  
         col=c("blue", "orange"),  
         bty="n")
}
#say if we want to compare apprehensions in January by sectors
graph_sectors(1)


#find the index of the sector that has most apprehensions in 2010 
most_apprehensions_2010 <- match(max(B2010[1:9,13]), B2010[1:9,13])
#it is Tucson
rownames(B2010)[most_apprehensions_2010]

#find the index of the sector that has most apprehensions in 2017
most_apprehensions_2017 <- match(max(B2017[1:9,13]), B2017[1:9,13])
#it is Rio Grande Valley
rownames(B2017)[most_apprehensions_2017]

#create the dataset for Tucson in 2010
B2010_Tuson <- B2010[most_apprehensions_2010,1:12]
#create the dataset for Rio Grande Valley in 2017
B2017_RGV <- B2017[most_apprehensions_2017,1:12]

#test whether these two samples have same sample mean 
t.test(B2010_Tuson, B2017_RGV)
#by doing Welch Two Sample t-test, we get p-value = 0.06346 > 0.05 
#so we fail to reject the null hypothesis that the mean of apprehension of Tucson in 2010 is equal to 
# the mean of apprehension of Rio Grande Valley in 2017, and therefore conclude that there is no 
#significant difference between the maximum of apprehensions by secoter in 2010 and 2017


#find the three months that has the most apprehensions in 2010
most_3m_apprehensions_2010 <- order(B2010[10,1:12], decreasing = TRUE)[1:3]
#they are March, April, and May
colnames(B2010)[most_3m_apprehensions_2010]

#find the three months that has the most apprehensions in 2017
most_3m_apprehensions_2017 <- order(B2017[10,1:12], decreasing = TRUE)[1:3]
#they are November, October, and December 
colnames(B2017)[most_3m_apprehensions_2017]

#create the dataset for March, April, and May in 2010
B2010_3m <- B2010[most_3m_apprehensions_2010,1:12]
#create the dataset for November, October, and December in 2017
B2017_3m <- B2017[most_3m_apprehensions_2017,1:12]

#test whether these two samples have same sample mean 
t.test(B2010_3m, B2017_3m)
#by doing Welch Two Sample t-test, we get p-value = 0.3454 > 0.05 
#so we fail to reject the null hypothesis that the mean of apprehension of from March to May in 2010 is equal to 
# the mean of apprehension from October to December in 2017, and therefore conclude that there is no 
#significant difference between the maximum of apprehensions from March to May in 2010 and from October to December in 2017






