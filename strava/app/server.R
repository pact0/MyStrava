options(shiny.maxRequestSize=30*1024^2)


library(FITfileR)
library(leaflet)
library(dplyr)
library(tools)
library(stringr)
library(openxlsx)
library(trackeR)
library(RSQLite)



saveData <- function(value, user) {
  # Connect to the database
  db <- dbConnect(SQLite(), 'database.sqlite')
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "UPDATE credentials SET value = %s WHERE user = '%s'",
    paste(value),
    paste(user)
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}








server <- function(input, output) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials('./database.sqlite')
  )
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  

#C <- reactive ({ as.numeric(input$C) })  

#constant
coef <- 3.6
  
#output$verb <- renderPrint({ mydata() })

mydata<- reactive({
  

x<-req(input$file)
x<-x$name
ext<-strsplit(x, split="\\.")[[1]][length(strsplit(x, split="\\.")[[1]])]

if (tolower(ext)=="fit"){
data_f <- readFitFile(input$file$datapath)

e <- records(data_f) %>% 
  bind_rows() %>% 
  arrange(timestamp) 

e_df<-as.data.frame(e)
} # od fit

if (tolower(ext)=="fit"){
data_f <- readFitFile(input$file$datapath)

e <- records(data_f) %>% 
  bind_rows() %>% 
  arrange(timestamp) 

e_df<-as.data.frame(e)
} # od fit


if (tolower(ext)=="tcx"){
e <- readTCX(input$file$datapath)

e_df<-e[colSums(!is.na(e)) > 0.1*length(e[,1])]

ti<-str_detect(colnames(e_df), "time")
k<-dim(e_df)[2]

for (i in 1:k){
	if (ti[i]==FALSE){
		e_df[,i]<-(approxfun(1:length(e_df[,i]),e_df[,i])(1:length(e_df[,i])))
	}
}
} # od tcx

if (tolower(ext)=="gpx"){
e <- readGPX(input$file$datapath)

e_df<-e[colSums(!is.na(e)) > 0.1*length(e[,1])]

ti<-str_detect(colnames(e_df), "time")
k<-dim(e_df)[2]

for (i in 1:k){
	if (ti[i]==FALSE){
		e_df[,i]<-(approxfun(1:length(e_df[,i]),e_df[,i])(1:length(e_df[,i])))
	}
}
} # od gpx



e_df

}) #mydata



###map
output$mymap <- renderLeaflet({

e_df<-mydata()

cols<-colnames(e_df)
lo<-str_detect(colnames(e_df), "long")
la<-str_detect(colnames(e_df), "lat")

coords <- e_df %>% 
  select(cols[lo], cols[la])

m <- coords %>% 
  as.matrix() %>%
  leaflet(  ) %>%
  addTiles() %>%
  addPolylines( )    
m


})

output$Fit_plot <- renderPlot({

e_df<-mydata()

cols<-colnames(e_df)
lo<-str_detect(colnames(e_df), "long")
la<-str_detect(colnames(e_df), "lat")
ti<-str_detect(colnames(e_df), "time")
sp<-str_detect(colnames(e_df), "speed")
di<-str_detect(colnames(e_df), "dist")

if (sum(sp)==0){
len<-length(e_df[,ti])
speed<-rep(NA,len)
speed[2:len]<-e_df[2:(len),ti]-e_df[1:(len-1),ti]
speed[speed==0]<-NA
speed[2:len]<-e_df[2:(len),di]-e_df[1:(len-1),di]


e_df$speed<-speed
cols<-colnames(e_df)
lo<-str_detect(colnames(e_df), "long")
la<-str_detect(colnames(e_df), "lat")
ti<-str_detect(colnames(e_df), "time")
di<-str_detect(colnames(e_df), "dist")
sp<-str_detect(colnames(e_df), "speed")
di<-str_detect(colnames(e_df), "dist")
}


saveData(runif(1, 5.0, 7.5), res_auth$user)

if (input$kmh==TRUE)
{
e_df[sp]<-e_df[sp]*3.6
}

all<-rep(TRUE,length(cols))
sel<-as.logical(all-ti-lo-la)
sel_v<-which(sel)
len<-length(sel_v)

par(mfrow=c(len,1),mar = c(4,4,0.1,0))
for (i in sel_v[1:(length(sel_v)-1)]){
plot(e_df[ti],e_df[,i],xlab="",ylab=cols[i],t="l",lwd=2,cex.lab=1.4)
}
plot(e_df[ti],e_df[,sel_v[len]],xlab="Time",ylab=cols[sel_v[len]],t="l",lwd=2,cex.lab=1.4)

})

#output$verb <- renderPrint({ summary(mydata()) })

#output$Fit_table <- renderTable({mydata()},digits = 4)


output$Box_plot <- renderPlot({

e_df<-mydata()

cols<-colnames(e_df)
lo<-str_detect(colnames(e_df), "long")
la<-str_detect(colnames(e_df), "lat")
ti<-str_detect(colnames(e_df), "time")
sp<-str_detect(colnames(e_df), "speed")
di<-str_detect(colnames(e_df), "dist")

if (sum(sp)==0){
len<-length(e_df[,ti])
speed<-rep(NA,len)
speed[2:len]<-e_df[2:(len),ti]-e_df[1:(len-1),ti]
speed[speed==0]<-NA
speed[2:len]<-e_df[2:(len),di]-e_df[1:(len-1),di]
e_df$speed<-speed
cols<-colnames(e_df)
lo<-str_detect(colnames(e_df), "long")
la<-str_detect(colnames(e_df), "lat")
ti<-str_detect(colnames(e_df), "time")
di<-str_detect(colnames(e_df), "dist")
sp<-str_detect(colnames(e_df), "speed")
di<-str_detect(colnames(e_df), "dist")
}


if (input$kmh==TRUE)
{
e_df[sp]<-e_df[sp]*3.6
}

all<-rep(TRUE,length(cols))
sel<-as.logical(all-ti-lo-la)
sel_v<-which(sel)
len<-length(sel_v)

par(mfrow=c(2,ceiling(len/2)),mar = c(0.6,2,2,1))
for (i in sel_v[1:(length(sel_v)-1)]){
boxplot(e_df[,i],main=cols[i],cex.axis=1.2)
}
boxplot(e_df[,sel_v[len]],main=cols[sel_v[len]],cex.axis=1.2)

})


output$downloadData <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file),".xlsx")
    },

    content = function(file) {
      write.xlsx(mydata(), file)
    }
)


}








