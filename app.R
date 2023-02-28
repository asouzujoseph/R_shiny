library("magrittr")
library("tidyverse")
library("dplyr")
library("lubridate")
library("shiny")
library("ggplot2")
library("plotly")


path<-"C:/Users/ASOUZUN/Desktop/NIPT"

### function to extract all the files in the directory
extract_files <- function(path){
  merged_list=list()
  for (year_folders in list.files(path,full.names=TRUE)){
    for (files in list.files(year_folders,pattern="*.tab",full.names = TRUE)){
      merged_list=append(merged_list,files)
    }
  }
  return(merged_list)
}

merged_list <- extract_files(path) ## call function
## convert the tab files to a list of data frames
data = lapply(merged_list, function(x) data.frame(read.table(x, header=T, sep="\t", comment.char="#",na.strings=".",stringsAsFactors=FALSE,quote="",fill=FALSE), filename = x))

#### function to select required columns from the data frames, extract the data frames from the list and combine them into one big dataset for use in R-shiny app
combine_DF <- function(data){
  n=length(data)
  df_list = vector("list", length = n)
  counter = 0
  for (i in data){
    df=data.frame(i)
    df= df %>% select(ReportData,FetalSex,HL_AAOP_RunTimestamp,FetalFractionQC,SignalToNoise,SampleIntegrity,ArrayQuality,Signal,Noise)
    counter=counter+1
    df_list[[counter]] <- df
  }
  dataset <- do.call(rbind,df_list)
  return (dataset)
}

### data cleaning and formatting
dataset <- combine_DF(data)
dataset$HL_AAOP_RunTimestamp = gsub("( [0-9]+:[0-9]+)$", "\\1:00", dataset$HL_AAOP_RunTimestamp)  ## concatenate "00" to rows that do not have seconds component
dataset$HL_AAOP_RunTimestamp<-mdy_hms(dataset$HL_AAOP_RunTimestamp)
dataset$ReportData <- toupper(dataset$ReportData)
dataset$Signal <- as.numeric(dataset$Signal)
DF <- dataset %>% rename(Date_time=HL_AAOP_RunTimestamp) %>% group_by(Date_time) %>% 
  mutate(Number_of_Males = sum(FetalSex=="Male",na.rm=TRUE),Number_of_Females=sum(FetalSex=="Female",na.rm=TRUE), Sex_Inconclusive=sum(FetalSex=="Test Inconclusive",na.rm=TRUE), Sex_ratio = round((Number_of_Females / Number_of_Males),2),
         Passed_samples = sum(ReportData==TRUE,na.rm=TRUE), Failed_samples = 94-Passed_samples, Failure_rate = round((Failed_samples / 94)*100,2), Low_signal_to_noise = sum(SignalToNoise < 0.9 & FetalFractionQC > 0.04),
         Sample_Integrity = sum(SampleIntegrity < 0.9 & FetalFractionQC > 0)+sum(SampleIntegrity < 0.9 & FetalFractionQC == 0), Low_Fetal_Fraction=sum(FetalFractionQC < 0.04 & ArrayQuality > 0.9 & SampleIntegrity > 0.9),
         Array_Quality_Failure=sum(ArrayQuality < 0.9 & Signal != "NA"),
         Noise_only = sum(Noise < 0.9& FetalFractionQC > 0.04 & ArrayQuality > 0.9 & SampleIntegrity > 0.9 & SignalToNoise > 0.9), Signal_Failure=sum(Signal<200,na.rm=TRUE))

## Collapse and generate numeric dataset for use in R-shiny
final_DF <- DF %>% select(-c(ReportData,FetalSex,FetalFractionQC,SignalToNoise,SampleIntegrity,ArrayQuality,Signal,Noise)) 
final_DF <- aggregate(final_DF, by=list(final_DF$Date_time),FUN=mean, na.rm=TRUE)
rownames(final_DF) <- final_DF$Date_time
final_DF$Date <- as.Date(final_DF$Date_time)
final_DF <- final_DF[,c(-1)] %>% relocate(Date, .after = Date_time)


#### Shiny_App
ui <- fluidPage(
    theme = bslib::bs_theme(version = 4,bootswatch = "cerulean"),
    titlePanel(h1("NIPT Quality Metrics", align="center")),
    br(),
    
    tabsetPanel(
      tabPanel("Dashboard", 
    fluidRow(
      column(4,selectInput("metric", label = "Please choose a metric",
          choices = c("Number_of_Males","Number_of_Females","Sex_Inconclusive","Sex_ratio","Passed_samples","Failed_samples","Failure_rate",
                      "Low_signal_to_noise","Sample_Integrity","Low_Fetal_Fraction","Array_Quality_Failure","Noise_only","Signal_Failure"), 
          selected="Failure_rate")),
      column(4,dateRangeInput("date", "Select a range of dates",start="2023-01-01")),
      column(4,verbatimTextOutput("summary"))),
    br(),
    fluidRow(
      column(12,plotOutput("graph",click = "plot_click"),
             verbatimTextOutput("info"))),
    br()),
    
    tabPanel("Dataset", 
    fluidRow(
      column(12,dataTableOutput("filtered"))),
      column(12,downloadButton("download", "Download.tsv")))
    )
)  

    
server <- function(input, output, session) {
      filtered=reactive(final_DF %>% filter(between(Date, as.Date(input$date[1]), as.Date(input$date[2]))))
      
      DF=reactive({
        new_DF=final_DF %>% filter(between(Date, as.Date(input$date[1]), as.Date(input$date[2])))
        new_DF=new_DF[,c("Date_time",input$metric)]
      })
      
      
      output$filtered <- renderDataTable({filtered()})
      #output$graph <- renderPlot({plot(DF(),lwd = 3, col='sky blue',type="h",main=paste("Plot of ",input$metric," over time"))}, res = 96)
      output$graph <- renderPlot({
        #req(DF())
        #ggplot(DF())+ geom_line(aes(y=input$metric))               
        ggplot(DF(), aes(DF()$Date_time,y=DF()[,c(input$metric)]))+ geom_histogram(col="sky blue",stat = "identity") +labs(x="Date",y=input$metric,title =(paste("Plot of ",input$metric," over time"))) +
                      theme(plot.title = element_text(hjust = 0.5,face="bold",size=15),axis.title.x = element_text(size=12, face="bold"),
                            axis.title.y = element_text(size=12, face="bold"))
      })
      
      output$info <- renderPrint({
        req(input$plot_click)
        x <- as.character(as.Date(round(as.numeric(input$plot_click$x), 0), origin = origin))
        y <- round(input$plot_click$y, 2)
        cat("[", y, "]", sep = "")
      })
      
      output$summary <- renderPrint({ 
        summary(DF()[,c(input$metric)])
      })
        
      output$download <- downloadHandler(
        filename = function() {
          paste0("Dataset", ".tsv")
        },
        content = function(file) {
          vroom::vroom_write(filtered(), file)
        }
      )
}

shinyApp(ui, server)






