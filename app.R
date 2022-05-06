`#------------------------------- LOADING LIBRARIES -------------------------------

#rshiny
library(shiny)
library(shinydashboard)
#data & plots
library(wordcloud2)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(factoextra)
library(hopkins)
#prettify rshiny dashboard
library(shinythemes)
library(shinyWidgets)
'shinyWidgetsGallery()' #gallery
#install newest version of wordcloud2 to fix issue with 2 plots rendering
#please run these lines once before working the dashboard!!!!
'install.packages("remotes")
remotes::install_github("Lchiffon/wordcloud2")'


#------------------------------- LOADING DATA -----------------------------------

#this code is for part 1 (exploring themes)
#please run this once (without quotation marks) if the plots for "Data Exploration" aren't showing
#You should also save the new environment generated into the R Workspace
'
d2<-read_csv("dat_2.csv")
d3<-read_csv("dat_3.csv")
d6<-read_csv("dat_6.csv")

# QUESTION 1 (EXPLORING THEMES IN THE DATA)
#adding data
d4<-read_csv("dat_4.csv")
d5<-read_csv("dat_5.csv")


#CHANGING D4
d4 <- d4 %>% 
  mutate(course_major=str_extract(id, "^.*(?=\\_)"), .after=id)

#create lists of majors
major_arts <- c("ART","DGM","MSIC","MUS","PHE")
major_business <- c("ACC", "AVM", "CDV", "CED", "ECN","ECNM","FIN","HRM","INS",
                    "LDR", "MGT", "MKT", "NPM", "PBR", "PDP", "PJM", "SIA", "SMT")
major_socsci <- c("ANT", "CJS", "CMMN", "CMN", "COP", "EDU", "EDUC", "ENG", "ESL",
                  "ESLG", "HLS", "HST", "HSTY", "HSV", "INT", "LNG", "LST", "LWP",
                  "PHL", "PHLS", "PLSC", "POL", "SCLY", "SOC")
major_stem <- c("ALY", "BIO", "BTC","CET","CHM","CMG","EAI","EET","ESC","GBST",
                "GET","GIS","GST","HMG","HSC","ITC","MATM","MET","MTH","NTR",
                "PHY","PMC","PSY","PTH","RFA","RGA","RMS","RPT","TCC")
major_elective <- c("ASL", "CRS", "ETC","FRN","GEN","GEO","GSE","HIA","HUM","IRM",
                    "ITP","JLS","KMG","MIS","OCM","SCI","SPN","TCM") 

#use list of majors to assign major types
d4 <- d4 %>% rowwise() %>% 
  mutate(major_type = case_when(
    course_major %in% major_arts ~"Arts",
    course_major %in% major_business ~"Business & Management",
    course_major %in% major_socsci ~"Social Sciences",
    course_major %in% major_stem ~"STEM",
    course_major %in% major_elective ~"Electives", TRUE ~"ERROR"
  ), .after=course_major) %>% ungroup()


#CHANGING D5  
d5 <- d5 %>% 
  mutate(course_major=str_extract(id, "^.*(?=\\_)"), .after=id)


#use list of majors to assign major types to d5
d5 <- d5 %>% rowwise() %>% 
  mutate(major_type = case_when(
    course_major %in% major_arts ~"Arts",
    course_major %in% major_business ~"Business & Management",
    course_major %in% major_socsci ~"Social Sciences",
    course_major %in% major_stem ~"STEM",
    course_major %in% major_elective ~"Electives", TRUE ~"ERROR"
  ), .after=course_major)


#create list of college level
level_undergrad <- c("UC", "US", "UG", "US | UG")
level_grad <- "GP"
level_license <- "CP"
level_both <- "UG | GR"

#use list to assign levels
d4 <- d4 %>% rowwise() %>% 
  mutate(Level_Type = case_when(
    Level %in% level_undergrad ~"Undergrad",
    Level %in% level_grad ~"Grad",
    Level %in% level_license ~"Licensure/Professional Dev.",
    Level %in% level_both ~"Undergrad & Grad",
    TRUE ~"ERROR"
  ), .after=Level)


#use list to assign levels to d5
d5 <- d5 %>% rowwise() %>% 
  mutate(Level_Type = case_when(
    Level %in% level_undergrad ~"Undergrad",
    Level %in% level_grad ~"Grad",
    Level %in% level_license ~"Licensure/Professional Dev.",
    Level %in% level_both ~"Undergrad & Grad",
    TRUE ~"ERROR"
  ), .after=Level)


#QUESTION 2 - TOPIC ANALYSIS


#Use the topic probabilities to judge topic content
d3<-d3%>%
  separate(term,c("term","topic_2"))%>%
  select(topic,term,beta,diff_ratio)


#Top keywords for each topic

topic_data<-d3%>%
  group_by(topic)%>%
  slice_max(beta,n=20)%>%
  ungroup()%>%
  mutate(term = reorder_within(term,beta, topic))


#create labels for each topic
topic_labels=c("1"="Healthcare Policy & Law",
               "2"="Digital Media & Design",
               "3"="Pharmacy & Drug Regulation",
               "4"="Leadership & Education",
               "5"="Public Policy",
               "6"="Language & Literature",
               "7"="Business Intelligence & Analytics",
               "8"="Research Developement & Writing",
               "9"="Free Elective (for Credit)",
               "10"="Project & Systems Management",
               "11"="Mathematics & Engineering",
               "12"="Research & Science",
               "13"="Nutrition & Food Sciences",
               "14"="Global Studies & International Politics",
               "15"="Business Strategy & Communication")
               
               
#QUESTION 3 - THEME ANALYSIS

#building topic table for each observation

topic_table<-d4%>%
  gather("column","value",X1:X15)%>%
  group_by(id,word)%>%
  mutate(m_val = max(value))%>%
  ungroup()%>%
  mutate(is_max = ifelse(value == m_val,sample(column),NA))%>%
  drop_na()%>% #225 NA
  group_by(id,word)%>%
  mutate(topic = sample(column,1))%>%
  ungroup()%>%
  select(id,topic)%>%
  distinct() #topic_table missing course IDs ALY_6983 , BIO_2101 , BIO_2501
  
  
#Assign topics from topic_table to data

d4<-d4%>%
  left_join(topic_table,
            by = c("id")
  )  

d5<-d5%>%
  left_join(topic_table,
            by = c("id")
  )
  
d6<-d6%>%
  left_join(topic_table,
            by = c("id")
  )
  
#create new column for topic names in d4 (used for graphing)
d4 <- d4 %>% rowwise() %>% 
  mutate(topic_name = case_when(
    topic %in% "X1" ~"Healthcare Policy & Law",
    topic %in% "X2" ~"Digital Media & Design",
    topic %in% "X3" ~"Pharmacy & Drug Regulation",
    topic %in% "X4" ~"Leadership & Education",
    topic %in% "X5" ~"Public Policy",
    topic %in% "X6" ~"Language & Literature",
    topic %in% "X7" ~"Business Intelligence & Analytics",
    topic %in% "X8" ~"Research Developement & Writing",
    topic %in% "X9" ~"Free Elective (for Credit)",
    topic %in% "X10" ~"Project & Systems Management",
    topic %in% "X11" ~"Mathematics & Engineering",
    topic %in% "X12" ~"Research & Science",
    topic %in% "X13" ~"Nutrition & Food Sciences",
    topic %in% "X14" ~"Global Studies & International Politics",
    topic %in% "X15" ~"Business Strategy & Communication"
  ), .after=topic)
  
  
  d5 <- d5 %>% rowwise() %>% 
  mutate(topic_name = case_when(
    topic %in% "X1" ~"Healthcare Policy & Law",
    topic %in% "X2" ~"Digital Media & Design",
    topic %in% "X3" ~"Pharmacy & Drug Regulation",
    topic %in% "X4" ~"Leadership & Education",
    topic %in% "X5" ~"Public Policy",
    topic %in% "X6" ~"Language & Literature",
    topic %in% "X7" ~"Business Intelligence & Analytics",
    topic %in% "X8" ~"Research Developement & Writing",
    topic %in% "X9" ~"Free Elective (for Credit)",
    topic %in% "X10" ~"Project & Systems Management",
    topic %in% "X11" ~"Mathematics & Engineering",
    topic %in% "X12" ~"Research & Science",
    topic %in% "X13" ~"Nutrition & Food Sciences",
    topic %in% "X14" ~"Global Studies & International Politics",
    topic %in% "X15" ~"Business Strategy & Communication"
  ), .after=topic)




#QUESTION 4 - CLUSTERING

#set.seed for reproducibility
set.seed(50)

#clustering with positive sentiment column (sample of 20,000)
data_scaled <- scale(d4%>% ungroup() %>%
                select(positive) %>% 
                sample_n(20000))


#hopkins(data_scaled) #hopkins value = 1


fviz_nbclust(data_scaled, kmeans, method="silhouette") #best k=2 (3 is also okay)

fviz_nbclust(data_scaled, kmeans, method="wss") #best k=3 (2 is also ok)

#final decision: clusters = 2 and 3


#this is just for the graph to render

set.seed(2)

data_scaled2 <- scale(d4%>% ungroup() %>%
                        select(positive) %>% 
                        sample_n(3000))

fviz_nbclust(data_scaled2, kmeans, method="silhouette")
fviz_nbclust(data_scaled2, kmeans, method="wss")



#set clusters=2
k2 <- kmeans(data_scaled, centers = 2, nstart = 25)

#set clusters=3
k3 <- kmeans(data_scaled, centers = 3, nstart = 25)


d4_sample <- d4%>% ungroup() %>% sample_n(20000)

#add col with d4 data & 2 clusters
d4_sample <-tibble(d4_sample,k2_cluster = k2$cluster)

#add col with d4 data & 3 clusters
d4_sample <-tibble(d4_sample,k3_cluster = k3$cluster)


#find mean positive score
mean_pos_score_k2 <- d4_sample%>%
  group_by(k2_cluster) %>%
  summarize(mean_pos_score = mean(positive))

mean_pos_score_k3 <- d4_sample%>%
  group_by(k3_cluster) %>%
  summarize(mean_pos_score = mean(positive))
  
  
  
# QUESTION 5 - PROFILE OF TOPICS


#creating df with probability of cluster (k=2)
d4_sample_plot_k2 <- d4_sample %>%
  group_by(k2_cluster)%>%
  summarize(across(X1:X15,c(mean))) %>%
  rename_with(~ gsub("_1", "",.x)) %>%
  gather(X1:X15,key = "Topic",value = "Probability")

#creating df with probability of cluster (k=3)
d4_sample_plot_k3 <- d4_sample %>%
  group_by(k3_cluster)%>%
  summarize(across(X1:X15,c(mean)))%>%
  rename_with(~ gsub("_1", "",.x)) %>%
  gather(X1:X15,key = "Topic",value = "Probability")
  
'


#################################################################################

#please keep codes below this line
#if you run a code that changes the work environment, 
#please put it above (in the yellow portion)



#------------------------------- CREATING UI ----------------------------------


#putting a navigation bar on top & adding title to dashboard
ui <- navbarPage(theme = shinytheme("united"),
                 
                 title= div(img(src="https://i.pinimg.com/originals/0a/0d/da/0a0dda7933e3d66a17a109e723cf74be.png",
                                width=110,
                                height=40,
                                style = "margin:-20px 5px"),
                            "CPS Data Dashboard"),
                 
                 
                 #--------------------------- Page 1 Layout -----------------------------
                 tabPanel("Data Exploration",
                          #sidebar  
                          sidebarLayout(
                            sidebarPanel(
                              
                              #checkbox for course type  
                              awesomeCheckboxGroup(
                                inputId = "majortype",
                                label = "Course Type", 
                                choices = c(unique(d4$major_type)),
                                selected = c(unique(d4$major_type)),
                                status = "danger",
                              ),
                              #disclaimer
                              h6("(Electives are 1-4 credit courses that
                                   allow students to fill credit requirements)"),
                              
                              br(), #linebreak
                              
                              #buttons for program type
                              checkboxGroupButtons(
                                inputId = "leveltype",
                                label = "Program Type",
                                choices=c("Undergrad","Grad","Undergrad & Grad",
                                          "Licensure/Professional Dev."),
                                selected=c("Undergrad"),
                                status = "danger"
                              ),
                              
                              #disclaimer
                              h6("(Multiple Selection Enabled)"),
                              
                              br(), #linebreak
                              
                              #numeric input for word n selection
                              numericInput("top_n","Top Number of Words",value = 10)
                            ),
                            
                            #main panel      
                            mainPanel(align="center",
                                      tabsetPanel(
                                        #tab 1 - bar plots  
                                        tabPanel("Plot",
                                                 h4("Single Word"),
                                                 plotOutput("themeplot"),
                                                 
                                                 br(),
                                                 
                                                 h4("Two-words Combination"),
                                                 plotOutput("themeplot2")),
                                        
                                        #tab 2 - word cloud  
                                        tabPanel("Word Cloud",
                                                 h4("Single Word"),
                                                 wordcloud2Output("themecloud"),
                                                 
                                                 br(),
                                                 
                                                 h4("Two-words Combination"),
                                                 wordcloud2Output("themecloud2")),
                                        
                                        #tab 3 - results table 
                                        tabPanel("Table",
                                                 h4("Single Word"),
                                                 tableOutput("themetable"),
                                                 
                                                 br(), 
                                                 
                                                 h4("Two-words Combination"),
                                                 tableOutput("themetable2"))
                                      ) #end tabset panel
                            ) #end main panel
                          )#end sidebar layout
                 ), #end tab panel
                 
                 
                 
                 
                 
                 
                 
                 
                 #--------------------------- Page 2 Layout -----------------------------           
                 
                 tabPanel("Topics, Themes & Clusters",
                          
                          tabsetPanel( #define all the tabsets
                            
                            #-----# tab 1 - Question 2 #-----#     
                            tabPanel("Topics", #define individual tabs
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         
                                         ##set slider color = red2
                                         setSliderColor(color="red", sliderId =1),
                                         #slider for number of words
                                         sliderInput("wordslider", "Number of Words:",
                                                     min = 5, max = 30, value = 11 ),
                                         
                                         
                                       ), #end sidebar panel
                                       
                                       mainPanel(align="center",
                                                 h4("Terms Occurrences by Topic"),
                                                 
                                                 #add button for scaling
                                                 actionButton("scalefix", 
                                                              "X Scale Toggle (Fixed/Free)"),
                                                 br(),
                                                 
                                                 uiOutput("topicplot")
                                       ) #end main panel
                                     ) #end sidebar layout
                            ), #end tab panel 1 - question 2
                            
                            
                            
                            #-----# tab 2 - Question 3 #-----#   
                            
                            tabPanel("Themes",
                                     #please put your question 3 code below
                                     
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         
                                         pickerInput(
                                           inputId = "topicpicker",
                                           label = "Choose a Topic", 
                                           choices = c(unique(na.omit(d4$topic_name))),
                                           options = list(
                                             style = "btn-danger")
                                         ), #end picker input
                                         
                                         
                                         #disclaimer
                                         h6("(Electives are 1-4 credit courses that
                                   allow students to fill credit requirements)"),
                                         
                                         br(), #line break
                                         
                                         #numeric input for word n selection
                                         numericInput("topic_n",
                                                      "Top Number of Words",value = 10) #end numeric input
                                         
                                       ), #end sidebar panel
                                       
                                       mainPanel(align="center",
                                                 
                                                 h4("Theme Occurrences by Topic"),
                                                 
                                                 helpText("(Words in graphic may take a short while to populate)"),
                                                 
                                                 #switch for grams
                                                 switchInput(
                                                   inputId = "gramtogg",
                                                   label="Text",
                                                   onLabel = "2-gram",
                                                   offLabel = "1-gram",
                                                   onStatus = "danger"
                                                 ), #end switch input
                                                 
                                                 uiOutput("thematiccloud")
                                                 
                                       )#end main panel
                                       
                                     ) #end sidebar layout
                                     
                            ), #end of tab panel 2 - question 3 
                            
                            
                            
                            
                            #-----# tab 3 - Question 4 #-----#   
                            
                            tabPanel("Clusters",
                                     
                                     
                                     navlistPanel(
                                       
                                       "Cluster Identification",
                                       
                                       tabPanel("Silhoutte Score",
                                                
                                                
                                                helpText("(Plot may take a short while to appear)"),
                                                
                                                br(), #line break
                                                
                                                mainPanel(align="center",
                                                          
                                                          plotOutput("sil_plot", width = "150%")
                                                          
                                                )#end main panel
                                                
                                       ), #end of Silhoutte Score tab
                                       
                                       
                                       tabPanel("Within-cluster Sum of Squares",
                                                
                                                
                                                helpText("(Plot may take a short while to appear)"),
                                                
                                                br(), #line break
                                                
                                                mainPanel(align="center",
                                                          
                                                          plotOutput("wss_plot", width="150%")
                                                          
                                                )#end main panel tab
                                       ), #end of WSS score tab
                                       
                                       
                                       "Plotting Average Positivity Scores",
                                       
                                       tabPanel("2 Clusters",
                                                
                                                br(), #line break
                                                
                                                mainPanel(align="center",
                                                          
                                                          br(), #line break
                                                          
                                                          plotOutput("k2_avg_plot", 
                                                                     width="150%")
                                                )#end of main panel
                                       ), #end of K=2 plot
                                       
                                       
                                       
                                       tabPanel("3 Clusters",
                                                
                                                br(), #line break
                                                
                                                mainPanel(align="center",
                                                          
                                                          br(), #line break
                                                          
                                                          plotOutput("k3_avg_plot", 
                                                                     width="150%")
                                                )#end main panel
                                       ) #end k=3 plot
                                       
                                     ) #end navlist panel
                                     
                                     
                                     #please keep your Q4 code above this line         
                            ) #end of tab panel 3 - question 4
                            
                          ) #end tabset panel        
                 ), #end of Topics, Themes, Clusters tab panel
                 
                 
                 #--------------------------- Page 3 Layout -----------------------------    
                 
                 tabPanel("Profile of Topics",         
                          #code for page 3 below 
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              #button for cluster number
                              
                              radioGroupButtons(
                                inputId = "profile_choice",
                                label = "Show Plot For",
                                choices = c("2 Clusters", 
                                            "3 Clusters"),
                                justified = TRUE,
                                status="danger"
                              ) #end button
                              
                            ), #end sidebar panel
                            
                            mainPanel(align="center",
                                      
                                      h3("Prevalence of Topics in Identified Clusters"),
                                      
                                      br(),
                                      
                                      plotOutput("profile_plot",
                                                 width="100%"),
                                      
                                      br(),
                                      
                                      tableOutput("topic_ref")
                                      
                                      
                            ) #end main panel
                            
                          ) #end sidebar layout
                          
                          
                          #please do not change this part, just put your codes above please           
                 ) #end of Profile of Topics tab panel
) #end of navbar page & also end of UI







#--------------------------- CREATING SERVER INPUTS -----------------------------

server <- function(input, output) {
  
  
  
  #--------------------------- Page 1 Inputs -----------------------------
  
  #creating plot 1
  output$themeplot <- renderPlot({
    filtered_data <- d4  %>% ungroup()%>% filter(major_type %in% input$majortype &
                                                   Level_Type %in% input$leveltype)
    
    #ggplot for overall theme exploration (single word count)    
    to_plot<-filtered_data%>% 
      ungroup()%>%
      count(word)%>%
      arrange(-n)%>%
      slice_max(n,n=input$top_n)
    ggplot(to_plot,aes(y=reorder(word,n,function(x) x),x=n))+
      geom_col(fill = "orange")+
      labs(y="Word", x="Count of Word")
    
  })
  
  #create table for plot 1
  output$themetable <- renderTable({
    filtered_table<-d4 %>%
      ungroup() %>% 
      filter(major_type %in% input$majortype &
               Level_Type %in% input$leveltype)
    
    
    to_plot<-filtered_table %>% 
      ungroup() %>%
      count(word) %>%
      arrange(-n) %>%
      slice_max(n,n=input$top_n) %>%
      rename(Word=word, Count=n)
    
    to_plot})
  
  
  #creating plot 2
  output$themeplot2 <- renderPlot({
    filtered_data2 <- d5  %>% 
      ungroup() %>% 
      filter(major_type %in% input$majortype &
               Level_Type %in% input$leveltype)
    
    #ggplot for overall theme exploration (two word count)    
    to_plot<-filtered_data2 %>% 
      ungroup() %>%
      count(gram1,gram2) %>%
      unite("word",gram1:gram2,sep = " ") %>%
      arrange(-n)%>%
      slice_max(n,n=input$top_n)
    ggplot(to_plot,aes(y=reorder(word,n,function(x) x),x=n))+
      geom_col(fill = "orange")+
      labs(y="Word Combination", x="Count")
    
  })
  
  #create table for plot 2
  output$themetable2 <- renderTable({
    filtered_data2 <-d5 %>%
      ungroup() %>% 
      filter(major_type %in% input$majortype &
               Level_Type %in% input$leveltype)
    
    
    to_plot<- filtered_data2 %>% 
      ungroup() %>%
      count(gram1,gram2) %>%
      unite("word",gram1:gram2,sep = " ") %>%
      arrange(-n) %>%
      slice_max(n,n=input$top_n) %>%
      rename(Word_Combination=word, Count=n)
    
    to_plot})
  
  
  #word cloud 1
  output$themecloud <- renderWordcloud2({
    filtered_data <- d4  %>% 
      ungroup()%>% 
      filter(major_type %in% input$majortype &
               Level_Type %in% input$leveltype)
    
    #cloud
    to_plot<-filtered_data %>% 
      ungroup()%>%
      count(word)%>%
      arrange(-n)%>%
      slice_max(n,n=input$top_n) %>%
      rename(freq=n) %>%
      wordcloud2(., size=0.5)
    
  }) 
  
  
  #word cloud 2
  output$themecloud2 <- renderWordcloud2({
    filtered_data2 <-d5 %>%
      ungroup() %>% 
      filter(major_type %in% input$majortype &
               Level_Type %in% input$leveltype)
    
    #cloud
    to_plot<-filtered_data2 %>% 
      ungroup() %>%
      count(gram1,gram2) %>%
      unite("word",gram1:gram2,sep = "_") %>%
      arrange(-n) %>%
      slice_max(n,n=input$top_n) %>%
      rename(freq=n) %>%
      wordcloud2(., size=0.5)
    
  })
  
  
  
  
  
  
  #--------------------------- Page 2 Inputs -----------------------------
  
  #QUESTION 2 - TOPIC ANALYSIS
  
  showPlot <- reactiveVal(TRUE)
  
  observeEvent(input$scalefix, {
    showPlot(!showPlot())
  })
  
  
  
  output$topicplot <- renderUI({
    if (showPlot()){
      
      renderPlot({
        
        topic_data<-d3%>%
          group_by(topic)%>%
          slice_max(beta,n=input$wordslider)%>%
          ungroup()%>%
          mutate(term = reorder_within(term,beta, topic))
        
        #plot
        ggplot(topic_data,aes(y = term, x = beta))+
          geom_col(fill="orange")+
          scale_y_reordered()+
          labs(y = "Term", x = "Beta")+
          ggtitle("Fixed X Scale (for Between-Plots Comparison) \n", )+
          facet_wrap(~topic, scales="free_y", 
                     labeller = as_labeller(topic_labels),
                     ncol=3,
          )+
          #change the size of x ticks
          theme(axis.text=element_text(size=8),
                axis.title= element_text(size=15),
                strip.text = element_text(size=12),
                plot.title=element_text(hjust=0.5, size=15, face="bold"))
        
      }, height=1200, width=900 ) #end plot1
    } #end if clause
    
    else{
      renderPlot({
        
        topic_data<-d3%>%
          group_by(topic)%>%
          slice_max(beta,n=input$wordslider)%>%
          ungroup()%>%
          mutate(term = reorder_within(term,beta, topic))
        
        #plot
        ggplot(topic_data,aes(y = term, x = beta))+
          geom_col(fill="orange")+
          scale_y_reordered()+
          labs(y = "Term", x = "Beta")+
          ggtitle("Free X Scale (For Within-Plot Comparison) \n")+
          facet_wrap(~topic, scales="free",
                     labeller = as_labeller(topic_labels),
                     ncol=3,
          )+
          #change the size of x ticks
          theme(axis.text=element_text(size=8),
                axis.title= element_text(size=15),
                strip.text = element_text(size=12),
                plot.title = element_text(hjust=0.5, size=15, face="bold"))
        
      }, height=1200, width=900 ) #end plot2
    } #end else clause
  }) #end renderUI for topic plot
  
  
  
  # QUESTION 3 - THEME ANALYSIS
  
  
  showPlot2 <- reactiveVal(TRUE)
  
  observeEvent(input$gramtogg, {
    showPlot2(!showPlot2())
  })
  
  
  #1-gram output
  output$thematiccloud <- renderUI({
    
    if (showPlot2()){
      
      renderWordcloud2({
        
        chosen_topic <- input$topicpicker
        
        d5_temp<- d5%>%
          filter(topic_name %in% input$topicpicker)%>%
          count(gram1,gram2)%>%
          unite("word",gram1:gram2,sep = "_") %>%
          drop_na()%>%
          arrange(-n)%>%
          ungroup() %>%
          slice_max(n,n=input$topic_n)
        
        wordcloud2(d5_temp, size=0.5)
        
        
      })#end render word cloud 2 grams
      
      
      
    } #end if clause
    
    else {
      
      renderWordcloud2({
        
        chosen_topic <- input$topicpicker
        
        d4_temp<- d4%>%
          filter(topic_name %in% input$topicpicker)%>%
          count(word)%>%
          drop_na()%>%
          arrange(-n)%>%
          ungroup() %>%
          slice_max(n,n=input$topic_n)
        
        wordcloud2(d4_temp, size=0.5)
        
      }) #end 1-gram word cloud
      
    }#end else clause
    
  }) #end render UI for thematic cloud
  
  
  
  
  
  
  
  # QUESTION 4 - CLUSTERING
  #please put your codes for question 4 below
  
  
  # silhoutte plot
  output$sil_plot <- renderPlot({
    
    fviz_nbclust(data_scaled2, kmeans, method="silhouette")
    
  }) #end sil_plot
  
  
  #wss plot
  output$wss_plot <- renderPlot({
    
    fviz_nbclust(data_scaled2, kmeans, method="wss")
    
  }) #end wss_plot
  
  
  #k=2 average positivity plot
  
  output$k2_avg_plot <- renderPlot({
    
    mean_pos_score_k2 %>%
      ggplot(aes(x=k2_cluster, y=mean_pos_score, label=round(mean_pos_score,2) ))+
      geom_col(aes(fill=k2_cluster))+
      geom_text(nudge_y = 0.5)+
      labs(title="Average Positive Sentiment Score for K=2", x="Cluster Number",
           y="Score for Average Positivity") +
      theme(legend.position = "none")
    
    
  }) #end k=2 plot
  
  
  output$k3_avg_plot <- renderPlot({
    
    mean_pos_score_k3 %>%
      ggplot(aes(x=k3_cluster, y=mean_pos_score,label=round(mean_pos_score,2)))+
      geom_col(aes(fill=k3_cluster))+
      geom_text(nudge_y = 0.5)+
      labs(title="Average Positive Sentiment Score for K=3", x="Cluster Number",
           y="Score for Average Positivity")+
      theme(legend.position = "none")
    
  }) #end k=3 plot
  
  
  
  
  #--------------------------- Page 3 Inputs -----------------------------
  
  # QUESTION 5 - PROFILE OF TOPICS
  
  observe({
    
    observeEvent(input$profile_choice, {
      
      if (input$profile_choice=="2 Clusters") {
        
        #plot output for profiles of K=2
        
        output$profile_plot <- renderPlot({
          
          ggplot(d4_sample_plot_k2,
                 aes(x=k2_cluster,y=Probability, 
                     group = Topic,
                     fill=k2_cluster))+
            geom_col(position = position_dodge2(preserve = "total"))+
            geom_text(aes(label=Topic), position=position_dodge(width=0.9), vjust=-0.5)+
            labs(title="2 Clusters",
                 x="",y="Probability of Positive Sentiment")+
            theme(legend.position="none")
          
        }) #end render plot 1 (profiles for K=2)
        
      } #end if clause
      
      else if (input$profile_choice=="3 Clusters") {
        
        output$profile_plot <- renderPlot({
          
          ggplot(d4_sample_plot_k3 ,
                 aes(x=k3_cluster,y=Probability, 
                     group = Topic, 
                     fill=k3_cluster))+
            geom_col(position = position_dodge2(preserve = "total"))+
            geom_text(aes(label=Topic), position=position_dodge(width=0.9), vjust=-0.5)+
            labs(title="3 Clusters",
                 x="",y="Probability of Positive Sentiment")+
            theme(legend.position="none")
          
          
        }) #end render plot 2 (profiles for K=3)
        
      } #end else clause
      
    }) #end observe event
    
  }) #end observe
  
  
  #topic reference table
  
  output$topic_ref <- renderTable({
    
    my_table
    
  }) #end render table
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###################################################################################
  
  #please keep your code above this line (just add more space if you need, thanks!)  
} 
#--------------------------- RUNNING THE APP -----------------------------#

shinyApp(ui, server)
