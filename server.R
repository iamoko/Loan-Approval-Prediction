library(shiny)
library(shinydashboard)
library(DBI)
library(RMySQL)
library(ggplot2)
library(plyr)
library(rpart)
library(car)
library(dplyr)
library(knitr)
library(markdown)
library(rmarkdown)

train<-read.csv(file="Datasets/train.csv",na.strings = c(""," ",NA))
test<-read.csv(file="Datasets/test.csv",na.strings = c(""," ",NA))

shinyServer(function(input, output,session){

  #Identify the client username so as to retrieve the client id displayed in dashboard and used in final report
  Client_username <- reactiveValues(Name = "admin")
  
  #Read and store the client ID on identifying the client in the database
  Client_ID <- reactiveValues(ID = "admin")
  
  #Check user login 
  USER <- reactiveValues(Logged = FALSE)
  
  #Determin the type of user either admin or client for different dashboards
  TYPE <- reactiveValues(Admin = FALSE)
  
  #check if the form details have been entered to switch interface
  ENTERED <- reactiveValues(entered = FALSE)
  
  #Continue form submission
  CONT <- reactiveValues(cont = FALSE)
  
  #User data entry check up to provide user with output
  observeEvent(input$submit, {
    if(input$gender!="" && input$dep!="" && input$married!="" && input$educ!="" && input$emp!="" && input$inc!="" && input$ch!="" && input$coinc!="" && input$lamt!="" && input$lamtT!="" && input$prop!=""){
      ENTERED$entered = TRUE 
    }
    
  })

   observeEvent(input$clear, {
    ENTERED$entered = FALSE
    output$submission <- renderText("All has been cleared")
    })
  #Deteremine the type of user
  user_type = FALSE
  
  observeEvent(input$login, {
    
    if(input$username!="" && input$password!=""){

      #Establish a connection to the mysql
      con <-dbConnect(MySQL(),
                      dbname="recess",
                      host="localhost",
                      user="root",
                      password="")
      res <- dbGetQuery(con,"select *from login")
      
      for (row in 1:nrow(res)){
        username <- res[row,'username']
        passwo <- res[row,'password']
        utype <- res[row,'user_type']
        
        #For the admin User identification
        if(input$username == username && input$password ==passwo) {
          
          TYPE$Admin <- ifelse(utype == 'admin', TRUE, FALSE)
          Client_username$Name <- username
          USER$Logged <- TRUE
          dbDisconnect(con)
          
        }
        else{
          output$message = renderText("Invalid user name or password,please try again!")
          show("message")
          
        }
      } 
    }
    else{
      output$message = renderText("Enter user name or password,please!!!")
      show("message")
    }
    
  })
  

  #Get action for logout button
  observeEvent(input$logout,{
    USER$Logged<-FALSE
  })


  #Render the whole shiny user interface
  output$body <- renderUI(

    #User Login Form if login is false

    if (!isTRUE(USER$Logged)) {
      fluidRow(column(width=6, offset = 3,
                      wellPanel(id = 'panel_login',
                                textInput('username', 'Username:'),
                                passwordInput('password', 'Password:'),
                                div(actionButton('login', 'Log in'), style='text-align: center;')
                      ),
                      textOutput("message")
      ))
    } 
    else {

      #Body if Login is True and user is admin

      if(isTRUE(TYPE$Admin)){
        
        tabItems(
          tabItem(
            tabName = 'Dashboard',
            fluidPage(
              fluidRow(
                h2('Train Dataset Info', align='center'),
                br()
              ),
              fluidRow(
                valueBoxOutput('applicants'),
                valueBoxOutput('approved'),
                valueBoxOutput('rejected')
              )
            )
          ),
          tabItem(
            tabName = 'data',
            fluidPage(
              h2("Table data"),
              tabsetPanel(
                type='tab',
                tabPanel("Applicant's Information", DT::dataTableOutput("mytable1")),
                tabPanel("More Information", DT::dataTableOutput("mytable2"))
              )
            )
          ),
          tabItem(
            tabName = 'anl',
            fluidPage(
              h2("Analysis"),
              tabsetPanel(
                tabPanel("summary", verbatimTextOutput('summary')),
                tabPanel("Structure", verbatimTextOutput('structure')),
                tabPanel('Missing Values', verbatimTextOutput('missing'))
              )
            )
            
          )
          ,tabItem(
            tabName = 'pred',
            h2("Predictor Variables Distibution", align='center'),
            p(''),
            fluidPage(
              tabsetPanel(type = "tabs",
                          
                          tabPanel("Response Variable",h4('Loan Status in the dataset'),plotOutput('loaned')),
                          tabPanel("Gender ",h4('Gender factor with two levels.Has na\'s in both train and test sets.'),verbatimTextOutput('gender'), plotOutput('gen')),
                          tabPanel('Married', h4('Married factor with two levels. Has na\'s only in test set.'),verbatimTextOutput('married'), plotOutput('mar')),
                          tabPanel('Dependents',h4('Dependents - factor with 4 levels. Has na\'s in both sets'),verbatimTextOutput('dependents'), plotOutput('depend')),
                          tabPanel("Education",h4('Education is a factor with 2 levels. There are no missing values.'),verbatimTextOutput('education'), plotOutput('educ')),
                          tabPanel("Self-Employed",h4('Self-Employed -factor with two levels. Has missing values in both sets.'),verbatimTextOutput('self_employed'), plotOutput('empl')),
                          tabPanel("Applicant and CoApplicant Income",h4('Applicant Income and CoApplicant Income. Both numeric variables. No na\'s.'), plotOutput('appinc')),
                          tabPanel('Loan Amoun',h4('Loan Amount - numeric. Has na\'s in both sets'), plotOutput('lnamt')),
                          tabPanel('Loan Amount Term',h4('Loan amount term - numeric. Both sets have na\'s'),verbatimTextOutput('loanamountterm'), plotOutput('lnamtT')),
                          tabPanel('Credit History',h4('Credit History - integer. This should actually be a factor variable. Both sets have na\'s'),verbatimTextOutput('credithistory'), plotOutput('cred')),
                          tabPanel('Property Area',h4('property Area - factor with three levels. No missing values.'),verbatimTextOutput('propertyarea'), plotOutput('prop'))
                          
              )
            )
            
          ),

          tabItem(
            tabName = 'loan_status',
            fluidPage(
              h2("Loan status"),
              fluidRow(
                tabBox(
                  tabPanel(
                    title='Selection Tab',
                    selectInput(
                      inputId='histogram', 'Histogram of Loan_Status against', choices = c('Gender','Married','ApplicantIncome','Self_Employed','Loan_Amount_Term','Credit_History','LoanAmount','Property_Area','CoapplicantIncome','Education','Dependents')
                    )
                  )
                )
              ),
              fluidPage(
                h4(textOutput('head')),
                tabsetPanel(type = "tabs",
                            tabPanel("Plot", plotOutput("ggplots"))
                )
              )
            )
          ),
          
          tabItem(
            tabName = 'analysis',
            fluidPage(
              tabsetPanel(
                tabPanel("Complete Model Results", verbatimTextOutput('model1')),
                tabPanel("Logistic Regression Results", verbatimTextOutput('model')),
                tabPanel("Model Accuracy", verbatimTextOutput('accuracy'))
              )
            )
            
          ),
          tabItem(
            tabName = 'box',
            fluidPage(
              h2("Table data"),
              tabsetPanel(
                type='tab',
                tabPanel("Client Details", DT::dataTableOutput("mytable3")),
                tabPanel("More Client Details", DT::dataTableOutput("mytable4"))
              )
            )
            
            
          ),
          tabItem(
            tabName = 'clients_rest',
            fluidPage(
              h2("Client Applications"),
              tabsetPanel(
                type='tab',
                tabPanel("Client Application Details", DT::dataTableOutput("mytable5")),
                tabPanel("More Client Application Details", DT::dataTableOutput("mytable6"))
              )
            )
          ),
          tabItem(
            tabName = 'clients_preds',
            fluidPage(
              h2("Client Predictions"),
              tabsetPanel(
                type='tab',
                tabPanel("Results of Prediction", DT::dataTableOutput("mytable7"))
              )
            )
          )
        )
        
        #Client User Body
      }else{
        
        #Body if Login is True and user is Client
        tabItems(
          tabItem(tabName = 'entry',
                  fluidRow(column(width=8, offset=2,
                                  uiOutput('client_form')
                                  
                  )
                  )
                  ),
          tabItem(tabName = 'rest',
            uiOutput('resultsonly')
          )
        )
      }
    } 
  )

  #Side Panel

  output$sides <- renderUI({

    #Side Panel for Admin

    if (isTRUE(USER$Logged) && isTRUE(TYPE$Admin)) {
      sidebarMenu(
        menuItem('Welcome Admin', icon = icon('user-circle')),
        menuItem('Dashboard', tabName = 'Dashboard', icon = icon('dashboard')),
        menuItem('Train Dataset', tabName = 'data', icon = icon('database')),
        menuItem('Analysis', tabName = 'anl', icon = icon('chart-line')),
        menuItem('Predictors Distribution', tabName = 'pred', icon = icon('angular')),
        menuItem('Loan Status Against Other Variables', tabName = 'loan_status', icon = icon('bar-chart')),
        
        menuItem('Detailed Analysis and Model', tabName = 'analysis', icon = icon('align-center')),
        menuItem('Test Dataset Predictions', tabName = 'box', icon=icon('arrow-down')),
        menuItem('Client Applications', tabName = 'clients_rest', icon=icon('app-store')),
        menuItem('Client Predictions', tabName = 'clients_preds', icon=icon('arrow-circle-down')),
        actionButton("logout","LOGOUT")
      )
    }else if(isTRUE(USER$Logged) && !isTRUE(TYPE$Admin)){

      #Side Panel for Client

      sidebarMenu(
        menuItem('Entry Form', tabName = 'entry', icon = icon('centercode')),
        menuItem('Result', tabName = 'rest', icon = icon('centercode')),
        actionButton("logout","LOGOUT")
      )
    }
  })
  
  
  
  #Admin Dashboard

  output$applicants <- renderValueBox({
    valueBox(nrow(train), 'Loan Applications',icon=icon('bar-chart'))
  })
  output$approved <- renderValueBox({
    appro <- subset(train, train$Loan_Status == 'Y')
    valueBox(nrow(appro), 'Approved Applications',icon=icon('thumbs-up'), color = 'green')
  })
  output$rejected <- renderValueBox({
    appro <- subset(train, train$Loan_Status != 'Y')
    valueBox(nrow(appro), 'Rejected Applications',icon=icon('warning'), color = 'red')
  })
  
  
  #Admin Applicant's Data

  output$mytable1 <- DT::renderDataTable({
    DT::datatable(train[,c(1,2,3,4,5,6,7)])
  })
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(train[,-c(2,3,4,5,6,7)])
  })
  
  
  #Analysis Data

  output$summary <- renderPrint({
    summary(train)
  }) 
  output$structure <- renderPrint(
    str(train)
  )
  output$missing <- renderPrint({
    sapply(train, function(x) sum(is.na(x)))
  })


  #Predictor Variables

  output$loaned <- renderPlot({
    barplot(table(train$Loan_Status))
  })
  
  output$gender <- renderPrint({
    prop.table(table(train$Gender))
  }) 
  output$gen <- renderPlot({
    par(mfrow=c(1,2))
    barplot(table(train$Gender),main="train set")
    barplot(table(test$Gender),main="test set")
  })
  
  output$married <- renderPrint({
    prop.table(table(train$Married))
  })
  output$mar <- renderPlot({
    par(mfrow=c(1,2))
    barplot(table(train$Married),main="train set")
    barplot(table(test$Married),main="test set")
  })
  
  output$education <- renderPrint({
    prop.table(table(train$Education))
  })
  output$educ <- renderPlot({
    par(mfrow=c(1,2))
    barplot(table(train$Education),main="train set")
    barplot(table(test$Education),main="test set")
  })
  
  output$dependents <- renderPrint({
    prop.table(table(train$Dependents))
  })
  output$depend <- renderPlot({
    par(mfrow=c(1,2))
    barplot(table(train$Dependents),main="train set")
    barplot(table(test$Dependents),main="test set")
  })
  
  output$self_employed <- renderPrint({
    prop.table(table(train$Self_Employed))
  })
  output$empl <- renderPlot({
    par(mfrow=c(1,2))
    barplot(table(train$Self_Employed),main="train set")
    barplot(table(test$Self_Employed),main="test set")
  })
  output$appinc <- renderPlot({
    par(mfrow=c(1,2))
    boxplot(train$ApplicantIncome,train$CoapplicantIncome,names=c("App Income","Coapp Income"),main="train set")
    boxplot(test$ApplicantIncome,test$CoapplicantIncome,names=c("App Income","Coapp Income"),main="test set")
  })
  output$lnamt <- renderPlot({
    par(mfrow=c(1,2))
    boxplot(train$LoanAmount,main="train set")
    boxplot(test$LoanAmount,main="test set")
  })
  
  output$loanamountterm <- renderPrint({
    summary(train$Loan_Amount_Term)
  })
  output$lnamtT <- renderPlot({
    par(mfrow=c(1,2))
    hist(train$Loan_Amount_Term,breaks=500,main="train set")
    hist(test$Loan_Amount_Term,breaks=500,main="test set")
  })
  
  output$credithistory <- renderPrint({
    prop.table(table(train$Credit_History))
  }) 
  output$cred <- renderPlot({
    par(mfrow=c(1,2))
    train$Credit_History <-as.factor(train$Credit_History)
    test$Credit_History <- as.factor(test$Credit_History)
    barplot(table(train$Credit_History),main="train set")
    barplot(table(test$Credit_History),main="test set")
  })
  
  output$propertyarea <- renderPrint({
    prop.table(table(train$Property_Area))
  }) 
  output$prop <- renderPlot({
    par(mfrow=c(1,2))
    barplot(table(train$Property_Area),main="train set")
    barplot(table(test$Property_Area),main="test set")
  })
  
  
  
  
  #Loan Status against other variables

  output$ggplots <- renderPlot({
    if(input$histogram == 'Gender'){
      output$head <- renderText({
        paste('Male applicant has higher loan approval and rejection count than feamle applicant')
      })
      print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Gender)+ggtitle("Loan Status by Gender of Applicant"))
    }
    else if(input$histogram == 'Married'){
      output$head <- renderText({
        paste('A larger proportion of not married applicants are refused than married ones')
      })
      print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Married)+ggtitle("Loan Status by Marital Status of Applicant"))
    }
    else if(input$histogram == 'Dependents'){
      output$head <- renderText({
        paste('A smaller proportion of applicants with 2 dependents is refused than other numbers')
      })
      print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Dependents)+ggtitle("Loan Status by number of Dependents of Applicant"))
    }
    else if(input$histogram == 'ApplicantIncome'){
      output$head <- renderText({
        paste('Shows that low income people are mainly applying for loans and number of loan rejection is more in the lowest income segment')
      })
      print(ggplot(train, aes(x=Loan_Status,y=ApplicantIncome))+geom_boxplot()+ggtitle("Loan Status by Applicant income"))
    }
    else if(input$histogram == 'Education'){
      output$head <- renderText({
        paste('A larger proportion on non graduates are refused than graduates')
      })
      print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Education)+ggtitle("Loan Status by Education of Applicant"))
      
    }
    else if(input$histogram == 'Self_Employed'){
      output$head <- renderText({
        paste('Not self employed seems to be slightly preferred')
      })
      print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Self_Employed)+ggtitle("Loan Status by Employment status of Applicant"))
      
    }
    else if(input$histogram == 'Loan_Amount_Term'){
      output$head <- renderText({
        paste('Difficult to see any patterns, most of the loans are for 360 months')
      })
      print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Loan_Amount_Term)+ggtitle("Loan Status by terms  of loan"))
      
    }
    
    else if(input$histogram == 'Credit_History'){
      output$head <- renderText({
        paste('Almost all applicants with history=0 are rejected')
      })
      print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Credit_History)+ggtitle("Loan Status by credit history of Applicant"))
    }  
    else if(input$histogram == 'Property_Area'){
      output$head <- renderText({
        paste('it\'s easiest to get a loan if the property is semi urban and hardest if it is rural')
      })
      print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Property_Area)+ggtitle("Loan Status by property area"))
    }
    else if(input$histogram == 'CoapplicantIncome'){
      output$head <- renderText({
        paste('Low income people are mainly applying for loans and number of loan rejection is more in the lowest income segment')
      })
      print(ggplot(train, aes(x=Loan_Status,y=CoapplicantIncome))+geom_boxplot()+ggtitle("Loan Status by coapplicant income"))
      
    }
    else if(input$histogram == 'LoanAmount'){
      output$head <- renderText({
        paste('The third quartile of the refused loans is higher')
      })
      print(ggplot(train, aes(x=Loan_Status,y=LoanAmount))+geom_boxplot()+ggtitle("Loan Status by Loan Amount"))
      
    }
  })
  

  #Model and Data tidying
  
  train$Credit_History <-as.factor(train$Credit_History)
  test$Credit_History <- as.factor(test$Credit_History)
  
  
  ##########################################################################################################################################
  
  ########################################### TIDYING THE DATA   ##################################################################
  
  alldata<-rbind(train[,2:12],test[,2:12])
  
  ############################################     MARRIED MISSING VALUES  ####################
  
  #Adding another column to dataframe for totalincome
  alldata2<-mutate(alldata,TotalIncome=ApplicantIncome+CoapplicantIncome)
  
  #Assuming that the institution coapplicants are most possible the wife or husband for marriage
  alldata2$Married[is.na(alldata2$Married) & alldata2$CoapplicantIncome==0]<-"No"
  alldata2$Married[is.na(alldata2$Married)]<- "Yes"
  
  
  
  ############################################     GENDER AND DEPENDENTS   #####################
  alldata2[is.na(alldata2$Gender) & is.na(alldata2$Dependents),]  #checking for missing both gender && dependents
  
  #solution for missing both by choice
  alldata2$Gender[is.na(alldata2$Gender) & is.na(alldata2$Dependents)] <- "Male"
  #print(ggplot(alldata2,aes(x=Dependents, fill=Gender)) + geom_bar() + facet_grid(.~Married))  
  
  
  
  ###     FROM THE PLOT     ###
  alldata2$Dependents[is.na(alldata2$Dependents) & alldata2$Married=="No"]<- "0"
  mm <- alldata2[(alldata2$Gender=="Male" & alldata2$Married=="Yes"),c(3,6:9,11)]
  mmtrain<-mm[!is.na(mm$Dependents),]
  mmtest<- mm[is.na(mm$Dependents),]
  
  depFit <- rpart(data=mmtrain,Dependents~.,xval=3)
  #accuracy
  p<-predict(depFit,mmtrain,type="class")
  acc=sum(p==mmtrain[,1])/length(p)
  
  ###########      MISSING DEPENDANTS VALUES FINALLY ################
  alldata2$Dependents[is.na(alldata2$Dependents) & alldata2$Gender=="Male" & alldata2$Married == "Yes"]<- predict(depFit,newdata=mmtest,type="class")
  #View(alldata2)
  
  
  ###########      MISSING GENDER    ###############################
  #Gender with no missing values
  gtrain<-alldata2[!is.na(alldata2$Gender),1:7]
  gtest<-alldata2[is.na(alldata2$Gender),1:7]
  genFit<-rpart(data=gtrain,Gender~.,xval=3)
  
  #Accuracy for Gender Prediction
  p<-predict(genFit,gtrain,type="class")
  acc<-sum(p==gtrain[,1])/length(p)
  
  ########### Filling in the missing gender values ################################

  alldata2$Gender[is.na(alldata2$Gender)]<-predict(genFit,gtest,type="class")

  ############################################    MISSING SELF EMPLOYED      ###################
  #since earlier, most are not self employed. These missing values will be imputed using the mode ="No"
  
  alldata2$Self_Employed[is.na(alldata$Self_Employed)] <- "No"

  ############################################    MISSING CREDIT HISTORY      #########################
  #nas in credithistory treated specially
  
  alldata2$Credit_History<-car::recode(alldata2$Credit_History,"NA=2")
  
  
  
  ############################################    MISSING LOAN AMOUNT         ####################
  ltrain<-alldata2[!is.na(alldata2$LoanAmount) & alldata2$LoanAmount<500,c(1:8,10)]
  ltest <- alldata2[is.na(alldata2$LoanAmount),c(1:8,10)]
  loanFit <- glm(data=ltrain,LoanAmount~.,na.action=na.exclude)
  #impute
  alldata2$LoanAmount[is.na(alldata2$LoanAmount)] <- predict(loanFit,newdata=ltest)
  
  
  ############################################    MISSING LOAN AMOUNT TERM      ##################
  alldata2$Loan_Amount_Term <- as.factor(alldata2$Loan_Amount_Term)
  #print(ggplot(data=alldata2,aes(x=Loan_Amount_Term))+geom_bar())
  alldata2$Loan_Amount_Term[is.na(alldata2$Loan_Amount_Term)]<-"360"  #bcoz its the mode
  alldata2$Loan_Amount_Term[alldata2$Loan_Amount_Term=='350']<-'360'
  alldata2$Loan_Amount_Term[alldata2$Loan_Amount_Term=='6']<-'60'
  #print(ggplot(data=alldata2,aes(x=Loan_Amount_Term))+geom_bar())
  
  
  ##################################################################################################################################################################
  
  newtrain <- cbind(Loan_Status=train$Loan_Status,alldata2[1:614,])
  
  
  #Dummy Loan status for test set
  Loan_Status <- as.factor(sample(c("N","Y"),replace=TRUE,size=dim(test)[1]))
  newtest <- cbind(alldata2[615:981,], Loan_Status)
  
  
  
  model1 <- glm(Loan_Status~., data=newtrain,family='binomial')
  
  model <- glm(Loan_Status~Married+Credit_History +Property_Area, data=newtrain,family='binomial')
  

    #Prediction
  p<-predict(model,newtest,type="response")
  Loan_Statuslog <- ifelse(p > 0.5, 1, 0)
  Loan_Status <- factor(Loan_Statuslog, levels=c(0, 1))
  Loan_Status <-ifelse(Loan_Status==1,'Y','N')
  
  acc=sum(Loan_Statuslog)/length(p)
  gettwe <- cbind(Loan_Status,test)
  
  
  
  

  
  
  
  
  
  #Detailed Analysis

  output$model <- renderPrint({
    summary(model)
  })
  output$model1 <- renderPrint({
    summary(model1)
  })
  output$accuracy <- renderPrint({
    paste(acc*100,'%')
  })
  
  


  #Prediction Results on Test Dataset

  output$mytable3 <- DT::renderDataTable({
    DT::datatable(gettwe[,1:7])
  })
  output$mytable4 <- DT::renderDataTable({
    DT::datatable(gettwe[,c(1,2,8:11,13)])
  })
  
  
  #Produce Output after prediction in Client
  headers <- train[-c(1:613),-c(1,4,13)]
  
  
  prediction <- function(headers, applicant_info,user_ID ){
    latest_app <- rbind(headers,applicant_info )
    latest_app$Property_AreaSemiurban_d=ifelse(latest_app$Property_Area=='Semiurban',1,0)
    prob_predlog = predict(model, type = 'response', newdata = latest_app[2,])
    Loan_Statuslog = ifelse(prob_predlog>0.5, 1, 0)
    Loan_Status = ifelse(Loan_Statuslog == 1, "Y","N")
 
    
    con <-dbConnect(MySQL(),
                    dbname="recess",
                    host="localhost",
                    user="root",
                    password="")
    predicts <- paste("INSERT INTO `loan`VALUES ('",user_ID,"','",latest_app[2,7],"'
               ,'",latest_app[2,8],"','",Loan_Status,"','",Sys.Date(),"')")
    dbGetQuery(con, predicts)
    dbDisconnect(con)
    print(Loan_Status)
  }
  
      
  output$resultsonly <- renderUI({
    
    #Reading the input content from application form
    
    if(isTRUE(CONT$cont)){
      user_ID <- Client_ID$ID
      applicant <- list(input$gender,input$married,input$educ,input$emp,input$inc,input$coinc,input$lamt,input$lamtT,input$ch,input$prop)
      if(prediction(headers, applicant,user_ID) == 'N'){
        fluidPage(
          h4('Your FeedBack', align='center'),
          tabsetPanel(
            tabItem(title = 'Results',
              br(),
              h4('Sorry, You are not eligible for a loan')
            )
          ) 
        )
      }else{
        fluidPage(
          h4('Your FeedBack', align='center'),
          tabsetPanel(
            tabItem(title = 'Results',
                    br(),
                    h5('Congratulations, You are eligble for a loan'),
                    br(),
                    h6('Download The report below'),
                    downloadButton('report')
            )
          )
         
        )
      }
      
    }else{
      fluidPage(
        h4("You haven\'t filled the form yet", align='center'),
        br(),
        h4('No results yet', align='center')
      )
    }
  })
  
  
  
  #Prediction Results on Clients from database

  

  con <-dbConnect(MySQL(),
                  dbname="recess",
                  host="localhost",
                  user="root",
                  password="")
  rests <-data.frame(dbGetQuery(con, paste("SELECT *FROM `loan`")))
  app_rests <-data.frame(dbGetQuery(con, paste("SELECT *FROM `loan_applicant`")))
  
  #usered <-data.frame(dbGetQuery(con, paste("SELECT `Client_ID` FROM `login` WHERE `username` = '",userNamed,"'")))
  
  dbDisconnect(con)
  output$mytable5 <- DT::renderDataTable({
    DT::datatable(app_rests[,1:6])
  })
  output$mytable6 <- DT::renderDataTable({
    DT::datatable(app_rests[,c(1,7:12)])
  })
  output$mytable7 <- DT::renderDataTable({
    DT::datatable(rests)
  })
  
  #Report Generator
  output$report = downloadHandler(
    filename = 'myreport.pdf',
    content = function(file) {
      out = knit2pdf('input.Rnw', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
    }
  )
  username <- 'people'
  user_client <- function(arg){
    con <-dbConnect(MySQL(),
                    dbname="recess",
                    host="localhost",
                    user="root",
                    password="")
    usered <-data.frame(dbGetQuery(con,paste("SELECT `Client_ID` FROM `login` WHERE `username` = '",arg,"'", sep = "") ))
    
    for (row in 1:nrow(usered)){
      Client_ID$ID <- usered[row,'Client_ID']
    }
    
    dbDisconnect(con)
  }
  account <- user_client('client')
  output$user_id <- renderText({
    user_client(Client_username$Name)
    Client_ID$ID
  })
  
  #Absolute panel fr confirmation
  output$panels <- renderUI({
    if(isTRUE(ENTERED$entered)){
      fluidRow(
        h4('Please Verify if the information below is correct and press \'Continue\' to proceed')
      )
    }
  })
  
  output$client_form <- renderUI({
    if(isTRUE(ENTERED$entered)){
      wellPanel(
        h3('Please Verify if the information below is correct and press \'Continue\' to proceed', align='center'),
        br(),br(),
        DT::dataTableOutput("verification_data"),
        div(actionButton('cont','Continue'),actionButton('back','Go Back'), style='text-align:center'),
        textOutput('submission')
      )
    }else{
      wellPanel(
        textOutput('user_id'),
        selectInput(inputId = 'gender',
                    label = "Select Gender" ,
                    choices = c('Male','Female'),
                    selected = 'Male'
        ),
        selectInput(inputId = 'married',
                    label = 'Married' ,
                    choices = c('Yes','No'),
                    selected = 'No'
        ),
        numericInput(inputId='dep',
                     label='Dependents',
                     value=0,
        ),
        
        selectInput(inputId ='educ' ,
                    label ='Education' ,
                    choices = c('Graduate', 'Not Graduate'),
                    selected = 'NotGraduate'
        ),
        selectInput(inputId = 'emp' ,
                    label = 'Self Employed' ,
                    choices = c('No', 'Yes'),
                    selected = 'No'
        ),
        numericInput(inputId='inc',
                     label='Applicant\'s Income',
                     value=0,
        ),
        numericInput(inputId='ch',
                     label='Credit History',
                     value=0,
        ),
        numericInput(inputId='coinc',
                     label='CoApplicant\'s Income',
                     value=0,
        ),
        numericInput(inputId='lamt',
                     label='Loan Amount',
                     value=0,
        ),
        numericInput(inputId='lamtT',
                     label='Loan Amount Term',
                     value=0,
        ),
        selectInput(inputId = 'prop',
                    label = 'Property Area',
                    choices = c('Urban','Semiurban', 'Rural'),
                    selected = 'Urban'
        ),
        actionButton(inputId = 'submit',
                     label = 'Submit Form'),
        actionButton(inputId = 'clear',
                     label = 'Reset Results')
      )
    }
  })
  
  
  #Continue to submit form
  observeEvent(input$cont, {
    CONT$cont <- TRUE
    con <-dbConnect(MySQL(),
                    dbname="recess",
                    host="localhost",
                    user="root",
                    password="")
    query <- paste("INSERT INTO `loan_applicant` VALUES ('",Sys.Date(),"','",Client_ID$ID,"','",input$inc,"','",input$coinc,"','",input$ch,"',
               '",input$educ,"','",input$gender,"','",input$lamt,"','",input$lamtT,"','",input$married,"','",input$dep,"','",input$prop,"','",input$emp,"')")
    
    dbGetQuery(con, query)
    dbDisconnect(con)
    output$submission <- renderText("Data Submitted , Check the results now")
  })
  observeEvent(input$back, {
    ENTERED$entered <- FALSE
  })
  
  #Verification Data table for the form
  
  output$verification_data <- DT::renderDataTable({
    DT::datatable(data.frame(cbind(c('Client ID','Gender','Married','Dependents','Education','Self Employed','Applicant\'s Income','Credit History','CoApplicant\'s Income','Loan Amount','Loan Amount Term','Property Area'), c(Client_ID$ID, input$gender, input$married, input$dep, input$educ, input$emp, input$inc, input$ch, input$coinc, input$lamt, input$lamtT, input$prop))))
  })


})
