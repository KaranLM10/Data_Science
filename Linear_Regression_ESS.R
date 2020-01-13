#Assignment 2 Of Intro to R
#Student Name : Karan Jadhav
#Student ID : R001883239

#Reading data into R.
library(readxl)
setwd("C:/Users/91993/Documents/R_language/Assignment2/")
Data <- read.csv("Assignment_2.csv", sep = ",", header = TRUE)
View(Data)


sum(is.na(Data))
sum(is.na(Data$pm2.5))#2067 NA values thats about 4.71% of the data

Data1 <- na.omit(Data)
sum(is.na(Data1))
View(Data1)
str(Data1)
unique(Data1$cbwd)
Data1$cbwd <- as.numeric(Data1$cbwd)
#unique(Data1$year)


#Data1 = cbind(Data,year_cat=replicate(1,Data$year))
#Data1$year_cat <- replace(Data1$year_cat,Data1$year_cat=="2010",1)
#Data1$year_cat <- replace(Data1$year_cat,Data1$year_cat=="2011",2)
#Data1$year_cat <- replace(Data1$year_cat,Data1$year_cat=="2012",3)
#Data1$year_cat <- replace(Data1$year_cat,Data1$year_cat=="2013",4)
#Data1$year_cat <- replace(Data1$year_cat,Data1$year_cat=="2014",5)

#Data1$year_cat <- as.numeric(Data1$year_cat)

#View(Data1)

#1 Linear Regression considering y or dependent variable as TEMP.
cor(Data1$TEMP,Data1$year)   
cor.test(Data1$TEMP,Data1$year)
model_temp_year <- lm(TEMP~year,data = Data1)
summary(model_temp_year)  #R-squared: 0.003098
AIC(model_temp_year)    #327111.8


cor(Data1$TEMP,Data1$month)
cor.test(Data1$TEMP,Data1$month)
model_temp_mnt <- lm(TEMP~month,data = Data1)
summary(model_temp_mnt)  #R-squared: 0.02963
AIC(model_temp_mnt)     #325985.3


cor(Data1$TEMP,Data1$day)
cor.test(Data1$TEMP,Data1$day)
model_temp_day <- lm(TEMP~day,data = Data1)
summary(model_temp_day)  #R-squared: 0.00005231
AIC(model_temp_day)     #327219.5


cor(Data1$TEMP,Data1$hour)
cor.test(Data1$TEMP,Data1$hour)
model_temp_hour <- lm(TEMP~hour,data = Data1)
summary(model_temp_hour)  #R-squared: 0.02233
AIC(model_temp_hour)    #326298.2


cor(Data1$TEMP,Data1$pm2.5)
cor.test(Data1$TEMP,Data1$pm2.5)
model_temp_pm <- lm(TEMP~pm2.5,data = Data1)
summary(model_temp_pm)  #R-squared: 0.008196
AIC(model_temp_pm)      #326897.7


cor(Data1$TEMP,Data1$DEWP)
cor.test(Data1$TEMP,Data1$DEWP)
model_temp_dewp <- lm(TEMP~DEWP,data = Data1)
summary(model_temp_dewp)  #R-squared: 0.6787
AIC(model_temp_dewp)    #279833.7


cor(Data1$TEMP,Data1$PRES)
cor.test(Data1$TEMP,Data1$PRES)
model_temp_pres <- lm(TEMP~PRES,data = Data1)
summary(model_temp_pres)  #R-squared: 0.68838
AIC(model_temp_pres)    #279167.3


model_temp_cbwd <- lm(TEMP~cbwd,data = Data1)
summary(model_temp_cbwd)  #R-squared: 0.1142
AIC(model_temp_cbwd)    #322179.6


cor(Data1$TEMP,Data1$Iws)
cor.test(Data1$TEMP,Data1$Iws)
model_temp_iws <- lm(TEMP~Iws,data = Data1)
summary(model_temp_iws)  #R-squared: 0.02238
AIC(model_temp_iws)     #326296


cor(Data1$TEMP,Data1$Is)
cor.test(Data1$TEMP,Data1$Is)
model_temp_is <- lm(TEMP~Is,data = Data1)
summary(model_temp_is)  #R-squared: 0.0008984
AIC(model_temp_is)      #326864.5


cor(Data1$TEMP,Data1$Ir)
cor.test(Data1$TEMP,Data1$Ir)
model_temp_ir <- lm(TEMP~Ir,data = Data1)
summary(model_temp_ir)  #R-squared: 0.002455
AIC(model_temp_ir)      #327138.7

#R-squared value of PRES & DEWP is higher which are 0.68838 & 0.6787 respectively.
#AIC of PRES & DEWP is lower which are 279167.3 & 279833.7 respectively, so the model accuracy is more in this case.
#Let's combine the 2 varuables together & check

model_temp_comb1 <- lm(TEMP~PRES+DEWP,data = Data1)
summary(model_temp_comb1)  #R-squared: 0.7664
AIC(model_temp_comb1)      #266518.9

#R-squared value of is higher in comparison to single variable.
#The combined model gives lower AIC in comparison to single variable. Let's try adding few more variables.

model_temp_comb2 <- lm(TEMP~PRES+DEWP+cbwd+month,data = Data1)
summary(model_temp_comb2)  #R-squared: 0.7739
AIC(model_temp_comb2)     #265171.3

#R-squared value is getting higher as we add multiple variables to the model.
#The AIC goes on decreasing we are adding variables in the model. Let's try & add all the variables.

model_temp_comb_final <- lm(TEMP~PRES+DEWP+cbwd+month+Iws+hour+Is+pm2.5+year+Ir+day+No,data = Data1)
summary(model_temp_comb_final)  #R-squared: 0.8436
AIC(model_temp_comb_final)    #249793.8


#We are received the maximum R-squared value with singificant relation between variables.
#The AIC is also lowest in the model.



#####################################################################

#2 Using Shiny App for Visualization of Scatter Plot,Box Plot & Histogram

library(shiny)
ShinyAPP_For_Plots <- fluidPage(
  
  titlePanel("Data1 Visualisation"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons("type", "Plot type:",
                   c("Scatter" = "scat",
                     "Boxplot" = "box",
                     "Histogram"="hist")),
      
      
      selectInput('x', 'Select X Variable', names(Data1), names(Data1)[[2]]),
      selectInput('y', 'Select Y Variable', names(Data1), names(Data1)[[1]]),
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plotxy")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)
server_Plots <- function(input, output) {
  
  output$plotxy <- renderPlot({
    
    if(input$type=="scat"){
      plot(Data1[input$x][[1]],Data1[input$y][[1]],
           xlab = input$x, ylab=input$y,
           main = paste("Scatterplot of", input$y, "vs", input$x))
    } 
    if(input$type=="box"){
      boxplot(
        as.formula(
          paste(Data1[input$y]," ~ ",Data1[input$x])),
        xlab = input$x, ylab=input$y,
        main = paste("Boxplot of", input$y, "vs", input$x) )
    }
    if(input$type=="hist"){
      hist(Data1[input$x][[1]], 
           xlab = input$x, 
           main = paste("Histogram of", input$x))
    }
    print(p)
  })
  output$table <- renderTable({
    matrix(c(Data1[input$x][[1]],Data1[input$y][[1]]), ncol = 2,
           dimnames = list(rownames(Data1),c(input$x,input$y)))
  }, rownames = TRUE)
  
}

shinyApp(ShinyAPP_For_Plots, server_Plots)


########################################################################

#3 Fitting linear model in the scatter plot of Shiny App

ShinyAPP_For_Plots_model <- fluidPage(
  
  titlePanel("Data Visualization"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons("type", "Plot type:",
                   c("Scatter" = "scat",
                     "Boxplot" = "box",
                     "Histogram"="hist")),
      
      
      selectInput('x', 'Select X Variable', names(Data1), names(Data1)[[2]]),
      selectInput('y', 'Select Y Variable', names(Data1), names(Data1)[[1]]),
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plotxy")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)
server_Plots_model <- function(input, output) {
  
  output$plotxy <- renderPlot({
    
    if(input$type=="scat"){
      model_lm <- lm(Data1[input$y][[1]]~Data1[input$x][[1]])
      plot(Data1[input$x][[1]],Data1[input$y][[1]],
           xlab = input$x, ylab=input$y,
           main = paste("Scatterplot of", input$y, "vs", input$x))
           abline(lm(Data1[input$y][[1]]~Data1[input$x][[1]]))
           legend("topleft",title="Intercept & Slope",ncol = 2L,
                  legend = c("Intercept","Slope",round(model_lm$coefficient[1],2),round(model_lm$coefficient[2],2)))
    } 
    if(input$type=="box"){
      boxplot(
        as.formula(
          paste(Data1[input$y]," ~ ",Data1[input$x])),
        xlab = input$x, ylab=input$y,
        main = paste("Boxplot of", input$y, "vs", input$x))
    }
    if(input$type=="hist"){
      hist(Data1[input$x][[1]], 
           xlab = input$x, 
           main = paste("Histogram of", input$x))
    }
    print(p)
  })
  output$table <- renderTable({
    model_lm <- lm(Data1[input$y][[1]]~Data1[input$x][[1]])
    a=matrix(c(model_lm$coefficients), ncol = 2,nrow = 2)
    colnames(a) <- c(model_lm$coefficients[1],model_lm$coefficients[2])
  })
}
#dimnames = list(rownames(model_lm$coefficient[1],model_lm$coefficient[2]),c(model_lm$coefficient[1],model_lm$coefficient[2])))
shinyApp(ShinyAPP_For_Plots_model, server_Plots_model)


#####################################################################################

#4 Monte Carlo

model_check = lm(TEMP ~ pm2.5+PRES+Is+Iws+DEWP+Ir ,data = Data1 )
summary(model_check)

#mean absolute deviation for first model
disp=mad(summary(lm(TEMP ~ pm2.5+PRES+Is+Iws+DEWP+Ir ,data = Data1 ))$resid)
disp

model_mcj=(summary(lm(TEMP ~ pm2.5+PRES+Is+Iws+DEWP+year ,data = Data1))$coef)
model_mcj

rss=sum((summary(lm(TEMP ~ pm2.5+PRES+Is+Iws+DEWP+Ir+year ,data = Data1))$resi)^2)

#mean absolute deviation for second model
disp1=mad(summary(lm(TEMP ~ pm2.5+PRES+Is+Iws+DEWP+Ir+year+day ,data = Data1 ))$resid)
disp1

#Model Coefficient for second model
model_mcjb=(summary(lm(TEMP ~ pm2.5+PRES+Is+Iws+DEWP+Ir+year+day ,data = Data1))$coef)
model_mcjb


#Creating model with random normalised values
ysimulation=NULL
for(i in 1:2500){
  ysimulation=cbind(ysimulation ,model_mcj[1] + model_mcj[2]*mean(Data1$pm2.5) + model_mcj[3]*mean(Data1$PRES) +  
               model_mcj[4]*mean(Data1$Is) + model_mcj[5]*mean(Data1$Iws) +model_mcj[6]*mean(Data1$DEWP)+
               model_mcj[7]*mean(Data1$year)+rnorm(1,0,2*disp) )
}

max(ysimulation)
min(ysimulation)
summary(ysimulation)
hist(ysimulation, xlim = c(-20,55))

#Comparing value based on original data
abline(v=mean(Data1$TEMP))

hist(Data1$TEMP, xlim = c(-15,40))

plot(Data1$TEMP)
plot(ysimulation[,])

#MC Simulation for first model
rsss=NULL
for(i in 1:500){
  ysimulation=model_mcj[1] + model_mcj[2]*Data1$pm2.5 + model_mcj[3]*Data1$PRES +  
    model_mcj[4]*(Data1$Is) + model_mcj[5]*(Data1$Iws) +model_mcj[6]*(Data1$DEWP)+
    model_mcj[7]*(Data1$year)+rnorm(length(Data1),0,disp)
  rsss=cbind(rsss,sum((lm(ysimulation~pm2.5+PRES+Is+Iws+DEWP+Ir,data=Data1)$resid)^2))
}

max(rsss)
min(rsss)
#simulated distribution for RSS
hist(rsss,xlim = c(150000,3500000))

#Comparing to the calculated Residual sum of square value
abline(v=rss)

#Accept Null hypothesis(Ho)

#Creating model with normalised error
ysimulation2=NULL
for(i in 1:2500){
  ysimulation2=cbind(ysimulation2 ,model_mcjb[1] + model_mcjb[2]*mean(Data1$pm2.5) + model_mcjb[3]*mean(Data1$PRES) +  
                model_mcjb[4]*mean(Data1$Is) + model_mcjb[5]*mean(Data1$Iws) +model_mcjb[6]*mean(Data1$DEWP)+
                model_mcjb[7]*mean(Data1$Ir)+model_mcjb[8]*mean(Data1$year)+model_mcjb[9]*mean(Data1$day)+
                  rnorm(1,0,2*disp1) )
  
}


max(ysimulation2)
min(ysimulation2)
summary(ysimulation2)
hist(ysimulation2, xlim = c(-40,50))

#Comparing value based on original data
abline(v=mean(Data1$TEMP))

hist(Data1$TEMP, xlim = c(-15,40))

plot(Data1$TEMP)
plot(ysimulation2[,])


#MC Simulation for second model
rsss=NULL
for(i in 1:500){
  ysimulation2=model_mcjb[1] + model_mcjb[2]*Data1$pm2.5 + model_mcjb[3]*Data1$PRES +  
    model_mcjb[4]*(Data1$Is) + model_mcjb[5]*(Data1$Iws) +model_mcjb[6]*(Data1$DEWP)+
    model_mcjb[7]*(Data1$Ir)+model_mcjb[8]*(Data1$year)+model_mcjb[9]*(Data1$day)+rnorm(length(Data1),0,disp1)
  rsss=cbind(rsss,sum((lm(ysimulation2~pm2.5+PRES+Is+Iws+DEWP+Ir+year+day,data=Data1)$resid)^2))
}

max(rsss)
min(rsss)
rsss
#simulated distribution for RSS
hist(rsss,xlim = c(10000,3000000))
#compare to calcualted value
abline(v=rss)


#Accept Null hypothesis(Ho)


summary(ysimulation2)
hist(ysimulation2, xlim = c(-15,40))

#Comapring data with mean value of original data
abline(v=mean(Data1$TEMP))

hist(data$TEMP, xlim = c(-15,40))

plot(Data1$TEMP)
plot(ysimulation2[,])

#model 1(model_mcj) is better
#####################################################################################

#5 ESS Statistics

cor(Data1$TEMP,Data1$PRES)
cor.test(Data1$TEMP,Data1$PRES)
model_tr <- lm(TEMP~PRES,data = Data1)
summary(model_tr)  #R-squared: 0.6838
AIC(model_tr)      #279167.3

model_trw <- lm(TEMP~PRES+Iws,data = Data1)
summary(model_trw)  #R-squared: 0.6838
AIC(model_trw)      #279168.9


ss1 <- sum((model_tr$residuals)^2)

ss2 <- sum((model_trw$residuals)^2)


DF1 <-  model_tr$df.residual
DF2 <-  model_trw$df.residual

ss <- (ss1-ss2)/ss2
DF <- (DF1-DF2)/DF2

ss <- round(ss,2)
DF <- round(DF,2)

if(ss==DF){
  print("Simple Model i.e TEMP~PRES is correct")
} else
{
  print("Complex Model i.e TEMP~PRES+Iws is correct")
}

#Note: If the sum of squares value between both models is greater than the degree of freedom between both the models,
      #we can use F ratio. If F-ratio is to 1, we can conclude Simpler model is correct else the complex model is correct.
#Creating model with normalised error

#Carrying out Simulation using Monte Carlo

model_best=(summary(lm(TEMP~PRES,data = Data1))$coef)
model_best

rss_best=sum((summary(lm(TEMP~PRES,data = Data1))$resi)^2)

#mean absolute deviation for best model
disp_best=mad(summary(lm(TEMP~PRES,data = Data1))$resid)
disp_best

ysimulation3=NULL
for(i in 1:2500){
  ysimulation3=cbind(ysimulation3 ,model_mcjb[1] + model_mcjb[2]*mean(Data1$PRES)+rnorm(1,0,2*disp_best) )
}


max(ysimulation3)
min(ysimulation3)
summary(ysimulation3)
hist(ysimulation3, xlim = c(-230,-310))

#Comparing value based on original data
abline(v=mean(Data1$TEMP))


plot(ysimulation3[,])


#MC Simulation for best model
rsss_best=NULL
for(i in 1:500){
  ysimulation3=model_mcjb[1] + model_mcjb[2]*Data1$PRES+rnorm(length(Data1),0,disp_best)
  rsss_best=cbind(rsss_best,sum((lm(ysimulation3~PRES,data=Data1)$resid)^2))
}

max(rsss_best)
min(rsss_best)
rsss_best
#simulated distribution for RSS
hist(rsss_best,xlim = c(400000,5000000))
#compare to calcualted value
abline(v=rss_best)


#Accept Null hypothesis 