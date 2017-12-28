
library(shiny)
library(sourcetools)




## Purpose: a Rshiny to help engage and reinforce confidence interval 
## Concepts. The App is composed of two sections, Shiny User Input
## (Part A) and Server (Part B). User input gets the information from
## the user and displays outut whereas the server does the calculation
## For use of editing a (C) is added to each line, so that code can be
## modiified for another dataset for more variables.
##
## For more information about R Shiny go to the following website
##  https://shiny.rstudio.com/tutorial/

###############
##  Part A: User Input
###############
# 1. Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  # 2. Application title
  titlePanel("NBA Stars First Year"),
  # 3. Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      ###############
      ## Note 1: Input Style
      ## Change the Input style at your own risk. This is set up
      ## this way so that students cannot put errorenous inputs
      ###############   
      
      
      ## 4. Changing the sample size can occur here, based on 
      ## input data, one shoudl change this according
      sliderInput("n",
                  "Sample Size (Number of Games):",
                  min = 1,     ## Minimum Sample Size (C)
                  max = 71,    ## Maximum Sample Size (C)
                  value = 50), ## Initial Value Sample Size (C)
      sliderInput("alpha",
                  "Level of Confidence:",
                  min = 0.1,    ## Minimum amount of confidence (C)
                  max = 1,      ## Maximum amount of confidence (C)
                  value = 0.95), ## Initial Value amount of confidence
      selectInput("stat",label=h5("Statistics"), ## Variable Choice, Change these
                  ## Variables according to data set (C)
                  choices = list("Points"  = 1, ## (C)
                                 "Assists" = 2, ## (C)
                                 "D. Rebounds" = 3, ## (C)
                                 "Steals" = 4 ## (C)
                  )
      ),
      ## 5. Button to display results. Should not have
      ## to change button to calculate
      actionButton("button","Calculate") 
    ),
    ## 6. Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

###############
##  Part B: Server
###############
server <- shinyServer(function(input, output) {
  inform <- eventReactive(input$button, {
    ##1) Gather Information for Input
    info_ss    <- as.numeric(input$n) ## Sample Size
    max_n=71                         ## Max Number of Games
    c(sample(max_n,info_ss) ) ## Sample of Numbers
    
  })
  
  
  ## 2) Gather info from user
  output$distPlot <- renderPlot({
    inform<-inform()
    ## 3) The dataset (C)
    data.nba <- read.csv("Combine_First_Year1.csv")
    
    ## 4) Only choose statistics: pts, ast, drb, stl 2 offensive and 2 defensive.
    ## Can choose more/different statistics from thi line (C)
    new_data <- subset(data.nba,select = c(PTS,AST,DRB,STL,Name))
    
    ###############  
    ## Note 2: Buttons for calculation
    ## Each statistics has a different button number it is important
    ## to note this.
    ## PTS: 1, AST: 2, DRB: 3, STL: 4
    ###############  
    
    ## 5) Gather the sample size, variable, confidence level from user
    ## and make characters numeric
    info_ss    <- as.numeric(input$n)      ## Sample Size   
    info_stat  <- as.numeric(input$stat)   ## Variable    
    info_alpha <- as.numeric(input$alpha)  ## Confidence level   
    
    ## 6) Globally define global variable maximum (C)
    max_n=71  
    
    ## 7) Rewrite variable inputs for better understanding
    st = info_stat; ns = info_ss; a = info_alpha; 
    
    ## 8) Critical Value calculation 
    t_cv = abs(qt((1-a)/2,ns-1))           
    
    
    ## 9) Obtain information about player and stat from dataset (C) variable
    ## names not methodology
    sample_parts <- inform
    mj_sb_data <- subset(new_data,new_data$Name=="MJ")[sample_parts,st]
    lj_sb_data <- subset(new_data,new_data$Name=="LJ")[sample_parts,st]
    kb_sb_data <- subset(new_data,new_data$Name=="KB")[sample_parts,st]
    sc_sb_data <- subset(new_data,new_data$Name=="SC")[sample_parts,st]
    ## 10) Calculation of each player's
    ## Mean; Standard Deviation; 
    ## Standard Error; Margin of Error 
    ## Lower Bound; Upper Bound for particular statistic
    ## Paste all information together 
    
    ## Michael Jordan Information
    mj_mean = mean(mj_sb_data);mj_sd = sd(mj_sb_data); 
    se_mj = mj_sd/sqrt(ns); me_mj = se_mj * t_cv
    
    
    
    lb_mj <- mj_mean - me_mj;ub_mj <- mj_mean + me_mj
    mj_text <- paste0("LB: ",round(lb_mj,2),
                      "| Mean: ",round(mj_mean,2),
                      "| UB: ",round(ub_mj,2),
                      "| SD: ",round(mj_sd,2),
                      "| SE: ",round(se_mj,2),"  ")
    px_mj_limits = c(lb_mj,ub_mj);
    lx_mj <- c(lb_mj,ub_mj);
    ly_mj <- c(0,0)
    
    ## Lebron James Information
    lj_mean = mean(lj_sb_data);lj_sd = sd(lj_sb_data); se_lj = lj_sd/sqrt(ns); me_lj = se_lj * t_cv
    lb_lj <- lj_mean - me_lj;ub_lj <- lj_mean + me_lj
    lj_text <- paste0("LB: ",round(lb_lj,2),
                      "| Mean: ",round(lj_mean,2),
                      "| UB: ",round(ub_lj,2),
                      "| SD: ",round(lj_sd,2),
                      "| SE: ",round(se_lj,2),"  ")
    px_lj_limits = c(lb_lj,ub_lj);
    lx_lj <- c(lb_lj,ub_lj);
    ly_lj <- c(0,0);
    
    ## Kobe Byrant Information
    kb_mean = mean(kb_sb_data);kb_sd = sd(kb_sb_data); se_kb = kb_sd/sqrt(ns); me_kb = se_kb * t_cv
    lb_kb <- kb_mean - me_kb;ub_kb <- kb_mean + me_kb
    kb_text <- paste0("LB: ",round(lb_kb,2),
                      "| Mean: ",round(kb_mean,2),
                      "| UB: ",round(ub_kb,2),
                      "| SD: ",round(kb_sd,2),
                      "| SE: ",round(se_kb,2),"  ")
    px_kb_limits = c(lb_kb,ub_kb);
    lx_kb <- c(lb_kb,ub_kb);
    ly_kb <- c(0,0)
    
    ## Stephen Curry Information
    sc_mean = mean(sc_sb_data);sc_sd = sd(sc_sb_data); se_sc = sc_sd/sqrt(ns); me_sc = se_sc * t_cv
    lb_sc <- sc_mean - me_sc;ub_sc <- sc_mean + me_sc
    sc_text <- paste0("LB: ",round(lb_sc,2),
                      "| Mean: ",round(sc_mean,2),
                      "| UB: ",round(ub_sc,2),
                      "| SD: ",round(sc_sd,2),
                      "| SE: ",round(se_sc,2),"  ")
    px_sc_limits = c(lb_sc,ub_sc);
    lx_sc <- c(lb_sc,ub_sc);
    ly_sc <- c(0,0)
    
    
    ## 11) Information to plot (C)
    xlim <- c(min(new_data[,st]),max(new_data[,st]));
    ylim <- c(0,10);
    py = c(0,0)
    w_size <- 1.5
    CI_lvl <- 3
    num_siz <- 3
    linethick <- 4
    axis_size <-1.8
    place_for_texty <- -1
    place_for_textx <- 1
    
    ## 12) Display Confidence Intervals 
    par(mfrow=c(4,1))
    par(xaxs='i',yaxs='i',mar=c(5,1,2,1));
    plot(lx_mj,ly_mj,xlim=xlim,ylim=ylim,axes=F,ann=F,type="l",
         cex=num_siz,col="red",cex.lab=num_siz,cex.axis=0.2,
         main=paste0("MJ ",mj_text));
    axis(1,cex.axis=axis_size);
    lines(lx_mj,ly_mj,col="red",cex=9,lwd=linethick)
    points(px_mj_limits,py,pch=c("[","]"),xpd=NA,col="red",cex=CI_lvl);
    points(mj_mean,0,pch=16,xpd=NA,col="red",cex=CI_lvl);
    text(max(new_data[,st])/2,5,paste0("MJ: ",mj_text),cex=w_size)
    
    par(xaxs='i',yaxs='i',mar=c(5,1,1,1));
    plot(lx_lj,ly_lj,xlim=xlim,ylim=ylim,axes=F,ann=F,type="l",cex=num_siz,col="gold",cex.lab=num_siz);
    axis(1,cex.axis=axis_size);
    lines(lx_lj,ly_lj,col="gold",cex=9,lwd=linethick)
    points(px_lj_limits,py,pch=c("[","]"),xpd=NA,col="gold",cex=CI_lvl );
    points(lj_mean,0,pch=16,xpd=NA,col="gold",cex=CI_lvl );
    text(max(new_data[,st])/2,5,paste0("LJ: ",lj_text),cex=w_size)
    
    par(xaxs='i',yaxs='i',mar=c(5,1,1,1));
    plot(lx_kb,ly_kb,xlim=xlim,ylim=ylim,axes=F,ann=F,type="l",cex=num_siz,col="purple",cex.lab=num_siz);
    axis(1,cex.axis=axis_size);
    lines(lx_kb,ly_kb,col="purple",cex=9,lwd=linethick)
    points(px_kb_limits,py,pch=c("[","]"),xpd=NA,col="purple",cex=CI_lvl );
    points(kb_mean,0,pch=16,xpd=NA,col="purple",cex=CI_lvl );
    text(max(new_data[,st])/2,5,paste0("KB: ",kb_text),cex=w_size)
    
    par(xaxs='i',yaxs='i',mar=c(5,1,1,1));
    plot(lx_sc,ly_sc,xlim=xlim,ylim=ylim,axes=F,ann=F,type="l",cex=num_siz ,col="black",cex.lab=num_siz);
    axis(1,cex.axis=axis_size);
    lines(lx_sc,ly_sc,col="blue",cex=9,lwd=linethick)
    points(px_sc_limits,py,pch=c("[","]"),xpd=NA,col="blue",cex=CI_lvl );
    points(sc_mean,0,pch=16,xpd=NA,col="blue",cex=CI_lvl );
    text(max(new_data[,st])/2,5,paste0("SC: ",sc_text),cex=w_size)
    
    
  })
  
  
})

# 13) Run the application 
shinyApp(ui = ui, server = server)

