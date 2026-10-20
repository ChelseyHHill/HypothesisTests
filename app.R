library(shiny)
#l
# 1. UI Section (Paste content from your old ui.R here)
ui <- fluidPage(navbarPage("One-Sample Hypothesis Tests",
                           #tabPanel("Instructions",
                           #         mainPanel(
                          #            h4("App Overview:"),
                          #            p("Use this app to perform one-sample hypothesis tests for the mean and proportion."),
                          #            br(),
                          #            p("The app contains the following tabs:"),
                          #            tags$ol(
                          #              tags$li(tags$b("Mean: Known Sigma:"), "use a Z-Critical Value and Z-Test Statistic to perform a hypothesis test for the mean (\u03BC) when the population standard deviation (\u3C3) is", tags$em("known.")), 
                           #             tags$li(tags$b("Mean: Unknown Sigma:"), "use a t-Critical Value and t-Test Statistic to perform a hypothesis test for the mean (\u03BC) when the population standard deviation (\u3C3) is", tags$em("unknown.")), 
                           #             tags$li(tags$b("Proportion:"), "use a Z-Critical Value and Z-Test Statistic to perform a hypothesis test for the proportion (\u03C0).")
                           #           ),
                            #          br(),
                            #          hr(),
                            #          h4("Decision Rules:"),
                            #          p("A general overview of decision rules using the traditional approach (comparing the Critical Value and Test Statistic to determine if the Test Statistic falls in the rejection region) and the p-value approach (comparing the p-Value and significance level, alpha) is below."),
                             #         tags$img(src = "hyptestdec.jpg", width = "600px", height = "300px", align = "center"),
                             #         hr(),
                                      
                             #       ),),
                           tabPanel("Mean: Known Sigma",
                                    sidebarLayout(sidebarPanel(
                                      numericInput("mu", 
                                                   paste0("Hypothesized Population Mean (", "\u03BC",")"), 
                                                   value = 0),
                                      selectInput("test", 
                                                  "Alternative Hypothesis (H1)",
                                                  choices=c("<"="lt", "\u2260"="neq", ">"="gt"),
                                                  selected = "neq"),
                                      numericInput("alpha", 
                                                   paste0("Significance Level (", "\u03B1", ")"), 
                                                   value = 0.05), 
                                      numericInput("sig", 
                                                   paste0("Population Standard Deviation (", "\u3C3", ")"), 
                                                   value = 1),
                                      numericInput("n", 
                                                   "Sample Size (n)", 
                                                   value = 30),
                                      numericInput("xn", 
                                                   paste0("Sample Mean (", "\u0058\u0304",")"), 
                                                   value = 0),
                                      submitButton("Update View", icon("refresh"))
                                    ),
                                    
                                    # Show a plot of the generated distribution
                                    mainPanel(fluidRow(
                                      column(6,plotOutput("os_ks")),  
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      column(6,tableOutput("table_ks")))
                                    )
                                    )
                           ),
                           tabPanel("Mean: Unknown Sigma",
                                    sidebarLayout(sidebarPanel(
                                      numericInput("mu1", 
                                                   paste0("Hypothesized Population Mean (", "\u03BC",")"),
                                                   value = 0),
                                      selectInput("test1", 
                                                  "Alternative Hypothesis (H1)",
                                                  choices=c("<"="lt", "\u2260"="neq", ">"="gt"),
                                                  selected = "neq"),
                                      numericInput("alpha1", 
                                                   paste0("Significance Level (", "\u03B1", ")"),
                                                   value = 0.05),    
                                      numericInput("xn1", 
                                                   paste0("Sample Mean (", "\u0058\u0304",")"),  
                                                   value = 0),
                                      numericInput("s1", 
                                                   "Sample Standard Deviation (S)", 
                                                   value = 1),
                                      numericInput("n1", 
                                                   "Sample Size (n)", 
                                                   value = 30),
                                      submitButton("Update View", icon("refresh"))
                                    ),
                                    mainPanel(fluidRow(
                                      column(6,plotOutput("os_us")),  
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      column(6,tableOutput("table_us")))
                                    )
                                    )),
                           tabPanel("Proportion",
                                    sidebarLayout(sidebarPanel(
                                      numericInput("pi", 
                                                   paste0("Hypothesized Population Proportion (", "\u03C0", ")"), 
                                                   value = 0.3),
                                      selectInput("test2", 
                                                  "Alternative Hypothesis (H1)",
                                                  choices=c("<"="lt", "\u2260"="neq", ">"="gt"),
                                                  selected = "neq"),
                                      numericInput("alpha2", 
                                                   paste0("Significance Level (", "\u03B1", ")"), 
                                                   value = 0.05),    
                                      numericInput("prop", 
                                                   "Sample  Proportion (p)", 
                                                   value = 0.3),
                                      numericInput("n2", 
                                                   "Sample Size (n)", 
                                                   value = 30),
                                      submitButton("Update View", icon("refresh"))
                                    ),
                                    mainPanel(fluidRow(
                                      column(6,plotOutput("os_prop")),  
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      column(6,tableOutput("table_prop")))
                                    )
                                    ))))

  
  # 2. Server Section (Paste content from your old server.R here)
  server <- function(input, output) {
    
    output$os_ks <- renderPlot({
      if(input$test=="lt"){
        zcritp <- qnorm(input$alpha, lower.tail = TRUE)
        se <- input$sig/sqrt(input$n)
        ztest <- (input$xn - input$mu)/se
        zg <- seq(-4,4,length=100)
        hsg <- dnorm(zg,0,1)
        plot(zg, hsg, type="n", yaxt="n", bty="n", xlab="", ylab="",
             main="", sub = paste0("Z-Critical = ", round(zcritp,2), "\n", "Z-Test = ", round(ztest,2)))
        zpol=seq(min(zg),zcritp,length=100)
        y=dnorm(zpol, 0, 1)
        polygon(c(min(zpol),zpol,zcritp),c(0,y,0),col="lightgray")
        lines(zg, hsg)
        abline(v=ztest, col="red", lwd=1)
      } else if(input$test=="gt"){
        zcritp <- qnorm(input$alpha, lower.tail = FALSE)
        se <- input$sig/sqrt(input$n)
        ztest <- (input$xn - input$mu)/se
        zg <- seq(-4,4,length=100)
        hsg <- dnorm(zg,0,1)
        plot(zg, hsg, type="n", yaxt="n", bty="n", xlab="", ylab="",
             main="", sub = paste0("Z-Critical = ", round(zcritp,2),"\n", "Z-Test = ", round(ztest,2)))
        zpol=seq(zcritp,max(zg),length=100)
        y=dnorm(zpol, 0, 1)
        polygon(c(zcritp,zpol,max(zpol)),c(0,y,0),col="lightgray")
        lines(zg, hsg)
        abline(v=ztest, col="red", lwd=1)
      } else{
        zcritp_l <- qnorm((input$alpha/2), lower.tail = TRUE)
        zcritp_h <- qnorm((input$alpha/2), lower.tail = FALSE)
        se <- input$sig/sqrt(input$n)
        ztest <- (input$xn - input$mu)/se
        zg <- seq(-4,4,length=100)
        hsg <- dnorm(zg,0,1)
        plot(zg, hsg, type="n", yaxt="n", bty="n", xlab="", ylab="",
             main="", sub = paste0("Z-Critical = +/-", round(zcritp_h,2), "\n", "Z-Test = ", round(ztest,2)))
        zpol=seq(zcritp_h,max(zg),length=100)
        y=dnorm(zpol, 0, 1)
        polygon(c(zcritp_h,zpol,max(zpol)),c(0,y,0),col="lightgray")
        zpol2=seq(min(zg),zcritp_l,length=100)
        y2=dnorm(zpol2, 0, 1)
        polygon(c(min(zpol2),zpol2,zcritp_l),c(0,y2,0),col="lightgray")
        lines(zg, hsg)
        abline(v=ztest, col="red", lwd=1)
      }
    })
    
    
    output$table_ks <- renderTable({
      if(input$test=="lt"){
        null_hyp <- paste0("H0: ", "\u03BC", " \u2265 ", input$mu)
        alt_hyp <- paste0("H1: ", "\u03BC", " < ", input$mu)
        zcrit <- qnorm(input$alpha, lower.tail = TRUE)
        sep <- input$sig/sqrt(input$n)
        ztesty <- (input$xn - input$mu)/sep
        pval <- pnorm(ztesty, lower.tail = TRUE)
        Measures <- c("Null Hypothesis", "Alternative Hypothesis","Standard Error", paste0("Alpha (","\u03B1", ")"), "Z Critical Value", "Z-Test Statistic", "p-Value")
        Values <- c(null_hyp, alt_hyp, round(sep,4), input$alpha, round(zcrit,4), round(ztesty,4), round(pval,4))
        cbind(Measures,Values)
      }else if(input$test == "gt"){
        null_hyp <- paste0("H0: ", "\u03BC", " \u2264 ", input$mu)
        alt_hyp <- paste0("H1: ", "\u03BC", " > ", input$mu)
        zcrit <- qnorm(input$alpha, lower.tail = FALSE)
        sep <- input$sig/sqrt(input$n)
        ztesty <- (input$xn - input$mu)/sep
        pval <- pnorm(ztesty, lower.tail = FALSE)
        Measures <- c("Null Hypothesis", "Alternative Hypothesis", "Standard Error", paste0("Alpha (","\u03B1", ")"),  "Z-Critical Value", "Z-Test Statistic", "p-Value")
        Values <- c(null_hyp, alt_hyp, round(sep,4), input$alpha, round(zcrit,4), round(ztesty,4), round(pval,4))
        cbind(Measures,Values)
      } else{
        null_hyp <- paste0("H0: ", "\u03BC", " = ", input$mu)
        alt_hyp <- paste0("H1: ", "\u03BC", " \u2260 ", input$mu)
        zcrit <- qnorm((input$alpha)/2, lower.tail = FALSE)
        sep <- input$sig/sqrt(input$n)
        ztesty <- (input$xn - input$mu)/sep
        pval <- pnorm(abs(ztesty), lower.tail = FALSE)*2
        cilb <- input$xn - (abs(zcrit))*sep
        ciub <- input$xn + (abs(zcrit))*sep
        Measures <- c("Null Hypothesis", "Alternative Hypothesis", "Standard Error", paste0("Alpha (","\u03B1", ")"), "Z-Critical Value", "Z-Test Statistic", "p-Value", "Confidence Interval")
        Values <- c(null_hyp, alt_hyp, round(sep,4), input$alpha, paste0("+/-",round(zcrit,4)), round(ztesty,4), round(pval,4), paste0(round(cilb,4), " < ","\u03BC", " < ",round(ciub,4)))
        cbind(Measures = Measures,Values = Values)}
      
    })
    
    output$os_us <- renderPlot({
      if(input$test1=="lt"){
        tcrit <- qt(input$alpha1, df = (input$n1-1), lower.tail = TRUE)
        ses <- input$s1/sqrt(input$n1)
        ttest <- (input$xn1 - input$mu1)/ses
        tg <- seq(-4,4,length=100)
        hxb2 <- dt(tg,df = (input$n1-1))
        plot(tg, hxb2, type="n", yaxt="n", bty="n", xlab="", ylab="",
             main="", sub = paste0("t-Critical = ", round(tcrit,2), "\n", "t-Test = ", round(ttest,2)))
        tx=seq(min(tg), tcrit,length=100)
        ty=dt(tx,df=(input$n1-1))
        polygon(c(min(tx),tx,tcrit),c(0,ty,0),col="lightgray")
        lines(tg, hxb2)
        abline(v=ttest, col="red", lwd=1)
      } else if(input$test1=="gt"){
        tcrit <- qt(input$alpha1, df = (input$n1-1), lower.tail = FALSE)
        ses <- input$s1/sqrt(input$n1)
        ttest <- (input$xn1 - input$mu1)/ses
        tg <- seq(-4,4,length=100)
        hxb2 <- dt(tg,df = (input$n1-1))
        plot(tg, hxb2, type="n", yaxt="n", bty="n", xlab="", ylab="",
             main="", sub = paste0("t-Critical = ", round(tcrit,2), "\n", "t-Test = ", round(ttest,2)))
        tx=seq(tcrit, max(tg),length=100)
        ty=dt(tx,df=(input$n1-1))
        polygon(c(tcrit,tx,max(tx)),c(0,ty,0),col="lightgray")
        lines(tg, hxb2)
        abline(v=ttest, col="red", lwd=1)
      } else{
        tcrit <- qt(input$alpha1/2, df = (input$n1-1), lower.tail = FALSE)
        se1 <- input$s1/sqrt(input$n1)
        ttest <- (input$xn1 - input$mu1)/se1
        tg <- seq(-4,4,length=100)
        hxb2 <- dt(tg,df = (input$n1-1))
        plot(tg, hxb2, type="n", yaxt="n", bty="n", xlab="", ylab="",
             main="", sub = paste0("t-Critical = +/-", round(tcrit,2), "\n", "t-Test = ", round(ttest,2)))
        tx=seq(tcrit,max(tg),length=100)
        y=dt(tx,df=(input$n1-1))
        polygon(c(tcrit,tx,max(tx)),c(0,y,0),col="lightgray")
        
        tx1=seq(min(tg), -tcrit,length=100)
        ty=dt(tx1,df=(input$n1-1))
        polygon(c(min(tx1),tx1,-tcrit),c(0,ty,0),col="lightgray")
        lines(tg, hxb2)
        abline(v=ttest, col="red", lwd=1)
      }
    })
    
    output$table_us <- renderTable({
      if(input$test1=="lt"){
        null_hyp1 <- paste0("H0: ", "\u03BC", " \u2265 ", input$mu1)
        alt_hyp1 <- paste0("H1: ", "\u03BC", " < ", input$mu1)
        tcrit <- qt(input$alpha1, df = (input$n1-1), lower.tail = TRUE)
        ses <- input$s1/sqrt(input$n1)
        ttest1 <- (input$xn1 - input$mu1)/ses
        pval1 <- pt(ttest1, df = (input$n1-1), lower.tail = TRUE)
        Measures1 <- c("Null Hypothesis", "Alternative Hypothesis","Standard Error", paste0("Alpha (","\u03B1", ")"), "t-Critical Value", "t-Test Statistic", "p-Value")
        Values1 <- c(null_hyp1, alt_hyp1, round(ses,4), input$alpha1, round(tcrit,4), round(ttest1,4), round(pval1,4))
        cbind(Measures = Measures1,Values = Values1)
      }else if(input$test1 == "gt"){
        null_hyp1 <- paste0("H0: ", "\u03BC", " \u2264 ", input$mu1)
        alt_hyp1 <- paste0("H1: ", "\u03BC", " > ", input$mu1)
        tcrit <- qt(input$alpha1, df = (input$n1-1), lower.tail = FALSE)
        ses <- input$s1/sqrt(input$n1)
        ttest1 <- (input$xn1 - input$mu1)/ses
        pval1 <- pt(ttest1, df = (input$n1-1), lower.tail = FALSE)
        Measures1 <- c("Null Hypothesis", "Alternative Hypothesis", "Standard Error", paste0("Alpha (","\u03B1", ")"), "t-Critical Value", "t-Test Statistic", "p-Value")
        Values1 <- c(null_hyp1, alt_hyp1, round(ses,4), input$alpha1, round(tcrit,4), round(ttest1,4), round(pval1,4))
        cbind(Measures = Measures1,Values = Values1)
      } else{
        null_hyp1 <- paste0("H0: ", "\u03BC", " = ", input$mu1)
        alt_hyp1 <- paste0("H1: ", "\u03BC", " \u2260 " , input$mu1)
        tcrit <- qt(input$alpha1/2, df = (input$n1-1), lower.tail = FALSE)
        ses <- input$s1/sqrt(input$n1)
        ttest1 <- (input$xn1 - input$mu1)/ses
        pval1 <- pt(abs(ttest1), df = (input$n1-1), lower.tail = FALSE)*2
        cilbt <- input$xn1 - (abs(tcrit))*ses
        ciubt <- input$xn1 + (abs(tcrit))*ses
        Measures1 <- c("Null Hypothesis", "Alternative Hypothesis", "Standard Error", paste0("Alpha (","\u03B1", ")"), "t-Critical Value", "t-Test Statistic", "p-Value", "Confidence Interval")
        Values1 <- c(null_hyp1, alt_hyp1, round(ses,4), input$alpha1, paste0("+/-",round(tcrit,4)), round(ttest1,4), round(pval1,4), paste0(round(cilbt,4), " < ","\u03BC", " < ",round(ciubt,4)))
        cbind(Measures = Measures1,Values = Values1)}
      
    })
    output$os_prop <- renderPlot({
      if(input$test2=="lt"){
        zcritpr <- qnorm(input$alpha2, lower.tail = TRUE)
        sepr <- sqrt((input$pi*(1-input$pi))/input$n2)
        ztestpr <- (input$prop - input$pi)/sepr
        zgpr <- seq(-4,4,length=100)
        hsgpr <- dnorm(zgpr,0,1)
        plot(zgpr, hsgpr, type="n", yaxt="n", bty="n", xlab="", ylab="",
             main="", sub = paste0("Z-Critical = ", round(zcritpr,2), "\n", "Z-Test = ", round(ztestpr,2)))
        zpolpr=seq(min(zgpr),zcritpr,length=100)
        ypr=dnorm(zpolpr, 0, 1)
        polygon(c(min(zpolpr),zpolpr,zcritpr),c(0,ypr,0),col="lightgray")
        lines(zgpr, hsgpr)
        abline(v=ztestpr, col="red", lwd=1)
      } else if(input$test2=="gt"){
        zcritpr <- qnorm(input$alpha2, lower.tail = FALSE)
        sepr <- sqrt((input$pi*(1-input$pi))/input$n2)
        ztestpr <- (input$prop - input$pi)/sepr
        zgpr <- seq(-4,4,length=100)
        hsgpr <- dnorm(zgpr,0,1)
        plot(zgpr, hsgpr, type="n", yaxt="n", bty="n", xlab="", ylab="",
             main="", sub = paste0("Z-Critical = ", round(zcritpr,2), "\n", "Z-Test = ", round(ztestpr,2)))
        zpolpr=seq(zcritpr,max(zgpr),length=100)
        ypr=dnorm(zpolpr, 0, 1)
        polygon(c(zcritpr,zpolpr,max(zpolpr)),c(0,ypr,0),col="lightgray")
        lines(zgpr, hsgpr)
        abline(v=ztestpr, col="red", lwd=1)
      } else{
        zcritp_lp <- qnorm((input$alpha2/2), lower.tail = TRUE)
        zcritp_hp <- qnorm((input$alpha2/2), lower.tail = FALSE)
        sepr <- sqrt((input$pi*(1-input$pi))/input$n2)
        ztestpr <- (input$prop - input$pi)/sepr
        zgpr <- seq(-4,4,length=100)
        hsgpr <- dnorm(zgpr,0,1)
        plot(zgpr, hsgpr, type="n", yaxt="n", bty="n", xlab="", ylab="",
             main="", sub = paste0("Z-Critical = +/-", round(zcritp_hp,2), "\n", "Z-Test = ", round(ztestpr,2)))
        zpolpr=seq(zcritp_hp,max(zgpr),length=100)
        ypr=dnorm(zpolpr, 0, 1)
        polygon(c(zcritp_hp,zpolpr,max(zpolpr)),c(0,ypr,0),col="lightgray")
        zpol2pr=seq(min(zgpr),zcritp_lp,length=100)
        y2pr=dnorm(zpol2pr, 0, 1)
        polygon(c(min(zpol2pr),zpol2pr,zcritp_lp),c(0,y2pr,0),col="lightgray")
        lines(zgpr, hsgpr)
        abline(v=ztestpr, col="red", lwd=1)
      }
    })
    
    output$table_prop <- renderTable({
      if(input$test2=="lt"){
        null_hyp2 <- paste0("H0: ", "\u03C0", " \u2265 ", input$pi)
        alt_hyp2 <- paste0("H1: ", "\u03C0", " < ", input$pi)
        zcritpr2 <- qnorm(input$alpha2, lower.tail = TRUE)
        seprt <- sqrt((input$pi*(1-input$pi))/input$n2)
        ztestypr <- (input$prop - input$pi)/seprt
        pvalpr <- pnorm(ztestypr, lower.tail = TRUE)
        Measurespr <- c("Null Hypothesis", "Alternative Hypothesis","Standard Error", paste0("Alpha (","\u03B1", ")"), "Z-Critical Value", "Z-Test Statistic", "p-Value")
        Valuespr <- c(null_hyp2, alt_hyp2, round(seprt,4), input$alpha2, round(zcritpr2,4), round(ztestypr,4), round(pvalpr,4))
        cbind(Measures = Measurespr,Values = Valuespr)
      }else if(input$test2 == "gt"){
        null_hyp2 <- paste0("H0: ", "\u03C0", " \u2264 ", input$pi)
        alt_hyp2 <- paste0("H1: ", "\u03C0", " > ", input$pi)
        zcritpr2 <- qnorm(input$alpha2, lower.tail = FALSE)
        seprt <- sqrt((input$pi*(1-input$pi))/input$n2)
        ztestypr <- (input$prop - input$pi)/seprt
        pvalpr <- pnorm(ztestypr, lower.tail = FALSE)
        Measurespr <- c("Null Hypothesis", "Alternative Hypothesis","Standard Error", paste0("Alpha (","\u03B1", ")"), "Z-Critical Value", "Z-Test Statistic", "p-Value")
        Valuespr <- c(null_hyp2, alt_hyp2, round(seprt,4), input$alpha2, round(zcritpr2,4), round(ztestypr,4), round(pvalpr,4))
        cbind(Measures = Measurespr,Values = Valuespr)
      } else{
        null_hyp2 <- paste0("H0: ", "\u03C0", " = ", input$pi)
        alt_hyp2 <- paste0("H1: ", "\u03C0", " \u2260 ", input$pi)
        zcritpr2 <- qnorm((input$alpha2)/2, lower.tail = FALSE)
        seprt <- sqrt((input$pi*(1-input$pi))/input$n2)
        sepci <- sqrt((input$prop*(1-input$prop))/input$n2)
        ztestypr <- (input$prop - input$pi)/seprt
        pvalpr <- (pnorm(abs(ztestypr), lower.tail = FALSE))*2
        cilbpr <- input$prop - (abs(zcritpr2))*sepci
        ciubpr <- input$prop + (abs(zcritpr2))*sepci
        Measurespr <- c("Null Hypothesis", "Alternative Hypothesis", "Standard Error", paste0("Alpha (","\u03B1", ")"), "Z-Critical Value", "Z-Test Statistic", "p-Value", "Confidence Interval")
        Valuespr <- c(null_hyp2, alt_hyp2, round(seprt,4), input$alpha2, paste0("+/-",round(zcritpr2,4)), round(ztestypr,4), round(pvalpr,4), paste0(round(cilbpr,4), " < ","\u03C0", " < ",round(ciubpr,4)))
        cbind(Measures = Measurespr,Values = Valuespr)}
      
    })
    
  }

    
    # 3. App Launch (This binds the UI and Server together)
    shinyApp(ui = ui, server = server)
