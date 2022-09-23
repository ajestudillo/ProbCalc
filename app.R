# This is where IT have installed the shiny library
.libPaths(new=c("C:/ProgramData/R/Rlibs/R420"))
# Functions

FTail <- function(U=NULL, df_n=100, df_d = 100, curveColor=1, border=1, col="#808080", xlim=NULL, ylim=NULL, xlab='', ylab='', detail=999){
  if(U <= 5){xlim <- c(0,5)}
  if(U > 5){xlim <- c(0,U+0.01*U)}
  temp <- diff(range(xlim))
  x    <- seq(xlim[1] - temp/4, xlim[2] + temp/4, length.out=detail)
  y    <- df(x, df_n, df_d)
  ylim <- range(c(0,y))
  plot(x, y, type='l', xlim=xlim, ylim=ylim, axes=FALSE, col=curveColor, xlab = "", ylab = "")
  these <- (x >= U)
  X <- c(x[these][1], x[these], rev(x[these])[1])
  Y <- c(0, y[these], 0)
  polygon(X, Y, border=border, col=col)
  abline(h=0)
  axis(1, at = c(0,U), label = c(NA,round(U,4)))
}

normTail <- function(m=0, s=1, L=NULL, U=NULL, M=NULL, df=1000, curveColor=1, border=1, col="#808080",  xlim=NULL, ylim=NULL, xlab='', ylab='', digits=2, axes=1, detail=999, xLab=c('number', 'symbol'), cex.axis=1, xAxisIncr=1, ...){
  if(is.null(xlim)[1]){
    xlim <- m + c(-1,1)*3.5*s
  }
  temp <- diff(range(xlim))
  x    <- seq(xlim[1] - temp/4, xlim[2] + temp/4, length.out=detail)
  y    <- dt((x-m)/s, df)/s
  if(is.null(ylim)[1]){
    ylim <- range(c(0,y))
  }
  plot(x, y, type='l', xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, axes=FALSE, col=curveColor, ...)
  if(!is.null(L[1])){
    these <- (x <= L)
    X <- c(x[these][1], x[these], rev(x[these])[1])
    Y <- c(0, y[these], 0)
    polygon(X, Y, border=border, col=col)
  }
  if(!is.null(U[1])){
    these <- (x >= U)
    X <- c(x[these][1], x[these], rev(x[these])[1])
    Y <- c(0, y[these], 0)
    polygon(X, Y, border=border, col=col)
  }
  if(all(!is.null(M[1:2]))){
    these <- (x >= M[1] & x <= M[2])
    X <- c(x[these][1], x[these], rev(x[these])[1])
    Y <- c(0, y[these], 0)
    polygon(X, Y, border=border, col=col)
  }
  
  if(axes == 1 || axes > 2){
    if(xLab[1]=='symbol'){
      xAt  <- m + (-3:3)*s
      xLab <- expression(mu-3*sigma, mu-2*sigma,
                         mu-sigma, mu,	mu+sigma,
                         mu+2*sigma, mu+3*sigma)
    } else if(xLab[1] != 'number'){
      stop('Argument "xLab" not recognized.\n')
    } else {
      temp <- seq(xAxisIncr, max(abs(xlim-m))/s, xAxisIncr)*s
      xAt <- m + c(-temp, 0, temp)
      xLab <- round(xAt, digits=digits)
    }
  }
  if(axes > 2){
    axis(1, at=xAt, labels=xLab, cex.axis=cex.axis)
    buildAxis(2, c(y,0), n=3, nMax=3, cex.axis=cex.axis)
  } else if(axes > 1){
    buildAxis(2, c(y,0), n=3, nMax=3, cex.axis=cex.axis)
  } else if(axes > 0){
    axis(1, at=xAt, labels=xLab, cex.axis=cex.axis)
  }
  
  abline(h=0)
}

buildAxis <- function(side, limits, n, nMin = 2, nMax = 10, extend = 2, eps = 10^-12, ...) {
  if (!all(is.finite(limits))) {
    stop("Must provide finite limits.\n")
  }
  limits <- range(limits)
  if (limits[1] == limits[2]) {
    stop('Range of "limits" is too small. Scale the data.\n')
  }
  
  L <- limits
  l <- L + c(-1, 1) * diff(L) * extend
  s <- sign(l)
  l10 <- round(log10(abs(l)))
  d <- diff(l)
  d10 <- round(log10(d))
  
  L1 <- L
  temp <- round(L1[1] / 10^(d10))
  L <- L1 - 10^(d10) * temp
  Lup <- temp * 10^(d10)
  l1 <- l
  # 	temp <- round(l1[1]/10^(d10))
  l <- l1 - 10^(d10) * temp
  lup <- temp * 10^(d10)
  
  si <- list()
  si[[1]] <- seq(-6, 5, 0.01) / 10
  si[[2]] <- seq(-6, 5, 0.015) / 10
  si[[3]] <- seq(-6, 5, 0.02) / 10
  si[[4]] <- seq(-6, 5, 0.025) / 10
  si[[5]] <- seq(-6, 5, 0.03) / 10
  
  si[[6]] <- seq(-6, 5, 0.04) / 10
  si[[7]] <- seq(-6, 5, 0.05) / 10
  si[[8]] <- seq(-6, 5, 0.06) / 10
  si[[9]] <- seq(-7, 5, 0.07) / 10
  si[[10]] <- seq(-6, 5, 0.08) / 10
  
  AES <- c(
    8, 0, 7, 5, 3,
    4, 7, 2, 1, 2
  )
  for (i in 0:2) {
    for (j in 1:10) {
      temp <- round(10000 * si[[j]] * 10^i) / 10000
      si[[i * 10 + j]] <- temp
      AES[i * 10 + j] <- AES[j]
    }
  }
  
  if (0 >= L[1] && 0 <= L[2]) {
    start <- 0
  } else {
    start <- -10^max(round(log10(abs(L)) + 0.5))
    go <- rep(TRUE, 2)
    temp <- 10^max(round(log10(abs(L)) - 0.5))
    while (all(go)) {
      go <- FALSE
      if (start < L[1]) {
        start <- start + temp
        go <- TRUE
      }
    }
  }
  
  br <- list()
  se <- list()
  ss <- list()
  le <- c()
  for (i in 1:length(si)) {
    br[[i]] <- si[[i]] * 10^d10
    se[[i]] <- start + br[[i]]
    these <- (se[[i]] <= l[2] + eps) &
      (se[[i]] >= l[1] - eps)
    ss[[i]] <- se[[i]][these]
    these <- (se[[i]] <= L[2] + eps) &
      (se[[i]] >= L[1] - eps)
    le[i] <- sum(these)
    ss[[i]] <- ss[[i]] + Lup
  }
  L <- L1
  l <- l1
  
  these <- which(le >= nMin & le <= nMax)
  aes <- c()
  for (i in these) {
    min((ss[[i]][ss[[i]] > L[1]] - L[1]) / d)
    min((L[1] - ss[[i]][ss[[i]] < L[1]]) / d)
    abs(n - le[[i]])
    temp <- ss[[i]][ss[[i]] > L[1]] - L[1]
    temp1 <- -log(max(c(0.01, min(temp / d))), 5)
    temp <- L[1] - ss[[i]][ss[[i]] < L[1]]
    temp2 <- -log(max(c(0.01, min(temp / d))), 5)
    temp3 <- -abs(le[i] - n)^2 / (n + 1)
    AES[i] <- AES[i] + temp1 + temp2 + temp3
  }
  select <- which.max(AES[these])[1]
  
  l <- ss[[these[select]]]
  
  temp <- -round(log10(eps))
  l <- (round(l * 10^temp)) * 10^(-temp)
  axis(side, at = l, ...)
  invisible(l)
}

# UI

ui <- fluidPage(
  fluidRow(
    # input goes here
    column(
      4,
      div(helpText('Probability calculator'), align = "left", style="font-size:140%"),
      br(),
      selectInput(inputId = "dist",
                 label = "Distribution:",
                 choices = c("Normal"      = "rnorm",
                             "t"           = "rt",
                             "F"           = "rf"),
                 selected = "rnorm"),
      uiOutput("mean"),
      uiOutput("sd"),
      uiOutput("N"),
      uiOutput("df_1"),
      uiOutput("df_2"),
      br(),
      br(),
      helpText("Region of p-value:"),
      div(textOutput("region"),style="text-indent:20px;font-size:100%;"),
      br(),
      uiOutput("tail"),
      uiOutput("lower_bound"),
      uiOutput("upper_bound"),
      uiOutput("a"),
      uiOutput("b")  
    ), # close column 1
    
    # output goes here
    column(
      8,
      tabsetPanel(
        tabPanel(
          "Summary",
          plotOutput("plot"),
          div(textOutput("area"), align = "center", style="font-size:150%;")
        ),
        )
      ) # close column 2
    ) # close fluid row
  ) # close Fluid page



# Server

server = function(input, output) { 
  
  output$tail = renderUI(
    {
      if (input$dist == "rf"){
        selectInput(inputId = "tail",
                    label = "Find Area:",
                    choices = c("Upper Tail"="upper"),
                    selected = "upper")
      }
      else
      {
        selectInput(inputId = "tail",
                    label = "Find Area:",
                    choices = c("Lower Tail"="lower", 
                                "Upper Tail"="upper", 
                                "Both Tails"="both",
                                "Middle"="middle"),
                    selected = "lower")
      }
    })
  
  get_region_text = reactive(
    {
      if (is.null(input$tail)){
        shiny:::flushReact()
        return()
      }
      
      low_less = "<"
      low_greater = ">"
      
      up_less = "<"
      up_greater = ">"
      
      text = ""
      if (length(input$tail) != 0)
      {
        if (input$tail == "lower")
        {
          # P(X < a)
          text = paste0("P(X ", low_less, " a)")
        }
        else if (input$tail == "upper")
        {
          # P(X > a)
          text = paste0("P(X ", low_greater, " a)")
        }
        else if (input$tail == "middle")
        {
          # P(a < X < b)
          text = paste0("P(a ", low_less, " X ", up_less, " b)")
        }
        else if (input$tail == "both")
        {
          # P(X < a or X > b)
          text = paste0("P(X ", low_less, " a or X ", up_greater, " b)")
        }
        else if (input$tail == "equal")
        {
          # P(X = a)
          text = paste0("P(X = a)")
        }
      }
      
      return(text)
    })
  
  output$region = renderText(
    {
      get_region_text()
    })
  
  
  # Normal distribution stuff #

  output$mean = renderUI(
    {
      if (input$dist == "rnorm")
      {
        numericInput("mu",
                     "Mean",
                     value = 0)
      }
    })
  
  output$sd = renderUI(
    {
      if (input$dist == "rnorm")
      {
        numericInput("sd",
                     "Standard deviation",
                     value = 1)
      }
    })
  
  output$N = renderUI(
    {
      if (input$dist == "rnorm")
      {
        numericInput("N",
                     "Number of participants",
                     value = 1,
                     min=1)
      }
    })
  
  output$df_1 = renderUI(
    {
      if (input$dist == "rt")
      {
        numericInput("df",
                     "Degrees of freedom",
                     value = 10,
                     step=1,
                     min=1)
      }
      else if (input$dist == "rf")
      {
        numericInput("df_1",
                     "Numerator degrees of freedom",
                     value = 2,
                     step=1,
                     min=1)
      }
    })
  
  output$df_2 = renderUI(
    {
      if (input$dist == "rf")
      {
        numericInput("df_2",
                     "Denominator degrees of freedom",
                     value = 10,
                     step=1,
                     min=1)
      }
    })
  
  output$a = renderUI(
    {
      value = -1.96
      min = 0
      max = 1
      step = 1
      numericInput("a", 
                   label = "a",
                   value = value,
                   step=step,
                   min=min,
                   max=max)
    })
  
  output$b = renderUI(
    {
      if (is.null(input$tail))
      {
        shiny:::flushReact()
        return()
      }
      
      if (input$tail %in% c("middle","both"))
      {
        value = 1
        min = 0
        max = 1
        step = 1
        numericInput("b",
                     label = "b",
                     value = value,
                     step=step,
                     min=min,
                     max=max)
      }
    })  
  

  # Plotting #

  output$plot = renderPlot(
    { 
      if (is.null(input$tail) | is.null(input$a))
      {
        shiny:::flushReact()
        return()
      }
      
      L = NULL
      U = NULL
      
      error = FALSE
      
      if (input$tail == "lower" | input$tail == "equal")
      {
        L = input$a 
      }
      else if (input$tail == "upper")
      {
        U = input$a 
      }
      else if (input$tail %in% c("both","middle"))
      {
        if (is.null(input$b)){
          shiny:::flushReact()
          return()
        }
        
        L = input$a
        U = input$b
        
        if (L > U)
          error = TRUE
      }
      
      if (error)
      {
        plot(0,0,type='n',axes=FALSE,xlab="",ylab="",mar=c(1,1,1,1))
        text(0,0,"Error: Lower bound greater than upper bound.",col="red",cex=2)
      }
      else
      {
        if (input$dist == "rnorm" | input$dist == "rt") 
        {
          M = NULL
          if (input$tail == "middle")
          {
            M = c(L,U)
            L = NULL
            U = NULL
          }
          
          if(input$dist == "rnorm")
          {
            if(is.null(input$mu) | is.null(input$sd))
            {
              shiny:::flushReact()
              return()
            }
            
            #normTail(m=input$mu, s=input$sd, L=L, U=U, M=M, axes=3, cex.axis=1.5)
            normTail(m=input$mu, s=input$sd/sqrt(input$N), L=L, U=U, M=M, axes=3, cex.axis=1.5)
            title(main="Normal Distribution")
          }
          else if (input$dist == "rt")
          {
            if(is.null(input$df))
            {
              shiny:::flushReact()
              return()
            }
            
            normTail(m=0, s=1, df=input$df, L=L, U=U, M=M, axes=3, cex.axis=1.5)
            title(main="t Distribution")
          }
        }
        else if (input$dist == "rf")
        {        
          if(is.null(input$df_1) | is.null(input$df_2))
          {
            shiny:::flushReact()
            return()
          }
          
    
          FTail(U=U,df_n=input$df_1, df_d=input$df_2)
          title(main="F Distribution")
        }
        
      }
    })
  

  # Calculations #

  
  output$area = renderText(
    {
      if (is.null(input$tail) | is.null(input$a))
      {
        shiny:::flushReact()
        return()
      }
      
      L = input$a
      U = NULL
      
      if (input$tail %in% c("both","middle")) 
      {
        if (is.null(input$b))
        {
          shiny:::flushReact()
          return()
        }
        
        U = input$b
        
        error = FALSE
        if (L>U) error = TRUE
        if (error){
          return()
        }
      }
      
      
      
      f = function() NULL
      
      if (input$dist == "rnorm")
      {
        if (is.null(input$mu) | is.null(input$sd))
        {
          shiny:::flushReact()
          return()
        }
        
        # f = function(x) pnorm(x,input$mu,input$sd)
        f = function(x) pnorm(x,input$mu,input$sd/sqrt(input$N))
      }  
      else if (input$dist == "rt")
      {
        if (is.null(input$df))
        {
          shiny:::flushReact()
          return()
        }
        
        f = function(x) pt(x,input$df)
      }
      else if (input$dist == "rf"){
        if (is.null(input$df_1) | is.null(input$df_2))
        {
          shiny:::flushReact()
          return()
        }
        
        f = function(x) pf(x,input$df_1,input$df_2)
      }    
      
      
      val = NA
      if (input$tail == "lower")
        val = f(L)
      else if (input$tail == "upper")
        val = 1-f(L)
      else if (input$tail == "equal")
        val = f(L)
      else if (input$tail == "both")
        val = f(L) + (1-f(U))
      else if (input$tail == "middle")
        val = f(U) - f(L)
      
      text = paste("p = ",signif(val,3))
      
      
      text = sub("a",input$a,text)
      if (input$tail %in% c("both","middle")) 
        text = sub("b",input$b,text)
      
      text
    })
  } # close server 

shinyApp(ui, server)

