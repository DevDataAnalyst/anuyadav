library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Test Data Certification"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Copy the line below to make a file upload manager
      fileInput("file1", label = h3("Upload File for Environment 1")),
     
      tags$hr(),
      
      # Copy the line below to make a file upload manager
      # fileInput("file2", label = h3("Upload File for Environment 2")),
      
      tags$hr(),
      actionButton("EXECUTE", "RUN"),
      actionButton("Display", "Display Result")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(
        # Output: Data file ----
        column(12,
        tableOutput("contents1")),
        column(12,
        tableOutput("contents2"))
      )
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents1 <- renderTable({
   
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath)
   df1 = head(df1)
   
  })
  
  # Call Onclick
  
  #onclick("EXECUTE", source("C:\\Personal\\misctasks\\anu\\work\\laplace.R"))
  
  #### laplace code  ####
  
  #install.packages('cluster')
  #install.packages('gower')
  #install.packages('abind')
  library(cluster)
  library(gower)
  library(abind)
  #change the working directory and the data file to be run on below two line respectively
  setwd("C:\\Personal\\misctasks\\anu\\work")
  
  data <-  read.csv("wiscon_200.csv", header = T, stringsAsFactors = T)
  #str(data)
  
  LapScore<-function (data,t,k,nf)
  {
    ## Rep replicates the value
    ##NROW returns the number of rows in data
    on<-rep(1,nrow(data)) 
    one<-t(t(on))
    lr<-c()
    s<-c()
    ls<-c()
    
    for(i in 1 :ncol(data))##NCOL returns the number of column in data
    {
      fr<-data[i]
      dm<-as.matrix(daisy(fr, metric = "gower",stand = TRUE))
      ## Daisy function list out all the dissimilarities between the observation in the data set.
      for (j in 1:ncol(dm))
      {
        col<-dm[j,]
        z<-which(order(col)<=k)
        temp<-rep(0,ncol(dm))
        temp[z]<-exp(1)^(-col[z]/t)
        s<-rbind(s,temp)
      }
      s1<-s%*%one
      s1<-as.vector(s1)
      d<-diag(s1)
      l<-d - s
      imd<-(t(fr)%*%d%*%one)/(t(one)%*%d%*%one)
      imd<-c(imd)
      frbar<-fr - imd*as.matrix(one)
      frbar<-as.matrix(frbar)
      lr[i]<-(t(frbar)%*%l%*%frbar)/(t(frbar)%*%d%*%frbar)
      s<-c()
    }
    z<-which(order(lr)<=nf)
    ls$lr<-lr[z]
    ls$z<-z
    ls$flr<-lr
    ls
  }
  
  fselect<-function(data,thresh)
  {
    # Initialization
    fscore<-c()
    # Normalization of data and then applying PCA 
    x<-scale(data)
    
    xpca<-prcomp(x)
    #  Find the no. of components based on % of variance explained
    xev<-xpca$sdev^2
    xevm<-as.matrix(xev)
    xevm<-xevm/sum(xevm[,1])
    pc<-1
    sum<-0
    for (i in 1:nrow(xevm))
    {
      sum<-sum + xevm[i,]
      if(sum>thresh)
      {
        pc<-i
        break
      }
    }
    #  Select features and score them on correlation with the main principal components
    corpc<-cor(x,xpca$x[,1:pc])
    abscorpc<-abs(corpc)
    evpc<-as.matrix(xevm[1:pc,])
    fscore$x<-abscorpc %*% evpc
    fscore$pc<-pc
    return(fscore)
  }
  
  purity <- function(data,nc,nvr)
  {
    purity<- c()
    sumpure<-0
    #removing the class
    datac<-data[-nvr]
    datac<-scale(datac)
    #performing K means on the data
    
    for ( l in 1 : 100)
    {
      x1<-sample(1:10000,1)
      
      set.seed(x1)
      kmdata<-kmeans(datac,nc,iter.max = 25, nstart = 10)
      # comparing the class and the cluster information
      
      pure<-as.matrix(table(kmdata$cluster,t(data[nvr])))
      
      
      # Looping to find out maximum of each class
      
      for(i in 1 : ncol(pure))
      {
        sumpure<-sumpure+max(pure[i,])
        
      }
      
      purity<-c(purity,sumpure/nrow(data))
      sumpure<-0
    }
    return(mean(purity))
    
  }
  
  getSWunique <- function(subsetdata, maxclus)
  {
    x <- as.data.frame(subsetdata)
    
    id <- as.integer(x[1,1])
    people <- length(as.vector(x[,1]))
    
    if (people == 1){
      p = 0
    }
    else {
      diss <- daisy(x, metric="gower")
      asw <- numeric(maxclus)
      
      for (k in 2:maxclus) {
        asw[[k]] <- pam(diss, k, diss=T)$silinfo$avg.width
      }
      k.best <- which.max(asw)
      swg <- asw[k.best]
    }
    swg
  }
  
  runLaplacian <- function(data)
  {
    class <- data[ncol(data)]
    data <- data[-ncol(data)]
    
    fs<-fselect(data,.9)
    k<-LapScore(data,0.5,5,fs$pc)
    print("Laplacian : ")
    print(length(k$z))
    print(k$z)
    data2<-cbind(data[k$z],class)
    l<-purity(data2,length(table(class)),ncol(data2))
    
    maxclus <- nrow(unique(class))
    l <- getSWunique(data2[-ncol(data2)], maxclus)
    
    return(data2)
    # to check the cluster and purity uncomment below return type and comment above return type
    #return(l) 
    
  }
  
  m <- runLaplacian(data)
  
  m
  
  write.csv(m, file = "new_file.csv")
  #read.csv("new_file.csv", nrows=5)
  
  #### end of laplace code ####
  #output$contents2 = renderTable({
    #req(input$file2)
    
    #df2 <- read.csv("new_file.csv")
    #m = head(m)
  #})
}

# Create Shiny app ----
shinyApp(ui, server)
