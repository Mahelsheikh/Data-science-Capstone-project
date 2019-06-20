library(shiny)
library(shinydashboard)
ui<-dashboardPage(
        dashboardHeader(title ="Single word prediction application"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Application",tabName = "application",
                                 icon = icon("keyboard")),
                        menuItem("Documentation",tabName = "documentation",
                                 icon = icon("book"))
                )
        ),
        dashboardBody(
                tabItems(tabItem(tabName = "application",
                                 fluidPage(
                                         box(title = "Sentence entry",
                                             textInput(inputId = "Text",
                                                       label = "Enter a sentence",
                                         ),
                                         solidHeader = TRUE,background = "blue"),
                                         box(title = "Predicted word",
                                             h3(uiOutput("suggestion")),
                                             solidHeader = TRUE,
                                             background = "green"),
                                         infoBox("Instructions", 
                                                 value = "Type in the sentence to get the prediction", 
                                                 subtitle = NULL,
                                                 icon = shiny::icon("info"), 
                                                 color = "orange", width = 6,
                                                 href = NULL, fill = TRUE))
                ),
                tabItem(tabName = "documentation",h3(
                        "The following links has detailed steps to produce this data product:",
                        br(),
                                                     "Exploratory analysis of text file:",
                        br(),
                                                     a("Milestone report part 1",
                                                       href="http://rpubs.com/mah_elsheikh/499028"),
                        br(),
                                                     "Processing the data and Creating algorithm:",
                        br(),
                                                     a("Milestone report part 2",
                                                       href="http://rpubs.com/mah_elsheikh/506474"),
                        br(),
                                                     "Data product code:",br(),
                                                     a("Milestone report part 2",
                                                       href="http://rpubs.com/mah_elsheikh/506208"),
                        br(),
                                                     "Data product code:",
                        br(),
                                                     a("Data product presentation",
                                                       href="http://rpubs.com/mah_elsheikh/506553"))
                )
                )
                
        ))


server<-(function(input,output){
        require(ngram)
        preword<-function(x,n){
                n<-n-1
                ngram_length<-wordcount(x)
                if (ngram_length == 1) {
                        preword<-x
                }else{
                        preword<-paste(strsplit(x,
                                                split = " ")[[1]][(ngram_length-n):ngram_length],
                                       collapse = " ")
                }
                return(preword)
        }
        require(stopwords)
        stopen<-stopwords::stopwords("en",source = "snowball")
        wordsuggestion<-function(pre){
                ## Creating needed vectors
                require(ngram)
                pre<-trimws(tolower(pre))
                quadmatrix<-probmatrixquad
                inwordsquad<-rownames(quadmatrix)
                probmatrix1 <- probmatrix1
                inwordspre1<-rownames(probmatrix1)
                probmatrix2 = probmatrix2
                inwordspre2<-rownames(probmatrix2)
                probmatrix3 = probmatrix3
                inwordspre3<-rownames(probmatrix3)
                ## Defining preword from the given sentence if no word is given
                if (pre == ""){pre =="1" }
                ##  If the given sentence is longer than three words
                if(wordcount(pre) > 3){
                        pre<-preword(pre,3)
                        ## Testing if the preword exist in the quadgram matrix
                        d<-pre %in% inwordsquad
                        if (d == TRUE) {
                                ## Getting word suggestion from quad gram matrix 
                                ## in case the preword exists in the quadgram
                                wordrange<-quadmatrix[(rownames(quadmatrix) == pre) == TRUE,]
                        }else{
                                ## If no words are found shortening the preword
                                ## by one word
                                pre<-preword(pre,2)
                        }
                }
                
                if (exists("wordrange") == FALSE){
                        ## Creating logical vectors for each matrix if the 
                        ## preword found in each matrix or not  
                        x<-pre %in% inwordspre1
                        y<-pre %in% inwordspre2
                        z<-pre %in% inwordspre3
                        ## Getting the row from the matrix that the preword 
                        ## found in which contains the probabilities
                        if (x == TRUE) {
                                wordrange<-probmatrix1[(rownames(probmatrix1) == pre) == TRUE,]
                        }
                        if (y == TRUE) {
                                wordrange<-probmatrix2[(rownames(probmatrix2) == pre) == TRUE,]
                        }
                        if (z == TRUE) {
                                wordrange<-probmatrix3[(rownames(probmatrix3) == pre) == TRUE,]
                        }
                        
                        if (x == FALSE & y == FALSE & z == FALSE){
                                ## If no words are found shortening the 
                                ## preword by two words
                                pre<-suppressWarnings(preword(pre,1))
                        }
                }
                
                if (exists("wordrange") == FALSE){
                        ## Creating logical vectors for each matrix if 
                        ## the preword found in each matrix or not 
                        x2<-pre %in% inwordspre1
                        y2<-pre %in% inwordspre2
                        z2<-pre %in% inwordspre3
                        ## Getting the row from the matrix that 
                        ## the preword found in which contains the probabilities
                        if (x2 == TRUE) {
                                wordrange<-probmatrix1[(rownames(probmatrix1) == pre) == TRUE,]
                        }
                        if (y2 == TRUE) {
                                wordrange<-probmatrix2[(rownames(probmatrix2) == pre) == TRUE,]
                        }
                        if (z2 == TRUE) {
                                wordrange<-probmatrix3[(rownames(probmatrix3) == pre) == TRUE,]
                        }
                }
                ## If a result was found from the above algorithm
                if (exists("wordrange") == TRUE){
                        wordrange<-sort(wordrange,decreasing = TRUE)
                        result<-names(wordrange[1])
                }else{
                        ## if no result was found. The algorithm will return 
                        ## the top three words in the English language
                        result<-c("the")
                }
                return(result)
        }
        
        wordsuggestion2<-function(pre){
                ## Creating needed vectors
                require(ngram)
                pre<-trimws(tolower(pre))
                quadmatrix<-probmatrixquad
                inwordsquad<-rownames(quadmatrix)
                probmatrix1 <- probmatrix1
                inwordspre1<-rownames(probmatrix1)
                probmatrix2 = probmatrix2
                inwordspre2<-rownames(probmatrix2)
                probmatrix3 = probmatrix3
                inwordspre3<-rownames(probmatrix3)
                ## Defining preword from the given sentence if no word is given
                if (pre == ""){pre =="1" }
                ##  If the given sentence is longer than three words
                if(wordcount(pre) > 3){
                        pre<-preword(pre,3)
                        ## Testing if the preword exist in the quadgram matrix
                        d<-pre %in% inwordsquad
                        if (d == TRUE) {
                                ## Getting word suggestion from quad gram 
                                ## matrix in case the preword exists in the quadgram
                                wordrange<-quadmatrix[(rownames(quadmatrix) == pre) == TRUE,]
                        }else{
                                ## If no words are found shortening the preword by one word
                                pre<-preword(pre,2)
                        }
                }
                
                if (exists("wordrange") == FALSE){
                        ## Creating logical vectors for each matrix if
                        ## the preword found in each matrix or not  
                        x<-pre %in% inwordspre1
                        y<-pre %in% inwordspre2
                        z<-pre %in% inwordspre3
                        ## Getting the row from the matrix that the preword 
                        ## found in which contains the probabilities
                        if (x == TRUE) {
                                wordrange<-probmatrix1[(rownames(probmatrix1) == pre) == TRUE,]
                        }
                        if (y == TRUE) {
                                wordrange<-probmatrix2[(rownames(probmatrix2) == pre) == TRUE,]
                        }
                        if (z == TRUE) {
                                wordrange<-probmatrix3[(rownames(probmatrix3) == pre) == TRUE,]
                        }
                        
                        if (x == FALSE & y == FALSE & z == FALSE){
                                ## If no words are found shortening the preword 
                                ## by two words
                                pre<-suppressWarnings(preword(pre,1))
                        }
                }
                
                if (exists("wordrange") == FALSE){
                        ## Creating logical vectors for each matrix if 
                        ## the preword found in each matrix or not 
                        x2<-pre %in% inwordspre1
                        y2<-pre %in% inwordspre2
                        z2<-pre %in% inwordspre3
                        ## Getting the row from the matrix that 
                        ## the preword found in which contains the probabilities
                        if (x2 == TRUE) {
                                wordrange<-probmatrix1[(rownames(probmatrix1) == pre) == TRUE,]
                        }
                        if (y2 == TRUE) {
                                wordrange<-probmatrix2[(rownames(probmatrix2) == pre) == TRUE,]
                        }
                        if (z2 == TRUE) {
                                wordrange<-probmatrix3[(rownames(probmatrix3) == pre) == TRUE,]
                        }
                }
                ## If a result was found from the above algorithm
                if (exists("wordrange") == TRUE){
                        wordrange<-sort(wordrange,decreasing = TRUE)
                        if (preword(pre,1) %in% stopen){
                                wordrange<-wordrange[!(names(wordrange) %in% stopen)]
                        }
                        result<-names(wordrange[1])
                }else{
                        ## if no result was found. The algorithm will
                        ## return the top three words in the English language
                        result<-c("the")
                }
                return(result)
        }
        ## Wrap up of the two above word suggestion functions
        wordsuggestionwrap<-function(pre){
                if (preword(pre,1) %in% stopen){
                        wordsuggestion2(pre) 
                }else{
                        wordsuggestion(pre)
                }
                
        }
        
        output$suggestion<-renderText(wordsuggestionwrap(input$Text))
        
})

shinyApp(ui = ui,server = server)