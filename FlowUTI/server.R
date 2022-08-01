shinyServer(function(input, output) {

  #___________Upload database__________________________
  
  output$checkbox<-renderUI({
    if (is.null(input$file)) {
      selectInput("checkbox", label = "Demos data",
                  choices = c("Elderly","Neonates","None"),selected = "None",
                  multiple = FALSE,width ="90%") }
  }) 
  #___________Separacion en documento csv__________________________
  output$ArgText <- renderUI({
    selectInput("sep","Arg Sep type:",choices = c("Comma","Semicolon"))
  })
  
  Dataset <- reactive({
    if (is.null(input$file)) {
      if (input$checkbox=="None"){ 
        return(data.frame())
      }
      else{ 
        if (input$checkbox=="Elderly"){
          return(as.data.frame(Elderly))}
        else{
          if(input$checkbox=="Neonates")
          {return(Neonates)}
        }}
    }
    else{
      if(input$sep=="Comma"){
        read.csv(input$file$datapath,
                 header = TRUE,
                 sep = ",",
                 quote = '"')}
      else{
        if(input$sep=="Semicolon"){
          read.csv(input$file$datapath,
                   header = TRUE,
                   sep = ";",
                   quote = '"')}
      }}
  })
  ################## Database view ########################################## 
  output$texta<-renderText({
    if (is.null(input$file)) {
      if(input$checkbox=="None"){
        paste("Select file in CSV format" )
      }
      else{
        if (input$checkbox=="Elderly")
        { paste("Elderly demo database" )}
        else{
          if(input$checkbox=="Neonates")
          { paste("Neonates demo database" )}
        }}
    }
    else{
      paste("File",input$file[1] )
    }
  })
  
  output$texta1<-renderText({
    nombrebd1<-length(Dataset())
    if (is.null(input$file)) {
      if(input$checkbox=="None"){
        paste("" )
      }
      else{paste("3 variables" )
      }}
    else{
      paste(nombrebd1," Variables ")
    }
  })
  
  output$texta2<-renderText({
    f2<-dim(Dataset())[1]
    f2d1<-dim(Elderly)[1]
    f2d2<-dim(Neonates)[1]
    if (is.null(input$file)) {
      if(is.null(input$checkbox)){
        paste("" )
      }
      else{
        if (input$checkbox=="Elderly")
        { paste(f2d1,"Records" )}
        else{
          if(input$checkbox=="Neonates")
          { paste(f2d2,"Records" )}
        }}
    }
    else{
      paste (f2," Records")}
  })
  
  output$ex1<-renderTable({
      dplyr::slice(Dataset(), 1:input$ntabla)
  })  
 
  ####################### Variables  #############################################  
  factores<-reactive({
    fac1<-Dataset()%>%select_if(is.factor)
    fac2<-Dataset()%>%select_if(is.character)
    factores<-bind_cols(fac1,fac2)
  })
  
  #fnum<-reactive({
   # Dataset() %>% select_if(function(col) is.integer(col) && n_distinct(col)<5)
  #})
  
  #FACTORES<-reactive({
   # bind_cols(factores(),fnum())
  #}) 
  
  numericas<-reactive({
    Dataset()%>%select_if(function(col)  is.numeric(col) | is.integer(col))
  })  
  
  output$vdecorte<-renderUI({
    selectInput(inputId = "vdecorte", "Marker",
                names(numericas()))
  })
  output$gold<-renderUI({
    if(input$checkbox=="Elderly"){ge<-"Culture"}else{
      if(input$checkbox=="Neonates"){ge<-"Culture_result"}else{
        ge<-names(Dataset())
      } }
    
    selectInput(inputId = "gold", "Gold standar", ge)
    #selectInput(inputId = "gold", "Gold standar",factores())
  })
  
  ############################ Validation ##########################################
  
  output$corte <- renderUI({
    if(length(input$vdecorte)>0){
      sliderInput(inputId = "corte",
                  label = paste("Cut point",input$vdecorte, "variable "),
                  min= input$smin, max =input$smax, value =1, step = input$step,
                  animate=T)
    }
  })
  
  output$Textocorte1<-renderText({
    if(length(input$vdecorte)>0){
      paste(input$vdecorte,"variable with cut-point ",input$corte)}
  })
  
  output$resulcorte <- renderTable({
    if(length(input$vdecorte)>0){
      corte=input$corte
      ts <- subset(Dataset(), subset=Dataset()[,input$vdecorte]>=corte)
      ti<-subset(Dataset(), subset=Dataset()[,input$vdecorte]<corte)
      
      .Tablea <- with(ts, table(ts[,input$gold]))
      vp<-.Tablea[2]
      fp<-.Tablea[1]
      
      .Tableb <- with(ti, table(ti[,input$gold]))
      vn<-.Tableb[1]
      fn<-.Tableb[2]
      
      vp1<-vp
      fn1<-fn
      fp1<-fp
      vn1<-vn
      dat <- as.table(matrix(c(vp1,fp1,fn1,vn1), nrow = 2, byrow = TRUE))
      rval <- epi.tests(dat, conf.level = 0.95)
      col11<-c("Sensitivity","Specificity","Proportion of false positives","Proportion of false negatives",
               "Positive predictive value","Negative predictive value","Diagnostic accuracy (effectiveness)",
               "Diagnostic odds ratio","Youden's index","Likelihood ratio for positive test","Likelihood ratio for negative test")
      col21<-c(rval$sensitivity$est,rval$specificity$est,
               1-rval$specificity$est,1-rval$sensitivity$est,
               rval$pv.positive$est,rval$pv.negative$est,
               rval$diag.acc$est,rval$diag.or$est,rval$youden$est,
               rval$lr.positive$est,rval$lr.negative$est)
      col31<-c(rval$sensitivity$lower,rval$specificity$lower,1-rval$specificity$upper,
               1-rval$sensitivity$upper,rval$pv.positive$lower,rval$pv.negative$lower,
               rval$diag.acc$lower,rval$diag.or$lower,rval$youden$lower,rval$lr.positive$lower,
               rval$lr.negative$lower)
      col41<-c(rval$sensitivity$upper,rval$specificity$upper,1-rval$specificity$lower,
               1-rval$sensitivity$lower,rval$pv.positive$upper,rval$pv.negative$upper,
               rval$diag.acc$upper,rval$diag.or$upper,rval$youden$upper,rval$lr.positive$upper,
               rval$lr.negative$upper)
      ret1<-as.data.frame(cbind(col11,col21,col31,col41))
      ret1$col21<-round(as.numeric(as.character( ret1$col21)),3)
      ret1$col31<-round(as.numeric(as.character( ret1$col31)),3)
      ret1$col41<-round(as.numeric(as.character( ret1$col41)),3)
      colnames(ret1)<-c("Measure","Value","CI low","CI up")
      xtable(ret1)}
  })
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$curvas <- renderPlot({
    ranges2 <- reactiveValues(x = NULL, y = NULL)
    corte=input$corte
    if(length(input$vdecorte)>0){    
      ## Grafico de densidad ##
      
      corte=input$corte
      ggplot(Dataset(),aes(Dataset()[,input$vdecorte],fill=Dataset()[,input$gold]))+
        geom_density(alpha=0.55)+
        xlim(0,input$smax)+
        geom_vline(xintercept = corte,col=2)+
        
        labs(title=paste("Select display area"),x=input$vdecorte,y="Density")+
        theme(legend.position="rigth")+
        scale_fill_discrete(name=input$gold)+
        theme(axis.text.y=element_text( size=12, vjust=0.5),
              plot.title = element_text(size = 14),axis.text.x=element_text( size=12, 
                                                                             vjust=0.5), axis.title.x=element_text( size=12, vjust=0.5),
              axis.title.y=element_text( size=12, vjust=0.5))+
        theme(legend.text = element_text(size = 10))
      
    } 
  })
  
  output$plot3 <- renderPlot({
    
    corte=input$corte
    if(length(input$vdecorte)>0){    
      ## Grafico de densidad ##
      
      corte=input$corte
      ggplot(Dataset(),aes(Dataset()[,input$vdecorte],fill=Dataset()[,input$gold]))+
        geom_density(alpha=0.55)+
        xlim(0,input$smax+20)+
        geom_vline(xintercept = corte,col=2)+
        labs(x=input$vdecorte,y="Density",title=paste("Density plot with threshold ",corte,input$vdecorte))+
        theme(legend.position="top")+
        scale_fill_discrete(name=input$gold)+
        theme(axis.text.y=element_text( size=12, vjust=0.5),
              plot.title = element_text(size = 16),axis.text.x=element_text( size=12,vjust=0.5), axis.title.x=element_text( size=12, vjust=0.5),
              axis.title.y=element_text( size=12, vjust=0.5))+
        theme(legend.text = element_text(size = 10))+
        coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
    }
  })
  #___________Selección area del gráfico__________________________  
  observe({
    
    brush <- input$curvas_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  #___________Texto auxiliar__________________________
  output$expgraf<-renderText({
    paste("The densities shown in this figure correspond to a subsample that meets the condition ",
          input$vdecorte," is less than ",input$smax,
          ". The real densities would be obtained with a value in the x axis equal to the maximum value of the variable", input$vdecorte,"(",max(Dataset()[,input$vdecorte]),")")
  }) 
  
  ############################ Report ##########################################
  
  output$Report <- downloadHandler(
    filename = function() {
      
      "my_report.html"
    },
    
    content = function(file) {
      src <- normalizePath('informeHTML.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'informeHTML.Rmd', overwrite = TRUE)
      out <- render('informeHTML.Rmd',html_document()
                    
      )
      file.rename(out, file)
    })
  
  ################### TABLES sens and spec ###############################################
  
  output$vdetabla<-renderUI({
    selectInput(inputId = "vdetabla", "Marker",
                names(numericas()))
  })
  output$gold2<-renderUI({
    if(input$checkbox=="Elderly"){ge<-"Culture"}else{
      if(input$checkbox=="Neonates"){ge<-"Culture_result"}else{
        ge<-names(Dataset())
      }}
    selectInput(inputId = "gold2", "Gold standar",ge)
    #names(Dataset()))
  })
  output$cabecera <- renderText({ 
    if(length(input$vdetabla)>0){
      paste("Sensitivity and specificity according to thresholds")}
    
  })
  output$subcabecera <- renderText({
    if(length(input$vdetabla)>0){
      paste("Variable",input$vdetabla)}
  })    
  
  output$tabla<-renderTable({
    if(length(input$vdetabla)>0){
      bd1<-dplyr::filter(Dataset(),complete.cases(Dataset()))
      mat2<-ci.thresholds(bd1[,input$gold2],bd1[,input$vdetabla],boot.n=1000, conf.level=0.9, stratified=FALSE)
      
      mat2<-as.data.frame(mat2)
      mat2<-mutate(mat2,cutpoint=rownames(mat2))
      mat2$cutpoint<-as.numeric(mat2$cutpoint)
      mat2<-dplyr::select(mat2,cutpoint,sensitivity.50.,specificity.50.)
      mat2$sensitivity.50.<-as.numeric(mat2$sensitivity.50.)
      mat2$sensitivity.50.<-round(mat2$sensitivity.50.,3)
      mat2$specificity.50.<-as.numeric(mat2$specificity.50.)
      mat2$specificity.50.<-round(mat2$specificity.50.,3)
      colnames(mat2)<-c("Threshold", "Sensitivity", "specificity")
      xtable(mat2)
    }
  },rownames = FALSE) 
  
 
  
  output$sensi<-renderPlot({
    bd1<-dplyr::filter(Dataset(),complete.cases(Dataset()))
    mat2<-ci.thresholds(bd1[,input$gold2],bd1[,input$vdetabla],boot.n=1000, conf.level=0.9, stratified=FALSE)
    mat2<-as.data.frame(mat2)
    if(length(input$vdetabla)>0){
      mat2<-mutate(mat2,variable=rownames(mat2))
      mat2$variable<-as.numeric(mat2$variable)
      mat2r<-filter(mat2,variable<input$lsup)
      
      ggplot(mat2r,aes(x=variable))+geom_line(aes(y=sensitivity.50.),color="blue")+
        labs(x=input$vdetabla,y="sensitivity/specificity")+
        geom_ribbon(aes(ymin=sensitivity.5.,ymax=sensitivity.95.), alpha=0.2)+
        geom_line(aes(y=specificity.50.),color="red")+
        geom_ribbon(aes(ymin=specificity.5.,ymax=specificity.95.), alpha=0.2)+
        ggtitle("Sensitivity and specificity according to thresholds.\n  In blue the sensitivity and in red the specificity")+
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  ################### ROC curvec ###############################################
  
  output$roc<-renderPlot({
    cr<-roc(Dataset()[,input$gold2],Dataset()[,input$vdetabla])
    df = data.frame(fp=1-cr$specificities, vp=cr$sensitivities)
    tmp <- data.frame(fp=c(0, 1, 1), vp=c(0, 1, 0))
    
    ggplot(data = df, aes(x =fp , y = vp)) +   geom_path(colour = 'navy', size = 1.5)+
      ylab('Sensitivity') + xlab('1-specificity') +
      ggtitle("ROC curve")+
      geom_polygon( data= df, aes(x=fp, y=vp), fill="gray70",alpha = 0.6) +
      geom_polygon(data=tmp, aes(fp, vp), fill="gray70",alpha = 0.6) +
      geom_segment(aes(x=0,y=0,xend=1,yend=1),lty=2,col="gray50", alpha = 0.3)
    
    #p+ annotate("text",label = textlab, size = 5)
    
  })
  output$roctext=renderText({
    cr<-roc(Dataset()[,input$gold2],Dataset()[,input$vdetabla])
    paste("AUC =",round(auc(cr),3))
  })
})


