shinyServer(function(input, output) {
  #___________Using your data__________________________
  
  
  # arguments
  ArgNames <- reactive({
    Names <- names(formals(input$readFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  # import bd
  Dataset <- reactive({
    if (is.null(input$file)) {
      return(data.frame())
    }
    
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    argList <- argList[names(argList) %in% ArgNames()]
    Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
    return(Dataset)
  })
  

#___________Creation of variable groups __________________________
 
 factores<-reactive({
   Dataset()%>%select_if(is.factor)
 })
 fnum<-reactive({
   Dataset() %>% select_if(function(col) is.integer(col) && n_distinct(col)<5)
   
 })
 fnum2<-reactive({
   fnum2<-as.factor(fnum())
 })
 FACTORES<-reactive({
   bind_cols(factores(),fnum())
 }) 
 FACTORES_2CAT<-reactive({
   select(FACTORES(), length(2))
 }) 
 numericas<-reactive({
   Dataset()%>%select_if(function(col)  is.numeric(col) | is.integer(col))
 })  
 NUMERICAS<-reactive({
   numericas() %>% select_if(function(col) n_distinct(col)>5)
 })   
  
 ################## Showing database ########################################## 
  
  output$texta<-renderText({
  
    paste("Database",input$file[1] )
  })
  
  output$texta1<-renderText({
    nombrebd1<-length(Dataset())
    paste(nombrebd1," Variable ")
  })
  
  output$texta2<-renderText({
        f2<-dim(Dataset())[1]
        paste (f2," Items")
  })
  
  # Showing table:

   output$ex1 <- DT::renderDataTable(
    DT::datatable(Dataset(), list(columnDefs = list(list(className = 'dt-rigth')),
    searching = FALSE,pageLength=5,lengthMenu=c(5,10,20),
    style = "default",class = "compact",rownames=FALSE)
   
  ))
  
 ##################### Variable selection ################################################# 
    output$vars2 <- renderUI({
      selectInput("vars2", "Choose date variable",
      names(Dataset()), names(Dataset()), multiple =FALSE)
    })
    
################ Aspect ggtherm ##########################################################    
    #ggthemr('flat', layout = 'scientific')
    
################################################################################################ 
    
    ############################################################################################
#### DESCRIPTIVE STATISTICS                   
    ############################################################################################
    
    output$clase = renderUI({
      if(is.integer(Dataset()[,input$vars2])|is.numeric(Dataset()[,input$vars2])){
        selectInput("Others","Choose other statistics", 
                    choices=list("No","Standard deviation","Interquartile range","Range","Variance"),selected="No")
      }
    })
    
    output$his = renderUI({
      if(is.integer(Dataset()[,input$vars2])|is.numeric(Dataset()[,input$vars2])){
        selectInput("his2","I prefer another graphic", 
                    choices=list("No","Histogram","Frequency polygon","Density plot","Histogram + density"),selected="No")
      }
    })
    
    output$normal1 = renderUI({
      if(is.integer(Dataset()[,input$vars2])|is.numeric(Dataset()[,input$vars2])){
        selectInput("normal2","Normality test",
                    choices=list("No","Shapiro Wilk","Kolmogroff Smirnoff","Lilliefors","Anderson Darling"),selected="No")          
      }
    })
    
    output$text1<-renderText({
      paste(c(input$vars2))
    })
    
    output$text4<-renderText({
      cl<-class(Dataset()[,input$vars2])
      paste("Variable tipo ",cl)
    })
    
    output$text5<-renderText({
      f<-sum(is.na(Dataset()[,input$vars2]))
      paste(f,"Missing values")
    })
    
    output$text6<-renderText({
      paste(".")
    })
    
    output$text3<-renderText({
      if(is.integer(Dataset()[,input$vars2])|is.numeric(Dataset()[,input$vars2])){
        if(input$otros=="Standard deviation"){
          res<-round(sd(Dataset()[,input$vars2],na.rm = TRUE),2)
          paste (c(input$otros," ",res))
        }
        else{
          if(input$otros=="Interquartile range"){
            res1<-round(IQR(Dataset()[,input$vars2],na.rm = TRUE),2)
            paste (c(input$otros," ",res1))}
          else{
            if(input$otros=="Range"){
              res1<-round(range(Dataset()[,input$vars2],na.rm = TRUE),2)
              paste (c(input$otros," ",res1))} 
            else{
              if(input$otros=="Variance"){
                res1<-round(var(Dataset()[,input$vars2],na.rm = TRUE),2)
                paste (c(input$otros," ",res1))} 
            }}}}
     
    })
    
    output$normalidad2<-renderText({
      if(is.integer(Dataset()[,input$vars2])|is.numeric(Dataset()[,input$vars2])){
        if(input$normal2=="Shapiro Wilk"){
          s<- shapiro.test(Dataset()[,input$vars2])
          paste("p value = ",s$p.value)
        }
        else{
          if(input$normal2=="Kolmogroff Smirnoff"){
            s1<-ks.test(Dataset()[,input$vars2],pnorm,mean(Dataset()[,input$vars2],na.rm = TRUE),sd(Dataset()[,input$vars2],na.rm = TRUE))
            paste("p value = ",s1$p.value)
            
          }
          else{
            if(input$normal2=="Lilliefors"){
              s2<-lillie.test(Dataset()[,input$vars2])
              paste("p value = ",s2$p.value)
              
              
            }
            else{
              if(input$normal2=="Anderson Darling"){
                s3<-ad.test(Dataset()[,input$vars2])
                paste("p value = ",s3$p.value)
                
              }
            }}}
      }
    })
    
    
    output$tabla1<-renderTable({
     
      if(is.integer(Dataset()[,input$vars2])|is.numeric(Dataset()[,input$vars2])){
        tabla<-matrix(c(min(Dataset()[,input$vars2],na.rm = TRUE),max(Dataset()[,input$vars2],na.rm = TRUE),
                        mean(Dataset()[,input$vars2],na.rm = TRUE),quantile(Dataset()[,input$vars2],0.25,na.rm = TRUE),
                        median(Dataset()[,input$vars2],na.rm = TRUE),quantile(Dataset()[,input$vars2],0.75,na.rm = TRUE)))
        rownames(tabla) = c("Minimum","Maximum","Mean","First Quartile","Median","Third Quartile")#,"Missing")
        colnames(tabla) = c("Value")
        na.omit(tabla)
        tabla
      }

        else{
          if(class(Dataset()[,input$vars2])=="factor"){
            .Table<-with(Dataset(), table(Dataset()[,input$vars2],useNA="no"))
            
            tb<-matrix(.Table)
            rownames(tb) = c(names(table(Dataset()[,input$vars2],useNA="no")))
            colnames(tb)=c("Frecuencia")
            tb
          }
        }
    },bordered = FALSE,  
    rownames = TRUE,  
    digits = 3) 
    
    output$tabla2<-renderTable({
      if(class(Dataset()[,input$vars2])=="factor"){
        .Table2<-with(Dataset(),table(Dataset()[,input$vars2],useNA="no"))
        perc2<-(round(100*.Table2/sum(.Table2), 2))
        tb2<-matrix(perc2)
        rownames(tb2) = c(names(table(Dataset()[,input$vars2],useNA="no")))
        colnames(tb2)=c("Percentage")
        tb2
      }
      
    },
    bordered = FALSE,  
    rownames = TRUE,  
    digits = 3) 
    
    chica<-reactive({
      filter(Dataset(),input$vars2<300)
    })
    
    output$grafico<-renderPlot({
      
      bdInput<-filter(Dataset(),complete.cases(Dataset()))
      if(is.integer(Dataset()[,input$vars2])|is.numeric(Dataset()[,input$vars2])){
        
        a<-ggplot(bdInput,aes(x="",y=bdInput[,input$vars2]))+
          geom_boxplot(fill="salmon3")+
          ylim(min(bdInput[,input$vars2]),quantile(bdInput[,input$vars2],0.75))+
          scale_x_discrete(name="")+
          stat_summary(fun.y=mean, geom="point", shape=21, size=2)+
          labs(y=input$vars2)+
          ggtitle(paste("Distribution of the variable", input$vars2," (recortada)"))+
          theme(plot.title = element_text(size = 16,face="bold"))
        
        print(a)
      }

        else{
          if(class(Dataset()[,input$vars2])=="factor"){ 
            
            b<-ggplot(bdInput,aes(factor(bdInput[,input$vars2])))
            
            b1<-b+geom_bar(fill="salmon3",color="peru")#fill="006600",color="black")
            
            b1<-b1+ggtitle(paste("Distribution of the variable", input$vars2) )+
              labs(x=input$vars2,y="Frequency")+
              theme(plot.title = element_text(size = 18,face="bold"))
            b1
          }
        }
    })
    
    output$graficob<-renderPlot({
      
      bdInput<-filter(Dataset(),complete.cases(Dataset()))
      if(is.integer(Dataset()[,input$vars2])|is.numeric(Dataset()[,input$vars2])){
        ls<-max(Dataset()[,input$vars2])+10
        if(input$his2=="Histogram"){
          c<-ggplot(bdInput,aes(x=bdInput[,input$vars2]))+
            geom_histogram(fill="salmon3",color="black",na.rm = TRUE)+
            #xlim(min(bdInput[,input$vars2]),max(bdInput[,input$vars2]))+
            #ylim(0,300)+
          labs(x=input$vars2,y="Frequency")
          print(c)
        }
        else{
          if(input$his2=="Frequency polygon"){
            c<-ggplot(bdInput,aes(x=bdInput[,input$vars2]))+
              geom_freqpoly(color="salmon4")+
                            #,binwidth=10)
            
              #xlim(0,max(bdInput[,input$vars2]))+
              labs(x=input$vars2,y="Frequency")
            print(c)
          }
          else{
            if(input$his2=="Density"){
              ggplot(data=bdInput,aes(x=bdInput[,input$vars2])) + 
                geom_density( fill="salmon1", alpha=.2)+
                #xlim(0,500)+
                labs(x=input$vars2,y="Density")
            } 
            else{
              if(input$his2=="Histogram + density"){
                c<-ggplot(bdInput,aes(x=bdInput[,input$vars2]))+
                  geom_histogram(aes(y=..density..),fill="salmon3",color="black")+
                  geom_density(fill="sienna", alpha=.2)+#xlim(0,500)+
                  labs(x=input$vars2,y="Density")
                print(c)
              }   
            }}}}
     
    })
    
##################################################################################################
  
    ############################ VALIDATING DIAGNOSTIC TEST ##########################################
    output$vdecorte<-renderUI({
      selectInput(inputId = "vdecorte", "Analysis variable",
                names(numericas()))
    })
    output$gold<-renderUI({
      selectInput(inputId = "gold", "Gold standar",
                  names(FACTORES()))
    })
    
   
    output$corte <- renderUI({
      if(length(input$vdecorte)>0){
         sliderInput(inputId = "corte",
                  label = paste("Punto de corte, variable ",input$vdecorte),
                  min= input$smin, max =input$smax, value =1, step = input$step,
                  animate=T)
    }
      })
    
    output$Textocorte1<-renderText({
      if(length(input$vdecorte)>0){
      paste("Variable",input$vdecorte, "con punto de corte ",input$corte)}
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
      col11<-c("Sensibilidad","Especificidad","Proporcion de falsos positivos","Proporcion de falsos negativos",
               "Valor predictivo positivo","Valor predictivo negativo","Exactitud",
               "Odds ratio diagnostica","Indice de Youden","Razon de verosimilitud positiva","Razon de verosimilitud negativa")
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
      ret1$col21<-round(as.numeric(as.character(ret1$col21)),3)
      ret1$col31<-round(as.numeric(as.character(ret1$col31)),3)
      ret1$col41<-round(as.numeric(as.character(ret1$col41)),3)
      colnames(ret1)<-c("Medida","Valor","Inferior","Superior")
      retlist1<-list(ret1)}
    })
    
    output$curvas <- renderPlot({
      corte=input$corte
      if(length(input$vdecorte)>0){    
      ## Grafico de densidad ##
      
        corte=input$corte
        subcit <- subset(Dataset(), subset=Dataset()[input$vdecorte]<100)
        dtb    <- data.table(subcit[,input$gold],subcit[,input$vdecorte])
        ggplot(dtb,aes(subcit[,input$vdecorte],fill=subcit[,input$gold]))+
        geom_density(alpha=0.55)+
          xlim(0,input$smax)+
        geom_vline(xintercept = corte,col=2)+
        labs(x=input$vdecorte,y="Densidad",title=paste("Curvas de densidad con punto de corte ",corte,input$vdecorte))+
        theme(legend.position="top")+
        scale_fill_discrete(name=input$gold)+
        theme(axis.text.y=element_text( size=12, vjust=0.5),
              plot.title = element_text(size = 12),axis.text.x=element_text( size=12, 
              vjust=0.5), axis.title.x=element_text( size=12, vjust=0.5),
              axis.title.y=element_text( size=12, vjust=0.5))+
        theme(legend.text = element_text(size = 10))
          #geom_ribbon(aes(ymin=0,ymax=density(subcit[,input$vdecorte]),x=corte))
      } 
    })
################### TABLAS ###############################################    
    output$vdetabla<-renderUI({
      selectInput(inputId = "vdetabla", "Variable de analisis",
                  names(numericas()))
    })
    output$gold2<-renderUI({
      selectInput(inputId = "gold2", "gold standar",
                  names(FACTORES()))
    })
    output$cabecera <- renderText({ 
      if(length(input$vdetabla)>0){
      paste("Medianas de Sensibilidad y especifidad por punto de corte (bootstraping)")}
      
    })
    output$subcabecera <- renderText({
      if(length(input$vdetabla)>0){
      paste("Variable",input$vdetabla)}
      
    })
    output$tabla <- DT::renderDataTable({
      if(length(input$vdetabla)>0){

        bd1<-filter(Dataset(),complete.cases(Dataset()))
        mat2<-ci.thresholds(bd1[,input$gold],bd1[,input$vdetabla],boot.n=input$boot, conf.level=0.9, stratified=FALSE)
        mat2<-as.data.frame(mat2)
        #mat2r<-select(mat2,sensitivity.50.,specificity.50.)
        mat2<-select(mat2,sensitivity.50.,specificity.50.)
        mat2$sensitivity.50.<-as.numeric(mat2$sensitivity.50.)
        mat2$sensitivity.50.<-round(mat2$sensitivity.50.,3)
        mat2$specificity.50.<-as.numeric(mat2$specificity.50.)
        mat2$specificity.50.<-round(mat2$specificity.50.,3)
        #mat2<-mutate(mat2,cutpoint=rownames(mat2))
        #mat2$cutpoint<-as.numeric(mat2$cutpoint)
        #xtable(mat2)
        DT::datatable(mat2,list(pageLength=10,lengthMenu=c(10,20,30)),
                               style = "default",class = 'cell-border stripe',rownames=TRUE,
                                selection = 'none'
                               )
        
      }
    }) 

output$sensi<-renderPlot({
  #if(input$tipotabla=="Medianas"){
  s = input$tabla_rows_selected
  bd1<-filter(Dataset(),complete.cases(Dataset()))
  mat2<-ci.thresholds(bd1[,input$gold],bd1[,input$vdetabla],boot.n=input$boot, conf.level=0.9, stratified=FALSE)
  mat2<-as.data.frame(mat2)
  if(length(input$vdetabla)>0){
  mat2<-mutate(mat2,variable=rownames(mat2))
  mat2$variable<-as.numeric(mat2$variable)
  mat2r<-filter(mat2,variable<100)
  ggplot(mat2r,aes(x=variable))+geom_line(aes(y=sensitivity.50.),color="blue")+
    labs(x=input$vdetabla,y="sensibilidad/especificidad")+
    geom_ribbon(aes(ymin=sensitivity.5.,ymax=sensitivity.95.), alpha=0.2)+
    geom_line(aes(y=specificity.50.),color="red")+
    geom_ribbon(aes(ymin=specificity.5.,ymax=specificity.95.), alpha=0.2)+
    ggtitle("Sensibilidad y especificidad,según punto de corte.\n En azul la sensibilidad y en rojo la especificidad")+
    theme(plot.title = element_text(hjust = 0.5))
  #}
  }
})
output$roc<-renderPlot({
  s = input$tabla_rows_selected
  cr<-roc(Dataset()[,input$gold2],Dataset()[,input$vdetabla])
  plot.roc(cr,print.auc=TRUE,auc.polygon=TRUE, grid=c(0.1, 0.2),
           grid.col=c("green", "red"), max.auc.polygon=TRUE,
           auc.polygon.col="gainsboro",main="Curva ROC")
  plot.roc(smooth(cr),add=TRUE, col="blue")
})

})