

shinyUI(navbarPage(h3("FlowUTI"),theme=shinytheme("sandstone"),
####### INSTRUCTIONS ##############################################
                   tabPanel(h5("Instructions") ,
                            column(11,br(),br(),
                                   h4(strong(helpText("FlowUTI Application"))),
                                   
                                   p(div("FlowUTI is an interactive web application drawn up for both validation and exploratory testing of data obtained from flow cytometry analyzers used in urinary tract infection screening.
                                            This easy-to-use application will allow clinical microbiologist to optimize the use of urine flow cytometry, improving decision-making and management of laboratory resources.
                                            This app is divided in three sections:",align="justify")),
                                   p("1-	Upload data: Browse, upload and data visualization."),
                                   p("2-	Diagnostic accuracy: calculation of diagnostic parameters and density graph display for culture results."),
                                   p(div("3-	Table of sensitivity and specificity: sensitivity and specificity for different cut-points, calculated by using bootstrapping (n=1000). Sensitivity and specificity graph, and overall accuracy expressed as area under the ROC curve (AUC).",align="justify")),
                                  
                                   
                                   br(),br(),
                                   
                                   strong(helpText("Recommendations:",style="strong")),
                                   p(" The database should be composed at least by two columns, one for the culture result (gold standard), and other for the independent variable of interest (e.g. bacteria or leukocyte counts)."),
                                   p(" The input dataset must be in comma-separated values (CSV) file format without row names."),
                                   p(" It is important to bear in mind that before conducting a screening study is crucial to determine a sufficient sample size (1,2)."),
                                   
                                   br(),
                                   
                                   strong(helpText ("Bibliography:",style="strong")),
                                   p("1.-Buderer NM. Statistical methodology: I. Incorporating the prevalence of disease into the sample size calculation for sensitivity and specificity. Acad Emerg Med. 1996;3:895-900."),
                                   p("2.-Bujang MA, Adnan TH. Requirements for minimum sample size for sensitivity and specificity snalysis. J Clin Diagn Res. 2016;10:YE01-YE06.")
                                   )
                                   ),                                      
                   
######### DATA DISPLAY #############################################

tabPanel(h5(" Upload data "),
                            
sidebarPanel(
 
                            
fileInput("file", "Select file in CSV format to upload",
          multiple = FALSE,
          accept = c(".csv")),
br(),br(),
numericInput("ntabla","Nº of records to show",10),
br(),br(),
h4(strong(div(uiOutput("checkbox"),style = "color:blue")))

),
                          
    mainPanel(
              h3(textOutput("texta")),
              h4(textOutput("texta1")),
              h4(textOutput("texta2")),
             
              tableOutput("ex1")
             
              )
              ),

######### DIAGNOSTIC VALIDATION ###############################################################

tabPanel(h5("Diagnostic accuracy"),
         br(),
         
         column(3,
                wellPanel(
                  uiOutput("vdecorte"),
                  br(), br(),
                  uiOutput("gold"),
                  br(), br(),
                  
                  numericInput("smin"," Slider minimum value ",0),
                  numericInput("smax"," Slider maximum value",1000),
                  numericInput("step"," Slider interval",1)),
                
                fluidRow({column(3,offset = 2,
                                 downloadButton("Report", "Report"))})
         ),
         
         #__________________ Slider-bar and tables (tableOutput) ______________ 
         
         column(4,
                fluidRow(
                  p(h4(strong("Select threshold"))),
                  uiOutput("corte")),
                
                fluidRow(
                  p(h4(strong("Diagnostic accurary "))),
                  strong(textOutput("Textocorte1")),
                  tableOutput("resulcorte"))),
         
         #_________________ Density graph (plotOutput) ______________________________________________
         
         column(5,
                fluidRow(
                  plotOutput("curvas",height = "200px",width="400px",
                             
                             brush = brushOpts(
                               id = "curvas_brush",
                               resetOnNew = TRUE)),
                  br(),
                  plotOutput("plot3", height = "400px",width="590px"),
                  br())),
         column(4,offset=8,
                fluidRow(
                  p(h6(div(textOutput("expgraf"),align="left")))
                  
                ))),

############################## TABLES ######################################################
tabPanel(h5("Tables of sensitivity and specificity"),
         br(),
       
         column(3,
                wellPanel(
                  uiOutput("vdetabla"),
                  br(),br(),
                  uiOutput("gold2"),
                  br(),br(),
                  numericInput("lsup"," Marker maximum value",1000),
                  br(),br()),
                br()
               
         ),
         #__________________SPACE FOR TABLE (tableOutput) _____________________________________________
         column(4,        
                h4(textOutput("cabecera")),
                strong(textOutput("subcabecera")),
                br(),br(),
                tableOutput("tabla")
         ),
         
         
         #________SPACE FOR GRAPHS WITH TABLES (plotOutput) ________________________________________________
         column(5,
                plotOutput("sensi", height = "300px",width = "75%"),
                plotOutput("roc", height = "300px",width = "75%"))
)
###########################################################################################
))
