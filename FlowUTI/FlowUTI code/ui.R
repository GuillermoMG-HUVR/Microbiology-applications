####### Web Building ##############################################
#_______ tabpanel web _____________________________________________________

shinyUI(navbarPage("FlowUTI app",theme=shinytheme("sandstone"), 
                   tabPanel(" Use your data ", titlePanel(h4("First step: uploading your data")),
                            
 # Input side panel:
                            
sidebarPanel(
  tags$style(type='text/css', ".well { max-width: 20em; }"),
  
    # Choose type of file:
                              
    selectInput("readFunction", "Choose type of file",
              c("read.csv",
                "read.spss",
                "read.dta"
                )),
                              
    fileInput("file", "Your input file:")
                 ),
                            
                            
########## Display data main panel (using DT) #############                          
    mainPanel(
              h3(textOutput("texta")),
              h4(textOutput("texta1")),
              h4(textOutput("texta2")),
              DT::dataTableOutput('ex1')
              )
              ),


        ##################################################################################################
######### DESCRIPTIVE STATISTICS                   
        ##################################################################################################      
        tabPanel("Descriptive statistics ", titlePanel(h4("Simple summaries about the sample and the measures, together with simple graphics analysis")),
                 fluidPage( 
                   fluidRow( 
                     column(3,uiOutput("vars2"),
                            
                            div(textOutput("text4"),style = "color:blue"), 
                            div(textOutput("text5"),style = "color:blue"),
                            br(),br(),
                            
                            uiOutput("clase"),
                            br(),
                            uiOutput("his"),
                            br(),
                            uiOutput("normal1")
                     ),
                     
                     column(3, 
                            h4("Variable "),
                            h4(textOutput("text1")),
                            textOutput("text2"),
                            tableOutput("tabla1"),
                            
                            conditionalPanel(condition = "class(variableInput())=='factor'",
                                             tableOutput("tabla2")),
                            strong(em((textOutput("text3"))))),
                     
                     
                     column(4,  
                            plotOutput("grafico"),height = "300px"))),
                 
                 
                 fluidRow(
                   column(3,strong(em(textOutput("normalidad2")))),
                   column(3,uiOutput("slider1")),
                   column(4,plotOutput("graficob"),height = "300px")
                 )
        ),
        

########################## VALIDATING DIAGNOSTIC TEST ###############################################################
tabPanel("Screening test",
        br(),
#____________ Side panel and inputs ________________________  

           column(3,
                  wellPanel(
                  
                  uiOutput("vdecorte"),
                  br(), br(),
                  uiOutput("gold"),
                  br(), br(),
           #),
                  #wellPanel(
           numericInput("smin","Minimum value - slider",0),
           numericInput("smax","Maximum value - slider",100),
           numericInput("step","step",1))),

#__________________  (tableOutput) ______________ 

           column(4,
                  fluidRow(
                  p(h4(strong("Choose your cut-off point"))),
                  uiOutput("corte")),
         
                  fluidRow(
                  p(h4(strong("Statistics obtained according to the selected cut-off point"))),
                  strong(textOutput("Textocorte1")),
                  tableOutput("resulcorte"))),

#_________________ (plotOutput) ______________________________________________

           column(5,
                  br(),br(),br(),br(),
                  br(),br(),br(),br(),
                  fluidRow(
                  plotOutput("curvas"),height = "600px")
                 
           )),

############################## tables ######################################################
tabPanel("Diagnostic utility of Flow Cytometry",
         br(),
#___________________ Variable side panel _____________________________
         column(3,
                wellPanel(


uiOutput("vdetabla"),
br(),br(),
uiOutput("gold2"),
br(),br(),
selectInput("boot","n boot",c(2,100,500,1000,2000,10000),2)
)),
#__________________  (tableOutput) _____________________________________________
                  column(4,        
         h4(textOutput("cabecera")),
         strong(textOutput("subcabecera")),
         br(),br(),
         DT::dataTableOutput("tabla")
),
    

#________Graphic - table (plotOutput) ________________________________________________
column(5,
       plotOutput("sensi"),height = "300px",
       plotOutput("roc"),height = "300px")
),

###########################################################################################
tabPanel("Instructions" ,
         column(11,br(),br(),
                
                helpText("Esta aplicación puede ser utilizada como herramienta para la validación de pruebas diagnosticas.
                         La App consta de cuatro secciones:"),
                helpText("1-	Búsqueda descarga y visualización de la base de datos."),
                helpText("2-	 Análisis estadístico descriptivo."),
                helpText("3-	 Calculo de sensibilidad, especificad, valores predictivos, exactitud, odds ratio diagnóstica, índice de Youden y razones de verosimilitud. Gráfico de densidad interactivo para distintos puntos de corte de la variable de estudio."),
                helpText("4-	Tabla de sensibilidad y especificidad para distintos puntos de corte, calculadas mediante boostrapping. Gráfico de sensibilidad y especificidad. Curva ROC con el área bajo curva calculada."),
                helpText("Para los cálculos principales, la App utiliza los paquetes", strong("epiR")," de Mark Stevenson y cols. (sección 3) y",strong( "pROC ")," de Xavier Robin et al. para la sección 4."),
                
                br(),br(),
                
                strong(helpText("Recomendaciones de uso:",style="strong")),
                helpText(" La base de datos contendrá, al menos, el gold estándar y una variable de estudio. El formato de la base de datos puede ser csv, spss o stata."),
                helpText("Gold estándar: debe ser una variable tipo factor dicotómica."),
                helpText("La(s) variable(s) de estudio que constituyen los resultados de la prueba a evaluar deben ser del tipo “numeric” o “integer” "),
                helpText("En la sección 4, los cálculos se realizan mediante un re muestreo no paramétrico estratificado. 
                         El número de repeticiones para el cálculo, se controla por la ventana n.boot. Al objeto de obtener una respuesta rápida, su valor por defecto es 2, pero evidentemente se necesita un numero de repeticiones mucho más alto para obtener una estimación precisa de los estadísticos y sus intervalos de confianza. Los autores recomiendan un n.boot  mínimo de  2000.
                         Es necesario tener en cuenta que valores altos de n.boot consumen un tiempo considerable.")
                      )
                )
         
                ))

