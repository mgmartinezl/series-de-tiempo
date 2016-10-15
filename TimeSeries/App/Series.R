library(shiny)
library(ggplot2)
library(forecast)
library(TSA)

#Primero, definimos la interfaz de usuario con UI
ui <- shinyUI(fluidPage(
   
   # Definimos el título de la aplicación
   titlePanel("Ajuste y pronóstico de series de tiempo"),
   
   # Definimos el panel de selecciones 
   sidebarLayout( #Define la interfaz en general
      sidebarPanel( #Define la parte lateral de la aplicación
        fileInput('file1', 'Inserte un csv'), #Esto me permite insertar el archivo con los datos
        checkboxInput('header', 'Header', TRUE), #Reviso si tiene encabezado el archivo
        
        radioButtons('modelo', 'Seleccione el modelo de ajuste:', #Selecciones de posibles modelos para ajustar
                   c(Lineal='1',
                     Cuadrático='2',
                     Cúbico='3', 
                     HoltWinters = '4')),
        
        numericInput("pron", "Número de periodos a pronosticar:", 6), #Número de períodos a pronosticar
        numericInput("año", "Año inicial:", 2000), #Año de inicio
        numericInput("periodo", "Periodo inicial:", 1), #Periodo de inicio
        numericInput("frecuencia", "Frecuencia de la serie:", 4), #Frecuencia de la serie (trimestres)
          
        sliderInput("bins",
                    "Número de barras:",
                    min = 1,
                    max = 50,
                    value = 30)
        
       ),
      
      mainPanel( #Dentro de sidebarLayout define el panel de resultados
        # plotOutput("distPlot"), #Arroja una gráfica como resultado
         
         tabsetPanel(type = "tabs", 
                     tabPanel("Ajuste/Predicción", plotOutput("distPlot")),
                     tabPanel("Histograma", plotOutput("histPlot")),
                     tabPanel("Residuales", plotOutput("resPlot"))),
         
         fluidRow(
           tableOutput("table") #Arroja una tabla como resultado
         ),
         
         fluidRow(
           tableOutput("pron") #Arroja una tabla como resultado
         )
         
         #fluidRow(
        #   tableOutput("pronCub") #Arroja una tabla como resultado
         #),
         
         #fluidRow(
          # tableOutput("pronCuad") #Arroja una tabla como resultado
        # ),
         
         #fluidRow(
          # tableOutput("pronLin") #Arroja una tabla como resultado
         #)
         
         )
         
      )
   )
)


# Ahora, programamos la funcionalidad de la aplicación
server <- shinyServer(function(input, output) {
   
   output$distPlot <- renderPlot({ #La salida del gráfico
     
      infile<- input$file1 #Lee el archivo introducido
      
      y = read.csv(infile$datapath, header=input$header) #Añade header del archivo si lo tiene
      y <- ts(y,freq=input$frecuencia, start = c(input$año,input$periodo)) #Convierte los datos a una serie de tiempo
      t<- seq(1:length(y)) #Secuencia de la longitud de los datos                            
      tt<- t*t #Para construir modelo cuadrático
      ttt = t*t*t #Para construir modelo cúbico
      t2 = (length(y)+1):(length(y)+input$pron) #Periodos a pronosticar
      
      model = input$modelo #El modelo será el que el usuario elija en la interfaz
      
      m1 <- lm(y~t) #Modelo lineal de ajuste
      y1 = predict(m1,newdata=list(t=t2)) #Predicciones con modelo lineal
      y1<-ts(y1,freq=input$frecuencia) #Predicciones con modelo lineal como serie de tiempo
       
       
      m2 <- lm(y~t+tt) #Modelo cuadrático de ajuste
      tt2 = t2 * t2
      y2 = predict(m2,newdata=data.frame(t=t2,tt= tt2))
      y2<-ts(y2,freq=input$frecuencia)
       
      m3 <- lm(y~t+tt+ttt) #Modelo cúbico de ajuste
      ttt3 = t2*t2*t2
      y3 = predict(m3,newdata=data.frame(t=t2,tt= tt2,ttt=ttt3))
      y3<-ts(y3,freq=input$frecuencia)
      
      m4 = HoltWinters(y,seasonal = 'additive') #Modelo HW para serie de componentes aditivas
      y4 <- predict(m4,n.ahead = input$pron, prediction.interval = TRUE)
      
      if (model == 4){ #Gráficos de los modelos y sus pronósticos
        plot(y,type = "o",lwd = 2, xlim = c(input$año,(input$año + as.integer(((length(y)+input$pron)/input$frecuencia)))),
             ylim = c(min(y,y4),max(y,y4)))
        lines(m4$fitted[,1],col = "red",lwd =2)
        lines(y4[,1], col = "dark blue",  lwd = 2)
        pronHW = data.frame( #Resultado de la tabla que se muestra
          Modelo = c('HoltWinters'),
          Predicción = y4, Tiempo = t2
        )
        
        output$pron = renderTable({
          pronHW
        })
        
      } else if(model == 3){
        plot(t,y,type = "o",lwd = 3,xlim = c(0,(length(t)+input$pron)),ylim = c(min(y,y3),max(y,y3)))
        lines(m3$fitted.values, col = "red", lwd = 2)
        lines(t2,y3, col = "dark blue",  lwd = 2)
        pronCub = data.frame( #Resultado de la tabla que se muestra
          Modelo = c('Cúbico'),
          Predicción = y3, Tiempo = t2
        )
        
        output$pron = renderTable({
          pronCub
        })
        
      } else if(model == 2){
        plot(t,y,type = "o",lwd = 3,xlim = c(0,(length(t)+input$pron)),ylim = c(min(y,y2),max(y,y2)))
        lines(m2$fitted.values, col = "red", lwd = 2)
        lines(t2,y2, col = "dark blue",  lwd = 2)
        pronCuad = data.frame( #Resultado de la tabla que se muestra
          Modelo = c('Cuadrático'),
          Predicción = y2, Tiempo = t2
        )
        
        output$pron = renderTable({
          pronCuad
        })
        
        
      } else {
        plot(t,y,type = "o",lwd = 3,xlim = c(0,(length(t)+input$pron)),ylim = c(min(y,y1),max(y,y1)))
        lines(m1$fitted.values, col = "red", lwd = 2)
        lines(t2,y1, col = "dark blue",  lwd = 2)
        pronLin = data.frame( #Resultado de la tabla que se muestra
          Modelo = c('Lineal'),
          Predicción = y1, Tiempo = t2
        )
        
        output$pron = renderTable({
          pronLin
        })
      }
      
      table = data.frame( #Resultado de la tabla que se muestra
        Modelo = c('Lineal','Cuadrático','Cúbico','HoltWinters'),
        SSE = c(m1$SSE, m2$SSE, m3$SSE, m4$SSE),
        AIC = c(AIC(m1),AIC(m2),AIC(m3), NA),
        BIC = c(BIC(m1),BIC(m2),BIC(m3), NA)
        #MAPE = c(print(accuracy(y.for,y1)[,4],NA,NA,NA)),
      )
      
      output$table = renderTable({
        table
      })
      
      
    })
   
   output$histPlot <- renderPlot({
     
     infile<- input$file1 #Lee el archivo introducido
     y = read.csv(infile$datapath, header=input$header) #Añade header del archivo si lo tiene
     y <- ts(y,freq=input$frecuencia, start = c(input$año,input$periodo)) #Convierte los datos a una serie de tiempo
     bins<- seq(min(y), max(y), length.out = input$bins + 1)
     hist(y,  breaks=bins, col="dark blue", border="white", main="Histograma de la serie")
   })
   
   output$resPlot <- renderPlot({
     
     model = input$modelo #El modelo será el que el usuario elija en la interfaz
     
     infile<- input$file1 #Lee el archivo introducido
     
     y = read.csv(infile$datapath, header=input$header) #Añade header del archivo si lo tiene
     y <- ts(y,freq=input$frecuencia, start = c(input$año,input$periodo)) #Convierte los datos a una serie de tiempo
     t<- seq(1:length(y)) #Secuencia de la longitud de los datos                            
     tt<- t*t #Para construir modelo cuadrático
     ttt = t*t*t #Para construir modelo cúbico
     t2 = (length(y)+1):(length(y)+input$pron) #Periodos a pronosticar
     
     m1 <- lm(y~t) #Modelo lineal de ajuste
     y1 = predict(m1,newdata=list(t=t2)) #Predicciones con modelo lineal
     y1<-ts(y1,freq=input$frecuencia) #Predicciones con modelo lineal como serie de tiempo
     
     
     m2 <- lm(y~t+tt) #Modelo cuadrático de ajuste
     tt2 = t2 * t2
     y2 = predict(m2,newdata=data.frame(t=t2,tt= tt2))
     y2<-ts(y2,freq=input$frecuencia)
     
     m3 <- lm(y~t+tt+ttt) #Modelo cúbico de ajuste
     ttt3 = t2*t2*t2
     y3 = predict(m3,newdata=data.frame(t=t2,tt= tt2,ttt=ttt3))
     y3<-ts(y3,freq=input$frecuencia)
     
     m4 = HoltWinters(y,seasonal = 'additive') #Modelo HW para serie de componentes aditivas
     y4 <- predict(m4,n.ahead = input$pron, prediction.interval = TRUE)
     
     infile<- input$file1 #Lee el archivo introducido
     y = read.csv(infile$datapath, header=input$header) #Añade header del archivo si lo tiene
     y <- ts(y,freq=input$frecuencia, start = c(input$año,input$periodo)) #Convierte los datos a una serie de tiempo
     
     if (model == 4){ #Gráficos de los modelos y sus pronósticos
       par(mfrow=c(2,2))
       options(repr.plot.width=10, repr.plot.height=6)
       r4=m4$residuals
       plot(y,r4, type='l', ylab='',main="Residuales Modelo HW",col="red")
       abline(h=0,lty=2)
       #plot(density(r4),xlab='x', main="Densidad Residuales Modelo HW",col="red")
       qqnorm(r4)               # Gráfica qqnorm para probar normalidad
       qqline(r4,col=2)
       acf(r4, ci.type="ma",60) # Prueba ACF
       
     } else if(model == 3){
       par(mfrow=c(2,2))
       options(repr.plot.width=10, repr.plot.height=6)
       r3=m3$residuals
       plot(y,r3, type='l', ylab='',main="Residuales Modelo Cúbico",col="red")
       abline(h=0,lty=2)
       plot(density(r3),xlab='x', main="Densidad Residuales Modelo Cúbico",col="red")
       qqnorm(r3)               # Gráfica qqnorm para probar normalidad
       qqline(r3,col=2)
       acf(r3, ci.type="ma",60) # Prueba ACF
       
     } else if(model == 2){
       par(mfrow=c(2,2))
       options(repr.plot.width=10, repr.plot.height=6)
       r2=m2$residuals
       plot(y,r2, type='l', ylab='',main="Residuales Modelo Cuadrático",col="red")
       abline(h=0,lty=2)
       plot(density(r2),xlab='x', main="Densidad Residuales Modelo Cuadrático",col="red")
       qqnorm(r2)               # Gráfica qqnorm para probar normalidad
       qqline(r2,col=2)
       acf(r2, ci.type="ma",60) # Prueba ACF
       
     } else {
       par(mfrow=c(2,2))
       options(repr.plot.width=10, repr.plot.height=6)
       r1=m1$residuals
       plot(y,r1, type='l', ylab='',main="Residuales Modelo Lineal",col="red")
       abline(h=0,lty=2)
       plot(density(r1),xlab='x', main="Densidad Residuales Modelo Lineal",col="red")
       qqnorm(r1)               # Gráfica qqnorm para probar normalidad
       qqline(r1,col=2)
       acf(r1, ci.type="ma",60) # Prueba ACF
     }
     
   })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

