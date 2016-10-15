library(shiny)
library(ggplot2)
library(forecast)
library(TSA)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title="TS Forecast"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datos", tabName = "Datos"),
      menuItem("Selecciones", tabName = "Selecciones"),
      menuItem("Resultados", tabName = "Resultados"),
      menuItem("Histograma", tabName = "Histograma"),
      menuItem("Residuales", tabName = "Residuales")
      )
    ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName="Datos",
        fileInput('file1', 'Inserte un archivo csv que contenga la serie de tiempo')
        ),
      
      tabItem(tabName="Selecciones",
        checkboxInput('header', 'Header', TRUE),
        numericInput("pron", "Numero de periodos a pronosticar:", 6),
        numericInput("year", "year inicial:", 2000),
        numericInput("periodo", "Periodo inicial:", 1),
        numericInput("frecuencia", "Frecuencia de la serie:", 4),
        radioButtons('modelo', 'Seleccione el modelo de ajuste:', 
                     c(Lineal='1', Cuadratico='2', Cubico='3', HoltWinters = '4')),
        sliderInput("bins","Numero de barras para el histograma:",min = 1,max = 50,value = 30)
        ),
      
      tabItem(tabName="Resultados",
         plotOutput("distPlot"),
         fluidRow(tableOutput("table")),
         fluidRow(tableOutput("pron"))
         ),
      
      tabItem(tabName="Histograma",
        plotOutput("histPlot")),
      
      tabItem(tabName="Residuales",
        plotOutput("resPlot"))
        
      )
    )
  )


# Ahora, programamos la funcionalidad de la aplicacion
server <- shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({ #La salida del grafico
    
    infile<- input$file1 #Lee el archivo introducido
    
    y = read.csv(infile$datapath, header=input$header) #Anade header del archivo si lo tiene
    y <- ts(y,freq=input$frecuencia, start = c(input$year,input$periodo)) #Convierte los datos a una serie de tiempo
    t<- seq(1:length(y)) #Secuencia de la longitud de los datos                            
    tt<- t*t #Para construir modelo Cuadratico
    ttt = t*t*t #Para construir modelo Cubico
    t2 = (length(y)+1):(length(y)+input$pron) #Periodos a pronosticar
    
    #tp = seq(1:(length(t)-input$pron))
    #tp2 = tp * tp
    #tp3 = tp * tp * tp
    #print(tp)
    
    #y.fit = window(y,
    #               start=c(input$year,input$periodo),
    #               end = c(input$year + as.integer((length(y)-input$pron)/input$frecuencia),
    #                                                           (input$period+(length(y)-input$pron))%%input$frecuencia))
    
    #y.for = window(y,start = c(input$year + as.integer((length(y)-input$pron)/input$frecuencia),
    #                           (input$period+(length(y)-input$pron))%%input$frecuencia + 1),
    #               end = c(input$year + as.integer((length(y)/input$frecuencia)),
    #                       (input$period+(length(y)))%%input$frecuencia))
    
    #tf = seq(length(y.fit)+1,length(y.fit)+input$pron,1)
    #tf2 = tf * tf
    #tf3 = tf * tf * tf
    #print(tf)
    #print(t)
    
    model = input$modelo #El modelo sera el que el usuario elija en la interfaz
    
    m1 <- lm(y~t) #Modelo lineal de ajuste
    y1 = predict(m1,newdata=list(t=t2)) #Predicciones con modelo lineal
    y1<-ts(y1,freq=input$frecuencia) #Predicciones con modelo lineal como serie de tiempo
    
   # m1p = lm(y.fit ~ tp)
   # y1p = predict(m1p,newdata = list(tp = tf))
   # y1p = ts(y1p, freq = input$frecuencia, start = c(input$year + as.integer((length(y)-input$pron)/input$frecuencia),
   #                                                  (input$period+(length(y)-input$pron))%%input$frecuencia + 1) )
    
    
    m2 <- lm(y~t+tt) #Modelo Cuadratico de ajuste
    tt2 = t2 * t2
    y2 = predict(m2,newdata=data.frame(t=t2,tt= tt2))
    y2<-ts(y2,freq=input$frecuencia)
    
    #m2p = lm(y.fit ~ tp + tp2)
    #y2p = predict(m2p,newdata = data.frame(tp = tf,tp2 = tf2))
    #y2p = ts(y2p, freq = input$frecuencia, start = c(input$year + as.integer((length(y)-input$pron)/input$frecuencia),
    #                                                 (input$period+(length(y)-input$pron))%%input$frecuencia + 1) )
    
    m3 <- lm(y~t+tt+ttt) #Modelo Cubico de ajuste
    ttt3 = t2*t2*t2
    y3 = predict(m3,newdata=data.frame(t=t2,tt= tt2,ttt=ttt3))
    y3<-ts(y3,freq=input$frecuencia)
    
    #m3p = lm(y.fit ~ tp + tp2 + tp3)
    #y3p = predict(m3p,newdata = data.frame(tp = tf,tp2 = tf2, tp3 = tf3))
    #y3p = ts(y3p, freq = input$frecuencia, start = c(input$year + as.integer((length(y)-input$pron)/input$frecuencia),
    #                                                 (input$period+(length(y)-input$pron))%%input$frecuencia + 1) )
    
    m4 = HoltWinters(y,seasonal = 'additive') #Modelo HW para serie de componentes aditivas
    y4 <- predict(m4,n.ahead = input$pron, prediction.interval = TRUE)
    
    #m4p = HoltWinters(y.fit,seasonal = 'additive') #Modelo HW para serie de componentes aditivas
    #y4p <- predict(m4p,n.ahead = input$pron, prediction.interval = TRUE)
    
    if (model == 4){ #Graficos de los modelos y sus Pronosticos
      plot(y,main="Modelo ajustado",type = "o",lwd = 2, xlim = c(input$year,(input$year + as.integer(((length(y)+input$pron)/input$frecuencia)))),
           ylim = c(min(y,y4),max(y,y4)))
      lines(m4$fitted[,1],col = "red",lwd =2)
      lines(y4[,1], col = "dark blue",  lwd = 2)
      pronHW = data.frame( #Resultado de la tabla que se muestra
        Modelo = c('HoltWinters'),
        Prediccion = y4, Tiempo = t2
      )
      
      output$pron = renderTable({
        pronHW
      })
      
    } else if(model == 3){
      plot(t,y,main="Modelo ajustado",type = "o",lwd = 3,xlim = c(0,(length(t)+input$pron)),ylim = c(min(y,y3),max(y,y3)))
      lines(m3$fitted.values, col = "red", lwd = 2)
      lines(t2,y3, col = "dark blue",  lwd = 2)
      pronCub = data.frame( #Resultado de la tabla que se muestra
        Modelo = c('Cubico'),
        Prediccion = y3, Tiempo = t2
      )
      
      output$pron = renderTable({
        pronCub
      })
      
    } else if(model == 2){
      plot(t,y,main="Modelo ajustado",type = "o",lwd = 3,xlim = c(0,(length(t)+input$pron)),ylim = c(min(y,y2),max(y,y2)))
      lines(m2$fitted.values, col = "red", lwd = 2)
      lines(t2,y2, col = "dark blue",  lwd = 2)
      pronCuad = data.frame( #Resultado de la tabla que se muestra
        Modelo = c('Cuadratico'),
        Prediccion = y2, Tiempo = t2
      )
      
      output$pron = renderTable({
        pronCuad
      })
      
      
    } else {
      plot(t,y,main="Modelo ajustado",type = "o",lwd = 3,xlim = c(0,(length(t)+input$pron)),ylim = c(min(y,y1),max(y,y1)))
      lines(m1$fitted.values, col = "red", lwd = 2)
      lines(t2,y1, col = "dark blue",  lwd = 2)
      pronLin = data.frame( #Resultado de la tabla que se muestra
        Modelo = c('Lineal'),
        Prediccion = y1, Tiempo = t2
      )
      
      output$pron = renderTable({
        pronLin
      })
    }
    
    table = data.frame( #Resultado de la tabla que se muestra
      Modelo = c('Lineal','Cuadratico','Cubico','HoltWinters'),
      AIC = c(AIC(m1),AIC(m2),AIC(m3), NA),
      BIC = c(BIC(m1),BIC(m2),BIC(m3), NA)
      #MAPE = c(accuracy(y.for,y1p)[,5],accuracy(y.for,y2p)[,5],accuracy(y.for,y3p)[,5],accuracy(y.for,y4p)[,5])
    )
    
    output$table = renderTable({
      table
    })
    
    
  })
  
  output$histPlot <- renderPlot({
    
    infile<- input$file1 #Lee el archivo introducido
    y = read.csv(infile$datapath, header=input$header) #Anade header del archivo si lo tiene
    y <- ts(y,freq=input$frecuencia, start = c(input$year,input$periodo)) #Convierte los datos a una serie de tiempo
    bins<- seq(min(y), max(y), length.out = input$bins + 1)
    hist(y,  breaks=bins, col="dark blue", border="white", main="Histograma de la serie")
  })
  
  output$resPlot <- renderPlot({
    
    model = input$modelo #El modelo sera el que el usuario elija en la interfaz
    
    infile<- input$file1 #Lee el archivo introducido
    
    y = read.csv(infile$datapath, header=input$header) #Anade header del archivo si lo tiene
   
    
    y <- ts(y,freq=input$frecuencia, start = c(input$year,input$periodo)) #Convierte los datos a una serie de tiempo
    t<- seq(1:length(y)) #Secuencia de la longitud de los datos                            
    tt<- t*t #Para construir modelo Cuadratico
    ttt = t*t*t #Para construir modelo Cubico
    t2 = (length(y)+1):(length(y)+input$pron) #Periodos a pronosticar
    
    m1 <- lm(y~t) #Modelo lineal de ajuste
    y1 = predict(m1,newdata=list(t=t2)) #Predicciones con modelo lineal
    y1<-ts(y1,freq=input$frecuencia) #Predicciones con modelo lineal como serie de tiempo
    
    
    m2 <- lm(y~t+tt) #Modelo Cuadratico de ajuste
    tt2 = t2 * t2
    y2 = predict(m2,newdata=data.frame(t=t2,tt= tt2))
    y2<-ts(y2,freq=input$frecuencia)
    
    m3 <- lm(y~t+tt+ttt) #Modelo Cubico de ajuste
    ttt3 = t2*t2*t2
    y3 = predict(m3,newdata=data.frame(t=t2,tt= tt2,ttt=ttt3))
    y3<-ts(y3,freq=input$frecuencia)
    
    m4 = HoltWinters(y,seasonal = 'additive') #Modelo HW para serie de componentes aditivas
    y4 <- predict(m4,n.ahead = input$pron, prediction.interval = TRUE)
    
    infile<- input$file1 #Lee el archivo introducido
    y = read.csv(infile$datapath, header=input$header) #Anade header del archivo si lo tiene
    y <- ts(y,freq=input$frecuencia, start = c(input$year,input$periodo)) #Convierte los datos a una serie de tiempo
    
    if (model == 4){ #Graficos de los modelos y sus Pronosticos
      par(mfrow=c(2,2))
      options(repr.plot.width=10, repr.plot.height=6)
      
      #error = y - m4$fitted[,1]
      #print(m4$fitted[,1])
      
      plot(residuals(m4), type='l', ylab='',main="Residuales Modelo HW",col="red")
      abline(h=0,lty=2)
      plot(density(residuals(m4)),xlab='x', main="Densidad Residuales Modelo HW",col="red")
      qqnorm(residuals(m4))               # Grafica qqnorm para probar normalidad
      qqline(residuals(m4),col=2)
      acf(residuals(m4), ci.type="ma",60) # Prueba ACF
      
    } else if(model == 3){
      par(mfrow=c(2,2))
      options(repr.plot.width=10, repr.plot.height=6)
      r3=m3$residuals
      plot(t,r3, type='l', ylab='',main="Residuales Modelo Cubico",col="red")
      abline(h=0,lty=2)
      plot(density(r3),xlab='x', main="Densidad Residuales Modelo Cubico",col="red")
      qqnorm(r3)               # Grafica qqnorm para probar normalidad
      qqline(r3,col=2)
      acf(r3, ci.type="ma",60) # Prueba ACF
      
    } else if(model == 2){
      par(mfrow=c(2,2))
      options(repr.plot.width=10, repr.plot.height=6)
      r2=m2$residuals
      plot(t,r2, type='l', ylab='',main="Residuales Modelo Cuadratico",col="red")
      abline(h=0,lty=2)
      plot(density(r2),xlab='x', main="Densidad Residuales Modelo Cuadratico",col="red")
      qqnorm(r2)               # Grafica qqnorm para probar normalidad
      qqline(r2,col=2)
      acf(r2, ci.type="ma",60) # Prueba ACF
      
    } else {
      par(mfrow=c(2,2))
      options(repr.plot.width=10, repr.plot.height=6)
      r1=m1$residuals
      plot(t,r1, type='l', ylab='',main="Residuales Modelo Lineal",col="red")
      abline(h=0,lty=2)
      plot(density(r1),xlab='x', main="Densidad Residuales Modelo Lineal",col="red")
      qqnorm(r1)               # Grafica qqnorm para probar normalidad
      qqline(r1,col=2)
      acf(r1, ci.type="ma",60) # Prueba ACF
    }
    
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)


