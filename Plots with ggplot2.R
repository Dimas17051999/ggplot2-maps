
#if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}library(ggplot2)

#Bar plots with ggplot2 (cuantitive)
library(ggplot2)
mtcars #data set de ggplot2 
View(mtcars)
mtcars$cyl
qplot(mtcars$cyl, 
      geom = "bar", #bar o histogram
      fill= I("pink"), #color
      xlab= "Cylinder",
      ylab= "Number of vehicles", 
      main= "Cylinders in vehicles")

#Histograms with ggplot 2 (qualitative)
qplot(mtcars$hp, 
      geom = "histogram",
      binwidth=25,#grosor de cada barra "smoothness"
      colour=I("Blue"), #contorno
      xlim = c(0,400),
      xlab="Horse power",
      ylab="Number of cars",
      main="Histogram",
      alpha= I(0) #quitar el color de relleno
      )

#PieChart with ggplot2 

#To make a pie chart, you fisrt need to create 
#a stacked bar plot 

sta <- ggplot(mtcars, aes(x=1, #solamente define los valores dentro del eje x 
                          y=sort(carb), #sort=clasificar, por lo tanto clasifica el eje y con la info de carb
                          fill=sort(carb)))+ #ahora llena de informacion con la misma clasificacion de carb
  geom_bar(stat = "identity") #esa funcion es para apilar la informacion en la manera correcta 
  
print(sta)

pie <- sta + coord_polar(theta = "y") +#la funci?n coord_polar hace que se distribuya la barra 
              theme(axis.line=element_blank(),
              axis.text.x=element_blank(), 
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.y=element_blank(),#Todos los anteriores fue para eliminar las etiquetas entro de la funcion theme()
              panel.background=element_blank())+ #este es para eliminar el color de fondo 
              labs(y="CARBS")
print(pie)

# Scatter plot with ggplot2 

qplot(mpg, wt, data = mtcars) #gr?fia de dispersion b?sica 

#Si quiero algo m?s personalizado 

ggplot(mtcars, aes(x=mpg, y=wt))+ geom_point(shape=1)

#Para crear categorias 

mtcars$cylFactor <- factor(mtcars$cyl) #primero en una variable metes la informacion de la categoria 

ggplot(mtcars, aes(x=mpg, y=wt, shape=cylFactor))+ geom_point(shape=19, colour= "blue")

ggplot(mtcars, aes(x=mpg, y=wt,color=cylFactor ))+ geom_point(shape=19)+#este esta mejor 
        xlab("Miles per gallon ")+
        ylab("Weight")+
        labs(colour="Cylinders")+
        ggtitle("Scatterplot")


#Line plotswith ggplot2 

stock <- as.data.frame(EuStockMarkets)
head(EuStockMarkets)

#Simple line plor 
ggplot(stock, aes(x=c(1:nrow(stock)), y= DAX))+
          geom_line(size=1, colour="light blue")+
          labs(x="Time", y="Stocks")
#Two line plots 
two<-  ggplot()+
        geom_line(data= stock, aes(x=c(1:nrow(stock)), y= DAX), colour="light blue")+
        geom_line(data= stock, aes(x=c(1:nrow(stock)), y= SMI), colour="blue")+
        labs(x="Time", y="Stocks")
print(two) 

#Four line plots 
four <-  ggplot()+
  geom_line(data= stock, aes(x=c(1:nrow(stock)), y= DAX), colour="light blue")+
  geom_line(data= stock, aes(x=c(1:nrow(stock)), y= SMI), colour="blue")+
  geom_line(data= stock, aes(x=c(1:nrow(stock)), y= CAC), colour="red")+
  geom_line(data= stock, aes(x=c(1:nrow(stock)), y= FTSE), colour="pink")+
  labs(x="Days", y="Price")+
  ggtitle("Stocks")
print(four)


#Linear Regression with ggplot 2 
ggplot(mtcars, aes(x=mpg, y=wt))+
      geom_point(shape=19)+ #for a scatterplot 
      geom_smooth(method = "lm",se=F,  color="red")#this function allow us to plot the linear regression 
        #se es el argumento para que se ve el intervalo de confianza del 95% 
ggplot(mtcars,aes(x=mpg, y=wt, color=cylFactor))+
  geom_point(shape=19)+
  geom_smooth(method = "lm",se=T,  color="red")+
  xlab("Miles per gallon")+
  ylab("Weight")+
  labs(colour="Cylinders")+
  ggtitle("Linear regression")

#Gaussian regression
ggplot(mtcars,aes(x=mpg, y=wt, color=cylFactor))+
  geom_point(shape=19)+
  geom_smooth(method = "auto",se=T,  color="red")+ #para la regresi?n gaussiana es en metho="auto"
  xlab("Miles per gallon")+
  ylab("Weight")+
  labs(colour="Cylinders")+
  ggtitle("Gaussian regression")



#Word Clouds with ggplo2  NO PUDE REALIZAR ESTE :(

install.packages("tm") #text mining
install.packages("wordcloud")
library(tm)
library(wordcloud)

dir.create("/files_path/wordcloud")
download.file("https://ibm.box.com/shared/static/cmid70rpa7xe4ocitcga1bve7r0kqnia.txt", destfile = "/files_path/wordcloud/churchill_speeches.txt",quiet = T) #to download an internet file

dirPath <- "files_path/wordcloud/" #select the directory that our text is located 
speech <- Corpus(DirSource(dirPath)) #Load the data as a corpus 

inspect(speech)

speech <- tm_map(speech, content_transformer(tolower))
speech <- tm_map(speech, removeNumbers)
speech <- tm_map(speech, removeWords, stopwords("english"))
speech <- tm_map(speech, removeWords, c("floccinaucinihilipifaction", "squirrelled"))
speech <- tm_map(speech, removePunctuation)
speech <- tm_map(speech, stripWhitespace)

dtm <- TermDocumentMatrix(speech) #create a term document matrix
m <- as.matrix(dmt) #matrix transformation
v <- sort(rowSums(m), decreasing = T) #sort it show the most frequent words 
d <- data.frame(word= names(v), freq=v) #transform to a data frame 
head(d, 10)

wordcloud(words= d$word, freq = d$freq, min.freq = 1, max.words = 250, 
            colors= brewer.pal(8, "Dark2"), random.order = F) #simple word cloud 




#Radar Charts with ggplot 2  NO PUDE INSTALAR GGRADAR 
install.packages("ggradar")
install.packages("scales")

library(ggradar)
library(dplyr)
library(scales)
library(devtools)
mtcars %>%
  add_rownames(var="group") %>% #stribute rownames to a variable 
  mutate_each(funs(rescale), -group) %>% #assign each varaible --car names-- to their related variables 
  head(3) %>% select(1:10) -> mtcars_radar #select which data to plot 

IRkernel::set_plot_options(width=950, height=600, units='px')

options(warn = -1)
ggradar(mtcars_radar)




#Waffle charts with ggplot........Espero que s? se pueda :(, s? pude :) 

install.packages("waffle")
library(waffle)


  expenses <- c(`Health($43,212)`=43212, #crear un vector que contenga la info
                `Education($113,412)`=113412, #`` se utiliza para 
                `Transportatio($20,231)`=20231,
                `Entertainment($28,145)`=28145)



#IRkernel::set_plot_options(width=950, height=600, units='px') #adjust shape
waffle(expenses/1235, rows=5, size = 1, title = "Imaginary Household Expenses"
                      , xlab="1 square = $934")





#BoxPlot wih ggplot2 
set.seed(1234)
seta <- rnorm(200, 1, 2)
setb <- rnorm(200, 0, 1)

df <- data.frame(label=factor(rep(c("A", "B"), each=200)), value=c(seta, setb))
df

install.packages("plotly")
library(plotly)


ggplot(df, aes(x=label, y=value)) +
  geom_boxplot() #resultado gen?rico
ggplotly() #resultado m?s estetico

#Ahora con mtcars
qplot(factor(cyl), mpg, data=mtcars, geom = "boxplot") #con funcion qplot
    #como cyl es una varaible categorica, se utiliza factor()

cars <- ggplot(mtcars, aes(factor(cyl), mpg))
cars + geom_boxplot() #el mismo resultado pero con ggplot2

x <- c(3, 5, 7, 8, 12, 13, 14, 18, 21)
summary(x)



































