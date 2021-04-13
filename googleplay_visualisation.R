#######  Univariate and Bivariate analysis of GooglePlay complex dataset with different visualisations  #########
### Author: Barbara Salas ###

# Install and load required packages
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")

library("tidyr")
library("dplyr")
library("ggplot2")

# Import dataset: googleplay_cleaned.CSV, googleplay_cleaned_Paid.CSV and googleplay_Free.CSV. 

#dataset is saved in an object called 'googleplay'
googleplay<-googleplay_cleaned
googleplay_paid<-googleplay_cleaned_Paid
googleplay_free<-googleplay_cleaned_Free

#to view the table
View(googleplay)

#to know dimensions of the data frame
dim(googleplay)

#To know the datatype of variables
str(googleplay)

#data type (or class) for column 'Installs' is unknown, so it is converted into numeric, then into integer:
googleplay$Installs <- as.numeric(googleplay$Installs)
googleplay$Installs <- as.integer(googleplay$Installs)
class(googleplay$Installs)

googleplay_cleaned_Paid$Installs <- as.numeric(googleplay_cleaned_Paid$Installs)
googleplay_cleaned_Free$Installs <- as.numeric(googleplay_cleaned_Free$Installs)

#Convert the Year_lastUpdated from integer to Factor class:
googleplay$Year_lastUpdated <- as.factor(googleplay$Year_lastUpdated)

#to get a statistical summary of the dataset
summary(googleplay)

#This is a tibble that shows the first ten rows of the dataset
tbl_df(googleplay)

# To get specific statistics on numerical columns for Standard Deviation:
summarise(googleplay, mean_Rating=mean(Rating), st_dev_Rating=sd(Rating))
summarise(googleplay, mean_Size=mean(Size), st_dev_Size=sd(Size))
summarise(googleplay, mean_Price=mean(Price), st_dev_Price=sd(Price))

summarise(googleplay_paid, mean_PricePaid=mean(Price), st_dev_PricePaid=sd(Price))

summarise(googleplay, mean_Reviews=mean(Reviews), st_dev_Reviews=sd(Reviews))
summarise(googleplay, mean_Installs=mean(Installs), st_dev_Installs=sd(Installs))


#********************************* Univariate ******************************************

#************** 1. Categorical data: Frequence of categories represented by a circular plot  **********

#-----------------For googleplay (entire table) --------------
#Grouping categories to display the count
cat_type <- googleplay %>%
  select("Category") %>% 
  group_by(Category) %>%
  summarise(cat_count = n())

#Rows from cat_type are counted and saved into an object
id <- seq_len(nrow(cat_type))

#Category and its count  are combined and stored in the same variable 'cat_type'
cat_type <- cbind(cat_type, id)

#cat_type is saved into an  object called 'label_data'
label_data = cat_type

#to calculate the angle of the labels
number_of_bar = nrow(label_data) 

#to center
angle = 90 - 360 * (label_data$id - 0.5) / number_of_bar 

#to align labels
label_data$hjust <- ifelse(angle < -90, 1, 0) 

#To flip angle
label_data$angle <- ifelse(angle < -90, angle + 180, angle) 

#id is converted into a factor for the x-axis and frequency for the y-axis. Other attributes were added
ggplot(cat_type, aes(x = as.factor(id), y = cat_count)) +
  geom_bar(stat = "identity", fill = alpha("purple", 0.7)) +  #to include colour
  geom_text(data = label_data, aes(x = id, y = cat_count + 10, label = Category, hjust = hjust),
            color = "black", alpha = 0.6, size = 3, angle =  label_data$angle, inherit.aes = FALSE ) +
  coord_polar(start = 0) +
  ylim(-500, 2000) + #to set teh size of the circle
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-4,4), "in"),
        plot.title = element_text(margin = margin(t = 10, b = -10)))


#-----------------For googleplay_paid (paid applications) --------------

#table googpley_paid with frequencies 
freq_paid=googleplay_paid %>% group_by(Category) %>% count ()
freq_paid=as.vector(freq_paid)
View(freq_paid)

#Grouping categories to display the count
cat_type <- googleplay_paid %>%
  select("Category") %>% 
  group_by(Category) %>%
  summarise(cat_count = n())

#Rows from cat_type are counted and saved into an object
id <- seq_len(nrow(cat_type))

#Category and its count  are combined and stored in the same variable 'cat_type'
cat_type <- cbind(cat_type, id)

#cat_type is saved into an  object called 'label_data'
label_data = cat_type

#to calculate the angle of the labels
number_of_bar = nrow(label_data) 

#to center
angle = 90 - 360 * (label_data$id - 0.5) / number_of_bar 

#to align labels
label_data$hjust <- ifelse(angle < -90, 1, 0) 

#To flip angle
label_data$angle <- ifelse(angle < -90, angle + 180, angle) 

#id is converted into a factor for the x-axis and frequency for the y-axis. Other attributes were added
ggplot(cat_type, aes(x = as.factor(id), y = cat_count)) +
  geom_bar(stat = "identity", fill = alpha("purple", 0.7)) +  #to include colour
  geom_text(data = label_data, aes(x = id, y = cat_count + 10, label = Category, hjust = hjust),
            color = "black", alpha = 0.6, size = 3, angle =  label_data$angle, inherit.aes = FALSE ) +
  coord_polar(start = 0) +
  ylim(-500, 2000) + #to set teh size of the circle
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-4,4), "in"),
        plot.title = element_text(margin = margin(t = 10, b = -10)))


#-----------------For googleplay_free (free applications) --------------

#For googleplay_free (free applications)
freq_free=googleplay_free %>% group_by(Category) %>% count ()
freq_free=as.vector(freq_free)
View(freq_free)

#Grouping categories to display the count
cat_type <- googleplay_free %>%
  select("Category") %>% 
  group_by(Category) %>%
  summarise(cat_count = n())

#Rows from cat_type are counted and saved into an object
id <- seq_len(nrow(cat_type))

#Category and its count  are combined and stored in the same variable 'cat_type'
cat_type <- cbind(cat_type, id)

#cat_type is saved into an  object called 'label_data'
label_data = cat_type

#to calculate the angle of the labels
number_of_bar = nrow(label_data) 

#to center
angle = 90 - 360 * (label_data$id - 0.5) / number_of_bar 

#to align labels
label_data$hjust <- ifelse(angle < -90, 1, 0) 

#To flip angle
label_data$angle <- ifelse(angle < -90, angle + 180, angle) 

#id is converted into a factor for the x-axis and frequency for the y-axis. Other attributes were added
ggplot(cat_type, aes(x = as.factor(id), y = cat_count)) +
  geom_bar(stat = "identity", fill = alpha("purple", 0.7)) +  #to include colour
  geom_text(data = label_data, aes(x = id, y = cat_count + 10, label = Category, hjust = hjust),
            color = "black", alpha = 0.6, size = 3, angle =  label_data$angle, inherit.aes = FALSE ) +
  coord_polar(start = 0) +
  ylim(-500, 2000) + #to set teh size of the circle
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-4,4), "in"),
        plot.title = element_text(margin = margin(t = 10, b = -10)))


#************** 1. Categorical data: Frequence of categories represented by a Barplot ************ 

#Grouping categories to display the count
googleplay %>% group_by(Category) %>% count ()

#to generate a barplot for each category and its frequency (for the whole dataset)
ggplot(googleplay, aes(x = Category)) + geom_bar(stat="count", fill="blue")+
  coord_flip()

#for paid applications
ggplot(googleplay_paid, aes(x = Category)) + geom_bar(stat="count", fill="blue")+
  coord_flip()

#for free applications
ggplot(googleplay_free, aes(x = Category)) + geom_bar(stat="count", fill="blue")+
  coord_flip()


#*****************  2. numerical data: Histogram and Boxplot *********************

#To get an histogram and boxplot for a numerical variable (size and Rating)
#A function was used to get an histogram and boxplot for Size and Rating

install.packages("modeest")
library("modeest", lib.loc="~/R/win-library/3.5")

"chmeza" <-
  function (x,...) {
    op<-par(no.readonly=TRUE)
    on.exit(par(op))
    layout(matrix(c(1,2),2,1),heights=c(3,1))         
    par(mai=c(1,1,1,1)/2)
    lines(hist(x,xlab=FALSE,yaxt='n',col="lightblue",...))
    abline(v = mean(x), #Add a line foro the mean
           col = "blue", #colour for the the mean
           lwd = 2)  #Thickness of the line
    abline(v = median(x),  #Add a line for the median
           col = "brown",  #Colour for the median
           lwd = 2)
    legend(x = "topright", # location of legend within plot area
           c("Mean", "Median"), #mean of lines
           col = c("blue", "brown"), #colours of lines
           lwd = c(2, 2, 2))
    rug(x)
    boxplot(x,horizontal=TRUE,col="lightblue") #boxplot is added
  }


#This function is called to execute the double plot for Size
chmeza(googleplay$Size, main = "Histogram and Boxplot for the size of applications") 

#This function is called to execute the double plot for Rating
chmeza(googleplay$Rating, main = "Histogram and Boxplot for the rating of applications") 

#to change the shape of the histogram, a logarithmic scale is added as a new variable to reduce skewness
#new variables with this new transformation are created for size and rating
googleplay$log10_size<-log10(googleplay$Size)
View(googleplay)

googleplay$log10_rating<-log10(googleplay$Rating)
View(googleplay)

#Also, a new histogram for log10 of size is plotted using this package
install.packages("rcompanion")
library(rcompanion)

#To get the Transformation log and a plot for Size
T_log = log10(googleplay$Size)
plotNormalHistogram(T_log, ylab="Frequency", xlab="log10_Size", col= 'lightblue', 
                    main = "Transformation log for the size of the applications")
                    
#transformation log and plot for Rating
T_log = log10(googleplay$Rating)
plotNormalHistogram(T_log, ylab="Frequency", xlab="log10_Rating", col= 'lightblue', main = "Transformation log for the rating of the applications")


#****************************** Bivariate ************************************************************

#*******************  1. categorical versus categorical *********************************

#**************** 1.1 Stacked plot for Year_lastUpdated vs Type(free or paid) ******************
googleplay_tidy<- count(googleplay,Year_lastUpdated,Type) #values were saved into a new table

#using ggplot, a stacked plot was made adding labels based on the previous table
ggplot(googleplay_tidy) +
  geom_col(aes(x=Year_lastUpdated, y= n, fill = Type)) +
  labs(title = "Stacked plot for type vs Year last updated", 
       x = "Year_lastUpdated", y = "Frequency") +
       theme(plot.title = element_text(hjust = 0.5))

#contingency table grouping by Year_lastUpdated and type was created
CT_Year_Type= googleplay %>%
  group_by(Year_lastUpdated, Type) %>%
  summarize(frequency = n()) %>% 
  spread(Type, frequency, fill = 0) 

#to view the contingency table
View(CT_Year_Type)

#********************* 1.2 stream graph was made for Year last updated versus Category *********************

#Package to install and load
install.packages("devtools")
library(devtools)

#package is installed in this way from github
devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)

#a new ontingency table is created with frequencies for Category and Year last updated
googleplay_tidy<- count(googleplay,Year_lastUpdated,Category)
googleplay_tidy$Year_lastUpdated <- as.Date(googleplay_tidy$Year_lastUpdated)

#to check whether the dataset contains na
sapply(googleplay_tidy, function(x) sum(is.na(x)))

#to create a stream graph (interactive graph)
googleplay_tidy %>%
  streamgraph("Category", "n", "Year_lastUpdated", scale = "continuous" ,offset="zero", interpolate="cardinal") %>% 
  sg_fill_brewer("PuOr") %>%   #add colour
  sg_legend(TRUE, "Category: ") #combo box with categories


#******************** 2. Numerical versus categorical ********************

# Install and load package ggpubr
install.packages("ggpubr")
library("ggpubr")

#*****************  2.1 violin graph for Content.Rating vs Rating **********

ggviolin(googleplay, x = "Content.Rating", y = "Rating",color = "blue", 
         title= '      Violin plots with boxplot inside for Rating versus Content of Rating',
         align=TRUE, add = "boxplot")

#****************  2.2 Violin graph for Year_lastUpdated vs Rating *********

ggviolin(googleplay, x = "Year_lastUpdated", y = "Rating",color = "purple",
         title= '                   Violin plots with boxplot inside for Year last Updated 
         versus Rating', align=TRUE, add = "boxplot")


#********************* 3. Numerical versus numerical ****************

#contigency table with relative frequencies to show all the correlations for numerical variables
googleplay %>% select_if(is.numeric) %>% cor 

#Package used to create a correlation matrix
install.packages("ggcorrplot")
library("ggcorrplot")

#******** 3.1 Filter to calculate correlation for those numeric variables and plot in a correlation matrix
googleplay %>%
  select_if(is.numeric) %>%
  cor %>% 
  ggcorrplot(type = "lower", ggtheme = theme_minimal, colors = c("yellow4","white","skyblue4"),
             show.diag = T,
             lab = T, lab_size = 5,
             title = "Correlation Matrix for googleplay Database",
             legend.title = "Correlation Value",
             outline.color = "white",
             hc.order = T)

#******** 3.2 to generate a Scatter plot for Installs vs Reviews
library("ggpubr")
ggscatter(googleplay, x = "Installs", y = "Reviews", 
          add = "reg.line", conf.int = TRUE, color = "blue",
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of installs", ylab = "Number of reviews")


#------------------------ Unresolved questions-------------------------

#-------- 1. To calculate histograms with no outliers ----------

#table "googleplay_noOutliers" is imported
googleplay_noOutliers$Installs <- as.numeric(googleplay_noOutliers$Installs) #convert into numeric variable

#a function is created to do not consider na values (blank) from this table
"chmeza3" <-
  function (x,...) {
    op<-par(no.readonly=TRUE)
    on.exit(par(op))
    layout(matrix(c(1,2),2,1),heights=c(3,1))         
    par(mai=c(1,1,1,1)/2)
    lines(hist(x,xlab=FALSE,yaxt='n',col="lightblue",...))
    abline(v = mean(x, na.rm=TRUE), #Add a line for the mean
           col = "blue", #colour for the the mean
           lwd = 2)  #Thickness of the line
    abline(v = median(x, na.rm=TRUE),  #Add a line for the median
           col = "brown",  #Colour for the median
           lwd = 2)
    legend(x = "topright", # location of legend within plot area
           c("Mean", "Median"), #mean of lines
           col = c("blue", "brown"), #colours of lines
           lwd = c(2, 2, 2))
    rug(x)
    boxplot(x,horizontal=TRUE,col="lightblue") #boxplot is added
  }

#This function is called to execute the double plot for Size and get plots
chmeza3(googleplay_noOutliers$Size, main = "Size of applications (without otliers)") 
chmeza3(googleplay_noOutliers$Rating, main = "Rating of application (without outliers)")

#--------------------2. Content.Rating vs Rating ------------
#with outliers
ggplot(googleplay,aes(x=Rating,fill=Content.Rating)) + #from dataset, indicate variables 
  geom_histogram(binwidth=0.2) +  #a histogram is created
  theme_minimal() +
  facet_wrap(~Content.Rating) #to wrap a variable

#no outliers
ggplot(googleplay_ContVSRating_noOutliers,aes(x=Rating,fill=Content.Rating)) +  #from dataset, indicate variables
  geom_histogram(binwidth=0.2) + #a histogram is created
  theme_minimal() +
  facet_wrap(~Content.Rating) #to wrap a variable


