#Project Part II - R Programming
#March 3 - March 8 2022

#Import the Data
getwd()
setwd("C:/Users/.........../Desktop")
ins <- read.csv("ViewAll.csv")

#Data Exploration
#show top rows
head(ins)
#show column summaries
summary(ins)
#show structure of the dataset
str(ins) 



#Activate GGPlot2
install.packages("ggplot2")
library(ggplot2)

#create a bar plot for age group
ggplot(data=ins, aes(x=age_group)) + geom_bar()
#age group 18-29 is most frequently billed

#create a bar plot for region
ggplot(data=ins, aes(x=region)) + geom_bar()
#Southeast is most frequently billed

#create a bar plot for children
ggplot(data=ins, aes(x=children)) + geom_bar()
#Insurance bills with no children are most frequent

#filter the dataset to leave only certain age groups and children
#create the age group filter and use the Logical 'OR'
#operator to select multiple age groups:
filt <- (ins$age_group == "18-29") | (ins$age_group == "30-39") | (ins$age_group == "40-49")

#create the children filter:
#filt2 <- (ins$children == 0) | (ins$children == 1) | (ins$children == 2) | (ins$children == 3)
filt2 <- ins$children %in% c(0,1,2,3) 
 
filt
filt2

#Apply the row filters to the dataframe
ins2 <- ins[filt & filt2,]
head(ins2)
tail(ins2)

#Prepare the plot's data and aes layers
#Use str() or summary() to review a new dataset
str(ins2)
summary(ins2)

#get column names
colnames(ins2)

#rename column names using vector
colnames(ins2) <- c("Age","AgeGroup","GenderId","Gender","SubRegion",
                  "Region","ChildrenNum","Children","BMI","Smoker",
                  "Charges")
colnames(ins2)
str(mov2)

#plot gender and charges data

p <- ggplot(data=ins2, aes(x=Gender, y=Charges)) 

p #Nothing happens. We need a geom.

#Add a Point Geom Layer
p + 
  geom_point()

#plot charges againts age groups as boxplot
a <- ggplot(data=ins2, aes(x=AgeGroup, y=Charges))
a +  geom_boxplot()

#Notice that outliers are part of the boxplot layer

#plot charges against subregions as points and boxplot
s <- ggplot(data=ins2, aes(x=SubRegion, y=Charges))
s + 
  geom_boxplot() + 
  geom_point()


#Replace points with jitter
a + 
  geom_boxplot() + 
  geom_jitter()

#Place boxplot on top of jitter
a + 
  geom_jitter() + 
  geom_boxplot() 

#Add boxplot transparency
a + 
  geom_jitter() + 
  geom_boxplot(alpha=0.7) 

#Add size and color to the points:
a + 
  geom_jitter(aes(size=ChildrenNum, color=Smoker)) + 
  geom_boxplot(alpha=0.7)
 
 
#Remove the remaining black points
a + 
  geom_jitter(aes(size=ChildrenNum, color=Smoker)) + 
  geom_boxplot(alpha= 0.7, outlier.colour = NA) 

#Save progress by placing it into a new object:
q <- a + 
  geom_jitter(aes(size=ChildrenNum, color=Smoker)) + 
  geom_boxplot(alpha= 0.7, outlier.colour = NA)
q

#Non-data ink, overwrite q with q+
q <- q +
  xlab("Age Group") + #x axis title
  ylab("Charges USD") + #y axis title
  ggtitle("Bill Charges by Age Group") #plot title
q

#Theme
q <- q + 
  theme(
    #this is a shortcut to alter font for all text elements at once:
    text = element_text(family="Comic Sans MS"),
    
    #Axes titles:
    axis.title.x = element_text(colour="Blue", size=13),
    axis.title.y = element_text(colour="Blue", size=13),
    
    #Axes texts:
    axis.text.x = element_text(size=10),
    axis.text.y = element_text(size=10),  
    
    #Plot title:
    plot.title = element_text(colour="Black",
                            size=16, family="Comic Sans MS"),
    
    #Legend title:
    legend.title = element_text(size=11),
    
    #Legend text
    legend.text = element_text(size=9)
  )
q


#change individual legend title
q$labels$size = "Children"
q

#get warnings
warnings(q)

#return to the original dataset to visualize all age groups and all children
colnames(ins)
ggplot(data=ins, aes(y=children, color=smoker)) + geom_bar()
#Non-smoker claimants with zero, one or two children are most frequently billed 

#histograms
h <- ggplot(data=ins, aes(x=charges))
h + geom_histogram(binwidth=500)
#Skewed normal distribution, with most charges below 15 thousand USD

#add color
h + geom_histogram(binwidth=500, fill="Blue")

#add a border
h + geom_histogram(binwidth=500, aes(fill=smoker), color="Black")
#Smoker charges are less frequent, but higher

#confirm smoker versus non-smoker count in the dataset
ggplot(data=ins, aes(x=smoker, fill=gender)) + geom_bar()
#There are about 80% non-smokers, and 20% smokers; about 65% of smokers are male

#density charts
h <- ggplot(data=ins, aes(x=charges))
h + geom_density(aes(fill=smoker), position="stack")

h + geom_density(aes(fill=region), position="stack")

h + geom_density(aes(fill=gender), position="stack")


#Statistical Transformations

#geom_smooth aids the eye in seeing patterns in the presence of overplotting.
m <- ggplot(data=ins, aes(x=region))
m + geom_point() + geom_smooth()

#Using Facets
v <- ggplot(data=ins, aes(x=bmi))
v + geom_histogram(binwidth=6, aes(fill=smoker),color="Black")
#the histogram shows normal distribution

#bmi by smoker
v + geom_histogram(binwidth=6, aes(fill=smoker),color="Black")+
    facet_grid(smoker~.)

#bmi by gender
v + geom_histogram(binwidth=6, aes(fill=gender),color="Black")

v + geom_histogram(binwidth=6, aes(fill=gender),color="Black")+
    facet_grid(gender~.)

#bmi by region
v + geom_histogram(binwidth=6, aes(fill=region),color="Black")

v + geom_histogram(binwidth=6, aes(fill=region),color="Black")+ facet_grid(region~.)

#bmi by region with its own scale
v + geom_histogram(binwidth=6, aes(fill=region),color="Black")+ 
       facet_grid(region~.,scales="free") +
       xlab("BMI by region with its own scale") +
       ylab("Frequency")

#bmi by children with its own scale
v + geom_histogram(binwidth=6, aes(fill=children),color="Black")+ 
       facet_grid(children~.,scales="free")

v + geom_histogram(binwidth=6, aes(fill=children),color="Black")+ 
       facet_grid(children~.,scales="free") +
       xlab("BMI by children with its own scale") +
       ylab("Frequency")

#bmi by age_group with its own scale
v + geom_histogram(binwidth=6, aes(fill=age_group),color="Black")+ 
       facet_grid(age_group~.,scales="free")

v + geom_histogram(binwidth=6, aes(fill=age_group),color="Black")+ 
       facet_grid(age_group~.,scales="free")+
	 xlab("BMI distribution by age_group") +
       ylab("Frequency")

#histogram of charges by region
region <- ggplot(data=ins, aes(x=charges))
region + geom_histogram(binwidth=6, aes(fill=region),color="Black")+ 
       facet_grid(region~.,scales="free")


#cut out a piece, to return charges between 10K and 11K
region + geom_histogram(binwidth=6, aes(fill=region_up),color="Blue")+ 
       facet_grid(region_up~.,scales="free") +
       coord_cartesian(xlim=c(10000,11000))

#zoom into smoker status and charges between 40K and 45K
#There are no charges above 40K for non-smokers
smoker <- ggplot(data=ins, aes(x=charges))
smoker + geom_histogram(binwidth=6, aes(fill=smoker),color="Blue")+ 
       facet_grid(smoker~.,scales="free") +
       coord_cartesian(xlim=c(40000,45000))



