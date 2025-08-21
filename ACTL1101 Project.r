library(readxl)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(readr)
library(moments)

#Set the working directory
setwd("C:/Users/Quen/Documents/ACTL1101/ACTL1101 Project/")

#Create folder to store plots
dir.create("figs")

#SECTION 1 of the Report
#Cleaning up the dataframe by removing empty rows and renaming some repeated names
X14100DS0002_2017_03 <- read_excel("data/14100DS0002_2017-03.xlsx")
PopData<-X14100DS0002_2017_03
PopDataRows<-c("CODE","LABEL","YEAR",unlist(PopData[7,4:73]),unlist(PopData[6,74:92]))
PopData[8,]<-PopDataRows
PopData<-PopData[9:2863,]
colnames(PopData)<-PopDataRows
colnames(PopData)[77]<-'deathrate'

#Aboriginal data for mortality rates only appeared in 2011, so data only from 2011 was extracted into a dataframe
'Logical2011'<-PopData$'YEAR'==2011
abdat<-PopData[Logical2011,]
Abdata<-abdat[,c(77,79)]
colnames(Abdata)<-c('Deathrate','Aboriginal')

#Removal of null values '-'
LogicalAboriginalPercent<-Abdata$Aboriginal=='-'
PopDataRows<-Abdata[!LogicalAboriginalPercent,]
Logical.Aboriginal.Deathrate<-PopDataRows$Deathrate=='-'
Abdata<-PopDataRows[!Logical.Aboriginal.Deathrate,]

#Plotting distribution of mortality rates against proportion of aboriginal population and saving them
png(filename = "figs/Fig. 1.4.png",width=800,height=600)
colnames(Abdata)[2]<-'Aboriginal'
Abdata[,1]<-as.numeric(unlist(Abdata[,1]))
Abdata[,2]<-as.numeric(unlist(Abdata[,2]))
ggplot(Abdata,aes(Aboriginal,Deathrate))+geom_point()+geom_smooth(method = "lm", se = FALSE)+ggtitle('Distribution of Mortality Rates and Aboriginal Population in each LGA ') +xlab('Proportion of Population who are Aboriginal or Torres Strait Islanders')+ylab('Standardised Mortality Rates per 1000')
temp<-coef(lm(Deathrate~Aboriginal,data=Abdata))
dev.off()

#Generating summary statistics
summary(Abdata)
median(Abdata$Deathrate[Abdata$Aboriginal>0.1])
varabdata<-Abdata$Deathrate
varabdata1<-Abdata$Aboriginal
var(varabdata)
skewness(varabdata) 
var(varabdata1)
skewness(varabdata1)

#Gathering datasets for individual years using logical masks
PopData.Years<-PopData
LogicalPopData.Years<-PopData.Years$deathrate=='-'
PopData.Years<-PopData.Years[!LogicalPopData.Years,]
'Logical2011'<-PopData.Years$'YEAR'==2011 
eleven<-PopData.Years[Logical2011,]
'Logical2012'<-PopData.Years$'YEAR'==2012
twelve<-PopData.Years[Logical2012,]
'Logical2013'<-PopData.Years$'YEAR'==2013
thirteen<-PopData.Years[Logical2013,]
'Logical2014'<-PopData.Years$'YEAR'==2014
fourteen<-PopData.Years[Logical2014,]
'Logical2015'<-PopData.Years$'YEAR'==2015
fifteen<-PopData.Years[Logical2015,]
colnames(eleven)[69]<-'Totalpop'
colnames(twelve)[69]<-'Totalpop'
colnames(thirteen)[69]<-'Totalpop'
colnames(fourteen)[69]<-'Totalpop'
colnames(fifteen)[69]<-'Totalpop'

#Calculating the population mortality rate by weighting the age standardised mortality rate per county by population for each year
elevenn<-(as.numeric(eleven$deathrate)*as.numeric(eleven$Totalpop))/sum(as.numeric(eleven$Totalpop))
twelvee<-(as.numeric(twelve$deathrate)*as.numeric(twelve$Totalpop))/sum(as.numeric(twelve$Totalpop))
thirteenn<-(as.numeric(thirteen$deathrate)*as.numeric(thirteen$Totalpop))/sum(as.numeric(thirteen$Totalpop))
fourteenn<-(as.numeric(fourteen$deathrate)*as.numeric(fourteen$Totalpop))/sum(as.numeric(fourteen$Totalpop))
fifteenn<-(as.numeric(fifteen$deathrate)*as.numeric(fifteen$Totalpop))/sum(as.numeric(fifteen$Totalpop))

#Plotting the mortality rates over time and saving them
popstandard<-data.frame('el'=(sum(elevenn)),'tw'=(sum(twelvee)),'th'=(sum(thirteenn)),'fo'=(sum(fourteenn)),'fif'=(sum(fifteenn)))
newpopstandard<-data.frame('Year'=c(2011,2012,2013,2014,2015),'mortratees'=c(sum(elevenn),sum(twelvee),sum(thirteenn),sum(fourteenn),sum(fifteenn)))
png(filename = "figs/Fig. 1.1.png",width=1000,height=600)
ggplot(data=newpopstandard,aes(Year,mortratees))+geom_line()+geom_point()+ylab('Standardised Mortality Rates of Population')+ggtitle('Australian Mortality Rates over Time')
dev.off()

#Calculating cumulative distribution of mortality rates
png(filename = "figs/Fig. 1.2.png",width=800,height=600)

ggplot()+stat_ecdf(data=eleven,aes(as.numeric(deathrate)),color='red')+stat_ecdf(data=twelve,aes(as.numeric(deathrate)),color='yellow')+stat_ecdf(data=thirteen,aes(as.numeric(deathrate)),color='purple')+stat_ecdf(data=fourteen,aes(as.numeric(deathrate)),color='green')+stat_ecdf(data=fifteen,aes(as.numeric(deathrate)),color='blue')+xlab('standardised mortality rates per 1000')+ggtitle('Cumulative Distribution of Mortality Rates per LGA in Different Years')+ylab('Probability')
dev.off()

#SECTION 2 of the Report
# Importing LGA data file
X14100DS0002_2017_03 <- read.csv("data/14100DS0002_2017-03.csv")

# Extracting relevant columns
LGA_Deaths<-X14100DS0002_2017_03[-c(1:8), c(1, 2, 3, 76, 77)]
colnames(LGA_Deaths)<- c("CODE", "LGA","Year","Total Deaths","Standardised Deaths" )

# Loading LGA Remoteness Area Correspondences
LGA_RA_2011 <- read.csv("data/LGA_RA_2011.csv")
LGA_RA<- LGA_RA_2011[-c(1:6), c(1,2,4)]
colnames(LGA_RA)<-c("CODE","LGA","Remoteness Area")

# Merging both files
LGA_RA_Deaths<-merge(LGA_Deaths, LGA_RA)
LGA_RA_Deaths<-LGA_RA_Deaths[-c(1:8),]

# Removing nonexistent factors and reordering levels to reflect an increasing remoteness order
LGA_RA_Deaths$`Remoteness Area`<-factor(LGA_RA_Deaths$`Remoteness Area`)
LGA_RA_Deaths$`Remoteness Area`<-factor(LGA_RA_Deaths$`Remoteness Area`, levels = levels(LGA_RA_Deaths$`Remoteness Area`)[c(2,1,3,4,5)], ordered = TRUE)

# Converting character strings to numeric
LGA_RA_Deaths$`Standardised Deaths`<-as.numeric(as.character(LGA_RA_Deaths$`Standardised Deaths`))

# Creating and saving boxplot
png(filename = "figs/Fig. 2.1.png",width=1000,height=600)
ggplot(LGA_RA_Deaths, aes(LGA_RA_Deaths$`Remoteness Area`, LGA_RA_Deaths$`Standardised Deaths`))+
  geom_boxplot()+
  xlab("Remoteness Area")+ylab("Standardised Death Rate")+ggtitle("Boxplot of LGA Mortality Rates by Remoteness Area")+
  theme_light()
dev.off()

# Loading remoteness area mortality rate data
mort_rem <- read.csv("data/mort-rem-2010-2014.csv")
# Extracting relevant data
mort_rem<-mort_rem[-c(1:4, 31:40), c(37:53)]
# Replacing column names
colnames(mort_rem) <- as.character(unlist(mort_rem[1,]))
mort_rem<-mort_rem[-1,]
attach(mort_rem)
# Removing nonexistent factors
mort_rem$Name <- factor(mort_rem$Name)
mort_rem$Year <- factor(mort_rem$Year)
# Creating a matrix of data and converting character strings to numeric
Standardised_Rate<-matrix(data = `Age-standardised rate (per 100,000)`, nrow = 5, byrow = TRUE)
Standardised_Rate<-as.double(Standardised_Rate)
Standardised_Rate<-matrix(data = Standardised_Rate, nrow = 5, byrow = TRUE)

# Creating and saving heat map
png(filename = "figs/Fig. 2.2.png",width=1000,height=600)
image(1:5, 1:5, t(Standardised_Rate), axes = FALSE, xlab = "Remoteness Area", ylab = "Year", col = rev(heat.colors(30)), main = "Age-Standardised Rate by Remoteness Area (per 100,000)")
axis(side = 1, at = 1:5, tick = FALSE, labels = levels(mort_rem$Name)[c(2, 1, 3, 4, 5)])
axis(side = 2, at = 1:5, tick = FALSE, labels = levels(mort_rem$Year))
text(1, 1:5, Standardised_Rate[1:5])
text(2, 1:5, Standardised_Rate[1:5, 2])
text(3, 1:5, Standardised_Rate[1:5, 3])
text(4, 1:5, Standardised_Rate[1:5, 4])
text(5, 1:5, Standardised_Rate[1:5, 5])
dev.off()

# Creating matrix and converting character strings to numeric 
Standardised_PAD<-matrix(data = mort_rem$`PAD age-standardised rate (per 100,000)`)
Standardised_PAD<-as.double(Standardised_PAD)

# Creating vector of mean PAD values per remoteness area
Mean_Standardised_PAD<-c(mean(Standardised_PAD[1:5]), mean(Standardised_PAD[6:10]), mean(Standardised_PAD[11:15]), mean(Standardised_PAD[16:20]), mean(Standardised_PAD[21:25]))
# Creating and saving barplot
png(filename = "figs/Fig. 2.3.png",width=1000,height=600)
barplot(Mean_Standardised_PAD,
        main = "Potentially Avoidable Deaths (Age-Standardised Rate, per 100,000)",
        names.arg = c(levels(mort_rem$Name)[c(2,1,3,4,5)]),
        col = blues9,
        cex.names = 0.8,
        xlab = "Remoteness Area",
        ylab = "PAD Age-Standardised Rate")
dev.off()

#SECTION 3 of the Report
#Import excel file, excluding the first irrelevant lines
data.income<- read_excel("data/14100DS0006_2017-03.xlsx",skip=7)
data.mortality<-read_excel("data/14100DS0002_2017-03.xlsx",skip=8)

#Extract relevant data
data.income<-data.income[,c(1,2,3,20,21)]
data.mortality<-data.mortality[,c(1,2,3,77,79,69)]

#Changing data to numeric
data.income[4]<-as.numeric(unlist(data.income[4]))
data.income[5]<-as.numeric(unlist(data.income[5]))

data.mortality[3] <- as.numeric(unlist(data.mortality[3]))
data.mortality[4]<-as.numeric(unlist(data.mortality[4]))
data.mortality[5]<-as.numeric(unlist(data.mortality[5]))
data.mortality[6]<-as.numeric(unlist(data.mortality[6]))

#Replace NA proportion of aboriginal values with the value found within the same region
names(data.mortality)[5]<-"ATSI Proportion(%)"
setDT(data.mortality)[,`ATSI Proportion(%)`:=unique(`ATSI Proportion(%)`[!is.na(`ATSI Proportion(%)`)]),by=CODE]

#Remove NA data
data.income<-na.omit(data.income)
data.mortality<-na.omit(data.mortality)

#Name columns
names(data.income)[4]<-"Total Income ($m)"
names(data.income)[5]<-"Total Income Earners"
names(data.mortality)[4]<-"Standardised Death Rate"
names(data.mortality)[6]<-"Total Population"

#Create column that calculates the income per capita
data.income[6]<-data.income[4]/data.income[5]*1000000
names(data.income)[6]<-"Income Per Capita ($)"

#Create column to group proportion of aboriginal into interval
data.mortality$`Interval of ATSI Proportion(%)`<-cut(data.mortality$`ATSI Proportion(%)`,seq(0,100,10))

#Create column to calculate the ATSI population
data.mortality$`ATSI Population`<-data.mortality$`ATSI Proportion(%)`*data.mortality$`Total Population`/100

#Merge income and mortality data
data.income.mortality<-merge.data.frame(data.income,data.mortality)

#Extract state code
data.income.mortality[1]<-sapply(data.income.mortality[1],function(x) substring(x,first=c(1),last=c(1)))
data.income.mortality[1]<-as.numeric(unlist(data.income.mortality[1]))
names(data.income.mortality)[1]<-"code"

#Replace state code with state code name
state.code<-data.frame(code=c(1:9),name=c("NSW","VIC","QLD","SA","WA","TAS","NT","ACT","Other Territories"))
#state.code$code<-lapply(state.code$code,as.numeric)
data.income.mortality[["code"]]<-state.code[match(data.income.mortality[['code']],state.code[['code']]),'name']

#Name columns
names(data.income.mortality)[1]<-"State Name"
names(data.income.mortality)[2]<-"Region"
names(data.income.mortality)[3]<-"Year"
names(data.income.mortality)[4]<-"Total Income ($m)"
names(data.income.mortality)[5]<-"Total Income Earners"
names(data.income.mortality)[6]<-"Income Per Capita ($)"
names(data.income.mortality)[7]<-"Standardised Death Rate"
names(data.income.mortality)[9]<-"Total Population"

#View dataframe
View(data.income.mortality)

#attach
attach(data.income.mortality)

#Plot graph of income per capita vs. mortality rate and the linear regression line
png(filename = "figs/Fig. 3.1.png",width=1000,height=600)
ggplot(data=data.income.mortality,aes(x=`Income Per Capita ($)`,y=`Standardised Death Rate`))+
  geom_point(aes(color=`State Name`, shape=`Interval of ATSI Proportion(%)`),size=2)+
  geom_smooth(method = "lm",se=TRUE,color="black")+
  labs(title="Income Per Capita vs. SDR")+
  theme_classic()+theme(legend.position = c(0.85,0.73))
dev.off()

#Find correlation between income per capita and mortality rate
cor.test(data.income.mortality$`Income Per Capita ($)`,data.income.mortality$`Standardised Death Rate`)

#Calculate the ATSI proportion (%) in state
`ATSI Proportion(%)`<-aggregate(cbind(data.income.mortality$`ATSI Population`,data.income.mortality$`Total Population`)~data.income.mortality$`State Name`,data=data.income.mortality,FUN=sum)

#Boxplots & Barplot
png(filename = "figs/Fig. 3.2.png",width=500,height=600)

plot1<-ggplot(data.income.mortality,aes(x=reorder(`State Name`,`Income Per Capita ($)`,FUN=median),y=`Income Per Capita ($)`,fill=`State Name`))+
  geom_boxplot()+labs(x="State",title="Income Per Capita, SDR and ATSI Proportion(%) by State")+theme(legend.position = "none")
plot2<-ggplot(data.income.mortality,aes(x=reorder(`State Name`,`Income Per Capita ($)`,FUN=median),y=`Standardised Death Rate`,fill=`State Name`))+
  geom_boxplot()+labs(x="State")+theme(legend.position = "none")
plot3<-ggplot(data.income.mortality ,aes(reorder(`State Name`,`Income Per Capita ($)`,FUN=median),`ATSI Proportion(%)`,fill=`State Name`))+
  geom_bar(stat = "identity")+labs(x="State")+theme(legend.position = "none")

grid.arrange(ggplotGrob(plot1),ggplotGrob(plot2),ggplotGrob(plot3),layout_matrix=cbind(c(1,1,2,2,3,3)))
dev.off()

#detach
detach(data.income.mortality)

#SECTION 4 of the Report

#Import the excel file and changing relevant columns to numeric
Education <- read_excel("data/14100DS0006_2017-03.xlsx", skip=6)
#extracting useful rows and columns
Education<-Education[2:3464,c(2:3,36:42)]
#Changing column types to numeric
cols.num <- c("Percentage of total population aged 15 years and over","Postgraduate degree")
Education[cols.num] <- sapply(Education[cols.num],as.numeric)
cols.num <- c("Graduate Diploma, Graduate Certificate","Bachelor Degree")
Education[cols.num] <- sapply(Education[cols.num],as.numeric)
cols.num <- c("Advanced Diploma, or Diploma","Certificate")
Education[cols.num] <- sapply(Education[cols.num],as.numeric)
cols.num <- c("Certificate","Inadequately described, not stated")
Education[cols.num] <- sapply(Education[cols.num],as.numeric)
#Omit the rows with "NA"
Education<-na.omit(Education)
#Rename columns
names(Education)[1]<-"LABEL"
names(Education)[2]<-"YEAR"
#import another excel file and changing relevant columns to numeric format
Population <- read_excel("data/14100DS0002_2017-03.xlsx", skip = 8)
#extract relevant columns
Population<-Population[,c(2:3,76:77)]
#extract relevant rows
Population<-subset(Population,YEAR==2011)
#rename columns
names(Population)[3]<-"Number of deaths"
names(Population)[4]<-"Standardised mortality rate"
#omit NA rows
cols.num <- c("Number of deaths","Standardised mortality rate")
Population[cols.num] <- sapply(Population[cols.num],as.numeric)
Population<-na.omit(Population)
#merge two datasets by common columns
NEW<-merge.data.frame(Education,Population)
#Sum up the total percentage of those who have attained education
Educated_People_Percentages<-c(rowSums(NEW[,4:8]))
#Adding this column to the merged dataset and rename
NEW[12]<-Educated_People_Percentages
names(NEW)[12]<-"Educated_People_Percentage"

Plot data and regression line
png(filename = "figs/Fig. 4.1.png",width=1000,height=600)
Graph1<-ggplot(data=NEW,aes(x=NEW$Educated_People,y=NEW$`Standardised mortality rate`))+geom_point(size=2,aes(colour= NEW$Educated_People))+geom_smooth(method="lm",se=TRUE,colour='red')+ggtitle("Education vs. Mortality Rate")+theme(plot.title = element_text(hjust=0.5))
#to find the gradient and intercept of the regression line
coef(lm(NEW$`Standardised mortality rate`~NEW$Educated_People,data=NEW))
#change axis names and scales
Graph1+scale_x_continuous(name="Educated Persons(%)", limits=c(min(NEW$Educated_People),max(NEW$Educated_People)),breaks=pretty(NEW$Educated_People),n=5)+scale_y_continuous(name="Mortality Rate(%)",limits=c(min(NEW$`Standardised mortality rate`),max(NEW$`Standardised mortality rate`)),breaks=pretty(NEW$`Standardised mortality rate`),n=2)+ annotate("text",x=40,y=13,label="Gradient of regression line=-0.07242715", size=5, col='purple4')#annotate the gradient onto the plot
dev.off()

#To find correlation between education and mortality
cor.test(NEW$Educated_People,NEW$`Standardised mortality rate`)
#Import another file on education
Education_dataset <- read_excel("data/Education dataset.xlsx", sheet = "Sheet2")
View(Education_dataset)
#Extract the columns and rows relevant to "Male"
Education_dataset_Male<-Education_dataset[c(1:3,7:9),c(1,3:4)]
#Group the information into a table and rename columns and rows according to variables
Table_Male<-matrix(c(Education_dataset_Male$`Residual life expectancy`),ncol=3,byrow=TRUE)
colnames(Table_Male)<-c(Education_dataset_Male$Ages[1:3])
rownames(Table_Male)<-c(Education_dataset_Male$Education[c(1,4)])
#Extract the columns and rows relevant to "Female"
Education_dataset_Female<-Education_dataset[c(4:6,10:12),c(1,3:4)]
#Group the information into a table and rename columns and rows according to variables
Table_Female<-matrix(c(Education_dataset_Female$`Residual life expectancy`),ncol=3,byrow=TRUE)
colnames(Table_Female)<-c(Education_dataset_Female$Ages[1:3])
rownames(Table_Female)<-c(Education_dataset_Female$Education[c(1,4)])
#Changing format of the two tables
Table_Male<-as.table(Table_Male)
Table_Female<-as.table(Table_Female)

#To plot the two side-by-side barplots
png(filename = "figs/Fig. 4.2.png",width=1000,height=600)
par(mfrow=c(1,2))
Plot_Male<-barplot(Table_Male,bes=TRUE,xlab = "Age(years)",ylab = "Residual Life Expectancy(years)",ylim = c(0,70),main = "Education vs Life Expectancy Male",col=c("lightblue","lightgreen"),legend=c("12 years","over 12 years"),args.legend = list(title="Level of Education",cex=0.7))
#Adding the values onto the plot
text(Plot_Male, 0, round(Table_Male, 2),cex=1,pos=3)

#Plotting the second barplot right next to the first
Plot_Female<-barplot(Table_Female,bes=TRUE,xlab = "Age(years)",ylab = "Residual Life Expectancy(years)",ylim = c(0,70),main = "Education vs Life Expectancy Female",col=c("mistyrose","lavender"),legend=c("12 years","over 12 years"),args.legend = list(title="Level of Education",cex=0.7))
#adding values once again
text(Plot_Female, 0, round(Table_Female, 2),cex=1,pos=3)
dev.off()

#import a csv dataset
Education2016<- read_csv("data/2016Census_I06_AUS_LGA Education.csv")
Education2016<-Education2016[,c(1:3)]
names(Education2016)[2]<-"Indigenous"
names(Education2016)[3]<-"Non-Indigenous"
#plot a scatterplot with two variables
df_melt <- reshape2::melt(Education2016, id.var = 'LGA_CODE_2016')
png(filename = "figs/Fig. 4.3.png",width=1000,height=600)
Graph<-ggplot(df_melt, aes(x = factor(LGA_CODE_2016), y = value, colour = variable)) +  geom_point() + xlab('LGA_2016')+ggtitle("Year 12 or equivalent") + theme(plot.title = element_text(hjust=0.5))
#remove some outliers
Graph+scale_y_continuous(name="number of people",limits=c(0,10000))
dev.off()

#SECTION 5 of the Report
#read the mortality table
MORT <- read_excel("data/Mortality.xlsx", sheet="Table 1")
#compact the mortality book so that a simple dataset can be produced
MORT[c(5:45,51:55),c(38,39,43)]->MORT.death
#import the dataset regarding the offender rates in various states (this data was extracted from another file called "Offender States Territories.xlsx which is included)
States_Statistics_Extracted <- read_excel("data/States Statistics Extracted.xlsx")
#clearing out unnecessary columns to make the dataset cleaner
Offender.States<-States_Statistics_Extracted[,2:4]
#creating a new combined dataset in the original order
Compare.Offender<-merge(MORT.death,Offender.States, by = (c(1,2)),sort=F)
#make the first row become the column headers
names(Compare.Offender) <- Compare.Offender[1, ]
Compare.Offender <- Compare.Offender[-1, ]
Compare.Offender[] <- lapply(Compare.Offender, function(x) type.convert(as.character(x)))
#convert all the state names into abbreviations so they can fit onto the heatmap
levels(Compare.Offender$Name) <- sub("New South Wales", "NSW", levels(Compare.Offender$Name))
levels(Compare.Offender$Name) <- sub("Victoria", "Vic.", levels(Compare.Offender$Name))
levels(Compare.Offender$Name) <- sub("Queensland", "Qld.", levels(Compare.Offender$Name))
levels(Compare.Offender$Name) <- sub("Western Australia", "WA", levels(Compare.Offender$Name))
levels(Compare.Offender$Name) <- sub("South Australia", "SA", levels(Compare.Offender$Name))
levels(Compare.Offender$Name) <- sub("Tasmania", "Tas.", levels(Compare.Offender$Name))
levels(Compare.Offender$Name) <- sub("Australian Capital Territory", "ACT", levels(Compare.Offender$Name))
levels(Compare.Offender$Name) <- sub("Northern Territory", "NT", levels(Compare.Offender$Name))
#make the column headers factors
Compare.Offender$Name <- factor(Compare.Offender$Name)
Compare.Offender$Year <- factor(Compare.Offender$Year)
#create a new dataset which can be used for the heatmap of the standardised death rate
Mortality<-Compare.Offender[,1:3]
#attach the data so that R can read it easily
attach(Mortality)
#create a matrix with the numeric values of the standardised death rate as double
Standardised_Rate<-matrix(data = `Age-standardised rate (per 100,000)`, nrow = 8, byrow = TRUE)
Standardised_Rate<-as.double(Standardised_Rate)
Standardised_Rate<-matrix(data = `Age-standardised rate (per 100,000)`, nrow = 8, byrow = TRUE)
png(filename = "figs/Fig. 5.1.png",width=700,height=600)
image(1:8, 1:5, Standardised_Rate, axes = FALSE, xlab = "State", ylab = "Year", col = rev(heat.colors(30)), main = "Standardised Death Rate by State (per 100,000)")
#add axis labels using the original Compare Offender dataset
axis(side = 1, at = 1:8, tick = FALSE, labels = levels(Compare.Offender$Name)[c(2,7,4,8,5,6,1,3)])
axis(side = 2, at = 1:5, tick = FALSE, labels = levels(Compare.Offender$Year)[1:5])
#add the values of the standardised death rate onto the heatmap to allow for effective visual communication of the information
text(1, 1:8, t(Standardised_Rate)[1:5])
text(2, 1:8, t(Standardised_Rate)[1:5, 2])
text(3, 1:8, t(Standardised_Rate)[1:5, 3])
text(4, 1:8, t(Standardised_Rate)[1:5, 4])
text(5, 1:8, t(Standardised_Rate)[1:5, 5])
text(6, 1:8, t(Standardised_Rate)[1:5, 6])
text(7, 1:8, t(Standardised_Rate)[1:5, 7])
text(8, 1:8, t(Standardised_Rate)[1:5, 8])
#to stop imaging, the dev.off function must be used
dev.off()
#now to detach the data so another dataset can be attached
detach(Mortality)

#NOW, to create a heat map representing the offender rate, the following extraction from the original data set must be made
Compare.Offender[,c(1,2,4)]->Offender_Rate
#attaching the data again to allow for easy comprehension by R
attach(Offender_Rate)
#now to create a matrix where the values are set as doubles, hence allowing a heatmap to be made out of it
Offender_Matrix<-matrix(data = `Offender Rate per 100,000`, nrow = 8, byrow = TRUE)
Offender_Matrix<-as.double(Offender_Matrix)
Offender_Matrix<-matrix(data = `Offender Rate per 100,000`, nrow = 8, byrow = TRUE)
#then the heat map can be created
png(filename = "figs/Fig. 5.2.png",width=700,height=600)
image(1:8, 1:5, Offender_Matrix, axes = FALSE, xlab = "State", ylab = "Year", col = rev(heat.colors(30)), main = "Offender Rate by State (per 100,000)")
#add axis labels using the original Compare Offender dataset
axis(side = 1, at = 1:8, tick = FALSE, labels = levels(Offender_Rate$Name)[c(2,7,4,8,5,6,1,3)])
axis(side = 2, at = 1:5, tick = FALSE, labels = levels(Offender_Rate$Year)[1:5])
#add the values of the offender rate onto the heatmap to allow for effective visual communication of the information
text(1, 1:8, t(Offender_Matrix)[1:5])
text(2, 1:8, t(Offender_Matrix)[1:5, 2])
text(3, 1:8, t(Offender_Matrix)[1:5, 3])
text(4, 1:8, t(Offender_Matrix)[1:5, 4])
text(5, 1:8, t(Offender_Matrix)[1:5, 5])
text(6, 1:8, t(Offender_Matrix)[1:5, 6])
text(7, 1:8, t(Offender_Matrix)[1:5, 7])
text(8, 1:8, t(Offender_Matrix)[1:5, 8])
dev.off()
#similarly detach the dataset for a new dataset to be attached later on
detach(Offender_Rate)
#testing correlation, we can use the cor.test function
cor.test(Compare.Offender$`Age-standardised rate (per 100,000)`, Compare.Offender$`Offender Rate per 100,000`, method = c("pearson"))

#read the indigenous/non-indigenous crime table
read_excel("data/Indigenous Crime.xls", sheet="Table 23")->Indigenous_Crime
#clear out any unnecessary cells that are not needed (ACT was not included because there was not enough sufficient data for all the years)
Indigenous_Crime<-Indigenous_Crime[c(15,18:22),c(1:3,5,6,8,9,11,12)]
#Add the state names in the first row so they can easily be recognised
#Add the string year to the first row, first column so it is obvious that the column is regarding the year
#Also change the name of the year to a specific year, so it can be compared other data
Indigenous_Crime[1,2:3]<-"NSW"
Indigenous_Crime[1,4:5]<-"Qld"
Indigenous_Crime[1,6:7]<-"SA"
Indigenous_Crime[1,8:9]<-"NT"
Indigenous_Crime[2,1]<-"2010"
Indigenous_Crime[3,1]<-"2011"
Indigenous_Crime[4,1]<-"2012"
Indigenous_Crime[5,1]<-"2013"
Indigenous_Crime[6,1]<-"2014"
Indigenous_Crime[1,1]<-"Year"

#in order to visualise the indigenous offenders by themselves, a new dataset must be created this must be done through various data manipulations in R shown below
#first a loop of the years between 2010-2014 must be created in a dataset
Indigenous_Only_Crime<-rbind(Indigenous_Crime[2:6,1],Indigenous_Crime[2:6,1],Indigenous_Crime[2:6,1],Indigenous_Crime[2:6,1])
#a new column must be created such that data from the original dataset Indigenous_Crime can be input
Indigenous_Only_Crime["Indigenous Offender Rate"] <- NA
#now the original data is input
Indigenous_Only_Crime[1:5,2] <- Indigenous_Crime[2:6,2]
Indigenous_Only_Crime[6:10,2] <- Indigenous_Crime[2:6,4]
Indigenous_Only_Crime[11:15,2] <- Indigenous_Crime[2:6,6]
Indigenous_Only_Crime[16:20,2] <- Indigenous_Crime[2:6,8]
#create another column at the front so that the states can be input, allowing for a clearer visualistation
cbind(Name = 0, Indigenous_Only_Crime)->Indigenous_Only_Crime
#insert the state names
Indigenous_Only_Crime[1:5,1]<-"NSW"
Indigenous_Only_Crime[6:10,1]<-"Qld."
Indigenous_Only_Crime[11:15,1]<-"SA"
Indigenous_Only_Crime[16:20,1]<-"NT"
#change the name of the column header of the second row to "Year"
colnames(Indigenous_Only_Crime)[2]<-"Year"
colnames(Indigenous_Only_Crime)[3]<-"Indigenous Offender Rate (per 100,000)"
#make the column headers factors
Indigenous_Only_Crime$Name <- factor(Indigenous_Only_Crime$Name)
Indigenous_Only_Crime$Year <- factor(Indigenous_Only_Crime$Year)
#now a dataset named Indigenous_Only_Crime has been created such that it can be analysed to display statistics about Indigenous Offenders by state
#attach the data so that R can read it easily
attach(Indigenous_Only_Crime)
#create a matrix with the numeric values of the standardised death rate as double
Ind_Offender_mat<-matrix(data = `Indigenous Offender Rate (per 100,000)`, nrow = 4, byrow = TRUE)
Ind_Offender_mat<-as.double(Ind_Offender_mat)
Ind_Offender_mat<-matrix(data = Ind_Offender_mat, nrow = 5, byrow = TRUE)
png(filename = "figs/Fig. 5.3.png",width=700,height=600)
image(1:4, 1:5, t(Ind_Offender_mat), axes = FALSE, xlab = "State", ylab = "Year", col = rev(heat.colors(30)), main = "Age-Standardised Indigenous Offender Rate by State (per 100,000)")
#add axis labels using the original Compare Offender dataset
axis(side = 1, at = 1:4, tick = FALSE, labels = levels(Indigenous_Only_Crime$Name)[c(1,3,4,2)])
axis(side = 2, at = 1:5, tick = FALSE, labels = levels(Indigenous_Only_Crime$Year)[1:5])
#add the values of the standardised death rate onto the heatmap to allow for effective visual communication of the information
text(1, 1:5, Ind_Offender_mat[1:5,1])
text(2, 1:5, Ind_Offender_mat[1:5, 2])
text(3, 1:5, Ind_Offender_mat[1:5, 3])
text(4, 1:5, Ind_Offender_mat[1:5, 4])
dev.off()
detach(Indigenous_Only_Crime)