##################################################
################### CONSTANTS ####################
##################################################

# Period range, data source
START_YEAR = 2008
END_YEAR = 2014

##################################################
################### PACKAGES #####################
##################################################

# Load the libraries
library('WDI')            # Retrieve data from worldbank website
library('countrycode')    # Convert iso2c to iso3c
library("plyr")           # plyr data manipulation
library("dplyr")          # dplyr data manipulation
library("magrittr")       # magrittr syntax
library('ggplot2')
library('ggrepel')
library('gridExtra')
library('grid')
library('reshape2')       # melt() function
library('arules')         # association rules
library('cluster')        # cluster analysis

##################################################
################### VARIABLES ####################
##################################################

# Full list of indicators
indicators.full.list = list(
  c("Technology","IT.NET.BBND.P2","internet_subscriptions_percent","Fixed broadband subscriptions (per 100 people)","Fixed broadband subscriptions refers to fixed subscriptions to high-speed access to the public Internet (a TCP/IP connection), at downstream speeds equal to, or greater than, 256 kbit/s. This includes cable modem, DSL, fiber-to-the-home/building, other fixed (wired)-broadband subscriptions, satellite broadband and terrestrial fixed wireless broadband. This total is measured irrespective of the method of payment. It excludes subscriptions that have access to data communications (including the Internet) via mobile-cellular networks. It should include fixed WiMAX and any other fixed wireless technologies. It includes both residential subscriptions and subscriptions for organizations."),
  c("Technology","IT.NET.USER.P2","internet_users_percent","Internet users (per 100 people)","Internet users are individuals who have used the Internet (from any location) in the last 12 months. Internet can be used via a computer, mobile phone, personal digital assistant, games machine, digital TV etc."),
  c("Technology","IT.CEL.SETS.P2","cell_users_percent","Mobile cellular subscriptions (per 100 people)","Mobile cellular telephone subscriptions are subscriptions to a public mobile telephone service that provide access to the PSTN using cellular technology. The indicator includes (and is split into) the number of postpaid subscriptions, and the number of active prepaid accounts (i.e. that have been used during the last three months). The indicator applies to all mobile cellular subscriptions that offer voice communications. It excludes subscriptions via data cards or USB modems, subscriptions to public mobile data services, private trunked mobile radio, telepoint, radio paging and telemetry services."),
  c("Technology","IP.JRN.ARTC.SC","scientific_publications","Scientific and technical journal articles","Scientific and technical journal articles refer to the number of scientific and engineering articles published in the following fields: physics, biology, chemistry, mathematics, clinical medicine, biomedical research, engineering and technology, and earth and space sciences."),
  c("Technology","IT.NET.SECR.P6","internet_servers_permillion","Secure Internet servers (per 1 million people)","Secure servers are servers using encryption technology in Internet transactions."),
  c("Technology","BX.GSR.CCIS.CD","ict_service_exports","ICT service exports (BoP, current US$)","Information and communication technology service exports include computer and communications services (telecommunications and postal and courier services) and information services (computer data and news-related service transactions). Data are in current U.S. dollars."),
  c("Technology","BX.GSR.CCIS.ZS","ict_service_exports_percent","ICT service exports (% of service exports, BoP)","Goods exports refer to all movable goods (including nonmonetary gold and net exports of goods under merchanting) involved."),
  c("Technology","BX.GSR.MRCH.CD","ict_good_exports","Goods exports (BoP, current US$)","Information and communication technology goods imports include telecommunications, audio and video, computer and related equipment; electronic components; and other information and communication technology goods. Software is excluded."),
  c("Technology","TX.VAL.ICTG.ZS.UN","ict_good_exports_percent","ICT goods exports (% of total goods exports)","Information and communication technology goods exports include telecommunications, audio and video, computer and related equipment; electronic components; and other information and communication technology goods. Software is excluded."),
  c("Technology","IT.NET.BBND","internet_subscriptions","Fixed broadband subscriptions","Fixed broadband subscriptions refers to fixed subscriptions to high-speed access to the public Internet (a TCP/IP connection), at downstream speeds equal to, or greater than, 256 kbit/s. This includes cable modem, DSL, fiber-to-the-home/building, other fixed (wired)-broadband subscriptions, satellite broadband and terrestrial fixed wireless broadband. This total is measured irrespective of the method of payment. It excludes subscriptions that have access to data communications (including the Internet) via mobile-cellular networks. It should include fixed WiMAX and any other fixed wireless technologies. It includes both residential subscriptions and subscriptions for organizations."),
  c("Technology","IT.NET.SECR","internet_servers","Secure Internet servers","Secure servers are servers using encryption technology in Internet transactions."),
  c("Physiology","SH.STA.ACSN","improved_sanitation_percent","Improved sanitation facilities (% of population)","Access to improved sanitation facilities refers to the percentage of the population using improved sanitation facilities. Improved sanitation facilities are likely to ensure hygienic separation of human excreta from human contact. They include flush/pour flush (to piped sewer system, septic tank, pit latrine), ventilated improved pit (VIP) latrine, pit latrine with slab, and composting toilet."),
  c("Safety","SP.DYN.LE00.IN","life_expectancy","Life expectancy at birth, total (years)",""),
  c("Safety","VC.IHR.PSRC.P5","homicide_percent_1000","Intentional homicides (per 100,000 people)",""),
  c("Gender Equality","SG.GEN.PARL.ZS","women_parliament_percent","% of seats held by women in national parliaments","Women in parliaments are the percentage of parliamentary seats in a single or lower chamber held by women."),
  c("Gender Equality","SL.TLF.TOTL.FE.ZS","women_laborforce_percent","Labor force, female (% of total labor force)","Female labor force as a percentage of the total show the extent to which women are active in the labor force. Labor force comprises people ages 15 and older who meet the International Labour Organization's definition of the economically active population."),
  c("Education","SE.PRM.ENRL","enrolment_primary_education","Enrolment in primary education, both sexes (number)","Total number of students enrolled in public and private primary education institutions regardless of age."),
  c("Education","SE.SEC.ENRL.GC","enrolment_secondary_education","Enrolment in secondary general, both sexes (number)","Total number of students enrolled in general programmes at public and private secondary education institutions regardless of age."),
  c("Fertility","SP.DYN.TFRT.IN","fertility_rate","Fertility rate, total (births per woman)","Total fertility rate represents the number of children that would be born to a woman if she were to live to the end of her childbearing years and bear children in accordance with current age-specific fertility rates."),
  c("Economy","NY.GDP.PCAP.CD","GDP_per_capita","GDP per capita (current US$)","GDP per capita is gross domestic product divided by midyear population. GDP is the sum of gross value added by all resident producers in the economy plus any product taxes and minus any subsidies not included in the value of the products. It is calculated without making deductions for depreciation of fabricated assets or for depletion and degradation of natural resources. Data are in current U.S. dollars."),
  c("Economy","SL.UEM.TOTL.ZS","unemployement_over_laborforce_percent","Unemployment, total (% of total labor force)","Unemployment refers to the share of the labor force that is without work but available for and seeking employment.")
)

# Extract series codes to the `indicators.codes` variable
indicators.codes = sapply(indicators.full.list, function(x) x[2])

# Extract indicator user-friendly names to the `indicators.names` variable
indicators.names = sapply(indicators.full.list, function(x) x[3])

# Extract indicator full names to the `indicators.full.names` variable
indicators.full.names = sapply(indicators.full.list, function(x) x[4])

# Extract indicator topics to the `indicators.topics` variable
indicators.topics = sapply(indicators.full.list, function(x) x[1])

# Extract indicator topics to the `indicators.definitions` variable
indicators.definitions = sapply(indicators.full.list, function(x) x[5])

# Create indicators.df to store indicators info in a readable format 
indicators.df = data.frame(indicators.codes, indicators.names, indicators.full.names, indicators.definitions, indicators.topics)

# Print friendly indicator.df
indicators.df %>%
  select(indicators.topics, indicators.names)


##################################################
################ WDI DATA EXTRACT ################
##################################################

# Read the WDI data for the `indicator.codes` into the `df` dataframe
df <- WDI(indicator=indicators.codes, start = START_YEAR, end = END_YEAR,
          extra=TRUE # returns additional variables, see documentation
)

# Rename variables in `df` using user friendly indicator names
# Not using dplyr `rename` because changing variable set over time
for (i in 1:length(indicators.codes))
{
  colnames(df)[which(names(df) == indicators.codes[i])] <- indicators.names[i]
}


##################################################
#################### ISO3C #######################
##################################################

# Convert iso2c to iso3c with `countrycode-package`
df$iso3c <- countrycode(df$iso2c, "iso2c", "iso3c", warn = FALSE)

# Necessary because WDI iso3c did not respect international std.
# Will be critical to filter out non-country entities from set.
#     eg: iso3c of 'World' before: "WLD"
#         iso3c of 'World' after:  NA

##################################################
################# AS.FACTOR ######################
##################################################

# Drop variables that I don't want
# Change other variables to factors
df %>%
  select(-capital,
         -longitude,
         -latitude,
         -lending,
         -iso2c) %>%
  mutate(country=factor(country),
         iso3c  =factor(iso3c),
         year   =factor(year),
         region =factor(region),
         income =factor(income)) %>%
         { . } -> df

##################################################
##### FILTER: Country vs Non-country entities ####
##################################################

# Regroup records from non-country entities in `aggregates.df`
aggregates.df <- subset(df, is.na(iso3c))

# Regroup records from country entities in `countries.df`
countries.df <- subset(df, !is.na(iso3c))

##################################################
############ Deal with missing data ##############
##################################################

# For each country, retrieve most recent indicator data available
# on period (START_YEAR - END_YEAR)

# Initialize final.df with distinct ISO3C code for each country
countries.df %>%
  select(iso3c, country, income) %>%
  distinct() -> final.df

# Generate intermediate table to store
# most recent data available by country for a given variable
get_recent_data <- function(df, var){
  df %>%
    mutate(year = as.numeric(levels(df$year))[df$year]) %>%
    select(iso3c, year, match(var, names(.))) %>%
    na.omit() %>%
    group_by(iso3c) %>%
    filter(year == max(year)) %>%
    select(iso3c, match(var, names(.))) -> recent.data.df
  
  return(recent.data.df)
}

# Left join the indicator data with the ISO3C code incrementally
# to build summary data.frame by indicator
for (i in 1:length(indicators.names)){
  x <- get_recent_data(countries.df, indicators.names[i])
  final.df %>% 
    left_join(x) -> final.df
}


##################################################
################ QUANTILIZE ######################
##################################################

# helper; @return `cutoff` vector
cutoff <- function(num_quantiles)
{
  i <- 1/num_quantiles
  cutoff = list()
  for (k in 1:(num_quantiles-1)){
    cutoff[k] <- k*i
  }
  cutoff_vec = unlist(cutoff)
  return(cutoff_vec)
}

# helper; @return quantile `labels` vector
labelize.it <- function(num_quantiles)
{
  labels = list()
  for (k in 1:num_quantiles){
    labels[k] <- paste(num_quantiles-k+1,'/',num_quantiles,'th',sep='')
  }
  labels_vec = unlist(labels)
  return(labels_vec) 
}

# main;  @return factor vector of quantiles for a given numeric vector variable
quantilize <- function(vec, num_quantiles)
{
  if(!is.numeric(vec)){
    stop('Please choose numeric vector')
  }
  
  quantile(vec,
           cutoff(num_quantiles),
           na.rm=TRUE 
  ) %>%
  { c(-Inf, ., Inf) } %>%
    cut(vec,
        breaks = .,
        labels = labelize.it(num_quantiles)
    )
}

# Quantilize some of the numeric variables in `final.df` for graph categories
final.df %>%
  mutate(
    GDP_per_capita_factor = quantilize(final.df$GDP_per_capita, 4),
    internet_servers_factor = quantilize(final.df$internet_servers, 4)
  ) %>%
  { . } -> final.df


##################################################
################ SUMMARY BY VARIABLE #############
##################################################

#************** Internet users ******************#

# Internet Users (%population)

# HISTOGRAM: Country distribution by proportion of internet users
p1.internet <- ggplot(data=final.df, aes(internet_users_percent)) +
  geom_histogram(breaks=seq(0, 100, by=25),
                 col="black", 
                 fill ="#139194", 
                 alpha = .9) +
  labs(title="Country distribution\nby proportion of internet users") +
  labs(x="Internet Users (%)", y="Country count") + 
  xlim(c(-5,105))

p1.internet


# Internet Users (%population) vs GDP per Capita

# Helper df free of NA values: Internet users | GDP
internet.gdp.df <-
  final.df %>%
  select(internet_users_percent, GDP_per_capita, GDP_per_capita_factor) %>%
  na.omit()

# Coeff Correlation: internet usage vs gdp
cor.internet.gdp <- 
  cor(internet.gdp.df$GDP_per_capita, internet.gdp.df$internet_users_percent) %>%
  round(digits = 3) %>%
  paste("CORRELATION = ",.)

# SCATTERPLOT: internet user vs GDP
internet.vs.gdp <-
  ggplot(internet.gdp.df, aes(x = GDP_per_capita, y = internet_users_percent)) +
  geom_point() +
  annotate("text", x=100000, y=25, label = cor.internet.gdp) +
  ggtitle("GDP per capita\nVS\nproportion of Internet users in countries")

internet.vs.gdp

# DENSITY PLOT: Country distribution of Internet users(%) categorized by GDPpCapita
density.internet.gdp <-
  ggplot(internet.gdp.df, aes(x=internet_users_percent, fill=GDP_per_capita_factor)) +
  geom_density(alpha=.3) +
  ggtitle("GDP per capita vs proportion of Internet users \n  View per quantile of GDP per capita")

density.internet.gdp


#*************** Cell owners *********************#

# HISTOGRAM:
# Country distribution by proportion of Cell users
p1.cell <-
  ggplot(final.df, aes(x = cell_users_percent)) +
  geom_histogram(breaks=seq(0, 250, by=25),
                 col="black", 
                 fill ="#139194", 
                 alpha = .9) +
  labs(title="Country distribution\nby proportion of Cell owners") +
  labs(x="Cell owners (%)", y="Country count") + 
  xlim(c(-5,260)) +
  scale_x_continuous(breaks=seq(0, 250, by=25), labels=seq(0, 250, by=25)) +
  scale_y_continuous(breaks=seq(0, 70, by=10), labels=seq(0, 70, by=10))

p1.cell


# DENSITY PLOT:
# Country distribution of Cell owners(%) - by GDPpCapita

# Helper df free of NA values: Cell users | GDP
cell.gdp.df <-
  final.df %>%
  select(cell_users_percent, GDP_per_capita, GDP_per_capita_factor) %>%
  na.omit()

# DENSITY PLOT
density.cell.gdp <-
  ggplot(cell.gdp.df, aes(x=cell_users_percent, fill=GDP_per_capita_factor)) +
  geom_density(alpha=.3) +
  ggtitle("GDP per capita vs proportion of cell-phone owners \n 
          View per quantile of GDP per capita")

density.cell.gdp

#*************** Internet servers (ecommerce) ******************#

# SCATTERPLOT : Intensity of ecommerce vs Economic performance

# Helper df free of NA values: ecommerce.df
# Only top quartile in volume of internet servers
ecommerce.df <-
  final.df %>%
  select(country, internet_servers, internet_servers_factor, internet_servers_permillion, GDP_per_capita) %>%
  filter(internet_servers_factor == "1/4th" ) %>%
  na.omit()

# SCATTERPLOT
p.ecommerce <-
  ggplot(ecommerce.df, aes(x = GDP_per_capita, y = internet_servers_permillion)) +
  geom_point() +
  ggtitle('Intensity of ecommerce vs Economic performance') +
  geom_text_repel(data=filter(ecommerce.df, internet_servers_permillion>500), aes(label=country), size=3)

p.ecommerce



#BAR PLOT: top10 Servers countries + Server per million

# Helper df free of NA values: internet.servers.df
internet.servers.df <-
  final.df %>%
  select(country, internet_servers, internet_servers_permillion) %>%
  na.omit() %>%
  arrange(desc(internet_servers)) %>%
  slice(1:10) #%>%
#mutate(internet_servers_label = paste(round(internet.servers.df[,2]/1000,1),"K",sep=''))

# Source: http://stackoverflow.com/questions/9852270/barplot-with-2-variables-2-y-axis
test <- data.frame(country = internet.servers.df$country,
                   internet_servers = internet.servers.df$internet_servers,
                   internet_servers_permillion = internet.servers.df$internet_servers_permillion)

funProp <- function(testCol) {
  test[, testCol]/max(test[, testCol])
}

test$internet_servers.prop <- funProp("internet_servers")
test$internet_servers_permillion.prop <- funProp("internet_servers_permillion")

# Configure right and left axis
myLeftAxisLabs <- pretty(seq(0, max(test$internet_servers), length.out = 10))
myRightAxisLabs <- pretty(seq(0, max(test$internet_servers_permillion), length.out = 10))

myLeftAxisAt <- myLeftAxisLabs/max(test$internet_servers)
myRightAxisAt <- myRightAxisLabs/max(test$internet_servers_permillion)

#BAR PLOT
barplot(t(as.matrix(test[, c("internet_servers.prop", "internet_servers_permillion.prop")])),
        beside = TRUE,
        yaxt = "n",
        names.arg = test$country,
        main = "Top countries by number of secure servers",
        col = c("#F38630", "#E0E4CC"),
        cex.names=0.8,
        las = 2)
axis(2, at = myLeftAxisAt, labels = myLeftAxisLabs)
axis(4, at = myRightAxisAt, labels = myRightAxisLabs)
legend("top", 
       legend = c("Servers","Servers per million"), 
       fill = c("#F38630", "#E0E4CC"),
       cex = 0.6)

#*************** Internet subscription ******************#
#Helper df
internet.subscriptions.df <-
  final.df %>%
  select(country, internet_users_percent, internet_subscriptions_percent, GDP_per_capita_factor) %>%
  mutate(suscriptions_density = internet_subscriptions_percent/internet_users_percent) %>%
  na.omit()

#SCATTER PLOT:
p.internet.subscriptions <-
  ggplot(internet.subscriptions.df, aes(x = internet_users_percent, y = internet_subscriptions_percent)) +
  geom_point(aes(color = GDP_per_capita_factor)) +
  ggtitle('Internet Subscriptions(%)\nVS\nInternet Users(%)\nby economic performance') +
  labs(x="Percentage of internet users",y="Percentage of people with internet subscription")

p.internet.subscriptions

p.internet.subscriptions.gdp <-
  p.internet.subscriptions <-
  ggplot(internet.subscriptions.df, aes(x = GDP_per_capita_factor, y = suscriptions_density)) +
  geom_point(aes(color = GDP_per_capita_factor)) +
  ggtitle('Economic Performance:\n influence on the density of subscriptions across internet users') +
  labs(x="GDP per capita",y="Density of subscriptions across internet users") +
  theme(legend.position="none")

p.internet.subscriptions.gdp

#*****************Gender Equality*****************# 

#Helper df
women.df <-
  final.df %>%
  select(country, women_parliament_percent, women_laborforce_percent, GDP_per_capita_factor) %>%
  na.omit()

#SCATTER PLOT : x=women_parliament_percent, y = women_laborforce_percent
p.women <-
  ggplot(women.df, aes(x = women_parliament_percent, y = women_laborforce_percent)) +
  geom_point(aes(color = GDP_per_capita_factor)) +
  ggtitle('Gender Equality - Leadership role in society and general labor')

p.women


#*****************Fertility*****************#   

fertility.df <-
  final.df %>%
  select(country, fertility_rate, GDP_per_capita, GDP_per_capita_factor) %>%
  na.omit()

p.fertility <-
  ggplot(fertility.df, aes(x = GDP_per_capita, y = fertility_rate)) +
  geom_point(aes(color = GDP_per_capita_factor)) +
  ggtitle('Fertility rate of countries by GDP per capita') +
  labs(x="GDP per Capita", y="Fertility rate") +
  geom_hline(yintercept = 2.33) +
  annotate("text", x = 75000, y = 2.5, label = "Total fertility rate at replacement: 2.33")

p.fertility



##################################################
############### ASSOCIATION RULES ################
##################################################

# Setup a dataframe to be used by the `apriori` function
countries.df %>%
  mutate(
    internet_subscriptions_percent = quantilize(internet_subscriptions_percent,4),
    internet_users_percent         = quantilize(internet_users_percent,4),
    cell_users_percent             = quantilize(cell_users_percent,4),
    scientific_publications        = quantilize(scientific_publications,4),
    internet_servers_permillion    = quantilize(internet_servers_permillion,4),
    ict_service_exports            = quantilize(ict_service_exports,4),
    ict_service_exports_percent    = quantilize(ict_service_exports_percent,4),
    ict_good_exports               = quantilize(ict_good_exports,4),
    ict_good_exports_percent       = quantilize(ict_good_exports_percent,4),
    internet_subscriptions         = quantilize(internet_subscriptions,4),
    internet_servers               = quantilize(internet_servers,4),
    improved_sanitation_percent    = quantilize(improved_sanitation_percent,4),
    life_expectancy                = quantilize(life_expectancy,4),
    homicide_percent_1000          = quantilize(homicide_percent_1000,4),
    women_parliament_percent       = quantilize(women_parliament_percent,4),
    women_laborforce_percent       = quantilize(women_laborforce_percent,4),
    enrolment_primary_education    = quantilize(enrolment_primary_education,4),
    enrolment_secondary_education  = quantilize(enrolment_secondary_education,4),
    fertility_rate                 = quantilize(fertility_rate,4),
    GDP_per_capita                        = quantilize(GDP_per_capita,4),
    unemployement_over_laborforce_percent = quantilize(unemployement_over_laborforce_percent,4)
  ) %>%
  { . } -> df.arules


# Run the `apriori` function
rules = apriori(df.arules, 
                parameter=list(maxlen=2,
                               support=0.01,
                               confidence=0.01))

# Select the rules
rules %>%
  subset(lift>1.2) %>%       # filter the rules
  sort(by='lift') %>%        # sort the rules
  { . } -> cool.rules

cool.rules %>%
  { .[c(5)] } %>%             # display only rule 5
  { . } -> cool.rules1

cool.rules %>%
  { .[c(7)] } %>%             # display only rule 7
  { . } -> cool.rules2

cool.rules %>%
  { .[c(9)] } %>%             # display only rule 9
  { . } -> cool.rules3

cool.rules %>%
  { .[c(11)] } %>%             # display only rule 11
  { . } -> cool.rules4

cool.rules %>%
  { .[c(13)] } %>%             # display only rule 13
  { . } -> cool.rules5

inspect(cool.rules)
str(df.arules)



##################################################
#################### CLUSTERING ##################
##################################################

#####################################
#Helper function: Numeric variable normalization
normalize1 = function(vec) (vec-mean(vec, na.rm=TRUE))/sd(vec, na.rm=TRUE)
normalize2 = function(vec) (vec-min(vec))/(max(vec)-min(vec))

#####################################
# Heat map: Explore variable correlation

final.df[,4:(ncol(final.df)-2)] %>%
  na.omit() %>%
  { . } -> corr.df 

qplot(x=Var1, y=Var2, data=melt(cor(corr.df), use='p'), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 90))


##########################################
# CLUSTER#1 | Life expectancy vs Internet Users
# 			  by clusters of GDP per capita

final.df %>%
  select(country, internet_users_percent, life_expectancy, GDP_per_capita_factor) %>%
  na.omit() %>%
  mutate(internet_users_percent = normalize1(internet_users_percent),
         life_expectancy = normalize1(life_expectancy)) %>%
         { . } -> final.cluster1.df

# Create PAM cluster1
pam.result.1 = pam(x=final.cluster1.df[,c('internet_users_percent','life_expectancy')], k=4)
final.cluster1.df$cluster = factor(pam.result.1$cluster)
final.cluster1.df %>% 
  ggplot(aes(x=internet_users_percent, y=life_expectancy)) + 
  geom_point(aes(color=cluster, shape=GDP_per_capita_factor, size=1)) + 
  guides(size=FALSE) %>%
  { . } -> plot.cluster.1
plot.cluster.1



##########################################
# CLUSTER#2 | Life expectancy vs fertility rate
# 			  by GDP_per_capita quartile

final.df %>%
  select(country, life_expectancy, fertility_rate, GDP_per_capita_factor) %>%
  na.omit() %>%
  mutate(fertility_rate = normalize1(fertility_rate),
         life_expectancy = normalize1(life_expectancy)) %>%
         { . } -> final.cluster2.df

# Create PAM cluster2
pam.result.2 = pam(x=final.cluster2.df[,c('fertility_rate','life_expectancy')], k=4)
final.cluster2.df$cluster = factor(pam.result.2$cluster)
final.cluster2.df %>% 
  ggplot(aes(x=fertility_rate, y=life_expectancy)) + 
  geom_point(aes(color=cluster, shape=GDP_per_capita_factor, size=1)) + 
  guides(size=FALSE) %>%
  { . } -> plot.cluster.2
plot.cluster.2




##########################################
# CLUSTER#3 | ICT_service_exports vs ICT_good_exports
# 			  by GDP_per_capita quartile

final.df %>%
  select(country, GDP_per_capita, ict_service_exports, GDP_per_capita_factor) %>%
  na.omit() %>%
  mutate(GDP_per_capita = normalize2(GDP_per_capita),
         ict_service_exports = normalize2(ict_service_exports)) %>%
         { . } -> final.cluster3.df


# Create PAM cluster3
pam.result.3 = pam(x=final.cluster3.df[,c('ict_service_exports','GDP_per_capita')], k=2)
final.cluster3.df$cluster = factor(pam.result.3$cluster)
final.cluster3.df %>% 
  ggplot(aes(x=GDP_per_capita, y=ict_service_exports)) + 
  geom_point(aes(color=cluster, shape=GDP_per_capita_factor, size=1)) + 
  guides(size=FALSE) %>%
  { . } -> plot.cluster.3
plot.cluster.3




# Same with density clusters using DBSCAN

final.cluster3.df %>%
  select(GDP_per_capita, ict_service_exports) %>%
  na.omit() %>%
  mutate(GDP_per_capita = normalize2(GDP_per_capita),
         ict_service_exports = normalize2(ict_service_exports)) %>%
         { . } -> final.cluster4.df
library('dbscan')
res <- dbscan(final.cluster4.df, eps = .1, minPts = 5)
res

plot(final.cluster4.df, col=res$cluster+1L)

########################################
############# CLUSTERING 2.0 ###########
########################################

# Data preparation

# Create `cluster.maker.df`: a dataframe with numerical vars only
final.df %>%
  select_(.dots = indicators.names) %>%
  na.omit()  %>%
  { . } -> cluster.maker.df

# Create two vectors of indicator names : `tech.indicators.names` and `wellbeing.indicators.names`
tech.indicators.names <-
c("internet_subscriptions_percent",
"internet_users_percent",
"cell_users_percent",
"scientific_publications",
"internet_servers_permillion",
"ict_service_exports",
"ict_good_exports",
"ict_good_exports_percent",
"internet_subscriptions",
"internet_servers")

wellbeing.indicators.names <-
c("improved_sanitation_percent",
"life_expectancy",
"homicide_percent_1000",
"women_parliament_percent",
"women_laborforce_percent",
"enrolment_primary_education",
"enrolment_secondary_education",
"fertility_rate",
"GDP_per_capita",
"unemployement_over_laborforce_percent")

# Normalize values for each variable
cluster.maker.df <- apply(cluster.maker.df, 2, normalize1)

# GENERAL CLUSTERS (All variables included)
# Perform PAM clustering on entire data set
cluster.count = 4
pam.cluster.maker.df = pam(x=cluster.maker.df[,indicators.names], k=cluster.count)
pam.cluster.maker.df$cluster_all = factor(pam.cluster.maker.df$cluster)

# Concatenate Cluster columns to `cluster.maker.df`
cluster.maker.df <- cbind(cluster.maker.df, cluster_all = pam.cluster.maker.df$cluster_all)

# WELLBEING CLUSTERS (Only well being var included)
# Perform PAM clustering on well-being indicators only
cluster.count = 4
pam.cluster.maker.df = pam(x=cluster.maker.df[,wellbeing.indicators.names], k=cluster.count)
pam.cluster.maker.df$cluster_wellbeing = factor(pam.cluster.maker.df$cluster)

# Concatenate Cluster columns to `cluster.maker.df`
cluster.maker.df <- cbind(cluster.maker.df, cluster_wellbeing = pam.cluster.maker.df$cluster_wellbeing)

# TECH CLUSTERS (Only tech var included)
# Perform PAM clustering on tech indicators only
cluster.count = 4
pam.cluster.maker.df = pam(x=cluster.maker.df[,tech.indicators.names], k=cluster.count)
pam.cluster.maker.df$cluster_tech = factor(pam.cluster.maker.df$cluster)

# Concatenate Cluster columns to `cluster.maker.df`
cluster.maker.df <- cbind(cluster.maker.df, cluster_tech = pam.cluster.maker.df$cluster_tech)

# Convert to df
cluster.maker.df <- as.data.frame(cluster.maker.df)


# Swap cluster#2 with cluster#1 for clarity
cluster.maker.df %>%
mutate(cluster_all = ifelse(cluster_all == 2, 0, cluster_all)) %>%
mutate(cluster_all = ifelse(cluster_all == 1, 2, cluster_all)) %>%
mutate(cluster_all = ifelse(cluster_all == 0, 1, cluster_all)) %>%

mutate(cluster_tech = ifelse(cluster_tech == 1, 0, cluster_tech)) %>%
mutate(cluster_tech = ifelse(cluster_tech == 3, 1, cluster_tech)) %>%
mutate(cluster_tech = ifelse(cluster_tech == 0, 3, cluster_tech)) %>%
  
mutate(cluster_wellbeing = ifelse(cluster_wellbeing == 2, 0, cluster_wellbeing)) %>%
mutate(cluster_wellbeing = ifelse(cluster_wellbeing == 1, 2, cluster_wellbeing)) %>%
mutate(cluster_wellbeing = ifelse(cluster_wellbeing == 0, 1, cluster_wellbeing)) %>%
  
{ . } -> cluster.maker.df

cluster.maker.df

# Make factor variables
# Set the quantile count for each new categorical var
gdp.quantilize.count = 4
internet_users.quantilize.count = 4
fertility_rate.quantilize.count = 4

# Create factor var
cluster.maker.df %>%
  mutate(
    GDP_per_capita_factor = quantilize(GDP_per_capita, gdp.quantilize.count),
    internet_users_percent_factor = quantilize(internet_users_percent, internet_users.quantilize.count),
    fertility_rate_factor = quantilize(fertility_rate, fertility_rate.quantilize.count)
  ) %>%
  { . } -> cluster.maker.df



# **********************
# CLUSTER HEATMAPS

# I - GDP HEATMAPS

# I.A - ALL VARIABLES CLUSTERS

# Get cluster country frequency by GDP_per_capita quartile
table (cluster.maker.df$GDP_per_capita_factor, cluster.maker.df$cluster_all) %>%
  data.frame () %>%
  rename(GDP_per_capita_factor = Var1, cluster_all = Var2) %>%
  { . } -> gdp.cluster_all.df

# Create heat map
heatmap_cluster_all_gdp <-
  ggplot(gdp.cluster_all.df, aes(x = cluster_all, y = GDP_per_capita_factor)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(name = '#countries', low = "white", high = "steelblue") +
  theme(plot.title = element_text(lineheight=0.8, face="bold"), axis.title.y = element_blank()) +
  labs(x = 'all variables cluster', y = 'GDP per capita quartile', title = 'All variables clusters\nvs\nGDP per capita')

heatmap_cluster_all_gdp


# I.B - WELL BEING VARIABLES

# Get cluster country frequency by GDP_per_capita quartile
table (cluster.maker.df$GDP_per_capita_factor, cluster.maker.df$cluster_wellbeing) %>%
  data.frame () %>%
  rename(GDP_per_capita_factor = Var1, cluster_wellbeing = Var2) %>%
  { . } -> gdp.cluster_wellbeing.df

# Create heat map
heatmap_cluster_wellbeing_gdp <-
  ggplot(gdp.cluster_wellbeing.df, aes(x = cluster_wellbeing, y = GDP_per_capita_factor)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(name = '#countries', low = "white", high = "steelblue") +
  theme(plot.title = element_text(lineheight=.8, face="bold"), axis.title.y = element_blank()) +
  labs(x = 'wellbeing variables cluster', y = 'GDP per capita quartile', title = 'Well-being variables clusters\nvs\nGDP per capita')

heatmap_cluster_wellbeing_gdp


# I.C - TECH VARIABLES

# Get cluster country frequency by GDP_per_capita quartile
table (cluster.maker.df$GDP_per_capita_factor, cluster.maker.df$cluster_tech) %>%
  data.frame () %>%
  rename(GDP_per_capita_factor = Var1, cluster_tech = Var2) %>%
  { . } -> gdp.cluster_tech.df

# Create heat map
heatmap_cluster_tech_gdp <-
  ggplot(gdp.cluster_tech.df, aes(x = cluster_tech, y = GDP_per_capita_factor)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(name = '#countries', low = "white", high = "steelblue") +
  theme(plot.title = element_text(lineheight=.8, face="bold"), axis.title.y = element_blank()) +
  labs(x = 'tech variables cluster', y = 'GDP per capita quartile', title = 'Tech variables clusters\nvs\nGDP per capita')

heatmap_cluster_tech_gdp


# II - Internet_Users_Percent heatmap

# II.A - ALL VARIABLES

# Get cluster country frequency by GDP_per_capita quartile
table (cluster.maker.df$internet_users_percent_factor, cluster.maker.df$cluster_all) %>%
  data.frame () %>%
  rename(internet_users_percent_factor = Var1, cluster_all = Var2) %>%
  { . } -> internet_users.cluster.df

# Create heat map
heatmap_cluster_all_internet_users <-
  ggplot(internet_users.cluster.df, aes(x = cluster_all, y = internet_users_percent_factor)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(name = '#countries', low = "white", high = "steelblue") +
  theme(plot.title = element_text(lineheight=.8, face="bold"), axis.title.y = element_blank()) +
  labs(x = 'cluster', y = 'Internet users (%) quartiles', title = 'All variables clusters\nvs\ninternet users (%)')

heatmap_cluster_all_internet_users


# II.B - WELL BEING VARIABLES

# Get cluster country frequency by GDP_per_capita quartile
table (cluster.maker.df$internet_users_percent_factor, cluster.maker.df$cluster_wellbeing) %>%
  data.frame () %>%
  rename(internet_users_percent_factor = Var1, cluster_wellbeing = Var2) %>%
  { . } -> internet_users.cluster_wellbeing.df

# Create heat map
heatmap_cluster_wellbeing_internet_users <-
  ggplot(internet_users.cluster_wellbeing.df, aes(x = cluster_wellbeing, y = internet_users_percent_factor)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(name = '#countries', low = "white", high = "steelblue") +
  theme(plot.title = element_text(lineheight=.8, face="bold"), axis.title.y = element_blank()) +
  labs(x = 'well being variables cluster', y = 'Internet users (%) quartiles', title = 'Well-being variables clusters\nvs\ninternet users (%)')

heatmap_cluster_wellbeing_internet_users


# II.C TECH VARIABLES

# Get cluster country frequency by GDP_per_capita quartile
table (cluster.maker.df$internet_users_percent_factor, cluster.maker.df$cluster_tech) %>%
  data.frame () %>%
  rename(internet_users_percent_factor = Var1, cluster_tech = Var2) %>%
  { . } -> internet_users.cluster_tech.df

# Create heat map
heatmap_cluster_tech_internet_users <-
  ggplot(internet_users.cluster_tech.df, aes(x = cluster_tech, y = internet_users_percent_factor)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(name = '#countries', low = "white", high = "steelblue") +
  theme(plot.title = element_text(lineheight=.8, face="bold"), axis.title.y = element_blank()) +
  labs(x = 'tech variables cluster', y = 'Internet users (%) quartiles', title = 'Tech variables clusters\nvs\ninternet users (%)')

heatmap_cluster_tech_internet_users




# III - Fertility_rate Heatmap

# III.A - ALL VARIABLES

# Get cluster country frequency by GDP_per_capita quartile
table (cluster.maker.df$fertility_rate_factor, cluster.maker.df$cluster_all) %>%
  data.frame () %>%
  rename(fertility_rate_factor = Var1, cluster_all = Var2) %>%
  { . } -> fertility_rate.cluster_all.df

# Create heat map
heatmap_cluster_all_fertility_rate <-
  ggplot(fertility_rate.cluster_all.df, aes(x = cluster_all, y = fertility_rate_factor)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(name = '#countries', low = "white", high = "steelblue") +
  theme(plot.title = element_text(lineheight=0.8, face="bold"), axis.title.y = element_blank()) +
  labs(x = 'All variables cluster', y = 'fertility rate quartiles', title = 'All variables clusters\nvs\nfertility rate')

heatmap_cluster_all_fertility_rate


# III.B - WELL BEING VARIABLES

# Get cluster country frequency by GDP_per_capita quartile
table (cluster.maker.df$fertility_rate_factor, cluster.maker.df$cluster_wellbeing) %>%
  data.frame () %>%
  rename(fertility_rate_factor = Var1, cluster_wellbeing = Var2) %>%
  { . } -> fertility_rate.cluster_wellbeing.df

# Create heat map
heatmap_cluster_wellbeing_fertility_rate <-
  ggplot(fertility_rate.cluster_wellbeing.df, aes(x = cluster_wellbeing, y = fertility_rate_factor)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(name = '#countries', low = "white", high = "steelblue") +
  theme(plot.title = element_text(lineheight=0.8, face="bold"), axis.title.y = element_blank()) +
  labs(x = 'Well being variables cluster', y = 'fertility rate quartiles', title = 'Well-being variables clusters\nvs\nfertility rate')

heatmap_cluster_wellbeing_fertility_rate


# III.C - TECH VARIABLES

# Get cluster country frequency by GDP_per_capita quartile
table (cluster.maker.df$fertility_rate_factor, cluster.maker.df$cluster_tech) %>%
  data.frame () %>%
  rename(fertility_rate_factor = Var1, cluster_tech = Var2) %>%
  { . } -> fertility_rate.cluster_tech.df

# Create heat map
heatmap_cluster_tech_fertility_rate <-
  ggplot(fertility_rate.cluster_tech.df, aes(x = cluster_tech, y = fertility_rate_factor)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(name = '#countries', low = "white", high = "steelblue") +
  theme(plot.title = element_text(lineheight=0.8, face="bold"), axis.title.y = element_blank()) +
  labs(x = 'tech variables cluster', y = 'fertility rate quartiles', title = 'Tech variables clusters\nvs\nfertility rate')

heatmap_cluster_tech_fertility_rate




# QA
grid.arrange(heatmap_cluster_all_gdp, heatmap_cluster_wellbeing_gdp, heatmap_cluster_tech_gdp, ncol = 2)
grid.arrange(heatmap_cluster_all_internet_users, heatmap_cluster_wellbeing_internet_users, heatmap_cluster_tech_internet_users, ncol =2)
grid.arrange(heatmap_cluster_all_fertility_rate, heatmap_cluster_wellbeing_fertility_rate, heatmap_cluster_tech_fertility_rate, ncol = 2)
