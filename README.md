#### CONTENT
* `WDI_report.pdf` actual report
* `WDI_report.Rmd` markdown file to generate the report
* `main_20160305.R` R file containing all the code for data manipulation, analysis and graphs





# Investigating the relationship between technology and well-being

* Introduction
* Variable summary
* Association rules
* Clustering analysis
* Conclusion

## Introduction

This research studies the relationship between technology and well-being in countries. The data source used is the World Development Indicators data set created by the World Bank.

## World Bank’s World Development Indicators

The World Bank is a cooperative organization made up of 188 members countries and is governed by the
minister of Finance or Development of member countries. The mission of the World Bank is to alleviate
poverty and support development. The World Bank works of those fronts by providing both financial and
technical assistance to countries.

One of the output of the World Bank efforts is the World Development Indicators (WDI) data set. The WDI
presents the most current and accurate global development data available. The original data sources are the
statistical systems of member countries and important international organizations. Because the quality of
statistical systems varies from country to country and from organization to organization, the World Bank
partners with the different national and international agencies to enforce a professional standard in the
methodologies, definitions and classifications when sourcing the data.

There is a total to date of over 1300 indicators, open for non-commercial use. For each indicator, data is
available by country/region with 248 countries/regions represented and by year from 1960 to present. The
list of topics covered by the indicators include Agriculture & Rural Development, Aid Effectiveness, Climate
Change, Economy & Growth, Education, Energy & Mining, External Debt, Financial Sector, Gender, Health,
Science & Technology, Infrastructure, Labor & Social Protection, Poverty, Private & Public Sector, Social
Development and Urban Development.


## Missing data policy

Some indicators have been created more recently than others. Overall, recent data tend to be more complete
than older data. Furthermore, the data is not equally available from one country to another. For each
indicator there can be missing data for multiple countries and/or for multiple years. Too much missing data can make an indicator irrelevant. I will assume that the critical amount of data for an indicator to be relevant
is that it covers 80% of the total 248 countries/regions with at least one data point from 2010 to present.
Any indicator that falls below this standard will not be used.

## Indicators selection

I chose a subset of indicators data that would represent country’s level technological development as well as
their level of well-being. Well-being is a complicated concept that can be defined in various ways but for
the purpose of this research, I have defined it as a function of Safety, Gender equality, Education and
Fertility. These topics represent different layers of needs inspired by the ‘Hierarchy of needs’ as introduced
by A. Maslow in his Theory of Human Motivation.

## Research framework

The goal of this research is to uncover existing relationships between technological development and well-being
in countries.

* My first objective is to define a metric to measure technological development at the country level. In
order to study technological development in countries, I will analyze the countries distribution for the
different technological indicators. This will enable me to study the relationship between economic
performance and technological development in countries.
* The second objective is to understand the relationships between technological development and the
different indicators of well-being (safety, equality, education, fertility, economy). For this purpose, I will
use association rule learning to identify potential strong relationships existing between the variables
selected.
* Ultimately, the reserach aims at defining the cases, if any, for which technological development has
a relationship with the level of well-being in countries. I will perform cluster analysis on countries
using different sets of variables to discover which are the variables that best define similarity between countries in terms of technological development and in terms of well-being. This will enable me to
study how naturally the groupings of countries by category of technological development overlap with
the groupings of countries by category of level of well-being and reach a conclusion on the existence of
a relationship between technology and well-being.


Please read rest of the report in `WDI_report.pdf`
