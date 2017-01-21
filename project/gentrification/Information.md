# Business Openings as an Indicator of Gentrification
#### Dan Smilowitz

This project was completed for the Knowledge & Visual Analytics (DATA 608) course in the curriculum of the City University of New York School of Professional Studies (CUNY SPS) Masters of Science in Data Analytics program.  Other projects completed for this course can be found on [GitHub](https://jlaurito.github.io/CUNY_IS608/).

### Project Motivation
Living in a neighborhood in the midst of transitioning demographics, the opening of new businesses has seemed to me a barometer of the change in the area.  The purpose of this project is to investigate if the opening of new businesses can be used as an indicator of gentrification in New York City.  Predictors of gentrification can hopefully be used to predict the need for the development of affordable housing to prevent residents of changing neighborhoods from being displaced due to rising real estate prices.

### Data Visualization
The visualization showcased in this Shiny app utilizes the Plotly API to show a motion chart illustrating the number of businesses opened in each zip code and change in real estate prices over time.  It was believed that this chart might show that the opening of businesses is a leading indicator of increasing real estate prices to come, but the two variables seem to be more in sync with one another than expected.  There does, however, seem to be a correlation between businesses opening and real estate prices rising -- generally, the markers on the graph tend to move in the same direction on both axes (i.e. either up and to the right or down and to the left).  This can be seen most strongly following the recession of 2008-2009, especially in zip codes in Brooklyn.

The 'Data Exploration' tab allows for further investigation of the links between business openings, real estate prices, and a number of population, demographic, and economic indicators.

### Data Sources
The primary data sources utilized in this analysis are real estate prices by zip code from Zillow and a list of all corporations in New York State beginning in 1800.  This data is supplemented with zip code level data from the US Census Bureau and the Internal Revenue Service.  Full data sources are available in the 'References' tab.

Downloaded data and code outlining detailed data preparation in R can be found on [GitHub](https://github.com/dsmilo/DATA608/blob/master/project/dataprep.R).
