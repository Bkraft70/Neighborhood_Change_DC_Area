# Neighborhood_Change_DC_Area
Neighborhood change in the Washington DC Metropolitan Region with a focus on Montgomery County


This project is based on the methodology from American Neighborhood Change in the 21st Century: Gentrification and Decline (https://law.umn.edu/institute-metropolitan-opportunity/gentrification) created by the University of Minnesota Institute on Metropolitan Opportunity. 

Like the University of Minnesota's analysis, this project tracks the net and percent changes in people living above and below 200 percent of the federal poverty level in Census tracts over time.

Neighborhood Change in the Washington DC area limits its analysis to the Washington, DC metropolitan area (the Univ. of Minnesota's is national). It uses the 2000 Decennial Census as the starting point and the 2015-2019 5-year American Community Survey as the ending point. It uses Brown University's Longitudinal Tract Database (https://s4.ad.brown.edu/Projects/Diversity/Researcher/Bridging.htm) to reconcile Census tract geography changes that occured in 2010. 

Neighborhood Change in the Washington DC area also includes race and ethnicity and housing variables.

The code provided here extracts Census and ACS data, allocates it into 2010 tract geographies, and computes variables that can be analyzed in conjunction with the core neighborhood change variables, which are the numbers and percentages of people in each tract above and below 200% of the Federal poverty level. The end result is an R dataframe which can be written to a .txt, .csv, or .xlsx file. The file also includes script for writing the dataframe to a shapefile that can be used in geographic information systems. The script can be adapted to other regions or the U.S.
