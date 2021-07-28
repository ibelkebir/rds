# RDS Application

This application is intended to help users analyze RDS datasets. 

To use this application, download or clone this repository and open "app.rmd" in RStudio. The required packages for this application are 
* RDS
* shiny
* DT
* dplyr
* ggplot2
* reshape2
* visNetwork
* RColorBrewer

If these packages are not already installed, go to *Tools > Install Packages* and install the required packages.

If all the required packages are downloaded, click *Run > Run All* in RStudio and the application will open in a seperate window. You can select *Open in browser* from the top menu in order to open the application in your default browser.

On the landing page of the application, you can select which dataset to analyze from preset datasets and your own custom dataset. When uploading your own dataset, make sure to include column headers and ensure that they follow the format described on the landing page.

This application has 3 different pages: *Datasets*, *Summary*, and *Visualizations*. You can click on the tabs at the top of the application to switch between them. A short description of each page is given at the top.
