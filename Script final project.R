################################################################################
################ Final Project - Digital Methods for Historians ################
################             A Democratic Crisis?               ################
################   Trends in Democratization from 1979 to 2019  ################
################################################################################

## Author: Christian Alexander Abildgaard Nielsen (Student-ID: 201706639)

## Date: 8th of January 2021

## Using the script: The "Source" function will create all relevant data, data frames and vectors, but the commands constructing the plots have to be run individually (these are found on line 118, 150, and 175).
## Make sure you have installed the necessary packages if you use the "Source" function (packages are on line 19-25).



################################################################################
###################### Installing and activating packages ######################

## Install the following packages, if you do not already have them.
#install.packages(downloader)
#install.packages(readxl)
#install.packages(tidyr)
#install.packages(reshape2)
#install.packages(dplyr)
#install.packages(ggplot2)

## Activate the packages.
library(downloader) ## Used For downloading from https sites.
library(readxl) ## Used For reading the downloaded files.
library(tidyr) ## Used for data cleaning.
library(reshape2) ## Used for data cleaning.
library(dplyr) ## Used for data cleaning (mutate function).
library(ggplot2) ## Used to construct visualizations.



################################################################################
######################## Downloading Freedom House data ########################

## Downloading the Freedom House data directly from their website to my working directory with the name "FH_raw".
download("https://freedomhouse.org/sites/default/files/2020-02/2020_Country_and_Territory_Ratings_and_Statuses_FIW1973-2020.xlsx", dest="FH_raw", mode = "wb")



################################################################################
######## Reading and cleaning the "Historical distribution" excel sheet ########

## Reading the "Historical distribution" excel sheet into a data frame named fh_his_raw.
## I specify the file, the sheet and the specific cells I'm taking data from. I thereby avoid notes and other information in the file.
fh_his_raw <- read_excel("FH_raw", sheet = "Historical distribution") %>%
  .[c(1:48), c(1,1:9)] %>%
  data.frame()
## Optionally check if it was done correctly and see the structure of the data.
#View(fh_his_raw)

## Cleaning the Year under review variable.
## Creating a vector with the contents of the third column of the data frame, which is the year under review.
year_rev_c <- fh_his_raw[[3]]
## Changing values that are intervals to specific years.
year_rev_c[c(14:17, 31:37, 38)] <- c(2006:2003, 1989:1983, 1981)
## Making it a numeric vector.
year_rev <- as.numeric(year_rev_c)



################################################################################
####### Reading and cleaning the "Country Ratings, Statuses" excel sheet #######

## Reading the "Country Ratings, Statuses" excel sheet into a data frame named fh_country_raw.
## Once again, I specify file, sheet, and specific cells. This time I skip 2 columns because there are 3 columns in the sheet, and make "-" missing values.
fh_country_raw <- read_excel("FH_raw", sheet = "Country Ratings, Statuses ", na = "-", skip = 2) %>%
  .[c(1:205), c(1,2:142)] %>%
  data.frame()
## Optionally check if it was done correctly and see the structure of the data.
#View(fh_country_raw)
## Naming the first column "country".
names(fh_country_raw)[1] <- "country"

## The data is however a  mess with multiple problems, especially the multiple column names. I clean it with the following code.
fh_country_clean <- melt(fh_country_raw, id.vars = c("country")) %>% 
  mutate(year_rev_country = rep(c(1972:1981, 1983:2019), each = 3*205), year_rep_country = year_rev_country + 1) %>% 
  rename(metric = variable, result = value) %>% 
  mutate(metric = tolower(gsub("\\..*", "", metric))) %>% 
  spread(metric, result) %>% 
  mutate_each(funs(as.numeric), cl, pr)
## I create a new data frame in a long data format using the melt function.
## I use the mutate function to create a year under review variable and a variable for the year the report came out. The specific years under review are specified in the code, and each covers 3 columns (political rights, civil liberties and status) and 205 rows (number of countries). The year it was the reported is always the same +1.
## I rename the metrics and remove meaningless characters.
## I then use the spread function to get the variables in the correct long format, with a value on cl, pr and status for all countries every year.
## I lastly make the cl and pr variables numeric.
## Optionally check if it was done correctly and see the structure of the data.
#View(fh_country_clean)



################################################################################
#### Visualizing the distribution of countries by Freedom status 1972-2019 #####

## To simplify the process, I make a new data frame from the fh_his_raw and the cleaned year variable. I only add the elements needed in the visualization.
## I make vectors from the variables that contain the percentage of free, partly free and not free countries. I multiply by 100 to get the percentage in full numbers instead of decimals in visualizations.
pct_free <- fh_his_raw[[6]]*100
pct_partly_free <-fh_his_raw[[8]]*100
pct_not_free <- fh_his_raw[[10]]*100
## I put the three vectors together with the cleaned year variable in a data frame.
fh_his_vis <- data.frame(year_rev, pct_free, pct_partly_free, pct_not_free)
## This data frame can be used for mapping one of the three country-types (free, partly free and over not free) across time, but not all three in the same model.
## If you want this subset of the data saved, use the following command.
# write.csv(fh_his_vis, "FH_his_vis.csv", row.names = F)

## I create a variable that can be used for mapping all three country-types simultaneously.
fh_his_plot <- fh_his_vis %>% 
  select(year_rev, pct_free, pct_partly_free, pct_not_free) %>% 
  gather(key = "freedom_status", value = "percentage", - year_rev)
## I use the data from fh_his_vis and create a data frame with only 3 variables, one containing the year under review, one containing the freedom status and one containing the percentage of countries within each status.
## Explained simply, it encapsulates the percentage of countries in each freedom status for each year.

## With this data frame i can now construct the desired plot.
fh_his_plot %>% 
  ggplot(aes(x = year_rev, y = percentage)) +
  geom_line(aes(color = freedom_status, linetype = "solid")) +
  geom_point(colour="gray25", size = 1.2) +
  scale_color_manual(values = c("deepskyblue", "firebrick2", "darkorchid2"), name="Freedom status", labels =c("Free", "Not free", "Partly free")) +
  scale_x_continuous(breaks = round(seq(min(1970), max(2020), by = 5),1)) +
  scale_y_continuous(breaks = round(seq(min(20), max(50), by = 5),1)) +
  geom_vline(xintercept = 1989) +
  labs(title = "Percentage of free, partly free and not free countries 1972-2019", x = "Year", y = "Percentage of the world's countries") +
  guides(linetype = FALSE) +
  theme_bw()
## I plot year on the x-axis and percentage of the worlds countries on the y-axis.
## I then add solid lines for each freedom status and color them individually.
## I change the scales to make them better for analysis. We now see every 5th year and 5th percentage point, instead of every 10th.
## I add a vertical line to the x-axis value 1989.
## I label titel the model, label different elements, remove irrelevant elements and set a theme.


################################################################################
######## Visualizing levels of freedom for specific countries 1972-2019 ########

## The data used above didn't make analyses about specific countries possible. However, this can be done with the cleaned country data.
## To stay in the spirit of the cold war my examples will focus on Eastern Europe, but the same methods can be used on the other countries in the data.
## This data includes the specific levels of political rights and civil liberties, which is a more precise indicator of democracy.
## The indicator can however be confusing. The lower the score is, the more free a country is. Because of this it can be understood as an indicator of repression of political rights and civil liberties.
## I always look at the two added together to get the general level of democracy in each country.
clpr <- fh_country_clean[[4]] + fh_country_clean[[5]]
## Simplifying the analysis by putting the year under review variable and the country variable in logically named vectors (otherwise i run into problems with the names of variables, when constructing the plot)
country_plot <- fh_country_clean[[1]]
year_rev_plot <- fh_country_clean[[2]]

## If you are only interested in one country the simple filter option can be used.
fh_country_clean %>% 
  filter(country =="Poland") %>% 
  ggplot(aes(x = year_rev_country, y = cl+pr)) + 
  geom_line() +
  geom_point(colour="gray25", size = 1.5) +
  scale_x_continuous(breaks = round(seq(min(1970), max(2020), by =5),1)) +
  scale_y_continuous(breaks = round(seq(min (2), max(14), by =1),1)) +
  geom_vline(xintercept = 1989) +
  labs(title = "Freedom in Poland", x = "Year", y = "Repression of political rights and civil liberties") +
  theme_bw()
## This is just a standard ggplot with the same aesthetic changes as the earlier model.

## It was a more complex to add more countries to the same plot, which is useful for comparisons and if you are interested in regions.
## I did it by creating vectors that contained the sum of the political rights and civil liberties for a specific country.
## I chose to look at some of the former soviet states in Eastern Europe, specifically Ukraine, Poland and the Czech Republic.
## First i had to find what rows each country was covering, which i did with this command and then searching in the search-bar.
#View(fh_country_clean)
## I then added the political rights and civil liberties for each country into their country-specific vector.
clpr_cze <- fh_country_clean[2116:2162, 4]+fh_country_clean[2116:2162, 5]
clpr_pol <- fh_country_clean[6581:6627, 4]+fh_country_clean[6581:6627, 5]
clpr_ukr <- fh_country_clean[8790:8836, 4]+fh_country_clean[8790:8836, 5]
## And put them all into a data frame together with the year variable and clpr variable.
fh_east_plot <- data.frame(year_rev_plot, country_plot, clpr, clpr_cze, clpr_pol, clpr_ukr)

## I can now construct the desired plot.
fh_east_plot %>% 
  ggplot(aes(x = year_rev_plot, y = clpr)) +
  geom_line(aes(y=clpr_cze, color = "dodgerblue3")) + 
  geom_line(aes(y=clpr_pol, color = "firebrick2")) +
  geom_line(aes(y=clpr_ukr, color = "gold")) + 
  scale_color_manual(values = c("dodgerblue3", "firebrick2", "gold"), name = "Country", labels = c("Czech Republic", "Poland", "Ukraine")) +
  scale_x_continuous(breaks = round(seq(min(1970), max(2020), by =5),1)) +
  scale_y_continuous(breaks = round(seq(min (2), max(14), by =1),1)) +
  geom_vline(xintercept = 1989) +
  labs(title = "Freedom in Eastern European countries", x = "Year", y = "Repression of political rights and civil liberties") +
  theme_bw()

## I plot year on the x-axis and freedom score on the y-axis.
## The line for each country is then added with individual commands.
## They are then colored and labeled.
## After this i make the same aesthetic changes as earlier models.

## If the downward trend being a positive democratization is too confusing, solve it like this:
## Write scale_y_reverse instead of scale_y_continuous. No other change is needed.

################################################################################
################################################################################