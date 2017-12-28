# Legendary NBA Players
## R Shiny
Using this R Shiny is not technologically taxing but modifying the R Shiny requires R programming skills as mentioned previously. It is 
important to note that internet is required to run the application. The link that is provided in the appendix directly takes you to the 
application. Only three options will be shown Sample Size, Confidence Level and Statistic (i.e. game variable of interest) once you arrive at 
the website. The offensive and defensive statistics for each player can be selected using the drop-down menu. When the calculate button is 
clicked, the confidence intervals graphics will appear. If a change occurs in any of the options, the calculate button must be clicked again 
for the confidence interval graphics to reflect the changes made.

## NBA Data
The application uses data from four NBA legendary basketball players (Basketball Reference 2016). This R Shiny was designed to compare four basketball legends: Stephen Curry (SC), Kobe Bryant (KB), LeBron James (LJ) and Michael Jordan (MJ) in terms of confidence intervals with respect to the player’s performance. To compare confidence intervals a point of reference was needed since these players played at different
times. Therefore, the data from each player’s first year in the NBA are used. The game data used to compare these legends were points, assists,
defensive rebounds and steals. These were chosen because they are the typical variables used to compare players in which two are offensive statistics (points and assists) and two are defensive (rebounds and steals). It is important to note that while in this example the NBA and certain players were chosen, however, this application could be built for any sport or player. The link for the R Shiny is found in the appendix as well as an annotated version of the code and the format of the data required for the R Shiny. Modifying the R code requires a familiarity with programming in R.
