#####Problem 1#####
#Write a loop which simulates 1000 times a martingale strategy based on a coin flip
#Martingale is a gambling strategy where you multiply your next bet twice if
#you have lost your previous one. You bet 1 and you win. Because you won you bet 1
# again. You lose. Then you bet 2, you lose again. You bet four and you win.
#Because you won, you go back to betting one etc. You start with 100 USD and you
#base bet is one. 
#If the coin flip is biased and you have 48.60% chance to win, when do you
#go broke on average(out of those 1000 simulations)? Look at the help for sample,
#to figure out how to pick incorporate the 48.6% probability.
#You can use a while loop for simulating when you go broke. A while loop
#loops until a condition is TRUE. Example:
# i <- 1
# while (i < 6) {
#   print(i)
#   i <- i + 1
# } 
#In your case you want to loop until your budget is > 0.
# Budget <- 100
# while (Budget > 0) {
#   Do something
# } 
#Pay attention to the fact that you can't bet more money than you have.
#If you lose 1, 2, 4, 8, 16, 32. Then your remaining money will be 
#100-32-16-8-4-2-1 = 37, so you can bet max 37 USD.
#####Problem 1#####

ResultVector <- NULL
  for(i in 1:1000){
    Budget <- 100
    Bet <- 1
    BetNumber <- 0
    while (Budget > 0) {
      BetNumber <- BetNumber + 1
      if (Bet > Budget){
        Bet <- Budget
      }
      cointoss <- sample (c("win", "lose"), 1, prob = c(0.486, 0.514))
      if(cointoss == "win"){
      Budget <- Budget + Bet
      Bet <- 1
    } else {
      Budget <- Budget - Bet
      Bet <- Bet * 2
    }
  }
  ResultVector <- c(ResultVector, BetNumber)
}



#####Problem 2#####
# Read everything from https://r4ds.had.co.nz/transform.html, up until
# 5.6 Grouped summaries with summarise(). If you want to, you can
# read everything and then https://r4ds.had.co.nz/relational-data.html

#Do all the exercises:

library(nycflights13)
library(tidyverse)
flights <- flights


# 5.2.4 Exercises 

#Question 1#
number_one <- dplyr::filter(flights, arr_delay >= 120)
number_two <- dplyr::filter(flights, dest == "HOU" | dest == "IAH")
number_three <- dplyr::filter(flights, carrier == "UA" | carrier == "AA" | carrier == "DL")
number_four <- dplyr::filter(flights, month == 7 | month == 8 | month == 9)
number_five <- dplyr::filter(flights, arr_delay > 120 & dep_delay <= 0)
number_six <- dplyr::filter(flights, arr_delay >= 60 & (dep_delay - arr_delay) <= 30)
number_seven <- dplyr::filter(flights, hour == 6 | hour == 24)

#Question 2#
example <- dplyr::filter(flights, dplyr::between(dep_time, 0, 600))
##between() can be used in order to simplify inequalities.

#Question 3#
miss_time <- dplyr::count(dplyr::filter(flights, is.na(dep_time)))
print(miss_time)
#8255 flights have a missing dep. time. 
#Other varibales that are missing include dep_delay and arr_time. 
#This means the flight was either canceled or mischeduled. 

#Question 4# 
#If you can't search for something that doesn’t exist, 
#an NA doesn’t exist in the data base rather a blank exists and an NA is returned.


# 5.3.1 Exercises 

#Question 1#
flights %>% 
  dplyr::arrange(desc(is.na(dep_time)),
          desc(is.na(dep_delay)),
          desc(is.na(arr_time)), 
          desc(is.na(arr_delay)),
          desc(is.na(tailnum)),
          desc(is.na(air_time)))

#Question 2#
earliest <- dplyr::arrange(flights, dep_delay)
most_delayed <- dplyr::arrange(flights, desc(dep_delay))

#Question 3#
fastest <- dplyr::arrange(flights, air_time)

#Question 4#
shortest <- dplyr::arrange(flights, distance)
farthest<- dplyr::arrange(flights, desc(distance))


# 5.4.1 Exercises 

#Question 1#
first_select <- dplyr::select(flights, dep_time, dep_delay, arr_time, arr_delay)

#Question 2#
second_select <- dplyr::select(flights, year, month, day, month, day)
#When we use View() it shows each variable only once (not multiple times).

#Question 3#
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
dplyr::select(flights, one_of(vars))
#one_of() function allows you to select from a character vector.

#Question 4#
code_test <- dplyr::select(flights, contains("TIME"))
empty <- dplyr::select(flights, contains("TIME",ignore.case = FALSE))


# 5.5.2 Exercises 

#Question 1#
convert_1 <- transmute(flights,
                dep_time,
                dep_time_minutes = (dep_time %/% 100)*60 + (dep_time %% 100) 
)

convert_2 <- transmute(flights,
                sched_dep_time,
                sched_dep_time_minutes = (sched_dep_time %/% 100)*60 + (sched_dep_time %% 100) 
)
#Question 2#
flights_airtime <-
  dplyr::mutate(flights,
         dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         arr_time = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
         air_time_diff = air_time - arr_time + dep_time
  )

#Question 3#
flights_deptime <-
  dplyr::mutate(flights,
         dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         sched_dep_time_min = (sched_dep_time %/% 100 * 60 +
                                 sched_dep_time %% 100) %% 1440,
         dep_delay_diff = dep_delay - dep_time_min + sched_dep_time_min
  )

#Question 4#
head(arrange(flights,desc(row_number(flights$dep_delay))), 10)
#TIES WILL BE BROKEN WHILE FIRST WILL GET SMALLER RANK

head(arrange(flights,desc(min_rank(flights$dep_delay))), 10)
# TIES WILL ALL GET THE SAME SMALLEST RANK

#Question 5#
1:3 + 1:10
#The code also produces a warning that the shorter vector is not a multiple of the longer vector. 
#This indicates a bug in the code. That is why a warning is provided.

#Question 6#
#All trigonometric functions are all described in a single help page, named Trig.
#All trigonometric functions that are available in R are the sine, cosine, and tangent method and their inverse methods.



#You can also read the official dplyr site.
#https://dplyr.tidyverse.org/index.html
#https://dplyr.tidyverse.org/articles/dplyr.html
#####Problem 2#####

#Copy to the recording https://unisofiafaculty.sharepoint.com/:v:/r/sites/AccountingFinanceandDigitalapplicationsSeminargroupI/Shared%20Documents/General/Recordings/Quantitative%20methods%20in%20finance%202022-20220317_180538-Meeting%20Recording.mp4?csf=1&web=1&e=3m90AT
