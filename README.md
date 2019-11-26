# git-vis
My program to interrogate and visualize GitHub data for CS3021 software engineering. The program draws a social graph of a given user. The user is show as a circle whose size and colour depends on the total size of their repos and their most used language respectively. All the users followers are then also displayed the same way. Lines connecting these circles mean that these circles follow each other. Implemented in Haskell for the challenge.

## Overview
![My small little social graph](https://i.imgur.com/ArmDWYV.png)

The program takes in a given username from the console. The program then gives you the option to ignore all repos that a user forked and all repos that a user does not own. In the background, an authentication token is loaded from a .env file. This ensures the program will be able to successfully make all its requests.

![Example of user input at the console](https://i.imgur.com/xyCbUCg.png)

The program then displays the social graph. The main user is displayed in the center and their followers are in a circle around them. No matter how many followers a user has they will always fit cleanly around him in a circle. This is achieved by making the distance a follower is from the main user proportional to the total number of followers.

![A much bigger social graph](https://i.imgur.com/FDzdh5o.png)

The colours of the circles represent the most used language of that user. The most used language is found by grouping each repo according to their language use, summing each groups size in kb and returning the language that had the greatest size. These colours are the same as the colours GitHub assigns each language on their website. This was done by reading a JSON file with these colour-language pairs.

![example of the different colours that GitHub gives languages](https://i.imgur.com/hRiO0uh.png)

The lines between circles represent if the users follow each other. A line is drawn if either of the users follow each other.

## Running the project

This program is managed using Stack. It is necessary to run stack build in order to fetch all the dependencies.

This program also uses an Auth token to allow more requests with the GitHub api. You'll need to add the following line to a .env file in your projects folder
  TOKEN = <Your GitHub token goes here>

## Haskell GitHub api

I used [Phadejs Haskell library for querying the GitHub api](https://github.com/phadej/github). While I was using the api I wrote two functions that I was surprised weren't included in the api to begin with. So when I finished this project I forked the GitHub Haskell api, added my new functions and [made a pull request](https://github.com/phadej/github/pull/413). The changes are currently under review.

![Image of my pull request](https://i.imgur.com/ju7nWEf.png)

## Challenges and room for improvement

* I'd never used Haskell in a project of this scale before so I was very much learning as I go along. In particular, I learned a lot about error handling and dealing with IO in Haskell. If I was starting this project from scratch again I would definitely try include more error handling.

* Early on I set myself the challenge of doing this project 100% in Haskell. That meant also using a Haskell graphics library. I choose to use [Gloss](https://hackage.haskell.org/package/gloss). I thought this library was quite ugly, especially the text and the way that circles turn into polygons when you zoom out. Next time I would use Haskell just for data visualization and use a JS frontend to make nicer charts.

* The sizing algorithm of the circles could use some fine tuning as I am not happy with the current algorithm. The size of the users circle is proportional to the log of the total size of all the users repos. I choose this because some users had forked projects that were absolutely enormous and hence they were taking up most of the space on the screen. While the log system stopped this, the circle sizes are now misleading and hard to interpret.

* I originally wanted to draw an arrow from one circle to all their followers. However, the limitations of Gloss prevented me from doing this. Hence it isn't clear which circles follow which or if they both follow each other.
