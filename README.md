ajpeluR 
---------------

Author: [@ajpelu](https://twitter.com/ajpelu) 

mail: <ajperez@ugr.es> 

Here I put a collection of different R functions that I use. In the future I will put them into a R package. 

License: Creative Commons Attribution 4.0 International License
![http://creativecommons.org/licenses/by/4.0/](https://i.creativecommons.org/l/by/4.0/88x31.png)

##### Use
* Option 1 
> library(devtools)
> source_url('url')

Usage option 1: 
> # with url as the raw url of the function. Example
> source_url('https://raw.githubusercontent.com/ajpelu/ajpeluR/master/R/exportggplot.R')

* Option 2 
Use a function to read script from github.
[Source](http://stackoverflow.com/questions/7715723/sourcing-r-script-over-https)

Load this function
> github.download = function(url) {
  fname <- tempfile()
  system(sprintf("curl -3 %s > %s", url, fname))                                                                 (fname)
}

Usage option 2: 
> source(github.download('https://raw.githubusercontent.com/ajpelu/ajpeluR/master/R/exportggplot.R'))