### Create md and html files from Rmd documents 

Firs set working directory 
> `setwd('my_path')`

Next, load the packages 
> `require(knitr)`

> `require(rmarkdown)`

Finally convert documents. 
> `rmarkdown::render('filename.Rmd', 'all')`
