# pcaseTemplate
A template for NRES 450 problem case models with Shiny. 

Import this template to a new repository. Problem case repositories should be named M?-P?, 
where the '?' are replaced with module number and problem case id respectively. 

use 

```
  git remote add master template https://github.com/atyre2/pcaseTemplate.git
```

to add the template repository as a remote. This will allow changes to e.g. footers etc to be merged in to each 
problem case as necessary using 

```
  git fetch template
  git merge template
  
```

I think. 

The files here should run as a shiny app without modification. It won't do anything, but it will run. 
To run it directly from github use

```r
shiny::runGitHub("pcaseTemplate", "atyre2")
```
