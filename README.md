# Common Workflow Languange R 

R classes mapped to CWL, enable OOP, with utils like JSON/YAML converter

Reference please check 
[draft2](http://common-workflow-language.github.io/draft-2/#common_workflow_language,_draft_2)

## Install

Please pay attention that the github project name is **NOT** `cwl`, but `cwl.R'

### From github
Directly install from github, need to install Bioc dependency first. 

```{r}
source("http://bioconductor.org/biocLite.R")
biocLite("S4Vectors")

library(devtools)
install_github('tengfei/cwl.R')
```
### From Bioconductor

When it's on Bioconductor, to install released version

```{r}
source("http://bioconductor.org/biocLite.R")
biocLite("cwl")
```

To install developmental version

```{r}
source("http://bioconductor.org/biocLite.R")
useDevel()
biocLite("cwl")
```
