This is a package for exploring some of the CoV-19 data. The world data is downloaded from JHU. The US data is downloaded from JHU and CovidTracking. The Italy data is downloaded from the Italian CDC (Protezione Civile) 

Sources:
* https://github.com/CSSEGISandData/COVID-19
* https://covidtracking.com/api/ 
* https://github.com/pcm-dpc/COVID-19

To install the package

```
library(devtools)
install_github(eeholmes/CoV19)
```

To get the data:
```
statedata
italydata
worlddata
```
Use `head()` to look at it. Should be pretty self-evident what it is.



