#OPTIMAL PROPERTY ANALYSIS

We are analyzing the best location of properties and year to buy, while also finding a relation between property area and year using R and statistical tests.

## Installation

I)  library(ggplot2)
II) library(dplyr)
III)library(tidyr)
IV) library(lubridate)
V)  library(tmaptools)
VI) library(stringr)

```bash
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("stringr")
install.packages("magrittr")
install.packages("ggplot2")
```

## Usage

We first sort the data and clean our data making prices columns into numeric data type and date column into date type, and then forming the statistical analysis based on the sorted data after that we introduce a new variable price per square area i.e
```bash 
PPA = dat$Price / dat$Square.ft
``` 
Then we analyze data concerning the availability of baths and beds in a certain year(2021,2022,2023) and we also find a relation between the price increases w.r.t year, and plotting various regression lines and scatter plots between:
i) Price vs Sqft
ii) Price vs Year
iii) PPA vs 

![Screenshot (3047)](https://github.com/Chuggu12/Property-Analysis/assets/108995664/1f2b7366-91e9-4b7d-9c80-c9d651c9b2ed)
![Screenshot (3039)](https://github.com/Chuggu12/Property-Analysis/assets/108995664/6fd9b4da-76d5-4aae-b1be-2e67159c4427)
![Screenshot (3040)](https://github.com/Chuggu12/Property-Analysis/assets/108995664/7f82c27e-c130-42cb-8c15-4c54a161d310)


and critically analyzing data based on the above graphs, summary, and statistical tests.


## Contributing

Pull requests are welcome. For major changes, please open an issue first
to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License

[MIT](https://choosealicense.com/licenses/mit/)
