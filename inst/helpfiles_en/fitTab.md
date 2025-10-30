## Fit distributions


1. Specify **which column contains concentration values**. The app attempts to guess which column contains concentration values based on data column names. This may need to be corrected.
2. **Select or deselect distributions to fit the data**.  
The fitted distributions plot includes the model average estimates. 
Note that if two or more models have overlapping fits then support for this model shape will be over inflated in the model averaged parameters.  
Please see the article [here](https://bcgov.github.io/ssdtools/articles/distributions.html) for more information.  
The outputs may take a moment to update.
3. Format the plot using inputs in the sidebar and **download plot and goodness of fit table** as png and csv files, respectively. 
Select units to display them in the x-axis title. 

Additional information about the **goodness of fit table**:
The columns in the goodness of fit table are the distribution (dist), the Anderson-Darling statistic (ad), the Kolmogorov-Smirnov statistic (ks), the Cramer-von Mises statistic (cvm), Akaike’s Information Criterion (aic), Akaike’s Information Criterion corrected for sample size (aicc), Bayesian Information Criterion (bic), the AICc difference (delta) and the AICc based Akaike weight (weight). 
The prediction is the model averaged (using aicc) estimate of the fit. 
The percent hazard concentration is the concentration of the chemical which is predicted to affect that percent of the species tested.
