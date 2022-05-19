# Replication of Martin, Wagner (2019): "What is the expected return on a stock?"
We used the NASDAQ100 (NDX) index instead of the S&amp;P500 and S&amp;P100 utilized in the original paper. 
The files SVIX_replication_functions.r and SVIX_replication_master.r are taken from the Supplementary Files section available at https://doi.org/10.1111/jofi.12778.

## Data folder
The data has been downloaded from OptionMetrics and the NASDAQ website. All the data spans through the period January 1, 2014 - December 31, 2021.
The files price_ndx_idx.csv price_ndx_comp.csv contain the daily prices for NDX and for its components, respectively. 
The files volsur_ndx_idx.csv and volsur_ndx_comp contain daily volatility surface data (implied volatility, implied strike price, and implied premium) for NDX and for its components, respectively.

## Data cleaning 
The data from above has been cleaned in the data_cleaning.R file using the procedure described in Appendix A of Martin, Wagner (2019) and Mariana, Coelho (2020).
