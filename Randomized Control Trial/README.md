# Community Currencies as Crisis Response: Results from a Randomized Control Trial in Kenya

This document outlines the Python code for implementing a Randomized Control Trial (RCT) aimed at understanding the impact of Community Currencies (CCs) as an effective form of Unconditional Cash Transfer (UCT). 


* **Data source:** https://www.grassrootseconomics.org/research (with additional timestamps acquired at personal request)
* **Data format:** .csv
* **Data files**: 
 * userData: 40K+ rows containing anonymized meta-data for every registred user on the Sarafu network e.g. gender, wallet address, wallet balance, etc.
 * txnData: 500K+ rows containing every transaction between every user on the Sarafu Network. Datapoints include the timestamp of the transaction, wallet address of each sender and recipient, trade amount, trade category (e.g. Food/Water/Education), etc.

**Data cleaning brief**

The goal is to take txnData and userData as inputs, and generate userData snapshots for specific timestamps, which can be used to compare pre- and post-treatment conditions. 

Steps:
1. Load .csv files as pandas DataFrames
2. Clean DataFrames to handle missing values, location errors and categorical inconsistencies.
3. Convert DataFrames into dictionary format (this significantly speeds up processing times for 500K+ rows of txnData and 40K+ rows of userData)  
4. Loop through every user in the dictionary and use the txnData to compute time-specific user attributes like total trade volume, number of trade partners, gender ratio of trade partners, etc.
5. Once dictionaries are updated, converted them back into DataFrames and save in .csv format.

**Study sample selection**

This study has two location arms: urban (Nairobi) and rural (Kwale). In each location, users are filtered according to the following criteria:
* Has been an active user for at least 30 days
* Makes a purchase at least 1x per week on average
* Makes a sale at least 1x per week on average

To select the appropriate treatment size in each community, a power test is performed for a 95% confidence interval. The number produced by this test is then fed into a random selector for each community which splits eligible wallets into treatment and control.
