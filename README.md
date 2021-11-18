# AutomatedEconomicAnalysis

- This is the Github repository with the code creating posts for https://twitter.com/EconomicsShiny. 

- The rationale for why I'm posting tweets with the data I'm posting is here: https://whyitmatters.netlify.app.

Note: To run the code, you will need to have access to FRED. In the code you will see a reference to the file "Credentials.R". This file contains information needed to download data from FRED (fred.stlouisfed.org), to post to Twitter, and to send emails via GMail. The most important one is access to FRED, since that's where all the data comes from. The connections to Twitter and GMail can easily be commented out. Check the information in the packages fredr, rtweet, and gmailr on how to set this up.
