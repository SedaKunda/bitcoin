# bitcoin
A Haskell application which retrieves historical bitcoin data from 
Yahoo finance website and uses a database to store and manipulate the data.
Source - https://uk.finance.yahoo.com/quote/BTC-GBP/history?p=BTCGBP&

1) Navigate to destination folder
2) Run stack build
3) Run stack exec bitcoin-exe
4) Prompt “Which currency are you interested in?” – Respond with one of the
options
5) Prompt “What type of information would you like to see?” – Choose
Database, Graph or Stats
6) After Choosing Database
- Data downloaded and inserted into database
7) After choosing Stats
- Average Price for Bitcoin is shown to user
- Currency details displayed
- Prompt “Select a date to view the closing price for. (YYYY-MM-DD)” –
Choose a date
8) After Choosing Graph
- Prompt “What type of graph would you like to see?” – Choose High, Low,
Open, Close, Adj or Volume
