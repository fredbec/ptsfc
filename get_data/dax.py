import requests, json, time
import pandas as pd
from io import StringIO
import datetime
from math import floor


# request smard data with default values
def requestDAXdata (
    modulIDs = [5000410], 
    days = 7, 
    time_from = datetime.datetime.utcnow(),
    region   = "DE",
    language = "de",
    type     = "discrete"
    ):

    
    epoch_time = datetime.datetime(1970, 1, 1)
    timestamp_to = floor((time_from - epoch_time).total_seconds())
    timestamp_from =  floor(timestamp_to - (days * 24 * 3600)) 

    
    urlp1 = "https://query1.finance.yahoo.com/v7/finance/download/%5EGDAXI?period1=" 
    urlp2 = '&period2='
    urlp3 = '&interval=1d&events=history&includeAdjustedClose=true'

    # http request content
    url = urlp1 +str(timestamp_from) + urlp2 + str(timestamp_to) + urlp3
    # http response
    data = requests.get(url,  headers={'User-agent': 'Mozilla/5.0'})
    open('dax.csv', 'wb').write(data.content)

    # create pandas dataframe out of response string (csv)
    df = pd.read_csv(StringIO(data.text), sep=',')
    colnames = {"Date": "date", "Open": "open", "High": "high", "Low": "low", "Close": "close", "Adj Close": "adjclose", "Volume": "vol"}

    df = df.rename(columns = colnames)
    return df
