import requests, json, time
import pandas as pd
from io import StringIO
import datetime


# request smard data with default values
def requestSmardData (
    modulIDs = [5000410], 
    days = 7, 
    time_from = datetime.datetime.utcnow(),
    region   = "DE",
    language = "de",
    type     = "discrete"
    ):

    
    epoch_time = datetime.datetime(1970, 1, 1)
    timestamp = (time_from - epoch_time).total_seconds() 

    timestamp_from_in_milliseconds = (timestamp * 1000) - (days * 24 * 3600) * 1000
    timestamp_to_in_milliseconds = timestamp * 1000
    
    # http request content
    url  = "https://www.smard.de/nip-download-manager/nip/download/market-data"
    body = json.dumps({
        "request_form": [
            {
                "format": "CSV",
                "moduleIds": modulIDs,
                "region": region,
                "timestamp_from": timestamp_from_in_milliseconds,
                "timestamp_to": timestamp_to_in_milliseconds,
                "type": type,
                "language": language
            }]})
    
    # http response
    data = requests.post(url, data = body)

    # create pandas dataframe out of response string (csv)
    df = pd.read_csv(StringIO(data.text), sep=';')
    colnames = {"Datum": "date", "Uhrzeit": "time", "Gesamt (Netzlast)[MWh]": "demand"}

    df = df.rename(columns = colnames)

    return df

