from requests import Request, Session
from requests.exceptions import ConnectionError, Timeout, TooManyRedirects
from collections import defaultdict
from coinmarketcapapi import CoinMarketCapAPI
from pandas.io.json import json_normalize  # package for flattening json in pandas df
import pandas as pd
import json

# url = 'https://pro-api.coinmarketcap.com/v1/cryptocurrency/info'
url = "https://pro-api.coinmarketcap.com/v1/cryptocurrency/listings/latest"


cmc = CoinMarketCapAPI("f43cf05e-ba8f-42f8-8c8c-d8dbe6c7531b")
data_id_map = cmc.cryptocurrency_listings_latest()
normalized = pd.json_normalize(data_id_map.data)
df = pd.DataFrame(normalized)
df.set_index("name", inplace=True)
print(df.head())
