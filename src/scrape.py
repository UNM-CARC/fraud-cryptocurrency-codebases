from requests import Request, Session
from requests.exceptions import ConnectionError, Timeout, TooManyRedirects
from collections import defaultdict
from coinmarketcapapi import CoinMarketCapAPI
from pandas.io.json import json_normalize  # package for flattening json in pandas df
import pandas as pd
import json

# url = 'https://pro-api.coinmarketcap.com/v1/cryptocurrency/info'
url = "https://pro-api.coinmarketcap.com/v1/cryptocurrency/listings/latest"

data_final = "/home/ghostbird/Hacking/cybersecurity/fraud-cryptocurrency-codebases/data_final/data_final.csv"
# data_final = "/carc/scratch/projects/bridges2016099/data_final/data_final.csv"
df_data_final = pd.read_csv(data_final)
# df_data_final.set_index("name", inplace=True)
# print(df_data_final)

cmc = CoinMarketCapAPI("f43cf05e-ba8f-42f8-8c8c-d8dbe6c7531b")
data_id_map = cmc.cryptocurrency_listings_latest()
# data_id_map = cmc.cryptocurrency_info(symbol="BTC")
normalized = pd.json_normalize(data_id_map.data)
df = pd.DataFrame(normalized)
df.set_index("slug", inplace=True)
# print(df)
df_merge = pd.merge(df_data_final, df, on="slug")
print(df_merge)
