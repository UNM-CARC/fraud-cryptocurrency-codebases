# This example uses Python 2.7 and the python-request library.

from requests import Request, Session
from requests.exceptions import ConnectionError, Timeout, TooManyRedirects
from collections import defaultdict
import pandas as pd
import json

from coinmarketcapapi import CoinMarketCapAPI

# vals = list(range(1,2396))

# url = 'https://pro-api.coinmarketcap.com/v1/cryptocurrency/info'
url = "https://pro-api.coinmarketcap.com/v1/cryptocurrency/listings/latest"
# parameters = {
#  'id':'1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16'
# }
# headers = {
#  'Accepts': 'application/json',
#  'X-CMC_PRO_API_KEY': 'a3ddb8f0-b7c2-43e4-81d5-93bf3f50520f',
# }

# parameters = {"start": "1", "limit": "2000", "convert": "USD"}
# headers = {
#    "Accepts": "application/json",
#    "X-CMC_PRO_API_KEY": "f43cf05e-ba8f-42f8-8c8c-d8dbe6c7531b",
# }

cmc = CoinMarketCapAPI("f43cf05e-ba8f-42f8-8c8c-d8dbe6c7531b")
data_id_map = cmc.cryptocurrency_listings_latest()
df = pd.DataFrame(data_id_map.data)
df.set_index("name", inplace=True)
print(df.head())

# session = Session()
# session.headers.update(headers)
#
# try:
#    response = session.get(url, params=parameters)
#    data = json.loads(response.text)
#    with open("coinmarketcap.json", "w+") as outfile:
#        json.dump(data, outfile, indent=4)
#    # d = defaultdict(dict)
#    # for item in data:
#    #    d[item[""]]
#    # print(data)
# except (ConnectionError, Timeout, TooManyRedirects) as e:
#    print(e)
#
# data_frame = pd.read_json(r"coinmarketcap.json")
# print(data_frame.head())
