#from requests import Request, Session
#from requests.exceptions import ConnectionError, Timeout, TooManyRedirects
#import json
#
#url = 'https://pro-api.coinmarketcap.com/v1/cryptocurrency/listings/latest'
#parameters = {
#  'start':'1',
#  'limit':'5000',
#  'convert':'USD'
#}
#headers = {
#  'Accepts': 'application/json',
#  'X-CMC_PRO_API_KEY': 'a3ddb8f0-b7c2-43e4-81d5-93bf3f50520f',
#}
#
#session = Session()
#session.headers.update(headers)
#
#try:
#  response = session.get(url, params=parameters)
#  data = json.loads(response.text)
#  print(data)
#except (ConnectionError, Timeout, TooManyRedirects) as e:
#  print(e)
  

 #This example uses Python 2.7 and the python-request library.

from requests import Request, Session
from requests.exceptions import ConnectionError, Timeout, TooManyRedirects
import json

#vals = list(range(1,2396))

url = 'https://pro-api.coinmarketcap.com/v1/cryptocurrency/info'
parameters = {
  'id':'1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16'
}
headers = {
  'Accepts': 'application/json',
  'X-CMC_PRO_API_KEY': 'a3ddb8f0-b7c2-43e4-81d5-93bf3f50520f',
}

session = Session()
session.headers.update(headers)

try:
  response = session.get(url, params=parameters)
  data = json.loads(response.text)
  print(data)
except (ConnectionError, Timeout, TooManyRedirects) as e:
  print(e)
