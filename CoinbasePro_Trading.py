

# https://blog.shrimpy.io/blog/python-scripts-for-crypto-trading-bots
# https://help.shrimpy.io/en/collections/1409920-tutorials

# https://medium.com/@ethanbond39/how-to-begin-algorithmic-trading-in-python-981edd51baa1


# pip install shrimpy-python

#import shrimpy

### Imports

import numpy as np
import pandas as pd
import datetime as dt
import cbpro
import time


#

#exchange_name = "coinbasepro"

#Trade/View

try: 
    pw = pd.read_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/CoinbasePro_API.csv")

except:
    # In case something went wrong with loading passwords
    break

##### Trade/VIEW/Transfer

my_api_key = pw.iloc[0,0]
my_secret = pw.iloc[0,1]
my_passphrase = pw.iloc[0,2]


#auth_client = cbpro.AuthenticatedClient(api_key,api_secret,api_passphrase)

auth_client = cbpro.AuthenticatedClient(my_api_key,my_secret,my_passphrase)




####

################################################################################
                        ### Investment Details ###


# Activates or deactivates the Trading Algorithm
On_Trading = False   #or True


# Amount of Invests, Reserves and Logs
MinimumInvestment = 20.00  #EUR
MinimumInvestLog = 20.00   #EUR
MinimumCryptoLog = 0.1   #ETH



# Currency to trade, for reference:
# 'BCH-USD' = Bitcoin Cash, 'BTC-USD' = Bitcoin, 'ETH-USD' = Ether
# currency = 'ETH-USD'
currency = "ETH-EUR"


# Will return the ID of your specific currency account
def getSpecificAccount(cur):
    x = auth_client.get_accounts()
    for account in x:
        if account['currency'] == cur:
            return account['id']



# Get the currency's specific ID

specificID_ETH = getSpecificAccount("ETH")
specificID_ETH

specificID_EUR = getSpecificAccount("EUR")
specificID_EUR

# Granularity (in seconds). So 300 = data from every 5 min, 900 = 15 Min
period = 900

# Start off by looking to buy

action = "HOLD"   #"BUY"  Or  "SELL"


################################################
################################################

################################################################################
### Get Current Data ###

try:
    # Wait for 1 second, to avoid API limit
    time.sleep(1)

    # Get latest data and show to the user for reference
    newData = auth_client.get_product_ticker(product_id=currency)
    print(newData)
    currentPrice=newData['price']
    currentPrice

except:
    # In case something went wrong with cbpro
    print("Error Encountered")
    break

#####################################################
#####################################################

### Strategy ###

try:
    Historical_Data_15 = pd.read_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/ETH_EUR_15_Trading.csv")

    Historical_Data_60 = pd.read_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/ETH_EUR_60.csv")

except:
    break


Historical_Data_15



################################################################################
### Funds to Use ###


# The amount of currency owned and available
available_Crypto = float(auth_client.get_account(specificID_ETH)['available'])
available_Crypto = available_Crypto - MinimumCryptoLog
available_Crypto

# The amount of currency owned
owned_Crypto = float(auth_client.get_account(specificID_ETH)['balance'])
owned_Crypto

# The amount of FIAT currency owned and available and tradeable
available_FIAT = float(auth_client.get_account(specificID_EUR)['available'])
available_FIAT
tradeable_FIAT = available_FIAT - MinimumInvestLog
tradeable_FIAT 

# The maximum amount of Cryptocurrency that can be purchased with your funds
possiblePurchase = (float(tradeable_FIAT)) / float(currentPrice)
possiblePurchase

# The value of the cryptourrency in USD
possibleIncome_all = float(currentPrice) * owned_Crypto
possibleIncome_all
possibleIncome_available = float(currentPrice) * available_Crypto
possibleIncome_available



################################################################################
###Decision Making###

# Buy Conditions: latest derivative is + and previous is -

if action == "BUY" and On_Trading == True and tradeable_FIAT > MinimumInvest:

    # Place the order
    auth_client.place_market_order(product_id=currency, side='buy', funds=str(tradeable_FIAT))

    # Print message in the terminal for reference
    message = "Buying Approximately " + str(possiblePurchase) + " " + \
    currency + "  Now @ " + str(currentPrice) + "/Coin. TOTAL = " + str(tradeable_FIAT)
    print(message)

    # Update funding level and Buy variable
    funding = 0
    #buy = False
    action = "HOLD"

# Sell Conditions: latest derivative is - and previous is +
if action == "SELL" :     ## buy == False and ######:

    # Place the order
    auth_client.place_market_order(product_id=currency,side='sell',size=str(available_Crypto))

    # Print message in the terminal for reference
    message = "Selling " + str(available_Crypto) + " " + currency + "Now @ " + \
    str(currentPrice) + "/Coin. TOTAL = " + str(possibleIncome_available)
    print(message)

    # Update funding level and Buy variable
    #funding = int(possibleIncome_available)
    #buy = True
    action = "HOLD"

# Stop loss: sell everything and stop trading if your value is less than 80% of initial investment
if (possibleIncome_available) <= 0.8 * initInvestment:

    # If there is any of the crypto owned, sell it all
    if available_Crypto > 0.0:
        auth_client.place_market_order(product_id = currency, side='sell', size = str(available_Crypto))
        print("STOP LOSS SOLD ALL")

    # Will break out of the while loop and the program will end
    break



# Printing here to make the details easier to read in the terminal
# print("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -")
# print("iteration number", iteration)

# # Print the details for reference
# print("Current Price: ", currentPrice)
# print("Your Funds = ", funding)
# print("You Own ", owned, "ETH")



# Wait for 30 seconds before repeating
time.sleep(30)









