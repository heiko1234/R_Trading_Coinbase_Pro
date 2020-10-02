


# Literature for a Trading Bot

# https://blog.shrimpy.io/blog/python-scripts-for-crypto-trading-bots
# https://help.shrimpy.io/en/collections/1409920-tutorials

# https://medium.com/@ethanbond39/how-to-begin-algorithmic-trading-in-python-981edd51baa1



# Don t for get to install cbpro!
# pip install cbpro

### Imports

import numpy as np
import pandas as pd
import datetime as dt
import cbpro
import time
#from datetime import datetime


def which(self):
    try: 
        self = list(iter(self))
    except TypeError as e:
        raise Exception(""" ' which' methode can only be applied to iterables{}""".format(str(e)))
    indices = [i for i, x in enumerate(self) if bool(x) == True]
    return(indices)


#exchange_name = "coinbasepro"

#Load Trading Passwords

# Windows PC
pw = pd.read_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/CoinbasePro_API.csv")

# raspberry: path = "home/pi/R/ETH.Data"
# pw = pd.read_csv("home/pi/R/CoinbasePro_API.csv")




##### Trade/VIEW APIs Keys

my_api_key = pw.iloc[0,0]
my_secret = pw.iloc[0,1]
my_passphrase = pw.iloc[0,2]


#auth_client = cbpro.AuthenticatedClient(api_key,api_secret,api_passphrase)

auth_client = cbpro.AuthenticatedClient(my_api_key,my_secret,my_passphrase)



####

################################################
### Investment Details #########################
################################################


# Activates or deactivates the Trading Algorithm
Automatic_Trading = False   #or True


# Amount of Invests, Reserves and Logs
MinimumInvestment = 20.00  #EUR
MinimumInvestLog = 20.00   #EUR
MinimumCryptoLog = 0.1   #ETH



# Currency to trade, for reference:
# 'ETH-EUR' = Ether
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


# Start off by looking to buy, sell or just hold
action = "HOLD"   #"BUY"  Or  "SELL"



################################################
################################################

#################################################
### Get Current Data ############################
#################################################

# try:
# Wait for 1 second, to avoid API limit
time.sleep(1)

# Get latest data and show to the user for reference
newData = auth_client.get_product_ticker(product_id=currency)
print(newData)
currentPrice=newData['price']
currentPrice

# except:
#     # In case something went wrong with cbpro
#     print("Error Encountered")
#     break


##################################################
##################################################
################### Strategy #####################
### Strategy comes from Data Analysis with R #####
##################################################
##################################################



Historical_Data_15 = pd.read_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/ETH_EUR_15_Trading.csv")

Historical_Data_60 = pd.read_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/ETH_EUR_60_Trading.csv")

Trading_History = pd.read_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/TradingLog.csv")





Historical_Data_15
Historical_Data_60
Trading_History


################################################################
### Funds to Use ###


# The amount of currency owned and available
available_Crypto = float(auth_client.get_account(specificID_ETH)['available'])
available_Crypto = available_Crypto - MinimumCryptoLog

if available_Crypto < 0:
    available_Crypto = 0
available_Crypto

# The amount of currency owned
owned_Crypto = float(auth_client.get_account(specificID_ETH)['balance'])
owned_Crypto
owned_FIAT = float(auth_client.get_account(specificID_EUR)['balance'])
owned_FIAT

# The amount of FIAT currency owned and available and tradeable
available_FIAT = float(auth_client.get_account(specificID_EUR)['available'])
available_FIAT
tradeable_FIAT = available_FIAT - MinimumInvestLog

if tradeable_FIAT < 0:
    tradeable_FIAT = 0
tradeable_FIAT 

# The maximum amount of Cryptocurrency that can be purchased with your funds
possiblePurchase = (float(tradeable_FIAT)) / float(currentPrice)
possiblePurchase

# The value of the cryptourrency in EUR
possibleIncome_all = float(currentPrice) * owned_Crypto
possibleIncome_all
possibleIncome_available = float(currentPrice) * available_Crypto
possibleIncome_available

# Value
crypto_value = owned_Crypto * float(currentPrice)
fiat_value = owned_FIAT
tradeable_value = tradeable_FIAT + available_Crypto*float(currentPrice)
total_value = owned_FIAT + owned_Crypto*float(currentPrice)

#Now
now = dt.datetime.now()
dt_string = now.strftime("%d.%m.%Y %H:%M:%S")
dt_string

#
ORDER = action
#ORDER = "BUY"

# Summary of infos
key_infos = [[dt_string, ORDER, tradeable_FIAT, available_Crypto, owned_Crypto, owned_FIAT, float(currentPrice), tradeable_value, total_value] ]
key_infos = pd.DataFrame(data = key_infos, columns = ["date", "order", "tradeable_fiat", "tradeable_crypto", "owned_crypto", "owned_fiat", "current_price", "tradeable_value", "total_value"])
#key_infos



#STOP LOSS LIMIT from last BUY
# Trading_History = pd.read_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/TradingLog.csv")
# Trading_History
Index_Last_BUY = which(Trading_History.loc[:,"order"] == "BUY")[0]
STOP_LOSS_LIMIT = Trading_History.loc[Index_Last_BUY,"tradeable_value"]
STOP_LOSS_LIMIT = 0.8 * STOP_LOSS_LIMIT


##################################################################
###Placing Orders###
###Check Conditions for either BUY or SELL Orders ###

# Buy Conditions:

if action == "BUY" and Automated_Trading == True and tradeable_FIAT > MinimumInvest:

    # Place the order
    auth_client.place_market_order(product_id=currency, side='buy', funds=str(tradeable_FIAT))

    # Print message in the terminal for reference
    # message = "Buying Approximately " + str(possiblePurchase) + " " + \
    # currency + "  Now @ " + str(currentPrice) + "/Coin. TOTAL = " + str(tradeable_FIAT)
    # print(message)

    # Update funding level and Buy variable
    action = "HOLD"

    # Update TradingLog
    ORDER = "BUY"

    key_infos = [[dt_string, ORDER, tradeable_FIAT, available_Crypto, owned_Crypto, owned_FIAT, float(currentPrice), tradeable_value, total_value] ]
    key_infos = pd.DataFrame(data = key_infos, columns = ["date", "order", "tradeable_fiat", "tradeable_crypto", "owned_crypto", "owned_fiat", "current_price", "tradeable_value", "total_value"])

    TH = key_infos.append(Trading_History)
    TH.to_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/TradingLog.csv", sep =",")



# Sell Conditions: latest derivative is - and previous is +
if action == "SELL" and Automated_Trading == True and available_Crypto > 0:

    # Place the order
    auth_client.place_market_order(product_id=currency,side='sell',size=str(available_Crypto))

    # Print message in the terminal for reference
    # message = "Selling " + str(available_Crypto) + " " + currency + "Now @ " + \
    # str(currentPrice) + "/Coin. TOTAL = " + str(possibleIncome_available)
    # print(message)

    # Update funding level and Buy variable
    action = "HOLD"

    # Update TradingLog
    ORDER = "SELL"

    key_infos = [[dt_string, ORDER, tradeable_FIAT, available_Crypto, owned_Crypto, owned_FIAT, float(currentPrice), tradeable_value, total_value] ]
    key_infos = pd.DataFrame(data = key_infos, columns = ["date", "order", "tradeable_fiat", "tradeable_crypto", "owned_crypto", "owned_fiat", "current_price", "tradeable_value", "total_value"])

    TH = key_infos.append(Trading_History)
    TH.to_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/TradingLog.csv", sep =",")


# Stop loss: sell everything and stop trading if your value is less than 80% of last buy investment
if (possibleIncome_available) <= STOP_LOSS_LIMIT:

    # If there is any of the crypto owned, sell it all
    if available_Crypto > 0.0:
        auth_client.place_market_order(product_id = currency, side='sell', size = str(available_Crypto))
        # print("STOP LOSS SOLD ALL")

        # Update TradingLog
        ORDER = "SELL_STOP_LOSS"

        key_infos = [[dt_string, ORDER, tradeable_FIAT, available_Crypto, owned_Crypto, owned_FIAT, float(currentPrice), tradeable_value, total_value] ]
        key_infos = pd.DataFrame(data = key_infos, columns = ["date", "order", "tradeable_fiat", "tradeable_crypto", "owned_crypto", "owned_fiat", "current_price", "tradeable_value", "total_value"])

        TH = key_infos.append(Trading_History)
        TH.to_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/TradingLog.csv", sep =",")



# Wait for 30 seconds before repeating
time.sleep(30)









