#! /user/bin/python3


# exchange_name = "coinbasepro"

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


# Run on windows or Run on raspberry
raspberry = True    #True or False


# Activates or deactivates the Trading Algorithm
Automated_Trading = True    #False   #or True

# No Trading, only for Testing, Test run when TRUE
Test_functions = False     #False or True

# Force a BUY or Sell
Force = False
Kind_Force = "BUY"


#Load Trading Passwords

# Windows PC
if raspberry == False:
    pw = pd.read_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/CoinbasePro_API.csv")
    #print("windows")

# raspberry: path = "home/pi/R/ETH.Data"
if raspberry == True:
    pw = pd.read_csv("/home/pi/R/ETH.Data/CoinbasePro_API.csv")
    #print("raspberry")


##### Trade/VIEW APIs Keys

my_api_key = pw.iloc[0,0]
my_secret = pw.iloc[0,1]
my_passphrase = pw.iloc[0,2]


#auth_client = cbpro.AuthenticatedClient(api_key,api_secret,api_passphrase)

auth_client = cbpro.AuthenticatedClient(my_api_key,my_secret,my_passphrase)

#auth_client.   #place limit order buy or sell

####

################################################
### Investment Details #########################
################################################





# Amount of Invests, Reserves and Logs
MinimumInvest = 20.00  #EUR
MinimumInvestLog = 20.00   #EUR
MinimumCryptoLog = 0.01   #ETH
MinimumBenefit = 1.5  # Procent %
ExtraBenefit = 2.2  # Procent % on 1. and 15. of each month, 2% more, total 4.5 %
ExtraBenefitCondition = 0.0  #Procent % on special Conditions, when special Condition reached



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
# Wait for 5 second, to avoid API limit
time.sleep(5)

# Get latest data and show to the user for reference
newData = auth_client.get_product_ticker(product_id=currency)
print(newData)
currentPrice=newData['price']
currentPrice = round(float(currentPrice), 2)
print(currentPrice)

# except:
#     # In case something went wrong with cbpro
#     print("Error Encountered")
#     break


##################################################
##################################################
# Historical Data Load

if raspberry == False:

    Historical_Data_15 = pd.read_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/ETH_EUR_15_Trading.csv")

    Historical_Data_60 = pd.read_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/ETH_EUR_60_Trading.csv")

    Trading_History = pd.read_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/TradingLog.csv")

#### Raspberry
# raspberry: path = "home/pi/R/ETH.Data"

if raspberry == True:
    Historical_Data_15 = pd.read_csv("/home/pi/R/ETH.Data/ETH_EUR_15_Trading.csv")

    Historical_Data_60 = pd.read_csv("/home/pi/R/ETH.Data/ETH_EUR_60_Trading.csv")

    Trading_History = pd.read_csv("/home/pi/R/ETH.Data/TradingLog.csv")




################################################################
### Funds to Use ###


# The amount of currency owned and available
available_Crypto = float(auth_client.get_account(specificID_ETH)['available'])
available_Crypto = available_Crypto - MinimumCryptoLog

if available_Crypto < 0:
    available_Crypto = 0
available_Crypto = round(available_Crypto, 2)
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
tradeable_FIAT =round(tradeable_FIAT, 2)
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
print(key_infos)



#STOP LOSS LIMIT from last BUY
# Trading_History = pd.read_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/TradingLog.csv")
# Trading_History
Index_Last_BUY = which(Trading_History.loc[:,"order"] == "BUY")[0]
STOP_LOSS_LIMIT = Trading_History.loc[Index_Last_BUY,"tradeable_value"]
STOP_LOSS_LIMIT = round(0.89 * STOP_LOSS_LIMIT, 2)
STOP_LOSS_LIMIT

##################################################

################### Strategy #####################
### Strategy comes from Data Analysis with R #####
##################################################
##################################################


################## USED DATA #####################
# Historical_Data_15
# Historical_Data_60
# Trading_History

msg = "Nothing to report"


##SELL
if Historical_Data_15.loc[0, "rsma50"] > 1.024:
    action = "SELL"
    msg = "SELL rsma50 > 1.024"
    print(msg)

if Historical_Data_15.loc[0, "rsma14"] > 1.04:
    action = "SELL"
    msg = "SELL rsma14 > 1.04"
    print(msg)

if Historical_Data_15.loc[0, "slopersma14"]  > 0.003 and Historical_Data_15.loc[0, "s4s10rsma14"] < 0:
    action = "SELL"
    print(Historical_Data_15.loc[0, "rsma14 and s4s10"])
    msg = "SELL slope_rsma14"
    print(msg)

if Historical_Data_15.loc[0, "rsi14"] > 74:  #war 70
    action = "SELL"
    msg = "SELL rsi14 > 74"
    print(msg)

if Historical_Data_15.loc[0, "rsi25"] > 69:
    action = "SELL"
    msg = "SELL rsi25 > 69"   # war > 65
    print(msg)

if Historical_Data_15.loc[0, "rsi50"] > 67:
    action = "SELL"
    msg = "SELL rsi50 > 67"
    print(msg)

#if Historical_Data_60.loc[0, "rsi14"]  > 65:
#    action = "SELL"
#    msg = "SELL 60rsi14 > 65"
#    print(msg)

#if Historical_Data_60.loc[0, "rsi50"]  > 58:
#    action = "SELL"
#    msg = "SELL 60rsi50 > 58"
#    print(msg)

#if Historical_Data_60.loc[0, "rsma50"]  > 1.025:
#    action = "SELL"
#    msg = "SELL 60rsma50 > 1.025"
#    print(msg)


#BUY

if Historical_Data_15.loc[0, "rsma50"]  < 0.995 and Historical_Data_15.loc[0, "slope25_5"]  > 0.0175:
    action = "BUY"
    print(Historical_Data_15.loc[0, "rsma50"])
    msg = "BUY RSMA50 < 0.995"
    print(msg)

if Historical_Data_15.loc[0, "rsma14"]  < 0.99 and Historical_Data_15.loc[0, "slope25_5"]  > -0.02:
    action = "BUY"
    print(Historical_Data_15.loc[0, "rsma14"])
    msg = "BUY RSMA14 < 0.99"
    print(msg)


if Historical_Data_15.loc[0, "rsma14"]  < 0.97:
    action = "BUY"
    print(Historical_Data_15.loc[0, "rsma14"])
    msg = "BUY RSMA14 < 0.97"
    print(msg)


if Historical_Data_15.loc[0, "slopersma14"]  < -0.003 and Historical_Data_15.loc[0, "s4s10rsma14"] > 0:
    action = "BUY"
    print(Historical_Data_15.loc[0, "rsma14 and s4s10"])
    msg = "BUY slope_rsma14"
    print(msg)


#
if Historical_Data_15.loc[0, "rsma50"]  < 0.98:
    action = "BUY"
    print(Historical_Data_15.loc[0, "rsma50"])
    msg = "BUY RSMA50 < 0.98"
    print(msg)


if Historical_Data_15.loc[0, "rsi14"] < 34:
    action = "BUY"
    print(Historical_Data_15.loc[0, "rsi14"])
    msg = "BUY rsi14 < 34"
    print(msg)


if Historical_Data_15.loc[0, "rsi25"]  < 34:
    action = "BUY"
    print(Historical_Data_15.loc[0, "rsi25"])
    msg = "BUY rsi25 < 34"
    print(msg)

# 
if Historical_Data_60.loc[0, "rsi14"] < 30:
    action = "BUY"
    print(Historical_Data_60.loc[0, "rsi14"])
    msg = "BUY 60rsi14 < 30"
    print(msg)


if Historical_Data_60.loc[0, "rsi25"]  < 30:
    action = "BUY"
    print(Historical_Data_60.loc[0, "rsi25"])
    msg = "BUY 60rsi25 < 30"
    print(msg)


if Historical_Data_60.loc[0, "rsi50"]  < 35:
    action = "BUY"
    print(Historical_Data_60.loc[0, "rsi50"])
    msg = "BUY 60rsi50 < 35"
    print(msg)


if Historical_Data_60.loc[0, "rsma50"]  < 0.98:
    action = "BUY"
    print(Historical_Data_60.loc[0, "rsma50"])
    msg = "BUY 60rsma50 < 0.98"
    print(msg)



#before Historical Data review.
print(action)
print(msg)

#####
Minimum_Benefit = 1 + (MinimumBenefit / 100)

# GET Sure SELL will benefit minimum 1 % Benefit
Index_Last_BUY = which(Trading_History.loc[:,"order"] == "BUY")[0]
Last_BUY_price = Trading_History.loc[Index_Last_BUY,"current_price"]
Limit_SELL_price = round(Last_BUY_price * (Minimum_Benefit + (ExtraBenefitCondition/100)), 2)

# for 01 and 15 of each month, a higher minimum sell price of ExtraBenefit
#dt.datetime.now().strftime("%d")
#Trading_History.loc[Index_Last_BUY, "date"][0:2]
#
if dt.datetime.now().strftime("%d") == "01":
    Limit_SELL_price = round(Last_BUY_price * (Minimum_Benefit + (ExtraBenefit/100) +(ExtraBenefitCondition/100) ), 2)

if dt.datetime.now().strftime("%d") == "14":
    Limit_SELL_price = round(Last_BUY_price * (Minimum_Benefit + (ExtraBenefit/100) + (ExtraBenefitCondition/100) ), 2)

if dt.datetime.now().strftime("%d") == "15":
    Limit_SELL_price = round(Last_BUY_price * (Minimum_Benefit + (ExtraBenefit/100) + (ExtraBenefitCondition/100) ), 2)


## BUY or SELL after a STOP LOSS
Index_Last_STOP_LOSS = which(Trading_History.loc[:,"order"] == "SELL_STOP_LOSS")[0]
Date_Last_STOP_LOSS = Trading_History.loc[Index_Last_STOP_LOSS, "date"]
now = dt.datetime.now()

Date_Last_STOP_LOSS = dt.datetime.strptime(Date_Last_STOP_LOSS,"%d.%m.%Y %H:%M:%S")
Date_Last_STOP_LOSS = Date_Last_STOP_LOSS + dt.timedelta(minutes= 1440)  #1 day
Date_Last_STOP_LOSS_SELL = Date_Last_STOP_LOSS + dt.timedelta(minutes= 4300) #3-day
# now
# Date_Last_STOP_LOSS
# now > Date_Last_STOP_LOSS   #normal True
#
if now < Date_Last_STOP_LOSS and action == "BUY":
    if currentPrice < (0.96 * Trading_History.loc[Index_Last_STOP_LOSS, "current_price"]):
        print("BUY after STOP LOSS")
        action = "BUY"
    else:
        action = "HOLD"
if now < Date_Last_STOP_LOSS_SELL and action == "SELL": 
    if currentPrice > (1.15 * Trading_History.loc[Index_Last_STOP_LOSS, "current_price"]):
        print("SELL after STOP LOSS")
        action = "SELL"
        Limit_SELL_price = round(Last_BUY_price * 1.15)
    else:
        action = "HOLD"


if action == "SELL" :
    if currentPrice > Limit_SELL_price:
        print("SELL Limit reached")
        action = action
    if currentPrice <= Limit_SELL_price:
        print("SELL Limit not reached!")
        action = "HOLD"


print("Indicator actions with Limit price")
print(action)
print("current Price:")
print(currentPrice)
print("SELL Limit:")
print(Limit_SELL_price)
print("#####################")


if Test_functions == True:
    print("Test of BUY and SELL functions")
    action = "BUY"
    print(action)


#### Date and time compairison, analysis date in timeframe of - 17 or 62 Minutes

#Now
now16 = dt.datetime.now()- dt.timedelta(minutes= 18)
actual_time16 = now16.strftime("%d.%m.%Y %H:%M:%S")
actual_time16

now61 = dt.datetime.now()- dt.timedelta(minutes= 63)
actual_time61 = now61.strftime("%d.%m.%Y %H:%M:%S")
actual_time61

last_HD_15 = Historical_Data_15.loc[0,"time"]
last_HD_15 = dt.datetime.strptime(last_HD_15,"%Y-%m-%d %H:%M:%S")
last_HD_15 = dt.datetime.strftime(last_HD_15,"%d.%m.%Y %H:%M:%S")
last_HD_15

last_HD_60 = Historical_Data_60.loc[0, "time"]
last_HD_60 = dt.datetime.strptime(last_HD_60,"%Y-%m-%d %H:%M:%S")
last_HD_60 = dt.datetime.strftime(last_HD_60,"%d.%m.%Y %H:%M:%S" )
last_HD_60

# actual_time16
# last_HD_15
# actual_time16 < last_HD_15

if actual_time16 < last_HD_15 and actual_time61 < last_HD_60:
    action = action
else:
    action = "HOLD"

#action = "BUY" # "SELL" or "HOLD"
print("action after Date compairison")
print(action)


print(actual_time16)
print(last_HD_15)
print(actual_time16 < last_HD_15)
print("###############")
print(actual_time61)
print(last_HD_60)
print(actual_time61 < last_HD_60)
print("###############")


if Test_functions == True:
    print("Test of functions")
    action = "BUY"
    print(action)    

if Force == True:
    action = Kind_Force
    if Kind_Force == "BUY":
        msg = "Forced BUY"
    if Kind_Force == "SELL":
        msg = "Forced SELL"

##################################################################
###Placing Orders###
###Check Conditions for either BUY or SELL Orders ###

# Buy Conditions:

if action == "BUY" and Automated_Trading == True and tradeable_FIAT > MinimumInvest:

    # Place the order
    if Test_functions != True:
        auth_client.place_market_order(product_id=currency, side='buy', funds=str(tradeable_FIAT))

    # Print message in the terminal for reference
    # message = "Buying Approximately " + str(possiblePurchase) + " " + \
    # currency + "  Now @ " + str(currentPrice) + "/Coin. TOTAL = " + str(tradeable_FIAT)
    # print(message)

    print("BUY Order")
    # Update funding level and Buy variable
    #action = "BUY"

    # Update TradingLog
    ORDER = "BUY"

    key_infos = [[dt_string, ORDER, tradeable_FIAT, available_Crypto, owned_Crypto, owned_FIAT, float(currentPrice), tradeable_value, total_value, msg] ]
    key_infos = pd.DataFrame(data = key_infos, columns = ["date", "order", "tradeable_fiat", "tradeable_crypto", "owned_crypto", "owned_fiat", "current_price", "tradeable_value", "total_value", "Order_Type"])
    print(key_infos)
    print(msg)
    if raspberry == True:
        Trading_History = Trading_History.iloc[:,1:]
    if raspberry == False:
        Trading_History = Trading_History
    TH = key_infos.append(Trading_History)
    #TH

    if raspberry == False:
        TH.to_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/TradingLog.csv", sep =",")

    if raspberry == True:
        TH.to_csv("/home/pi/R/ETH.Data/TradingLog.csv", sep =",")


# Sell Conditions: latest derivative is - and previous is +
if action == "SELL" and Automated_Trading == True and available_Crypto > 0:

    # Place the order
    if Test_functions != True:
        auth_client.place_market_order(product_id=currency,side='sell',size=str(available_Crypto))

    # Print message in the terminal for reference
    # message = "Selling " + str(available_Crypto) + " " + currency + "Now @ " + \
    # str(currentPrice) + "/Coin. TOTAL = " + str(possibleIncome_available)
    # print(message)

    print("SELL Order")
    # Update funding level and Buy variable
    #action = "SELL"

    # Update TradingLog
    ORDER = "SELL"

    key_infos = [[dt_string, ORDER, tradeable_FIAT, available_Crypto, owned_Crypto, owned_FIAT, float(currentPrice), tradeable_value, total_value, msg] ]
    key_infos = pd.DataFrame(data = key_infos, columns = ["date", "order", "tradeable_fiat", "tradeable_crypto", "owned_crypto", "owned_fiat", "current_price", "tradeable_value", "total_value", "Order_Type"])
    print(key_infos)
    print(msg)
    if raspberry == True:
        Trading_History = Trading_History.iloc[:,1:]
    if raspberry == False:
        Trading_History = Trading_History
    TH = key_infos.append(Trading_History)

    if raspberry == False:
        TH.to_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/TradingLog.csv", sep =",")

    if raspberry == True:
        TH.to_csv("/home/pi/R/ETH.Data/TradingLog.csv", sep =",")


# Stop loss: sell everything and stop trading if your value is less than 80% of last buy investment
if (possibleIncome_available) <= STOP_LOSS_LIMIT:

    # If there is any of the crypto owned, sell it all
    if available_Crypto > 0.0:
        if Test_functions != True:
            auth_client.place_market_order(product_id = currency, side='sell', size = str(available_Crypto))
        print("STOP LOSS SOLD ALL")
        msg = "STOP LOSS"

        # Update TradingLog
        ORDER = "SELL_STOP_LOSS"

        key_infos = [[dt_string, ORDER, tradeable_FIAT, available_Crypto, owned_Crypto, owned_FIAT, float(currentPrice), tradeable_value, total_value, msg] ]
        key_infos = pd.DataFrame(data = key_infos, columns = ["date", "order", "tradeable_fiat", "tradeable_crypto", "owned_crypto", "owned_fiat", "current_price", "tradeable_value", "total_value", "Order_Type"])
        if raspberry == True:
            Trading_History = Trading_History.iloc[:,1:]
        if raspberry == False:
            Trading_History = Trading_History

        TH = key_infos.append(Trading_History)
        
        print(TH)
        print(msg)
        if raspberry == False:
            TH.to_csv("C:/Users/Heiko/Visual.Studio/R_Trading_Coinbase_Pro/TradingLog.csv", sep =",")

        if raspberry == True:
            TH.to_csv("/home/pi/R/ETH.Data/TradingLog.csv", sep =",")



# Wait for 5 seconds before repeating
time.sleep(5)









