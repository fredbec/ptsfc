import datetime
import pandas as pd
import energy #own module
import dax

energyPath = "C:/Users/Friederike Becker/bwSyncShare/Kurse/PTSFC/ptsfc/data/energy/raw/energy"
daxPath = "C:/Users/Friederike Becker/bwSyncShare/Kurse/PTSFC/ptsfc/data/dax/raw/dax"

#last two hours not available yet
time_from = datetime.datetime.today() - datetime.timedelta(hours = 2)

#get data from yesterday
time_from_old = time_from - datetime.timedelta(days=1)

#get whole last 7 days in case there were corrections
energyToday = energy.requestSmardData(time_from = time_from, days = 7)
daxToday = dax.requestDAXdata(time_from = time_from, days = 7)

#read in previous data
try:
	energyHist = pd.read_csv(energyPath + str(time_from_old.strftime('%Y%m%d')) + ".csv")
	daxHist = pd.read_csv(daxPath + str(time_from_old.strftime('%Y%m%d')) + ".csv")

except:
	try:
		time_from_old = time_from_old - datetime.timedelta(days = 1)
		energyHist = pd.read_csv(energyPath + str(time_from_old.strftime('%Y%m%d')) + ".csv")
		daxHist = pd.read_csv(daxPath + str(time_from_old.strftime('%Y%m%d')) + ".csv")
	except:
		time_from_old = time_from_old - datetime.timedelta(days = 2)
		energyHist = pd.read_csv(energyPath + str(time_from_old.strftime('%Y%m%d')) + ".csv")
		daxHist = pd.read_csv(daxPath + str(time_from_old.strftime('%Y%m%d')) + ".csv")




#combine data and remove duplicates (keep later instance in case of corrections)
energyUpdate = pd.concat([energyHist, energyToday]).drop_duplicates(subset = ['date', 'time'], keep = 'last')
daxUpdate = pd.concat([daxHist, daxToday]).drop_duplicates(subset = ['date'], keep = 'last')

energyUpdate.to_csv(energyPath + str(time_from.strftime('%Y%m%d')) + ".csv",
	index = False) 
daxUpdate.to_csv(daxPath + str(time_from.strftime('%Y%m%d')) + ".csv",
	index = False) 
