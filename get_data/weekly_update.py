import datetime
import energy #own module
import pandas as pd

path_to_data = "C:/Users/Friederike Becker/bwSyncShare/Kurse/PTSFC/ptsfc/data/energy/energy"

time_from = datetime.datetime.today()

#replace with minus 7
time_from_old = datetime.datetime(2022,10,26,16, 0)


datatoday = energy.requestSmardData(time_from = time_from, days = 7)

historicdat = pd.read_csv(path_to_data + str(time_from_old.strftime('%Y%m%d')) + ".csv")
todaydat = datetime.date.today().strftime('%Y%m%d')

datatoday = pd.concat([historicdat, datatoday])
datatoday.to_csv(path_to_data + str(time_from.strftime('%Y%m%d')) + ".csv",
	index = False) 
