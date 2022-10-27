#from datetime import datetime, timedelta
#from pytz import timezone
#import pytz
import datetime
import energy

path_to_data = "C:/Users/Friederike Becker/bwSyncShare/Kurse/PTSFC/ptsfc/data/energy/energy"

time_from = datetime.datetime(2022,10,26,16, 0)

datatoday = energy.requestSmardData(time_from = time_from, days = 365*7)
print(datatoday.head())
todaydat = datetime.date.today().strftime('%Y%m%d')
datatoday.to_csv(path_to_data + str(time_from.strftime('%Y%m%d')) + ".csv",
	index = False) 
