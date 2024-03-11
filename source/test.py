import mdsstubbs as pmds
import matplotlib.pyplot as plt

msep_rate = 2000.0
dt_full = 0.01
msefitfun = 3
shot=195040
chan=2
quiet=0
bksub=7

pmds.mdsconnect('atlas.gat.com:8000::')
pmds.mdsopen('mse',shot)

if True:
    mdsmsep = pmds.mdsvalue("\msep08")
    mdsmsep_t = pmds.mdsvalue("dim_of(\msep08)")
    #mdsmsep = pmds.mdsvalue('\mse::top.analysis_02:msep08')
    #mdsmsep_t = pmds.mdsvalue('dim_of(\mse::top.analysis_02:msep08)')

    pmds.mdsdisconnect()
    plt.plot(mdsmsep_t/1000,mdsmsep,color='red')


from numpy import arange,float32
import numpy as np
import mse
from MDSplus import makeData

import sys



 

mse.set_mse_quiet(quiet)
mse.set_mse_beam_logic(mse_strict=0,max_t_beam_off=0.0,ok_30rt=0)
ms1a = mse.read_getdat_data(shot,'ms1a',-1.0)
datarange = ms1a.tdata[-1] - 0.05 
n_t = datarange * msep_rate
ln_t = int(n_t)
#times = (arange(ln_t,dtype=float32) / msep_rate + -0.05)
times = (arange(ln_t,dtype=float32) / msep_rate + 0.05)

#ln_t = int(100)
#times = (arange(ln_t,typecode=float32) / msep_rate + 3.9)
#ln_t = int(2)
#times = [3.909, 3.911]


#mavgs = mse.stark2cer(shot,times,ln_t,msefitfun=msefitfun,dt_full=dt_full,bksub=bksub)
mse.check_msefitfun(shot,msefitfun)
mseconfig = mse.get_mse_configuration(shot, quiet=quiet)
#bt = mse.read_phy_data(shot,'bt',-0.2)
bt = mse.read_phy_data(shot,'bcoil',-0.2)
bt_avg = mse.avg_bt(times,ln_t,0.05,bt.rdata,bt.tdata,bt.np)
mse.check_bt_direction(bt.rdata,bt.tdata,bt.np)
mse.do_setup_and_beams(shot)
mavgch = mse.average_mse_rch2(shot,times,ln_t,chan,bt.rdata,bt.tdata,bt.np,bt_avg,msefitfun,dt_full,quiet=quiet,bksub=bksub)
print("first call completed")
for i in range(7):
   mavgch = mse.average_mse_rch2(shot,times,ln_t,chan,bt.rdata,bt.tdata,bt.np,bt_avg,msefitfun,dt_full,quiet=quiet,bksub=bksub)
#mavg = mse.average_mse(shot,times,ln_t,msefitfun=msefitfun,dt_full=dt_full,bksub=bksub)

plt.plot(mavgch.mse_time,mavgch.raw_gamma,color='blue')
#for i in range(100):
   #print mavgch.raw_gamma[i] - mdsmsep[i]
#plt.plot(mavgch.times,np.arctan(mavgch.tan_gamma_avg[:])*180.0/np.pi,color='blue')
#plt.errorbar(mavgch.times,np.arctan(mavgch.tan_gamma_avg[:])*180.0/np.pi,yerr=np.arctan(mavgch.tan_gamma_std[:])*185.0/np.pi,color='cyan')
#plt.plot(mavgch.times,np.arctan(mavgch.tan_gamma_avg[:])*180.0/np.pi,color='cyan')
#plt.errorbar(mavgch.times,np.arctan(mavgch.tan_gamma_avg[:])*180.0/np.pi,yerr=np.arctan(mavgch.tan_gamma_std[:])*185.0/np.pi,color='cyan')
#plt.errorbar(mavgs.times,np.arctan(mavgs.tgamma[1][:])*180.0/np.pi,yerr=np.arctan(mavgs.egamma[1][:])*180.0/np.pi,color='magenta')
plt.plot(mavgs.times,np.arctan(mavgs.tgamma[chan-1][:])*180.0/np.pi,color='magenta')


plt.show()

#mdsconnect('localhost:8010::')

