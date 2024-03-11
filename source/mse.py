
from os import getenv
import matplotlib.pyplot as plt
import numpy as np
from numpy import sqrt
import ctypes as ct
from sys import stdout

PTDATA_MAX_PTS = 393216
PTDATA_MAX_PTS = 480256
MAX_TIMES = 262144
MAX_PULSES = 2048

__version__ = "$Name: V5_02 $"

#print("loaded new mse")
c_float_p = ct.POINTER(ct.c_float)
c_int_p = ct.POINTER(ct.c_int)
c_char_p = ct.c_char_p

shlib = ct.CDLL(getenv('MSE_LIB_DIR')+'/libmse.so')

def fsplit(s,slen):
    j= []
    for i in range(0,len(s.value),slen):
       j.append(s.value[i:i+slen])
    return j

class mse_container:
    """
    A class just used as a container for arrays and a hierarchy of classes.
    Gives the same sort of access as idl structures within structures.
    The _pprint method will print the hierarchy of classes.
    """

    def __init__(self):
        self.version = "$Name: V5_02 $"
        return

    def printver(self):
        print('Calling ', self.version, ' of the mse python interface')


def get_mse_max_chans():
    """
    maxchans = get_mse_max_chans()
            Get the number of channels built into the library.

    """

    chans = ct.c_int(0)
    try:
        _get_mse_max_chans = shlib.get_mse_max_chans_
    except:
        _get_mse_max_chans = shlib.get_mse_max_chans
    _get_mse_max_chans.argtypes = (c_int_p,)
    _get_mse_max_chans(ct.pointer(chans))
    return chans.value


def set_mse_quiet(quiet=0):
    """
        set_mse_quiet(quiet)
        where:
                 quiet = debug output level

        """
    try:
        _set_mse_quiet = shlib.set_mse_quiet_
    except:
        _set_mse_quiet = shlib.set_mse_quiet
    pquiet = ct.pointer(ct.c_int(quiet))
    _set_mse_quiet.argtypes = (c_int_p,)
    _set_mse_quiet(pquiet)


def set_mse_beam_logic(mse_strict=0, max_t_beam_off=0, ok_210lt=0, ok_30rt=0):
    """
        set_mse_beam_logic(mse_strict,max_t_beam_off,ok_210lt,ok_30rt)

        where:

        mse_strict = 0 - no interpolation when beams are off
                     1 - data interpolation between beam blips
        max_t_beam_off - float value off max tolerated beam off
                         duration which mse is still interpolated
        ok_210lt = 0 channels that view 210rt beam not useable if
                     210lt beam on
        ok_30rt = 0 channels that view 30lt beam not useable if
                     30rt beam on
        """
    try:
        _set_mse_beam_logic = shlib.set_mse_beam_logic_
    except:
        _set_mse_beam_logic = shlib.set_mse_beam_logic
    pmse_strict = ct.pointer(ct.c_int(mse_strict))
    pmax_t_beam_off = ct.pointer(ct.c_float(max_t_beam_off))
    pok_210lt = ct.pointer(ct.c_int(ok_210lt))
    pok_30rt = ct.pointer(ct.c_int(ok_30rt))
    _set_mse_beam_logic.argtypes = [c_int_p, c_float_p, c_int_p, c_int_p]
    _set_mse_beam_logic(pmse_strict, pmax_t_beam_off, pok_210lt, pok_30rt)

def set_cer_correction(use_cer=0, certree=0, use_cer330=0, use_cer210=0):
    """
        set_cer_correction(use_cer=0, certree=0, use_cer330=0, use_cer210=0):

        where:

        use_cer = 0 - no cer correction is done
                  1 - data interpolation between beam blips
        certree = 0 - use MDSPlus tree cerquick
                  1 - use cerauto
                  2 - use cerfit
                  3 - use cerneur
        use_cer330 = 0 - do not include the 330 system in cer correction
                      1 - include the 330 system in cer correction
        use_cer210 = 0 - include the 210 system in cer correction
                      1 - include the 210 system in cer correction
        """
    try:
        _set_cer_correction = shlib.set_cer_correction_
    except:
        _set_cer_correction = shlib.set_cer_correction
    puse_cer = ct.pointer(ct.c_int(use_cer))
    pcertree = ct.pointer(ct.c_int(certree))
    puse_cer330 = ct.pointer(ct.c_int(use_cer330))
    puse_cer210 = ct.pointer(ct.c_int(use_cer210))
    _set_cer_correction.argtypes = [c_int_p, c_int_p, c_int_p, c_int_p]
    _set_cer_correction(puse_cer, pcertree, puse_cer330, puse_cer210)

def set_mse_bksub(bksub=0):
    """
        set_mse_bksub(bksub=0)
        where:
                 bksub = background subtraction mode

        """
    try:
        _set_mse_bksub = shlib.set_mse_bksub_
    except:
        _set_mse_bksub = shlib.set_mse_bksub
    pbksub = ct.c_int(bksub)
    _set_mse_bksub.argtypes = (c_int_p,)
    _set_mse_bksub(ct.pointer(pbksub))


def stark2(shot, times, n_t, dt_full=0.01, msefitfun=3, bksub=0, quiet=0):
    """
        starkdata = stark2(shot,times,n_t)

        where:

        shot = shot number
        times = array of times (in seconds) at which to average the data
        n_t = number of times

        keywords:

          dt_full = full width of averaging window (in seconds) (default 0.01)
        msefitfun = flag for fitting function
                  = 1 - Wroblewski/Rice Tangent form 
                  = 3 - Moller fitting function (default)
            bksub = background subtract mode:
                0 = normal (default), 1 = multibeam, 2 = prior beam
            quiet = flag for printing messages
                  = 0 for minimum output (default)
                  = 1 for basic information
                  = 3 for a trace of the calling sequence of routines
                  = 6 for extensive output from this routine and those in mse_lib2

        starkdata is an object returned that has data arrays as members of the class.
        See dir(starkdata) to see what's there.
            """

    try:
        _stark2 = shlib.stark2_
    except:
        _stark2 = shlib.stark2
    _stark2.argtypes = [c_int_p, c_float_p, c_int_p, c_float_p,
                               c_int_p, c_float_p, c_float_p, c_float_p,
                               c_float_p, c_float_p, c_float_p, c_float_p,
                               c_float_p, c_float_p, c_float_p, c_float_p,
                               c_int_p, c_int_p, c_int_p]
    pshot = ct.pointer(ct.c_int(shot))
    ltimes = (ct.c_float * n_t)()  
    pn_t = ct.pointer(ct.c_int(n_t))
    pdt_full = ct.pointer(ct.c_float(dt_full))
    pmsefitfun = ct.pointer(ct.c_int(msefitfun))
    maxchans = get_mse_max_chans()
    tgamma = (ct.c_float * n_t * maxchans)()
    egamma = (ct.c_float * n_t * maxchans)()
    r_mse = (ct.c_float * maxchans)()
    z_mse = (ct.c_float * maxchans)()
    a1 = (ct.c_float * maxchans)()
    a2 = (ct.c_float * maxchans)()
    a3 = (ct.c_float * maxchans)()
    a4 = (ct.c_float * maxchans)()
    a5 = (ct.c_float * maxchans)()
    a6 = (ct.c_float * maxchans)()
    a7 = (ct.c_float * maxchans)()
    read_error = (ct.c_int * maxchans)()
    pbksub = ct.pointer(ct.c_int(bksub))
    pquiet = ct.pointer(ct.c_int(quiet))
    ltimes[:] = times[:]
   

    _stark2(pshot,ct.cast(ltimes,c_float_p),pn_t,pdt_full,pmsefitfun,
           ct.cast(tgamma,c_float_p), ct.cast(egamma,c_float_p),
           ct.cast(r_mse,c_float_p), ct.cast(z_mse,c_float_p),
           ct.cast(a1,c_float_p), ct.cast(a2,c_float_p),
           ct.cast(a3,c_float_p), ct.cast(a4,c_float_p), 
           ct.cast(a5,c_float_p), ct.cast(a6,c_float_p), 
           ct.cast(a7,c_float_p),
           ct.cast(read_error,c_int_p),pbksub,pquiet)

    # mstark2 has the max storage so use the container class to
    # trim off unneeded values in time and channel number

    m2 = mse_container()
    m2.shot = shot
    m2.times = np.array(ltimes).tolist()
    m2.n_t = n_t
    m2.msefitfun = msefitfun
    m2.chans = maxchans
    m2.gamma = np.zeros((m2.chans,n_t)).tolist()
    m2.tgamma = np.zeros((m2.chans,n_t)).tolist()
    m2.egamma = np.zeros((m2.chans,n_t)).tolist()
    for i in np.arange(m2.chans):
        m2.gamma[i][:] = np.degrees(np.arctan(np.array(tgamma[i][:]).tolist()))
        m2.tgamma[i][:] = np.array(tgamma[i][:]).tolist()
        m2.egamma[i][:] = np.array(egamma[i][:]).tolist()
    m2.r_mse = np.array(r_mse).tolist()
    m2.z_mse = np.array(z_mse).tolist()
    m2.a1 = np.array(a1).tolist()
    m2.a2 = np.array(a2).tolist()
    m2.a3 = np.array(a3).tolist()
    m2.a4 = np.array(a4).tolist()
    m2.a5 = np.array(a5).tolist()
    m2.a6 = np.array(a6).tolist()
    m2.a7 = np.array(a7).tolist()
    m2.readerror = np.array(read_error).tolist()
    m2.bksub = bksub

    return m2

def stark2cer(shot, times, n_t, dt_full=0.01, msefitfun=3, bksub=0, quiet=0):
    """
        starkdata = stark2cer(shot,times,n_t)

        where:

        shot = shot number
        times = array of times (in seconds) at which to average the data
        n_t = number of times

        keywords:

          dt_full = full width of averaging window (in seconds) (default 0.01)
        msefitfun = flag for fitting function
                  = 1 - Wroblewski/Rice Tangent form 
                  = 3 - Moller fitting function (default)
            bksub = background subtract mode:
                0 = normal (default), 1 = multibeam, 2 = prior beam
            quiet = flag for printing messages
                  = 0 for minimum output (default)
                  = 1 for basic information
                  = 3 for a trace of the calling sequence of routines
                  = 6 for extensive output from this routine and those in mse_lib2

        starkdata is an object returned that has data arrays as members of the class.
        See dir(starkdata) to see what's there.
            """

    try:
        _stark2cer = shlib.stark2cer_
    except:
        _stark2cer = shlib.stark2cer
    _stark2cer.argtypes = [c_int_p, c_float_p, c_int_p, c_float_p,
                               c_int_p, c_float_p, c_float_p, c_float_p,
                               c_float_p, c_float_p, c_float_p, c_float_p,
                               c_float_p, c_float_p, c_float_p, c_float_p,
                               c_int_p, c_int_p, c_int_p, c_float_p]
    pshot = ct.pointer(ct.c_int(shot))
    ltimes = (ct.c_float * n_t)()  
    pn_t = ct.pointer(ct.c_int(n_t))
    pdt_full = ct.pointer(ct.c_float(dt_full))
    pmsefitfun = ct.pointer(ct.c_int(msefitfun))
    maxchans = get_mse_max_chans()
    tgamma = (ct.c_float * n_t * maxchans)()
    tgammauncor = (ct.c_float * n_t * maxchans)()
    egamma = (ct.c_float * n_t * maxchans)()
    r_mse = (ct.c_float * maxchans)()
    z_mse = (ct.c_float * maxchans)()
    a1 = (ct.c_float * maxchans)()
    a2 = (ct.c_float * maxchans)()
    a3 = (ct.c_float * maxchans)()
    a4 = (ct.c_float * maxchans)()
    a5 = (ct.c_float * maxchans)()
    a6 = (ct.c_float * maxchans)()
    a7 = (ct.c_float * maxchans)()
    read_error = (ct.c_int * maxchans)()
    pbksub = ct.pointer(ct.c_int(bksub))
    pquiet = ct.pointer(ct.c_int(quiet))
    ltimes[:] = times[:]
   

    _stark2cer(pshot,ct.cast(ltimes,c_float_p),pn_t,pdt_full,pmsefitfun,
           ct.cast(tgamma,c_float_p), ct.cast(egamma,c_float_p),
           ct.cast(r_mse,c_float_p), ct.cast(z_mse,c_float_p),
           ct.cast(a1,c_float_p), ct.cast(a2,c_float_p),
           ct.cast(a3,c_float_p), ct.cast(a4,c_float_p), 
           ct.cast(a5,c_float_p), ct.cast(a6,c_float_p), 
           ct.cast(a7,c_float_p),
           ct.cast(read_error,c_int_p),pbksub,pquiet,
           ct.cast(tgammauncor,c_float_p))

    # mstark2_cer has the max storage so use the container class to
    # trim off unneeded values in time and channel number

    m2 = mse_container()
    m2.shot = shot
    m2.times = np.array(ltimes).tolist()
    m2.n_t = n_t
    m2.msefitfun = msefitfun
    m2.chans = maxchans
    m2.gamma = np.degrees(np.arctan(np.array(tgamma).tolist()))
    m2.tgamma = np.array(tgamma).tolist()
    m2.egamma = np.array(egamma).tolist()
    m2.tgammauncor = np.array(tgammauncor).tolist()
    m2.gammauncor = np.degrees(np.arctan(np.array(tgammauncor).tolist()))
    m2.r_mse = np.array(r_mse).tolist()
    m2.z_mse = np.array(z_mse).tolist()
    m2.a1 = np.array(a1).tolist()
    m2.a2 = np.array(a2).tolist()
    m2.a3 = np.array(a3).tolist()
    m2.a4 = np.array(a4).tolist()
    m2.a5 = np.array(a5).tolist()
    m2.a6 = np.array(a6).tolist()
    m2.a7 = np.array(a7).tolist()
    m2.readerror = np.array(read_error).tolist()
    m2.bksub = bksub

    return m2


def average_mse(shot, times, n_t, msefitfun=3, dt_full=0.01, bksub=0, quiet=0):
    """
        mse_avg = average_mse(shot,times,n_t)

        where:

        shot = shot number
        times = array of times (in seconds) at which to average the data
        n_t = number of times

        keywords:

          dt_full = full width of averaging window (in seconds) (default 0.01)
        msefitfun = flag for fitting function
                  = 1 - Wroblewski/Rice Tangent form (default)
                  = 3 - Moller fitting function
            bksub = background subtract mode:
                0 = normal (default), 1 = multibeam, 2 = prior beam
            quiet = flag for printing messages
                  = 0 for minimum output (default)
                  = 1 for basic information
                  = 3 for a trace of the calling sequence of routines
                  = 6 for extensive output from this routine and those in mse_lib2

        mse_avg is an object returned that has data arrays as members of the class.
        See dir(mse_avg) to see what's there.
            """

    try:
        _average_mse = shlib.average_mse_
    except:
        _average_mse = shlib.average_mse
    _average_mse.argtypes = [c_int_p, c_float_p, c_int_p, c_int_p, c_float_p,
                             c_float_p, c_float_p, c_float_p, c_float_p,
                             c_float_p, c_float_p, c_float_p, c_int_p]
                             

    set_mse_quiet(quiet=quiet)
    set_mse_bksub(bksub=bksub)
    pshot = ct.pointer(ct.c_int(shot))
    ltimes = (ct.c_float * n_t)()  
    pn_t = ct.pointer(ct.c_int(n_t))
    pmsefitfun = ct.pointer(ct.c_int(msefitfun))
    pdt_full = ct.pointer(ct.c_float(dt_full))
    maxchans = get_mse_max_chans()
    ms_a_avg = (ct.c_float * n_t * maxchans)()
    ms_b_avg = (ct.c_float * n_t * maxchans)()
    ms_a_std = (ct.c_float * n_t * maxchans)()
    ms_b_std = (ct.c_float * n_t * maxchans)()
    tan_gamma_avg = (ct.c_float * n_t * maxchans)()
    tan_gamma_std = (ct.c_float * n_t * maxchans)()
    bt_avg = (ct.c_float * n_t)()
    read_error = (ct.c_int * maxchans)()
    ltimes[:] = times[:]
   

    _average_mse(pshot,ct.cast(ltimes,c_float_p),pn_t,pmsefitfun,pdt_full,
           ct.cast(ms_a_avg,c_float_p), ct.cast(ms_b_avg,c_float_p),
           ct.cast(ms_a_std,c_float_p), ct.cast(ms_b_std,c_float_p),
           ct.cast(tan_gamma_avg,c_float_p), ct.cast(tan_gamma_std,c_float_p),
           ct.cast(bt_avg,c_float_p), ct.cast(read_error,c_int_p))

    # mstark2 has the max storage so use the container class to

    # mavg has the max storage so use the container class to
    # trim off unneeded values in time and channel number

    m2 = mse_container()
    m2.shot = shot
    m2.times = np.array(ltimes).tolist()
    m2.n_t = n_t
    m2.msefitfun = msefitfun
    m2.chans = maxchans
    m2.bt_avg = np.array(bt_avg).tolist()
    m2.ms_a_avg = np.zeros((m2.chans,n_t)).tolist()
    m2.ms_b_avg = np.zeros((m2.chans,n_t)).tolist()
    m2.ms_a_std = np.zeros((m2.chans,n_t)).tolist()
    m2.ms_b_std = np.zeros((m2.chans,n_t)).tolist()
    m2.tan_gamma_avg = np.zeros((m2.chans,n_t)).tolist()
    m2.tan_gamma_std = np.zeros((m2.chans,n_t)).tolist()
   
    for i in range(m2.chans):
        m2.ms_a_avg[i][:] = np.array(ms_a_avg[i][:]).tolist()
        m2.ms_b_avg[i][:] = np.array(ms_b_avg[i][:]).tolist()
        m2.ms_a_std[i][:] = np.array(ms_a_std[i][:]).tolist()
        m2.ms_b_std[i][:] = np.array(ms_b_std[i][:]).tolist()
        m2.tan_gamma_avg[i][:] = np.array(tan_gamma_avg[i][:]).tolist()
        m2.tan_gamma_std[i][:] = np.array(tan_gamma_std[i][:]).tolist()

    m2.readerror = np.array(read_error).tolist()

    return m2

def average_mse_cer(shot, times, n_t, msefitfun=1, dt_full=0.01, bksub=0, quiet=0):
    """
        mse_avg = average_mse_cer(shot,times,n_t)

        where:

        shot = shot number
        times = array of times (in seconds) at which to average the data
        n_t = number of times

        keywords:

          dt_full = full width of averaging window (in seconds) (default 0.01)
        msefitfun = flag for fitting function
                  = 1 - Wroblewski/Rice Tangent form (default)
                  = 3 - Moller fitting function
            bksub = background subtract mode:
                0 = normal (default), 1 = multibeam, 2 = prior beam
            quiet = flag for printing messages
                  = 0 for minimum output (default)
                  = 1 for basic information
                  = 3 for a trace of the calling sequence of routines
                  = 6 for extensive output from this routine and those in mse_lib2

        mse_avg is an object returned that has data arrays as members of the class.
        Use set_cer_correction to configure Cer correction parameters 
        See dir(mse_avg) to see what's there.
            """

    try:
        _average_mse_cer = shlib.average_mse_cer_
    except:
        _average_mse_cer = shlib.average_mse_cer
    _average_mse_cer.argtypes = [c_int_p, c_float_p, c_int_p, c_int_p, c_float_p,
                             c_float_p, c_float_p, c_float_p, c_float_p,
                             c_float_p, c_float_p, c_float_p, c_int_p, c_float_p]
                             

    set_mse_quiet(quiet=quiet)
    set_mse_bksub(bksub=bksub)
    pshot = ct.pointer(ct.c_int(shot))
    ltimes = (ct.c_float * n_t)()  
    pn_t = ct.pointer(ct.c_int(n_t))
    pmsefitfun = ct.pointer(ct.c_int(msefitfun))
    pdt_full = ct.pointer(ct.c_float(dt_full))
    maxchans = get_mse_max_chans()
    ms_a_avg = (ct.c_float * n_t * maxchans)()
    ms_b_avg = (ct.c_float * n_t * maxchans)()
    ms_a_std = (ct.c_float * n_t * maxchans)()
    ms_b_std = (ct.c_float * n_t * maxchans)()
    tan_gamma_avg = (ct.c_float * n_t * maxchans)()
    tan_gamma_std = (ct.c_float * n_t * maxchans)()
    bt_avg = (ct.c_float * n_t)()
    read_error = (ct.c_int * maxchans)()
    tan_gamma_avg_uncor = (ct.c_float * n_t * maxchans)()
    ltimes[:] = times[:]
   

    _average_mse_cer(pshot,ct.cast(ltimes,c_float_p),pn_t,pmsefitfun,pdt_full,
           ct.cast(ms_a_avg,c_float_p), ct.cast(ms_b_avg,c_float_p),
           ct.cast(ms_a_std,c_float_p), ct.cast(ms_b_std,c_float_p),
           ct.cast(tan_gamma_avg,c_float_p), ct.cast(tan_gamma_std,c_float_p),
           ct.cast(bt_avg,c_float_p), ct.cast(read_error,c_int_p),
           ct.cast(tan_gamma_avg_uncor,c_float_p))

    # mstark2 has the max storage so use the container class to

    # mavg has the max storage so use the container class to
    # trim off unneeded values in time and channel number

    m2 = mse_container()
    m2.shot = shot
    m2.times = np.array(ltimes).tolist()
    m2.n_t = n_t
    m2.msefitfun = msefitfun
    m2.chans = maxchans
    m2.bt_avg = np.array(bt_avg).tolist()
    m2.ms_a_avg = np.zeros((m2.chans,n_t)).tolist()
    m2.ms_b_avg = np.zeros((m2.chans,n_t)).tolist()
    m2.ms_a_std = np.zeros((m2.chans,n_t)).tolist()
    m2.ms_b_std = np.zeros((m2.chans,n_t)).tolist()
    m2.tan_gamma_avg = np.zeros((m2.chans,n_t)).tolist()
    m2.tan_gamma_std = np.zeros((m2.chans,n_t)).tolist()
    m2.tan_gamma_avg_uncor = np.zeros((m2.chans,n_t)).tolist()
    for i in range(m2.chans):
        m2.ms_a_avg[i][:] = np.array(ms_a_avg[i][:]).tolist()
        m2.ms_b_avg[i][:] = np.array(ms_b_avg[i][:]).tolist()
        m2.ms_a_std[i][:] = np.array(ms_a_std[i][:]).tolist()
        m2.ms_b_std[i][:] = np.array(ms_b_std[i][:]).tolist()
        m2.tan_gamma_avg[i][:] = np.array(tan_gamma_avg[i][:]).tolist()
        m2.tan_gamma_std[i][:] = np.array(tan_gamma_std[i][:]).tolist()
        m2.tan_gamma_avg_uncor[i][:] = np.array(tan_gamma_avg_uncor[i][:]).tolist()

    m2.readerror = np.array(read_error).tolist()

    return m2

def average_mse_ch(shot, times, n_t, chan, bt_data, bt_time, n_bt,
                   bt_avg, msefitfun=3, dt_full=0.01, bksub=0, quiet=0):
    """
        mse_avg_ch = average_mse_ch(shot,times,n_t,chan)

        where:

        shot = shot number
        times = array of times (in seconds) at which to average the data
        n_t = number of times
        chan = channel number to return

        keywords:

          dt_full = full width of averaging window (in seconds) (default 0.01)
        msefitfun = flag for fitting function
                  = 1 - Wroblewski/Rice Tangent form (default)
                  = 3 - Moller fitting function
            bksub = background subtract mode:
                0 = normal (default), 1 = multibeam, 2 = prior beam
            quiet = flag for printing messages
                  = 0 for minimum output (default)
                  = 1 for basic information
                  = 3 for a trace of the calling sequence of routines
                  = 6 for extensive output from this routine and those in mse_lib2

        mse_avg is an object returned that has data arrays as members of the class.
        See dir(mse_avg) to see what's there.
            """
    try:
        _average_mse_ch = shlib.average_mse_ch_
    except:
        _average_mse_ch = shlib.average_mse_ch
    _average_mse_ch.argtypes = [c_int_p, c_float_p, c_int_p, c_int_p, c_int_p, c_float_p,
                             c_float_p, c_float_p, c_float_p, c_float_p,
                             c_float_p, c_float_p, c_float_p, c_float_p, c_int_p, c_float_p, c_int_p]


    maxchans = get_mse_max_chans()
    set_mse_quiet(quiet=quiet)
    set_mse_bksub(bksub=bksub)
    pshot = ct.pointer(ct.c_int(shot))
    ltimes = (ct.c_float * n_t)()
    pn_t = ct.pointer(ct.c_int(n_t))
    pchan = ct.pointer(ct.c_int(chan))
    pmsefitfun = ct.pointer(ct.c_int(msefitfun))
    pdt_full = ct.pointer(ct.c_float(dt_full))
    ms_a_avg = (ct.c_float * n_t)()
    ms_b_avg = (ct.c_float * n_t)()
    ms_a_std = (ct.c_float * n_t)()
    ms_b_std = (ct.c_float * n_t)()
    tan_gamma_avg = (ct.c_float * n_t)()
    tan_gamma_std = (ct.c_float * n_t)()
    lbt_data = (ct.c_float * n_bt)()
    lbt_time = (ct.c_float * n_bt)()
    pn_bt = ct.pointer(ct.c_int(n_bt))
    lbt_avg = (ct.c_float * n_t)()
    ltimes[:] = times[:]
    lbt_data[:] = bt_data[:]
    lbt_time[:] = bt_time[:]
    lbt_avg[:] = bt_avg[:]
    read_error = (ct.c_int * maxchans)()


    _average_mse_ch(pshot,ct.cast(ltimes,c_float_p),pn_t,pchan,pmsefitfun,pdt_full,
           ct.cast(ms_a_avg,c_float_p), ct.cast(ms_b_avg,c_float_p),
           ct.cast(ms_a_std,c_float_p), ct.cast(ms_b_std,c_float_p),
           ct.cast(tan_gamma_avg,c_float_p), ct.cast(tan_gamma_std,c_float_p),
           ct.cast(lbt_data,c_float_p), ct.cast(lbt_time,c_float_p),pn_bt,
           ct.cast(lbt_avg,c_float_p), ct.cast(read_error,c_int_p))

    m2 = mse_container()
    m2.shot = shot
    m2.times = np.array(ltimes).tolist()
    m2.n_t = n_t
    m2.chan = chan
    m2.msefitfun = msefitfun
    m2.ms_a_avg = np.array(ms_a_avg).tolist()
    m2.ms_b_avg = np.array(ms_b_avg).tolist()
    m2.ms_a_std = np.array(ms_a_std).tolist()
    m2.ms_b_std = np.array(ms_b_std).tolist()
    m2.tan_gamma_avg = np.array(tan_gamma_avg).tolist()
    m2.tan_gamma_std = np.array(tan_gamma_std).tolist()
    m2.bt_avg = np.array(bt_avg).tolist()
    m2.bt_avg = np.array(bt_avg).tolist()
    m2.readerror = read_error.value
    m2.bksub = bksub
    return m2


def average_mse_rch(shot, times, n_t, chan, bt_data, bt_time, n_bt,
                   bt_avg, msefitfun=3, dt_full=0.01, bksub=0, quiet=0):
    """
        mse_avg_rch = average_mse_rch(shot,times,n_t,chan)

        where:

        shot = shot number
        times = array of times (in seconds) at which to average the data
        n_t = number of times
        chan = channel number to return

        keywords:

          dt_full = full width of averaging window (in seconds) (default 0.01)
        msefitfun = flag for fitting function
                  = 1 - Wroblewski/Rice Tangent form (default)
                  = 3 - Moller fitting function
            bksub = background subtract mode:
                0 = normal (default), 1 = multibeam, 2 = prior beam
            quiet = flag for printing messages
                  = 0 for minimum output (default)
                  = 1 for basic information
                  = 3 for a trace of the calling sequence of routines
                  = 6 for extensive output from this routine and those in mse_lib2

        mse_avg is an object returned that has data arrays as members of the class.
        See dir(mse_avg) to see what's there.
            """
    try:
        _average_mse_rch = shlib.average_mse_rch_
    except:
        _average_mse_rch = shlib.average_mse_rch
    _average_mse_rch.argtypes = [c_int_p, c_float_p, c_int_p, c_int_p, c_int_p, c_float_p,
                             c_float_p, c_float_p, c_float_p, c_float_p,
                             c_float_p, c_float_p, c_float_p, c_float_p, c_int_p, c_float_p, c_int_p,
                             c_float_p, c_float_p, c_float_p, c_float_p, c_int_p, 
                             c_float_p, c_float_p, c_float_p, c_float_p]


    maxchans = get_mse_max_chans()
    set_mse_quiet(quiet=quiet)
    set_mse_bksub(bksub=bksub)
    pshot = ct.pointer(ct.c_int(shot))
    ltimes = (ct.c_float * n_t)()
    pn_t = ct.pointer(ct.c_int(n_t))
    pchan = ct.pointer(ct.c_int(chan))
    pmsefitfun = ct.pointer(ct.c_int(msefitfun))
    pdt_full = ct.pointer(ct.c_float(dt_full))
    ms_a_avg = (ct.c_float * n_t)()
    ms_b_avg = (ct.c_float * n_t)()
    ms_a_std = (ct.c_float * n_t)()
    ms_b_std = (ct.c_float * n_t)()
    tan_gamma_avg = (ct.c_float * n_t)()
    tan_gamma_std = (ct.c_float * n_t)()
    lbt_data = (ct.c_float * n_bt)()
    lbt_time = (ct.c_float * n_bt)()
    pn_bt = ct.pointer(ct.c_int(n_bt))
    lbt_avg = (ct.c_float * n_t)()
    read_error = ct.c_int(0)
    pread_error = ct.pointer(read_error)
    ltimes[:] = times[:]
    lbt_data[:] = bt_data[:]
    lbt_time[:] = bt_time[:]
    lbt_avg[:] = bt_avg[:]

    mse_sin = (ct.c_float * PTDATA_MAX_PTS)()
    mse_cos = (ct.c_float * PTDATA_MAX_PTS)()
    mse_time = (ct.c_float * PTDATA_MAX_PTS)()
    raw_gamma = (ct.c_float * PTDATA_MAX_PTS)()
    bksin = ct.c_float()
    bkcos = ct.c_float()
    bksdsin = ct.c_float()
    bksdcos = ct.c_float()
    mse_npts = ct.c_int()
    pbksin = ct.pointer(bksin)
    pbkcos = ct.pointer(bkcos)
    pbksdsin = ct.pointer(bksdsin)
    pbksdcos = ct.pointer(bksdcos)
    pmse_npts = ct.pointer(mse_npts)


    _average_mse_rch(pshot,ct.cast(ltimes,c_float_p),pn_t,pchan,pmsefitfun,pdt_full,
           ct.cast(ms_a_avg,c_float_p), ct.cast(ms_b_avg,c_float_p),
           ct.cast(ms_a_std,c_float_p), ct.cast(ms_b_std,c_float_p),
           ct.cast(tan_gamma_avg,c_float_p), ct.cast(tan_gamma_std,c_float_p),
           ct.cast(lbt_data,c_float_p), ct.cast(lbt_time,c_float_p),pn_bt,
           ct.cast(lbt_avg,c_float_p), preaderror,
           ct.cast(mse_sin,c_float_p), ct.cast(mse_cos,c_float_p),
           ct.cast(raw_gamma,c_float_p), ct.cast(mse_time,c_float_p),pmse_npts,
           pbksin,pbkcos,pbksdsin,pbksdcos)

    m2 = mse_container()
    m2.shot = shot
    m2.times = np.array(ltimes).tolist()
    m2.n_t = n_t
    m2.chan = chan
    m2.msefitfun = msefitfun
    m2.ms_a_avg = np.array(ms_a_avg).tolist()
    m2.ms_b_avg = np.array(ms_b_avg).tolist()
    m2.ms_a_std = np.array(ms_a_std).tolist()
    m2.ms_b_std = np.array(ms_b_std).tolist()
    m2.tan_gamma_avg = np.array(tan_gamma_avg).tolist()
    m2.tan_gamma_std = np.array(tan_gamma_std).tolist()
    m2.bt_avg = np.array(bt_avg).tolist()
    m2.bt_avg = np.array(bt_avg).tolist()
    m2.readerror = readerror
    m2.bksub = bksub
    m2.mse_sindata = np.array(mse_sin).tolist()
    m2.mse_cosdata = np.array(mse_cos).tolist()
    m2.mse_time = np.array(mse_time).tolist()
    m2.raw_gamma = (np.array(raw_gamma) *  180.0 / np.pi).tolist()
    m2.mse_npts = mse_npts.value
    m2.bksin = bksin.value
    m2.bksdcos = bksdcos.value
    m2.bksdsin = bksdsin.value
    m2.bkcos = bkcos.value
    return m2

def average_mse_rch2(shot, times, n_t, chan, bt_data, bt_time, n_bt,
                   bt_avg, msefitfun=3, dt_full=0.01, bksub=0, quiet=0):
    """
        mse_avg_rch = average_mse_rch(shot,times,n_t,chan)

        where:

        shot = shot number
        times = array of times (in seconds) at which to average the data
        n_t = number of times
        chan = channel number to return

        keywords:

          dt_full = full width of averaging window (in seconds) (default 0.01)
        msefitfun = flag for fitting function
                  = 1 - Wroblewski/Rice Tangent form (default)
                  = 3 - Moller fitting function
            bksub = background subtract mode:
                0 = normal (default), 1 = multibeam, 2 = prior beam
            quiet = flag for printing messages
                  = 0 for minimum output (default)
                  = 1 for basic information
                  = 3 for a trace of the calling sequence of routines
                  = 6 for extensive output from this routine and those in mse_lib2

        mse_avg is an object returned that has data arrays as members of the class.
        See dir(mse_avg) to see what's there.
            """
    try:
        _average_mse_rch2 = shlib.average_mse_rch2_
    except:
        _average_mse_rch2 = shlib.average_mse_rch2
    _average_mse_rch2.argtypes = [c_int_p, c_float_p, c_int_p, c_int_p, c_int_p, c_float_p,
                             c_float_p, c_float_p, c_float_p, c_float_p,
                             c_float_p, c_float_p, c_float_p, c_float_p, c_int_p, c_float_p, c_int_p,
                             c_float_p, c_float_p, c_float_p, c_float_p, c_int_p, 
                             c_float_p, c_float_p, c_float_p, c_float_p, c_float_p]


    maxchans = get_mse_max_chans()
    set_mse_quiet(quiet=quiet)
    set_mse_bksub(bksub=bksub)
    pshot = ct.pointer(ct.c_int(shot))
    ltimes = (ct.c_float * n_t)()
    pn_t = ct.pointer(ct.c_int(n_t))
    pchan = ct.pointer(ct.c_int(chan))
    pmsefitfun = ct.pointer(ct.c_int(msefitfun))
    pdt_full = ct.pointer(ct.c_float(dt_full))
    ms_a_avg = (ct.c_float * n_t)()
    ms_b_avg = (ct.c_float * n_t)()
    ms_a_std = (ct.c_float * n_t)()
    ms_b_std = (ct.c_float * n_t)()
    tan_gamma_avg = (ct.c_float * n_t)()
    tan_gamma_std = (ct.c_float * n_t)()
    lbt_data = (ct.c_float * n_bt)()
    lbt_time = (ct.c_float * n_bt)()
    pn_bt = ct.pointer(ct.c_int(n_bt))
    lbt_avg = (ct.c_float * n_t)()
    read_error = ct.c_int()
    pread_error = ct.pointer(read_error)
    ltimes[:] = times[:]
    lbt_data[:] = bt_data[:]
    lbt_time[:] = bt_time[:]
    lbt_avg[:] = bt_avg[:]

    mse_sin = (ct.c_float * PTDATA_MAX_PTS)()
    mse_cos = (ct.c_float * PTDATA_MAX_PTS)()
    mse_time = (ct.c_float * PTDATA_MAX_PTS)()
    raw_gamma = (ct.c_float * PTDATA_MAX_PTS)()
    gamma_avg = (ct.c_float * n_t)()
    bksin = ct.c_float()
    bkcos = ct.c_float()
    bksdsin = ct.c_float()
    bksdcos = ct.c_float()
    mse_npts = ct.c_int()
    pbksin = ct.pointer(bksin)
    pbkcos = ct.pointer(bkcos)
    pbksdsin = ct.pointer(bksdsin)
    pbksdcos = ct.pointer(bksdcos)
    pmse_npts = ct.pointer(mse_npts)


    _average_mse_rch2(pshot,ct.cast(ltimes,c_float_p),pn_t,pchan,pmsefitfun,pdt_full,
           ct.cast(ms_a_avg,c_float_p), ct.cast(ms_b_avg,c_float_p),
           ct.cast(ms_a_std,c_float_p), ct.cast(ms_b_std,c_float_p),
           ct.cast(tan_gamma_avg,c_float_p), ct.cast(tan_gamma_std,c_float_p),
           ct.cast(lbt_data,c_float_p), ct.cast(lbt_time,c_float_p),pn_bt,
           ct.cast(lbt_avg,c_float_p), pread_error,
           ct.cast(mse_sin,c_float_p), ct.cast(mse_cos,c_float_p),
           ct.cast(raw_gamma,c_float_p), ct.cast(mse_time,c_float_p),pmse_npts,
           pbksin,pbkcos,pbksdsin,pbksdcos,ct.cast(gamma_avg,c_float_p))

    m2 = mse_container()
    m2.shot = shot
    m2.times = np.array(ltimes).tolist()
    m2.n_t = n_t
    m2.chan = chan
    m2.msefitfun = msefitfun
    m2.ms_a_avg = np.array(ms_a_avg).tolist()
    m2.ms_b_avg = np.array(ms_b_avg).tolist()
    m2.ms_a_std = np.array(ms_a_std).tolist()
    m2.ms_b_std = np.array(ms_b_std).tolist()
    m2.tan_gamma_avg = np.array(tan_gamma_avg).tolist()
    m2.tan_gamma_std = np.array(tan_gamma_std).tolist()
    m2.bt_avg = np.array(bt_avg).tolist()
    m2.bt_avg = np.array(bt_avg).tolist()
    m2.readerror = read_error.value
    m2.bksub = bksub
    m2.mse_sindata = np.array(mse_sin).tolist()
    m2.mse_cosdata = np.array(mse_cos).tolist()
    m2.mse_time = np.array(mse_time).tolist()
    m2.raw_gamma = (np.array(raw_gamma) *  180.0 / np.pi).tolist()
    m2.mse_npts = mse_npts.value
    m2.bksin = bksin.value
    m2.bksdcos = bksdcos.value
    m2.bksdsin = bksdsin.value
    m2.bkcos = bkcos.value
    m2.gamma_avg = np.array(gamma_avg).tolist()
    return m2


def average_mse_rch3(shot, times, n_t, chan, bt_data, bt_time, n_bt,
                   bt_avg, msefitfun=3, dt_full=0.01, bksub=0, quiet=0):
    """
        mse_avg_rch = average_mse_rch(shot,times,n_t,chan)

        where:

        shot = shot number
        times = array of times (in seconds) at which to average the data
        n_t = number of times
        chan = channel number to return

        keywords:

          dt_full = full width of averaging window (in seconds) (default 0.01)
        msefitfun = flag for fitting function
                  = 1 - Wroblewski/Rice Tangent form (default)
                  = 3 - Moller fitting function
            bksub = background subtract mode:
                0 = normal (default), 1 = multibeam, 2 = prior beam
            quiet = flag for printing messages
                  = 0 for minimum output (default)
                  = 1 for basic information
                  = 3 for a trace of the calling sequence of routines
                  = 6 for extensive output from this routine and those in mse_lib2

        mse_avg is an object returned that has data arrays as members of the class.
        See dir(mse_avg) to see what's there.
            """
    try:
        _average_mse_rch3 = shlib.average_mse_rch3_
    except:
        _average_mse_rch3 = shlib.average_mse_rch3
    _average_mse_rch3.argtypes = [c_int_p, c_float_p, c_int_p, c_int_p, c_int_p, c_float_p,
                             c_float_p, c_float_p, c_float_p, c_float_p,
                             c_float_p, c_float_p, c_float_p, c_float_p, c_int_p, c_float_p, c_int_p,
                             c_float_p, c_float_p, c_float_p, c_float_p, c_int_p, 
                             c_float_p, c_float_p, c_float_p, c_float_p, c_float_p, c_float_p]


    maxchans = get_mse_max_chans()
    set_mse_quiet(quiet=quiet)
    set_mse_bksub(bksub=bksub)
    pshot = ct.pointer(ct.c_int(shot))
    ltimes = (ct.c_float * n_t)()
    pn_t = ct.pointer(ct.c_int(n_t))
    pchan = ct.pointer(ct.c_int(chan))
    pmsefitfun = ct.pointer(ct.c_int(msefitfun))
    pdt_full = ct.pointer(ct.c_float(dt_full))
    ms_a_avg = (ct.c_float * n_t)()
    ms_b_avg = (ct.c_float * n_t)()
    ms_a_std = (ct.c_float * n_t)()
    ms_b_std = (ct.c_float * n_t)()
    tan_gamma_avg = (ct.c_float * n_t)()
    tan_gamma_std = (ct.c_float * n_t)()
    lbt_data = (ct.c_float * n_bt)()
    lbt_time = (ct.c_float * n_bt)()
    pn_bt = ct.pointer(ct.c_int(n_bt))
    lbt_avg = (ct.c_float * n_t)()
    read_error = ct.c_int(0)
    pread_error = ct.pointer(read_error)
    ltimes[:] = times[:]
    lbt_data[:] = bt_data[:]
    lbt_time[:] = bt_time[:]
    lbt_avg[:] = bt_avg[:]

    mse_sin = (ct.c_float * PTDATA_MAX_PTS)()
    mse_cos = (ct.c_float * PTDATA_MAX_PTS)()
    mse_time = (ct.c_float * PTDATA_MAX_PTS)()
    raw_gamma = (ct.c_float * PTDATA_MAX_PTS)()
    beam_mask = (ct.c_float * PTDATA_MAX_PTS)()
    gamma_avg = (ct.c_float * n_t)()
    bksin = ct.c_float()
    bkcos = ct.c_float()
    bksdsin = ct.c_float()
    bksdcos = ct.c_float()
    mse_npts = ct.c_int()
    pbksin = ct.pointer(bksin)
    pbkcos = ct.pointer(bkcos)
    pbksdsin = ct.pointer(bksdsin)
    pbksdcos = ct.pointer(bksdcos)
    pmse_npts = ct.pointer(mse_npts)


    _average_mse_rch3(pshot,ct.cast(ltimes,c_float_p),pn_t,pchan,pmsefitfun,pdt_full,
           ct.cast(ms_a_avg,c_float_p), ct.cast(ms_b_avg,c_float_p),
           ct.cast(ms_a_std,c_float_p), ct.cast(ms_b_std,c_float_p),
           ct.cast(tan_gamma_avg,c_float_p), ct.cast(tan_gamma_std,c_float_p),
           ct.cast(lbt_data,c_float_p), ct.cast(lbt_time,c_float_p),pn_bt,
           ct.cast(lbt_avg,c_float_p), pread_error,
           ct.cast(mse_sin,c_float_p), ct.cast(mse_cos,c_float_p),
           ct.cast(raw_gamma,c_float_p), ct.cast(mse_time,c_float_p),pmse_npts,
           pbksin,pbkcos,pbksdsin,pbksdcos,ct.cast(gamma_avg,c_float_p),
           ct.cast(beam_mask,c_float_p))

    m2 = mse_container()
    m2.shot = shot
    m2.times = np.array(ltimes).tolist()
    m2.n_t = n_t
    m2.chan = chan
    m2.msefitfun = msefitfun
    m2.ms_a_avg = np.array(ms_a_avg).tolist()
    m2.ms_b_avg = np.array(ms_b_avg).tolist()
    m2.ms_a_std = np.array(ms_a_std).tolist()
    m2.ms_b_std = np.array(ms_b_std).tolist()
    m2.tan_gamma_avg = np.array(tan_gamma_avg).tolist()
    m2.tan_gamma_std = np.array(tan_gamma_std).tolist()
    m2.bt_avg = np.array(bt_avg).tolist()
    m2.readerror = read_error.value
    m2.bksub = bksub
    m2.mse_sindata = np.array(mse_sin).tolist()
    m2.mse_cosdata = np.array(mse_cos).tolist()
    m2.mse_time = np.array(mse_time).tolist()
    m2.raw_gamma = (np.array(raw_gamma) *  180.0 / np.pi).tolist()
    m2.mse_npts = mse_npts.value
    m2.bksin = bksin.value
    m2.bksdcos = bksdcos.value
    m2.bksdsin = bksdsin.value
    m2.bkcos = bkcos.value
    m2.gamma_avg = np.array(gamma_avg).tolist()
    m2.beam_mask = np.array(beam_mask).tolist()
    return m2



def preprocess_mse_data(shot, chan):
    try:
        _preprocess_mse_data = shlib.preprocess_mse_data_
    except:
        _preprocess_mse_data = shlib.preprocess_mse_data
    _preprocess_mse_data.argtypes = [c_int_p, c_int_p, c_float_p, c_float_p, 
                                     c_float_p, c_int_p, c_float_p, c_float_p,
                                     c_float_p, c_float_p, c_int_p, c_int_p]
    pshot = ct.pointer(ct.c_int(shot))
    pchan = ct.pointer(ct.c_int(chan))
    mse_sindata = (ct.c_float * PTDATA_MAX_PTS)()
    pmse_sindata = ct.cast(mse_sindata,c_float_p)
    mse_cosdata = (ct.c_float * PTDATA_MAX_PTS)()
    pmse_cosdata = ct.cast(mse_cosdata,c_float_p)
    mse_time = (ct.c_float * PTDATA_MAX_PTS)()
    pmse_time = ct.cast(mse_time,c_float_p)
    mse_npts = ct.c_int()
    pmse_npts = ct.pointer(mse_npts)
    on_beg = (ct.c_float * MAX_PULSES)()
    pon_beg = ct.cast(on_beg,c_float_p)
    on_end = (ct.c_float * MAX_PULSES)()
    pon_end = ct.cast(on_end,c_float_p)
    off_beg = (ct.c_float * MAX_PULSES)()
    poff_beg = ct.cast(off_beg,c_float_p)
    off_end = (ct.c_float * MAX_PULSES)()
    poff_end = ct.cast(off_end,c_float_p)
    npulses = ct.c_int()
    pnpulses = ct.pointer(npulses)
    bksub_error = ct.c_int()
    pbksub_error = ct.pointer(bksub_error)

    _preprocess_mse_data( pshot, pchan, pmse_sindata,
        pmse_cosdata, pmse_time, pmse_npts, pon_beg, pon_end, poff_beg,
        poff_end, pnpulses, pbksub_error )


    mpp = mse_container()
    mpp.shot =  shot
    mpp.chnum = chan
    mpp.mse_sindata = np.array(mse_sindata).tolist()
    mpp.mse_cosdata = np.array(mse_cosdata).tolist()
    mpp.mse_time = np.array(mse_time).tolist()
    mpp.mse_npts = mse_npts.value
    mpp.on_beg = np.array(on_beg).tolist()
    mpp.on_end = np.array(on_end).tolist()
    mpp.off_beg = np.array(off_beg).tolist()
    mpp.off_end = np.array(off_end).tolist()
    mpp.npulses = npulses.value
    mpp.kerror = bksub_error.value
    return mpp






def get_mse_configuration(shot, quiet=0):

    try:
        _get_mse_configuration = shlib.get_mse_configuration_
    except:
        _get_mse_configuration = shlib.get_mse_configuration
    _get_mse_configuration.argtypes = [c_int_p, c_int_p, c_float_p, c_float_p, 
                                       c_float_p, c_float_p, c_float_p, c_float_p,
                                       c_float_p, c_float_p, c_float_p, c_float_p,
                                       c_char_p, c_char_p, c_char_p, c_float_p, c_float_p,
                                       c_int_p, c_float_p, c_float_p, c_char_p,
                                       c_char_p, c_char_p, c_char_p, c_char_p,
                                       c_float_p, c_int_p, c_int_p]



    pshot = ct.pointer(ct.c_int(shot))
    n_chans = ct.c_int()
    pn_chans = ct.pointer(n_chans)
    maxchans = get_mse_max_chans()
    alpha = (ct.c_float * maxchans)()
    palpha = ct.cast(alpha,c_float_p)
    omega = (ct.c_float * maxchans)()
    pomega = ct.cast(omega,c_float_p)
    theta = (ct.c_float * maxchans)()
    ptheta = ct.cast(theta,c_float_p)
    phi_d = (ct.c_float * maxchans)()
    pphi_d = ct.cast(phi_d,c_float_p)
    r_mse = (ct.c_float * maxchans)()
    pr_mse = ct.cast(r_mse,c_float_p)
    x_mse = (ct.c_float * maxchans)()
    px_mse = ct.cast(x_mse,c_float_p)
    y_mse = (ct.c_float * maxchans)()
    py_mse = ct.cast(y_mse,c_float_p)
    z_mse = (ct.c_float * maxchans)()
    pz_mse = ct.cast(z_mse,c_float_p)
    phi_tor = (ct.c_float * maxchans)()
    pphi_tor = ct.cast(phi_tor,c_float_p)
    a_coefs = (ct.c_float * 7 * maxchans)()
    pa_coefs = ct.cast(a_coefs,c_float_p)
    mse_viewport = ct.create_string_buffer(maxchans * 8)
    mse_energy = ct.create_string_buffer(maxchans * 8)
    data_acq_system = ct.create_string_buffer(maxchans * 8)
    xyz_lens = (ct.c_float * 3 * maxchans)()
    pxyz_lens = ct.cast(xyz_lens,c_float_p)
    resolution = (ct.c_float * maxchans)()
    presolution = ct.cast(resolution,c_float_p)
    beam_index = (ct.c_int * maxchans)()
    pbeam_index = ct.cast(beam_index,c_int_p)
    mcal1_gsso = (ct.c_float * 4 * maxchans)()
    pmcal1_gsso = ct.cast(mcal1_gsso,c_float_p)
    mcal3_gpsd = (ct.c_float * 4 * maxchans)()
    pmcal3_gpsd = ct.cast(mcal3_gpsd,c_float_p)
    mrz0_filename = ct.create_string_buffer(128)
    mcg1_filename = ct.create_string_buffer(128)
    mcg3_filename = ct.create_string_buffer(128)
    mcg5_filename = ct.create_string_buffer(128)
    msetup2_filename = ct.create_string_buffer(128)
    wavelengths = (ct.c_float * 3 * maxchans)()
    pwavelengths = ct.cast(wavelengths,c_float_p)  
    first_bcoil_shot = ct.c_int() 
    pfirst_bcoil_shot = ct.pointer(first_bcoil_shot)
    pquiet = ct.pointer(ct.c_int(quiet))
 
    _get_mse_configuration(pshot,pn_chans, palpha, pomega,
        ptheta, pphi_d, pr_mse, px_mse, py_mse, pz_mse, pphi_tor, pa_coefs,
        mse_viewport, mse_energy, data_acq_system, pxyz_lens, presolution,
        pbeam_index, pmcal1_gsso, pmcal3_gpsd, mrz0_filename,
        mcg1_filename, mcg3_filename, mcg5_filename,msetup2_filename,
        pwavelengths,pfirst_bcoil_shot,pquiet )


    ch_viewports = []
    viewports = []
    for viewport in fsplit(mse_viewport,8):
        if viewport != '\x00\x00\x00\x00\x00\x00\x00\x00':
            try:
               ch_viewports.append('a' + viewport.strip())
            except:
               ch_viewports.append('a' + viewport.decode().strip())
        else:
            ch_viewports.append('')
        if ch_viewports[-1] != '' and ch_viewports[-1] not in viewports:
            viewports.append(ch_viewports[-1])

    mse_config = mse_container()
    mc = mse_container()
    mse_config.beam_energy = 81000.0  # ev
    mse_config.n_chans = n_chans.value
    mc.n_chans = mse_config.n_chans
    mse_config.shot = shot
    mc.shot = mse_config.shot
    mse_config.first_bcoil_shot = first_bcoil_shot.value
    mse_config.v_beam = sqrt(
        2.0 * mse_config.beam_energy * 1.6022e-19 / 2.0 / 1.6726e-27)

    mse_config.file_names = mse_container()
    mse_config.file_names.mrz = mrz0_filename.value
    mse_config.file_names.mcalgain = mcg1_filename.value
    mse_config.file_names.mcalgain3 = mcg3_filename.value
    mse_config.file_names.msetup2 = msetup2_filename.value
    mse_config.file_names.mcalgain5 = mcg5_filename.value
    mc.mrz0_filename = mse_config.file_names.mrz
    mc.mcg1_filename = mse_config.file_names.mcalgain
    mc.mcg3_filename = mse_config.file_names.mcalgain3
    mc.mcg5_filename = mse_config.file_names.mcalgain5
    mc.msetup2_filename = mse_config.file_names.msetup2

    mse_config.chan_info = mse_container()
    mse_config.chan_info.alpha = np.array(alpha).tolist()
    mse_config.chan_info.omega = np.array(omega).tolist()
    mse_config.chan_info.theta = np.array(theta).tolist()
    mse_config.chan_info.phi_d = np.array(phi_d).tolist()
    mse_config.chan_info.phi_tor = np.array(phi_tor).tolist()
    mse_config.chan_info.beam_index = np.array(beam_index).tolist()
    mse_config.chan_info.data_acq_system = fsplit(data_acq_system,8)
    mse_config.chan_info.energy = fsplit(mse_energy,8)
    mse_config.chan_info.viewport = ch_viewports
    mse_config.chan_info.x_mse = np.array(x_mse).tolist()
    mse_config.chan_info.y_mse = np.array(y_mse).tolist()
    mse_config.chan_info.z_mse = np.array(z_mse).tolist()
    mse_config.chan_info.r_mse = np.array(r_mse).tolist()
    mse_config.chan_info.resolution = np.array(resolution).tolist()
    mse_config.chan_info.xyz_lens = np.array(xyz_lens).tolist()
    mc.alpha = mse_config.chan_info.alpha
    mc.omega = mse_config.chan_info.omega
    mc.theta = mse_config.chan_info.theta
    mc.phi_d = mse_config.chan_info.phi_d
    mc.phi_tor = mse_config.chan_info.phi_tor
    mc.beam_index = mse_config.chan_info.beam_index
    mc.data_acq_system = mse_config.chan_info.data_acq_system
    mc.mse_energy = mse_config.chan_info.energy
    mc.x_mse = mse_config.chan_info.x_mse
    mc.y_mse = mse_config.chan_info.y_mse
    mc.z_mse = mse_config.chan_info.z_mse
    mc.r_mse = mse_config.chan_info.r_mse
    mc.resolution =  mse_config.chan_info.resolution
    mc.xyz_lens = mse_config.chan_info.xyz_lens
    mc.mse_viewport  = fsplit(mse_viewport,8)


    mse_config.chan_info.a_coefs = mse_container()
    a = np.reshape(np.array(a_coefs),(-1,maxchans)).tolist()
    mse_config.chan_info.a_coefs.a1 = a[0]
    mse_config.chan_info.a_coefs.a2 = a[1]
    mse_config.chan_info.a_coefs.a3 = a[2]
    mse_config.chan_info.a_coefs.a4 = a[3]
    mse_config.chan_info.a_coefs.a5 = a[4]
    mse_config.chan_info.a_coefs.a6 = a[5]
    mse_config.chan_info.a_coefs.a7 = a[6]
    mc.a_coefs = a

    mse_config.chan_info.tangent_slope = mse_container()
    a = np.reshape(np.array(mcal1_gsso),(-1,maxchans)).tolist()
    mse_config.chan_info.tangent_slope.gain = a[0]
    mse_config.chan_info.tangent_slope.slope = a[1]
    mse_config.chan_info.tangent_slope.bt_scale = a[2]
    mse_config.chan_info.tangent_slope.bt_offset = a[3]
    mc.mcal1_gsso = a
#
    mse_config.chan_info.tangent_offset = mse_container()
    a = np.reshape(np.array(mcal3_gpsd),(-1,maxchans)).tolist()
    mse_config.chan_info.tangent_offset.gain = a[0]
    mse_config.chan_info.tangent_offset.phase = a[1]
    mse_config.chan_info.tangent_offset.bt_scale = a[2]
    mse_config.chan_info.tangent_offset.dc_offset = a[3]
    mc.mcal3_gpsd = a

    a = np.reshape(np.array(wavelengths),(-1,maxchans)).tolist()
    mse_config.chan_info.wavelengths = mse_container()
    mse_config.chan_info.wavelengths.full = a[0]
    mse_config.chan_info.wavelengths.half = a[1]
    mse_config.chan_info.wavelengths.third = a[2]
    mc.wavelengths = a
   

    array_info = mse_container()
    for viewport in viewports:
        array_info.__dict__[viewport] = mse_container()
        for datum in ['alpha', 'omega', 'theta', 'phi_d', 'phi_tor', 'beam_index', 'data_acq_system',
                      'energy', 'x_mse', 'y_mse', 'z_mse', 'r_mse', 'resolution', 'xyz_lens', 'channels']:
            array_info.__dict__[viewport].__dict__[datum] = []
        for datum in ['a_coefs', 'tangent_slope', 'tangent_offset', 'wavelengths']:
            array_info.__dict__[viewport].__dict__[datum] = mse_container()
        for datum in ['a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7']:
            array_info.__dict__[viewport].a_coefs.__dict__[datum] = []
        for datum in ['gain', 'slope', 'bt_scale', 'bt_offset']:
            array_info.__dict__[viewport].tangent_slope.__dict__[datum] = []
        for datum in ['gain', 'phase', 'bt_scale', 'dc_offset']:
            array_info.__dict__[viewport].tangent_offset.__dict__[datum] = []
        for datum in ['full', 'half', 'third']:
            array_info.__dict__[viewport].wavelengths.__dict__[datum] = []

    for i_ch in range(n_chans.value):
        viewport = mse_config.chan_info.viewport[i_ch]
        array_info.__dict__[viewport].__dict__['channels'].append(i_ch + 1)
        for datum in ['alpha', 'omega', 'theta', 'phi_d', 'phi_tor', 'beam_index', 'data_acq_system',
                      'energy', 'x_mse', 'y_mse', 'z_mse', 'r_mse', 'resolution', 'xyz_lens']:
            array_info.__dict__[viewport].__dict__[datum].append(
                mse_config.chan_info.__dict__[datum][i_ch])
        for datum in ['a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7']:
            array_info.__dict__[viewport].a_coefs.__dict__[datum].append(
                mse_config.chan_info.a_coefs.__dict__[datum][i_ch])
        for datum in ['gain', 'slope', 'bt_scale', 'bt_offset']:
            array_info.__dict__[viewport].tangent_slope.__dict__[datum].append(
                mse_config.chan_info.tangent_slope.__dict__[datum][i_ch])
        for datum in ['gain', 'phase', 'bt_scale', 'dc_offset']:
            array_info.__dict__[viewport].tangent_offset.__dict__[datum].append(
                mse_config.chan_info.tangent_offset.__dict__[datum][i_ch])
        for datum in ['full', 'half', 'third']:
            array_info.__dict__[viewport].wavelengths.__dict__[datum].append(
                mse_config.chan_info.wavelengths.__dict__[datum][i_ch])

    mse_config.array_info = array_info
    mse_config.mc = mc




    return mse_config


def read_phy_data(shot, pointname, starttime, maxpts=PTDATA_MAX_PTS):
    try:
        _read_phy_data = shlib.read_phy_data_
    except:
        _read_phy_data = shlib.read_phy_data
    _read_phy_data.argtypes = [c_int_p, c_char_p, c_float_p, c_int_p,
                               c_float_p, c_float_p, c_int_p, c_int_p]
    pshot = ct.pointer(ct.c_int(shot))
    name = ct.create_string_buffer(pointname.encode())
    pstarttime = ct.pointer(ct.c_float(starttime))
    pmaxpts = ct.pointer(ct.c_int(maxpts))
    tdata = (ct.c_float * maxpts)()
    rdata = (ct.c_float * maxpts)()

    num = ct.c_int(0)
    pnum = ct.pointer(num)
    ierr = ct.c_int(0)
    pierr = ct.pointer(ierr)
    _read_phy_data(pshot, name, pstarttime, pmaxpts, ct.cast(
        tdata, c_float_p), ct.cast(rdata, c_float_p), pnum, pierr)

    m = mse_container()
    m.shot = shot
    m.pointname = pointname
    m.starttime = starttime
    m.maxpts = maxpts
    m.np = num.value
    m.ierr = ierr.value
    m.tdata = np.array(tdata[0:m.np]).tolist()
    m.rdata = np.array(rdata[0:m.np]).tolist()

    return m

def read_getdat_data(shot, pointname, starttime, maxpts=PTDATA_MAX_PTS):
    try:
        _read_getdat_data = shlib.read_getdat_data_
    except:
        _read_getdat_data = shlib.read_getdat_data
    _read_getdat_data.argtypes = [c_int_p, c_char_p, c_float_p, c_int_p,
                               c_float_p, c_float_p, c_int_p, c_int_p]
    pshot = ct.pointer(ct.c_int(shot))
    name = ct.create_string_buffer(pointname.encode())
    pstarttime = ct.pointer(ct.c_float(starttime))
    pmaxpts = ct.pointer(ct.c_int(maxpts))
    tdata = (ct.c_float * maxpts)()
    rdata = (ct.c_float * maxpts)()

    num = ct.c_int(0)
    pnum = ct.pointer(num)
    ierr = ct.c_int(0)
    pierr = ct.pointer(ierr)
    _read_getdat_data(pshot, name, pstarttime, pmaxpts, ct.cast(
        tdata, c_float_p), ct.cast(rdata, c_float_p), pnum, pierr)

    m = mse_container()
    m.shot = shot
    m.pointname = pointname
    m.starttime = starttime
    m.maxpts = maxpts
    m.np = num.value
    m.ierr = ierr.value
    m.tdata = np.array(tdata[0:m.np]).tolist()
    m.rdata = np.array(rdata[0:m.np]).tolist()

    return m

def avg_bt(times, n_t, dt_full, bt_data, bt_time, n_bt):
    try:
        _avg_bt = shlib.avg_bt_
    except:
        _avt_bt = shlib.avg_bt
    _avg_bt.argtypes = [c_float_p, c_int_p, c_float_p, c_float_p, c_float_p, c_int_p, c_float_p]



    pn_t = ct.pointer(ct.c_int(n_t))
    pn_bt = ct.pointer(ct.c_int(n_bt))
    pdt_full = ct.pointer(ct.c_float(dt_full))
    ltimes = (ct.c_float * n_t)()
    lbt_data = (ct.c_float * n_bt)()
    lbt_time = (ct.c_float * n_bt)()
    bt_avg = (ct.c_float * n_t)()
    ltimes[:] = times[:]
    lbt_data[:] = bt_data[:]
    lbt_time[:] = bt_time[:]
    _avg_bt(ct.cast(ltimes,c_float_p), pn_t, pdt_full, 
           ct.cast(lbt_data,c_float_p), ct.cast(lbt_time,c_float_p),pn_bt,ct.cast(bt_avg,c_float_p))
    return np.array(bt_avg).tolist()

def check_bt_direction(bt_data,bt_time, n_bt):
    try:
        _check_bt_direction = shlib.check_bt_direction_
    except:
        _check_bt_direction = shlib.check_bt_direction
    _check_bt_direction.argtypes = [c_float_p, c_float_p, c_int_p]



    pn_bt = ct.pointer(ct.c_int(n_bt))
    lbt_data = (ct.c_float * n_bt)()
    lbt_time = (ct.c_float * n_bt)()
    lbt_data[:] = bt_data[:]
    lbt_time[:] = bt_time[:]
    _check_bt_direction(ct.cast(lbt_data,c_float_p), 
                        ct.cast(lbt_time,c_float_p),
                        pn_bt)
                        
    return 

def check_msefitfun(shot, msefitfun=3):
    try:
        _check_msefitfun = shlib.check_msefitfun_
    except:
        _check_msefitfun = shlib.check_msefitfun
    _check_msefitfun.argtypes = [c_int_p, c_int_p]
    lmsefitfun = ct.c_int(msefitfun)
    pmsefitfun = ct.pointer(lmsefitfun)
    pshot = ct.pointer(ct.c_int(shot))
    _check_msefitfun(pshot, pmsefitfun)
    msefitfun = lmsefitfun.value
    return msefitfun

def do_setup_and_beams(shot):
    try:
        _do_setup_and_beams = shlib.do_setup_and_beams_
    except:
        _do_setup_and_beams = shlib.do_setup_and_beams
    _do_setup_and_beams.argtypes = [c_int_p, c_int_p]
    pshot = ct.pointer(ct.c_int(shot))
    kerror = ct.c_int()
    pkerror = ct.pointer(kerror)
    _do_setup_and_beams(pshot, pkerror)
    return kerror.value

MAX_MSE_CHANS=get_mse_max_chans()


#shot = 183046
#msep_rate = 2000.0
#dt_full = 0.01
#msefitfun = 3
#dlen = 40960
#times = np.arange(dlen,dtype=np.float32) / msep_rate + -0.05
#set_mse_quiet(quiet=3)
#print(get_mse_max_chans())
#set_mse_beam_logic()
#set_mse_bksub()
#bt = read_phy_data(shot,'bt',-0.2)
#bt_avg = avg_bt(times,dlen,0.01,bt.rdata,bt.tdata,bt.np)
#plt.figure('BT')
#plt.plot(bt.tdata, bt.rdata)
#plt.plot(times, bt_avg)
#plt.show(block=False)
#m = read_phy_data(shot, 'BCOIL', -10.0)
#print(dir(m))
#bt_avg = avg_bt(times,dlen,0.01,m.rdata,m.tdata,m.np)
#plt.plot(m.tdata, m.rdata*1e-5)
#plt.plot(times, bt_avg*1e-5)
#plt.show(block=False)
#
#
#m2 = stark2(shot,times,dlen,dt_full=dt_full,msefitfun = msefitfun,quiet=0)
#plt.figure('MSEP')
#plt.plot(m2.times,m2.tgamma[:][1])
#plt.plot(m2.times,m2.tgamma[:][4])
#mavg = average_mse(shot,times,dlen,msefitfun,dt_full)
#plt.plot(mavg.times,mavg.tan_gamma_avg[1][:],color='red')
#plt.plot(mavg.times,mavg.tan_gamma_avg[4][:],color='red')
#plt.show(block=False)
#
#plt.figure('Raw')
#m3 = average_mse_rch3(shot,times,dlen,5,bt.rdata,bt.tdata,bt.np,bt_avg,quiet=1)
##plt.plot(times,m3.tan_gamma_avg)
#plt.plot(m3.mse_time,m3.beam_mask)
#plt.show(block=False)
#
#m = get_mse_configuration(shot)
#print(check_msefitfun(shot,msefitfun=1))
#set_mse_bksub(bksub=1)
#set_mse_quiet(quiet=9)
#mpp = preprocess_mse_data(shot,1)
