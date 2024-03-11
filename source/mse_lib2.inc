c-------------------------------------------------------------------------------
C FILE:    MSE_LIB2.INC
C AUTHOR:  M. Makowski & Michael D. Brown, LLNL
C DESC:    Include file for use with STARK2 routine.
C
C change:
c
c   10/07/2005 MAM - Added coding for 69 channel system
c              Many other significant changes for the two beam system
c
c-------------------------------------------------------------------------------

      integer*4 max_mse_chans, max_sightlines, max_times
      integer*4 max_spave_vars , max_u , max_w 
      parameter (max_mse_chans = 69)
      parameter (max_sightlines = 69)
c
c Was 1024 but changed since library changed so that
c caller provides storage. Only stark2 still has the
c 1024 time point limit.
c
      parameter (max_times = 262144)
      parameter (max_spave_vars = 9)
      parameter (max_u = 5)
      parameter (max_w = 3)

      integer*4  first_16chan_shot
      parameter (first_16chan_shot = 80540)

      integer*4  first_35chan_shot
      parameter (first_35chan_shot = 91300)

      integer*4  first_36chan_shot
      parameter (first_36chan_shot = 97401)

      integer*4  first_40chan_shot
      parameter (first_40chan_shot = 100500)

      integer*4  first_45chan_shot
      parameter (first_45chan_shot = 108800)

      integer*4  first_69chan_shot
      parameter (first_69chan_shot = 127100)

      integer*4  first_allpcs_shot
      parameter (first_allpcs_shot = 161715)
c
c switch to using bcoil instead of bt after this shot
c also switch to using msetup5 and mcalgain5 after this shot
c
      integer*4  first_bcoil_shot
      parameter (first_bcoil_shot = 187328)

      real*4    mse_systematic_error
      real*4    mse_badchannel_std 
c
c 6/5/2008 Bill M. Fixed statictical error so decided
c                  to remove systematic error.
c
c     parameter (mse_systematic_error = 0.35)  ! Degree
c
      parameter (mse_systematic_error = 0.0)  ! Degree
      parameter (mse_badchannel_std = 100.0)  ! standard deviation to use when problem with data

c     Set maximum number of point to retrieve from ptdata

      integer*4 ptdata_max_pts
      parameter (ptdata_max_pts = 480256)

c     Background subtraction commom block variables and parameters

      integer*4 bksub_mode                         ! valid range: 0-4
      real*4    multi_pulse_bksub_min_width
      real*4    multi_pulse_bksub_max_width
      parameter (multi_pulse_bksub_min_width = 0.002)
      parameter (multi_pulse_bksub_max_width = 0.500)

c     Beam information

      integer*4 max_beams
      parameter (max_beams = 4)

      integer*4 max_pulses
      parameter (max_pulses = 2048)

      real*4    beam_rise_time
      parameter (beam_rise_time = 0.001)           ! seconds

      real*4    beam_fall_time
      parameter (beam_fall_time = 0.002)           ! seconds

      real*4    beam_min_on_time       ! minimum beam on time for a valid pulse
      parameter (beam_min_on_time = 0.004)         ! seconds

      real*4    beam_on_level(4)       ! minimum count as beam on
      real*4    beam_v_on_level(4)       ! minimum count as beam on
      real*4    beam_ttl_on_volts      ! minimum voltage to count as beam on
      parameter (beam_ttl_on_volts = 2.0)

      integer*4 n_beam_pts(4)
      real*4    beam_data( ptdata_max_pts, 4 )
      real*4    beam_time_base( ptdata_max_pts, 4 )

      integer*4 n_beams                ! actual number of mse beams: 1 or 2

      real*4    fwtgam(max_mse_chans)  ! channel fitting weights from efit
      integer*4 use_fwtgam             ! defaults to not use weights, ie fwtgam=1

c     Setup information for current shot.

      character*8 energy(max_mse_chans)            ! 'half' or 'full'
      character*8 viewport(max_mse_chans)          ! 315, 45, 15, 195l, 195u
      character*8 data_acq_sys(max_mse_chans)      ! 'camac', 'pcs', or 'none'
      character*96 mcaldir, msetup_comment
      character*128 mrz_filename, mcalgain_filename
      character*128 msetup_filename, mcalgain3_filename
      character*128 mcalgain4_filename,mcalgain5_filename
      character*32 spave_dir_name
      integer*4 n_mse_chans, n_sightlines          ! actual number
      integer*4 sightline_for_chan(max_mse_chans)  ! from 1 to 69  off=0

c     Calibration data array for tangent_slope fitting function

      real*4    mcal_gain(max_mse_chans)
      real*4    mcal_slope(max_mse_chans)
      real*4    mcal_btscale(max_mse_chans)
      real*4    mcal_btoffset(max_mse_chans)

c     Calibration data arrays for the tangent offset model - mam 11/09/01

      real*4    mcal3_gain(max_mse_chans)
      real*4	mcal3_phase(max_mse_chans)
      real*4	mcal3_dc_offset(max_mse_chans)
      real*4	mcal3_btscale(max_mse_chans)
      real*4	mcal4_res(max_mse_chans,5)

c     Sightline R and z intersection positions, and A-coefficients.

      real*4    r(max_sightlines)
      real*4    z(max_sightlines)
      real*4    a_coefs_mrz( max_sightlines, 7 )

c     Spatial Average data (vars = R, z, A1, A2, A3, A4, A5, A6, A7)

c     integer*4 n_spave_vars /9/, n_u /5/, n_w /3/
      integer*4 n_spave_vars , n_u , n_w 
      real*4    spatial_average_data( max_mse_chans,
     $                        max_spave_vars, max_u, max_w )

      integer*4 qlevel
      integer*4 mse_strict , ok_210lt, ok_30rt, fitfun
      real*4 max_t_beam_off
      real*4 first_on_diagbeam
      real*4 last_on_diagbeam
      real*4 diagbeam_inc
      real*4 pi,r2d,d2r

c     Parameters for CER base Er correction

      integer*4 max_cer_chans
      parameter (max_cer_chans = 100)
      integer*4    cerfit     ! use cerfit cer data
      integer*4    cerneur    ! use cerneur cer data
      integer*4    cerquick   ! use cerquick cer data
      integer*4    cerauto    ! use cerauto cer data
      parameter (cerquick = 0)  
      parameter (cerauto = 1)  
      parameter (cerfit = 2)  
      parameter (cerneur = 3)  
      integer*4 use_cer,certree, use_cer330,use_cer210

c     Parameters for Bt/Bcoil direction

      integer*4 normBt,revBt
      parameter (normBt = 0)
      parameter (revBt = 1)
      integer*4 bt_direction


c
c
c
      common /mse2_char/ mcaldir, msetup_filename, mcalgain_filename, 
     $  mrz_filename,mcalgain3_filename,spave_dir_name,msetup_comment,
     $  mcalgain4_filename,mcalgain5_filename
      common /mse2_char_array/ energy, viewport, data_acq_sys
      common /mse2_integer/ n_mse_chans, n_sightlines, n_beams, 
     $  n_spave_vars, n_u, n_w, n_beam_pts, bksub_mode, qlevel,
     $  mse_strict, ok_210lt, ok_30rt, fitfun,use_fwtgam
      common /mse2_integer_array/ sightline_for_chan
      common /mse2_real/ max_t_beam_off,first_on_diagbeam,
     $  last_on_diagbeam,diagbeam_inc,pi,r2d,d2r
      common /mse2_real_array/ mcal_slope, mcal_gain, mcal_btscale, 
     $  mcal_btoffset, mcal3_gain, mcal3_phase, mcal3_dc_offset, 
     $  mcal3_btscale, r, z, a_coefs_mrz, beam_data, beam_time_base,
     $  spatial_average_data,mcal4_res,beam_on_level,fwtgam,
     $  beam_v_on_level
      common /mse2_cer/ use_cer,certree, use_cer330,use_cer210
      common /mse2_bt/ bt_direction
