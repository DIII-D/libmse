c-------------------------------------------------------------------------------
C ROUTINE: STARK2 - interface of mse shared library to the efit code
C AUTHORS: M. Makowski, W. Meyer, D. Wroblewski, M. Brown, B. Rice
C DESC:    Calculates the tangent of the pitch angle and an error estimate
C          for EFIT.  Also returns the R and Z positions, and the
C          geometry correction coefficients A1, A2, A3, A4, A5, A6, A7
C
C          Shots > 74150 use data from the new multichannel diagnostic and use
C          and new set of subroutines and setup files.
C          Shots <= 74150 use the original MSEP code and setup files (channel
C          one only).  The calling sequence of the old and the new routines
C          are exactly the same.
C          Shots >= 80540 have 16 channels
C          Shots >  91300 have 35 channels
C CHANGES:
c         07-Nov-05  mam  Added coding for 69 channel system
c                    Eliminated msesave_btavg and msesave_time0 parameters
c         11-Apr-06  mam  Eliminated stark_mode parameter
c
c-------------------------------------------------------------------------------
c Inputs:
c
c        shot = shot number
c       times = array of times (in seconds) at which to average the data
c         n_t = number of times
c     dt_full = full width of averaging window (in seconds)
c   msefitfun = flag for fitting function
c             = 1 - Wroblewski/Rice Tangent form (default)
c             = 3 - Moller fitting function
c       bksub = background subtract mode:
c               0 = normal, 1 = multibeam, 2 = prior beam
c       quiet = flag for printing messages
c             = 0 for minimum output
c             = 1 for basic information
c             = 3 for a trace of the calling sequence of routines
c             = 6 for extensive output from this routine and those in mse_lib2
c
c-------------------------------------------------------------------------------
c Outputs:
c
c       tgamma = array of the tangent of the pitch angle for each channel 
c                evaluatedat the input times
c       egamma = error in tgamma
c            R = radius of the interesection of the sightline and the neutral 
c                beam axis
c            z = verticle location of the emission point
c    a1 ... a7 = geometry coefficients used in the evaluation of the pitch angle
c   read_error = error code: 0 - OK, 1 - error
c
c-------------------------------------------------------------------------------

      subroutine stark2( shot, times, n_t, dt_full, msefitfun,
     $  tgamma, egamma, r_mse, z_mse, a1, a2, a3, a4, a5, a6, a7, 
     $  read_error, bksub, quiet )

      implicit none
      include 'mse_lib2.inc'

      integer*4 shot, n_t, msefitfun, bksub, quiet 
      integer*4 read_error(*),st_max_t
      parameter (st_max_t = 1024)
      real*4    tgamma( n_t, * )
      real*4    egamma( n_t, * )
      real*4    dt_full, times(*)
      real*4    r_mse(*), z_mse(*)
      real*4    a1(*), a2(*)
      real*4    a3(*), a4(*) 
      real*4    a5(*), a6(*)
      real*4    a7(*) 

      integer*4 i_ch, i_t
      real*4    ms_a_avg( n_t, max_mse_chans )
      real*4    ms_b_avg( n_t, max_mse_chans )
      real*4    ms_a_std( n_t, max_mse_chans )
      real*4    ms_b_std( n_t, max_mse_chans )
      real*4    bt_avg(n_t)
      real*4    tgammauncor(n_t,*)
      integer*4 jiabs
      integer*4 ep /0/

      ep = 1
      entry stark2cer( shot, times, n_t, dt_full, msefitfun,
     $  tgamma, egamma, r_mse, z_mse, a1, a2, a3, a4, a5, a6, a7, 
     $  read_error, bksub, quiet,tgammauncor )

      if (ep .ne. 1) ep = 2

      !-------------------------------------------------------------------------

      bksub_mode = bksub
      qlevel = quiet
      shot = iabs(shot)

      print *, 'Calling $Name: V5_02 $ of the mse '


      if (ep .eq. 1) then
      call average_mse( shot, times, n_t, msefitfun, dt_full, ms_a_avg,
     $  ms_b_avg, ms_a_std, ms_b_std, tgamma, egamma,
     $  bt_avg, read_error)
      endif
      if (ep .eq. 2) then
      call average_mse_cer( shot, times, n_t, msefitfun, dt_full,
     $  ms_a_avg, ms_b_avg, ms_a_std, ms_b_std, tgamma, egamma,
     $  bt_avg, read_error, tgammauncor )
          
      endif

      do i_ch = 1, max_mse_chans
        r_mse(i_ch) = r(i_ch)
        z_mse(i_ch) = z(i_ch)
        a1(i_ch) = a_coefs_mrz( i_ch, 1 )
        a2(i_ch) = a_coefs_mrz( i_ch, 2 )
        a3(i_ch) = a_coefs_mrz( i_ch, 3 )
        a4(i_ch) = a_coefs_mrz( i_ch, 4 )
        a5(i_ch) = a_coefs_mrz( i_ch, 5 )
        a6(i_ch) = a_coefs_mrz( i_ch, 6 )
        a7(i_ch) = a_coefs_mrz( i_ch, 7 )
      enddo


      return
      end

c-------------------------------------------------------------------------------
