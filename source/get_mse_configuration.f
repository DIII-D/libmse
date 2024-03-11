c-------------------------------------------------------------------------------
c routine: get_mse_configuration
c descrpt: returns static information about the mse system for a specified shot
c author:  m. makowski
c
c-------------------------------------------------------------------------------
c Inputs:
c
c    shot = shot number
c   quiet = setting for verbosity of informational messages
c
c-------------------------------------------------------------------------------
c Ouptut:
c
c            n_chans = number of mse channels for specified shot
c              alpha = array of angles between beam and toroidal field
c              omega = array of pi/2 + angle between t and toroidal field
c              theta = array of view angles relative to horizontal plane
c              phi_d = array of Doppler shift angles
c              r_mse = array of major radii of mse channels
c              x_mse = array of x_locations of mse channels
c              y_mse = array of y_locations of mse channels
c              z_mse = array of z locations of mse channels
c            phi_tor = array of DIII-D toroidal angles for each mse channel
c            a_coefs = array of the 7 A-coefficients for each channel
c       mse_viewport = array of viewports for each channel
c         mse_energy = array of energies for each channel
c    data_acq_system = array of type of acquisition system used for each channel
c           xyz_lens = array of xyz lens locations for each channel
c         resolution = radial resolution of channel (units = ?)
c         beam_index = array of beam indices (1: 30lt, 2: 210rt)
c         mcal1_gsso = calibration coefficients from the mcalgain file (gain,
c                      slope, btscale, btoffset)
c         mcal3_gpsd = calibration coefficients from the mcalgain3 file (gain,
c                      phase, btscale, dc_offset)
c       mrz_filename = name of the mrz file read
c      mcg1_filename = name of the mcalgain file read
c      mcg3_filename = name of the mcalgain3 file read
c      mcg5_filename = name of the mcalgain5 file read
c   msetup2_filename = name of for msetup file read
c        wavelengths = array of full, half, and third energy wavelengths
c        first_bcoil = first shot that bcoil used instead of bt for btscaling
c
c-------------------------------------------------------------------------------

      subroutine get_mse_configuration( shot, n_chans, alpha, omega,
     $  theta, phi_d, r_mse, x_mse, y_mse, z_mse, phi_tor, a_coefs, 
     $  mse_viewport, mse_energy, data_acq_system, xyz_lens, resolution, 
     $  beam_index, mcal1_gsso, mcal3_gpsd, mrz0_filename, 
     $  mcg1_filename, mcg3_filename, mcg5_filename,msetup2_filename,
     $  wavelengths,first_bcoil,quiet )

      implicit  none
      include   'mse_lib2.inc'

      character*8 mse_viewport(max_mse_chans)
      character*8 mse_energy(max_mse_chans)
      character*8 data_acq_system(max_mse_chans)
      character*128 mcg1_filename, mcg3_filename, mrz0_filename
      character*128 msetup2_filename,mcg5_filename

      character*8 lmse_viewport(max_mse_chans)
      character*8 lmse_energy(max_mse_chans)
      character*8 ldata_acq_system(max_mse_chans)
      character*128 lmcg1_filename, lmcg3_filename, lmrz0_filename
      character*128 lmsetup2_filename,lmcg5_filename
      integer*2 imse_viewport(4,max_mse_chans)
      integer*2 imse_energy(4,max_mse_chans)
      integer*2 idata_acq_system(4,max_mse_chans)
      integer*2 imcg1_filename(64), imcg3_filename(64)
      integer*2 imrz0_filename(64)
      integer*2 imsetup2_filename(64),imcg5_filename(64)


      integer*4 shot, quiet, n_chans, get_beam_index
      integer*4 beam_index(max_mse_chans),first_bcoil
      real*4    alpha(max_mse_chans), omega(max_mse_chans)
      real*4    theta(max_mse_chans), phi_d(max_mse_chans)
      real*4    r_mse(max_mse_chans), x_mse(max_mse_chans)
      real*4    y_mse(max_mse_chans), z_mse(max_mse_chans)
      real*4    resolution(max_mse_chans), phi_tor(max_mse_chans)
      real*4    wavelengths( max_mse_chans, 3 )
      real*4    a_coefs( max_mse_chans, 7 )
      real*4    xyz_lens( max_mse_chans, 3 )
      real*4    mcal1_gsso( max_mse_chans, 4 )
      real*4    mcal3_gpsd( max_mse_chans, 4 )

      integer*4 i_ch, kerror
      real*4     c_light, beam_energy, v_beam, D_alpha_wavelength
      integer*4 ep /0/


      
        equivalence(lmse_viewport,imse_viewport)
        equivalence(lmse_energy,imse_energy)
        equivalence(ldata_acq_system,idata_acq_system)
        equivalence(lmcg1_filename,imcg1_filename)
        equivalence(lmcg3_filename,imcg3_filename)
        equivalence(lmcg5_filename,imcg5_filename)
        equivalence(lmsetup2_filename,imsetup2_filename)
        equivalence(lmrz0_filename,imrz0_filename)
      integer*2 limse_viewport(4,max_mse_chans)
      integer*2 limse_energy(4,max_mse_chans)
      integer*2 lidata_acq_system(4,max_mse_chans)
      integer*2 limcg1_filename(64), limcg3_filename(64)
      integer*2 limrz0_filename(64)
      integer*2 limsetup2_filename(64),limcg5_filename(64)

        ep = 1

      entry get_mse_configuration_c( shot, n_chans, alpha, omega,
     $  theta, phi_d, r_mse, x_mse, y_mse, z_mse, phi_tor, a_coefs, 
     $  limse_viewport, limse_energy, lidata_acq_system, 
     $  xyz_lens, resolution, 
     $  beam_index, mcal1_gsso, mcal3_gpsd, limrz0_filename, 
     $  limcg1_filename, limcg3_filename, limcg5_filename,
     $  limsetup2_filename,
     $  wavelengths,first_bcoil,quiet )


      if (ep .eq. 0) ep = 2

      qlevel = quiet
      if ( qlevel .ge. 3 ) print *, '(get_mse_configuration.f)', shot


      pi = 4.0 * atan(1.0)
      r2d = 180.0 / pi
      d2r = pi / 180.0
      c_light = 2.997925e8         ! m/s
      D_alpha_wavelength = 6561.0  ! Angstrom  
      beam_energy = 81000.0        ! eV

      v_beam = c_light * sqrt( 2.0 * beam_energy / ( 2.0 * 1836.0 * 
     $  0.511 * 1.0e6  ) )         ! m/s

      ! Get the number of channels and number of beams for this shot

      call get_number_of_beams_and_channels( shot )
      if ( qlevel .ge. 3 ) print *, 
     $  ' Successful return from get_number_of_beams_and_channels'
      n_chans = n_mse_chans
      do i_ch = 1, n_mse_chans
        lmse_viewport(i_ch) = viewport(i_ch)
        lmse_energy(i_ch) =  energy(i_ch) 
        ldata_acq_system(i_ch) = data_acq_sys(i_ch)
        if (ep .eq. 1) then
           mse_viewport(i_ch) = viewport(i_ch)
           mse_energy(i_ch) = energy(i_ch)
           data_acq_system(i_ch) = data_acq_sys(i_ch)
        else if(ep .eq. 2) then
           limse_viewport(:,i_ch) = imse_viewport(:,i_ch)
           limse_energy(:,i_ch) = imse_energy(:,i_ch)
           lidata_acq_system(:,i_ch) = idata_acq_system(:,i_ch)
        endif
        
      enddo
      if ( qlevel .ge. 3 ) then
        print *, ' Populated mse_viewport, mse_energy, and '
        print *, '  data_acq_system arrays. Call to read_mse_files next'
      endif

      ! Read the mse setup file and get the set-up parameters for the
      ! given shot.  Also read in the calibration file and the positions
      ! file. All setup info is read into common.

      call read_mse_files( shot, kerror )
      if ( qlevel .ge. 3 ) print *, 
     $  ' Successful return from read_mse_files'

      lmsetup2_filename = msetup_filename
      lmrz0_filename = mrz_filename
      lmcg1_filename = mcalgain_filename
      lmcg3_filename = mcalgain3_filename
      lmcg5_filename = mcalgain5_filename
      if(ep .eq. 1) then
         msetup2_filename = msetup_filename
         mrz0_filename = mrz_filename
         mcg1_filename = mcalgain_filename
         mcg3_filename = mcalgain3_filename
         mcg5_filename = mcalgain5_filename
       else if (ep .eq. 2) then
         limsetup2_filename = imsetup2_filename
         limrz0_filename = imrz0_filename
         limcg1_filename = imcg1_filename
         limcg3_filename = imcg3_filename
         limcg5_filename = imcg5_filename
       endif
         
      first_bcoil = first_bcoil_shot

      do i_ch = 1, n_mse_chans
        mcal1_gsso(i_ch,1) = mcal_gain(i_ch)
        mcal1_gsso(i_ch,2) = mcal_slope(i_ch)
        mcal1_gsso(i_ch,3) = mcal_btscale(i_ch)
        mcal1_gsso(i_ch,4) = mcal_btoffset(i_ch)
        
        mcal3_gpsd(i_ch,1) = mcal3_gain(i_ch)
        mcal3_gpsd(i_ch,2) = mcal3_phase(i_ch)
        mcal3_gpsd(i_ch,3) = mcal3_btscale(i_ch)
        mcal3_gpsd(i_ch,4) = mcal3_dc_offset(i_ch)

        r_mse(i_ch) = r(i_ch)
        z_mse(i_ch) = z(i_ch)

        beam_index(i_ch) = get_beam_index(i_ch)
      enddo

      ! Get the channel dependent geometry quantities

      call mse_geometry( x_mse, y_mse, alpha, omega, theta, phi_d,
     $  phi_tor, a_coefs, xyz_lens, resolution )

      do i_ch = 1, n_mse_chans
        wavelengths( i_ch, 1 ) = D_alpha_wavelength * ( 1.0 - v_beam *
     $    cos( phi_d(i_ch) ) / c_light )
        wavelengths( i_ch, 2 ) = D_alpha_wavelength * ( 1.0 - v_beam *
     $    cos( phi_d(i_ch) ) / c_light / sqrt(2.0) )
        wavelengths( i_ch, 3 ) = D_alpha_wavelength * ( 1.0 - v_beam *
     $    cos( phi_d(i_ch) ) / c_light / sqrt(3.0) )

        alpha(i_ch) = alpha(i_ch) * r2d
        omega(i_ch) = omega(i_ch) * r2d
        theta(i_ch) = theta(i_ch) * r2d
        phi_d(i_ch) = phi_d(i_ch) * r2d
        phi_tor(i_ch) = phi_tor(i_ch) * r2d
      enddo

      ep = 0
      return
      end

c-------------------------------------------------------------------------------
