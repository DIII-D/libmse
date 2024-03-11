/* file: mse_0.c */
/* desc: interfaces between IDL/PVWAVE to unix externals */
/* routines: msep_multi_0            --> msep_multi_c_f */
/*           msep_multi_mm_0         --> msep_multi_mm_c_f */
/*           msep_multi_0            --> msep_multi */
/*           msep_multi_mm_0         --> msep_multi_mm */
/*           stark2_0                --> stark2 */
/*           stark2cer_0             --> stark2cer */
/*           get_mse_configuration_0 --> get_mse_configuration */
/*           get_mse_max_chans_0     --> get_mse_max_chans */
/*           get_mse_spatial_data_0  --> get_mse_spatial_data */
/*           read_mse_files_0        --> read_mse_calibs */
/*           preprocess_mse_data_0   --> preprocess_mse_data */
/*           do_setup_and_beams_0    --> do_setup_and_beams */
/*           read_phy_data_0         --> read_phy_data */
/*           avg_bt_0                --> avg_bt        */
/*           average_mse_rch3_0      --> average_mse_rch3 */
/*           average_mse_rch4_0      --> average_mse_rch4 */
/*           check_msefitfun_0       --> check_msefitfun */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "idl_export.h"
#define MSE_FILENAME_LEN 128
#define MSE_INFO_LEN 8

/* On the sun you need a trailing underscore */
#if defined(sun) || defined(__alpha) || defined(linux)
# define STARK2                stark2_
# define STARK2CER             stark2cer_
# define GET_MSE_CONFIGURATION get_mse_configuration_
# define GET_MSE_CONFIGURATION_C get_mse_configuration_c_
# define GET_MSE_MAX_CHANS     get_mse_max_chans_
# define GET_MSE_SPATIAL_DATA      get_mse_spatial_data_
# define READ_MSE_FILES        read_mse_files_
# define PREPROCESS_MSE_DATA   preprocess_mse_data_
# define SET_MSE_QUIET set_mse_quiet_
# define SET_MSE_BKSUB set_mse_bksub_
# define SET_MSE_BEAM_LOGIC set_mse_beam_logic_
# define SET_CER_CORRECTION set_cer_correction_
# define AVERAGE_MSE_RCH average_mse_rch_
# define DO_SETUP_AND_BEAMS do_setup_and_beams_
# define READ_PHY_DATA read_phy_data_
# define AVG_BT avg_bt_
# define AVERAGE_MSE_RCH3 average_mse_rch3_
# define AVERAGE_MSE_RCH4 average_mse_rch4_
# define CHECK_MSEFITFUN check_msefitfun_
#else
# define STARK2                stark2
# define STARK2CER             stark2cer
# define GET_MSE_CONFIGURATION get_mse_configuration
# define GET_MSE_CONFIGURATION_C get_mse_configuration_c
# define GET_MSE_MAX_CHANS     get_mse_max_chans
# define GET_MSE_SPATIAL_DATA      get_mse_spatial_data
# define READ_MSE_FILES        read_mse_files
# define PREPROCESS_MSE_DATA   preprocess_mse_data
# define SET_MSE_QUIET set_mse_quiet
# define SET_MSE_BKSUB set_mse_bksub
# define SET_MSE_BEAM_LOGIC set_mse_beam_logic
# define SET_CER_CORRECTION set_cer_correction
# define AVERAGE_MSE_RCH average_mse_rch
# define DO_SETUP_AND_BEAMS do_setup_and_beams
# define READ_PHY_DATA read_phy_data
# define AVG_BT avg_bt
# define AVERAGE_MSE_RCH3 average_mse_rch3
# define AVERAGE_MSE_RCH4 average_mse_rch4
# define CHECK_MSEFITFUN check_msefitfun
#endif

extern void STARK2();
extern void GET_MSE_CONFIGURATION();
extern void GET_MSE_MAX_CHANS();
extern void SET_MSE_QUIET();
extern void READ_MSE_FILES();
extern void PREPROCESS_MSE_DATA();
extern void AVERAGE_MSE_RCH();
extern void DO_SETUP_AND_BEAMS();
extern void READ_PHY_DATA();
extern void AVG_BT();
extern void AVERAGE_MSE_RCH3();
extern void CHECK_MSEFITFUN();

int stark2_0( int argc, void *argv[] )
{
  STARK2(  argv[0],  argv[1],  argv[2],  argv[3],  argv[4],  argv[5],
	   argv[6],  argv[7],  argv[8],  argv[9], argv[10], argv[11],
          argv[12], argv[13], argv[14], argv[15], argv[16], argv[17], 
          argv[18] );

  return *((long *)argv[16]);  /* kerror */
}

int stark2cer_0( int argc, void *argv[] )
{
  STARK2(  argv[0],  argv[1],  argv[2],  argv[3],  argv[4],  argv[5],
	   argv[6],  argv[7],  argv[8],  argv[9], argv[10], argv[11],
          argv[12], argv[13], argv[14], argv[15], argv[16], argv[17], 
          argv[18], argv[19] );

  return *((long *)argv[16]);  /* kerror */
}


int get_mse_configuration_0( int argc, void *argv[] )
{
int i;
int *nchans;
int mse_max_chans;


if (argc != 28) {
   printf("Get_mse_configuration: wrong number of args %d\n",argc);
   return 0;
}
GET_MSE_MAX_CHANS(&mse_max_chans);

  GET_MSE_CONFIGURATION_C( argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], 
	                 argv[6], argv[7], argv[8], argv[9], argv[10], argv[11], 
                         argv[12], argv[13], argv[14], argv[15], argv[16],
			 argv[17], argv[18], argv[19], argv[20],
                         argv[21], argv[22], argv[23], 
                         argv[24], argv[25], argv[26], argv[27] );
  return 0;
}


int get_mse_max_chans_0( int argc, void *argv[] )
{
    GET_MSE_MAX_CHANS( argv[0] );
}


int get_mse_spatial_data_0( int argc, void *argv[] )
{
    GET_MSE_SPATIAL_DATA( argv[0] );
}


int read_mse_files_0( int argc, void *argv[] )
{
  READ_MSE_FILES( argv[0], argv[1] );

  return 0;
}

int set_mse_quiet_0( int argc, void *argv[] )
{
  int *quiet;

  SET_MSE_QUIET( argv[0]);
  return 0;
}
int set_mse_bksub_0( int argc, void *argv[] )
{
  int *bksub;

  SET_MSE_BKSUB( argv[0]);
  return 0;
}
int set_mse_beam_logic_0( int argc, void *argv[] )
{
  SET_MSE_BEAM_LOGIC( argv[0],argv[1],argv[2],argv[3]);

  return 0;
}

int set_cer_correction_0( int argc, void *argv[] )
{
  SET_CER_CORRECTION( argv[0],argv[1],argv[2],argv[3]);

  return 0;
}


int preprocess_mse_data_0( int argc, void *argv[] )
{
  PREPROCESS_MSE_DATA( argv[0],  argv[1], argv[2], argv[3], argv[4], 
	               argv[5],  argv[6], argv[7], argv[8], argv[9], 
		      argv[10], argv[11] );

  return 0;
}
int average_mse_rch_0( int argc, void *argv[] )
{
  AVERAGE_MSE_RCH(  argv[0],  argv[1],  argv[2],  argv[3],  argv[4],  argv[5],
	   argv[6],  argv[7],  argv[8],  argv[9], argv[10], argv[11],
          argv[12], argv[13], argv[14], argv[15], argv[16], argv[17], 
          argv[18] ,argv[19], argv[20], argv[21], argv[22], argv[23],
	  argv[24],argv[25]);

  return 0;
}
int do_setup_and_beams_0( int argc, void *argv[] )
{
  DO_SETUP_AND_BEAMS( argv[0],  argv[1]);
  return 0;
}
int read_phy_data_0( int argc, void *argv[] )
{
  READ_PHY_DATA(  argv[0],  argv[1],  argv[2],  argv[3],  argv[4],  argv[5],
	   argv[6],  argv[7]);
  return 0;
}
int avg_bt_0( int argc, void *argv[] )
{
  AVG_BT(  argv[0],  argv[1],  argv[2],  argv[3],  argv[4],  argv[5],
	   argv[6]);
  return 0;
}
int average_mse_rch3_0( int argc, void *argv[] )
{
  AVERAGE_MSE_RCH3(  argv[0],  argv[1],  argv[2],  argv[3],  argv[4],  argv[5],
	   argv[6], argv[7],  argv[8],  argv[9], argv[10], argv[11],
          argv[12], argv[13], argv[14], argv[15], argv[16], argv[17], 
          argv[18], argv[19], argv[20], argv[21], argv[22], argv[23],
	  argv[24], argv[25], argv[26], argv[27]);

  return 0;
}
int average_mse_rch4_0( int argc, void *argv[] )
{
  AVERAGE_MSE_RCH4(  argv[0],  argv[1],  argv[2],  argv[3],  argv[4],  argv[5],
	   argv[6], argv[7],  argv[8],  argv[9], argv[10], argv[11],
          argv[12], argv[13], argv[14], argv[15], argv[16], argv[17], 
          argv[18], argv[19], argv[20], argv[21], argv[22], argv[23],
	  argv[24], argv[25], argv[26], argv[27],argv[28]);

  return 0;
}
int check_msefitfun_0(int argc, void *argv[] )
{

  CHECK_MSEFITFUN( argv[0], argv[1]);
  return 0;
}




#ifndef hpux
#include <signal.h>
int sigsetmask_( int how, const sigset_t *set, sigset_t *o_set )
{
#ifdef osf
  return( sigsetmask(how) );
#endif
}


#endif
int mse_hello()
{
printf("hello world\n");
}

int mse_hello_return( int argc, void *argv[] )
{
IDL_StrStore((IDL_STRING *)argv[0],(char *)"hello world");
return 0;
}
int mse_hello_return_f( int argc, void *argv[] )
{
mse_hello_world_f_((short *)argv[0]);
return 0;
}

