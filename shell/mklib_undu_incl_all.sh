#!/bin/bash

# +PATCH,//UNDUMAG/SHELL
# +DECK,mklib_undu_incl_all,T=SHELL.

if test $UNDUMAG_INCL; then

  cd $UNDUMAG_INCL
  rm -f lib/*.a

  . shell/mklib_undu_incl.sh
  . shell/mklib_util_incl.sh
  . shell/mklib_mshcern_incl.sh
  . shell/mklib_mshplt_incl.sh
  . shell/mklib_urad_incl.sh

fi #$UNDUMAG_INCL
