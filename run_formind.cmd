@echo off

set olddir=%CD%

REM cd dataInput\lowlandTropicalForest_8pft_climate

Formind.exe %olddir%\formind_parameters\paracouForest_controlPlots_8pft.par 1> %olddir%\stout.txt 2> %olddir%\sterr.txt
