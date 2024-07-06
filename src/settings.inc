; settings.inc

;X macro provided by Sylvie (rangi42) on the GBDev Discord
;The order in which these values are defined here are also the order in which they are shown and modified in the settings UI
;It does not define their layout in memory relative to each other -- the variables are still manually declared
/*Format: X 
  \1 string containing the name of the option
  \2 label of variable
  \3 number of nybbles - 1
  \4 min value
  \5 max value
  \6 label of handler for left press
  \7 label of handler for right press
  \8 label of handler for A press
  \9 default value
  \10 label of draw function
*/
  X "SERIAL REMOTE", Setting_SerialRemote,      $00,  $00,  $01, Setting_SerialRemote_Toggle, Setting_SerialRemote_Toggle, Setting_SerialRemote_Toggle, $01, DrawSetting_SerialRemote
  X "ON TAKE", Setting_OnTakeAction,            $00,  $00,  $03, Setting_OnTakeAction_Dec, Setting_OnTakeAction_Inc, Setting_OnTakeAction_SetDefault, $00, DrawSetting_OnTakeAction
  X "TIMER", Setting_TimerEnable,               $00,  $00,  $01, Setting_Timer_Toggle, Setting_Timer_Toggle, Setting_Timer_Toggle, $00, DrawSetting_TimerEnable
  X "PHOTO DELAY TIME", Setting_DelayTime,       $01,  $00,  $FF, Setting_DelayTime_Dec, Setting_DelayTime_Inc, Setting_DelayTime_SetDefault, $00, DrawSetting_DelayTime
  X "AEB MODE", Setting_AEB,                    $00,  $00,  $01, Setting_AEB_Toggle,Setting_AEB_Toggle,Setting_AEB_Toggle, $00, DrawSetting_AEB
  X "AEB COUNT", Setting_AEB_Count,             $00,  $00,  $0F, Setting_AEB_Count_Dec, Setting_AEB_Count_Inc, Setting_AEB_Count_SetDefault, $01, DrawSetting_AEB_Count
  X "AEB INTERVAL", Setting_AEB_Interval,       $01,  $00,  $20, Setting_AEB_Interval_Dec, Setting_AEB_Interval_Inc, Setting_AEB_Interval_SetDefault, $01, DrawSetting_AEB_Interval
  X "PRINT ALL", NULL,                          $00,  $00,  $01, NullFunction, NullFunction, Init_PrintAll, $00, NullFunction
  X "SAVE SETTINGS",NULL,                       $00,  $00,  $01, NullFunction, NullFunctin, Init_SaveSettings, $00, NullFunction
  X "SAVE CAM OPTIONS",NULL,                    $00,  $00,  $01, NullFunction, NullFunction, Init_SaveCamOpts, $00, NullFunction
PURGE X