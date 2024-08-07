; settings.inc

;X macro provided by Sylvie (rangi42) on the GBDev Discord
;The order in which these values are defined here are also the order in which they are shown and modified in the settings UI
;It does not define their layout in memory relative to each other -- the variables are still manually declared
/*Format: X 
  \1 string containing the name of the option
  \2 label of variable
  \3 boolean for whether or not the entry corresponds to a variable
  \4 min value
  \5 max value
  \6 label of handler for left press
  \7 label of handler for right press
  \8 label of handler for A press
  \9 initial value (default value is set manually in the default function)

*/
  DEF SETTINGS_X_INDEX = 0
  X "SERIAL REMOTE", Setting_SerialRemote,      $01,  $00,  $01, Setting_SerialRemote_Toggle, Setting_SerialRemote_Toggle, Setting_SerialRemote_Toggle, $01, DrawSetting_SerialRemote, $01
  X "ON TAKE", Setting_OnTakeAction,            $01,  $00,  $03, Setting_OnTakeAction_Dec, Setting_OnTakeAction_Inc, Setting_OnTakeAction_SetDefault, $00, DrawSetting_OnTakeAction, 1
;  X "TIMER", Setting_TimerEnable,               $00,  $00,  $01, Setting_Timer_Toggle, Setting_Timer_Toggle, Setting_Timer_Toggle, $00, DrawSetting_TimerEnable
  X "PHOTO DELAY TIME", Setting_DelayTime,       $01,  $00,  $FF, Setting_DelayTime_Dec, Setting_DelayTime_Inc, Setting_DelayTime_SetDefault, $00, DrawSetting_DelayTime, 1
  X "AEB MODE", Setting_AEB,                    $01,  $00,  $01, Setting_AEB_Toggle,Setting_AEB_Toggle,Setting_AEB_Toggle, $00, DrawSetting_AEB, 1
  X "AEB COUNT", Setting_AEB_Count,             $01,  $01,  $0E, Setting_AEB_Count_Dec, Setting_AEB_Count_Inc, Setting_AEB_Count_SetDefault, $01, DrawSetting_AEB_Count, 1
  X "AEB INTERVAL", Setting_AEB_Interval,       $01,  $01,  $20, Setting_AEB_Interval_Dec, Setting_AEB_Interval_Inc, Setting_AEB_Interval_SetDefault, $01, DrawSetting_AEB_Interval, 1
  X "PRINT ALL", NULL,                          $00,  $00,  $01, NullFunction, NullFunction, Init_PrintAll, $00, NullFunction, 0
  X "SAVE SETTINGS",NULL,                       $00,  $00,  $01, NullFunction, NullFunction, Init_SaveSettings, $00, NullFunction, 0
  X "SAVE CAM OPTIONS",NULL,                    $00,  $00,  $01, NullFunction, NullFunction, Init_SaveCamOpts, $00, NullFunction, 0
PURGE X