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
  TODO: Slashes higher than \9 are not allowed, so later values won't be used.
  \10 Function to draw the value of the setting to the screen
  \11 Initial value

*/
  DEF SETTINGS_X_INDEX = 0
  X "SERIAL REMOTE", Setting_SerialRemote,      $01,  $00,  $01, Setting_SerialRemote_Toggle, Setting_SerialRemote_Toggle, Setting_SerialRemote_Toggle, $01, DrawSetting_SerialRemote, $01
  X "ON TAKE", Setting_OnTakeAction,            $01,  $00,  $03, Setting_OnTakeAction_Dec, Setting_OnTakeAction_Inc, Setting_OnTakeAction_SetDefault, $00, DrawSetting_OnTakeAction, 0
;  X "TIMER", Setting_TimerEnable,               $00,  $00,  $01, Setting_Timer_Toggle, Setting_Timer_Toggle, Setting_Timer_Toggle, $00, DrawSetting_TimerEnable
  X "PHOTO DELAY TIME", Setting_DelayTime,       $01,  $00,  $FF, Setting_DelayTime_Dec, Setting_DelayTime_Inc, Setting_DelayTime_SetDefault, $00, DrawSetting_DelayTime, 0
  X "MODE", Setting_Burst_AEB,                    $01,  $00,  $02, Setting_Burst_AEB_Dec,Setting_Burst_AEB_Inc,Setting_Burst_AEB_SetDefault, $00, DrawSetting_Burst_AEB, 0
  X "SHOT COUNT", Setting_AEB_Count,             $01,  $01,  $0F, Setting_AEB_Count_Dec, Setting_AEB_Count_Inc, Setting_AEB_Count_SetDefault, $01, DrawSetting_AEB_Count, 3
  X "AEB SHIFT", Setting_AEB_Interval,       $01,  $01,  $06, Setting_AEB_Interval_Dec, Setting_AEB_Interval_Inc, Setting_AEB_Interval_SetDefault, $01, DrawSetting_AEB_Interval, 3
  X "FAST PRINT", Setting_Print_Speed,        $01,  $00,  $01,  Setting_Print_Speed_Toggle, Setting_Print_Speed_Toggle, Setting_Print_Speed_Toggle, $00, DrawSetting_Print_Speed, $00
  X "DOUBLE SPEED", Setting_Double_Speed,     $01,  $00,  $01,  Setting_Double_Speed_Toggle, Setting_Double_Speed_Toggle, Setting_Double_Speed_Toggle, $00, DrawSetting_Double_Speed, $00
  ;X "PRINT/XFER RANGE MIN", Setting_Print_Min,  $01, $01, $1E, Setting
  X "PALETTE NUMBER", Setting_Palette_Scheme,   $01,  $01,  $02, Setting_Palette_Scheme_Dec, Setting_Palette_Scheme_Inc, Setting_Palette_Scheme_SetDefault, $01
  X "PRINT ALL TEST", NULL,                          $00,  $00,  $01, NullFunction, NullFunction, Init_PrintAll, $00, NullFunction, 0
  X "TRANSFER ALL", NULL,                       $00,  $00, $01, NullFunction, NullFunction, Init_TransferAll, $00, NullFunction, 0
  X "SAVE SETTINGS",NULL,                       $00,  $00,  $01, NullFunction, NullFunction, Init_SaveSettings, $00, NullFunction, 0
  X "SAVE CAM OPTIONS",NULL,                    $00,  $00,  $01, NullFunction, NullFunction, Init_SaveCamOpts, $00, NullFunction, 0
  X "DELETE ALL PHOTOS",NULL,                   $00,  $00, $01, NullFunction, NullFunction, Init_DeleteAll, $00
PURGE X