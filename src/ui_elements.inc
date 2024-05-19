; ui_elements.inc

;X macro provided by Sylvie (rangi42) on the GBDev Discord
;The order in which these values are defined here are also the order in which they are shown and modified in the UI
;It does not define their layout in memory relative to each other -- the variables are still manually declared
/*Format: X 
  \1 label of variable
  \2 icon index within viewfinderUI.1bpp 
  \3 number of nybbles - 1
  \4 min value
  \5 max value
  \6 label of UIhandler for changing the variable
  \7 function to update the options buffer for this value
*/
  X CamOptC_RAM,          $0c,  $03, $00, $FF, ModifyCamOptC_UI
  X CamOptG_RAM,          $12,  $01, $00, $1F, ModifyCamOptG_UI
  X CamOptO_RAM,          $00,  $01, $20, $3F, ModifyCamOptO_UI
  X CamOptV_RAM,          $11,  $00, $00, $07, ModifyCamOptV_UI
;  X CamOptVH_RAM,         $13,  $00, $00, $03, ModifyCamOptVH_UI ;this is meant to be blank at the moment
;Temporary debugging entry for serial data
  X RemoteJoypadActive,   $13,  $01, $00, $FF, ModifyCamOptVH_UI
  X CamOptEdgeMode,       $10,  $00, $00, $03, ModifyEdgeMode_UI 
  X CamOptE_RAM,          $0E,  $00, $00, $07, ModifyCamOptE_UI
  X CamOptContrast,       $15,  $00, $00, $0F, ModifyContrast_UI
  X CamOptDitherTable,    $16,  $00, $00, $02, ModifyDitherTable_UI
  X CamOptDitherPattern,  $1F,  $00, $00, $01, ModifyDitherPattern_UI
PURGE X


/*X CamOptEdgeMode,       $10,  $00, $00, $03, ModifyEdgeMode_UI 
X CamOptVH_RAM,         $13,  $00, $00, $03, ModifyCamOptVH_UI
X CamOptC_RAM,          $0c,  $03, $00, $FF, ModifyCamOptC_UI
X CamOptO_RAM,          $00,  $01, $20, $3F, ModifyCamOptO_UI
X CamOptG_RAM,          $12,  $01, $00, $1F, ModifyCamOptG_UI
X CamOptE_RAM,          $0E,  $00, $00, $07, ModifyCamOptE_UI
X CamOptV_RAM,          $11,  $00, $00, $07, ModifyCamOptV_UI
X CamOptContrast,       $15,  $00, $00, $0F, ModifyContrast_UI
X CamOptDitherTable,    $16,  $00, $00, $02, ModifyDitherTable_UI
X CamOptDitherPattern,  $1F,  $00, $00, $01, ModifyDitherPattern_UI*/