# Swap It Like It's Hot (Silihcam)
    A custom Game Boy Camera/Pocket Camera ROM for CGB/GBA/GBASP that runs by cartswapping.

It can be used to manually modify registers that the original camera software could not, without needing to use a flash cart custom-built for the Game Boy Camera. If you have one of these custom carts, you'll probably want to use [Photo!](https://github.com/untoxa/gb-photo/) for more features and a better user experience.

Currently, it is missing several important features.

## Current Features
- Modify the camera registers and contrast setting
- Upside-down down version for systems like the GBA SP
- Remote control over the link cable
- Burst shot and AEB (Automatic Exposure Bracketing) modes
- Quickly switch between the original ROM and Silihcam with the press of two buttons
- Additional dithering patterns on top of the standard Bayer dithering
- Compatability with the [Pico Game Boy Printer](https://github.com/untoxa/pico-gb-printer) for transferring photos and [Pico Game Boy Web Camera](https://github.com/untoxa/pico-gb-webcamera/) for streaming/recording video

## Planned Features
- Printing support
- Gallery to view and delete pictures
- Autoexposure
- Assisted mode similar to Photo!

## Maybe-not-planned Features
- DMG support. This includes the original Game Boy / Pocket / Light / Super Game Boy 1 and 2. The DMG does not have enough RAM to store the entire payload, and we use CGB-exclusive features like DMA transfers and rotation. Though conceivably possible (and definitely possible for the SGB!), no support for these platforms is planned.

## Quickstart
Download the appropriate release from our [releases page](https://github.com/breademan/silih/releases/latest), flash it onto a flash cart, and run it on your Game Boy Color/Advance/Game Boy Advance SP.
When you see the screen that says "Press A to play," carefully remove the flash cart, insert your Game Boy Camera cart, and press any button. 
If for some reason you want to run it in an emulator, only some emulators support both the Game Boy Camera AND yanking Game Paks while the game is running. BGB or Sameboy are recommended.

## Controls
- __A__: Take a photo. In single-shot mode, pressing A again will save the picture, and pressing B will discard it.
- __Select+Up__: Switch to stock software stored on the ROM, allowing you to access the gallery, take a photo with the stock software's autoexposure, or print (printing is currently untested, but should work). You can return to Silihcam by holding SELECT+DOWN. 
- __Start__: Open the settings menu.

## Camera Option values
The camera registers are best described in [the readme for Photo!](https://github.com/untoxa/gb-photo?tab=readme-ov-file#effect-of-the-main-adressable-parameters), in [AntonioND's Game Boy Camera Technical Information](https://github.com/AntonioND/gbcam-rev-engineer/blob/master/doc/gb_camera_doc_v1_1_1.pdf) document, and in the sensor datasheet.
- __M__: Selects the edge enhancement mode (0: None, 1: Horizontal, 2: Vertical, or 3: 2D) using camera registers N and VH.
- __C__: Exposure Time (in steps of 16μS). Determines how long the sensor collects light before being read out.
- __O__: Output reference voltage. This is essentially the same as V, but changes the output voltage bias by smaller steps. In practice, increasing this value leads to all pixels looking 'brighter' to the MAC-GBD chip.
- __G__: Gain. Higher values need less light but produce a noiser image. Lower values need more light but produce a cleaner image.
- __E__: Edge Enhancement ratio. The higher it is, the more filtering will be done to make edges stand out in the image.
- __V__: Output node bias voltage (Vref), in steps of 0.5V. The original software always sets this to 3 (1.5V). In practice, increasing this value leads to all pixels looking 'brighter' to the MAC-GBD chip. Changing this may set the voltage the sensor outputs outside of the range of the MAC-GBD's analog-to-digital converter.
- __Contrast__: Determines what values are used in the dither table and how close together the values are to each other.
- __Dither Table__: The ROM has 3 tables of base values used for dithering: 0 for low light conditions, 1 for high light conditions, and 2 unused.
- __Dither Pattern__: Determines how the values generated from Contrast and Dither Table options are arranged into a 4px-by-4px matrix used for quantizing the analog sensor readings into 2-bit greyscale. In order, the patterns are: optimized Bayer matrix, no-dithering, checkerboard, vertical lines, horizontal lines, and two different variations on diagonal lines.

## Settings Menu
Most settings are currently unimplemented.
- __Serial Remote__: If checked, SILIH will listen for remote control button presses over the link port.
- __Mode__: Single shot, burst shot, or AEB (Automatic Exposure Bracketing). AEB mode will take an equal number of overexposed and underexposed shots for higher dynamic range.
- __Shot Count__: Number of photos to take in Burst or AEB mode. For AEB mode, this is restricted to odd numbers.
- __AEB Shift__: In AEB mode, affects the amount added or subtracted to get the exposure time (C) of the next shot. A higher AEB shift means a smaller AEB step.  
1 = ±50%, 2 = ±25%, 3 = ±12.5%, 4 = ±6.25%, 5 = ±3.125%.
- __Fast Print__: When transferring photos, enables CGB-only fast serial transfers. The RP2040-based devices are known to support this speed, but real Pocket Printers won't. Other printer emulators are unlikely to work at this speed.
- __Double Speed__: Enables the CGB-only double-speed CPU mode. Improves framerate and serial transfer speed, but consumes more power. Exposure time must be doubled to compensate. The Pocket Camera doesn't officially support this speed, and photos seem to be 'noisier' than an equivalent photo taken at single-speed. May be useful in very bright scenes to give finer control of exposure time.
- __Palette Number__: Determines the palette used to preview photos.
- __Print All Test__: Prints all active photos without fast printing. Currently broken, but may print a few pictures before breaking.
- __Transfer All__: Transfers all active photos using the Transfer protocol supported by the RP2040-based projects.
- __Webcam Mode__: Transfers each seen frame on the viewfinder over the link cable via the Transfer protocol. Using with Fast Print is highly recommended.
- __Delete All Photos__: Frees all photo slots. The image data is still available on the cart until overwritten.
- All other settings are non-functional.

## Remote Controller
Silihcam can be controlled remotely by another Game Boy over the link cable port. This is useful for taking stabilized shots.  
When the Silihcam cart is run on a device that is not CGB-compatible, pressing a button will instead launch the remote controller. It can also be launched on a CGB-compatible device by pressing a button in the launcher without cartswapping.

## Building on Linux / macOS
1. Install [RGBDS](https://rgbds.gbdev.io/install) 0.6.1 and GNU Make (most systems already have this installed)  
2. Download or git clone this repository  
3. Navigate to the root folder (the one that has a file called Makefile in it)  
4. Run "make". The generated .gbc file can be found in the bin folder, along with a .sym file for debugging.  
To recompile, run "make clean" before running make.


## Acknowledgements
- The contributors to the [GBDev Pan Docs](https://gbdev.io/pandocs/).
- Toxa for help and for developing [Photo!](https://github.com/untoxa/gb-photo/).
- HerrZatacke for help, developing [2bit PXLR Studio](https://github.com/HerrZatacke/2bit-pxlr-studio), and for the [extensive dithering documentation](https://github.com/HerrZatacke/dither-pattern-gen/).
- Lorenzooone, for [force-gb-mode](https://github.com/Lorenzooone/force-gb-mode), a substantial portion of which is used as a loader for the payload.
- [damieng](https://damieng.com/typography/zx-origins/) for the 8-bit font.
- All of the members of the Game Boy development community and the Game Boy Camera Club Discord server.