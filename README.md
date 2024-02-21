# Swap It Like It's Hot (Silihcam)
    A custom Game Boy Camera/Pocket Camera ROM for CGB/GBA that runs by cartswapping.

It can be used to manually modify registers that the original camera software could not, without needing to use a flash cart custom-built for the Game Boy Camera. If you have one of these custom carts, you'll probably want to use [Photo!](https://github.com/untoxa/gb-photo/) for more features and a better user experience.

Currently, it is missing several important features. Also, all the graphics are rotated to work with a GBA SP.

## Current Features
- Modify the camera registers and contrast setting
- Take a picture

## Planned Features
- Gallery to view and delete pictures
- Autoexposure
- Assisted mode similar to Photo!
- Right-side-up version

## Maybe-not-planned Features
- DMG support. This includes the original Game Boy / Pocket / Light / Super Game Boy 1 and 2. The DMG does not have enough RAM to store the entire payload, and we use CGB-exclusive features like DMA transfers and rotation. Though conceivably possible (and definitely possible for the SGB!), no support for these platforms are planned.

## Quickstart
Download the latest release from our [releases page](https://github.com/breademan/silih/releases/latest), flash it onto a flash cart, and run it on your Game Boy Color/Advance. 
When you see the screen that says "Press A to play," carefully remove the flash cart and insert your Game Boy Camera cart and press any button. 
If for some reason you want to run it in an emulator, some emulators do support both the Game Boy Camera AND yanking Game Paks while the game is running, such as BGB and Sameboy.

## Controls (subject to change because they're terrible)
- __A__: Change an option. Pressing B will let you exit the menu.
- __B__: Take a photo. Pressing B again will save the picture, and pressing A will discard it.

## Option values
The camera registers are best described in [the readme for Photo!](https://github.com/untoxa/gb-photo?tab=readme-ov-file#effect-of-the-main-adressable-parameters), in [AntonioND's Game Boy Camera Technical Information](https://github.com/AntonioND/gbcam-rev-engineer/blob/master/doc/gb_camera_doc_v1_1_1.pdf) document, and in the sensor datasheet.
- __M__: Selects the edge enhancement mode (0: None, 1: Horizontal, 2: Vertical, or 3: 2D) using camera registers N and VH.
- __C__: Exposure Time (in steps of 16μS). Determines how long the sensor collects light before being read out.
- __O__: Output reference voltage. This is essentially the same as V, but changes the output voltage bias by smaller steps. In practice, increasing this value leads to all pixels looking 'brighter' to the MAC-GBD chip.
- __G__: Gain. Higher values need less light but produce a noiser image. Lower values need more light but produce a cleaner image.
- __E__: Edge Enhancement ratio. The higher it is, the more filtering will be done to make edges stand out in the image.
- __V__: Output node bias voltage (Vref), in steps of 0.5V. The original software always sets this to 3 (1.5V). In practice, increasing this value leads to all pixels looking 'brighter' to the MAC-GBD chip. Changing this may set the voltage the sensor outputs outside of the range of the MAC-GBD's analog-to-digital converter.
- __Contrast__: Determines what values are used in the dither table and how close together the values are to each other.
- __Dither Table__: The ROM has 3 tables of base values used for dithering: 0 for low light conditions, 1 for high light conditions, and 2 unused.


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