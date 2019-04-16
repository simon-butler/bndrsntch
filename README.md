
# BNDRSNTCH

![BNDRSNTCH logo](/img/bndrsntch-logo.png)

**BNDRSNTCH** is a fan made game attempting to finish the Spectrum ZX game Stefan is making in _Black Mirror: Bandersnatch_, except for Commodore 64 platform.

So far it is a clone of what we see Stefan has made, with an additional map view.

![BNDRSNTCH logo](/img/bndrsntch-scr-all.png)

## Screenshots

Note that these are carefully constructed to not give too much away!

![BNDRSNTCH screenshot 1](/img/bndrsntch-scr-1.png)

![BNDRSNTCH screenshot 2](/img/bndrsntch-scr-2.png)

![BNDRSNTCH screenshot 3](/img/bndrsntch-scr-3.png)

![BNDRSNTCH screenshot 4](/img/bndrsntch-scr-4.png)

![BNDRSNTCH screenshot 5](/img/bndrsntch-scr-5.png)

## Code template used and thanks

This project forked from [acme-assembly-vscode-template](https://github.com/Esshahn/acme-assembly-vscode-template), an excellent template for creating 6503 assembly code programs quickly and easily on any of the main platforms. I won't repeat the beautifully detailed general instructions here, check it out directly for that. Here you'll just find the minimum to understand this project.

## Running and playing the game

You can run the latest build of BNDERSNTCH on any supporting emulator using the [main.prog](/build/main.prg) file. Just download it and run it on your favourite emulator.

When we get to some kind of coherent demo stage I'll create a simple way to play it online without any downloading but for now we're in alpha.

## Build set up

_Copied and modified from original README at acme-assembly-vscode-template_

You will need the following:

1. Visual Studio Code
2. Acme Cross Assembler extension for VSCode
3. VICE Emulator
4. A download / clone / fork of this project

### 1. Visual Studio Code

First, get [VSCode](https://code.visualstudio.com/).

### 2. Acme Cross Assembler extension for VSCode

Now let's add an extension for syntax highlighting. Click on the extension icon, enter '__acme__' in the search bar and click on the '__Acme Cross Aseembler__' extension. There's an install button on the right side where the extension is previewed.
Unfortunately, the extension does not have any compiler functionality, so we will have to deal with that in another way.

![acme-install](https://user-images.githubusercontent.com/434355/50896183-9692ae80-1408-11e9-8a2c-cbad1e925515.jpg)

![acme-preview](https://user-images.githubusercontent.com/434355/50896180-9692ae80-1408-11e9-9b50-484d9b088591.jpg)

### 3. VICE Emulator

Get it here: http://vice-emu.sourceforge.net/index.html#download

### 4. A download / clone / fork of this project

Save it anywhere you like.

## Building

First open the VSCode project file for this project (or just open the root directory in VSCode, it's the same).

### Configuring

You should open the file `/.vscode/tasks.json` and and inspect the paths. I have it set up with the VICE emulator installed at location `~/vice/` for Mac / Linux or `C:\tools\vice\` on Windows, you'll probably need to change this part.

### To build

Select menu item _Terminal_ > _Run Build Task..._, or use the shortcut CMD + SHIFT + B (at least that's it on Mac)

### Background on building

All binaries needed for compiling (the ACME assembler, the Pucrunch & Exomizer packers) are included in the ````bin```` folder. Code purists might wave their fist in agony now (as it is usually not advised to check in binaries in git), but I found this to be the most comfortable way for newbies to not get lost in configuration. There are folders for Mac, Windows and Linux binaries.

Again, for more general information please go to the [acme-assembly-vscode-template](https://github.com/Esshahn/acme-assembly-vscode-template) repo.

## License

The original repo is unlicenced so it's hard to know what to assume. I have chosen CC0 license here but I will change if the original author disagrees.

## Changelog

* v0.1.0 - create repo, code contains x,y plotting test only :D