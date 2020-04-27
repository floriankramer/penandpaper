#!/bin/bash

# first, bundle the current version
./bundle.sh

rm -rf AppDir
mkdir AppDir
mkdir -p AppDir/usr/share/penandpaper/
linuxdeploy --appdir AppDir -e ./bundle/penandpaper-server -d ./dist/penandpaper.desktop -i ./dist/penandpaper.png --custom-apprun=./dist/AppRun
cp -r ./bundle/cert ./AppDir/usr/share/penandpaper/
cp -r ./bundle/html ./AppDir/usr/share/penandpaper/
ARCH=x86_64 appimagetool AppDir penandpaper.appimage
