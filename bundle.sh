#!/bin/bash

echo "This will delete and recreate the bundle folder. Press Ctrl-C to abort,
any other key to continue"
read

if [ -d ./bundle/cert ] ; then
  cp -r ./bundle/cert .
fi

rm -rf bundle
mkdir -p bundle

pushd client/vue/penandpaper
npm run build
popd
cp -r client/vue/penandpaper/dist ./bundle/html

pushd server/cpp/build
make -j$(nproc)
popd
cp server/cpp/build/penandpaper-server ./bundle/


if [ -d ./cert ] ; then
  mv ./cert ./bundle
else
  cd bundle
  mkdir cert
  cd cert
  openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout key.pem -out certificate.pem -batch
  openssl dhparam -out dh1024.pem 2048 
fi
