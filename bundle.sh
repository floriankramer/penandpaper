#!/bin/bash

echo "This will delete and recreate the bundle folder. Press Ctrl-C to abort,
any other key to continue"
read

if [ -d ./bundle/cert ] ; then
  cp -r ./bundle/cert .
fi

rm -rf bundle
mkdir -p bundle

pushd client
npm run build
popd
cp -r client/dist ./bundle/html

mkdir -p server/build
pushd server/build
cmake ../src
make -j$(nproc)
popd
cp server/build/penandpaper-server ./bundle/


if [ -d ./cert ] ; then
  mv ./cert ./bundle
else
  cd bundle
  mkdir cert
  cd cert
  openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout key.pem -out certificate.pem -batch
  openssl dhparam -out dh1024.pem 2048
fi
