# penandpaper
![CI-server](https://github.com/floriankramer/penandpaper/workflows/CI-server/badge.svg)
![CI-client](https://github.com/floriankramer/penandpaper/workflows/CI-client/badge.svg)


A web application for playing pen and paper games over the internet.

## Installation

### Ubuntu
These instructions are taken from the github actions workflow and as such
have been tested on ubuntu 18.04.
First we need to install dependencies. In a terminal enter
* `sudo apt install npm build-essentials libasio-dev libssl-dev libsqlite3-dev`

Open a terminal at the location you want to install the software. Then run
* `git clone https://github.com/floriankramer/penandpaper.git`
* `cd penandpaper`
* `git submodule update --init --recursive`
* `cd client/vue/penandpaper/`
* `npm install`
* `cp -r node_module_patches/* node_modules/`
* `npm run build`
* `cd ../../../server/cpp`
* `mkdir build`
* `cd build`
* `cmake -DCMAKE_BUILD_TYPE=Release ../src`
* `make -j$(nproc)`
* `mkdir html`
* `cp -r ../../../../client/vue/penandpaper/dist/* ./html`
* `mkdir cert`
* `cd cert`
* `openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout key.pem -out certificate.pem`
* `openssl dhparam -out dh1024.pem 1024 `

The build directory now contains a penandpaper executable that can be started.
Visit `https://localhost:8082/auth?key=<key>`. The key is printed out to
stdout and prevent random people from connecting to the service.
If you are using a self signed certificate make sure to visit
`https://localhost:8081` once in your browser and accept the certificate.
Otherwise the web socket connection won't be made.
