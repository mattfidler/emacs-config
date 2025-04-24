#!/bin/bash
sudo apt update
sudo apt full-upgrade -y
sudo apt-get install --assume-yes \
     apt-transport-https \
     autoconf \
     automake \
     bsd-mailx \
     build-essential \
     ca-certificates \
     ccache \
     chromium-browser  \
     clang \
     curl \
     dbus-x11 \
     debhelper \
     dpkg-dev \
     fonts-firacode \
     gawk \
     gcc \
     gfortran \
     gnupg \
     gnupg-agent \
     gnutls-bin \
     gnutls-dev \
     gvfs \
     heif-gdk-pixbuf \
     ibus-gtk3 \
     imagemagick \
     libacl1-dev \
     libaspell15 \
     libasyncns0 \
     libatk-bridge2.0-0t64 \
     libatk1.0-0t64 \
     libatspi2.0-0t64 \
     libblas-dev \
     libbrotli1 \
     libc6 \
     libc6-dev \
     libcairo-gobject2 \
     libcairo2 \
     libcanberra-gtk3-0t64 \
     libcanberra-gtk3-module \
     libcanberra0t64 \
     libclang-dev \
     libconfig-dev \
     libdatrie1 \
     libdb5.3t64 \
     libdbus-1-dev \
     libdrm2 \
     libegl1 \
     libepoxy0 \
     libfontconfig1 \
     libfreetype6 \
     libgbm1 \
     libgcc-s1 \
     libgccjit-13-dev \
     libgdk-pixbuf2.0-0 \
     libgif-dev \
     libgif7 \
     libgl1 \
     libglvnd0 \
     libglx0 \
     libgpm-dev \
     libgpm2 \
     libgraphite2-3 \
     libgstreamer-gl1.0-0 \
     libgstreamer-plugins-base1.0-0 \
     libgstreamer1.0-0 \
     libgtk-3-0 \
     libgtk-3-dev \
     libgudev-1.0-0 \
     libharfbuzz-dev \
     libharfbuzz-icu0 \
     libharfbuzz0b \
     libhyphen0 \
     libibus-1.0-5 \
     libice6 \
     libindicator7 \
     libjansson-dev\
     libjansson4 \
     libjbig0 \
     libjpeg-dev \
     libjpeg-turbo8 \
     liblapack-dev \
     liblcms2-2 \
     liblcms2-dev \
     liblockfile-dev \
     liblockfile1 \
     libltdl7 \
     libm17n-0 \
     libm17n-dev \
     libmagickwand-dev \
     libmpc3 \
     libmpfr6 \
     libncurses5-dev \
     libnotify4 \
     libnss-mdns \
     libnss-myhostname \
     libnss-systemd \
     libogg0 \
     liborc-0.4-0 \
     liboss4-salsa2 \
     libotf-dev \
     libpango-1.0-0 \
     libpangocairo-1.0-0 \
     libpangoft2-1.0-0 \
     libpixman-1-0 \
     libpng-dev \
     libpng16-16 \
     libpulse0 \
     librsvg2-2 \
     librsvg2-dev \
     librsvg2-dev \
     libsasl2-2 \
     libsecret-1-0 \
     libselinux1-dev \
     libsm6 \
     libsndfile1 \
     libsoup2.4-1 \
     libsqlite3-dev \
     libstdc++6 \
     libsystemd-dev \
     libtdb1 \
     libthai0 \
     libtiff-dev \
     libtree-sitter-dev \
     libvorbis0a \
     libvorbisenc2 \
     libvorbisfile3 \
     libwebpdemux2 \
     libwoff1 \
     libx11-6 \
     libx11-dev \
     libx11-xcb1 \
     libxau6 \
     libxcb-cursor0 \
     libxcb-render0 \
     libxcb-shm0 \
     libxcb1 \
     libxcomposite1 \
     libxcursor1 \
     libxdamage1 \
     libxdmcp6 \
     libxext6 \
     libxfixes3 \
     libxft-dev \
     libxi6 \
     libxinerama1 \
     libxkbcommon0 \
     libxml2 \
     libxml2-dev \
     libxpm-dev \
     libxpm4 \
     libxrandr2 \
     libxrender1 \
     libxslt1.1 \
     libxss1 \
     libxt-dev \
     libyajl2 \
     make \
     pandoc \
     procps \
     quilt \
     sharutils \
     software-properties-common \
     sqlite3 \
     texinfo \
     texlive-latex-base \
     valgrind \
     w3m \
     wget \
     xaw3dg-dev \
     zlib1g-dev


wget https://mirrors.ibiblio.org/gnu/emacs/emacs-30.1.tar.xz
tar -xvf emacs-30.1.tar.xz
cd emacs-30.1

./autogen.sh
./configure --without-compress-install --with-native-compilation  --with-mailutils
make
sudo make install

systemctl enable --user emacs
systemctl start --user emacs

cd ..

wget https://raw.githubusercontent.com/eddelbuettel/r2u/refs/heads/master/inst/scripts/add_cranapt_noble.sh

chmod +x add_cranapt_noble.sh

sudo ./add_cranapt_noble.sh

rm add_cranapt_noble.sh

sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | sudo gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg
NODE_MAJOR=22
echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_$NODE_MAJOR.x nodistro main" | sudo tee /etc/apt/sources.list.d/nodesource.list

sudo apt-get update
sudo apt-get install nodejs -y

wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install --assume-yes ./google-chrome*.deb

rm ./google-chrome*.deb

sudo apt-add-repository ppa:fish-shell/release-3
sudo apt update
sudo apt install fish --assume-yes

curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish > fisher.fish
echo fisher install jorgebucaran/fisher >> fisher.fish

fish fisher.fish

rm fisher.fish

fish -c 'fisher install mbollmann/emacs.fish'

git config --global user.email matthew.fidler@gmail.com
git config --global user.name "Matthew Fidler"
git config --global commit.gpgsign true
git config --global tag.gpgSign true
git config --global user.signingkey 8CB11DF7273ADB54

curl -sS https://starship.rs/install.sh > install.sh
chmod +x install.sh

sudo ./install.sh -y
rm install.sh

sudo apt-get install --assume-yes openssh-server

sudo systemctl start sshd

# Calibre
sudo -v && wget -nv -O- https://download.calibre-ebook.com/linux-installer.sh | sudo sh /dev/stdin


unzip FiraCode.zip -d "${HOME}/.local/share/fonts"
rm "${HOME}/.local/share/fonts/LICENSE"
rm "${HOME}/.local/share/fonts/README.md"

cp NFM.ttf "${HOME}/.local/share/fonts/"
cp Quivira.otf "${HOME}/.local/share/fonts/"

unzip symbola.zip -d "${HOME}/.local/share/fonts"
rm "${HOME}/.local/share/fonts/Symbola.pdf"
rm "${HOME}/.local/share/fonts/Symbola.docx"

unzip Noto_Sans_Symbols.zip -d "${HOME}/.local/share/fonts"

cp ${HOME}/.local/share/fonts/static/*.ttf "${HOME}/.local/share/fonts/"
rm -rfv "${HOME}/.local/share/fonts/static"
rm -rfv "${HOME}/.local/share/fonts/OFL.txt"
rm -rfv "${HOME}/.local/share/fonts/README.txt"

unzip Noto_Sans.zip -d "${HOME}/.local/share/fonts"
cp ${HOME}/.local/share/fonts/static/*.ttf "${HOME}/.local/share/fonts/"
rm -rfv "${HOME}/.local/share/fonts/static"
rm -rfv "${HOME}/.local/share/fonts/OFL.txt"
rm -rfv "${HOME}/.local/share/fonts/README.txt"

fc-cache -fv

curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher

mkdir ~/src

cd ~/src

R -e 'install.packages("devtools")'

R -e 'install.packages("tidyverse")'

git clone git@github.com:nlmixr2/dparser-R

R -e 'devtools::install_dev_deps("~/src/dparser-R")'

R -e 'install.packages("dparser")'

git clone git@github.com:nlmixr2/lotri

R -e 'devtools::install_dev_deps("~/src/lotri")'

R -e 'install.packages("lotri")'

git clone git@github.com:nlmixr2/rxode2ll

R -e 'devtools::install_dev_deps("~/src/rxode2ll")'

R -e 'install.packages("rxode2ll")'

git clone git@github.com:nlmixr2/rxode2

R -e 'devtools::install_dev_deps("~/src/rxode2")'

R -e 'install.packages("rxode2")'

git clone git@github.com:nlmixr2/nlmixr2est

R -e 'devtools::install_dev_deps("~/src/nlmixr2est")'

R -e 'install.packages("nlmixr2est")'

git clone git@github.com:nlmixr2/nlmixr2extra

R -e 'devtools::install_dev_deps("~/src/nlmixr2extra")'

R -e 'install.packages("nlmixr2extra")'

git clone git@github.com:nlmixr2/nlmixr2plot

R -e 'devtools::install_dev_deps("~/src/nlmixr2plot")'

R -e 'install.packages("nlmixr2plot")'

git clone git@github.com:nlmixr2/nlmixr2

R -e 'devtools::install_dev_deps("~/src/nlmixr2")'

R -e 'install.packages("nlmixr2")'

git clone git@github.com:nlmixr2/nonmem2rx

R -e 'devtools::install_dev_deps("~/src/nonmem2rx")'

R -e 'install.packages("nonmem2rx")'

git clone git@github.com:nlmixr2/monolix2rx

R -e 'devtools::install_dev_deps("~/src/monolix2rx")'

R -e 'install.packages("monolix2rx")'

git clone git@github.com:nlmixr2/babelmixr2

git clone git@github.com:ergoemacs/ergoemacs-mode

R -e 'devtools::install_dev_deps("~/src/babelmixr2")'

R -e 'install.packages("babelmixr2")'

cd "${HOME}/src/emacs-config"

mkdir "${HOME}/.R"

cp  Makevars "${HOME}/.R/Makevars"

mkdir -p "${HOME}/.ccache"

cp ccache "${HOME}/.ccache/ccache.conf"
