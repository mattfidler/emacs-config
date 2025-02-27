#!/bin/bash
sudo apt-get install update
sudo apt-get install upgrade
sudo apt-get install --assume-yes \
     libwebkit2gtk-4.1-dev build-essential autoconf make gcc libgnutls28-dev \
     libgccjit-11-dev libgccjit-12-dev libtiff5-dev libgif-dev libjpeg-dev \
     libpng-dev libxpm-dev libncurses-dev texinfo libgccjit0 \
     libgccjit-10-dev gcc-10 g++-10 sqlite3 \
     libconfig-dev libgtk-3-dev gnutls-bin libacl1-dev libotf-dev libxft-dev \
     libsystemd-dev libncurses5-dev libharfbuzz-dev imagemagick libmagickwand-dev \
     xaw3dg-dev libx11-dev libtree-sitter-dev automake bsd-mailx dbus-x11 debhelper \
     dpkg-dev libasound2-dev libdbus-1-dev libgpm-dev liblcms2-dev liblockfile-dev \
     libm17n-dev liboss4-salsa2 librsvg2-dev libselinux1-dev libtiff-dev libxml2-dev \
     libxt-dev procps quilt sharutils zlib1g-dev gvfs libasound2 libaspell15 \
     libasyncns0 libatk-bridge2.0-0 libatk1.0-0 libatspi2.0-0 libbrotli1 libc6 \
     libc6-dev libcairo-gobject2 libcairo2 libcanberra-gtk3-0 libcanberra-gtk3-module \
     libcanberra0 libdatrie1 libdb5.3 libdrm2 libegl1 libepoxy0 libflac8 \
     libfontconfig1 libfreetype6 libgbm1 libgcc-s1 libgdk-pixbuf2.0-0 libgif7 libgl1 \
     libglvnd0 libglx0 libgpm2 libgraphite2-3 libgstreamer-gl1.0-0 \
     libgstreamer-plugins-base1.0-0 libgstreamer1.0-0 libgtk-3-0 libgudev-1.0-0 \
     libharfbuzz-icu0 libharfbuzz0b libhyphen0 libice6 libjbig0 libjpeg-turbo8 \
     liblcms2-2 liblockfile1 libltdl7 libm17n-0 libmpc3 libmpfr6 libnotify4 \
     libnss-mdns libnss-myhostname libnss-systemd libogg0 liborc-0.4-0 libpango-1.0-0 \
     libpangocairo-1.0-0 libpangoft2-1.0-0 libpixman-1-0 libpng16-16 libpulse0 \
     librsvg2-2 libsasl2-2 libsecret-1-0 libsm6 libsndfile1 libsoup2.4-1 \
     libstdc++6 libtdb1 libthai0 libtiff5 libvorbis0a libvorbisenc2 libvorbisfile3 \
     libwebpdemux2 libwoff1 libx11-6 libx11-xcb1 libxau6 libxcb-render0 libxcb-shm0 \
     libxcb1 heif-gdk-pixbuf libxcomposite1 libxcursor1 libxdamage1 \
     gawk ibus-gtk3 libibus-1.0-5 libxdmcp6 libxext6 \
     libxfixes3 libxi6 libxinerama1 libxkbcommon0 libxml2 libxpm4 \
     libxrandr2 libxrender1 libxslt1.1 libyajl2 clang libclang-dev \
     fonts-firacode apt-transport-https ca-certificates curl \
     gnupg-agent gnupg software-properties-common libjansson4 libjansson-dev\
     libxss1 libappindicator1 libindicator7 \
     wget chromium-browser  \
     libblas-dev liblapack-dev gfortran \
     texlive-latex-base \
     libxcb-cursor0 \
     ccache valgrind pandoc w3m

wget https://ftp.gnu.org/gnu/emacs/emacs-29.4.tar.xz
tar -xvf emacs-29.4.tar.xz
cd emacs-29.4

./autogen.sh
./configure --without-compress-install --with-native-compilation --with-json --with-mailutils
make
sudo make install

# without sudo call
systemctl enable --user emacs
systemctl start --user emacs

cd ..

sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | sudo gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg
NODE_MAJOR=22
echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_$NODE_MAJOR.x nodistro main" | sudo tee /etc/apt/sources.list.d/nodesource.list

sudo apt-get update

wget https://raw.githubusercontent.com/eddelbuettel/r2u/refs/heads/master/inst/scripts/add_cranapt_jammy.sh

chmod +x add_cranapt_jammy.sh

sudo ./add_cranapt_jammy.sh

rm add_cranapt_jammy.sh

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



declare -a fonts=(BitstreamVeraSansMono
    CodeNewRoman
    DroidSansMono
    FiraCode
    FiraMono
    Go-Mono
    Hack
    Hermit
    JetBrainsMono
    Meslo
    Noto
    Overpass
    ProggyClean
    RobotoMono
    SourceCodePro
    SpaceMono
    Ubuntu
    UbuntuMono)

# version='2.1.0'
# fonts_dir="${HOME}/.local/share/fonts"

# if [[ ! -d "$fonts_dir" ]]; then
#     mkdir -p "$fonts_dir"
# fi

# for font in "${fonts[@]}"; do
#     zip_file="${font}.zip"
#     download_url="https://github.com/ryanoasis/nerd-fonts/releases/download/v${version}/${zip_file}"
#     echo "Downloading $download_url"
#     wget "$download_url"
#     unzip "$zip_file" -d "$fonts_dir"
#     rm "$zip_file"
# done

# find "$fonts_dir" -name '*Windows Compatible*' -delete

# fc-cache -fv

sudo apt-get remove --assume-yes firefox

# Add ssh server
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

R -e 'devtools::install_dev_deps("~/src/babelmixr2")'

R -e 'install.packages("babelmixr2")'


cd "${HOME}/src/emacs-config"

mkdir "${HOME}/.R"

cp  Makevars "${HOME}/.R/Makevars"

mkdir -p "${HOME}/.ccache"

cp ccache "${HOME}/.ccache/ccache.conf"
