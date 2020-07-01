# Download and build srt

sudo apt-get update -q
sudo apt-get install tclsh pkg-config libssl-dev build-essential

git clone https://github.com/Haivision/srt.git

cd srt
./configure
make
sudo make install

# Fake Debian installation

sudo apt-get install equivs
equivs-control control
sed -i 's/^Package:.*$/Package: libsrt-dev/' control
equivs-build control
sudo dpkg -i libsrt-dev*.deb
