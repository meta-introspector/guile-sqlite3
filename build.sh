sudo apt install -y sqlite3 libsqlite3-dev
export PATH=/mnt/data1/nix/root/bin:$PATH
RT=/mnt/data1/nix/root/
RT2=/usr/local/
export PKG_CONFIG_LIBDIR=$RT/lib:$RT2/lib:/usr/lib/x86_64-linux-gnu/pkgconfig/
export PKG_CONFIG_PATH=${RT}lib/pkgconfig:${RT}lib64/pkgconfig/:${RT2}lib/pkgconfig:${RT2}lib64/pkgconfig/:/usr/lib/x86_64-linux-gnu/pkgconfig/
export ACLOCAL_PATH=${RT}/share/aclocal/:${RT2}/share/aclocal/

./configure  --prefix=/mnt/data1/nix/root/
make -j20
sudo make install
