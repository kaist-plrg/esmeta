Installing d8

```
cd $HOME

#install depot_tools and update
mkdir -p bin
mkdir -p bin/v8
(cd bin/v8; git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git)
export PATH="$HOME/bin/v8/depot_tools:$PATH"

# checkout v8 directory
cd bin/v8
~/bin/v8/depot_tools/fetch v8
cd v8
git checkout main
git pull
~/bin/v8/depot_tools/gclient sync

# use gm to build v8 (and d8)
# python3 tools/dev/gm.py x64.release
gn gen out/d8 --args='v8_use_external_startup_data=false'
ninja -C out/d8 d8
```

Option to ignore unhandled rejected promises
```
node --unhandled-rejections=none
d8 --ignore-unhandled-promises
```
