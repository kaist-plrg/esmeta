## Installing d8

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

Install Babel using npm
```
npm -g install @babel/cli@7.19.3 @babel/core@7.19.1 @babel/preset-env@7.19.1
```

## Installing JSC

1. Install xcode from app store and run at least once

2.
```console
$ xcode-select --install
```

3.
```console
$ git clone https://github.com/WebKit/WebKit.git WebKit
```

4.
$ sudo xcode-select -s /Applications/Xcode.app/Contents/Developer

5. Build JSC
$ {WEBKIT DIRECTORY}/Tools/Scripts/build-jsc --debug // --release도 가능
Build files are located at {WEBKIT DIRECTORY}/WebKitBuild

실행
$ {WEBKIT DIRECTORY}/Tools/Scripts/run-jsc
Running 1 time(s): DYLD_FRAMEWORK_PATH={WEBKIT DIRECTORY}/WebKitBuild/Debug {WEBKIT DIRECTORY}/WebKitBuild/Debug/jsc --useDollarVM=1
>>> 1+1
2
$ DYLD_FRAMEWORK_PATH={WEBKIT DIRECTORY}/WebKitBuild/Debug {WEBKIT DIRECTORY}/WebKitBuild/Debug/jsc -e "print(1);"
