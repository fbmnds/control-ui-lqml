export PATH=$PATH:/opt/android/sdk/platform-tools

export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
export ANDROID_HOME=/opt/android/sdk
export ANDROID_NDK_ROOT=$ANDROID_HOME/ndk/android-ndk-r19c
export ANDROID_NDK_TOOLCHAIN=$ANDROID_NDK_ROOT/toolchains/llvm/prebuilt/linux-x86_64

export ECL_ANDROID=/home/dev/ecl/android/ecl-android
export ECL_ANDROID_32=/home/dev/ecl/android/32bit/ecl-android

alias qmake=/home/dev/Qt5.9.0/5.9/gcc_64/bin/qmake
alias qmake-android=/home/dev/Qt5.9.0/5.9/android_armv7/bin/qmake
alias qmake-android32='/home/dev/Qt5.9.0/5.9/android_armv7/bin/qmake "CONFIG+=32bit"'
alias rebuild32='touch ../app.asd && make clean && qmake-android32 .. && make apk && ./install-run.sh && ./log.sh'
alias qmlscene='~/Qt/5.15.2/gcc_64/bin/qmlscene'
alias rebuild32='touch ../app.asd && make clean && qmake-android32 .. && make apk && ./install-run.sh && ./log.sh'
