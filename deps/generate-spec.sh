declare -a arr=(
    aarch64-pc-linux-gnu
    aarch64-unknown-linux-android
    arm-pc-linux-gnu
    arm-unknown-linux-androideabi
    i386-unknown-freebsd
    i386-unknown-openbsd
    i686-apple-darwin9
    i686-pc-linux-gnu
    i686-pc-windows-msvc
    i686-unknown-linux-android
    x86_64-apple-darwin9
    x86_64-pc-linux-gnu
    x86_64-pc-windows-msvc
    x86_64-unknown-freebsd
    x86_64-unknown-linux-android
    x86_64-unknown-openbsd)

for i in "${arr[@]}"
do
    c2ffi -Ideps/sokol/ ./deps/sokol.h -A "$i" > "./spec/sokol.$i.spec"
done
