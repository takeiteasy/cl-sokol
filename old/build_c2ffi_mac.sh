VERSION=llvm@14
HOMEBREW=/opt/homebrew
export CPPFLAGS="-I$HOMEBREW/$VERSION/include"
export LDFLAGS="-L$HOMEBREW/opt/$VERSION/lib"
export PATH="$HOMEBREW/opt/$VERSION/bin:$PATH"
cd deps/c2ffi
git apply ../mac_cxx_features.patch
mkdir build
cd build
LLVM_DIR=$HOMEBREW/opt/$VERSION/lib/cmake/llvm CC="clang" CXX="clang++" cmake -DCMAKE_PREFIX_PATH=$HOMEBREW/opt/$VERSION/lib/cmake/clang ..
make
cd ../../