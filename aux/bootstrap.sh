#!/usr/bin/env sh

if ! command -v "c2ffi" &> /dev/null
then
    echo "ERROR! c2ffi not found in path, run \`git clone https://github.com/rpav/cl-autowrap.git\` and build"
    exit 1
fi

HEADERS=($(find src/sokol/sokol*.h -maxdepth 1 -type f))
if [ ${#HEADERS[@]} -eq 0 ]
then
    echo "ERROR! sokol headers not found, run \`git submodule update --init --recursive\`"
    exit 1
fi

for header in "${HEADERS[@]}"
do
    fname=$(basename $header)
    if [ "$fname" != "sokol_glue.h" ]
    then
        name=$(echo "$fname" | cut -d "." -f 1 | cut -d "_" -f 2)
        outdir="src/cl-sokol/$name"
        mkdir -p "$outdir"
        outpath="$outdir/sokol_$name.c"
        echo "#define SOKOL_IMPL" > $outpath
        echo "#include \"$fname\"" >> $outpath
        echo "$header -> $outpath"
    fi
done
