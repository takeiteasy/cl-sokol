#!/usr/bin/env sh

RETURNS=($(c2ffi "$1" -D json 2>/dev/null | jq -r -c '.[] | select(."return-type" != null) | ."return-type"'))

print_type() {
    TAG=$(echo "$1" | jq -r '.tag')
    case "$TAG" in
        ":pointer")
            SUBTYPE=$(echo "$1" | jq -c -r '.type')
            #printf "*"
            print_type "$SUBTYPE"
            ;;
        ":array")
            #printf "[]"
            SUBTYPE=$(echo "$1" | jq -c -r '.type')
            print_type "$SUBTYPE"
            ;;
        *)
            echo "$TAG"
            ;;
    esac
}

for R in ${RETURNS[@]}
do
    print_type "$R"
done

PARAMS=($(c2ffi "$1" -D json 2>/dev/null | jq -r -c '.[] | select(.parameters != null and .parameters != []) | .parameters'))

for P in ${PARAMS[@]}
do
    for PP in $(jq -c -r '.[]' <<< "$P")
    do
        PPP=$(echo "$PP" | jq -c -r '.type')
        print_type "$PPP"
    done
done

FIELDS=($(c2ffi "$1" -D json 2>/dev/null | jq -r -c '.[] | select(.tag == "struct")'))

for F in ${FIELDS[@]}
do
    NAME=$(echo "$F" | jq -r '.name')
    if [[ "$NAME" != _* ]]
    then
        FFF=$(echo "$F" | jq -c -r '.fields')
        for FF in $(jq -r -c '.[]' <<< "$FFF")
        do
            FFFF=$(echo "$FF" | jq -c -r '.type')
            print_type "$FFFF"
        done
    fi
done
