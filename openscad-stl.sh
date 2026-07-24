#!/usr/bin/env bash

# openscad-stl — export parts from a make_parts()-based .scad file.
#
#   openscad-stl.sh my.scad a          # exports my_a.stl
#   openscad-stl.sh my.scad --list     # prints part names, one per line
#   openscad-stl.sh my.scad --all      # exports my_<part>.stl for every part
#
set -euo pipefail

usage() {
    echo "usage: openscad-stl FILE.scad (PART | --list | --all)" >&2
    exit 1
}

[ $# -eq 2 ] || usage
file=$1
arg=$2
base=${file%.scad}

[ -f "$file" ] || { echo "openscad-stl: no such file: $file" >&2; exit 1; }

list_parts() {
    # The "echo" export format runs the script and emits only ECHO output.
    openscad -o /dev/stdout --export-format echo \
        -D 'output_part="__list__"' "$file" 2>/dev/null \
        | sed -n 's/^ECHO: "PART: \(.*\)"$/\1/p'
}

export_part() {
    local part=$1
    local out="${base}_${part}.stl"
    echo "-> $out"
    openscad -o "$out" -D "output_part=\"$part\"" "$file"
}

case "$arg" in
    --list)
        list_parts
        ;;
    --all)
        parts=$(list_parts)
        [ -n "$parts" ] || { echo "openscad-stl: no parts found (is make_parts used?)" >&2; exit 1; }
        while IFS= read -r p; do
            export_part "$p"
        done <<< "$parts"
        ;;
    --*)
        usage
        ;;
    *)
        export_part "$arg"
        ;;
esac
