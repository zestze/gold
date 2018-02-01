#!/bin/bash
# run.sh
# Author: Ezekiel Reyna

# Shell function for testing GOLD compiler against test functions

if [ $# -eq 0 ]; then
        target=hello
fi

if [ $# -eq 1 ]; then
        target=$1
fi

if [ $# -gt 1 ]; then
        printf "Usage: ./run.sh <target>\n"
        printf "<target> doesn't have .gold suffix.\n"
        printf "use --help for more info\n"
        exit
fi

if [ "$target" == "--help" ]; then
        printf "Usage: ./run.sh <target>\n"
        echo "--list lists possible <target>'s"
        printf "--all runs all functions in test folder\n"
        exit
fi

# flag for listing values that can be passed as
# first arg 'target'
if [ "$target" == "--list" ]; then
        ls test/*.gold \
                | cut -d '/' -f 2 \
                | cut -d '.' -f 1
        exit
fi

# Move to src dir
cd src

# test all functions.
if [ "$target" == "--all" ]; then
        # implement doofus.
        funcs="$(ls ../test \
                | grep .gold \
                | cut -d "." -f 1)"

        make clean >> /dev/null
        make >> /dev/null

        for func in $funcs
        do
                printf "######## $func ########\n" 
                ./compile.sh ../test/"$func".gold >> /dev/null 2&>1
                ../test/"$func".exe
        done

        make clean >> /dev/null
        cd ../test && ./clean.sh >> /dev/null
        exit
fi

# build
make clean >> /dev/null
make >> /dev/null

# compile
./compile.sh ../test/"$target".gold

# execute
../test/"$target".exe

# clean
make clean >> /dev/null
cd ../test && ./clean.sh >> /dev/null
