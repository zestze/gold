#!/bin/sh


#MUST RUN MAKE BEFORE USING THIS SCRIPT

GOLD="./gold.native"
LLC="llc"
CC="gcc"

SignalError() {
    if [ $error -eq 0 ] ; then
        echo "FAILED"
        error=1
    fi
    echo "  $1"
}

# run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
        SignalError "$1 failed on $*"
        return 1
    }
}
if [ $# -ge 1 ]
then
    basename=`echo $1 | sed 's/\.[^.]*$//'`
    Run "$GOLD" "$1" ">" "${basename}.ll" &&
    Run "$LLC" "${basename}.ll" ">" "${basename}.s" &&
    Run "$CC" "-o" "${basename}.exe" "${basename}.s" 
    #Run "./${basename}.exe" > "${basename}.out"
fi


exit $globalerror

