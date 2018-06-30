#!/bin/sh

function usage
{
    echo "usage: $0 -p post-name | -h"
    echo ""
    echo "The post name has to be entered without extension"
    echo "Examples:"
    echo "$0 -p a-post-name"
}

postname=

while [ "$1" != "" ]; do
	case $1 in
		-p | --postname ) 	        shift
						postname=$1
						;;
		-h | --help )           	usage
						exit
						;;
		* )                     	usage
						exit 1
	esac
	shift
done

if [ "$postname" == "" ]; then
	usage
	exit 1
fi

../bin/hugo new post/$postname.md

