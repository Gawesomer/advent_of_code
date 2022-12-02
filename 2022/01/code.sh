#!/bin/bash

awk '
{
    	if ($0)
        	elf[i]+=$0
    	else {
		print elf[i]
		i++
	}
}
END {
	print elf[i]
}' $1 | sort -V | tail -n3 | awk '{sum+=$0}END{print sum}'
