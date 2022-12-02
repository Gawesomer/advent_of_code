#!/usr/bin/awk -f

{
    if ($0)
        curr+=$0
    else {
        max=(max<curr)?curr:max
        curr=0
    }
}
END {
    print max
}
