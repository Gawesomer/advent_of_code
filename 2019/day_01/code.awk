#!/usr/bin/awk -f

function f(m){return(int(m/3)-2)}
{x=f($1);A+=x;while(x>0){B+=x;x=f(x)}}
END{print A,B}
