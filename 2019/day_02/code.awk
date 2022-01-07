#!/usr/bin/awk -f

{n=split($0,a,",");
    a[2]=12;a[3]=2;
    for(p=1;a[p]!=99;p+=4){
        if(a[p]==1){
            a[a[p+3]+1]=a[a[p+1]+1]+a[a[p+2]+1];
        }else{
            a[a[p+3]+1]=a[a[p+1]+1]*a[a[p+2]+1];
        }
    }
    print a[1]}
