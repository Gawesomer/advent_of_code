#!/usr/bin/awk -f

function f(c,n,v){split(c,a,",");a[2]=n;a[3]=v;for(p=1;a[p]!=99;p+=4){
        a[a[p+3]+1]=a[p]==1?a[a[p+1]+1]+a[a[p+2]+1]:a[a[p+1]+1]*a[a[p+2]+1];
    }
    return(a[1])
}
{print f($0,12,2);
    for(n=0;n<100;n++){
        for(v=0;v<100;v++){
            if(f($0,n,v)==19690720){
                print 100*n+v;
                exit
            }
        }
    }
}
