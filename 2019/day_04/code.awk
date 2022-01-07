#!/usr/bin/awk -f

function is_valid(n){
    num_same=0;
    prev=n%10;
    n=int(n/10);
    for (; n; n=int(n/10)){
        if (n%10>prev){
            return (0);
        } else if (n%10==prev) {
            num_same++;
        }
        prev=n%10;
    }
    return (num_same);
}

BEGIN {
    min=147981;max=691423;count=0;
    for (i=min;i<=max;i++) {
        if (is_valid(i)) {
            print i;
            count++;
        }
    }
    print count;
}
