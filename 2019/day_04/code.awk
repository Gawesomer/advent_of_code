#!/usr/bin/awk -f

function is_validA(n){
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

function is_validB(n){
    found = 0;
    num_same=0;
    prev=n%10;
    n=int(n/10);
    for (; n; n=int(n/10)){
        if (n%10>prev){
            return (0);
        } else if (n%10==prev) {
            num_same++;
        } else {
            if (num_same == 1) {
                found = 1;
            }
            num_same=0;
        }
        prev=n%10;
    }
    return (found||num_same==1);
}

BEGIN {
    min=147981;max=691423;
    for (i=min;i<=max;i++) {
        if (is_validA(i)) {
            validA[i];
        }
        if (is_validB(i)) {
            validB[i];
        }
    }
    print length(validA), length(validB);
}
