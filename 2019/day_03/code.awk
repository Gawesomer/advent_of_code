#!/usr/bin/awk -f

function dist(x,y){return((x>0?x:-x)+(y>0?y:-y))}
BEGIN{FS=","}
{
    x=0;y=0;
    for(i=1;i<=NF;i++){
        dir=substr($i,0,1);
        delta=substr($i,2)+0;
        x_vel=0;y_vel=0;
        if (dir == "R"){
            x_vel=1;
        } else if (dir == "L") {
            x_vel=-1;
        } else if (dir == "U") {
            y_vel=1;
        } else if (dir == "D") {
            y_vel=-1;
        }
        for (j=0;j<delta;j++){
            if (NR == 1) {
                wire[x "," y];
            } else if (!(x==0 && y==0)) {
                if (x "," y in wire) {
                    if (!min_dist) {
                        min_dist=dist(x,y);
                        min_int=x "," y;
                    } else if (dist(x,y) < min_dist) {
                        min_dist=dist(x,y);
                        min_int=x "," y;
                    }
                }
            }
            x+=x_vel;
            y+=y_vel;
        }
    }
    print min_dist, min_int;
}
