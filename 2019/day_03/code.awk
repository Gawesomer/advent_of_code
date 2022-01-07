#!/usr/bin/awk -f

BEGIN{FS=",";A=B=":"}{x=y=c=0;for(i=1;i<=NF;i++){o=substr($i,0,1);for(j=0;
j<substr($i,2)+0;j++){p=x "," y;if(NR==1){w[p]=c;}else if(c!=0&&p in w){d=(x>0?x:
-x)+(y>0?y:-y);d<A?A=d:0;c+w[p]<B?B=c+w[p]:0;}x+=o=="L"?-1:o=="R";y+=o=="D"?-1:
o=="U";c++;}}print A,B;}
