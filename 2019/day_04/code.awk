#!/usr/bin/awk -f

function v(n){a=b=f=s=0;p=":";for(;n;n=int(n/10)){if(n%10>p)return 0;else
if(n%10==p)a=++s;else{if(s==1)f=1;s=0}p=n%10}b=f||s==1;return a}BEGIN{
for(i=147981;i<=691423;i++){A+=v(i)!=0;B+=b}print A,B}
