#include <stdio.h>
#include <stdlib.h>

#define MAXLINE 1000

main()
{
	char line[MAXLINE];
	int res, i, val;

	res = 0;
	while (fgets(line, MAXLINE, stdin) != 0) {
		i = val =  0;
		printf("%s\n", line);
		while (line[i] != '\0') {
			if (line[i] >= '0' && line[i] <= '9') {
				if (val == 0) {
					val = (line[i]-'0')*10 + line[i]-'0';
				} else {
					val = (val/10)*10 + line[i]-'0';
				}
			}
			printf("%d, ", val);
			i++;
		}
		printf("\n");
		res += val;
	}
	printf("%d\n", res);
}
