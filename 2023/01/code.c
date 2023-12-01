#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 1000

main()
{
	char line[MAXLINE];
	int res, i, j, val, c;
	char *digits[9] = {
		"one",
		"two",
		"three",
		"four",
		"five",
		"six",
		"seven",
		"eight",
		"nine",
	};

	res = 0;
	while (fgets(line, MAXLINE, stdin) != 0) {
		i = val =  c = 0;
		while (line[i] != '\0') {
			if (isdigit(line[i]))
				c = line[i]-'0';
			else
				for (j = 0; j < 9; j++)
					if (strlen(digits[j]) < strlen(line+i))  // Remember the trailing \n
						if (memcmp(digits[j], line+i, strlen(digits[j])) == 0)
							c = j+1;
			if (val == 0)
				val = c*10 + c;
			else
				val = (val/10)*10 + c;
			i++;
		}
		res += val;
	}
	printf("%d\n", res);
}
