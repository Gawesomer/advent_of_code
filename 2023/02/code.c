#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 2048

main()
{
	char line[MAXLINE];
	char *game, *draw, *color, *brkl, *brkg;
	int res1, res2, i, count, possible, max_red, max_green, max_blue;

	res1 = res2 = i = 0;
	while (fgets(line, MAXLINE, stdin) != 0) {
		printf("Game %d\n", ++i);
		max_red = max_blue = max_green = 0;
		possible = 1;
		for (game = strtok_r(line, ":", &brkl), game = strtok_r(NULL, ";\n", &brkl);
		     game;
		     game = strtok_r(NULL, ";\n", &brkl))
		{
			printf("game - %s\n", game);
			for (draw = strtok_r(game, ",", &brkg); draw; draw = strtok_r(NULL, ",", &brkg)) {
				color = strchr(draw+1, ' ');
				color[0] = '\0';
				color++;
				count = atoi(draw+1);
				printf("value - %d; color - %s\n", count, color);
				switch (color[0]) {
					case 'r':
						if (count > 12)
							possible = 0;
						if (count > max_red)
							max_red = count;
						break;
					case 'g':
						if (count > 13)
							possible = 0;
						if (count > max_green)
							max_green = count;
						break;
					case 'b':
						if (count > 14)
							possible = 0;
						if (count > max_blue)
							max_blue = count;
						break;
				}
			}
		}
		printf("possible - %d\n", possible);
		if (possible)
			res1 += i;
		res2 += max_red * max_green * max_blue;
	}
	printf("%d\n", res1);
	printf("%d\n", res2);
}
