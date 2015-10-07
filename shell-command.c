#include <stdio.h>
#include <stdlib.h>

int shell_command(char *cmd)
{
	return system(cmd);
}
