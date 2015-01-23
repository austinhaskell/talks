/*
 * Note: This module is used for demo purposes.  As such, I've taken liberties
 * and elided proper error checking.  Be sure to include error checking in your
 * own projects!
 */
#include <errno.h>
#include <string.h>

struct day {
	unsigned int	month;
	unsigned int	day;
	unsigned int	year;
};

struct student {
	const char	*name;
	struct day	birthday;
};

static struct student students[] = {
	{
		.name		= "John Smith",
		.birthday	= {
			.month	= 11,
			.day	= 24,
			.year	= 1972,
		},
	},
	{
		.name		= "Josh Cartwright",
		.birthday	= {
			.month	= 12,
			.day	= 20,
			.year	= 1986,
		},
	},
	{
		.name		= "Jane Smith",
		.birthday	= {
			.month	= 4,
			.day	= 13,
			.year	= 1982,
		},
	},
	{
		.name		= "Andy Patterson",
		.birthday	= {
			.month	= 11,
			.day	= 14,
			.year	= 1992,
		},
	},
};

#define ARRAY_SIZE(a) (sizeof((a))/sizeof((a)[0]))

struct student *get_student(unsigned int id)
{
	return &students[id];
}

void update_student(unsigned int id, const struct student *s)
{
	struct student *ns = &students[id];
	ns->name = strdup(s->name);
	ns->birthday = s->birthday;
}
