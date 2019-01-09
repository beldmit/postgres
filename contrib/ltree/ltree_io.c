/*
 * in/out function for ltree and lquery
 * Teodor Sigaev <teodor@stack.net>
 * contrib/ltree/ltree_io.c
 */
#include "postgres.h"

#include <ctype.h>

#include "ltree.h"
#include "utils/memutils.h"
#include "crc32.h"

PG_FUNCTION_INFO_V1(ltree_in);
PG_FUNCTION_INFO_V1(ltree_out);
PG_FUNCTION_INFO_V1(lquery_in);
PG_FUNCTION_INFO_V1(lquery_out);


#define UNCHAR ereport(ERROR, \
					   (errcode(ERRCODE_SYNTAX_ERROR), \
						errmsg("syntax error at position %d", \
						pos)));


typedef struct
{
	char	   *start;
	int			len;			/* length in bytes */
	int			flag;
	int			wlen;			/* length in characters */
} nodeitem;

#define LTPRS_WAITNAME	0
#define LTPRS_WAITDELIM 1
#define LTPRS_WAITESCAPED 2

static void
count_lquery_parts_ors(const char *ptr, int *plevels, int *pORs)
{
	int escape_mode = 0;
	int charlen;

	while (*ptr)
	{
		charlen = pg_mblen(ptr);

		if (escape_mode == 1)
			escape_mode = 0;
		else if (charlen == 1)
		{
			if (t_iseq(ptr, '\\'))
				escape_mode = 1;
			else if (t_iseq(ptr, '.'))
				(*plevels)++;
			else if (t_iseq(ptr, '|') && pORs != NULL)
				(*pORs)++;
		}

		ptr += charlen;
	}

	(*plevels)++;
	if (pORs != NULL)
		(*pORs)++;
}

static void
copy_unescaped(const char *src, char *dst, int max_len)
{
	uint16 copied = 0;
	int charlen;
	int escaping = 0;

	while (*src && copied < max_len)
	{
		charlen = pg_mblen(src);
		if ((charlen == 1) && t_iseq(src, '\\') && escaping == 0) {
			escaping = 1;
			src++;
			continue;
		};

		if (copied + charlen > max_len)
			elog(ERROR, "internal error during splitting levels");

		memcpy(dst, src, charlen);
		src += charlen;
		dst += charlen;
		copied += charlen;
		escaping = 0;
	}
}

static int
copy_escaped(const char *src, char *dst, int len, const char *to_escape)
{
	uint16 copied = 0;
	int charlen;
	int escapes = 0;
	char *buf = dst;

	while (*src && copied < len)
	{
		charlen = pg_mblen(src);
		if ((charlen == 1) && strchr(to_escape, *src))
		{
			*buf = '\\';
			buf++;
			escapes++;
		};

		if (copied + charlen > len)
			elog(ERROR, "internal error during merging levels");

		memcpy(buf, src, charlen);
		src += charlen;
		buf += charlen;
		copied += charlen;
	}
	return escapes;
}

Datum
ltree_in(PG_FUNCTION_ARGS)
{
	char	   *buf = (char *) PG_GETARG_POINTER(0);
	char	   *ptr;
	nodeitem   *list,
						 *lptr;
	int			levels = 0,
					totallen = 0;
	int			state = LTPRS_WAITNAME;
	ltree	   *result;
	ltree_level *curlevel;
	int			charlen;
	/* Position in strings, in symbols. */
	int			pos = 0;
	int     escaped_count = 0;

	ptr = buf;
	count_lquery_parts_ors(ptr, &levels, NULL);

	if (levels > MaxAllocSize / sizeof(nodeitem))
		ereport(ERROR,
				(errcode(ERRCODE_PROGRAM_LIMIT_EXCEEDED),
				 errmsg("number of levels (%d) exceeds the maximum allowed (%d)",
					 levels, (int) (MaxAllocSize / sizeof(nodeitem)))));
	list = lptr = (nodeitem *) palloc(sizeof(nodeitem) * (levels));
	/*
	 * This block calculates single nodes' settings
	 * */
	ptr = buf;
	while (*ptr)
	{
		charlen = pg_mblen(ptr);
		if (state == LTPRS_WAITNAME)
		{
			state = LTPRS_WAITDELIM;
			lptr->start = ptr;
			lptr->wlen = 0;
			escaped_count = 0;

			if (charlen == 1) {
				if (t_iseq(ptr, '.')) {
					UNCHAR;
				}
				else if (t_iseq(ptr, '\\')) {
					state = LTPRS_WAITESCAPED;
				}
			}
		}
		else if (state == LTPRS_WAITESCAPED)
		{
			state = LTPRS_WAITDELIM;
			escaped_count++;
		}
		else if (state == LTPRS_WAITDELIM)
		{
			if (charlen == 1 && t_iseq(ptr, '.'))
			{
				lptr->len = ptr - lptr->start - escaped_count;
				if (lptr->wlen > 255)
					ereport(ERROR,
							(errcode(ERRCODE_NAME_TOO_LONG),
							 errmsg("name of level is too long"),
							 errdetail("Name length is %d, must "
								 "be < 256, in position %d.",
								 lptr->wlen, pos)));

				totallen += MAXALIGN(lptr->len + LEVEL_HDRSIZE);
				lptr++;
				state = LTPRS_WAITNAME;
			}
			else if (charlen == 1 && t_iseq(ptr, '\\'))
			{
				state = LTPRS_WAITESCAPED;
			}
		}
		else
			/* internal error */
			elog(ERROR, "internal error in parser");
		ptr += charlen;
		if (state == LTPRS_WAITDELIM)
			lptr->wlen++;
		pos++;
	}

	if (state == LTPRS_WAITDELIM)
	{
		lptr->len = ptr - lptr->start - escaped_count;
		if (lptr->wlen > 255)
			ereport(ERROR,
					(errcode(ERRCODE_NAME_TOO_LONG),
					 errmsg("name of level is too long"),
					 errdetail("Name length is %d, must "
						 "be < 256, in position %d.",
						 lptr->wlen, pos)));

		totallen += MAXALIGN(lptr->len + LEVEL_HDRSIZE);
		lptr++;
	}
	else if (!(state == LTPRS_WAITNAME && lptr == list)) /* Empty string */
		ereport(ERROR,
				(errcode(ERRCODE_SYNTAX_ERROR),
				 errmsg("syntax error"),
				 errdetail("Unexpected end of line.")));

	result = (ltree *) palloc0(LTREE_HDRSIZE + totallen);
	SET_VARSIZE(result, LTREE_HDRSIZE + totallen);
	result->numlevel = lptr - list;
	curlevel = LTREE_FIRST(result);
	lptr = list;
	while (lptr - list < result->numlevel)
	{
		curlevel->len = (uint16) lptr->len;
		if (lptr->len > 0)
		{
			copy_unescaped(lptr->start, curlevel->name, lptr->len);
		}
		curlevel = LEVEL_NEXT(curlevel);
		lptr++;
	}

	pfree(list);
	PG_RETURN_POINTER(result);
}

Datum
ltree_out(PG_FUNCTION_ARGS)
{
	ltree	   *in = PG_GETARG_LTREE_P(0);
	char	   *buf,
			   *ptr;
	int			i;
	ltree_level *curlevel;

	ptr = buf = (char *) palloc(VARSIZE(in));
	curlevel = LTREE_FIRST(in);
	for (i = 0; i < in->numlevel; i++)
	{
		if (i != 0)
		{
			*ptr = '.';
			ptr++;
		}
		if (curlevel->len > 0) {
			int escapes = copy_escaped(curlevel->name, ptr, curlevel->len, "\\ .");
			ptr += curlevel->len;
			ptr += escapes;
		}
		curlevel = LEVEL_NEXT(curlevel);
	}

	*ptr = '\0';
	PG_FREE_IF_COPY(in, 0);

	PG_RETURN_POINTER(buf);
}

#define LQPRS_WAITLEVEL 0
#define LQPRS_WAITDELIM 1
#define LQPRS_WAITOPEN	2
#define LQPRS_WAITFNUM	3
#define LQPRS_WAITSNUM	4
#define LQPRS_WAITND	5
#define LQPRS_WAITCLOSE 6
#define LQPRS_WAITEND	7
#define LQPRS_WAITVAR	8
#define LQPRS_WAITESCAPED 9


#define GETVAR(x) ( *((nodeitem**)LQL_FIRST(x)) )
#define ITEMSIZE	MAXALIGN(LQL_HDRSIZE+sizeof(nodeitem*))
#define NEXTLEV(x) ( (lquery_level*)( ((char*)(x)) + ITEMSIZE) )

Datum
lquery_in(PG_FUNCTION_ARGS)
{
	char	   *buf = (char *) PG_GETARG_POINTER(0);
	char	   *ptr;
	int			levels = 0,
				totallen = 0,
				numOR = 0;
	int			state = LQPRS_WAITLEVEL;
	lquery	   *result;
	nodeitem   *lptr = NULL;
	lquery_level *cur,
			   *curqlevel,
			   *tmpql;
	lquery_variant *lrptr = NULL;
	bool		hasnot = false;
	bool		wasbad = false;
	int			charlen;
	int			pos = 0;
	int			escaped_count = 0;

	ptr = buf;
	count_lquery_parts_ors(ptr, &levels, &numOR);

	if (levels > MaxAllocSize / ITEMSIZE)
		ereport(ERROR,
				(errcode(ERRCODE_PROGRAM_LIMIT_EXCEEDED),
				 errmsg("number of levels (%d) exceeds the maximum allowed (%d)",
						levels, (int) (MaxAllocSize / ITEMSIZE))));
	curqlevel = tmpql = (lquery_level *) palloc0(ITEMSIZE * levels);
	ptr = buf;
	while (*ptr)
	{
		charlen = pg_mblen(ptr);

		if (state == LQPRS_WAITLEVEL)
		{
			escaped_count = 0;
			if (charlen == 1) {
				if (t_iseq(ptr, '!')) {
					GETVAR(curqlevel) = lptr = (nodeitem *) palloc0(sizeof(nodeitem) * numOR);
					lptr->start = ptr + 1;
					state = LQPRS_WAITDELIM;
					curqlevel->numvar = 1;
					curqlevel->flag |= LQL_NOT;
					hasnot = true;
				}
				else if (t_iseq(ptr, '*'))
					state = LQPRS_WAITOPEN;
				else if (t_iseq(ptr, '\\')) {
					GETVAR(curqlevel) = lptr = (nodeitem *) palloc0(sizeof(nodeitem) * numOR);
					lptr->start = ptr;
					curqlevel->numvar = 1;
					state = LQPRS_WAITESCAPED;
				}
				else if (t_iseq(ptr, '.') || t_iseq(ptr, '|')) {
					UNCHAR;
				} else {
					GETVAR(curqlevel) = lptr = (nodeitem *) palloc0(sizeof(nodeitem) * numOR);
					lptr->start = ptr;
					state = LQPRS_WAITDELIM;
					curqlevel->numvar = 1;
				}
			}
			else {
				GETVAR(curqlevel) = lptr = (nodeitem *) palloc0(sizeof(nodeitem) * numOR);
				lptr->start = ptr;
				state = LQPRS_WAITDELIM;
				curqlevel->numvar = 1;
			}
		}
		else if (state == LQPRS_WAITVAR)
		{
			escaped_count = 0;
			lptr++;
			lptr->start = ptr;
			curqlevel->numvar++;
			if (t_iseq(ptr, '.') || t_iseq(ptr, '|'))
				UNCHAR;

			state = (t_iseq(ptr, '\\')) ? LQPRS_WAITESCAPED : LQPRS_WAITDELIM;
		}
		else if (state == LQPRS_WAITDELIM)
		{
			if (charlen == 1 && t_iseq(ptr, '@'))
			{
				if (lptr->start == ptr)
					UNCHAR;
				lptr->flag |= LVAR_INCASE;
				curqlevel->flag |= LVAR_INCASE;
			}
			else if (charlen == 1 && t_iseq(ptr, '*'))
			{
				if (lptr->start == ptr)
					UNCHAR;
				lptr->flag |= LVAR_ANYEND;
				curqlevel->flag |= LVAR_ANYEND;
			}
			else if (charlen == 1 && t_iseq(ptr, '%'))
			{
				if (lptr->start == ptr)
					UNCHAR;
				lptr->flag |= LVAR_SUBLEXEME;
				curqlevel->flag |= LVAR_SUBLEXEME;
			}
			else if (charlen == 1 && t_iseq(ptr, '|'))
			{
				lptr->len = ptr - lptr->start - escaped_count -
					((lptr->flag & LVAR_SUBLEXEME) ? 1 : 0) -
					((lptr->flag & LVAR_INCASE) ? 1 : 0) -
					((lptr->flag & LVAR_ANYEND) ? 1 : 0);
				if (lptr->wlen > 255)
					ereport(ERROR,
							(errcode(ERRCODE_NAME_TOO_LONG),
							 errmsg("name of level is too long"),
							 errdetail("Name length is %d, must "
								 "be < 256, in position %d.",
								 lptr->wlen, pos)));

				state = LQPRS_WAITVAR;
			}
			else if (charlen == 1 && t_iseq(ptr, '.'))
			{
				lptr->len = ptr - lptr->start - escaped_count -
					((lptr->flag & LVAR_SUBLEXEME) ? 1 : 0) -
					((lptr->flag & LVAR_INCASE) ? 1 : 0) -
					((lptr->flag & LVAR_ANYEND) ? 1 : 0);
				if (lptr->wlen > 255)
					ereport(ERROR,
							(errcode(ERRCODE_NAME_TOO_LONG),
							 errmsg("name of level is too long"),
							 errdetail("Name length is %d, must "
								 "be < 256, in position %d.",
								 lptr->wlen, pos)));

				state = LQPRS_WAITLEVEL;
				curqlevel = NEXTLEV(curqlevel);
			}
			else if (charlen == 1 && t_iseq(ptr, '\\'))
			{
				if (lptr->flag)
					UNCHAR;
				state = LQPRS_WAITESCAPED;
			}
			else
				if (lptr->flag)
					UNCHAR;
		}
		else if (state == LQPRS_WAITOPEN)
		{
			if (charlen == 1 && t_iseq(ptr, '{'))
				state = LQPRS_WAITFNUM;
			else if (charlen == 1 && t_iseq(ptr, '.'))
			{
				curqlevel->low = 0;
				curqlevel->high = 0xffff;
				curqlevel = NEXTLEV(curqlevel);
				state = LQPRS_WAITLEVEL;
			}
			else
				UNCHAR;
		}
		else if (state == LQPRS_WAITFNUM)
		{
			if (charlen == 1 && t_iseq(ptr, ','))
				state = LQPRS_WAITSNUM;
			else if (t_isdigit(ptr))
			{
				curqlevel->low = atoi(ptr);
				state = LQPRS_WAITND;
			}
			else
				UNCHAR;
		}
		else if (state == LQPRS_WAITSNUM)
		{
			if (t_isdigit(ptr))
			{
				curqlevel->high = atoi(ptr);
				state = LQPRS_WAITCLOSE;
			}
			else if (charlen == 1 && t_iseq(ptr, '}'))
			{
				curqlevel->high = 0xffff;
				state = LQPRS_WAITEND;
			}
			else
				UNCHAR;
		}
		else if (state == LQPRS_WAITCLOSE)
		{
			if (charlen == 1 && t_iseq(ptr, '}'))
				state = LQPRS_WAITEND;
			else if (!t_isdigit(ptr))
				UNCHAR;
		}
		else if (state == LQPRS_WAITND)
		{
			if (charlen == 1 && t_iseq(ptr, '}'))
			{
				curqlevel->high = curqlevel->low;
				state = LQPRS_WAITEND;
			}
			else if (charlen == 1 && t_iseq(ptr, ','))
				state = LQPRS_WAITSNUM;
			else if (!t_isdigit(ptr))
				UNCHAR;
		}
		else if (state == LQPRS_WAITEND)
		{
			if (charlen == 1 && (t_iseq(ptr, '.') || t_iseq(ptr, '|')))
			{
				state = LQPRS_WAITLEVEL;
				curqlevel = NEXTLEV(curqlevel);
			}
			else
				UNCHAR;
		}
		else if (state == LQPRS_WAITESCAPED)
		{
			state = LQPRS_WAITDELIM;
			escaped_count++;
		}
		else
			/* internal error */
			elog(ERROR, "internal error in parser");

		ptr += charlen;
		if (state == LQPRS_WAITDELIM)
			lptr->wlen++;
		pos++;
	}

	if (state == LQPRS_WAITDELIM)
	{
		if (lptr->start == ptr)
			ereport(ERROR,
					(errcode(ERRCODE_SYNTAX_ERROR),
					 errmsg("syntax error"),
					 errdetail("Unexpected end of line.")));

		lptr->len = ptr - lptr->start - escaped_count -
			((lptr->flag & LVAR_SUBLEXEME) ? 1 : 0) -
			((lptr->flag & LVAR_INCASE) ? 1 : 0) -
			((lptr->flag & LVAR_ANYEND) ? 1 : 0);
		if (lptr->len == 0)
			ereport(ERROR,
					(errcode(ERRCODE_SYNTAX_ERROR),
					 errmsg("syntax error"),
					 errdetail("Unexpected end of line.")));

		if (lptr->wlen > 255)
			ereport(ERROR,
					(errcode(ERRCODE_NAME_TOO_LONG),
					 errmsg("name of level is too long"),
					 errdetail("Name length is %d, must "
							   "be < 256, in position %d.",
							   lptr->wlen, pos)));
	}
	else if (state == LQPRS_WAITOPEN)
		curqlevel->high = 0xffff;
	else if (state != LQPRS_WAITEND)
		ereport(ERROR,
				(errcode(ERRCODE_SYNTAX_ERROR),
				 errmsg("syntax error"),
				 errdetail("Unexpected end of line.")));
	else if (state == LQPRS_WAITESCAPED)
		ereport(ERROR,
				(errcode(ERRCODE_SYNTAX_ERROR),
				 errmsg("syntax error"),
				 errdetail("Unexpected end of line.")));


	curqlevel = tmpql;
	totallen = LQUERY_HDRSIZE;
	while ((char *) curqlevel - (char *) tmpql < levels * ITEMSIZE)
	{
		totallen += LQL_HDRSIZE;
		if (curqlevel->numvar)
		{
			lptr = GETVAR(curqlevel);
			while (lptr - GETVAR(curqlevel) < curqlevel->numvar)
			{
				totallen += MAXALIGN(LVAR_HDRSIZE + lptr->len);
				lptr++;
			}
		}
		else if (curqlevel->low > curqlevel->high)
			ereport(ERROR,
					(errcode(ERRCODE_SYNTAX_ERROR),
					 errmsg("syntax error"),
					 errdetail("Low limit(%d) is greater than upper(%d).",
							   curqlevel->low, curqlevel->high)));

		curqlevel = NEXTLEV(curqlevel);
	}

	result = (lquery *) palloc0(totallen);
	SET_VARSIZE(result, totallen);
	result->numlevel = levels;
	result->firstgood = 0;
	result->flag = 0;
	if (hasnot)
		result->flag |= LQUERY_HASNOT;
	cur = LQUERY_FIRST(result);
	curqlevel = tmpql;
	while ((char *) curqlevel - (char *) tmpql < levels * ITEMSIZE)
	{
		memcpy(cur, curqlevel, LQL_HDRSIZE);
		cur->totallen = LQL_HDRSIZE;
		if (curqlevel->numvar)
		{
			lrptr = LQL_FIRST(cur);
			lptr = GETVAR(curqlevel);
			while (lptr - GETVAR(curqlevel) < curqlevel->numvar)
			{
				cur->totallen += MAXALIGN(LVAR_HDRSIZE + lptr->len);
				lrptr->len = lptr->len;
				lrptr->flag = lptr->flag;
				copy_unescaped(lptr->start, lrptr->name, lptr->len);
				lrptr->val = ltree_crc32_sz(lrptr->name, lptr->len);
				lptr++;
				lrptr = LVAR_NEXT(lrptr);
			}
			pfree(GETVAR(curqlevel));
			if (cur->numvar > 1 || cur->flag != 0)
				wasbad = true;
			else if (wasbad == false)
				(result->firstgood)++;
		}
		else
			wasbad = true;
		curqlevel = NEXTLEV(curqlevel);
		cur = LQL_NEXT(cur);
	}

	pfree(tmpql);
	PG_RETURN_POINTER(result);
}

Datum
lquery_out(PG_FUNCTION_ARGS)
{
	lquery	   *in = PG_GETARG_LQUERY_P(0);
	char	   *buf,
			   *ptr;
	int			i,
				j,
				totallen = 1;
	lquery_level *curqlevel;
	lquery_variant *curtlevel;

	curqlevel = LQUERY_FIRST(in);
	for (i = 0; i < in->numlevel; i++)
	{
		totallen++;
		if (curqlevel->numvar)
			totallen += 1 + (curqlevel->numvar * 4) + curqlevel->totallen;
		else
			totallen += 2 * 11 + 4;
		curqlevel = LQL_NEXT(curqlevel);
	}

	ptr = buf = (char *) palloc(totallen);
	curqlevel = LQUERY_FIRST(in);
	for (i = 0; i < in->numlevel; i++)
	{
		if (i != 0)
		{
			*ptr = '.';
			ptr++;
		}
		if (curqlevel->numvar)
		{
			if (curqlevel->flag & LQL_NOT)
			{
				*ptr = '!';
				ptr++;
			}
			curtlevel = LQL_FIRST(curqlevel);
			for (j = 0; j < curqlevel->numvar; j++)
			{
				int escapes = 0;
				if (j != 0)
				{
					*ptr = '|';
					ptr++;
				}
				escapes = copy_escaped(curtlevel->name, ptr, curtlevel->len, "\\ .|");
				ptr += curtlevel->len;
				ptr += escapes;
				if ((curtlevel->flag & LVAR_SUBLEXEME))
				{
					*ptr = '%';
					ptr++;
				}
				if ((curtlevel->flag & LVAR_INCASE))
				{
					*ptr = '@';
					ptr++;
				}
				if ((curtlevel->flag & LVAR_ANYEND))
				{
					*ptr = '*';
					ptr++;
				}
				curtlevel = LVAR_NEXT(curtlevel);
			}
		}
		else
		{
			if (curqlevel->low == curqlevel->high)
			{
				sprintf(ptr, "*{%d}", curqlevel->low);
			}
			else if (curqlevel->low == 0)
			{
				if (curqlevel->high == 0xffff)
				{
					*ptr = '*';
					*(ptr + 1) = '\0';
				}
				else
					sprintf(ptr, "*{,%d}", curqlevel->high);
			}
			else if (curqlevel->high == 0xffff)
			{
				sprintf(ptr, "*{%d,}", curqlevel->low);
			}
			else
				sprintf(ptr, "*{%d,%d}", curqlevel->low, curqlevel->high);
			ptr = strchr(ptr, '\0');
		}

		curqlevel = LQL_NEXT(curqlevel);
	}

	*ptr = '\0';
	PG_FREE_IF_COPY(in, 0);

	PG_RETURN_POINTER(buf);
}
