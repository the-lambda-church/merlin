#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

/* FS case */

#ifdef __APPLE__

#include <fcntl.h>
#include <sys/param.h>
#include <unistd.h>

value ml_merlin_fs_exact_case(value path)
{
	CAMLparam1(path);
	CAMLlocal1(realpath);
	char realpath_c[MAXPATHLEN];

	realpath = path;

	int fd = open(String_val(path), O_EVTONLY | O_SYMLINK);
	if (fd != -1)
	{
		if (fcntl(fd, F_GETPATH, realpath_c) != -1)
		{
			realpath = caml_copy_string(realpath_c);
		}
		close(fd);
	}
	CAMLreturn(realpath);
}


#else

value ml_merlin_fs_exact_case(value path)
{
	return path;
}

#endif
