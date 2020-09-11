#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#ifdef _WIN32
/* GetNamedPipeServerProcessId requires Windows Vista+ */
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x600
#include <windows.h>
#include <Lmcons.h>
#include <process.h>
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif
#ifdef _MSC_VER
typedef SSIZE_T ssize_t;
#define PATH_MAX MAX_PATH
#ifndef _UCRT
#define snprintf _snprintf
#endif
#endif
#else
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <libgen.h>
#endif
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <limits.h>

#if defined(__linux)
#include <sys/param.h>
#elif defined(__APPLE__)
#include <sys/syslimits.h>
#elif defined(__OpenBSD__)
#include <sys/param.h>
#endif

/** Portability information **/

/* Determine OS, http://stackoverflow.com/questions/6649936
   __linux__       Defined on Linux
   __sun           Defined on Solaris
   __FreeBSD__     Defined on FreeBSD
   __NetBSD__      Defined on NetBSD
   __OpenBSD__     Defined on OpenBSD
   __APPLE__       Defined on Mac OS X
   __hpux          Defined on HP-UX
   __osf__         Defined on Tru64 UNIX (formerly DEC OSF1)
   __sgi           Defined on Irix
   _AIX            Defined on AIX
*/

/* Compute executable path, http://stackoverflow.com/questions/1023306
   Mac OS X       _NSGetExecutablePath() (man 3 dyld)
   Linux          readlink /proc/self/exe
   Solaris        getexecname()
   FreeBSD        sysctl CTL_KERN KERN_PROC KERN_PROC_PATHNAME -1
   NetBSD         readlink /proc/curproc/exe
   DragonFly BSD  readlink /proc/curproc/file
   Windows        GetModuleFileName() with hModule = NULL
*/

#define NO_EINTR(var, command) \
  do { (var) = command; } while ((var) == -1 && errno == EINTR)

static void dumpinfo(void);

static void failwith_perror(const char *msg)
{
  perror(msg);
  dumpinfo();
  exit(EXIT_FAILURE);
}

static void failwith(const char *msg)
{
  fprintf(stderr, "%s\n", msg);
  dumpinfo();
  exit(EXIT_FAILURE);
}

#define PATHSZ (PATH_MAX+1)

#define BEGIN_PROTECTCWD \
  { char previous_cwd[PATHSZ]; \
    if (!getcwd(previous_cwd, PATHSZ)) previous_cwd[0] = '\0';

/* Return from chdir is ignored */
#define END_PROTECTCWD \
    if (previous_cwd[0] != '\0') if (chdir(previous_cwd)) {} }

static const char *path_socketdir(void)
{
  static const char *tmpdir = NULL;
  if (tmpdir == NULL)
    tmpdir = getenv("TMPDIR");
  if (tmpdir == NULL)
    tmpdir = "/tmp";
  return tmpdir;
}

#ifdef _WIN32
/** Deal with Windows IPC **/

static void ipc_send(HANDLE hPipe, unsigned char *buffer, size_t len, HANDLE fds[3])
{
  DWORD dwNumberOfBytesWritten;
  if (!WriteFile(hPipe, fds, 3 * sizeof(HANDLE), &dwNumberOfBytesWritten, NULL) || dwNumberOfBytesWritten != 3 * sizeof(HANDLE))
    failwith_perror("sendmsg");
  if (!WriteFile(hPipe, buffer, len, &dwNumberOfBytesWritten, NULL) || dwNumberOfBytesWritten != len)
    failwith_perror("send");
}

#else
/** Deal with UNIX IPC **/

static void ipc_send(int fd, unsigned char *buffer, size_t len, int fds[3])
{
  char msg_control[CMSG_SPACE(3 * sizeof(int))];
  struct iovec iov = { .iov_base = buffer, .iov_len = len };
  struct msghdr msg = {
    .msg_iov = &iov, .msg_iovlen = 1,
    .msg_controllen = CMSG_SPACE(3 * sizeof(int)),
  };
  msg.msg_control = &msg_control;
  memset(msg.msg_control, 0, msg.msg_controllen);

  struct cmsghdr *cm = CMSG_FIRSTHDR(&msg);
  cm->cmsg_level = SOL_SOCKET;
  cm->cmsg_type = SCM_RIGHTS;
  cm->cmsg_len = CMSG_LEN(3 * sizeof(int));

  int *fds0 = (int*)CMSG_DATA(cm);
  fds0[0] = fds[0];
  fds0[1] = fds[1];
  fds0[2] = fds[2];

  ssize_t sent;
  NO_EINTR(sent, sendmsg(fd, &msg, 0));

  if (sent == -1)
    failwith_perror("sendmsg");

  while (sent < len)
  {
    ssize_t sent_;
    NO_EINTR(sent_, send(fd, buffer + sent, len - sent, 0));

    if (sent_ == -1)
      failwith_perror("sent");

    sent += sent_;
  }
}
#endif

/* Serialize arguments */

#define byte(x,n) ((unsigned)((x) >> (n * 8)) & 0xFF)

static void append_argument(unsigned char *buffer, size_t len, ssize_t *pos, const char *p)
{
  ssize_t j = *pos;
  while (*p && j < len)
  {
    buffer[j] = *p;
    j += 1;
    p += 1;
  }

  if (j >= len)
    failwith("maximum number of arguments exceeded");

  buffer[j] = 0;
  j += 1;
  *pos = j;
}

#ifdef _MSC_VER
extern __declspec(dllimport) char **environ;
#else
extern char **environ;
#endif

static ssize_t prepare_args(unsigned char *buffer, size_t len, int argc, char **argv)
{
  int i = 0;
  ssize_t j = 4;

  /* First put the current working directory */

  char cwd[PATHSZ];
  if (!getcwd(cwd, PATHSZ)) cwd[0] = '\0';
  append_argument(buffer, len, &j, cwd);

  /* Then append environ */
  for (i = 0; environ[i] != NULL; ++i)
  {
    const char *v = environ[i];
    if (v[0] == '\0') continue;

    append_argument(buffer, len, &j, environ[i]);
  }

  /* Env var delimiter */
  append_argument(buffer, len, &j, "");

  /* Append arguments */
  for (i = 0; i < argc && j < len; ++i)
  {
    append_argument(buffer, len, &j, argv[i]);
  }

  /* Put size at the beginning */
  buffer[0] = byte(j,0);
  buffer[1] = byte(j,1);
  buffer[2] = byte(j,2);
  buffer[3] = byte(j,3);
  return j;
}

#ifdef _WIN32
#define IPC_SOCKET_TYPE HANDLE
static HANDLE connect_socket(const char *socketname, int fail)
{
  HANDLE hPipe;
  hPipe = CreateFile(socketname, GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, 0);
  if (hPipe == INVALID_HANDLE_VALUE)
    if (fail) failwith_perror("connect");
  return hPipe;
}
#else
#define IPC_SOCKET_TYPE int
#define INVALID_HANDLE_VALUE -1
static int connect_socket(const char *socketname, int fail)
{
  int sock = socket(PF_UNIX, SOCK_STREAM, 0);
  if (sock == -1) failwith_perror("socket");

  int err;

  BEGIN_PROTECTCWD
    struct sockaddr_un address;
    int address_len;

    /* Return from chdir is ignored */
    err = chdir(path_socketdir());
    address.sun_family = AF_UNIX;
    snprintf(address.sun_path, 104, "./%s", socketname);
    address_len = strlen(address.sun_path) + sizeof(address.sun_family) + 1;

    NO_EINTR(err, connect(sock, (struct sockaddr*)&address, address_len));
  END_PROTECTCWD

  if (err == -1)
  {
    if (fail) failwith_perror("connect");
    close(sock);
    return -1;
  }

  return sock;
}
#endif

#ifdef _WIN32
static void start_server(const char *socketname, const char* eventname, const char *exec_path)
{
  char buf[PATHSZ];
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  HANDLE hEvent = CreateEvent(NULL, FALSE, FALSE, eventname);
  DWORD dwResult;
  sprintf(buf, "%s server %s %s", exec_path, socketname, eventname);
  ZeroMemory(&si, sizeof(si));
  si.cb = sizeof(si);
  ZeroMemory(&pi, sizeof(pi));
  /* Note that DETACHED_PROCESS means that the process does not appear in Task Manager
     but the server can still be stopped with ocamlmerlin server stop-server */
  if (!CreateProcess(exec_path, buf, NULL, NULL, FALSE, DETACHED_PROCESS, NULL, NULL, &si, &pi))
    failwith_perror("fork");
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);
  if (WaitForSingleObject(hEvent, 5000) != WAIT_OBJECT_0)
    failwith_perror("execlp");
}
#else
static void make_daemon(int sock)
{
  /* On success: The child process becomes session leader */
  if (setsid() < 0)
    failwith_perror("setsid");

  /* Close all open file descriptors */
  close(0);
  if (open("/dev/null", O_RDWR, 0) != 0)
    failwith_perror("open");
  dup2(0,1);
  dup2(0,2);

  /* Change directory to root, so that process still works if directory
   * is delete. */
  if (chdir("/") != 0)
    failwith_perror("chdir");

  //int x;
  //for (x = sysconf(_SC_OPEN_MAX); x>2; x--)
  //{
  //  if (x != sock)
  //    close(x);
  //}

  pid_t child = fork();
  signal(SIGHUP, SIG_IGN);

  /* An error occurred */
  if (child < 0)
    failwith_perror("fork");

  /* Success: Let the parent terminate */
  if (child > 0)
    exit(EXIT_SUCCESS);
}

static void start_server(const char *socketname, const char* ignored, const char *exec_path)
{
  int sock = socket(PF_UNIX, SOCK_STREAM, 0);
  if (sock == -1)
    failwith_perror("socket");

  int err;

  BEGIN_PROTECTCWD
    struct sockaddr_un address;
    int address_len;

    /* Return from chdir is ignored */
    err = chdir(path_socketdir());
    address.sun_family = AF_UNIX;
    snprintf(address.sun_path, 104, "./%s", socketname);
    address_len = strlen(address.sun_path) + sizeof(address.sun_family) + 1;
    unlink(address.sun_path);

    NO_EINTR(err, bind(sock, (struct sockaddr*)&address, address_len));
  END_PROTECTCWD

  if (err == -1)
    failwith_perror("bind");

  if (listen(sock, 5) == -1)
    failwith_perror("listen");

  pid_t child = fork();

  if (child == -1)
    failwith_perror("fork");

  if (child == 0)
  {
    make_daemon(sock);

    char socket_fd[50], socket_path[PATHSZ];
    sprintf(socket_fd, "%d", sock);
    snprintf(socket_path, PATHSZ, "%s/%s", path_socketdir(), socketname);
    //execlp("nohup", "nohup", exec_path, "server", socket_path, socket_fd, NULL);
    execlp(exec_path, exec_path, "server", socket_path, socket_fd, NULL);
    failwith_perror("execlp");
  }

  close(sock);
  wait(NULL);
}
#endif

static IPC_SOCKET_TYPE connect_and_serve(const char *socket_path, const char* event_path, const char *exec_path)
{
  IPC_SOCKET_TYPE sock = connect_socket(socket_path, 0);

  if (sock == INVALID_HANDLE_VALUE)
  {
    start_server(socket_path, event_path, exec_path);
    sock = connect_socket(socket_path, 1);
  }

  if (sock == INVALID_HANDLE_VALUE)
    abort();

  return sock;
}

/* OCaml merlin path */

static const char *search_in_path(const char *PATH, const char *argv0, char *merlin_path)
{
  static char binary_path[PATHSZ];
#ifdef _WIN32
  char *result = NULL;
  DWORD dwResult;
#endif

  if (PATH == NULL || argv0 == NULL) return NULL;

  while (*PATH)
  {
    int i = 0;
    // Copy one path from PATH
    while (i < PATHSZ-1 && *PATH && *PATH != ':')
    {
      binary_path[i] = *PATH;
      i += 1;
      PATH += 1;
    }

    // Append filename
    {
      const char *file = argv0;
      binary_path[i] = '/';
      i += 1;

      while (i < PATHSZ-1 && *file)
      {
        binary_path[i] = *file;
        i += 1;
        file += 1;
      }

      binary_path[i] = 0;
    }

    // Check path
#ifdef _WIN32
    dwResult = GetFullPathName(binary_path, PATHSZ, merlin_path, NULL);
    if (dwResult && dwResult < PATHSZ)
      if (GetLongPathName(binary_path, NULL, 0))
        result = binary_path;
#else
    char *result = realpath(binary_path, merlin_path);
#endif
    if (result != NULL)
      return result;

    // Seek next path in PATH
    while (*PATH && *PATH != ':')
      PATH += 1;

    while (*PATH == ':')
      PATH += 1;
  }

  return NULL;
}

static void prune_binary_name(char * buffer) {
  size_t strsz = strlen(buffer);
  while (strsz > 0 && buffer[strsz-1] != '/' && buffer[strsz-1] != '\\')
    strsz -= 1;
  buffer[strsz] = 0;
}

#ifdef _WIN32
static char ocamlmerlin_server[] = "ocamlmerlin-server.exe";
#else
static char ocamlmerlin_server[] = "ocamlmerlin-server";
#endif

static void compute_merlinpath(char merlin_path[PATHSZ], const char *argv0, struct stat *st)
{
  char argv0_dirname[PATHSZ];
  size_t strsz;

  strcpy(argv0_dirname, argv0);
  prune_binary_name(argv0_dirname);

  // Check if we were called with a path or not
  if (strlen(argv0_dirname) == 0) {
    if (search_in_path(getenv("PATH"), argv0, merlin_path) == NULL)
      failwith("cannot resolve path to ocamlmerlin");
  } else {
#ifdef _WIN32
    // GetFullPathName does not resolve symbolic links, which realpath does.
    // @@DRA GetLongPathName ensures that the file exists (better way?!).
    // Not sure if this matters.
    DWORD dwResult = GetFullPathName(argv0, PATHSZ, merlin_path, NULL);
    if (!dwResult || dwResult >= PATHSZ || !GetLongPathName(merlin_path, NULL, 0))
#else
    if (realpath(argv0, merlin_path) == NULL)
#endif
      failwith("argv0 does not point to a valid file");
  }

  prune_binary_name(merlin_path);
  strsz = strlen(merlin_path);

  // Append ocamlmerlin-server
  if (strsz + sizeof(ocamlmerlin_server) + 8 > PATHSZ)
    failwith("path is too long");

  strcpy(merlin_path + strsz, ocamlmerlin_server);

  if (stat(merlin_path, st) != 0)
  {
    strcpy(merlin_path + strsz, "ocamlmerlin_server.exe");
    if (stat(merlin_path, st) != 0)
    {
      strcpy(merlin_path + strsz, ocamlmerlin_server);
      failwith_perror("stat(ocamlmerlin-server, also tried ocamlmerlin_server.exe)");
    }
  }
}

#ifdef _WIN32
static void compute_socketname(char socketname[PATHSZ], char eventname[PATHSZ], const char merlin_path[PATHSZ])
#else
static void compute_socketname(char socketname[PATHSZ], struct stat *st)
#endif
{
#ifdef _WIN32
  CHAR user[UNLEN + 1];
  DWORD dwBufSize = UNLEN;
  BY_HANDLE_FILE_INFORMATION info;
  HANDLE hFile = CreateFile(merlin_path, FILE_READ_ATTRIBUTES, FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
  if (hFile == INVALID_HANDLE_VALUE || !GetFileInformationByHandle(hFile, &info))
    failwith_perror("stat (cannot find ocamlmerlin binary)");
  CloseHandle(hFile);

  if (!GetUserName(user, &dwBufSize))
    user[0] = '\0';
  // @@DRA Need to use Windows API functions to get meaningful values for st_dev and st_ino
  snprintf(eventname, PATHSZ,
      "ocamlmerlin_%s_%lx_%llx",
      user,
      info.dwVolumeSerialNumber,
      ((__int64)info.nFileIndexHigh) << 32 | ((__int64)info.nFileIndexLow));
  snprintf(socketname, PATHSZ,
      "\\\\.\\pipe\\%s", eventname);
#else
  snprintf(socketname, PATHSZ,
      "ocamlmerlin_%llu_%llu_%llu.socket",
      (unsigned long long)getuid(),
      (unsigned long long)st->st_dev,
      (unsigned long long)st->st_ino);
#endif
}

/* Main */

static char
  merlin_path[PATHSZ] = "<not computed yet>",
  socketname[PATHSZ] = "<not computed yet>",
  eventname[PATHSZ] = "<not computed yet>";
static unsigned char argbuffer[262144];

static void dumpinfo(void)
{
  fprintf(stderr,
      "merlin path: %s\nsocket path: %s/%s\n", merlin_path, path_socketdir(), socketname);
}

static void unexpected_termination(int argc, char **argv)
{
  int sexp = 0;
  int i;

  for (i = 1; i < argc - 1; ++i)
  {
    if (strcmp(argv[i], "-protocol") == 0 &&
        strcmp(argv[i+1], "sexp") == 0)
      sexp = 1;
  }

  puts(sexp
      ?  "((assoc) (class . \"failure\") (value . \"abnormal termination\") (notifications))"
      : "{\"class\": \"failure\", \"value\": \"abnormal termination\", \"notifications\": [] }"
      );
  failwith("abnormal termination");
}

int main(int argc, char **argv)
{
  char result = 0;
  int err = 0;
  struct stat st;
#ifdef _WIN32
  HANDLE fds[3];
  ULONG pid;
  HANDLE hProcess, hServerProcess;
  DWORD dwNumberOfBytesRead;
  CHAR argv0[PATHSZ];
  GetModuleFileName(NULL, argv0, PATHSZ);
  compute_merlinpath(merlin_path, argv0, &st);
#else
  compute_merlinpath(merlin_path, argv[0], &st);
#endif
  if (argc >= 2 && strcmp(argv[1], "server") == 0)
  {
    IPC_SOCKET_TYPE sock;
    ssize_t len;
#ifdef _WIN32
    compute_socketname(socketname, eventname, merlin_path);
#else
    compute_socketname(socketname, &st);
#endif

    sock = connect_and_serve(socketname, eventname, merlin_path);
    len = prepare_args(argbuffer, sizeof(argbuffer), argc-2, argv+2);
#ifdef _WIN32
    hProcess = GetCurrentProcess();
    if (!GetNamedPipeServerProcessId(sock, &pid))
      failwith_perror("GetNamedPipeServerProcessId");
    hServerProcess = OpenProcess(PROCESS_DUP_HANDLE, FALSE, pid);
    if (hServerProcess == INVALID_HANDLE_VALUE)
      failwith_perror("OpenProcess");
    if (!DuplicateHandle(hProcess, GetStdHandle(STD_INPUT_HANDLE), hServerProcess, &fds[0], 0, FALSE, DUPLICATE_SAME_ACCESS))
      failwith_perror("DuplicateHandle(stdin)");
    if (!DuplicateHandle(hProcess, GetStdHandle(STD_OUTPUT_HANDLE), hServerProcess, &fds[1], 0, FALSE, DUPLICATE_SAME_ACCESS))
      failwith_perror("DuplicateHandle(stdout)");
    CloseHandle(GetStdHandle(STD_OUTPUT_HANDLE));
    if (!DuplicateHandle(hProcess, GetStdHandle(STD_ERROR_HANDLE), hServerProcess, &fds[2], 0, FALSE, DUPLICATE_SAME_ACCESS))
      failwith_perror("DuplicateHandle(stderr)");
#else
    int fds[3] = { STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO };
#endif
    ipc_send(sock, argbuffer, len, fds);

#ifdef _WIN32
    if (ReadFile(sock, &result, 1, &dwNumberOfBytesRead, NULL) && dwNumberOfBytesRead == 1)
      err = 1;
#else
    NO_EINTR(err, read(sock, &result, 1));
#endif
    if (err == 1)
      exit(result);

    unexpected_termination(argc, argv);
  }
  else
  {
    argv[0] = ocamlmerlin_server;
#ifdef _WIN32
    int err = _spawnvp(_P_WAIT, merlin_path, argv);
    if (err < 0)
      failwith_perror("spawnvp(ocamlmerlin-server)");
    else
      exit(err);
#else
    execvp(merlin_path, argv);
    failwith_perror("execvp(ocamlmerlin-server)");
#endif
  }

  /* This is never reached */
  return 0;
}
