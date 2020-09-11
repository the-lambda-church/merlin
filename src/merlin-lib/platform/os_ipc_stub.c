#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#ifdef _WIN32
#include <windows.h>
#include <io.h>
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
#endif
#else
#include <unistd.h>
#include <fcntl.h>

#include <sys/socket.h>
#include <sys/select.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/threads.h>

#ifdef _MSC_VER
extern __declspec(dllimport) char **environ;
#else
extern char **environ;
#endif

CAMLprim value
ml_merlin_set_environ(value venviron)
{
  static char *buffer = NULL;

  const char *ptr = String_val(venviron);
  size_t length = caml_string_length(venviron);

  buffer = realloc(buffer, length);
  memcpy(buffer, ptr, length);

  // clearenv() is not portable
  if (environ)
    *environ = NULL;

  size_t i, j;

  for (i = 0, j = 0; i < length; ++i)
  {
    if (buffer[i] == '\0')
    {
      putenv(&buffer[j]);
      j = i + 1;
    }
  }

  return Val_unit;
}

// Seen in the wild: environment of 40k
#define BUFFER_SIZE 262144
static unsigned char buffer[BUFFER_SIZE];

#ifndef _WIN32
#define NO_EINTR(var, command) \
  do { (var) = command; } while ((var) == -1 && errno == EINTR)

#define unbyte(x,n) (((unsigned char)x) << (n * 8))

static ssize_t recv_buffer(int fd, int fds[3])
{
  char msg_control[CMSG_SPACE(3 * sizeof(int))];
  struct iovec iov = { .iov_base = buffer, .iov_len = sizeof(buffer) };
  struct msghdr msg = {
    .msg_iov = &iov, .msg_iovlen = 1,
    .msg_controllen = CMSG_SPACE(3 * sizeof(int)),
  };
  msg.msg_control = &msg_control;
  memset(msg.msg_control, 0, msg.msg_controllen);

  ssize_t recvd;
  NO_EINTR(recvd, recvmsg(fd, &msg, 0));
  if (recvd == -1)
  {
    perror("recvmsg");
    return -1;
  }

  if (recvd < 4)
  {
    ssize_t recvd_;
    do {
      NO_EINTR(recvd_, recv(fd, buffer + recvd, sizeof(buffer) - recvd, 0));
      if (recvd_ > 0)
        recvd += recvd_;
    } while (recvd_ > 0 && recvd < 4);
  }

  size_t target = -1;

  if (recvd > 4)
  {
    target =
      unbyte(buffer[0],0) | unbyte(buffer[1],1) |
      unbyte(buffer[2],2) | unbyte(buffer[3],3);

    if (recvd < target)
    {
      ssize_t recvd_;
      do {
        NO_EINTR(recvd_, recv(fd, buffer + recvd, sizeof(buffer) - recvd, 0));
        if (recvd_ > 0)
          recvd += recvd_;
      } while (recvd_ > 0 && recvd < target);
    }
  }

  struct cmsghdr *cm = CMSG_FIRSTHDR(&msg);

  if (cm == NULL)
  {
    perror("recvmsg");
    return -1;
  }
  int *fds0 = (int*)CMSG_DATA(cm);
  int nfds = (cm->cmsg_len - CMSG_LEN(0)) / sizeof(int);

  /* Check malformed packet */
  if (nfds != 3 || recvd != target || buffer[recvd-1] != '\0')
  {
    int i;
    for (i = 0; i < nfds; ++i)
      close(fds0[i]);
    return -1;
  }

  {
    int i;
    for (i = 0; i < 3; ++i)
    {
      fds[i] = fds0[i];
      if (fcntl(fds[i], F_SETFD, FD_CLOEXEC) == -1)
        perror("fcntl");
    }
  }

  return recvd;
}
#endif

value ml_merlin_server_setup(value path, value strfd)
{
  CAMLparam2(path, strfd);
  CAMLlocal2(payload, ret);
  char *endptr = NULL;
  int fd;

#ifdef _WIN32
  fd = 0;
  ret = strfd;
#else
  fd = strtol(String_val(strfd), &endptr, 0);
  if (!endptr || *endptr != '\0')
    fd = -1;
  else
    ret = Val_int(fd);
#endif

  if (fd != -1)
  {
    /* (path, fd) */
    payload = caml_alloc(2, 0);
    Store_field(payload, 0, path);
    Store_field(payload, 1, ret);

    /* Some payload */
    ret = caml_alloc(1, 0);
    Store_field(ret, 0, payload);
  }
  else
  {
    fprintf(stderr, "ml_merlin_server_setup(\"%s\",\"%s\"): invalid argument\n",
        String_val(path), String_val(strfd));
    unlink(String_val(path));
    /* None */
    ret = Val_unit;
  }

  CAMLreturn(ret);
}

value ml_merlin_server_accept(value server, value val_timeout)
{
  CAMLparam2(server, val_timeout);
  CAMLlocal3(ret, client, context);
  CAMLlocal3(wd, env, args);

  ssize_t len = -1;

#ifdef _WIN32
  static BOOL bDoneReset = FALSE;
  HANDLE hPipe = CreateNamedPipe(String_val(Field(server, 0)), PIPE_ACCESS_DUPLEX, PIPE_TYPE_MESSAGE | PIPE_READMODE_MESSAGE | PIPE_WAIT, PIPE_UNLIMITED_INSTANCES, 1024, 1024, NMPWAIT_USE_DEFAULT_WAIT, NULL);
  ret = Val_unit; /* None */
  if (hPipe != INVALID_HANDLE_VALUE)
  {
    if (!bDoneReset)
    {
      HANDLE hEvent = OpenEvent(EVENT_MODIFY_STATE, FALSE, String_val(Field(server, 1)));
      SetEvent(hEvent);
      CloseHandle(hEvent);
      bDoneReset = TRUE;
    }
    if (ConnectNamedPipe(hPipe, NULL) || GetLastError() == ERROR_PIPE_CONNECTED)
    {
      intptr_t fds[3];
      DWORD dwNumberOfBytesRead;
      if (ReadFile(hPipe, fds, 3 * sizeof(HANDLE), &dwNumberOfBytesRead, NULL) && dwNumberOfBytesRead == 3 * sizeof(HANDLE))
      {
        context = caml_alloc(4, 0); /* hPipe, stdin, stdout, stderr) */
        Store_field(context, 0, caml_copy_nativeint((intnat)hPipe));
        Store_field(context, 1, Val_int(_open_osfhandle(fds[0], 0)));
        Store_field(context, 2, Val_int(_open_osfhandle(fds[1], 0)));
        Store_field(context, 3, Val_int(_open_osfhandle(fds[2], 0)));
        if (ReadFile(hPipe, buffer, BUFFER_SIZE, &dwNumberOfBytesRead, NULL))
        {
          len = dwNumberOfBytesRead;
        }
        else
        {
          DisconnectNamedPipe(hPipe);
          CloseHandle(hPipe);
        }
      }
      else
      {
        DisconnectNamedPipe(hPipe);
        CloseHandle(hPipe);
      }
    }
    else
    {
      CloseHandle(hPipe);
    }
  }
#else
  // Compute timeout
  double timeout = Double_val(val_timeout);
  struct timeval tv;
  tv.tv_sec = timeout;
  tv.tv_usec = (timeout - tv.tv_sec) * 1000000;

  // Select on server
  int serverfd = Int_val(Field(server, 1));
  int selectres;
  fd_set readset;

  caml_release_runtime_system();
  do {
    FD_ZERO(&readset);
    FD_SET(serverfd, &readset);
    selectres = select(serverfd + 1, &readset, NULL, NULL, &tv);
  } while (selectres == -1 && errno == EINTR);
  caml_acquire_runtime_system();

  int fds[3], clientfd;

  if (selectres > 0)
  {
    NO_EINTR(clientfd, accept(serverfd, NULL, NULL));
    len = recv_buffer(clientfd, fds);
  }

  if (len == -1)
    ret = Val_unit; /* None */
  else {
    context = caml_alloc(4, 0); /* (clientfd, stdin, stdout, stderr) */
    Store_field(context, 0, Val_int(clientfd));
    Store_field(context, 1, Val_int(fds[0]));
    Store_field(context, 2, Val_int(fds[1]));
    Store_field(context, 3, Val_int(fds[2]));
  }
#endif

  if (len != -1)
  {
    ssize_t i, j;
    int argc;
    i = 4;

    // Extract working directory
    wd = caml_copy_string((const char *)&buffer[i]);
    i += caml_string_length(wd) + 1;

    // Extract environment
    if (buffer[i] == '\0')
    {
      env = caml_alloc_string(0);
      i += 1;
    }
    else
    {
      ssize_t env_start = i;
      i += 1;
      for (; i < len; ++i)
      {
        if (buffer[i-1] == '\0' && buffer[i] == '\0')
        {
          i += 1;
          break;
        }
      }
      env = caml_alloc_string(i - env_start);
      memcpy((char *)String_val(env), &buffer[env_start], i - env_start);
    }

    // Extract remaining args
    ssize_t args_start = i;
    argc = 0;

    for (i = args_start; i < len; ++i)
    {
      if (buffer[i] == '\0')
        argc += 1;
    }

    args = caml_alloc(argc, 0);

    argc = 0;
    for (i = args_start, j = args_start; i < len; ++i)
    {
      if (buffer[i] == '\0')
      {
        Store_field(args, argc, caml_copy_string((const char *)&buffer[j]));
        j = i + 1;
        argc += 1;
      }
    }

    client = caml_alloc(4, 0); /* (context, wd, environ, args) */
    Store_field(client, 0, context);
    Store_field(client, 1, wd);
    Store_field(client, 2, env);
    Store_field(client, 3, args);

    ret = caml_alloc(1, 0); /* Some client */
    Store_field(ret, 0, client);
  }

  CAMLreturn(ret);
}


value ml_merlin_server_close(value server)
{
  CAMLparam1(server);
#ifndef _WIN32
  unlink(String_val(Field(server, 0)));
  close(Int_val(Field(server, 1)));
#endif
  CAMLreturn(Val_unit);
}

static void setup_fds(int fd0, int fd1, int fd2)
{
  static int copy0 = -1, copy1 = -1, copy2 = -1;

  // Backup original
  if (copy0 == -1) copy0 = dup(STDIN_FILENO);
  if (copy1 == -1) copy1 = dup(STDOUT_FILENO);
  if (copy2 == -1) copy2 = dup(STDERR_FILENO);

  // Copy or restore new ones
  if (fd0 != -1) dup2(fd0, STDIN_FILENO);
  else { dup2(copy0, STDIN_FILENO); close(copy0); copy0 = -1; }

  if (fd1 != -1) dup2(fd1, STDOUT_FILENO);
  else { dup2(copy1, STDOUT_FILENO); close(copy1); copy1 = -1; }

  if (fd2 != -1) dup2(fd2, STDERR_FILENO);
  else { dup2(copy2, STDERR_FILENO); close(copy2); copy2 = -1; }
}

value ml_merlin_context_setup(value context)
{
  CAMLparam1(context);
  setup_fds(
      Int_val(Field(context, 1)),
      Int_val(Field(context, 2)),
      Int_val(Field(context, 3))
      );
  CAMLreturn(Val_unit);
}

value ml_merlin_context_close(value context, value return_code)
{
  CAMLparam1(context);
  char code = (char)(Int_val(return_code));
#ifdef _WIN32
  HANDLE hPipe;
  DWORD dwNumberOfBytesWritten;
#else
  ssize_t wrote_ = -1;
#endif
  setup_fds(-1, -1, -1);

#ifdef _WIN32
  hPipe = (HANDLE)Nativeint_val(Field(context, 0));
  WriteFile(hPipe, &code, sizeof(char), &dwNumberOfBytesWritten, NULL);
#else
  NO_EINTR(wrote_, write(Int_val(Field(context, 0)), &code, sizeof(char)));
#endif

  // Close stdin, stdout, stderr
  close(Int_val(Field(context, 1)));
  close(Int_val(Field(context, 2)));
  close(Int_val(Field(context, 3)));

  // Close client connection
#ifdef _WIN32
  FlushFileBuffers(hPipe);
  DisconnectNamedPipe(hPipe);
  CloseHandle(hPipe);
#else
  close(Int_val(Field(context, 0)));
#endif

  CAMLreturn(Val_unit);
}
