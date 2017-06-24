#include <stdio.h>
#include <sys/socket.h>



int main(int argc, char **argv) {

  char buf[2048] = {0};
  FILE *socks_req = fopen(argv[1], "r");
  size_t socks_req_size = fread(buf, 1, sizeof(buf), socks_req);
  if (ferror(socks_req)) {
    fprintf(stderr, "Error reading SOCKS file\n");
    return -1;
  }

  /* Create socket */
  int sockfd = socket(PF_INET, SOCK_STREAM, 0);
  
  





  return 0;
}
