// Source - https://stackoverflow.com/
// Posted by Mark Adler, modified by community. See post 'Timeline' for change history
// Retrieved 2026-01-05, License - CC BY-SA 4.0

#include <stdio.h>
#include <string.h>
#include <time.h>
#include "zlib.h"

#define TEXT "This is a test of the emergency broadcast system.\n" \
             "Remember, this is only a test.\n"

int main(void) {
    z_stream strm = {0};
    int ret = deflateInit2(&strm, -1, 8, 15 + 16, 8, 0);
    if (ret != Z_OK)
        return 1;

    gz_header head;
    head.text = 1;
    head.time = time(NULL);
    head.os = 3;
    head.extra = (unsigned char *)"x1\004\0abcd";
    head.extra_len = 8;
    head.name = (unsigned char *)"foo.bar";
    head.comment = (unsigned char *)"no comment";
    head.hcrc = 1;
    deflateSetHeader(&strm, &head);

    strm.avail_in = strlen(TEXT);
    strm.next_in = (unsigned char *)TEXT;
    unsigned char out[256];
    strm.avail_out = sizeof(out);
    strm.next_out = out;
    ret = deflate(&strm, Z_FINISH);
    if (ret != Z_STREAM_END)
        return 1;

    deflateEnd(&strm);

    fwrite(out, 1, sizeof(out) - strm.avail_out, stdout);
    return 0;
}
