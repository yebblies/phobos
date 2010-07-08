//
// Add missing POSIX header file
//
module core.sys.posix.iconv;

alias void* iconv_t;

extern(C) @system nothrow
{
    iconv_t iconv_open(in char*, in char*);
    size_t  iconv(iconv_t, in ubyte**, size_t*, ubyte**, size_t*);
    int     iconv_close(iconv_t);
}

