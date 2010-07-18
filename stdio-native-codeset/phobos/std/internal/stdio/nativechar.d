/*
 * I/O ranges for system native codeset handling.
 *
 * - NativeCharacterReader(Source)
 *      Reads Unicode dchars converted from multibyte character sequences read
 *      from an input range Source.
 *
 * - NativeTextWriter(Sink)
 *      Writes Unicode strings converted to multibyte character sequences into
 *      an output range Sink.
 *
 * Windows ... WideCharToMultiByte and MultiByteToWideChar
 *   POSIX ... iconv
 */
module std.internal.stdio.nativechar;

import std.algorithm : swap;
import std.conv;
import std.exception;
import std.range;
import std.string    : toStringz;
import std.traits    : isSomeString;
import std.utf;
import std.variant   : Algebraic;

version (Windows)
{
    import core.sys.windows.windows;
    import std.windows.syserror;

    // missing constants
    enum
    {
        ERROR_INVALID_PARAMETER   =  87,
        ERROR_INSUFFICIENT_BUFFER = 122,
    }
    enum CP_UTF8 = 65001;
}
else version (Posix)
{
    import core.stdc.errno;
    import core.stdc.locale;

    import core.sys.posix.iconv;
    import core.sys.posix.langinfo;

//  debug = USE_LIBICONV;
}


class EncodingException : Exception
{
    this(string msg, string file, uint line)
    {
        super(msg, file, line);
    }
}


//----------------------------------------------------------------------------//
// versions for platform-dependent convertion means
//----------------------------------------------------------------------------//

version (Windows)
{
}
else version (linux)
{
    version = HAVE_ICONV;   // GNU
}
else version (OSX)
{
    version = HAVE_ICONV;
}
else version (FreeBSD)
{
}
else version (Solaris)
{
    version = HAVE_ICONV;
}
else static assert(0);


// use libiconv for debugging
debug (USE_LIBICONV)
{
    version = HAVE_ICONV;
    pragma(lib, "iconv");
}

// iconv codeset name for dchars
version (HAVE_ICONV) private
{
    version (LittleEndian)
    {
        enum ICONV_DSTRING = "UTF-32LE";
    }
    else version (BigEndian)
    {
        enum ICONV_DSTRING = "UTF-32BE";
    }
    else static assert(0);
}


version (Windows)
{
    version = USE_WINNLS;   // MultiByteToWideChar & WideCharToMultiByte
}
else version (HAVE_ICONV)
{
    version = USE_ICONV;    // iconv (UTF-32 <=> native)
}


//----------------------------------------------------------------------------//
// native codeset detection
//----------------------------------------------------------------------------//

version (Windows)
{
    // The 'ANSI' codepage at program startup.
    immutable DWORD nativeCodepage;

    // True if the native codeset is UTF-8.
    @safe @property pure nothrow bool isNativeUTF8()
    {
        return .nativeCodepage == CP_UTF8;
    }

    shared static this()
    {
        .nativeCodepage = GetACP();
    }
}
else version (Posix)
{
    // The default CODESET langinfo at program startup.
    immutable string nativeCodeset;

    // True if the native codeset is UTF-8.
    @safe @property pure nothrow bool isNativeUTF8()
    {
        return .nativeCodeset == "UTF-8";
    }

    shared static this()
    {
        immutable origLoc = to!string( setlocale(LC_CTYPE, null) );
        setlocale(LC_CTYPE, "");
        scope(exit) setlocale(LC_CTYPE, origLoc.toStringz());

        if (auto codeset = nl_langinfo(CODESET))
            .nativeCodeset = to!string(codeset).resolveCodeset();
        else
            .nativeCodeset = "US-ASCII";
    }

    // CSI systems tend to return iconv-incompatible CODESET langinfo
    private @safe nothrow string resolveCodeset(string codeset)
    {
        switch (codeset)
        {
          case "646": return "US-ASCII";
          case "PCK": return    "CP932";
          default   : return codeset;
        }
        assert(0);
    }
}
else
{
    // Dunno how to detect the native codeset.
    enum bool isNativeUTF8  = false;
}


// Detect GNU iconv as its behavior slightly differs from the POSIX
version (USE_ICONV)
{
    immutable bool isIconvGNU;      // true <=> iconv by GNU

    shared static this()
    {
        iconv_t cd = iconv_open("ASCII//TRANSLIT", "UCS-4-INTERNAL");

        if (cd != cast(iconv_t) -1)
        {
            .isIconvGNU = true;
            iconv_close(cd);
        }
    }
}


//----------------------------------------------------------------------------//
// NativeCharacterReader
//----------------------------------------------------------------------------//

/*
 * Input range that reads multibyte characters in the native codeset from
 * another input range $(D Source) and iterates through the corresponding
 * Unicode code points.
 */
@system struct NativeCharacterReader(Source)
//      if (isInputRange!(Source) && is(Unqual!(ElementType!Source) == ubyte))
{
    this(Source source)
    {
        if (.isNativeUTF8)
        {
            // We can use our own converter in std.utf.
            _reader = byCodePoint(AssumeUTF8(source));
            return;
        }

        version (USE_WINNLS)
        {
            _reader = WindowsNativeCharacterReader!Source(source);
        }
        else version (USE_ICONV)
        {
            _reader = IconvNativeCharacterReader!Source(source);
        }
        else
        {
            throw new EncodingException("Native to Unicode codeset convertion "
                ~"is not supported on this platform", __FILE__, __LINE__);
        }
    }

    dchar* getNext(ref dchar store)
    {
        return _reader.getNext(store);
    }


    //----------------------------------------------------------------//
private:
    static @system struct AssumeUTF8
    {
        Source source;

        char* getNext(ref char u)
        {
            return cast(char*) source.getNext(*cast(ubyte*) &u);
        }
    }

    version (USE_WINNLS)
    {
        Algebraic!(
                ByCodePoint!AssumeUTF8,
                WindowsNativeCharacterReader!Source
            ) _reader;
    }
    else version (USE_ICONV)
    {
        Algebraic!(
                ByCodePoint!AssumeUTF8,
                IconvNativeCharacterReader!Source
            ) _reader;
    }
    else
    {
        ByCodePoint!AssumeUTF8 _reader;
    }
}


/*
 * Converts native multibyte character sequence to the corresponding Unicode
 * code point(s) with MultiByteToWideChar.
 *
 * This implementation can't handle stateful encodings (e.g. ISO-2022) nor
 * longer multibyte encodings (e.g. GB18030, tri-byte EUC-JP).
 */
private @system struct WindowsNativeCharacterReader(Source)
//      if (isInputRange!(Source) && is(Unqual!(ElementType!Source) == ubyte))
{
    static assert(is(WCHAR == wchar));

    this(Source source)
    {
        _context          = new Context;
        _context.codepage = .nativeCodepage;
        swap(_context.source, source);
    }


    /*
     * Reads a next multibyte character (if any) from the source, and returns
     * the corresponding Unicode code point in $(D result).
     *
     * Returns:
     *  The address of $(D result) if a character is read, or $(D null) if
     *  the source is empty.
     */
    dchar* getNext(ref dchar result)
    {
        if (_context.queue.length > 0)
        {
            // Consume extra code point queued in the internal context.
            result = _context.queue[0];
            _context.queue = _context.queue[1 .. $];
            return &result;
        }

        // multibyte character sequence read from the source
        ubyte[2] mbcseq     = void;
        size_t   mbcseqRead = 0;

        if (auto p = std.range.getNext!ubyte(_context.source))
            mbcseq[mbcseqRead++] = *p;
        else
            return null; // empty!

        if (IsDBCSLeadByteEx(_context.codepage, mbcseq[0]) != FALSE)
            mbcseq[mbcseqRead++] = *enforce(std.range.getNext!ubyte(_context.source));

        // UTF-16 sequence corresponding to the input
        wchar[8] wcharsStack = void;
        wchar[]  wchars      = wcharsStack;

        while (true)
        {
            int rc;

            rc = MultiByteToWideChar(_context.codepage, 0,
                    cast(LPCSTR) mbcseq.ptr, mbcseqRead, wchars.ptr, wchars.length);
            if (rc <= 0)
            {
                switch (GetLastError())
                {
                  case ERROR_INVALID_PARAMETER:
                        throw new EncodingException("input string contains invalid "
                                ~"byte sequence in the native codeset", __FILE__, __LINE__);

                  case ERROR_INSUFFICIENT_BUFFER:
                      rc = MultiByteToWideChar(_context.codepage, 0,
                              cast(LPCSTR) mbcseq.ptr, mbcseqRead, null, 0);
                      if (rc <= 0) goto default;
                      wchars.length = rc;
                      continue;

                  default:
                    throw new Error(sysErrorString(GetLastError()), __FILE__, __LINE__);
                }
                assert(0);
            }

            wchars = wchars[0 .. rc];
            break;
        }
        assert(wchars.length > 0);

        if ((wchars[0] & 0xFC00) == 0xD800)
        {
            assert(wchars.length > 1);
            result = ((wchars[0] - 0xD7C0) << 10) + (wchars[1] - 0xDC00);
            wchars = wchars[2 .. $];
        }
        else
        {
            result = wchars[0];
            wchars = wchars[1 .. $];
        }

        // Queue the remaining code points if any.
        if (wchars.length > 0)
            _context.queue = wchars.toUTF32();

        return &result;
    }


private:
    Context* _context;

    struct Context
    {
        Source  source;
        DWORD   codepage;
        dstring queue;
    }
}

version (Windows) unittest
{
    if (.nativeCodepage == 932)
    {
        enum dstring witness = "\u002e\u7af9\u85ea\u3084\u3051\u305f\u002e";
        auto input = cast(ubyte[]) "\x2e\x92\x7c\xe5\x4d\x82\xe2\x82\xaf\x82\xbd\x2e".dup;
        auto   r = WindowsNativeCharacterReader!(ubyte[])(input);
        size_t k = 0;
        for (dchar c; r.getNext(c); ++k)
        {
            assert(c == witness[k]);
        }
        assert(k == witness.length);
    }
}

version (Windows) unittest
{
    if (.nativeCodepage == 1252)
    {
        enum dstring witness =
             "\u20ac\u201a\u0192\u201e\u2026\u2020\u2021\u02c6\u2030\u0160\u2039\u0152\u017d"
            ~"\u2018\u2019\u201c\u201d\u2022\u2013\u2014\u02dc\u2122\u0161\u203a\u0153\u017e\u0178";
        auto input = cast(ubyte[])
            ( "\x80\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8e"
             ~"\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9e\x9f").dup;
        auto   r = WindowsNativeCharacterReader!(ubyte[])(input);
        size_t k = 0;
        for (dchar c; r.getNext(c); ++k)
        {
            assert(c == witness[k]);
        }
        assert(k == 27);
    }
}


/*
 * Converts native multibyte character sequence to the corresponding Unicode
 * code point(s) with POSIX iconv.
 */
private @system struct IconvNativeCharacterReader(Source)
//      if (isInputRange!(Source) && is(Unqual!(ElementType!Source) == ubyte))
{
    this(Source source)
    {
        string encoding = .nativeCodeset;

        _context         = new Context;
        _context.decoder = iconv_open(ICONV_DSTRING, encoding.toStringz());
        errnoEnforce(_context.decoder != cast(iconv_t) -1,
                "starting convertion from the native codeset");
        swap(_context.source, source);
    }

    this(this)
    {
        if (_context)
            ++_context.refCount;
    }

    ~this()
    {
        if (_context && --_context.refCount == 0)
            iconv_close(_context.decoder);
    }


    /*
     * Reads a next multibyte character (if any) from the source, and returns
     * the corresponding Unicode code point in $(D result).
     *
     * Returns:
     *  The address of $(D result) if a character is read, or $(D null) if the
     *  source is empty.
     *
     * Throws:
     *  $(D ErrnoException) on convertion error.
     */
    dchar* getNext(ref dchar result)
    {
        if (_context.queue.length > 0)
        {
            // Consume extra code point (ligature or something) queued in
            // the internal context.
            result = _context.queue[0];
            _context.queue = _context.queue[1 .. $];
            return &result;
        }

        // multibyte character sequence read from the source
        ubyte[16] mbcseqStack = void;
        ubyte[]   mbcseq      = mbcseqStack;
        size_t    mbcseqRead  = 0;  // number of bytes read from the source
        size_t    mbcseqUsed  = 0;  // number of bytes converted

        if (auto p = std.range.getNext!ubyte(_context.source))
            mbcseq[mbcseqRead++] = *p;
        else
            return null; // empty!

        // UTF-32 sequence corresponding to the input
        dchar[4] dcharsStack = void;
        dchar[]  dchars      = dcharsStack;
        size_t   dcharsUsed  = 0;   // number of CPs stored in dchars[]

        do
        {
            auto src     = &mbcseq[mbcseqUsed];
            auto srcLeft = mbcseqRead - mbcseqUsed;
            auto dst     = cast(ubyte*) &dchars[dcharsUsed];
            auto dstLeft = (dchars.length - dcharsUsed) * dchar.sizeof;

            immutable rc = iconv(_context.decoder, &src, &srcLeft, &dst, &dstLeft);

            mbcseqUsed += mbcseqRead    - srcLeft;
            dcharsUsed += dchars.length - dstLeft/dchar.sizeof;

            if (rc == cast(size_t) -1)
            {
                switch (errno)
                {
                  case EINVAL:
                    if (mbcseq.length == mbcseqRead)
                        mbcseq.length *= 2;
                    mbcseq[mbcseqRead++] = *enforce(std.range.getNext!ubyte(_context.source));
                    continue;

                  case EILSEQ:
                    throw new EncodingException("input string contains invalid "
                            ~"byte sequence in the native codeset", __FILE__, __LINE__);

                  case E2BIG:
                    dchars.length *= 2;
                    continue;

                  default:
                    throw new ErrnoException("converting a native coded "
                        "character to the corresponding Unicode code point");
                }
                assert(0);
            }
        }
        while (mbcseqUsed < mbcseqRead);

        assert(dcharsUsed > 0);
        result = dchars[0];

        if (dcharsUsed > 1)
            _context.queue = dchars[1 .. dcharsUsed].idup;

        return &result;
    }


private:
    Context* _context;

    struct Context
    {
        Source  source;
        iconv_t decoder;
        dstring queue;
        uint    refCount = 1;
    }
}

version (HAVE_ICONV) unittest
{
    if (.nativeCodeset == "EUC-JP" || .nativeCodeset == "eucJP")
    {
        enum dstring witness = "\u002e\u7af9\u85ea\u3084\u3051\u305f\u002e";
        auto input = cast(ubyte[]) "\x2e\xc3\xdd\xe9\xae\xa4\xe4\xa4\xb1\xa4\xbf\x2e".dup;
        auto   r = IconvNativeCharacterReader!(ubyte[])(input);
        size_t k = 0;
        for (dchar c; r.getNext(c); ++k)
        {
            assert(c == witness[k]);
        }
        assert(k == witness.length);
    }
}

version (HAVE_ICONV) unittest
{
    if (.nativeCodeset == "UTF-8")
    {
        enum dstring witness = "\u0000\u007F\u0080\u07FF\u0800\uD7FF\uE000\uFFFD\U00010000\U0010FFFF";
        auto input = cast(ubyte[]) (cast(string) witness).dup;
        auto   r = IconvNativeCharacterReader!(ubyte[])(input);
        size_t k = 0;
        for (dchar c; r.getNext(c); ++k)
        {
            assert(c == witness[k]);
        }
        assert(k == witness.length);
    }
}

version (HAVE_ICONV) unittest
{
    if (.nativeCodeset == "ISO-8859-1" || .nativeCodeset == "ISO8859-1")
    {
        static struct Cover(T)
        {
            @property bool empty() { return _shadow == T.max; }
            @property T front() { return _front; }
            void popFront() { _shadow = _front++; }
            T _front  = T.min;
            T _shadow = T.min;
        }
        Cover!dchar witness;
        auto   r = IconvNativeCharacterReader!(Cover!ubyte)(Cover!ubyte());
        size_t k = 0;
        for (dchar c; r.getNext(c); ++k)
        {
            // The first 256 Unicode code points are ISO-8859-1.
            assert(c == witness.front);
            witness.popFront;
        }
        assert(k == 256);
    }
}


//----------------------------------------------------------------------------//
// NativeTextWriter
//----------------------------------------------------------------------------//

version (unittest)
{
    import std.array;
}

/*
 * Output range that writes Unicode characters to another output range $(D Sink)
 * in the native codeset.
 */
@system struct NativeTextWriter(Sink)
        if (isOutputRange!(Sink, ubyte[]))
{
    this(Sink sink)
    {
        if (.isNativeUTF8)
        {
            // We can use our own converter in std.utf.
            _writer = writeTextIn!char(sink);
            return;
        }

        version (USE_WINNLS)
        {
            _writer = writeTextIn!WCHAR(WindowsNativeTextWriter!Sink(sink));
        }
        else version (USE_ICONV)
        {
            _writer = writeTextIn!dchar(IconvNativeTextWriter!Sink(sink));
        }
        else
        {
            throw new EncodingException("Unicode to native codeset convertion "
                ~"is not supported on this platform", __FILE__, __LINE__);
        }
    }

    void put(S)(S str)
        if (isSomeString!(S))
    {
        _writer.put(str);
    }

    void put(C = dchar)(dchar c)
    {
        _writer.put(c);
    }


    //----------------------------------------------------------------//
private:
    version (USE_WINNLS)
    {
        Algebraic!(
                UTFTextWriter!(char, Sink),
                UTFTextWriter!(WCHAR, WindowsNativeTextWriter!Sink)
            ) _writer;

    }
    else version (USE_ICONV)
    {
        Algebraic!(
                UTFTextWriter!(char, Sink),
                UTFTextWriter!(dchar, IconvNativeTextWriter!Sink)
            ) _writer;
    }
    else
    {
        UTFTextWriter!(char, Sink) _writer;
    }
}


/*
 * Converts wchar[]s into the corresponding native multibyte character sequences
 * with WideCharToMultiByte.
 *
 * This implementation can't handle stateful encodings such as ISO-2022.
 */
private @system struct WindowsNativeTextWriter(Sink)
        if (isOutputRange!(Sink, ubyte[]))
{
    this(Sink sink)
    {
        _codepage = .nativeCodepage;
        swap(_sink, sink);
    }

    /*
     * Converts UTF-16 string $(D wstr) to the corresponding native multibyte
     * character sequence and puts them in the $(D sink).
     */
    void put(in WCHAR[] wstr)
    {
        if (wstr.length == 0)
            return;

        ubyte[128] mbstrStack = void;
        ubyte[]    mbstr      = mbstrStack;
        int        mbstrLen;    // size of the multibyte string

        mbstrLen = WideCharToMultiByte(_codepage, 0,
                wstr.ptr, wstr.length, null, 0, null, null);
        if (mbstrLen <= 0)
        {
            switch (GetLastError())
            {
              case ERROR_INVALID_PARAMETER:
                throw new EncodingException("invalid UTF sequence in the input string",
                        __FILE__, __LINE__);

              default:
                throw new Error(sysErrorString(GetLastError()), __FILE__, __LINE__);
            }
            assert(0);
        }

        if (mbstr.length < mbstrLen)
            mbstr = new ubyte[](mbstrLen);

        mbstrLen = WideCharToMultiByte(_codepage, 0,
                wstr.ptr, wstr.length, cast(LPSTR) mbstr.ptr, mbstr.length, null, null);
        enforce(mbstrLen > 0, sysErrorString(GetLastError()));

        _sink.put(mbstr[0 .. mbstrLen]);
    }

private:
    Sink  _sink;
    DWORD _codepage;
}

version (Windows) unittest
{
    if (.nativeCodepage == 932)
    {
        enum wstring input = "\u002e\u7af9\u85ea\u3084\u3051\u305f\u002e";
        enum string witness = "\x2e\x92\x7c\xe5\x4d\x82\xe2\x82\xaf\x82\xbd\x2e";
        ubyte[] s;
        auto a = appender(&s);
        auto w = WindowsNativeTextWriter!(typeof(a))(a);
        w.put(input[0 .. 4]);
        w.put(input[4 .. 6]);
        w.put(input[6 .. 7]);
        assert(s == cast(immutable ubyte[]) witness);
    }
}

version (Windows) unittest
{
    if (.nativeCodepage == 1252)
    {
        enum wstring input =
             "\u20ac\u201a\u0192\u201e\u2026\u2020\u2021\u02c6\u2030\u0160\u2039\u0152\u017d"
            ~"\u2018\u2019\u201c\u201d\u2022\u2013\u2014\u02dc\u2122\u0161\u203a\u0153\u017e\u0178";
        enum string witness =
             "\x80\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8e"
            ~"\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9e\x9f";
        ubyte[] s;
        auto a = appender(&s);
        auto w = WindowsNativeTextWriter!(typeof(a))(a);
        w.put(input[ 0 .. 13]);
        w.put(input[13 .. 20]);
        w.put(input[20 .. 24]);
        w.put(input[24 .. 26]);
        w.put(input[26 .. 27]);
        assert(s == cast(immutable ubyte[]) witness);
    }
}

version (Windows) unittest
{
    // non-representable character
    if (.nativeCodepage == 1252)
    {
        enum wstring input = "\u002e\u7af9\u85ea\u3084\u3051\u305f\u002e";
        ubyte[] s;
        auto a = appender(&s);
        auto w = WindowsNativeTextWriter!(typeof(a))(a);
        w.put(input);

        // '\u002e' at the both end must be successfully converted.  Other
        // non-convertible characters may be transliterated to replacement
        // characters, or just simply dropped.
        assert(s.length >= 2);
        assert(s[    0] == '\x2e');
        assert(s[$ - 1] == '\x2e');
    }
}


/*
 * Converts dchar[]s into the corresponding multibyte character sequences with
 * POSIX iconv.
 */
private @system struct IconvNativeTextWriter(Sink)
        if (isOutputRange!(Sink, ubyte[]))
{
    this(Sink sink)
    {
        string encoding = .nativeCodeset;
        if (.isIconvGNU) encoding ~= "//TRANSLIT";

        _context         = new Context;
        _context.encoder = iconv_open(encoding.toStringz(), ICONV_DSTRING);
        errnoEnforce(_context.encoder != cast(iconv_t) -1,
                "starting convertion to the native codeset");
        swap(_context.sink, sink);
    }

    this(this)
    {
        if (_context)
            ++_context.refCount;
    }

    ~this()
    {
        if (_context && --_context.refCount == 0)
            errnoEnforce(iconv_close(_context.encoder) != -1);
    }


    /*
     * Converts UTF-32 string $(D str) to the corresponding native multibyte
     * character sequence and puts them in the $(D sink).
     */
    void put(in dchar[] str)
    {
        ubyte[128] mcharsStack = void;
        ubyte[]    mchars      = mcharsStack;

        auto src     = cast(const(ubyte)*) str.ptr;
        auto srcLeft = dchar.sizeof * str.length;

        while (srcLeft > 0)
        {
            ubyte* dst     = mchars.ptr;
            size_t dstLeft = mchars.length;

            immutable rc = iconv(_context.encoder, &src, &srcLeft, &dst, &dstLeft);
            immutable iconvErrno = errno;

            // Output successfully converted characters (available even on error).
            if (dstLeft < mchars.length)
                _context.sink.put(mchars[0 .. $ - dstLeft]);

            if (rc == cast(size_t) -1)
            {
                switch (errno = iconvErrno)
                {
                  case EILSEQ:
                    if (.isIconvGNU && isValidDchar(*cast(dchar*) src))
                    {
                        // [workaround] GNU iconv raises EILSEQ on mapping failure
                        src     += dchar.sizeof;
                        srcLeft -= dchar.sizeof;
                        continue;
                    }
                    throw new EncodingException("invalid Unicode code point in "
                            ~"the input string", __FILE__, __LINE__);

                  case E2BIG:
                    mchars.length *= 2;
                    continue;

                  default:
                    throw new ErrnoException("iconv");
                }
            }
        }
    }

private:
    Context* _context;

    struct Context
    {
        Sink    sink;
        iconv_t encoder;
        uint    refCount = 1;
    }
}

version (HAVE_ICONV) unittest
{
    if (.nativeCodeset == "Shift_JIS" || .nativeCodeset == "SJIS")
    {
        enum dstring input = "\u002e\u7af9\u85ea\u3084\u3051\u305f\u002e";
        enum string witness = "\x2e\x92\x7c\xe5\x4d\x82\xe2\x82\xaf\x82\xbd\x2e";
        ubyte[] s;
        auto a = appender(&s);
        auto w = IconvNativeTextWriter!(typeof(a))(a);
        w.put(input[0 .. 4]);
        w.put(input[4 .. 6]);
        w.put(input[6 .. 7]);
        assert(s == cast(immutable ubyte[]) witness);
    }
}

version (HAVE_ICONV) unittest
{
    if (.nativeCodeset == "ISO-8859-1" || .nativeCodeset == "ISO8859-1")
    {
        enum dstring input = "\u0000\u001f\u0020\u007f\u0080\u009f\u00a0\u00ff";
        enum string witness = "\x00\x1f\x20\x7f\x80\x9f\xa0\xff";
        ubyte[] s;
        auto a = appender(&s);
        auto w = IconvNativeTextWriter!(typeof(a))(a);
        w.put(input[0 .. 4]);
        w.put(input[4 .. 7]);
        w.put(input[7 .. 8]);
        assert(s == cast(immutable ubyte[]) witness);
    }
}

version (HAVE_ICONV) unittest
{
    // non-representable character
    if (.nativeCodeset == "ISO-8859-1" || .nativeCodeset == "ISO8859-1")
    {
        enum wstring input = "\u002e\u7af9\u85ea\u3084\u3051\u305f\u002e";
        ubyte[] s;
        auto a = appender(&s);
        auto w = IconvNativeTextWriter!(typeof(a))(a);
        w.put(input);

        // '\u002e' at the both end must be successfully converted.  Other
        // non-convertible characters may be transliterated to replacement
        // characters, or just simply dropped.
        assert(s.length >= 2);
        assert(s[    0] == '\x2e');
        assert(s[$ - 1] == '\x2e');
    }
}


