// Written in the D programming language.

/**
 * Encode and decode UTF-8, UTF-16 and UTF-32 strings.
 *
 * For Win32 systems, the C wchar_t type is UTF-16 and corresponds to the D
 * wchar type.
 * For linux systems, the C wchar_t type is UTF-32 and corresponds to
 * the D utf.dchar type.
 *
 * UTF character support is restricted to (\u0000 &lt;= character &lt;= \U0010FFFF).
 *
 * See_Also:
 *  $(LINK2 http://en.wikipedia.org/wiki/Unicode, Wikipedia)<br>
 *  $(LINK http://www.cl.cam.ac.uk/~mgk25/unicode.html#utf-8)<br>
 *  $(LINK http://anubis.dkuug.dk/JTC1/SC2/WG2/docs/n1335)
 * Macros:
 *  WIKI = Phobos/StdUtf
 *
 * Copyright: Copyright Digital Mars 2000 - 2009.
 * License:   <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
 * Authors:   $(WEB digitalmars.com, Walter Bright)
 *
 *          Copyright Digital Mars 2000 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.utf;

import std.conv, std.exception, std.range, std.traits, std.typecons;

//debug=utf;        // uncomment to turn on debugging printf's

debug (utf) import core.stdc.stdio : printf;

deprecated class UtfError : Error
{
    size_t idx; // index in string of where error occurred

    this(string s, size_t i)
    {
        idx = i;
        super(s);
    }
}

/**********************************
 * Exception class that is thrown upon any errors.
 */

class UtfException : Exception
{
    //size_t idx;   /// index in string of where error occurred
    uint[4] sequence;
    size_t len;

    this(string s, dchar[] data...)
    {
        len = data.length;
        foreach (i, e; data) sequence[i] = e;
        super(s);
    }

    override string toString()
    {
        string result;
        if (len > 0)
        {
            result = "Invalid UTF sequence:";
            foreach (i; 0 .. len)
                result ~= " " ~ to!string(sequence[i], 16);
        }
        if (super.msg.length > 0)
        {
            if (result.length > 0)
                result ~= " - ";
            result ~= super.msg;
        }
        return result;
    }
}

// For unittests
version (unittest) private bool expectError_(lazy void expr)
{
    try
    {
        expr;
    }
    catch (UtfException e)
    {
        return true;
    }
    return false;
}


/*******************************
 * Test if c is a valid UTF-32 character.
 *
 * \uFFFE and \uFFFF are considered valid by this function,
 * as they are permitted for internal use by an application,
 * but they are not allowed for interchange by the Unicode standard.
 *
 * Returns: true if it is, false if not.
 */

bool isValidDchar(dchar c)
{
    /* Note: FFFE and FFFF are specifically permitted by the
     * Unicode standard for application internal use, but are not
     * allowed for interchange.
     * (thanks to Arcane Jill)
     */

    return c < 0xD800 ||
    (c > 0xDFFF && c <= 0x10FFFF /*&& c != 0xFFFE && c != 0xFFFF*/);
}

unittest
{
    debug(utf) printf("utf.isValidDchar.unittest\n");
    assert(isValidDchar(cast(dchar)'a') == true);
    assert(isValidDchar(cast(dchar)0x1FFFFF) == false);

    assert(!isValidDchar(cast(dchar)0x00D800));
    assert(!isValidDchar(cast(dchar)0x00DBFF));
    assert(!isValidDchar(cast(dchar)0x00DC00));
    assert(!isValidDchar(cast(dchar)0x00DFFF));
    assert(isValidDchar(cast(dchar)0x00FFFE));
    assert(isValidDchar(cast(dchar)0x00FFFF));
    assert(isValidDchar(cast(dchar)0x01FFFF));
    assert(isValidDchar(cast(dchar)0x10FFFF));
    assert(!isValidDchar(cast(dchar)0x110000));
}


private immutable ubyte[256] UTF8stride =
[
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
    4,4,4,4,4,4,4,4,5,5,5,5,6,6,0xFF,0xFF,
];


/**
 * stride() returns the length of a UTF-8 sequence starting at index i
 * in string s.
 * Returns:
 *  The number of bytes in the UTF-8 sequence or
 *  0xFF meaning s[i] is not the start of of UTF-8 sequence.
 */

uint stride(in char[] s, size_t i)
{
    immutable result = UTF8stride[s[i]];
    assert(result > 0 && result <= 6);
    return result;
}

/**
 * stride() returns the length of a UTF-16 sequence starting at index i
 * in string s.
 */

uint stride(in wchar[] s, size_t i)
{
    immutable uint u = s[i];
    return 1 + (u >= 0xD800 && u <= 0xDBFF);
}

/**
 * stride() returns the length of a UTF-32 sequence starting at index i
 * in string s.
 * Returns: The return value will always be 1.
 */

uint stride(in dchar[] s, size_t i)
{
    return 1;
}

/*******************************************
 * Given an index i into an array of characters s[],
 * and assuming that index i is at the start of a UTF character,
 * determine the number of UCS characters up to that index i.
 */

size_t toUCSindex(in char[] s, size_t i)
{
    size_t n;
    size_t j;

    for (j = 0; j < i; )
    {
        j += stride(s, j);
        n++;
    }
    if (j > i)
    {
        throw new UtfException("1invalid UTF-8 sequence");
    }
    return n;
}

/** ditto */

size_t toUCSindex(in wchar[] s, size_t i)
{
    size_t n;
    size_t j;

    for (j = 0; j < i; )
    {
    j += stride(s, j);
    n++;
    }
    if (j > i)
    {
    throw new UtfException("2invalid UTF-16 sequence");
    }
    return n;
}

/** ditto */

size_t toUCSindex(in dchar[] s, size_t i)
{
    return i;
}

/******************************************
 * Given a UCS index n into an array of characters s[], return the UTF index.
 */

size_t toUTFindex(in char[] s, size_t n)
{
    size_t i;

    while (n--)
    {
    uint j = UTF8stride[s[i]];
    if (j == 0xFF)
        throw new UtfException("3invalid UTF-8 sequence ", s[i]);
    i += j;
    }
    return i;
}

/** ditto */

size_t toUTFindex(in wchar[] s, size_t n)
{
    size_t i;

    while (n--)
    {   wchar u = s[i];

    i += 1 + (u >= 0xD800 && u <= 0xDBFF);
    }
    return i;
}

/** ditto */

size_t toUTFindex(in dchar[] s, size_t n)
{
    return n;
}

/* =================== Decode ======================= */

/***************
 * Decodes and returns character starting at s[idx]. idx is advanced past the
 * decoded character. If the character is not well formed, a UtfException is
 * thrown and idx remains unchanged.
 */

dchar decode(in char[] s, ref size_t idx)
out (result)
{
    assert(isValidDchar(result));
}
body
{
    enforce(idx < s.length, "Attempted to decode past the end of a string");

    size_t len = s.length;
    dchar V;
    size_t i = idx;
    char u = s[i];

    if (u & 0x80)
    {
        /* The following encodings are valid, except for the 5 and 6 byte
         * combinations:
         *  0xxxxxxx
         *  110xxxxx 10xxxxxx
         *  1110xxxx 10xxxxxx 10xxxxxx
         *  11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
         *  111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
         *  1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
         */
        uint n = 1;
        for (; ; n++)
        {
            if (n > 4)
                goto Lerr;      // only do the first 4 of 6 encodings
            if (((u << n) & 0x80) == 0)
            {
                if (n == 1)
                    goto Lerr;
                break;
            }
        }

        // Pick off (7 - n) significant bits of B from first byte of octet
        V = cast(dchar)(u & ((1 << (7 - n)) - 1));

        if (i + n > len)
            goto Lerr;          // off end of string

        /* The following combinations are overlong, and illegal:
         *  1100000x (10xxxxxx)
         *  11100000 100xxxxx (10xxxxxx)
         *  11110000 1000xxxx (10xxxxxx 10xxxxxx)
         *  11111000 10000xxx (10xxxxxx 10xxxxxx 10xxxxxx)
         *  11111100 100000xx (10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx)
         */
        auto u2 = s[i + 1];
        if ((u & 0xFE) == 0xC0 ||
                (u == 0xE0 && (u2 & 0xE0) == 0x80) ||
                (u == 0xF0 && (u2 & 0xF0) == 0x80) ||
                (u == 0xF8 && (u2 & 0xF8) == 0x80) ||
                (u == 0xFC && (u2 & 0xFC) == 0x80))
            goto Lerr;          // overlong combination

        foreach (j; 1 .. n)
        {
            u = s[i + j];
            if ((u & 0xC0) != 0x80)
                goto Lerr;          // trailing bytes are 10xxxxxx
            V = (V << 6) | (u & 0x3F);
        }
        if (!isValidDchar(V))
            goto Lerr;
        i += n;
    }
    else
    {
        V = cast(dchar) u;
        i++;
    }

    idx = i;
    return V;

  Lerr:
    //printf("\ndecode: idx = %d, i = %d, length = %d s = \n'%.*s'\n%x\n'%.*s'\n", idx, i, s.length, s, s[i], s[i .. $]);
    throw new UtfException("4invalid UTF-8 sequence", s[i]);
}

unittest
{   size_t i;
    dchar c;

    debug(utf) printf("utf.decode.unittest\n");

    static string s1 = "abcd";
    i = 0;
    c = decode(s1, i);
    assert(c == cast(dchar)'a');
    assert(i == 1);
    c = decode(s1, i);
    assert(c == cast(dchar)'b');
    assert(i == 2);

    static string s2 = "\xC2\xA9";
    i = 0;
    c = decode(s2, i);
    assert(c == cast(dchar)'\u00A9');
    assert(i == 2);

    static string s3 = "\xE2\x89\xA0";
    i = 0;
    c = decode(s3, i);
    assert(c == cast(dchar)'\u2260');
    assert(i == 3);

    static string[] s4 =
    [   "\xE2\x89",     // too short
    "\xC0\x8A",
    "\xE0\x80\x8A",
    "\xF0\x80\x80\x8A",
    "\xF8\x80\x80\x80\x8A",
    "\xFC\x80\x80\x80\x80\x8A",
    ];

    for (int j = 0; j < s4.length; j++)
    {
    try
    {
        i = 0;
        c = decode(s4[j], i);
        assert(0);
    }
    catch (UtfException u)
    {
        i = 23;
        delete u;
    }
    assert(i == 23);
    }
}

unittest
{
    size_t i;

    i = 0; assert(decode("\xEF\xBF\xBE"c, i) == cast(dchar) 0xFFFE);
    i = 0; assert(decode("\xEF\xBF\xBF"c, i) == cast(dchar) 0xFFFF);
    i = 0;
    assert(expectError_( decode("\xED\xA0\x80"c, i) ));
    assert(expectError_( decode("\xED\xAD\xBF"c, i) ));
    assert(expectError_( decode("\xED\xAE\x80"c, i) ));
    assert(expectError_( decode("\xED\xAF\xBF"c, i) ));
    assert(expectError_( decode("\xED\xB0\x80"c, i) ));
    assert(expectError_( decode("\xED\xBE\x80"c, i) ));
    assert(expectError_( decode("\xED\xBF\xBF"c, i) ));
}


/** ditto */

dchar decode(in wchar[] s, ref size_t idx)
out (result)
{
    assert(isValidDchar(result));
}
body
{
    enforce(idx < s.length, "Attempted to decode past the end of a string");

    string msg;
    dchar V;
    size_t i = idx;
    uint u = s[i];

    if (u & ~0x7F)
    {   if (u >= 0xD800 && u <= 0xDBFF)
        {   uint u2;

            if (i + 1 == s.length)
            {   msg = "surrogate UTF-16 high value past end of string";
                goto Lerr;
            }
            u2 = s[i + 1];
            if (u2 < 0xDC00 || u2 > 0xDFFF)
            {   msg = "surrogate UTF-16 low value out of range";
                goto Lerr;
            }
            u = ((u - 0xD7C0) << 10) + (u2 - 0xDC00);
            i += 2;
        }
        else if (u >= 0xDC00 && u <= 0xDFFF)
        {   msg = "unpaired surrogate UTF-16 value";
            goto Lerr;
        }
        else
            i++;
        // Note: u+FFFE and u+FFFF are specifically permitted by the
        // Unicode standard for application internal use (see isValidDchar)
    }
    else
    {
        i++;
    }

    idx = i;
    return cast(dchar)u;

  Lerr:
    throw new UtfException(msg, s[i]);
}

unittest
{
    size_t i;

    i = 0; assert(decode([ cast(wchar) 0xFFFE ], i) == cast(dchar) 0xFFFE && i == 1);
    i = 0; assert(decode([ cast(wchar) 0xFFFF ], i) == cast(dchar) 0xFFFF && i == 1);
}


/** ditto */

dchar decode(in dchar[] s, ref size_t idx)
{
    enforce(idx < s.length, "Attempted to decode past the end of a string");

    size_t i = idx;
    dchar c = s[i];

    if (!isValidDchar(c))
        goto Lerr;
    idx = i + 1;
    return c;

  Lerr:
    throw new UtfException("5invalid UTF-32 value", c);
}

/* =================== Encode ======================= */

/*******************************
Encodes character $(D c) into fixed-size array $(D s). Returns the
actual length of the encoded character (a number between 1 and 4 for
$(D char[4]) buffers, and between 1 and 2 for $(D wchar[2]) buffers).
 */

size_t encode(ref char[4] buf, in dchar c)
{
    if (c <= 0x7F)
    {
        assert(isValidDchar(c));
        buf[0] = cast(char) c;
        return 1;
    }
    if (c <= 0x7FF)
    {
        assert(isValidDchar(c));
        buf[0] = cast(char)(0xC0 | (c >> 6));
        buf[1] = cast(char)(0x80 | (c & 0x3F));
        return 2;
    }
    if (c <= 0xFFFF)
    {
        if (0xD800 <= c && c <= 0xDFFF)
            throw new UtfException(
                "encoding a surrogate code point in UTF-8", c);
        assert(isValidDchar(c));
        buf[0] = cast(char)(0xE0 | (c >> 12));
        buf[1] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        buf[2] = cast(char)(0x80 | (c & 0x3F));
        return 3;
    }
    if (c <= 0x10FFFF)
    {
        assert(isValidDchar(c));
        buf[0] = cast(char)(0xF0 | (c >> 18));
        buf[1] = cast(char)(0x80 | ((c >> 12) & 0x3F));
        buf[2] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        buf[3] = cast(char)(0x80 | (c & 0x3F));
        return 4;
    }

    assert(!isValidDchar(c));
    throw new UtfException(
        "encoding an invalid code point in UTF-8", c);
}

unittest
{
    char[4] buf;

    assert(encode(buf, '\u0000') == 1 && buf[0 .. 1] == "\u0000");
    assert(encode(buf, '\u007F') == 1 && buf[0 .. 1] == "\u007F");
    assert(encode(buf, '\u0080') == 2 && buf[0 .. 2] == "\u0080");
    assert(encode(buf, '\u07FF') == 2 && buf[0 .. 2] == "\u07FF");
    assert(encode(buf, '\u0800') == 3 && buf[0 .. 3] == "\u0800");
    assert(encode(buf, '\uD7FF') == 3 && buf[0 .. 3] == "\uD7FF");
    assert(encode(buf, '\uE000') == 3 && buf[0 .. 3] == "\uE000");
    assert(encode(buf, 0xFFFE) == 3 && buf[0 .. 3] == "\xEF\xBF\xBE");
    assert(encode(buf, 0xFFFF) == 3 && buf[0 .. 3] == "\xEF\xBF\xBF");
    assert(encode(buf, '\U00010000') == 4 && buf[0 .. 4] == "\U00010000");
    assert(encode(buf, '\U0010FFFF') == 4 && buf[0 .. 4] == "\U0010FFFF");

    assert(expectError_( encode(buf, cast(dchar) 0xD800) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDBFF) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDC00) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDFFF) ));
    assert(expectError_( encode(buf, cast(dchar) 0x110000) ));
}


/// Ditto
size_t encode(ref wchar[2] buf, dchar c)
{
    if (c <= 0xFFFF)
    {
        if (0xD800 <= c && c <= 0xDFFF)
            throw new UtfException(
                "encoding an isolated surrogate code point in UTF-16", c);
        assert(isValidDchar(c));
        buf[0] = cast(wchar) c;
        return 1;
    }
    if (c <= 0x10FFFF)
    {
        assert(isValidDchar(c));
        buf[0] = cast(wchar) ((((c - 0x10000) >> 10) & 0x3FF) + 0xD800);
        buf[1] = cast(wchar) (((c - 0x10000) & 0x3FF) + 0xDC00);
        return 2;
    }

    assert(!isValidDchar(c));
    throw new UtfException(
        "encoding an invalid code point in UTF-16", c);
}

unittest
{
    wchar[2] buf;

    assert(encode(buf, '\u0000') == 1 && buf[0 .. 1] == "\u0000");
    assert(encode(buf, '\uD7FF') == 1 && buf[0 .. 1] == "\uD7FF");
    assert(encode(buf, '\uE000') == 1 && buf[0 .. 1] == "\uE000");
    assert(encode(buf, 0xFFFE) == 1 && buf[0] == 0xFFFE);
    assert(encode(buf, 0xFFFF) == 1 && buf[0] == 0xFFFF);
    assert(encode(buf, '\U00010000') == 2 && buf[0 .. 2] == "\U00010000");
    assert(encode(buf, '\U0010FFFF') == 2 && buf[0 .. 2] == "\U0010FFFF");

    assert(expectError_( encode(buf, cast(dchar) 0xD800) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDBFF) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDC00) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDFFF) ));
    assert(expectError_( encode(buf, cast(dchar) 0x110000) ));
}


/*******************************
 * Encodes character c and appends it to array s[].
 */

void encode(ref char[] s, dchar c)
{
    char[] r = s;

    if (c <= 0x7F)
    {
        assert(isValidDchar(c));
        r ~= cast(char) c;
    }
    else
    {
        char[4] buf;
        uint L;

        if (c <= 0x7FF)
        {
            assert(isValidDchar(c));
            buf[0] = cast(char)(0xC0 | (c >> 6));
            buf[1] = cast(char)(0x80 | (c & 0x3F));
            L = 2;
        }
        else if (c <= 0xFFFF)
        {
            if (0xD800 <= c && c <= 0xDFFF)
                throw new UtfException(
                    "encoding a surrogate code point in UTF-8", c);
            assert(isValidDchar(c));
            buf[0] = cast(char)(0xE0 | (c >> 12));
            buf[1] = cast(char)(0x80 | ((c >> 6) & 0x3F));
            buf[2] = cast(char)(0x80 | (c & 0x3F));
            L = 3;
        }
        else if (c <= 0x10FFFF)
        {
            assert(isValidDchar(c));
            buf[0] = cast(char)(0xF0 | (c >> 18));
            buf[1] = cast(char)(0x80 | ((c >> 12) & 0x3F));
            buf[2] = cast(char)(0x80 | ((c >> 6) & 0x3F));
            buf[3] = cast(char)(0x80 | (c & 0x3F));
            L = 4;
        }
        else
        {
            assert(!isValidDchar(c));
            throw new UtfException(
                "encoding an invalid code point in UTF-8", c);
        }
        r ~= buf[0 .. L];
    }
    s = r;
}

unittest
{
    debug(utf) printf("utf.encode.unittest\n");

    char[] s = "abcd".dup;
    encode(s, cast(dchar)'a');
    assert(s.length == 5);
    assert(s == "abcda");

    encode(s, cast(dchar)'\u00A9');
    assert(s.length == 7);
    assert(s == "abcda\xC2\xA9");
    //assert(s == "abcda\u00A9");   // BUG: fix compiler

    encode(s, cast(dchar)'\u2260');
    assert(s.length == 10);
    assert(s == "abcda\xC2\xA9\xE2\x89\xA0");
}

unittest
{
    char[] buf;

    encode(buf, '\u0000'); assert(buf[0 .. $] == "\u0000");
    encode(buf, '\u007F'); assert(buf[1 .. $] == "\u007F");
    encode(buf, '\u0080'); assert(buf[2 .. $] == "\u0080");
    encode(buf, '\u07FF'); assert(buf[4 .. $] == "\u07FF");
    encode(buf, '\u0800'); assert(buf[6 .. $] == "\u0800");
    encode(buf, '\uD7FF'); assert(buf[9 .. $] == "\uD7FF");
    encode(buf, '\uE000'); assert(buf[12 .. $] == "\uE000");
    encode(buf, 0xFFFE); assert(buf[15 .. $] == "\xEF\xBF\xBE");
    encode(buf, 0xFFFF); assert(buf[18 .. $] == "\xEF\xBF\xBF");
    encode(buf, '\U00010000'); assert(buf[21 .. $] == "\U00010000");
    encode(buf, '\U0010FFFF'); assert(buf[25 .. $] == "\U0010FFFF");

    assert(expectError_( encode(buf, cast(dchar) 0xD800) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDBFF) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDC00) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDFFF) ));
    assert(expectError_( encode(buf, cast(dchar) 0x110000) ));
}


/** ditto */

void encode(ref wchar[] s, dchar c)
{
    wchar[] r = s;

    if (c <= 0xFFFF)
    {
        if (0xD800 <= c && c <= 0xDFFF)
            throw new UtfException(
                "encoding an isolated surrogate code point in UTF-16", c);
        assert(isValidDchar(c));
        r ~= cast(wchar) c;
    }
    else if (c <= 0x10FFFF)
    {
        wchar[2] buf;

        assert(isValidDchar(c));
        buf[0] = cast(wchar) ((((c - 0x10000) >> 10) & 0x3FF) + 0xD800);
        buf[1] = cast(wchar) (((c - 0x10000) & 0x3FF) + 0xDC00);
        r ~= buf;
    }
    else
    {
        assert(!isValidDchar(c));
        throw new UtfException(
            "encoding an invalid code point in UTF-16", c);
    }
    s = r;
}

unittest
{
    wchar[] buf;

    encode(buf, '\u0000'); assert(buf[0] == '\u0000');
    encode(buf, '\uD7FF'); assert(buf[1] == '\uD7FF');
    encode(buf, '\uE000'); assert(buf[2] == '\uE000');
    encode(buf, 0xFFFE); assert(buf[3] == 0xFFFE);
    encode(buf, 0xFFFF); assert(buf[4] == 0xFFFF);
    encode(buf, '\U00010000'); assert(buf[5 .. $] == "\U00010000");
    encode(buf, '\U0010FFFF'); assert(buf[7 .. $] == "\U0010FFFF");

    assert(expectError_( encode(buf, cast(dchar) 0xD800) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDBFF) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDC00) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDFFF) ));
    assert(expectError_( encode(buf, cast(dchar) 0x110000) ));
}


/** ditto */

void encode(ref dchar[] s, dchar c)
{
    if ((0xD800 <= c && c <= 0xDFFF) || 0x10FFFF < c)
        throw new UtfException(
            "encoding an invalid code point in UTF-32", c);
    assert(isValidDchar(c));
    s ~= c;
}

unittest
{
    dchar[] buf;

    encode(buf, '\u0000'); assert(buf[0] == '\u0000');
    encode(buf, '\uD7FF'); assert(buf[1] == '\uD7FF');
    encode(buf, '\uE000'); assert(buf[2] == '\uE000');
    encode(buf, 0xFFFE ); assert(buf[3] == 0xFFFE);
    encode(buf, 0xFFFF ); assert(buf[4] == 0xFFFF);
    encode(buf, '\U0010FFFF'); assert(buf[5] == '\U0010FFFF');

    assert(expectError_( encode(buf, cast(dchar) 0xD800) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDBFF) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDC00) ));
    assert(expectError_( encode(buf, cast(dchar) 0xDFFF) ));
    assert(expectError_( encode(buf, cast(dchar) 0x110000) ));
}


/**
Returns the code length of $(D c) in the encoding using $(D C) as a
code point. The code is returned in character count, not in bytes.
 */

ubyte codeLength(C)(dchar c)
{
    static if (C.sizeof == 1)
    {
        return
            c <= 0x7F ? 1
            : c <= 0x7FF ? 2
            : c <= 0xFFFF ? 3
            : c <= 0x10FFFF ? 4
            : (assert(false), 6);
    }
    else static if (C.sizeof == 2)
    {
    return c <= 0xFFFF ? 1 : 2;
    }
    else
    {
        static assert(C.sizeof == 4);
        return 1;
    }
}

/* =================== Validation ======================= */

/***********************************
Checks to see if string is well formed or not. $(D S) can be an array
 of $(D char), $(D wchar), or $(D dchar). Throws a $(D UtfException)
 if it is not. Use to check all untrusted input for correctness.
 */

void validate(S)(in S s)
{
    immutable len = s.length;
    for (size_t i = 0; i < len; )
    {
        decode(s, i);
    }
}

/* =================== Conversion to UTF8 ======================= */

char[] toUTF8(out char[4] buf, dchar c)
    in
    {
        assert(isValidDchar(c));
    }
    body
    {
        if (c <= 0x7F)
        {
            buf[0] = cast(char) c;
            return buf[0 .. 1];
        }
        else if (c <= 0x7FF)
        {
            buf[0] = cast(char)(0xC0 | (c >> 6));
            buf[1] = cast(char)(0x80 | (c & 0x3F));
            return buf[0 .. 2];
        }
        else if (c <= 0xFFFF)
        {
            buf[0] = cast(char)(0xE0 | (c >> 12));
            buf[1] = cast(char)(0x80 | ((c >> 6) & 0x3F));
            buf[2] = cast(char)(0x80 | (c & 0x3F));
            return buf[0 .. 3];
        }
        else if (c <= 0x10FFFF)
        {
            buf[0] = cast(char)(0xF0 | (c >> 18));
            buf[1] = cast(char)(0x80 | ((c >> 12) & 0x3F));
            buf[2] = cast(char)(0x80 | ((c >> 6) & 0x3F));
            buf[3] = cast(char)(0x80 | (c & 0x3F));
            return buf[0 .. 4];
        }
        assert(0);
    }

/*******************
 * Encodes string s into UTF-8 and returns the encoded string.
 */

string toUTF8(string s)
{
    validate(s);
    return s;
}

/** ditto */

string toUTF8(const(wchar)[] s)
{
    char[] r;
    size_t i;
    size_t slen = s.length;

    r.length = slen;

    for (i = 0; i < slen; i++)
    {   wchar c = s[i];

    if (c <= 0x7F)
        r[i] = cast(char)c;     // fast path for ascii
    else
    {
        r.length = i;
        while (i < slen)
            encode(r, decode(s, i));
        break;
    }
    }
    return assumeUnique(r);
}

/** ditto */

string toUTF8(const(dchar)[] s)
{
    char[] r;
    size_t i;
    size_t slen = s.length;

    r.length = slen;

    for (i = 0; i < slen; i++)
    {   dchar c = s[i];

    if (c <= 0x7F)
        r[i] = cast(char)c;     // fast path for ascii
    else
    {
        r.length = i;
        foreach (dchar d; s[i .. slen])
        {
        encode(r, d);
        }
        break;
    }
    }
    return assumeUnique(r);
}

/* =================== Conversion to UTF16 ======================= */

wchar[] toUTF16(ref wchar[2] buf, dchar c)
    in
    {
    assert(isValidDchar(c));
    }
    body
    {
    if (c <= 0xFFFF)
    {
        buf[0] = cast(wchar) c;
        return buf[0 .. 1];
    }
    else
    {
        buf[0] = cast(wchar) ((((c - 0x10000) >> 10) & 0x3FF) + 0xD800);
        buf[1] = cast(wchar) (((c - 0x10000) & 0x3FF) + 0xDC00);
        return buf[0 .. 2];
    }
    }

/****************
 * Encodes string s into UTF-16 and returns the encoded string.
 * toUTF16z() is suitable for calling the 'W' functions in the Win32 API that take
 * an LPWSTR or LPCWSTR argument.
 */

wstring toUTF16(const(char)[] s)
{
    wchar[] r;
    size_t slen = s.length;

    r.length = slen;
    r.length = 0;
    for (size_t i = 0; i < slen; )
    {
        dchar c = s[i];
        if (c <= 0x7F)
        {
            i++;
            r ~= cast(wchar)c;
        }
        else
        {
            c = decode(s, i);
            encode(r, c);
        }
    }
    return cast(wstring) r; // ok because r is unique
}

/** ditto */

const(wchar)* toUTF16z(in char[] s)
{
    wchar[] r;
    size_t slen = s.length;

    r.length = slen + 1;
    r.length = 0;
    for (size_t i = 0; i < slen; )
    {
    dchar c = s[i];
    if (c <= 0x7F)
    {
        i++;
        r ~= cast(wchar)c;
    }
    else
    {
        c = decode(s, i);
        encode(r, c);
    }
    }
    r ~= "\000";
    return r.ptr;
}

/** ditto */

wstring toUTF16(wstring s)
{
    validate(s);
    return s;
}

/** ditto */

wstring toUTF16(const(dchar)[] s)
{
    wchar[] r;
    size_t slen = s.length;

    r.length = slen;
    r.length = 0;
    for (size_t i = 0; i < slen; i++)
    {
    encode(r, s[i]);
    }
    return cast(wstring) r; // ok because r is unique
}

/* =================== Conversion to UTF32 ======================= */

/*****
 * Encodes string s into UTF-32 and returns the encoded string.
 */

dstring toUTF32(const(char)[] s)
{
    dchar[] r;
    size_t slen = s.length;
    size_t j = 0;

    r.length = slen;        // r[] will never be longer than s[]
    for (size_t i = 0; i < slen; )
    {
    dchar c = s[i];
    if (c >= 0x80)
        c = decode(s, i);
    else
        i++;        // c is ascii, no need for decode
    r[j++] = c;
    }
    return cast(dstring) r[0 .. j]; // legit because it's unique
}

/** ditto */

dstring toUTF32(const(wchar)[] s)
{
    dchar[] r;
    size_t slen = s.length;
    size_t j = 0;

    r.length = slen;        // r[] will never be longer than s[]
    for (size_t i = 0; i < slen; )
    {
    dchar c = s[i];
    if (c >= 0x80)
        c = decode(s, i);
    else
        i++;        // c is ascii, no need for decode
    r[j++] = c;
    }
    return cast(dstring) r[0 .. j]; // legit because it's unique
}

/** ditto */

dstring toUTF32(dstring s)
{
    validate(s);
    return s;
}

/* ================================ tests ================================== */

unittest
{
    debug(utf) printf("utf.toUTF.unittest\n");

    string c;
    wstring w;
    dstring d;

    c = "hello";
    w = toUTF16(c);
    assert(w == "hello");
    d = toUTF32(c);
    assert(d == "hello");
    c = toUTF8(w);
    assert(c == "hello");
    d = toUTF32(w);
    assert(d == "hello");

    c = toUTF8(d);
    assert(c == "hello");
    w = toUTF16(d);
    assert(w == "hello");


    c = "hel\u1234o";
    w = toUTF16(c);
    assert(w == "hel\u1234o");
    d = toUTF32(c);
    assert(d == "hel\u1234o");

    c = toUTF8(w);
    assert(c == "hel\u1234o");
    d = toUTF32(w);
    assert(d == "hel\u1234o");

    c = toUTF8(d);
    assert(c == "hel\u1234o");
    w = toUTF16(d);
    assert(w == "hel\u1234o");


    c = "he\U0010AAAAllo";
    w = toUTF16(c);
    //foreach (wchar c; w) printf("c = x%x\n", c);
    //foreach (wchar c; cast(wstring)"he\U0010AAAAllo") printf("c = x%x\n", c);
    assert(w == "he\U0010AAAAllo");
    d = toUTF32(c);
    assert(d == "he\U0010AAAAllo");

    c = toUTF8(w);
    assert(c == "he\U0010AAAAllo");
    d = toUTF32(w);
    assert(d == "he\U0010AAAAllo");

    c = toUTF8(d);
    assert(c == "he\U0010AAAAllo");
    w = toUTF16(d);
    assert(w == "he\U0010AAAAllo");
}

/**
Returns the total number of code points encoded in a string.

The input to this function MUST be validly encoded.

Supercedes: This function supercedes $(D std.utf.toUCSindex()).

Standards: Unicode 5.0, ASCII, ISO-8859-1, WINDOWS-1252

Params:
s = the string to be counted
 */
size_t count(E)(const(E)[] s) if (isSomeChar!E && E.sizeof < 4)
{
    return walkLength(s);
//     size_t result = 0;
//     while (!s.empty)
//     {
//         ++result;
//         s.popFront();
//     }
//     return result;
}

unittest
{
    assert(count("") == 0);
    assert(count("a") == 1);
    assert(count("abc") == 3);
    assert(count("\u20AC100") == 4);
}


//----------------------------------------------------------------------------//
// [devel]

import std.typetuple;

private alias enforceEx!UtfException enforceUTF;

version (unittest) private
{
    struct NaiveInput(A : E[], E)
    {
        A array;
        @property bool  empty() { return array.length == 0; }
        @property Unqual!E front() { return array[0]; }
        void popFront() { array = array[1 .. $]; }
    }

    struct NaiveCatenator(E)
    {
        E[] result;
        void put(in E[] str) { result ~= str; }
    }
}


private immutable dchar[4] UTF8lowerBound =
[
    '\u0000', '\u0080', '\u0800', '\U00010000'
];


/**
 * Extracts a UTF sequence at the beginning of the input range $(D source) and
 * decodes it to the corresponding Unicode code point.
 *
 * Params:
 *  source = input range of UTF code units.
 *  result = the code point is stored in $(D result) if $(D R) is not an input
 *      range of reference $(D dchar)s.
 *
 * Returns:
 *  The decoded code point, or $(D null) if $(D source) is empty.
 */
@system dchar* decodeNext(R)(ref R source, ref dchar result)
        if (isInputRange!(R) && is(ElementType!R == char))
{
    uint code;

    if (auto p = std.range.getNext!char(source))
        code = *p;
    else
        return null; // empty!

    immutable stride = UTF8stride[code];
    if (stride > 1)
    {
        enforceUTF(stride <= 4, "invalid UTF-8 lead byte");

        // Mask out (7-stride) significant bits.
        code &= (1 << (7 - stride)) - 1;

        // Decode trailing sequence of (stride-1) bytes.
        foreach (i; 1 .. stride)
        {
            immutable char u = *enforceUTF(std.range.getNext!char(source),
                    "decoding trailing UTF-8 sequence past the end of " ~ R.stringof);
            enforceUTF((u & 0b11000000) == 0b10000000, "invalid UTF-8 sequence");
            code = (code << 6) | (u & 0b00111111);
        }
        enforceUTF(code >= UTF8lowerBound[stride - 1], "overlong UTF-8 sequence");
        enforceUTF((code & 0x1FF800) != 0x00D800, "surrogate code point in UTF-8");
    }

    assert(isValidDchar(code));
    result = code;
    return &result;
}

/// ditto
@system dchar* decodeNext(R)(ref R source, ref dchar result)
        if (isInputRange!(R) && is(ElementType!R == wchar))
{
    uint code;

    if (auto p = std.range.getNext!wchar(source))
        code = *p;
    else
        return null; // empty!

    if ((code & 0xF800) == 0xD800)
    {
        enforceUTF((code & 0xFC00) == 0xD800, "sudden low surrogate appearance");
        immutable wchar pair = *enforceUTF(std.range.getNext!wchar(source),
                "surrogate pair past the end of " ~ R.stringof);
        enforceUTF((pair & 0xFC00) == 0xDC00, "expected a low surrogate");
        code = ((code - 0xD7C0) << 10) + (pair - 0xDC00);
    }

    assert(isValidDchar(code));
    result = code;
    return &result;
}

/// ditto
@system dchar* decodeNext(R)(ref R source, ref dchar result)
        if (isInputRange!(R) && is(ElementType!R == dchar))
{
    if (auto pch = std.range.getNext(source, result))
    {
        enforceUTF(*pch <= 0x10FFFF, "illegal code point decoding UTF-32");
        enforceUTF((*pch & 0x1FF800) != 0xD800, "surrogate code point decoding UTF-32");
        assert(isValidDchar(*pch));
        return pch;
    }
    else
    {
        return null;
    }
}

unittest
{
    enum dstring witness =
         "\u0000\u007F\u0080\u07FF\u0800\uD7FF\uE000\uFFFD"
        ~"\U00010000\U0010FFFF"
        ~"\U0001D800\U0001DBFF\U0001DC00\U0001DFFF\U0001FFFF";

    foreach (String; TypeTuple!(string, wstring, dstring))
    {
        auto r = NaiveInput!String(witness);
        dchar c;
        size_t k = 0;
        for (dchar* p; (p = decodeNext(r, c)) != null; ++k)
        {
            assert(*p == witness[k]);
        }
        assert(k == witness.length);
    }
}

unittest
{
    string[] wrong =
    [
        "\xED\xA0\x80",
        "\xED\xAD\xBF",
        "\xED\xAE\x80",
        "\xED\xAF\xBF",
        "\xED\xB0\x80",
        "\xED\xBE\x80",
        "\xED\xBF\xBF",
        "\xFF",
    ];
    foreach (str; wrong)
    {
        auto r = NaiveInput!string(str);
        dchar c;
        assert(expectError_( decodeNext(r, c) ));
    }
}

unittest
{
    wstring[] wrong =
    [
        [ 0xDC00 ],
        [ 0xD800 ],
        [ 0xD800, 0x0000 ]
    ];
    foreach (str; wrong)
    {
        auto r = NaiveInput!wstring(str);
        dchar c;
        assert(expectError_( decodeNext(r, c) ));
    }
}

unittest
{
    dstring[] wrong =
    [
        [ cast(immutable dchar) 0xD800 ],
        [ cast(immutable dchar) 0xDBFF ],
        [ cast(immutable dchar) 0xDC00 ],
        [ cast(immutable dchar) 0xDFFF ],
        [ cast(immutable dchar) 0x110000 ],
        [ cast(immutable dchar) 0xFFFFFFFF ]
    ];
    foreach (str; wrong)
    {
        auto r = NaiveInput!dstring(str);
        dchar c;
        assert(expectError_( decodeNext(r, c) ));
    }
}


/*
 * Returns an input range for iterating through a given input range $(D r) for
 * Unicode code points.  The element type of the input range $(D R) must be
 * $(D char), $(D wchar) or $(D dchar).
 */
ByCodePoint!R byCodePoint(R)(R r)
        if (isInputRange!(R) && isSomeChar!(ElementType!R))
{
    return ByCodePoint!R(r);
}

// ditto
struct ByCodePoint(Source)
{
    Source source;

    dchar* getNext(ref dchar result)
    {
        return decodeNext(source, result);
    }
}

unittest
{
    enum dstring witness =
         "\u0000\u007F\u0080\u07FF\u0800\uD7FF\uE000\uFFFD"
        ~"\U00010000\U0010FFFF"
        ~"\U0001D800\U0001DBFF\U0001DC00\U0001DFFF\U0001FFFF";

    foreach (String; TypeTuple!(string, wstring, dstring))
    {
        auto r = byCodePoint(NaiveInput!String(witness));
        dchar c;
        size_t k = 0;
        for (dchar* p; (p = r.getNext(c)) != null; ++k)
        {
            assert(*p == witness[k]);
        }
        assert(k == witness.length);
    }
}


__EOF__
// TODO TODO

/*
Returns an output range that writes strings to a given output range $(D sink)
as $(D Char[])s.  $(D Char) must be $(D char), $(D wchar) or $(D dchar).

Example:
--------------------
auto a = appender!(wchar[]);
auto w = writeTextIn!wchar(a);

w.put("The quick brown fox"c);
w.put(' ');
w.put("jumps over"w);
w.put(' ');
w.put("the lazy dog"d);

assert(a.data == "The quick brown fox jumps over the lazy dog"w);
--------------------
 */
UTFTextWriter!(Unqual!Char, Sink) writeTextIn(Char, Sink)(Sink sink)
        if (isSomeChar!(Char) && isOutputRange!(Sink, Char[]))
{
    return UTFTextWriter!(Unqual!Char, Sink)(sink);
}

// ditto
struct UTFTextWriter(Char, Sink)
        if (is(Char == char))
{
    Sink sink;

    void put(in char[] str)
    {
        sink.put(str);
    }

    void put(in wchar[] str)
    {
        char[4] ubuf = void;

        for (size_t i = 0; i < str.length; )
        {
            immutable dchar c = str.decode(i);
            sink.put(ubuf[0 .. encode(ubuf, c)]);
        }
    }

    void put(in dchar[] str)
    {
        char[4] ubuf = void;
        foreach (c; str)
            sink.put(ubuf[0 .. encode(ubuf, c)]);
    }

    void put(dchar c)
    {
        char[4] ubuf = void;
        sink.put(ubuf[0 .. encode(ubuf, c)]);
    }
}

// ditto
struct UTFTextWriter(Char, Sink)
        if (is(Char == wchar))
{
    Sink sink;

    void put(in char[] str)
    {
        wchar[2] wbuf = void;
        for (size_t i = 0; i < str.length; )
        {
            immutable dchar c = str.decode(i);
            sink.put(wbuf[0 .. encode(wbuf, c)]);
        }
    }

    void put(in wchar[] str)
    {
        sink.put(str);
    }

    void put(in dchar[] str)
    {
        wchar[2] wbuf = void;
        foreach (c; str)
            sink.put(wbuf[0 .. encode(wbuf, c)]);
    }

    void put(dchar c)
    {
        wchar[2] wbuf = void;
        sink.put(wbuf[0 .. encode(wbuf, c)]);
    }
}

// ditto
struct UTFTextWriter(Char, Sink)
        if (is(Char == dchar))
{
    Sink sink;

    void put(in char[] str)
    {
        for (size_t i = 0; i < str.length; )
            put(str.decode(i));
    }

    void put(in wchar[] str)
    {
        for (size_t i = 0; i < str.length; )
            put(str.decode(i));
    }

    void put(in dchar[] str)
    {
        sink.put(str);
    }

    void put(dchar c)
    {
        static if (isOutputRange!(Sink, dchar))
            sink.put(c);
        else
            sink.put((&c)[0 .. 1]);
    }
}

unittest
{
    enum dstring codepoints =
         "\u0000\u007F\u0080\u07FF\u0800\uD7FF\uE000\uFFFD"
        ~"\U00010000\U0010FFFF"
        ~"\U0001D800\U0001DBFF\U0001DC00\U0001DFFF\U0001FFFF";

    foreach (Char; TypeTuple!(char, wchar, dchar))
    {
        alias NaiveCatenator!Char Sink;

        auto w = writeTextIn!Char(Sink());
        w.put(cast( string) codepoints);
        w.put(cast(wstring) codepoints);
        w.put(cast(dstring) codepoints);
        foreach (c; codepoints) w.put(c);

        immutable(Char)[] witness;
        witness ~= codepoints ~ codepoints;
        witness ~= codepoints ~ codepoints;
        assert(w.sink.result == witness);
    }
}

