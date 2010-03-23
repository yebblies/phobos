// Written in the D programming language.

/**
 * Information about the target operating system, environment, and CPU
 *
 * Macros:
 *      WIKI = Phobos/StdSystem
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
module std.system;


import core.stdc.string;
// setenv() and unsetenv() are POSIX-specific.
version (Posix) import core.sys.posix.stdlib;
else import core.stdc.stdlib;

import std.contracts;
import std.conv;
import std.string;




/** Return the value of the environment variable with the given name.
    Calls core.stdc.stdlib._getenv internally.
*/
string getEnv(string name)
{
    // Cache the last call's result.
    static string lastResult;

    const valuez = getenv(toStringz(name));
    if (valuez == null)  return null;
    auto value = valuez[0 .. strlen(valuez)];
    if (value == lastResult) return lastResult;

    return lastResult = value.idup;
}


/** Set the value of the environment variable $(D name) to $(D value). */
version(Posix) void setEnv(string name, string value, bool overwrite)
{
    // errno message not very informative, hence not use errnoEnforce().
    enforce(setenv(toStringz(name), toStringz(value), overwrite) == 0,
        "Invalid environment variable name '"~name~"'");
}


/** Remove the environment variable with the given name.
    If the variable does not exist in the environment, this
    function succeeds without changing the environment.
*/
version(Posix) void unsetEnv(string name)
{
    enforce(unsetenv(toStringz(name)) == 0,
        "Invalid environment variable name '"~name~"'");
}


// Unittest for getEnv(), setEnv(), and unsetEnv()
unittest
{
    // New variable
    setEnv("foo", "bar", true);
    assert (getEnv("foo") == "bar");

    // Overwrite variable
    setEnv("foo", "baz", true);
    assert (getEnv("foo") == "baz");

    // Do not overwrite variable
    setEnv("foo", "bax", false);
    assert (getEnv("foo") == "baz");

    // Unset variable
    unsetEnv("foo");
    assert (getEnv("foo") == null);

    // Check that exceptions are thrown when they should be.
    try { setEnv("foo=bar", "baz", true); assert(false); } catch(Exception e) {}
    try { unsetEnv("foo=bar"); assert(false); } catch(Exception e) {}
}




const
{

    // Operating system family
    enum Family
    {
        Win32 = 1,              // Microsoft 32 bit Windows systems
        linux,                  // all linux systems
        OSX,
    }

    version (Win32)
    {
        Family family = Family.Win32;
    }
    else version (Posix)
    {
        Family family = Family.linux;
    }
    else version (OSX)
    {
        Family family = Family.OSX;
    }
    else
    {
        static assert(0);
    }

    // More specific operating system name
    enum OS
    {
        Windows95 = 1,
        Windows98,
        WindowsME,
        WindowsNT,
        Windows2000,
        WindowsXP,

        RedHatLinux,
        OSX,
    }

    /// Byte order endianness

    enum Endian
    {
        BigEndian,      /// big endian byte order
        LittleEndian    /// little endian byte order
    }

    version(LittleEndian)
    {
        /// Native system endianness
        Endian endian = Endian.LittleEndian;
    }
    else
    {
        Endian endian = Endian.BigEndian;
    }
}

// The rest should get filled in dynamically at runtime

OS os = OS.WindowsXP;

// Operating system version as in
// os_major.os_minor
uint os_major = 4;
uint os_minor = 0;


