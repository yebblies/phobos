// Written in the D programming language.

/**
 * This module implements a
 * $(LINK2 http://erdani.org/publications/cuj-04-2002.html,discriminated union)
 * type (a.k.a.
 * $(LINK2 http://en.wikipedia.org/wiki/Tagged_union,tagged union),
 * $(LINK2 http://en.wikipedia.org/wiki/Algebraic_data_type,algebraic type)).
 * Such types are useful
 * for type-uniform binary interfaces, interfacing with scripting
 * languages, and comfortable exploratory programming.
 *
 * Macros:
 *  WIKI = Phobos/StdVariant
 *
 * Synopsis:
 *
 * ----
 * Variant a; // Must assign before use, otherwise exception ensues
 * // Initialize with an integer; make the type int
 * Variant b = 42;
 * assert(b.type == typeid(int));
 * // Peek at the value
 * assert(b.peek!(int) !is null && *b.peek!(int) == 42);
 * // Automatically convert per language rules
 * auto x = b.get!(real);
 * // Assign any other type, including other variants
 * a = b;
 * a = 3.14;
 * assert(a.type == typeid(double));
 * // Implicit conversions work just as with built-in types
 * assert(a > b);
 * // Check for convertibility
 * assert(!a.convertsTo!(int)); // double not convertible to int
 * // Strings and all other arrays are supported
 * a = "now I'm a string";
 * assert(a == "now I'm a string");
 * a = new int[42]; // can also assign arrays
 * assert(a.length == 42);
 * a[5] = 7;
 * assert(a[5] == 7);
 * // Can also assign class values
 * class Foo {}
 * auto foo = new Foo;
 * a = foo;
 * assert(*a.peek!(Foo) == foo); // and full type information is preserved
 * ----
 *
 * Credits:
 *
 * Reviewed by Brad Roberts. Daniel Keep provided a detailed code
 * review prompting the following improvements: (1) better support for
 * arrays; (2) support for associative arrays; (3) friendlier behavior
 * towards the garbage collector.
 *
 * Copyright: Copyright Andrei Alexandrescu 2007 - 2009.
 * License:   <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
 * Authors:   $(WEB erdani.org, Andrei Alexandrescu)
 *
 *          Copyright Andrei Alexandrescu 2007 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.variant;

import std.algorithm, std.array, std.conv, std.traits, std.typetuple;
import core.stdc.string;

version(unittest)
{
    import std.exception, std.range, std.stdio;
}

private template maxSize(T...)
{
    static if (T.length == 1)
    {
        enum size_t maxSize = T[0].sizeof;
    }
    else
    {
        enum size_t maxSize = T[0].sizeof >= maxSize!(T[1 .. $])
            ? T[0].sizeof : maxSize!(T[1 .. $]);
    }
}

struct This;

template AssociativeArray(T)
{
    enum bool valid = false;
    alias void Key;
    alias void Value;
}

template AssociativeArray(T : V[K], K, V)
{
    enum bool valid = true;
    alias K Key;
    alias V Value;
}

template This2Variant(V, T...)
{
    static if (T.length == 0) alias TypeTuple!() This2Variant;
    else static if (is(AssociativeArray!(T[0]).Key == This))
    {
        static if (is(AssociativeArray!(T[0]).Value == This))
            alias TypeTuple!(V[V],
                    This2Variant!(V, T[1 .. $])) This2Variant;
        else
            alias TypeTuple!(AssociativeArray!(T[0]).Value[V],
                    This2Variant!(V, T[1 .. $])) This2Variant;
    }
    else static if (is(AssociativeArray!(T[0]).Value == This))
        alias TypeTuple!(V[AssociativeArray!(T[0]).Key],
                This2Variant!(V, T[1 .. $])) This2Variant;
    else static if (is(T[0] == This[]))
        alias TypeTuple!(V[], This2Variant!(V, T[1 .. $])) This2Variant;
    else static if (is(T[0] == This*))
        alias TypeTuple!(V*, This2Variant!(V, T[1 .. $])) This2Variant;
    else
       alias TypeTuple!(T[0], This2Variant!(V, T[1 .. $])) This2Variant;
}

/**
 * $(D_PARAM VariantN) is a back-end type seldom used directly by user
 * code. Two commonly-used types using $(D_PARAM VariantN) as
 * back-end are:
 *
 * $(OL $(LI $(B Algebraic): A closed discriminated union with a
 * limited type universe (e.g., $(D_PARAM Algebraic!(int, double,
 * string)) only accepts these three types and rejects anything
 * else).) $(LI $(B Variant): An open discriminated union allowing an
 * unbounded set of types. The restriction is that the size of the
 * stored type cannot be larger than the largest built-in type. This
 * means that $(D_PARAM Variant) can accommodate all primitive types
 * and all user-defined types except for large $(D_PARAM struct)s.) )
 *
 * Both $(D_PARAM Algebraic) and $(D_PARAM Variant) share $(D_PARAM
 * VariantN)'s interface. (See their respective documentations below.)
 *
 * $(D_PARAM VariantN) is a discriminated union type parameterized
 * with the largest size of the types stored ($(D_PARAM maxDataSize))
 * and with the list of allowed types ($(D_PARAM AllowedTypes)). If
 * the list is empty, then any type up of size up to $(D_PARAM
 * maxDataSize) (rounded up for alignment) can be stored in a
 * $(D_PARAM VariantN) object.
 *
 */

struct VariantN(size_t maxDataSize, AllowedTypesX...)
{
    alias This2Variant!(VariantN, AllowedTypesX) AllowedTypes;

private:
    // Compute the largest practical size from maxDataSize
    struct SizeChecker
    {
        int function() fptr;
        ubyte[maxDataSize] data;
    }
    enum size = SizeChecker.sizeof - (int function()).sizeof;
    static assert(size >= (void*).sizeof);

    /** Tells whether a type $(D_PARAM T) is statically allowed for
     * storage inside a $(D_PARAM VariantN) object by looking
     * $(D_PARAM T) up in $(D_PARAM AllowedTypes). If $(D_PARAM
     * AllowedTypes) is empty, all types of size up to $(D_PARAM
     * maxSize) are allowed.
     */
    public template allowed(T)
    {
        enum bool allowed
            = is(T == VariantN)
            ||
            //T.sizeof <= size &&
            (AllowedTypes.length == 0 || staticIndexOf!(T, AllowedTypes) >= 0);
    }

    // Each internal operation is encoded with an identifier. See
    // the "handler" function below.
    enum OpID { getTypeInfo, get, compare, testConversion, toString,
            index, indexAssign, catAssign, copyOut, length,
            apply }

    // state
    int function(OpID selector, ubyte[size]* store, void* data) fptr
        = &handler!(void);
    union
    {
        ubyte[size] store;
        // conservatively mark the region as pointers
        static if (size >= (void*).sizeof)
            void* p[size / (void*).sizeof];
    }

    // internals
    // Handler for an uninitialized value
    static int handler(A : void)(OpID selector, ubyte[size]*, void* parm)
    {
        switch (selector)
        {
        case OpID.getTypeInfo:
            *cast(TypeInfo *) parm = typeid(A);
            break;
        case OpID.copyOut:
            auto target = cast(VariantN *) parm;
            target.fptr = &handler!(A);
            // no need to copy the data (it's garbage)
            break;
        case OpID.compare:
            auto rhs = cast(VariantN *) parm;
            return rhs.peek!(A)
                ? 0 // all uninitialized are equal
                : int.min; // uninitialized variant is not comparable otherwise
        case OpID.toString:
            string * target = cast(string*) parm;
            *target = "<Uninitialized VariantN>";
            break;
        case OpID.get:
        case OpID.testConversion:
        case OpID.index:
        case OpID.indexAssign:
        case OpID.catAssign:
        case OpID.length:
            throw new VariantException(
                "Attempt to use an uninitialized VariantN");
        default: assert(false, "Invalid OpID");
        }
        return 0;
    }

    // Handler for all of a type's operations
    static int handler(A)(OpID selector, ubyte[size]* pStore, void* parm)
    {
        static A* getPtr(void* untyped)
        {
            if (untyped)
            {
                static if (A.sizeof <= size)
                    return cast(A*) untyped;
                else
                    return *cast(A**) untyped;
            }
            return null;
        }
        auto zis = getPtr(pStore);
        // Input: TypeInfo object
        // Output: target points to a copy of *me, if me was not null
        // Returns: true iff the A can be converted to the type represented
        // by the incoming TypeInfo
        static bool tryPutting(A* src, TypeInfo targetType, void* target)
        {
            alias TypeTuple!(A, ImplicitConversionTargets!(A)) AllTypes;
            foreach (T ; AllTypes)
            {
                if (targetType != typeid(T) &&
                        targetType != typeid(const(T)))
                {
                    static if (isImplicitlyConvertible!(T, immutable(T)))
                    {
                        if (targetType != typeid(immutable(T)))
                        {
                            continue;
                        }
                    }
                    else
                    {
                        continue;
                    }
                }
                // found!!!
                static if (is(typeof(*cast(T*) target = *src)))
                {
                    auto zat = cast(T*) target;
                    if (src)
                    {
                        assert(target, "target must be non-null");
                        *zat = *src;
                    }
                }
                else
                {
                    // type is not assignable
                    if (src) assert(false, A.stringof);
                }
                return true;
            }
            return false;
        }

        switch (selector)
        {
        case OpID.getTypeInfo:
            *cast(TypeInfo *) parm = typeid(A);
            break;
        case OpID.copyOut:
            auto target = cast(VariantN *) parm;
            assert(target);
            tryPutting(zis, typeid(A), cast(void*) getPtr(&target.store))
                || assert(false);
            target.fptr = &handler!(A);
            break;
        case OpID.get:
            return !tryPutting(zis, *cast(TypeInfo*) parm, parm);
        case OpID.testConversion:
            return !tryPutting(null, *cast(TypeInfo*) parm, null);
        case OpID.compare:
            auto rhsP = cast(VariantN *) parm;
            auto rhsType = rhsP.type;
            // Are we the same?
            if (rhsType == typeid(A))
            {
                // cool! Same type!
                auto rhsPA = getPtr(&rhsP.store);
                if (*rhsPA == *zis)
                {
                    return 0;
                }
                static if (is(typeof(A.init < A.init)))
                {
                    return *zis < *rhsPA ? -1 : 1;
                }
                else
                {
                    // type doesn't support ordering comparisons
                    return int.min;
                }
            }
            VariantN temp;
            // Do I convert to rhs?
            if (tryPutting(zis, rhsType, &temp.store))
            {
                // cool, I do; temp's store contains my data in rhs's type!
                // also fix up its fptr
                temp.fptr = rhsP.fptr;
                // now lhsWithRhsType is a full-blown VariantN of rhs's type
                return temp.opCmp(*rhsP);
            }
            // Does rhs convert to zis?
            *cast(TypeInfo*) &temp.store = typeid(A);
            if (rhsP.fptr(OpID.get, &rhsP.store, &temp.store) == 0)
            {
                // cool! Now temp has rhs in my type!
                auto rhsPA = getPtr(&temp.store);
                if (*rhsPA == *zis)
                {
                    return 0;
                }
                static if (is(typeof(A.init < A.init)))
                {
                    return *zis < *rhsPA ? -1 : 1;
                }
                else
                {
                    // type doesn't support ordering comparisons
                    return int.min;
                }
            }
            return int.min; // dunno
        case OpID.toString:
            auto target = cast(string*) parm;
            static if (is(typeof(to!(string)(*zis))))
            {
                *target = to!(string)(*zis);
                break;
            }
            else static if (is(typeof((*zis).toString)))
            {
                *target = (*zis).toString;
                break;
            }
            else
            {
                throw new VariantException(typeid(A), typeid(string));
            }

        case OpID.index:
            // Added allowed!(...) prompted by a bug report by Chris
            // Nicholson-Sauls.
            static if (isStaticArray!(A) && allowed!(typeof(A.init)))
            {
                enforce(0, "Not implemented");
            }
            static if (isDynamicArray!(A) && allowed!(typeof(A.init[0])))
            {
                // array type; input and output are the same VariantN
                auto result = cast(VariantN*) parm;
                size_t index = result.convertsTo!(int)
                    ? result.get!(int) : result.get!(size_t);
                *result = (*zis)[index];
                break;
            }
            else static if (isAssociativeArray!(A)
                    && allowed!(typeof(A.init.values[0])))
            {
                auto result = cast(VariantN*) parm;
                *result = (*zis)[result.get!(typeof(A.keys[0]))];
                break;
            }
            else
            {
                throw new VariantException(typeid(A), typeid(void[]));
            }

        case OpID.indexAssign:
            static if (isArray!(A) && is(typeof((*zis)[0] = (*zis)[0])))
            {
                // array type; result comes first, index comes second
                auto args = cast(VariantN*) parm;
                size_t index = args[1].convertsTo!(int)
                    ? args[1].get!(int) : args[1].get!(size_t);
                (*zis)[index] = args[0].get!(typeof((*zis)[0]));
                break;
            }
            else static if (isAssociativeArray!(A))
            {
                auto args = cast(VariantN*) parm;
                (*zis)[args[1].get!(typeof(A.keys[0]))]
                    = args[0].get!(typeof(A.values[0]));
                break;
            }
            else
            {
                throw new VariantException(typeid(A), typeid(void[]));
            }

        case OpID.catAssign:
            static if (is(typeof((*zis)[0])) && is(typeof((*zis) ~= *zis)))
            {
                // array type; parm is the element to append
                auto arg = cast(VariantN*) parm;
                alias typeof((*zis)[0]) E;
                if (arg[0].convertsTo!(E))
                {
                    // append one element to the array
                    (*zis) ~= [ arg[0].get!(E) ];
                }
                else
                {
                    // append a whole array to the array
                    (*zis) ~= arg[0].get!(A);
                }
                break;
            }
            else
            {
                throw new VariantException(typeid(A), typeid(void[]));
            }

        case OpID.length:
            static if (is(typeof(zis.length)))
            {
                return zis.length;
            }
            else
            {
                throw new VariantException(typeid(A), typeid(void[]));
            }

        case OpID.apply:
            assert(0);

        default: assert(false);
        }
        return 0;
    }

public:
    /** Constructs a $(D_PARAM VariantN) value given an argument of a
     * generic type. Statically rejects disallowed types.
     */

    this(T)(T value)
    {
        static assert(allowed!(T), "Cannot store a " ~ T.stringof
            ~ " in a " ~ VariantN.stringof);
        opAssign(value);
    }

    /** Assigns a $(D_PARAM VariantN) from a generic
     * argument. Statically rejects disallowed types. */

    VariantN opAssign(T)(T rhs)
    {
        //writeln(typeid(rhs));
        static assert(allowed!(T), "Cannot store a " ~ T.stringof
            ~ " in a " ~ VariantN.stringof ~ ". Valid types are "
                ~ AllowedTypes.stringof);
        static if (is(T : VariantN))
        {
            rhs.fptr(OpID.copyOut, &rhs.store, &this);
        }
        else static if (is(T : const(VariantN)))
        {
            static assert(false,
                    "Assigning Variant objects from const Variant"
                    " objects is currently not supported.");
        }
        else
        {
            static if (T.sizeof <= size)
            {
                memcpy(&store, &rhs, rhs.sizeof);
            }
            else
            {
                auto p = new T;
                *p = rhs;
                memcpy(&store, &p, p.sizeof);
            }
            fptr = &handler!(T);
        }
        return this;
    }

    /** Returns true if and only if the $(D_PARAM VariantN) object
     * holds a valid value (has been initialized with, or assigned
     * from, a valid value).
     * Example:
     * ----
     * Variant a;
     * assert(!a.hasValue);
     * Variant b;
     * a = b;
     * assert(!a.hasValue); // still no value
     * a = 5;
     * assert(a.hasValue);
     * ----
     */

    bool hasValue() const
    {
        // @@@BUG@@@ in compiler, the cast shouldn't be needed
        return cast(typeof(&handler!(void))) fptr != &handler!(void);
    }

    /**
     * If the $(D_PARAM VariantN) object holds a value of the
     * $(I exact) type $(D_PARAM T), returns a pointer to that
     * value. Otherwise, returns $(D_PARAM null). In cases
     * where $(D_PARAM T) is statically disallowed, $(D_PARAM
     * peek) will not compile.
     *
     * Example:
     * ----
     * Variant a = 5;
     * auto b = a.peek!(int);
     * assert(b !is null);
     * *b = 6;
     * assert(a == 6);
     * ----
     */
    T * peek(T)()
    {
        static if (!is(T == void))
            static assert(allowed!(T), "Cannot store a " ~ T.stringof
                    ~ " in a " ~ VariantN.stringof);
        return type == typeid(T) ? cast(T*) &store : null;
    }

    /**
     * Returns the $(D_PARAM typeid) of the currently held value.
     */

    TypeInfo type() const
    {
        TypeInfo result;
        fptr(OpID.getTypeInfo, null, &result);
        return result;
    }

    /**
     * Returns $(D_PARAM true) if and only if the $(D_PARAM VariantN)
     * object holds an object implicitly convertible to type $(D_PARAM
     * U). Implicit convertibility is defined as per
     * $(LINK2 std_traits.html#ImplicitConversionTargets,ImplicitConversionTargets).
     */

    bool convertsTo(T)()
    {
        TypeInfo info = typeid(T);
        return fptr(OpID.testConversion, null, &info) == 0;
    }

    // private T[] testing123(T)(T*);

    // /**
    //  * A workaround for the fact that functions cannot return
    //  * statically-sized arrays by value. Essentially $(D_PARAM
    //  * DecayStaticToDynamicArray!(T[N])) is an alias for $(D_PARAM
    //  * T[]) and $(D_PARAM DecayStaticToDynamicArray!(T)) is an alias
    //  * for $(D_PARAM T).
    //  */

    // template DecayStaticToDynamicArray(T)
    // {
    //     static if (isStaticArray!(T))
    //     {
    //         alias typeof(testing123(&T[0])) DecayStaticToDynamicArray;
    //     }
    //     else
    //     {
    //         alias T DecayStaticToDynamicArray;
    //     }
    // }

    // static assert(is(DecayStaticToDynamicArray!(immutable(char)[21]) ==
    //                  immutable(char)[]),
    //               DecayStaticToDynamicArray!(immutable(char)[21]).stringof);

    /**
     * Returns the value stored in the $(D_PARAM VariantN) object,
     * implicitly converted to the requested type $(D_PARAM T), in
     * fact $(D_PARAM DecayStaticToDynamicArray!(T)). If an implicit
     * conversion is not possible, throws a $(D_PARAM
     * VariantException).
     */

    T get(T)() if (!is(T == const))
    {
        union Buf
        {
            TypeInfo info;
            T result;
        };
        auto p = *cast(T**) &store;
        Buf buf = { typeid(T) };
        if (fptr(OpID.get, &store, &buf))
        {
            throw new VariantException(type, typeid(T));
        }
        return buf.result;
    }

    T get(T)() const if (is(T == const))
    {
        union Buf
        {
            TypeInfo info;
            Unqual!T result;
        };
        auto p = *cast(T**) &store;
        Buf buf = { typeid(T) };
        if (fptr(OpID.get, cast(typeof(&store)) &store, &buf))
        {
            throw new VariantException(type, typeid(T));
        }
        return buf.result;
    }

    /**
     * Returns the value stored in the $(D_PARAM VariantN) object,
     * explicitly converted (coerced) to the requested type $(D_PARAM
     * T). If $(D_PARAM T) is a string type, the value is formatted as
     * a string. If the $(D_PARAM VariantN) object is a string, a
     * parse of the string to type $(D_PARAM T) is attempted. If a
     * conversion is not possible, throws a $(D_PARAM
     * VariantException).
     */

    T coerce(T)()
    {
        static if (isNumeric!(T))
        {
            // maybe optimize this fella; handle ints separately
            return to!(T)(get!(real));
        }
        else static if (is(T : Object))
        {
            return to!(T)(get!(Object));
        }
        else static if (isSomeString!(T))
        {
            return to!(T)(toString);
        }
        else
        {
            // Fix for bug 1649
            static assert(false, "unsupported type for coercion");
        }
    }

    /**
     * Formats the stored value as a string.
     */

    string toString()
    {
        string result;
        fptr(OpID.toString, &store, &result) == 0 || assert(false);
        return result;
    }

    /**
     * Comparison for equality used by the "==" and "!="  operators.
     */

    // returns 1 if the two are equal
    bool opEquals(T)(T rhs)
    {
        static if (is(T == VariantN))
            alias rhs temp;
        else
            auto temp = Variant(rhs);
        return fptr(OpID.compare, &store, &temp) == 0;
    }

    /**
     * Ordering comparison used by the "<", "<=", ">", and ">="
     * operators. In case comparison is not sensible between the held
     * value and $(D_PARAM rhs), an exception is thrown.
     */

    int opCmp(T)(T rhs)
    {
        static if (is(T == VariantN))
            alias rhs temp;
        else
            auto temp = Variant(rhs);
        auto result = fptr(OpID.compare, &store, &temp);
        if (result == int.min)
        {
            throw new VariantException(type, temp.type);
        }
        return result;
    }

    /**
     * Computes the hash of the held value.
     */

    uint toHash()
    {
        return type.getHash(&store);
    }

    private VariantN opArithmetic(T, string op)(T other)
    {
        VariantN result;
        static if (is(T == VariantN))
        {
            if (convertsTo!(uint) && other.convertsTo!(uint))
                result = mixin("get!(uint) " ~ op ~ " other.get!(uint)");
            else if (convertsTo!(int) && other.convertsTo!(int))
                result = mixin("get!(int) " ~ op ~ " other.get!(int)");
            else if (convertsTo!(ulong) && other.convertsTo!(ulong))
                result = mixin("get!(ulong) " ~ op ~ " other.get!(ulong)");
            else if (convertsTo!(long) && other.convertsTo!(long))
                result = mixin("get!(long) " ~ op ~ " other.get!(long)");
            else if (convertsTo!(double) && other.convertsTo!(double))
                result = mixin("get!(double) " ~ op ~ " other.get!(double)");
            else
                result = mixin("get!(real) " ~ op ~ " other.get!(real)");
        }
        else
        {
            if (is(typeof(T.max) : uint) && T.min == 0 && convertsTo!(uint))
                result = mixin("get!(uint) " ~ op ~ " other");
            else if (is(typeof(T.max) : int) && T.min < 0 && convertsTo!(int))
                result = mixin("get!(int) " ~ op ~ " other");
            else if (is(typeof(T.max) : ulong) && T.min == 0
                     && convertsTo!(ulong))
                result = mixin("get!(ulong) " ~ op ~ " other");
            else if (is(typeof(T.max) : long) && T.min < 0 && convertsTo!(long))
                result = mixin("get!(long) " ~ op ~ " other");
            else if (is(T : double) && convertsTo!(double))
                result = mixin("get!(double) " ~ op ~ " other");
            else
                result = mixin("get!(real) " ~ op ~ " other");
        }
        return result;
    }

    private VariantN opLogic(T, string op)(T other)
    {
        VariantN result;
        static if (is(T == VariantN))
        {
            if (convertsTo!(uint) && other.convertsTo!(uint))
                result = mixin("get!(uint) " ~ op ~ " other.get!(uint)");
            else if (convertsTo!(int) && other.convertsTo!(int))
                result = mixin("get!(int) " ~ op ~ " other.get!(int)");
            else if (convertsTo!(ulong) && other.convertsTo!(ulong))
                result = mixin("get!(ulong) " ~ op ~ " other.get!(ulong)");
            else
                result = mixin("get!(long) " ~ op ~ " other.get!(long)");
        }
        else
        {
            if (is(typeof(T.max) : uint) && T.min == 0 && convertsTo!(uint))
                result = mixin("get!(uint) " ~ op ~ " other");
            else if (is(typeof(T.max) : int) && T.min < 0 && convertsTo!(int))
                result = mixin("get!(int) " ~ op ~ " other");
            else if (is(typeof(T.max) : ulong) && T.min == 0
                     && convertsTo!(ulong))
                result = mixin("get!(ulong) " ~ op ~ " other");
            else
                result = mixin("get!(long) " ~ op ~ " other");
        }
        return result;
    }

    /**
     * Arithmetic between $(D_PARAM VariantN) objects and numeric
     * values. All arithmetic operations return a $(D_PARAM VariantN)
     * object typed depending on the types of both values
     * involved. The conversion rules mimic D's built-in rules for
     * arithmetic conversions.
     */

    // Adapted from http://www.prowiki.org/wiki4d/wiki.cgi?DanielKeep/Variant
    // arithmetic
    VariantN opAdd(T)(T rhs) { return opArithmetic!(T, "+")(rhs); }
    ///ditto
    VariantN opSub(T)(T rhs) { return opArithmetic!(T, "-")(rhs); }

    // Commenteed all _r versions for now because of ambiguities
    // arising when two Variants are used

    /////ditto
    // VariantN opSub_r(T)(T lhs)
    // {
    //     return VariantN(lhs).opArithmetic!(VariantN, "-")(this);
    // }
    ///ditto
    VariantN opMul(T)(T rhs) { return opArithmetic!(T, "*")(rhs); }
    ///ditto
    VariantN opDiv(T)(T rhs) { return opArithmetic!(T, "/")(rhs); }
    // ///ditto
    // VariantN opDiv_r(T)(T lhs)
    // {
    //     return VariantN(lhs).opArithmetic!(VariantN, "/")(this);
    // }
    ///ditto
    VariantN opMod(T)(T rhs) { return opArithmetic!(T, "%")(rhs); }
    // ///ditto
    // VariantN opMod_r(T)(T lhs)
    // {
    //     return VariantN(lhs).opArithmetic!(VariantN, "%")(this);
    // }
    ///ditto
    VariantN opAnd(T)(T rhs) { return opLogic!(T, "&")(rhs); }
    ///ditto
    VariantN opOr(T)(T rhs) { return opLogic!(T, "|")(rhs); }
    ///ditto
    VariantN opXor(T)(T rhs) { return opLogic!(T, "^")(rhs); }
    ///ditto
    VariantN opShl(T)(T rhs) { return opLogic!(T, "<<")(rhs); }
    // ///ditto
    // VariantN opShl_r(T)(T lhs)
    // {
    //     return VariantN(lhs).opLogic!(VariantN, "<<")(this);
    // }
    ///ditto
    VariantN opShr(T)(T rhs) { return opLogic!(T, ">>")(rhs); }
    // ///ditto
    // VariantN opShr_r(T)(T lhs)
    // {
    //     return VariantN(lhs).opLogic!(VariantN, ">>")(this);
    // }
    ///ditto
    VariantN opUShr(T)(T rhs) { return opLogic!(T, ">>>")(rhs); }
    // ///ditto
    // VariantN opUShr_r(T)(T lhs)
    // {
    //     return VariantN(lhs).opLogic!(VariantN, ">>>")(this);
    // }
    ///ditto
    VariantN opCat(T)(T rhs)
    {
        auto temp = this;
        temp ~= rhs;
        return temp;
    }
    // ///ditto
    // VariantN opCat_r(T)(T rhs)
    // {
    //     VariantN temp = rhs;
    //     temp ~= this;
    //     return temp;
    // }

    ///ditto
    VariantN opAddAssign(T)(T rhs)  { return this = this + rhs; }
    ///ditto
    VariantN opSubAssign(T)(T rhs)  { return this = this - rhs; }
    ///ditto
    VariantN opMulAssign(T)(T rhs)  { return this = this * rhs; }
    ///ditto
    VariantN opDivAssign(T)(T rhs)  { return this = this / rhs; }
    ///ditto
    VariantN opModAssign(T)(T rhs)  { return this = this % rhs; }
    ///ditto
    VariantN opAndAssign(T)(T rhs)  { return this = this & rhs; }
    ///ditto
    VariantN opOrAssign(T)(T rhs)   { return this = this | rhs; }
    ///ditto
    VariantN opXorAssign(T)(T rhs)  { return this = this ^ rhs; }
    ///ditto
    VariantN opShlAssign(T)(T rhs)  { return this = this << rhs; }
    ///ditto
    VariantN opShrAssign(T)(T rhs)  { return this = this >> rhs; }
    ///ditto
    VariantN opUShrAssign(T)(T rhs) { return this = this >>> rhs; }
    ///ditto
    VariantN opCatAssign(T)(T rhs)
    {
        auto toAppend = VariantN(rhs);
        fptr(OpID.catAssign, &store, &toAppend) == 0 || assert(false);
        return this;
    }

    /**
     * Array and associative array operations. If a $(D_PARAM
     * VariantN) contains an (associative) array, it can be indexed
     * into. Otherwise, an exception is thrown.
     *
     * Example:
     * ----
     * auto a = Variant(new int[10]);
     * a[5] = 42;
     * assert(a[5] == 42);
     * int[int] hash = [ 42:24 ];
     * a = hash;
     * assert(a[42] == 24);
     * ----
     *
     * Caveat:
     *
     * Due to limitations in current language, read-modify-write
     * operations $(D_PARAM op=) will not work properly:
     *
     * ----
     * Variant a = new int[10];
     * a[5] = 42;
     * a[5] += 8;
     * assert(a[5] == 50); // fails, a[5] is still 42
     * ----
     */
    VariantN opIndex(K)(K i)
    {
        auto result = VariantN(i);
        fptr(OpID.index, &store, &result) == 0 || assert(false);
        return result;
    }

    unittest
    {
        int[int] hash = [ 42:24 ];
        Variant v = hash;
        assert(v[42] == 24);
        v[42] = 5;
        assert(v[42] == 5);
    }

    /// ditto
    VariantN opIndexAssign(T, N)(T value, N i)
    {
        VariantN[2] args = [ VariantN(value), VariantN(i) ];
        fptr(OpID.indexAssign, &store, &args) == 0 || assert(false);
        return args[0];
    }

    /** If the $(D_PARAM VariantN) contains an (associative) array,
     * returns the length of that array. Otherwise, throws an
     * exception.
     */
    @property size_t length()
    {
        return cast(size_t) fptr(OpID.length, &store, null);
    }

    /**
       If the $(D VariantN) contains an array, applies $(D dg) to each
       element of the array in turn. Otherwise, throws an exception.
     */
    int opApply(Delegate)(scope Delegate dg)
    {
        // @@@TODO@@@ make this much more general.
        alias ParameterTypeTuple!(Delegate)[0] A;
        auto arr = get!(A[]);
        foreach (ref e; arr)
        {
            if (dg(e)) return 1;
        }
        return 0;
    }
}

/**
$(D_PARAM Variant) is an alias for $(D_PARAM VariantN) instantiated
with the largest of $(D_PARAM creal), $(D_PARAM char[]), and $(D_PARAM
void delegate()). This ensures that $(D_PARAM Variant) is large enough
to hold all of D's predefined types, including all numeric types,
pointers, delegates, and class references.  You may want to use
$(D_PARAM VariantN) directly with a different maximum size either for
storing larger types, or for saving memory.
 */

alias VariantN!(maxSize!(creal, char[], void delegate())) Variant;

/**
 * Returns an array of variants constructed from $(D_PARAM args).
 * Example:
 * ----
 * auto a = variantArray(1, 3.14, "Hi!");
 * assert(a[1] == 3.14);
 * auto b = Variant(a); // variant array as variant
 * assert(b[1] == 3.14);
 * ----
 *
 * Code that needs functionality similar to the $(D_PARAM boxArray)
 * function in the $(D_PARAM std.boxer) module can achieve it like this:
 *
 * ----
 * // old
 * Box[] fun(...)
 * {
 *     ...
 *     return boxArray(_arguments, _argptr);
 * }
 * // new
 * Variant[] fun(T...)(T args)
 * {
 *     ...
 *     return variantArray(args);
 * }
 * ----
 *
 * This is by design. During construction the $(D_PARAM Variant) needs
 * static type information about the type being held, so as to store a
 * pointer to function for fast retrieval.
 */

Variant[] variantArray(T...)(T args)
{
    Variant[] result;
    foreach (arg; args)
    {
        result ~= Variant(arg);
    }
    return result;
}

/**
 * Thrown in three cases:
 *
 * $(OL $(LI An uninitialized Variant is used in any way except
 * assignment and $(D_PARAM hasValue);) $(LI A $(D_PARAM get) or
 * $(D_PARAM coerce) is attempted with an incompatible target type;)
 * $(LI A comparison between $(D_PARAM Variant) objects of
 * incompatible types is attempted.))
 *
 */

// @@@ BUG IN COMPILER. THE 'STATIC' BELOW SHOULD NOT COMPILE
static class VariantException : Exception
{
    /// The source type in the conversion or comparison
    TypeInfo source;
    /// The target type in the conversion or comparison
    TypeInfo target;
    this(string s)
    {
        super(s);
    }
    this(TypeInfo source, TypeInfo target)
    {
        super("Variant: attempting to use incompatible types "
                            ~ source.toString
                            ~ " and " ~ target.toString);
        this.source = source;
        this.target = target;
    }
}

unittest
{
    alias This2Variant!(char, int, This[int]) W1;
    alias TypeTuple!(int, char[int]) W2;
    static assert(is(W1 == W2));
}

unittest
{
    // @@@BUG@@@
    // alias Algebraic!(real, This[], This[int], This[This]) A;
    // A v1, v2, v3;
    // v2 = 5.0L;
    // v3 = 42.0L;
    // //v1 = [ v2 ][];
    //  auto v = v1.peek!(A[]);
    // //writeln(v[0]);
    // v1 = [ 9 : v3 ];
    // //writeln(v1);
    // v1 = [ v3 : v3 ];
    // //writeln(v1);
}

unittest
{
    // try it with an oddly small size
    VariantN!(1) test;
    assert(test.size > 1);

    // variantArray tests
    auto heterogeneous = variantArray(1, 4.5, "hi");
    assert(heterogeneous.length == 3);
    auto variantArrayAsVariant = Variant(heterogeneous);
    assert(variantArrayAsVariant[0] == 1);
    assert(variantArrayAsVariant.length == 3);

    // array tests
    auto arr = Variant([1.2].dup);
    auto e = arr[0];
    assert(e == 1.2);
    arr[0] = 2.0;
    assert(arr[0] == 2);
    arr ~= 4.5;
    assert(arr[1] == 4.5);

    // general tests
    Variant a;
    auto b = Variant(5);
    assert(!b.peek!(real) && b.peek!(int));
    // assign
    a = *b.peek!(int);
    // comparison
    assert(a == b, a.type.toString ~ " " ~ b.type.toString);
    auto c = Variant("this is a string");
    assert(a != c);
    // comparison via implicit conversions
    a = 42; b = 42.0; assert(a == b);

    // try failing conversions
    bool failed = false;
    try
    {
        auto d = c.get!(int);
    }
    catch (Exception e)
    {
        //writeln(stderr, e.toString);
        failed = true;
    }
    assert(failed); // :o)

    // toString tests
    a = Variant(42); assert(a.toString == "42");
    a = Variant(42.22); assert(a.toString == "42.22");

    // coerce tests
    a = Variant(42.22); assert(a.coerce!(int) == 42);
    a = cast(short) 5; assert(a.coerce!(double) == 5);

    // Object tests
    class B1 {}
    class B2 : B1 {}
    a = new B2;
    assert(a.coerce!(B1) !is null);
    a = new B1;
// BUG: I can't get the following line to pass:
//    assert(collectException(a.coerce!(B2) is null));
    a = cast(Object) new B2; // lose static type info; should still work
    assert(a.coerce!(B2) !is null);

//     struct Big { int a[45]; }
//     a = Big.init;

    // hash
    assert(a.toHash != 0);
}

// tests adapted from
// http://www.dsource.org/projects/tango/browser/trunk/tango/core/Variant.d?rev=2601
unittest
{
    Variant v;

    assert(!v.hasValue);
    v = 42;
    assert( v.peek!(int) );
    assert( v.convertsTo!(long) );
    assert( v.get!(int) == 42 );
    assert( v.get!(long) == 42L );
    assert( v.get!(ulong) == 42uL );

    // should be string... @@@BUG IN COMPILER
    v = "Hello, World!"c;
    assert( v.peek!(string) );

    assert( v.get!(string) == "Hello, World!" );
    assert(!is(char[] : wchar[]));
    assert( !v.convertsTo!(wchar[]) );
    assert( v.get!(string) == "Hello, World!" );

    // Literal arrays are dynamically-typed
    v = cast(int[5]) [1,2,3,4,5];
    assert( v.peek!(int[5]) );
    assert( v.get!(int[5]) == [1,2,3,4,5] );

    {
        // @@@BUG@@@: array literals should have type T[], not T[5] (I guess)
        // v = [1,2,3,4,5];
        // assert( v.peek!(int[]) );
        // assert( v.get!(int[]) == [1,2,3,4,5] );
    }

    v = 3.1413;
    assert( v.peek!(double) );
    assert( v.convertsTo!(real) );
    //@@@ BUG IN COMPILER: DOUBLE SHOULD NOT IMPLICITLY CONVERT TO FLOAT
    assert( !v.convertsTo!(float) );
    assert( *v.peek!(double) == 3.1413 );

    auto u = Variant(v);
    assert( u.peek!(double) );
    assert( *u.peek!(double) == 3.1413 );

    // operators
    v = 38;
    assert( v + 4 == 42 );
    assert( 4 + v == 42 );
    assert( v - 4 == 34 );
    assert( Variant(4) - v == -34 );
    assert( v * 2 == 76 );
    assert( 2 * v == 76 );
    assert( v / 2 == 19 );
    assert( Variant(2) / v == 0 );
    assert( v % 2 == 0 );
    assert( Variant(2) % v == 2 );
    assert( (v & 6) == 6 );
    assert( (6 & v) == 6 );
    assert( (v | 9) == 47 );
    assert( (9 | v) == 47 );
    assert( (v ^ 5) == 35 );
    assert( (5 ^ v) == 35 );
    assert( v << 1 == 76 );
    assert( Variant(1) << Variant(2) == 4 );
    assert( v >> 1 == 19 );
    assert( Variant(4) >> Variant(2) == 1 );
    assert( Variant("abc") ~ "def" == "abcdef" );
    assert( Variant("abc") ~ Variant("def") == "abcdef" );

    v = 38;
    v += 4;
    assert( v == 42 );
    v = 38; v -= 4; assert( v == 34 );
    v = 38; v *= 2; assert( v == 76 );
    v = 38; v /= 2; assert( v == 19 );
    v = 38; v %= 2; assert( v == 0 );
    v = 38; v &= 6; assert( v == 6 );
    v = 38; v |= 9; assert( v == 47 );
    v = 38; v ^= 5; assert( v == 35 );
    v = 38; v <<= 1; assert( v == 76 );
    v = 38; v >>= 1; assert( v == 19 );
    v = 38; v += 1;  assert( v < 40 );

    v = "abc";
    v ~= "def";
    assert( v == "abcdef", *v.peek!(char[]) );
    assert( Variant(0) < Variant(42) );
    assert( Variant(42) > Variant(0) );
    assert( Variant(42) > Variant(0.1) );
    assert( Variant(42.1) > Variant(1) );
    assert( Variant(21) == Variant(21) );
    assert( Variant(0) != Variant(42) );
    assert( Variant("bar") == Variant("bar") );
    assert( Variant("foo") != Variant("bar") );

    {
        auto v1 = Variant(42);
        auto v2 = Variant("foo");
        auto v3 = Variant(1+2.0i);

        int[Variant] hash;
        hash[v1] = 0;
        hash[v2] = 1;
        hash[v3] = 2;

        assert( hash[v1] == 0 );
        assert( hash[v2] == 1 );
        assert( hash[v3] == 2 );
    }
    /+
    // @@@BUG@@@
    // dmd: mtype.c:3886: StructDeclaration* TypeAArray::getImpl(): Assertion `impl' failed.
    {
        int[char[]] hash;
        hash["a"] = 1;
        hash["b"] = 2;
        hash["c"] = 3;
        Variant vhash = hash;

        assert( vhash.get!(int[char[]])["a"] == 1 );
        assert( vhash.get!(int[char[]])["b"] == 2 );
        assert( vhash.get!(int[char[]])["c"] == 3 );
    }
    +/
}

unittest
{
    // bug 1558
    Variant va=1;
    Variant vb=-2;
    assert((va+vb).get!(int) == -1);
    assert((va-vb).get!(int) == 3);
}

unittest
{
    Variant a;
    a=5;
    Variant b;
    b=a;
    Variant[] c;
    c = variantArray(1, 2, 3.0, "hello", 4);
    assert(c[3] == "hello");
}

unittest
{
    Variant v = 5;
    assert (!__traits(compiles, v.coerce!(bool delegate())));
}


unittest
{
    struct Huge {
        real a, b, c, d, e, f, g;
    }

    Huge huge;
    huge.e = 42;
    Variant v;
    v = huge;  // Compile time error.
    assert(v.get!(Huge).e == 42);
}

unittest
{
    const x = Variant(42);
    auto y1 = x.get!(const int)();
    // @@@BUG@@@
    //auto y2 = x.get!(immutable int)();
}

// test iteration
unittest
{
    auto v = Variant([ 1, 2, 3, 4 ][]);
    auto j = 0;
    foreach (int i; v)
    {
        assert(i == ++j);
    }
    assert(j == 4);
}


//----------------------------------------------------------------------------//
// Algebraic

/+
 + TODO:
 +
 +  - Make Algebraic.opBinary() etc. accept Algebraic rhs.
 +
 +  - Make Algebraic.empty sensible of infinite ranges.
 +
 +  - opApply
 +
 +  ? Prefer Algebraic!(T1,T2,...) to Algebraic!(R1,R2,...) for the return
 +    type of oveloaded operators of Algebraic!(T1,T2,...) if { R1,R2,...} is
 +    a subset of { T1,T2,... };  this might reduce program size and
 +    compilation time.
 +/


// Hook for canonicalizing template instantiation arguments.
template Algebraic(Types...)
        if (!isAlgebraicCanonicalized!Types)
{
    alias Algebraic!(AlgebraicCanonicalize!Types) Algebraic;
}

private template AlgebraicCanonicalize(Types...)
{
    alias NoDuplicates!Types AlgebraicCanonicalize;

    // validate
    static assert(AlgebraicCanonicalize.length > 0,
            "Attempted to instantiate Algebraic with empty argument");
    static assert(staticIndexOf!(void, AlgebraicCanonicalize) < 0,
            "void is not allowed for Algebraic");
    static assert(isTypeTuple!AlgebraicCanonicalize,
            "Attempted to isntantiate Algebraic with non-type argument(s)");
}

private template isAlgebraicCanonicalized(Types...)
{
    enum bool isAlgebraicCanonicalized =
        is(Types == AlgebraicCanonicalize!Types);
}

private template isAlgebraic(A)
{
    enum bool isAlgebraic =
        (UnpackAlgebraicInstantiationArguments!A.length > 0);
}

private template UnpackAlgebraicInstantiationArguments(A)
{
    alias UnpackAlgebraicInstantiationArgumentsImpl!A.Result
                UnpackAlgebraicInstantiationArguments;
}

private template UnpackAlgebraicInstantiationArgumentsImpl(A)
{
    struct Unpack(UU...)
    {
        alias UU Expand;
    }
    extern Unpack!UU unpack(UU...)(Algebraic!UU x);

    static if (is(typeof(unpack(A.init)).Expand Types))
        alias Types        Result;
    else
        alias TypeTuple!() Result;
}


/**
Algebraic data type restricted to a closed set of possible types.
$(D Algebraic) is useful when it is desirable to restrict what a
discriminated type could hold to the end of defining simpler and
more efficient manipulation.

$(D Algebraic) allows compile-time checking that all possible
types are handled by user code, eliminating a large class of
errors.
--------------------
Algebraic!(int, double) x;

// these lines won't compile since long and string are not allowed
auto n = x.Algebraic.instance!long;
x = "abc";
--------------------

Bugs:

$(UL
 $(LI Currently, $(D Algebraic) does not allow recursive data types.
     They will be allowed in a future iteration of the implementation.)
 $(LI $(D opCall) overloads are hidden due to $(BUGZILLA 4243).
 $(LI $(D opApply) overloads are hidden because it conflicts with
     the $(D range) primitives.)
)

Examples:
--------------------
Algebraic!(int, double, string) v = 5;
assert(v.Algebraic.isActive!int);

v = 3.14;
assert(v.Algebraic.isActive!double);

v *= 2;
assert(v > 6);
--------------------

You can call any method $(D Types) have against $(D Algebraic).
--------------------
Algebraic!(string, wstring, dstring) s;

s = "The quick brown fox jumps over the lazy dog.";
assert(s.front == 'T');
assert(s.back == '.');
assert(s.length == 44);
s.popBack;
assert(equal(take(retro(s), 3), "god"));
--------------------

Dispatch a holded object to handlers with $(D Algebraic.dispatch):
--------------------
Algebraic!(int, string) x = 42;

x.Algebraic.dispatch(
        (ref int n)
        {
            writeln("saw an int: ", n);
            ++n;
        },
        (string s)
        {
            writeln("saw a string: ", s);
        }
    );
assert(x == 43);    // incremented
--------------------
 */

struct Algebraic(Types...)
        if (isAlgebraicCanonicalized!(Types))
{
//  private alias This2Variant!(typeof(this), Types) _Types;    // @@@BUG4449@@@
    private alias Types _Types;

    /**
     * Constructs an $(D Algebraic) object holding an object $(D init).
     *
     * See_Also:
     *  $(D opAssign)
     */
    this(T)(T init)
    {
        static assert(_canAssign!(T), "Attempted to construct an "
                ~ typeof(this).stringof ~ " with a disallowed initial "
                "object of type " ~ T.stringof);
        _grab(init);
    }


    /**
     * Assigns $(D rhs) to the $(D Algebraic) object.
     *
     * If $(D T) is one of the $(D Types...), the active object (if any)
     * is destroyed and $(D rhs) takes place of it;  otherwise assignment
     * of $(D rhs) occurs on the active object.
     *
     * Throws:
     *  $(UL
     *   $(LI $(D VariantException) if $(D rhs) is an empty $(D Algebraic)
     *      object.)
     *   $(LI $(D VariantException) if $(D rhs) is an $(D Algebraic) object
     *      holding an object that is not assignable to the left hand side.)
     *  )
     */
    @system void opAssign(T)(T rhs)
            if (_canAssign!(T) && !_isCompatibleAlgebraic!(T))
    {
        static if (_typeCode!T != size_t.max)
        {
            if (_which != size_t.max)
                _dispose();
            _grab(rhs);
        }
        else
        {
            if (_which == size_t.max)
                _grab(rhs);
            else
                _assign(rhs);
        }
    }

    // simple blit
    @trusted void opAssign(T)(T rhs)
            if (is(T == typeof(this)))
    {
        swap(this, rhs);
    }

    // intersection
    @system void opAssign(T)(T rhs)
            if (_isCompatibleAlgebraic!(T) && !is(T == typeof(this)))
    {
        if (rhs._which == size_t.max)
            throw new VariantException("Attempted to assign an empty "
                    ~ T.stringof ~ " to " ~ typeof(this).stringof);

        final switch (rhs._which)
        {
            foreach (U; rhs._Types)
            {
            case rhs._typeCode!U:
                static if (_canAssign!U)
                    return opAssign(rhs._storageAs!U);
                else
                    throw new VariantException("Attempted to assign an "
                        ~ T.stringof ~ " holding " ~ U.stringof ~ " which "
                        "is incompatible with " ~ typeof(this).stringof);
            }
        }
        assert(0);
    }

    private template _canAssign(T, size_t i = 0)
    {
        static if (i < _Types.length && is(_Types[i] Type))
            enum bool _canAssign =
                   is(T : Type)
                || is(typeof(phony!Type = phony!T))
                || _isCompatibleAlgebraic!(T)
                || _canAssign!(T, i + 1);
        else
            enum bool _canAssign = false;
    }

    // Returns true if this and A have nonempty intersection
    private template _isCompatibleAlgebraic(A)
    {
        static if (isAlgebraic!(A) && is(A._Types UU))
            enum bool _isCompatibleAlgebraic =
                _Types.length + UU.length > NoDuplicates!(_Types, UU).length;
        else
            enum bool _isCompatibleAlgebraic = false;
    }

    // @@@BUG4424@@@ workaround
    private template _workaround4424()
        { @disable void opAssign(...) { assert(0); } }
    mixin _workaround4424 _workaround4424_;


    /**
     * Invokes the copy constructor on the active object if any.
     */
    this(this)
    {
        if (_which != size_t.max)
        {
            mixin (_onActiveObject!(
                q{
                    static if (__traits(compiles,
                            _storageAs!Active().__postblit() ))
                        _storageAs!Active().__postblit();
                }));
        }
    }


    /**
     * Invokes the destructor on the active object if any.
     */
    ~this()
    {
        if (_which != size_t.max)
            _dispose();
    }


    //------------------------------------------------------------------------//

    /**
     * $(D Algebraic) namespace for operating on the $(D Algebraic) object
     * itself, not a holded object.
     *
     * Example:
--------------------
Algebraic!(A, B) ab;
assert(ab.Algebraic.empty);

ab = A();
assert(ab.Algebraic.isActive!A);
--------------------
     */
    private template _Algebraic()
    {
        /**
         * The type tuple used to instantiate the $(D Algebraic) with no
         * duplicates.
         *
         * Example:
--------------------
Algebraic!(int, int, real, int) x;
assert(is( x.Algebraic.Types == TypeTuple!(int, real) ));
--------------------
         */
        alias _Types Types;


        /**
         * Returns $(D true) if type $(D T) is contained in $(D Types...).
         */
        template allowed(T)
        {
            enum bool allowed = (_typeCode!T != size_t.max);
        }


        /**
         * Returns $(D true) if an object of type $(D T) can be assigned to
         * the $(D Algebraic) object.
         */
        template canAssign(T)
        {
            enum bool canAssign = _canAssign!T;
        }


        /**
         * Returns $(D true) if the $(D Algebraic) object holds nothing.
         */
        @safe @property nothrow bool empty() const
        {
            return _which == size_t.max;
        }


        /**
         * Returns $(D true) if the type of the active object is $(D T).
         */
        @safe nothrow bool isActive(T)() const
        {
            return _which != size_t.max && _which == _typeCode!T;
        }


        /**
         * Returns $(D true) if and only if the $(D Algebraic) object holds
         * an object implicitly convertible to type $(D T).
         */
        @safe nothrow bool convertsTo(T)() const
        {
            if (_which == size_t.max)
                return false;
            mixin (_onActiveObject!(
                q{
                    return isImplicitlyConvertible!(Active, T);
                }));
            assert(0);
        }


        /**
         * Returns the active object as a reference of type $(D T).  $(D T)
         * must be the type of the active object, i.e.,
         * $(D isActive!T == true).
         *
         * Throws:
         * $(UL
         *   $(LI $(D VariantException) if the $(D Algebraic) object is
         *        empty or $(D T) is not active.)
         * )
         *
         * Example:
--------------------
// take a pointer to the active object
Algebraic!(A, B) ab = A();

assert(ab.Algebraic.isActive!A);
A* p = &(ab.Algebraic.instance!A());
B* q = &(ab.Algebraic.instance!B());    // throws VariantException
--------------------
         */
        @safe @property ref T instance(T)()
                if (allowed!(T))
        {
            if (!isActive!T)
                throw new VariantException("Attempting to peek a reference "
                        ~ "to " ~ T.stringof ~ " in " ~ typeof(this).stringof);
            return _storageAs!T;
        }
        /+ // @@@BUG3748@@@
        @trusted @property nothrow ref inout(T) instance(T)() inout
        +/


        /**
         * Returns the active object as a value of type $(D T) allowing
         * implicit convertion.
         *
         * Throws:
         *  $(UL
         *   $(LI Compile fails if none of $(D Types...) supports implicit
         *      convertion to $(D T).)
         *   $(LI $(D VariantException) if the $(D Algebraic) object is
         *      empty or the active object is not implicitly convertible
         *      to $(D T).)
         *  )
         */
        @safe T get(T)()
                if (canGet!(T))
        {
            if (_which == size_t.max)
                throw new VariantException("Attempted to get a value of "
                        "type " ~ T.stringof ~ " out of an empty "
                        ~ typeof(this).stringof);

            mixin (_onActiveObject!(
                q{
                    static if (isImplicitlyConvertible!(Active, T))
                        return _storageAs!Active;
                    else
                        throw new VariantException(Active.stringof
                            ~ "is not implicitly convertible to "
                            ~ T.stringof);
                }));
            assert(0);
        }
        /+ // @@@BUG3748@@@
        @trusted inout(T) get(T)() inout
        +/

        private template canGet(T, size_t i = 0)
        {
            // Convertion must be supported by at least one type.
            static if (i < _Types.length && is(_Types[i] Active))
                enum bool canGet = isImplicitlyConvertible!(Active, T)
                        || canGet!(T, i + 1);
            else
                enum bool canGet = false;
        }


        /**
         * Returns the active object explicitly converted to $(D T) using
         * $(D std.conv.to!T).
         *
         * Throws:
         *  $(UL
         *   $(LI Compile fails if none of $(D Types...) supports explicit
         *      convertion to $(D T).)
         *   $(LI $(D VariantException) if the $(D Algebraic) object is
         *      empty.)
         *   $(LI $(D ConvError) on any convertion error.)
         *  )
         */
        @system T coerce(T)()
                if (canCoerce!(T))
        {
            if (_which == size_t.max)
                throw new VariantException("Attempted to coerce an empty "
                        ~ typeof(this).stringof ~ " into " ~ T.stringof);

            mixin (_onActiveObject!(
                q{
                    static if (__traits(compiles, to!T(phony!Active)))
                        return to!T(_storageAs!Active);
                    else
                        throw new ConvError("Can't convert a value of type "
                            ~ Active.stringof ~ " to type " ~ T.stringof);
                }));
            assert(0);
        }

        private template canCoerce(T, size_t i = 0)
        {
            // Convertion must be supported by at least one type.
            static if (i < _Types.length && is(_Types[i] Active))
                enum bool canCoerce = __traits(compiles, to!T(phony!Active))
                        || canCoerce!(T, i + 1);
            else
                enum bool canCoerce = false;
        }


        /**
         * Passes a reference to the active object by reference to
         * appropriate $(D handlers) in turn.
         *
         * Returns:
         *  $(D true) if at least one handler is called.
         *
         * Example:
--------------------
Algebraic!(A, B, C) x = A.init;

// will print "saw an A" and "it's an A"
auto matched = x.Algebraic.dispatch(
        (A obj) { writeln("saw an A"); },
        (B obj) { writeln("saw a B"); },
        (ref A obj) { writeln("it's an A"); }
    );
assert(matched == true);
--------------------
         */
        bool dispatch(Handlers...)(Handlers handlers)
        {
            if (_which == size_t.max)
                return false;

            uint match = 0;
            mixin (_onActiveObject!(
                q{
                    foreach (handler; handlers)
                    {
                        static if (__traits(compiles, handler(_storageAs!Active)))
                        {
                            handler(_storageAs!Active);
                            ++match;
                        }
                    }
                }));
            return match != 0;
        }
    }

    /// Ditto
    alias _Algebraic!() Algebraic;


    //--------------------------------------------------------------------//
    // standard operator overloads

    /*
     * Generates code for returning $(D expr) evaluated on the active
     * object of type $(D Active).  $(D typeof(return)) must be inferable.
     */
    private template _returnUsingActiveObject(string expr)
    {
        enum string _returnUsingActiveObject = _onActiveObject!(
             "static if (is(typeof(0, (" ~ expr ~ ")) Ri))"
            ~"{"
                ~"static if (is(Ri == void))"
                ~"{"
                    ~"return (" ~ expr ~ "), byValue!(typeof(return));"
                ~"}"
                ~"else"
                ~"{"
                    ~"return byValue!(typeof(return))(" ~ expr ~ ");"
                ~"}"
            ~"}");
    }

    /*
     * Returns Algebraic!( RTs[0], RTs[1], ... ), or $(D void) if $(D RTs)
     * is empty or consisting only of $(D void)s.
     */
    private template _ReduceOpGenericRT(RTs...)
    {
        static if (is(Erase!(void, staticMap!(Unqual, RTs)) NVRTs) &&
                    NVRTs.length > 0)
            alias .Algebraic!NVRTs _ReduceOpGenericRT;
        else
            alias void             _ReduceOpGenericRT;
    }

    /*
     * Maps Types --> { typeof(expr) | Active in Types }, where expr is
     * an expression in which Active and Args are used.
     */
    private template _MapOpGenericRTs(size_t i, string expr, Args...)
    {
        static if (i < _Types.length && is(_Types[i] Active))
        {
            static if (is(typeof(0, mixin(expr)) R))
                alias TypeTuple!(R, _MapOpGenericRTs!(i + 1, expr, Args))
                            _MapOpGenericRTs;
            else
                alias _MapOpGenericRTs!(i + 1, expr, Args)
                            _MapOpGenericRTs;
        }
        else
        {
            alias TypeTuple!() _MapOpGenericRTs;
        }
    }


    /**
     * Evaluates a method or a property $(D op) on the active object.
     *
     * The operation $(D op) must be defined by at least one type in the
     * allowed $(D Types...).
     *
     * Params:
     *  args = The arguments to be passed to the method.
     *
     * Returns:
     *  The value returned by the active object's method $(D op).
     *
     *  The type of the result is the $(D CommonType) of all $(D op)s on
     *  possible $(D Types...).  It's returned by reference if all the
     *  return types are identical and returned by reference.
--------------------
Algebraic!(int, real) n = 0;
assert(is(typeof(n.max) == real));

Algebraic!(int[], Retro!(int[])) r = [ 1, 2, 3 ];
auto p = &(r.front());
assert(*p == 1);
--------------------
     *
     * Throws:
     *  $(UL
     *   $(LI $(D VariantException) if the $(D Algebraic) object is
     *        empty.)
     *   $(LI $(D VariantException) if the method $(D op) is not
     *        defined by the active object.)
     *  )
     *
     * BUGS:
     *  $(UL
     *   $(LI Forward reference errors may occur due to $(BUGZILLA 3294).
     *       For example, $(D hasLength) reports $(D false) even if the
     *       $(D length) property is really available.)
     *  )
     */
    auto ref opDispatch(string op, Args...)(auto ref Args args)
            if (_canDispatch!(op, Args))
    {
        alias CommonType!(_MapOpDispatchRTs!(op, Args)) RT;
        enum byRef    = _canDispatchByRef!(op, Args);

        enum attempt  = (Args.length ? op ~ Args.stringof : op);
        enum dispatch = "_storageAs!Active()."
                ~ (args.length ? op ~ "(args)" : op);

        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));
        //
        mixin (_onActiveObject!(
            q{
                static if (is(typeof(0, mixin(dispatch)) Ri))
                {
                    static if (byRef)
                    {
                        // Return the result by reference.
                        return mixin(dispatch);
                    }
                    else static if (!is(RT == void) && !is(Ri == void))
                    {
                        // Return the result by value of type RT.
                        return byValue!RT(mixin(dispatch));
                    }
                    else
                    {
                        // Just dispatch the method and return nothing.
                        return mixin(dispatch), byValue!RT;
                    }
                }
            }));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpDispatchRTs(string op, Args...)
    {
        alias _MapOpGenericRTs!(0,
                "phony!Active."
                    ~ (Args.length ? op ~ "(phonyList!Args)" : op),
                Args
            ) _MapOpDispatchRTs;
    }

    private template _canDispatch(string op, Args...)
    {
        enum bool _canDispatch =
            _MapOpDispatchRTs!(op, Args).length > 0;
    }

    private template _canDispatchByRef(string op, Args...)
    {
        enum bool _canDispatchByRef =
            NoDuplicates!(_MapOpDispatchRTs!(op, Args)).length == 1
            &&
            __traits(compiles, function(Args args)
                {
                    enum dispatch = "obj."
                        ~ (args.length ? op ~ "(args)" : op);
                    foreach (Type; _Types)
                    {
                        Type obj;
                        expectLvalue(mixin(dispatch));
                    }
                });
    }


    //--------------------------------------------------------------------//

    /*
     * <op>storageAs!Active
     */
    _ReduceOpGenericRT!(_MapOpUnaryRTs!(op))
        opUnary(string op)()
    {
        enum string attempt = "unary " ~ op;

        static assert(_MapOpUnaryRTs!(op).length, _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
                op~"_storageAs!Active"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpUnaryRTs(string op)
    {
        alias _MapOpGenericRTs!(0, op~"phony!Active") _MapOpUnaryRTs;
    }


    /*
     * storageAs!Active <op> rhs
     */
    _ReduceOpGenericRT!(_MapOpBinaryRTs!(op, RHS))
        opBinary(string op, RHS)(RHS rhs)
    {
        enum string attempt = "binary " ~ op ~ " " ~ RHS.stringof;

        static assert(_MapOpBinaryRTs!(op, RHS).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
                "_storageAs!Active() "~op~" rhs"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpBinaryRTs(string op, RHS)
    {
        alias _MapOpGenericRTs!(0,
                "phony!Active "~op~" phony!(Args[0])",
                RHS
            ) _MapOpBinaryRTs;
    }


    /*
     * lhs <op> storageAs!Active
     */
    _ReduceOpGenericRT!(_MapOpBinaryRightRTs!(op, LHS))
        opBinaryRight(string op, LHS)(LHS lhs)
            if (!_isCompatibleAlgebraic!(LHS))
    {
        enum string attempt = "binary " ~ LHS.stringof ~ " " ~ op;

        static assert(_MapOpBinaryRightRTs!(op, LHS).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
                "lhs "~op~" _storageAs!Active"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpBinaryRightRTs(string op, LHS)
    {
        alias _MapOpGenericRTs!(0,
                "phony!(Args[0]) " ~ op ~ " phony!Active",
                LHS
            ) _MapOpBinaryRightRTs;
    }


    /*
     * storageAs!Active[ indices[0], ... ]
     */
    _ReduceOpGenericRT!(_MapOpIndexRTs!(Indices))
        opIndex(Indices...)(Indices indices)
    {
        enum size_t K       = Indices.length;
        enum string attempt = to!string(K) ~ "-indexing";

        static assert(_MapOpIndexRTs!(Indices).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
//              "_storageAs!Active()[indices]" // @@@BUG4444@@@
                "_storageAs!Active()[" ~ expandArray("indices", K) ~ "]"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpIndexRTs(Indices...)
    {
        alias _MapOpGenericRTs!(0,
//              "phony!Active[phonyList!Args]", // @@@BUG4444@@@
                "phony!Active[" ~ expandArray("phonyList!Args", Indices.length) ~ "]",
                Indices
            ) _MapOpIndexRTs;
    }


    /*
     * storageAs!Active[i .. j]
     */
    _ReduceOpGenericRT!(_MapOpSliceRTs!(I, J))
        opSlice(I, J)(I i, J j)
    {
        enum string attempt = "slicing";

        static assert(_MapOpSliceRTs!(I, J).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
                "_storageAs!Active()[i .. j]"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpSliceRTs(I, J)
    {
        alias _MapOpGenericRTs!(0,
                "phony!Active[phony!(Args[0]) .. phony!(Args[1])]",
                I, J
            ) _MapOpSliceRTs;
    }


    /*
     * storageAs!Active[]
     */
    _ReduceOpGenericRT!(_MapOpSliceRTs!())
        opSlice()()
    {
        enum string attempt = "whole-slicing";

        static assert(_MapOpSliceRTs!().length, _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
                "_storageAs!Active()[]"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpSliceRTs()
    {
        alias _MapOpGenericRTs!(0, "phony!Active[]") _MapOpSliceRTs;
    }


    /*
     * <op>storageAs!Active[ indices[0], ... ]
     */
    _ReduceOpGenericRT!(_MapOpIndexUnaryRTs!(op, Indices))
        opIndexUnary(string op, Indices...)(Indices indices)
    {
        enum size_t K       = Indices.length;
        enum string attempt = to!string(K) ~ "-indexing unary " ~ op;

        static assert(_MapOpIndexUnaryRTs!(op, Indices).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
//              op~"_storageAs!Active()[indices]"   // @@@BUG4444@@@
                op~"_storageAs!Active()[" ~ expandArray("indices", K) ~ "]"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpIndexUnaryRTs(string op, Indices...)
    {
        alias _MapOpGenericRTs!(0,
//              op~"phony!Active[phonyList!Args]",  // @@@BUG4444@@@
                op~"phony!Active["
                    ~ expandArray("phonyList!Args", Indices.length) ~ "]",
                Indices
            ) _MapOpIndexUnaryRTs;
    }


    /*
     * <op>storageAs!Active[i .. j]
     */
    _ReduceOpGenericRT!(_MapOpSliceUnaryRTs!(op, I, J))
        opSliceUnary(string op, I, J)(I i, J j)
    {
        enum string attempt = "unary slicing " ~ op;

        static assert(_MapOpSliceUnaryRTs!(op, I, J).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
                op~"_storageAs!Active()[i .. j]"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpSliceUnaryRTs(string op, I, J)
    {
        alias _MapOpGenericRTs!(0,
                op~"phony!Active[phony!(Args[0]) .. phony!(Args[1])]",
                I, J
            ) _MapOpSliceUnaryRTs;
    }


    /*
     * <op>storageAs!Active[]
     */
    _ReduceOpGenericRT!(_MapOpSliceUnaryRTs!(op))
        opSliceUnary(string op)()
    {
        enum string attempt = "unary whole-slicing " ~ op;

        static assert(_MapOpSliceUnaryRTs!(op).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
                op~"_storageAs!Active()[]"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpSliceUnaryRTs(string op)
    {
        alias _MapOpGenericRTs!(0, op ~ "phony!Active[]") _MapOpSliceUnaryRTs;
    }


    /*
     * storageAs!Active[ indices[0], ... ] = rhs
     */
    _ReduceOpGenericRT!(_MapOpIndexAssignRTs!(RHS, Indices))
        opIndexAssign(RHS, Indices...)(RHS rhs, Indices indices)
            if (!_isCompatibleAlgebraic!(RHS))
    {
        enum size_t K       = Indices.length;
        enum string attempt = to!string(K) ~ "-indexing assignment of "
                                ~ RHS.stringof;

        static assert(_MapOpIndexAssignRTs!(RHS, Indices).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
//              "_storageAs!Active()[indices] = rhs"    // @@@BUG4444@@@
                "_storageAs!Active()[" ~ expandArray("indices", K) ~ "] = rhs"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpIndexAssignRTs(RHS, Indices...)
    {
        alias _MapOpGenericRTs!(0,
//              "phony!Active[ phonyList!(Args[1 .. $]) ] = phony!(Args[0])", // @@@BUG4444@@@
                "phony!Active[" ~ expandArray(
                    "phonyList!(Args[1 .. $])", Indices.length)
                        ~ "] = phony!(Args[0])",
                RHS, Indices
            ) _MapOpIndexAssignRTs;
    }


    /*
     * storageAs!Active[i .. j] = rhs
     */
    _ReduceOpGenericRT!(_MapOpSliceAssignRTs!(RHS, I, J))
        opSliceAssign(RHS, I, J)(RHS rhs, I i, J j)
            if (!_isCompatibleAlgebraic!(RHS))
    {
        enum string attempt = "slicing assignment of " ~ RHS.stringof;

        static assert(_MapOpSliceAssignRTs!(RHS, I, J).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
                "_storageAs!Active()[i .. j] = rhs"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpSliceAssignRTs(RHS, I, J)
    {
        alias _MapOpGenericRTs!(0,
                "phony!Active[phony!(Args[1]) .. phony!(Args[2])]"
                    ~ " = phony!(Args[0])",
                RHS, I, J
            ) _MapOpSliceAssignRTs;
    }


    /*
     * storageAs!Active[] = rhs
     */
    _ReduceOpGenericRT!(_MapOpSliceAssignRTs!(RHS))
        opSliceAssign(RHS)(RHS rhs)
            if (!_isCompatibleAlgebraic!(RHS))
    {
        enum string attempt = "whole-slicing assignment of " ~ RHS.stringof;

        static assert(_MapOpSliceAssignRTs!(RHS).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
                "_storageAs!Active()[] = rhs"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpSliceAssignRTs(RHS)
    {
        alias _MapOpGenericRTs!(0,
                "phony!Active[] = phony!(Args[0])",
                RHS
            ) _MapOpSliceAssignRTs;
    }


    /*
     * storageAs!Active <op>= rhs
     */
    _ReduceOpGenericRT!(_MapOpOpAssignRTs!(op, RHS))
        opOpAssign(string op, RHS)(RHS rhs)
            if (!_isCompatibleAlgebraic!(RHS))
    {
        enum string attempt = "binary assignment "~op~"= " ~ RHS.stringof;

        static assert(_MapOpOpAssignRTs!(op, RHS).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
                "_storageAs!Active() "~op~"= rhs"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpOpAssignRTs(string op, RHS)
    {
        alias _MapOpGenericRTs!(0,
                "phony!Active "~op~"= phony!(Args[0])",
                RHS
            ) _MapOpOpAssignRTs;
    }


    /*
     * storageAs!Active[ indices[0], ... ] <op>= rhs
     */
    _ReduceOpGenericRT!(_MapOpIndexOpAssignRTs!(op, RHS, Indices))
        opIndexOpAssign(string op, RHS, Indices...)(RHS rhs, Indices indices)
            if (!_isCompatibleAlgebraic!(RHS))
    {
        enum string attempt = "binary indexing assignment "
                                ~op~"= " ~ RHS.stringof;

        static assert(_MapOpIndexOpAssignRTs!(op, RHS, Indices).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
//              "_storageAs!Active()[indices] "~op~"= rhs"  // @@@BUG4444@@@
                "_storageAs!Active()["
                        ~ expandArray("indices", Indices.length)
                    ~ "] "~op~"= rhs"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpIndexOpAssignRTs(string op, RHS, Indices...)
    {
        alias _MapOpGenericRTs!(0,
//              "phony!Active[phonyList!(Args[1 .. $])] "
//                  ~op~"= phony!(Args[0])",    // @@@BUG4444@@@
                "phony!Active[" ~ expandArray(
                    "phonyList!(Args[1 .. $])", Indices.length) ~ "] "
                    ~op~"= phony!(Args[0])",
                RHS, Indices
            ) _MapOpIndexOpAssignRTs;
    }


    /*
     * storageAs!Active[i .. j] <op>= rhs
     */
    _ReduceOpGenericRT!(_MapOpSliceOpAssignRTs!(op, RHS, I, J))
         opSliceOpAssign(string op, RHS, I, J)(RHS rhs, I i, J j)
            if (!_isCompatibleAlgebraic!(RHS))
    {
        enum string attempt = "binary slicing assignment "
                                ~op~"= " ~ RHS.stringof;

        static assert(_MapOpSliceOpAssignRTs!(op, RHS, I, J).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
                "_storageAs!Active()[i .. j] "~op~"= rhs"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpSliceOpAssignRTs(string op, RHS, I, J)
    {
        alias _MapOpGenericRTs!(0,
                "phony!Active[phony!(Args[1]) .. phony!(Args[2])] "
                    ~op~"= phony!(Args[0])",
                RHS, I, J
            ) _MapOpSliceOpAssignRTs;
    }


    /*
     * storageAs!Active[] <op>= rhs
     */
    _ReduceOpGenericRT!(_MapOpSliceOpAssignRTs!(op, RHS))
        opSliceOpAssign(string op, RHS)(RHS rhs)
            if (!_isCompatibleAlgebraic!(RHS))
    {
        enum string attempt = "binary whole-slicing assignment "
                                ~op~"= " ~ RHS.stringof;

        static assert(_MapOpSliceOpAssignRTs!(op, RHS).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
                "_storageAs!Active()[] "~op~"= rhs"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpSliceOpAssignRTs(string op, RHS)
    {
        alias _MapOpGenericRTs!(0,
                "phony!Active[] "~op~"= phony!(Args[0])",
                RHS
            ) _MapOpSliceOpAssignRTs;
    }


    //--------------------------------------------------------------------//

    /*
     * cast(T) storageAs!Active
     */
    T opCast(T)()
    {
        enum attempt = "casting to " ~ T.stringof;

        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));
        mixin (_onActiveObject!(
            q{
                static if (is(Active : T))
                    return         _storageAs!Active;
//              else static if (__traits(compiles, cast(T) _storageAs!Active))  // @@@ e2ir
                else static if (is(typeof(Active.opCast!T) R == return) && is(R == T))
                    return cast(T) _storageAs!Active;
            }));
        throw new VariantException(_undefinedOpMsg(attempt));
    }


    /*
     * storageAs!Active == rhs
     */
    bool opEquals(RHS)(auto ref RHS rhs) const
            if (!_isCompatibleAlgebraic!(RHS))
    {
        enum attempt = "equality comparison with " ~ RHS.stringof;

        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));
        mixin (_onActiveObject!(
            q{
                static if (__traits(compiles,
                        _storageAs_const!Active() == rhs))
                    return _storageAs_const!Active() == rhs;
            }));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    // Algebraic vs. Algebraic
    bool opEquals(RHS)(ref const RHS rhs) const
            if (is(RHS == typeof(this)))
    {
        if (_which != size_t.max && _which == rhs._which)
        {
            mixin (_onActiveObject!(
                q{
                    return _storageAs_const!Active() ==
                            rhs._storageAs_const!Active;
                }));
            assert(0);
        }
        else
        {
            return false;
        }
    }


    /*
     * storageAs!Active <>= rhs
     */
    int opCmp(RHS)(auto ref RHS rhs) const
            if (!_isCompatibleAlgebraic!(RHS))
    {
        enum attempt = "ordering comparison with " ~ RHS.stringof;

        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));
        mixin (_onActiveObject!(
            q{
                static if (__traits(compiles, _storageAs_const!Active < rhs))
                {
                    return (_storageAs_const!Active < rhs) ? -1 :
                           (_storageAs_const!Active > rhs) ?  1 : 0;
                }
            }));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    // Algebraic vs. Algebraic
    int opCmp(RHS)(ref const RHS rhs) const
            if (is(RHS == typeof(this)))
    {
        enum attempt = "ordering comparison";

        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));
        mixin (_onActiveObject!(
            q{
                return -rhs.opCmp(_storageAs_const!Active);
            }));
        assert(0);
    }

/+
// @@@BUG4253@@@
    _ReduceOpGenericRT!(_MapOpCallRTs!(Args))
        opCall(Args...)(auto ref Args args)
    {
        enum string attempt = "calling operator";

        static assert(_MapOpCallRTs!(Args).length,
                _invalidOpMsg(attempt));
        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));

        mixin (_returnUsingActiveObject!(
                "_storageAs!Active(args)"
            ));
        throw new VariantException(_undefinedOpMsg(attempt));
    }

    private template _MapOpCallRTs(Args...)
    {
        alias _MapOpGenericRTs!(0,
                "phony!Active(phonyList!Args)",
                Args
            ) _MapOpCallRTs;
    }
+/

/+
// @@@ cannot coexist with input range primitives
    int opApply(Args...)(int delegate(ref Args) dg)
    {
        enum attempt = "foreach";

        if (_which == size_t.max)
            throw new VariantException(_emptyMsg(attempt));
        mixin (_onActiveObject!(
            q{
                static if (__traits(compiles,
                        _storageAs!Active().opApply(dg)))
                    return _storageAs!Active().opApply(dg);
            }));
        throw new VariantException(_undefinedOpMsg(attempt));
    }
+/


    //--------------------------------------------------------------------//
    // special functions

    @system string toString()
    {
        if (_which == size_t.max)
            return typeof(this).stringof ~ "(empty)";

        mixin (_onActiveObject!(
            q{
                static if (__traits(compiles,
                        to!string(_storageAs!Active) ))
                    return to!string(_storageAs!Active);
                else
                    return Active.stringof;
            }));
        assert(0);
    }
//  @system string toString(
//      void delegate(const(char)[]) sink = null,
//      string fmt = null)

    @system hash_t toHash() const
    {
        if (_which == size_t.max)
            return 0;

        mixin (_onActiveObject!(
            q{
                return typeid(Active).getHash(_storage.ptr);
            }));
        assert(0);
    }


    // input range primitives
    static if (_canDispatch!"front" &&
               _canDispatch!"empty" &&
               _canDispatch!"popFront")
    {
        @property bool empty()
        {
            return opDispatch!"empty"();
        }
        @property auto ref front()
        {
            return opDispatch!"front"();
        }
        void popFront()
        {
            opDispatch!"popFront"();
        }
    }

    // forward range primitive
    static if (_canDispatch!"save")
    {
        @property typeof(this) save()
        {
            enum string attempt = "range primitive save()";
            if (_which == size_t.max)
                throw new VariantException(_emptyMsg(attempt));
            mixin (_returnUsingActiveObject!(
                    "_storageAs!Active.save"
                ));
            throw new VariantException(_undefinedOpMsg(attempt));
        }
    }

    // bidirectional range primitives
    static if (_canDispatch!"back" &&
               _canDispatch!"popBack")
    {
        @property auto ref back()
        {
            return opDispatch!"back"();
        }
        void popBack()
        {
            opDispatch!"popBack"();
        }
    }


    //------------------------------------------------------------//
    // internals
private:

    /*
     * Returns the internal code number of the type $(D T), or
     * $(D size_t.max) if $(D T) is not in $(D Types...).
     */
    template _typeCode(T, size_t id = 0)
    {
        static if (id < _Types.length)
        {
            static if (is(T == _Types[id]))
                enum size_t _typeCode = id;
            else
                enum size_t _typeCode = _typeCode!(T, id + 1);
        }
        else
        {
            enum size_t _typeCode = size_t.max;
        }
    }


    /*
     * Returns a reference to the storage as an object of type $(D T).
     */
    @trusted nothrow ref T _storageAs(T)()
            if (_typeCode!T != size_t.max)
    {
        return *cast(T*) _storage.ptr;
    }
    @trusted nothrow ref const(T) _storageAs_const(T)() const
            if (_typeCode!T != size_t.max)
    {
        return *cast(const T*) _storage.ptr;
    }
    /+ // @@@BUG3748@@@
    @trusted nothrow ref inout(T) storageAs(T)() inout
    +/


    /*
     * Generates code for doing $(D stmt) on the active object.
     */
    template _onActiveObject(string stmt)
    {
        enum string _onActiveObject =
                 "assert(_which != size_t.max);"
            ~"L_chooseActive:"
                ~"final switch (_which)"
                ~"{"
                    ~"foreach (Active; _Types)"
                    ~"{"
                    ~"case _typeCode!Active:"
                        ~stmt
                        ~"break L_chooseActive;"
                    ~"}"
                ~"}";
    }


    /*
     * Error message for an attempting against any operation on an
     * empty Algebraic object.
     */
    static @safe pure nothrow string _emptyMsg(string attempt)
    {
        return "Attempted to evaluate " ~ attempt ~ " on an empty "
            ~ typeof(this).stringof;
    }

    /*
     * Error message for an attempting to evaluate any operation that
     * is not defined by any of $(D Types...).
     */
    static @safe pure nothrow string _invalidOpMsg(string op)
    {
        return "No type in " ~ typeof(this).stringof ~ " defines " ~ op;
    }

    /*
     * Error message for an attempting to evaluate any operation that
     * is not defined by the active object.
     */
    @safe nothrow string _undefinedOpMsg(string op) const
    in
    {
        assert(_which != size_t.max);
    }
    body
    {
        string typeName;

        mixin (_onActiveObject!(
            q{
                typeName = Active.stringof;
            }));
        return "An active object of type " ~ typeName ~ " does not "
            ~ "define " ~ op;
    }


    /*
     * Store $(D rhs) in the internal storage.
     */
    @system void _grab(T)(ref T rhs)
    in
    {
        assert(_which == size_t.max);
    }
    out
    {
        assert(_which != size_t.max);
    }
    body
    {
        static if (_typeCode!T != size_t.max)
        {
            // Simple blit.
            initialize(_storageAs!T);
            swap(_storageAs!T, rhs);
            _which = _typeCode!T;
        }
        else
        {
            // Use opAssign matched first.
            foreach (Active; _Types)
            {
                static if (__traits(compiles, _storageAs!Active() = rhs))
                {
                    initialize(_storageAs!Active);
                    _storageAs!Active() = rhs;  // may be @system
                    _which = _typeCode!Active;
                    break;
                }
            }
        }
    }


    /*
     * Assigns $(D rhs) to the non-empty active storage.
     */
    @system void _assign(T)(ref T rhs)
    in
    {
        assert(_which != size_t.max);
    }
    body
    {
        mixin (_onActiveObject!(
            q{
                static if (__traits(compiles, _storageAs!Active() = rhs))
                    return _storageAs!Active() = rhs;   // may be @system
            }));

        // Or, replace the content with rhs.
        _dispose();
        _grab(rhs);
    }


    /*
     * Destroys the active object (if it's a struct) and marks this
     * $(D Algebraic) object empty.
     */
    @trusted void _dispose()
    in
    {
        assert(_which != size_t.max);
    }
    out
    {
        assert(_which == size_t.max);
    }
    body
    {
        mixin (_onActiveObject!(
            q{
                static if (__traits(compiles, _storageAs!Active.__dtor()))
                    _storageAs!Active.__dtor();
                _which = size_t.max;
                return;
            }));
        assert(0);
    }


    //------------------------------------------------------------------------//
private:
    void*[(maxSize!_Types + (void*).sizeof - 1) / (void*).sizeof]
            _storage;
    size_t  _which = size_t.max;    // typeCode of the active object
}


version (unittest) private
{
    bool eq(S)(S a, S b)
    {
        foreach (i, _; a.tupleof)
        {
            if (a.tupleof[i] != b.tupleof[i])
                return false;
        }
        return true;
    }

    bool fails(lazy void expr)
    {
        try { expr; } catch (VariantException e) { return true; }
        return false;
    }
}

//-------------- doc examples

unittest
{
    // doc example 0
    Algebraic!(int, double) x;

    // these lines won't compile since long and string are not allowed
//  auto n = x.Algebraic.instance!long;
//  x = "abc";
    assert(!__traits(compiles, x.Algebraic.instance!long));
    assert(!__traits(compiles, x = "abc"));
}

unittest
{
    // doc example 1
    Algebraic!(int, double, string) v = 5;
    assert(v.Algebraic.isActive!int);

    v = 3.14;
    assert(v.Algebraic.isActive!double);

    v *= 2;
    assert(v > 6);
}

unittest
{
    // doc example 2
    Algebraic!(string, wstring, dstring) s;

    s = "The quick brown fox jumps over the lazy dog.";
    assert(s.front == 'T');
    assert(s.back == '.');
    assert(s.length == 44);
    s.popBack;
    assert(equal(take(retro(s), 3), "god"));
}

unittest
{
    // doc exmaple 3
    Algebraic!(int, double, string) x = 42;

    x.Algebraic.dispatch(
            (ref int n)
            {
                //writeln("saw an int: ", n);
                assert(n == 42);
                ++n;
            },
            (string s)
            {
                //writeln("saw a string: ", s);
                assert(0);
            }
        );
    assert(x == 43);    // incremented
}

unittest
{
    // doc example 4
    Algebraic!(int, int, real, int) x;
    assert(is( x.Algebraic.Types == TypeTuple!(int, real) ));
}

unittest
{
    // doc example 5 - take a pointer to the active object
    struct A {}
    struct B {}
    Algebraic!(A, B) ab;
    /+
    Algebraic!(A, B) ab = A();

    assert(ab.Algebraic.isActive!A);
    A* p = &(ab.Algebraic.instance!A());
//  B* q = &(ab.Algebraic.instance!B());    // throws VariantException
    B* q;
    assert(fails( q = &(ab.Algebraic.instance!B()) ));
    +/
}

//--------------

unittest
{
    // funny types
    assert(!__traits(compiles, Algebraic!()));
    assert(!__traits(compiles, Algebraic!(void)));
    assert(!__traits(compiles, Algebraic!(int, void, dchar)));
    assert(!__traits(compiles, Algebraic!(int, 42, short)));
    assert(is( Algebraic!(int, real, int) == Algebraic!(int, real) ));
}

unittest
{
    // copy constructor & destructor
    struct Counter
    {
        int* copies;
        this(this) { copies && ++*copies; }
        ~this()    { copies && --*copies; }
    }
    Algebraic!(Counter) a;
    a = Counter(new int);
    assert(*a.copies == 0);
    {
        auto b = a;
        assert(*a.copies == 1);
        {
            auto c = a;
            assert(*a.copies == 2);
        }
        assert(*a.copies == 1);
    }
    assert(*a.copies == 0);
}

unittest
{
    // basic use
    int afoo, bfoo;
    struct A {
        int inc() { return ++afoo; }
        int dec() { return --afoo; }
    }
    struct B {
        int inc() { return --bfoo; }
        int dec() { return ++bfoo; }
    }
    Algebraic!(A, B) ab;
    ab = A();
    assert(ab.inc() == 1 && afoo == 1);
    assert(ab.dec() == 0 && afoo == 0);
    ab = B();
    assert(ab.inc() == -1 && bfoo == -1);
    assert(ab.dec() ==  0 && bfoo ==  0);
}

unittest
{
    // constructor
    auto x = Algebraic!(int, real)(4.5);
    assert(x == 4.5);
}

unittest
{
    // meta interface
    Algebraic!(int, real) a;

    assert(is(a.Algebraic.Types == TypeTuple!(int, real)));
    assert(a.Algebraic.allowed!int);
    assert(a.Algebraic.allowed!real);
    assert(!a.Algebraic.allowed!string);

    assert(a.Algebraic.canAssign!int);
    assert(a.Algebraic.canAssign!real);
    assert(a.Algebraic.canAssign!byte);
    assert(a.Algebraic.canAssign!short);
    assert(a.Algebraic.canAssign!ushort);
    assert(a.Algebraic.canAssign!double);
    assert(a.Algebraic.canAssign!(const int));
    assert(a.Algebraic.canAssign!(immutable int));
    assert(!a.Algebraic.canAssign!string);
    assert(!a.Algebraic.canAssign!void);
    assert(!a.Algebraic.canAssign!(int*));
    assert(!a.Algebraic.canAssign!(int[]));

    assert(!__traits(compiles, a.Algebraic.get!(int[])));
    assert(!__traits(compiles, a.Algebraic.coerce!(int[])));
    assert(!__traits(compiles, a.Algebraic.instance!(int[])));

    assert(a.Algebraic.empty);

    a = 42;
    assert(!a.Algebraic.empty);
    assert( a.Algebraic.isActive!int);
    assert(!a.Algebraic.isActive!real);
    assert(!a.Algebraic.isActive!string);
    assert(fails( a.Algebraic.instance!real ));
    int* i = &(a.Algebraic.instance!int());
    assert(*i == 42);
    assert(a.Algebraic.convertsTo!(long));
    assert(a.Algebraic.convertsTo!(const uint));
    assert(a.Algebraic.get!real == 42);
    assert(a.Algebraic.coerce!string == "42");

    a = -21.0L;
    assert(!a.Algebraic.empty);
    assert(!a.Algebraic.isActive!int);
    assert( a.Algebraic.isActive!real);
    assert(!a.Algebraic.isActive!string);
    assert(fails( a.Algebraic.instance!int ));
    real* r = &(a.Algebraic.instance!real());
    assert(*r == -21.0L);
    assert(a.Algebraic.get!(const real) == -21.0L);
    assert(a.Algebraic.coerce!string == "-21");
    assert(fails( a.Algebraic.get!int ));

    a = 100;
    assert(a.Algebraic.dispatch(
            (int a) { assert(a == 100); },
            (ref real b) { assert(0); },
            (ref int a) { assert(a == 100); ++a; }
        ));
    assert(a == 101);

    a = a.init;
    assert(a.Algebraic.empty);
    assert(fails( a.Algebraic.instance!int ));
    assert(fails( a.Algebraic.instance!real ));
    assert(fails( a.Algebraic.get!real ));
    assert(fails( a.Algebraic.coerce!string ));
}

unittest
{
    // implicit convertion
    Algebraic!(real, const(char)[]) a;

    a = 42;
    assert(a.Algebraic.isActive!real);
    a = "abc";
    assert(a.Algebraic.isActive!(const(char)[]));
}

unittest
{
    // foreach over input range
    Algebraic!(string, wstring) str;

    assert(isInputRange!(typeof(str)));
    assert(isForwardRange!(typeof(str)));
    assert(isBidirectionalRange!(typeof(str)));
//  assert(isRandomAccessRange!(typeof(str)));  // @@@ forward reference
//  assert(hasLength!(typeof(str)));            // @@@ forward reference

    str = cast(string) "a";
    assert(str.length == 1);
    assert(str.front == 'a');
    str.popFront;
    assert(str.empty);

    str = cast(wstring) "bc";
    assert(str.length == 2);
    str.popFront;
    assert(str.front == 'c');
    assert(!str.empty);

    size_t i;
    str = "\u3067\u3043\u30fc";
    foreach (e; str)
        assert(e == "\u3067\u3043\u30fc"d[i++]);
    foreach_reverse (e; str)
        assert(e == "\u3067\u3043\u30fc"d[--i]);
}

//-------------- operator overloads

unittest
{
    // opDispatch
    struct Tag { string op; int n; }
    struct OpEcho {
        Tag opDispatch(string op)(int n) {
            return Tag(op, n);
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    auto r = obj.foo(42);
    assert(eq( r, Tag("foo", 42) ));
}

unittest
{
    // opDispatch (common type)
    struct K {
        @property  int value() { return 441; }
    }
    struct L {
        @property real value() { return 4.5; }
    }
    Algebraic!(K, L) obj;

    obj = K(); assert(obj.value == 441);
    obj = L(); assert(obj.value == 4.5);

    assert(!__traits(compiles, &(obj.value()) ));
//  assert(is( typeof(obj.value()) == real ));  // @@@ forward reference
    auto r = obj.value;
    assert(is( typeof(r) == real ));
}

unittest
{
    // opDispatch (ref argument & ref return)
    struct K {
        ref int foo(ref int a, ref int b) { ++b; return a; }
    }
    struct L {
        ref int foo(   long a, ref int b) {      return b; }
    }
    Algebraic!(K, L) q;
    int v, w;

    q = K();
    assert(&(q.foo(v, w)) == &v);
    assert(w == 1);

    q = L();
    assert(&(q.foo(v, w)) == &w);
}

unittest
{
    // opDispatch (empty / undefined)
    Algebraic!(int, real) obj;

    assert(fails( obj.max ));
    obj = 42;
    assert(fails( obj.nan ));
}

unittest
{
    // opAssign
    struct Tag { string op; int n; }
    struct OpEcho {
        int n;
        void opAssign(int n) {
            this.n = n;
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    obj = 42;
    assert(obj.n == 42);
}

unittest
{
    // opAssign (intersection)
    Algebraic!(   int, real) a;
    Algebraic!(string, real) b;

    a = 4;
    assert(a.Algebraic.isActive!int);
    b = a;
    assert(b.Algebraic.isActive!real);
    a = b;
    assert(a.Algebraic.isActive!real);
    b = "0";
    assert(b.Algebraic.isActive!string);

    // incompatible: string
    assert(fails( a = b ));
}

unittest
{
    // opUnary
    struct Tag { string op; }
    struct OpEcho {
        Tag opUnary(string op)() {
            return Tag(op);
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    foreach (op; TypeTuple!("+", "-", "~", "*", "++", "--"))
    {
        auto r = mixin(op ~ "obj");
        assert(r.Algebraic.isActive!Tag);
        assert(eq( r.Algebraic.instance!Tag, Tag(op) ));
    }
}

unittest
{
    // opUnary (empty, undefined)
    Algebraic!(int, string) p;

    assert(fails( ~p ));
    p = "D";
    assert(fails( +p ));
}

unittest
{
    // opIndexUnary
    struct Tag { string op; int x; real y; }
    struct OpEcho {
        Tag opIndexUnary(string op)(int x, real y) {
            return Tag(op, x, y);
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    foreach (op; TypeTuple!("+", "-", "~", "*", "++", "--"))
    {
        auto r = mixin(op ~ "obj[4, 2.5]");
        assert(r.Algebraic.isActive!Tag);
        assert(eq( r.Algebraic.instance!Tag, Tag(op, 4, 2.5) ));
    }
}

unittest
{
    // opIndexUnary (empty, undefined)
    Algebraic!(int[], int) obj;

    assert(fails( ++obj[0] ));
    obj = 42;
    assert(fails( --obj[4] ));
}

unittest
{
    // opSliceUnary
    struct Tag { string op; int x; real y; }
    struct OpEcho {
        Tag opSliceUnary(string op, int k = 2)(int x, real y) {
            return Tag(op, x, y);
        }
        Tag opSliceUnary(string op, int k = 0)() {
            return Tag(op, -1, -1);
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    foreach (op; TypeTuple!("+", "-", "~", "*", "++", "--"))
    {
        auto r = mixin(op ~ "obj[4 .. 5.5]");
        assert(r.Algebraic.isActive!Tag);
        assert(eq( r.Algebraic.instance!Tag, Tag(op, 4, 5.5) ));

        auto s = mixin(op ~ "obj[]");
        assert(r.Algebraic.isActive!Tag);
        assert(eq( s.Algebraic.instance!Tag, Tag(op, -1, -1) ));
    }
}

unittest
{
    // opSliceUnary (empty, undefined)
    Algebraic!(int[], int) obj;

    assert(fails( ++obj[0 .. 1] ));
    assert(fails( --obj[] ));
    obj = 42;
    assert(fails( ++obj[0 .. 1] ));
    assert(fails( --obj[] ));
}

unittest
{
    // opCast
    struct OpEcho {
        T opCast(T)() { return T.init; }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    foreach (T; TypeTuple!(int, string, OpEcho, Object, real))
    {
        T r = cast(T) obj;
    }
}

unittest
{
    // opCast (empty, undefined)
    Algebraic!(int, string) obj;

    assert(fails( cast(int) obj ));
    assert(fails( cast(string) obj ));
    obj = 42;
    assert(fails( cast(string) obj ));
    obj = "abc";
    assert(fails( cast(int) obj ));
}

unittest
{
    // opBinary, opBinaryRight
    struct LTag { string op; int v; }
    struct RTag { string op; int v; }
    struct OpEcho {
        LTag opBinary(string op)(int v) {
            return LTag(op, v);
        }
        RTag opBinaryRight(string op)(int v) {
            return RTag(op, v);
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    foreach (op; TypeTuple!("+", "-", "*", "/", "%", "^^", "&",
                "|", "^", "<<", ">>", ">>>", "~", "in"))
    {
        auto r = mixin("obj " ~ op ~ " 42");
        assert(r.Algebraic.isActive!LTag);
        assert(eq( r.Algebraic.instance!LTag, LTag(op, 42) ));

        auto s = mixin("76 " ~ op ~ " obj");
        assert(s.Algebraic.isActive!RTag);
        assert(eq( s.Algebraic.instance!RTag, RTag(op, 76) ));
    }
}

unittest
{
    // opBinary, opBinaryRight (empty, undefined)
    Algebraic!(int, real) obj;

    assert(fails( obj + 4 ));
    assert(fails( 4 + obj ));
    obj = 4.5;
    assert(fails( obj >> 4 ));
    assert(fails( 4 >> obj ));
}

unittest
{
    // opEquals (forward)
    struct Dummy {
        bool opEquals(int v) const {
            return v > 0;
        }
        bool opEquals(ref const Dummy) const { assert(0); }
    }
    Algebraic!(Dummy) obj;

    obj = Dummy();
    assert(obj ==  1);
    assert(obj !=  0);
    assert(obj != -1);
}

unittest
{
    // opEquals (meta)
    struct Dummy(int k) {
        bool opEquals(int kk)(ref const Dummy!kk rhs) const {
            return k == kk;
        }
    }
    Algebraic!(Dummy!0, Dummy!1) a, b;

    a = Dummy!0();
    b = Dummy!1();
    assert(a == a);
    assert(a != b);
    assert(b == b);
}

unittest
{
    // opCmp (forward)
    struct Dummy {
        int opCmp(int v) const {
            return 0 - v;
        }
        int opCmp(ref const Dummy) const { assert(0); }
    }
    Algebraic!(Dummy) a;

    a = Dummy();
    assert(a >= 0);
    assert(a > -1);
    assert(a < 1);
}

unittest
{
    // opCmp (meta)
    struct Dummy(int k) {
        int opCmp(int kk)(ref const Dummy!kk r) const {
            return k - kk;
        }
    }
    Algebraic!(Dummy!0, Dummy!1) a, b;

    a = Dummy!0();
    b = Dummy!1();
    assert(a >= a);
    assert(a <= b);
    assert(a < b);
    assert(b >= a);
    assert(b <= b);
    assert(b > a);
}

unittest
{
    // opCmp (empty, undefined)
    Algebraic!(int, string) obj;

    assert(fails( /+obj < 0+/ obj.opCmp(0) ));
    obj = "abc";
    assert(fails( /+obj > 0+/ obj.opCmp(1) ));
}

/+
// @@@BUG4253@@@
unittest
{
    // opCall
    struct Tag { int x; real y; }
    struct OpEcho {
        Tag opCall(int x, real y) {
            return Tag(x, y);
        }
    }
    Algebraic!(OpEcho) obj;

//  obj = OpEcho(); // @@@BUG@@@
    obj = OpEcho.init;
    auto r = obj(4, 8.5);
    assert(r.Algebraic.isActive!Tag);
    assert(r.Algebraic.instance!Tag == Tag(4, 8.5));
}
+/

unittest
{
    // opIndexAssign
    struct Tag { string v; int i; real j; }
    struct OpEcho {
        Tag opIndexAssign(string v, int i, real j) {
            return Tag(v, i, j);
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    auto r = (obj[1, 2.5] = "abc");
    assert(r.Algebraic.isActive!Tag);
    assert(eq( r.Algebraic.instance!Tag, Tag("abc", 1, 2.5) ));
}

unittest
{
    // opIndexAssign (empty, undefined)
    Algebraic!(int, int[]) obj;

    assert(fails( obj[0] = 4 ));
    obj = 42;
    assert(fails( obj[4] = 0 ));
}

unittest
{
    // opSliceAssign
    struct Tag { string v; int i; real j; }
    struct OpEcho {
        Tag opSliceAssign(string v, int i, real j) {
            return Tag(v, i, j);
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    auto r = (obj[1 .. 2.5] = "abc");
    assert(r.Algebraic.isActive!Tag);
    assert(eq( r.Algebraic.instance!Tag, Tag("abc", 1, 2.5) ));
}

unittest
{
    // opSliceAssign (empty, undefined)
    Algebraic!(int, int[]) obj;

    assert(fails( obj[0 .. 1] = 2 ));
    obj = 42;
    assert(fails( obj[1 .. 2] = 3 ));
}

unittest
{
    // opOpAssign
    struct Tag { string op; int v; }
    struct OpEcho {
        Tag opOpAssign(string op)(int v) {
            return Tag(op, v);
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    foreach (op; TypeTuple!("+", "-", "*", "/", "%", "^^", "&",
                "|", "^", "<<", ">>", ">>>", "~"))
    {
        auto r = mixin("obj " ~ op ~ "= 97");
        assert(r.Algebraic.isActive!Tag);
        assert(eq( r.Algebraic.instance!Tag, Tag(op, 97) ));
    }
}

unittest
{
    // opOpAssign (empty, undefined)
    Algebraic!(int, string) obj;

//  assert(fails( obj *= 4 ));  // exception uncaught. why?
    obj = "abc";
    assert(fails( obj /= 4 ));
}

unittest
{
    // opIndexOpAssign
    struct Tag { string op; int v; int x; real y; }
    struct OpEcho {
        Tag opIndexOpAssign(string op)(int v, int x, real y) {
            return Tag(op, v, x, y);
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    foreach (op; TypeTuple!("+", "-", "*", "/", "%", "^^", "&",
                "|", "^", "<<", ">>", ">>>", "~"))
    {
        auto r = mixin("obj[4, 7.5] " ~ op ~ "= 42");
        assert(r.Algebraic.isActive!Tag);
        assert(eq( r.Algebraic.instance!Tag, Tag(op, 42, 4, 7.5) ));
    }
}

unittest
{
    // opIndexOpAssign (empty, undefined)
    Algebraic!(int, int[]) obj;

    assert(fails( obj[0] += 4 ));
    obj = 42;
    assert(fails( obj[1] *= 4 ));
}

unittest
{
    // opSliceOpAssign
    struct Tag { string op; int v; int i; real j; }
    struct OpEcho {
        Tag opSliceOpAssign(string op, int k = 2)(int v, int i, real j) {
            return Tag(op, v, i, j);
        }
        Tag opSliceOpAssign(string op, int k = 0)(int v) {
            return Tag(op, v, -1, -1);
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    foreach (op; TypeTuple!("+", "-", "*", "/", "%", "^^", "&",
                "|", "^", "<<", ">>", ">>>", "~"))
    {
        auto r = mixin("obj[4 .. 7.5] " ~ op ~ "= 42");
        assert(r.Algebraic.isActive!Tag);
        assert(eq( r.Algebraic.instance!Tag, Tag(op, 42, 4, 7.5) ));

        auto s = mixin("obj[] " ~ op ~ "= 42");
        assert(s.Algebraic.isActive!Tag);
        assert(eq( s.Algebraic.instance!Tag, Tag(op, 42, -1, -1) ));
    }
}

unittest
{
    // opSliceOpAssign (empty, undefined)
    Algebraic!(int, int[]) obj;

    assert(fails( obj[0 .. 1] += 1 ));
    assert(fails( obj[]       -= 2 ));
    obj = 42;
    assert(fails( obj[2 .. 3] *= 3 ));
    assert(fails( obj[]       /= 4 ));
}

unittest
{
    // opIndex
    struct Tag { int i; real j; }
    struct OpEcho {
        Tag opIndex(int i, real j) {
            return Tag(i, j);
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    auto r = obj[4, 9.5];
    assert(r.Algebraic.isActive!Tag);
    assert(eq( r.Algebraic.instance!Tag, Tag(4, 9.5) ));
}

unittest
{
    // opIndex (empty, undefined)
    Algebraic!(int, int[]) obj;

    assert(fails( obj[0] ));
    obj = 42;
    assert(fails( obj[2] ));
}

unittest
{
    // opSlice
    struct Tag { int i; real j; }
    struct OpEcho {
        Tag opSlice(int i, real j) {
            return Tag(i, j);
        }
        Tag opSlice() {
            return Tag(-1, -1);
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    auto r = obj[4 .. 9.5];
    assert(r.Algebraic.isActive!Tag);
    assert(eq( r.Algebraic.instance!Tag, Tag(4, 9.5) ));

    auto s = obj[];
    assert(s.Algebraic.isActive!Tag);
    assert(eq( s.Algebraic.instance!Tag, Tag(-1, -1) ));
}

unittest
{
    // opSlice (empty, undefined)
    Algebraic!(int, int[]) obj;

    assert(fails( obj[0 .. 1] ));
    assert(fails( obj[] ));
    obj = 42;
    assert(fails( obj[2 .. 3] ));
    assert(fails( obj[] ));
}

/+
unittest
{
    // opApply
    struct OpEcho {
        int opApply(int delegate(ref size_t, ref real) dg)
        {
            foreach (i, ref e; [ 1.L, 2.5L, 5.5L ])
                if (auto r = dg(i, e))
                    return r;
            return 0;
        }
    }
    Algebraic!(OpEcho) obj;

    obj = OpEcho();
    foreach (size_t i, ref real e; obj)
        assert(e == [ 1.L, 2.5L, 5.5L ][i]);
}
+/

//-------------- special functions

unittest
{
    // toString
    Algebraic!(int, string, Object) obj;
    obj.toString();

    obj = 42;
    assert(obj.toString() == "42");

    obj = "The quick brown...";
    assert(obj.toString() == "The quick brown...");

    obj = new class { string toString() { return "mew"; } };
    assert(obj.toString() == "mew");

    assert(to!string(obj) == "mew");
}

unittest
{
    // toHash
    Algebraic!(string, Object) obj;
    obj.toHash();

    obj = "I'm in a box.";
    obj.toHash();

    obj = new class { hash_t toHash() { return 42; } };
    assert(obj.toHash() == 42);
}

//-------------- misc

unittest
{
    // class object
    class A {
        int foo(int a, int b) { return a + b; }
    }
    struct B {
        int foo(int a, int b) { return a * b; }
    }
    Algebraic!(A, B) ab;

    ab = new A;
    {
        auto c = ab;
        assert(c.foo(2, 3) == 5);
    }
    assert(ab.foo(4, 5) == 9);

    ab = B();
    assert(ab.foo(6, 7) == 42);
}

unittest
{
    // associative array
    Algebraic!(int[string], real[string]) map;

    map = (int[string]).init;
    map["abc"] = 42;
    assert(("abc" in map) != null);
    assert(map["abc"] == 42);
    map.rehash;
    assert(map.length == 1);

    map = (real[string]).init;
    map["xyz"] = 3.5;
    assert(("xyz" in map) != null);
    assert(map["xyz"] == 3.5);
    map.rehash;
    assert(map.length == 1);
}

/+
// @@@BUG4449@@@ ICE
unittest
{
    // recursive data type
    alias Algebraic!(dchar, This[]) Tree;

    static uint depth(ref Tree tree)
    {
        uint r;

        // pattern match against the tree node
        tree.Algebraic.dispatch(
                (dchar leaf)
                {
                    r = 1;
                },
                (Tree[] subtrees)
                {
                    uint mx = 0;
                    foreach (sub; subtrees)
                        mx = max(mx, depth(sub));
                    r = 1 + mx;
                }
            ) || assert(0);
        return r;
    }

    /*
     *  1:      <root>
     *         /      \
     *  2:   <ab>    <cde>
     *       /  \    /   \
     *  3:  a    b  c    <de>
     *                   /  \
     *  4:              d    e
     */
    Tree tree =
        [ Tree([ Tree('a'),
                 Tree('b') ]),
          Tree([ Tree('c'),
                 Tree([ Tree('d'),
                        Tree('e') ]) ]) ];
    assert(depth(tree) == 4);
}
+/


//----------------------------------------------------------------------------//

/*
 * A dummy lvalue of type T.
 */
private template phony(T)
{
    static extern T phony;
}

unittest
{
    real foo(ref string s) { return 0; }
    assert(is(typeof(++phony!int) == int));
    assert(is(typeof(foo(phony!string)) == real));
}


/*
 * A tuple of dummy lvalues of types TT.
 */
private template phonyList(TT...)
{
    static if (TT.length > 0)
        alias TypeTuple!(phony!(TT[0]), phonyList!(TT[1 .. $]))
                            phonyList;
    else
        alias TypeTuple!()  phonyList;
}

unittest
{
    int foo(Args...)(ref Args args) { return 0; }
    assert(is(typeof(foo(phonyList!(int, dchar))) == int));
}


/*
 * Returns x by a value of type R, or nothing if R is void.
 * Used by Algebraic.opDispatch().
 */
private @system R byValue(R, T)(auto ref T x)
{
    static if (is(R == T))
    {
        return x;
    }
    else static if (!is(R == void))
    {
        R r = x;
        return r;
    }
}

private @safe R byValue(R, T = void)()
{
    static if (!is(R == void))
        return R.init;
}

unittest
{
    static R foo(R)()
    {
        return byValue!R(42);
    }
    assert(foo!int() == 42);
    assert(foo!real() == 42.L);
    assert(is(typeof(foo!void()) == void));

    static R bar(R)()
    {
        return byValue!R;
    }
    assert(bar!int() == int.init);
    assert(is(typeof(bar!void()) == void));
}


/*
 * Compile fails if var is an rvalue.
 */
private @safe nothrow void expectLvalue(T)(ref T var);

unittest
{
    int x;
    assert(__traits(compiles, expectLvalue(x)));
    assert(!__traits(compiles, expectLvalue(42)));
}


/*
 * Initializes a variable $(D obj) with its default initializer without
 * invoking copy constructor nor destructor.
 */
private @trusted void initialize(T)(ref T obj)
{
    static T init;
    memcpy(&obj, &init, T.sizeof);
}

unittest
{
    struct S
    {
        int n;
        this(this) { assert(0); }
    }
    S s;
    s.n = 42;
    assert(s != s.init);
    initialize(s);
    assert(s == s.init);
}


/*
 * Workaround for the issue 4444.
 */
private @trusted string expandArray(string array, size_t n)
{
    string expr = "";

    foreach (i; 0 .. n)
    {
        if (i > 0)
            expr ~= ", ";
        expr ~= array ~ "[" ~ to!string(i) ~ "]";
    }
    return expr;
}

unittest
{
    enum s = expandArray("arr", 3);
    assert(s == "arr[0], arr[1], arr[2]");
}

