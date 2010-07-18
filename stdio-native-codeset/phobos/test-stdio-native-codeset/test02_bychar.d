
import std.stdio;

void main()
{
    auto r = LockingNativeTextReader(stdin);

    for (dchar c; r.getNext(c); )
        printf("-> U+%06x\n", cast(int) c);
}

