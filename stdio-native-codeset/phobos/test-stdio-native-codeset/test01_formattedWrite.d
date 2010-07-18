
import std.format;
import std.stdio;

void main()
{
    auto w = LockingNativeTextWriter(stdout);
    formattedWrite(w, "%s\n%s\n%s\n",
        "日本語 かなかな ﾊﾝｶｸｶﾅ"c,
        "מגדל בבל"w,
        "¿ Lâtín çhäràctêrs ?"d);
}

