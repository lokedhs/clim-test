#include <fontconfig/fontconfig.h>
#include <stdio.h>
#include <stdlib.h>

void checkError(FcBool result) {
    if(!result) {
        fprintf(stderr, "error from fontconfig call\n");
        abort();
    }
}

int main(void)
{
    FcConfig *config = FcInitLoadConfigAndFonts();

    FcPattern *pattern = FcPatternCreate();

    FcCharSet *charset = FcCharSetCreate();
    checkError(FcCharSetAddChar(charset, 0x63a));
    checkError(FcPatternAddCharSet(pattern, "charset", charset));
    FcCharSetDestroy(charset);

    checkError(FcConfigSubstitute(config, pattern, FcMatchPattern));

    FcDefaultSubstitute(pattern);

    FcResult result;
    FcPattern *resultPattern = FcFontMatch(config, pattern, &result);
    if(result != FcResultMatch) {
        fprintf(stderr, "no fonts matched\n");
        return 1;
    }

    FcValue value;
    FcResult patternResult = FcPatternGet(resultPattern, "file", 0, &value);
    if(patternResult != FcResultMatch) {
        fprintf(stderr, "file value not found\n");
        return 1;
    }

    if(value.type != FcTypeString) {
        fprintf(stderr, "expected to find a string value\n");
        return 1;
    }

    printf("font file = %s\n", value.u.s);

    FcPatternDestroy(pattern);

    return 0;
}
