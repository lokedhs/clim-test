/*
 * Demonstration of Xrender-based text rendering compile with:
 * cc -Wall -g `pkg-config --cflags freetype2 xrender harfbuzz` freetype-lib.c -o freetype-lib `pkg-config --libs freetype2 xrender harfbuzz`
 *
 * harfbuzz example: https://github.com/lxnt/ex-sdl-freetype-harfbuzz/blob/master/ex-sdl-freetype-harfbuzz.c
 */

#include <X11/Xlib.h>
#include <X11/extensions/Xrender.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#include <hb.h>
#include <hb-ft.h>

void
load_glyph(Display * display, GlyphSet gs, FT_Face face, int charcode)
{
    Glyph           gid;
    XGlyphInfo      ginfo;

    int             glyph_index = FT_Get_Char_Index(face, charcode);
    FT_Load_Glyph(face, glyph_index, FT_LOAD_RENDER | FT_LOAD_FORCE_AUTOHINT);

    FT_Bitmap      *bitmap = &face->glyph->bitmap;
    ginfo.x = -face->glyph->bitmap_left;
    ginfo.y = face->glyph->bitmap_top;
    ginfo.width = bitmap->width;
    ginfo.height = bitmap->rows;
    ginfo.xOff = face->glyph->advance.x / 64;
    ginfo.yOff = face->glyph->advance.y / 64;

    gid = charcode;

    int             stride = (ginfo.width + 3) & ~3;
    char            tmpbitmap[stride * ginfo.height];
    int             y;
    for (y = 0; y < ginfo.height; y++)
	memcpy(tmpbitmap + y * stride, bitmap->buffer + y * ginfo.width, ginfo.width);

    XRenderAddGlyphs(display, gs, &gid, &ginfo, 1, tmpbitmap, stride * ginfo.height);
    XSync(display, 0);
}

FT_Library library;

GlyphSet
load_glyphset(Display * display, char *filename, int size)
{
    static int      ft_lib_initialized = 0;
    int             n;
    XRenderPictFormat *fmt_a8 = XRenderFindStandardFormat(display, PictStandardA8);
    GlyphSet        gs = XRenderCreateGlyphSet(display, fmt_a8);

    if (!ft_lib_initialized)
	FT_Init_FreeType(&library);

    FT_Face         face;

    FT_New_Face(library, filename, 0, &face);

    FT_Set_Char_Size(face, 0, size * 64, 90, 90);

    for (n = 32; n < 128; n++)
	load_glyph(display, gs, face, n);

    FT_Done_Face(face);

    return gs;
}

Picture
create_pen(Display * display, int red, int green, int blue, int alpha)
{
    XRenderColor    color;
    color.red = red;
    color.green = green;
    color.blue = blue;
    color.alpha = alpha;
    XRenderPictFormat *fmt = XRenderFindStandardFormat(display, PictStandardARGB32);

    Window          root = DefaultRootWindow(display);
    Pixmap          pm = XCreatePixmap(display, root, 1, 1, 32);
    XRenderPictureAttributes pict_attr;
    pict_attr.repeat = 1;
    Picture         picture = XRenderCreatePicture(display, pm, fmt, CPRepeat, &pict_attr);
    XRenderFillRectangle(display, PictOpOver, picture, &color, 0, 0, 1, 1);
    XFreePixmap(display, pm);
    return picture;
}

static void setup_hb(char *filename)
{
    FT_Face face;

    FT_New_Face(library, filename, 0, &face);

    FT_Set_Char_Size(face, 0, 40 * 64, 90, 90);

    //FT_Done_Face(face);

    hb_font_t *font = hb_ft_font_create(face, NULL);
    hb_buffer_t *buf = hb_buffer_create();
    hb_buffer_set_direction(buf, HB_DIRECTION_LTR);
    hb_buffer_set_script(buf, HB_SCRIPT_LATIN);
    hb_buffer_set_language(buf, hb_language_from_string("en", 2));
    hb_buffer_add_utf8(buf, "Foo", 3, 0, 3);
    hb_shape(font, buf, NULL, 0);

    unsigned int num_glyphs;
    hb_glyph_info_t *glyph_info = hb_buffer_get_glyph_infos(buf, &num_glyphs);
    hb_glyph_position_t *glyph_pos = hb_buffer_get_glyph_positions(buf, &num_glyphs);

    printf("got %d glyphs\n", num_glyphs);

    for(int i = 0 ; i < num_glyphs ; i++) {
        printf("cluster: %d, codepoint: %d\n", glyph_info[i].cluster, glyph_info[i].codepoint);
        printf("  x=%d, y=%d, dx=%d, dy=%d\n",
               glyph_pos[i].x_advance, glyph_pos[i].y_advance, glyph_pos[i].x_offset, glyph_pos[i].y_offset);
    }
}

int
main(int argc, char **argv)
{
    Display        *display = XOpenDisplay(NULL);

    XRenderPictFormat *fmt = XRenderFindStandardFormat(display, PictStandardRGB24);
    int             screen = DefaultScreen(display);
    Window          root = DefaultRootWindow(display);

    Window          window = XCreateWindow(display, root, 0, 0, 640, 480, 0,
					   DefaultDepth(display, screen),
					   InputOutput,
					   DefaultVisual(display, screen),
					   0, NULL);

    char *filename = "/home/elias/.fonts/NotoSans-Regular.ttf";

    XRenderPictureAttributes pict_attr;

    pict_attr.poly_edge = PolyEdgeSmooth;
    pict_attr.poly_mode = PolyModeImprecise;
    Picture         picture = XRenderCreatePicture(display, window, fmt, CPPolyEdge | CPPolyMode,
						   &pict_attr);

    XSelectInput(display, window, KeyPressMask | KeyReleaseMask | ExposureMask | ButtonPressMask | StructureNotifyMask);

    Picture         fg_pen = create_pen(display, 0, 0, 0, 0xffff);

    GlyphSet        font = load_glyphset(display,
					 filename,
					 30);

    XMapWindow(display, window);
    XRenderColor    bg_color = { .red = 0xffff,.green = 0xffff,.blue = 0xffff,.alpha = 0xffff };

    setup_hb(filename);

    while (1) {
	XEvent          event;
	XNextEvent(display, &event);

	switch (event.type) {
	case Expose:
	    XRenderFillRectangle(display, PictOpOver, picture, &bg_color, 0, 0, 1640, 1640);
	    XRenderCompositeString8(display, PictOpOver,
				    fg_pen, picture, 0, font, 0, 0, 20, 50, "We are jumping over a black fox", 31);
	    break;
	case DestroyNotify:
	    return 0;
	}
    }

    return 0;
}
// Local Variables:
// flycheck-clang-include-path: ("/usr/include/harfbuzz" "/usr/include/glib-2.0" "/usr/lib/x86_64-linux-gnu/glib-2.0/include")
// company-clang-arguments: ("-I/usr/include/harfbuzz" "-I/usr/include/glib-2.0" "-I/usr/lib/x86_64-linux-gnu/glib-2.0/include")
// End:
