/*
 * ARIB STD-B24 caption decoder using the libaribcaption library
 * Copyright (c) 2022 TADANO Tokumei
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#define DEBUG 1

#include "avcodec.h"
#include "internal.h"
#include "libavutil/avstring.h"
#include "libavutil/avutil.h"
#include "libavutil/intreadwrite.h"
#include "libavutil/log.h"
#include "libavutil/opt.h"

#include <aribcaption/aribcaption.h>

#define ARIBCAPTION_MGMNT_TIMEOUT 180
#define RGBA(r,g,b,a) (((unsigned)(a) << 24) | ((r) << 16) | ((g) << 8) | (b))

typedef struct AribcaptionContext {
    AVClass *class;
    AVCodecContext *parent;
    AVSubtitle *sub;

    aribcc_context_t *context;
    aribcc_decoder_t *decoder;
    aribcc_renderer_t *renderer;

    int rendering_backend;
    char *font_name;
    bool replace_drcs;
    bool force_stroke_text;
    bool ignore_background;
    bool ignore_ruby;
    bool fadeout;
    float stroke_width;

    int64_t pts;
    int64_t duration;
    int64_t last_mngmnt_pts;
    AVRational time_base;
    int plane_width;
    int plane_height;

    aribcc_render_result_t render_result;
    uint32_t clut[256];
} AribcaptionContext;

static void hex_dump_debug(void *ctx, const char *buf, int buf_size)
{
    int i;

    for (i=0; i < buf_size; i++) {
        ff_dlog(ctx, "%02hhx ", buf[i]);
        if (i % 16 == 15)
            ff_dlog(ctx, "\n");
    }
    if (i % 16)
        ff_dlog(ctx, "\n");
}

static void logcat_callback(aribcc_loglevel_t level, const char* message, void* userdata)
{
    int lvl;

    switch (level) {
    case ARIBCC_LOGLEVEL_ERROR:
        lvl = AV_LOG_ERROR;
        break;
    case ARIBCC_LOGLEVEL_WARNING:
        lvl = AV_LOG_WARNING;
        break;
    default:
        lvl = AV_LOG_INFO;
    }
    av_log(userdata, lvl, "%s\n", message);
}

#ifdef DEBUG
static void png_save_sub(AribcaptionContext *ctx, const char *filename, uint32_t *bitmap, int w, int h)
{
    int x, y, v;
    FILE *f;
    char fname[40], fname2[40];
    char command[1024];

    snprintf(fname, sizeof(fname), "%s.ppm", filename);
    f = fopen(fname, "w");
    if (!f) {
        perror(fname);
        return;
    }
    fprintf(f, "P6\n"
            "%d %d\n"
            "%d\n",
            w, h, 255);
    for(y = 0; y < h; y++) {
        for(x = 0; x < w; x++) {
            v = bitmap[y * w + x];
            putc((v >> 16) & 0xff, f);
            putc((v >> 8) & 0xff, f);
            putc((v >> 0) & 0xff, f);
        }
    }
    fclose(f);

    snprintf(fname2, sizeof(fname2), "%s-a.pgm", filename);
    f = fopen(fname2, "w");
    if (!f) {
        perror(fname2);
        return;
    }
    fprintf(f, "P5\n"
            "%d %d\n"
            "%d\n",
            w, h, 255);
    for(y = 0; y < h; y++) {
        for(x = 0; x < w; x++) {
            v = bitmap[y * w + x];
            putc((v >> 24) & 0xff, f);
        }
    }
    fclose(f);

    snprintf(command, sizeof(command), "pnmtopng -alpha %s %s > %s.png 2> /dev/null", fname2, fname, filename);
    if (system(command) != 0) {
        av_log(ctx, AV_LOG_ERROR, "Error running pnmtopng\n");
        return;
    }

    snprintf(command, sizeof(command), "rm %s %s", fname, fname2);
    if (system(command) != 0) {
        av_log(ctx, AV_LOG_ERROR, "Error removing %s and %s\n", fname, fname2);
        return;
    }
}

static void png_save(AribcaptionContext *ctx)
{
    AVSubtitle *sub = ctx->sub;
    uint32_t *pbuf;
    char filename[32];
    static int fileno_index = 0;

    pbuf = av_mallocz(ctx->plane_width * ctx->plane_height * sizeof(uint32_t));
    if (!pbuf)
        return;

    for (int i = 0; i < ctx->render_result.image_count; i++) {
        AVSubtitleRect *rect = sub->rects[i];

        for (int y = 0; y < rect->h; y++) {
            for (int x = 0; x < rect->w; x++) {
                pbuf[((rect->y + y) * ctx->plane_width) + rect->x + x] =
                    ctx->clut[rect->data[0][y * rect->linesize[0] + x]];
            }
        }
    }

    snprintf(filename, sizeof(filename), "aribcap.%d", fileno_index);
    png_save_sub(ctx, filename, pbuf, ctx->plane_width, ctx->plane_height);

    av_freep(&pbuf);
    fileno_index++;
}
#endif

static int aribcaption_trans_subtitle(AribcaptionContext *ctx)
{
    AVSubtitle *sub = ctx->sub;
    int ret, index;
    uint32_t rgba;

    ctx->clut[0] = RGBA(0,0,0,0);
    index = 1;

    sub->rects = av_calloc(ctx->render_result.image_count, sizeof(*sub->rects));
    if (!sub->rects) {
        ret = AVERROR(ENOMEM);
        goto fail;
    }
    sub->num_rects = ctx->render_result.image_count;
    for (int i = 0; i < sub->num_rects; i++) {
        sub->rects[i] = av_mallocz(sizeof(*sub->rects[i]));
        if (!sub->rects[i]) {
            ret = AVERROR(ENOMEM);
            goto fail;
        }
    }

    for (int i = 0; i < sub->num_rects; i++) {
        AVSubtitleRect *rect = ctx->sub->rects[i];
        aribcc_image_t *image = &ctx->render_result.images[i];
        int w, h, n, idx, r, g, b, a;

        ff_dlog(ctx, "(%d, %d) %d x %d: stride = %d\n",
                image->dst_x, image->dst_y, image->width, image->height, image->stride);

        rect->data[0] = av_mallocz(image->bitmap_size / 4);
        if (!rect->data[0]) {
            ret = AVERROR(ENOMEM);
            goto fail;
        }

        rect->data[1] = av_mallocz(AVPALETTE_SIZE);
        if (!rect->data[1]) {
            ret = AVERROR(ENOMEM);
            goto fail;
        }

        n = 0;
        for (h = 0; h < image->height; w++) {
            for (w = 0; w < image->width; w++) {
                r = image->bitmap[n++];
                g = image->bitmap[n++];
                b = image->bitmap[n++];
                a = image->bitmap[n++];
                rgba = RGBA(r, g, b, a);
                for (idx = 0; idx < index; idx++) {
                    if (ctx->clut[idx] == rgba)
                        break;
                }
                if (idx >= index) {
                    index++;
                    if (index > 256) {
                        av_log(ctx, AV_LOG_ERROR, "CLUT overflow.\n");
                        ret = AVERROR_EXTERNAL;
                        goto fail;
                    }
                    idx++;
                    ctx->clut[idx] = rgba;
                }
                rect->data[0][n / 4] = idx;
            }
            if (w * 4 < image->stride) {
                n += image->stride - w * 4;
            }
        }

        rect->x = image->dst_x;
        rect->y = image->dst_y;
        rect->w = image->width;
        rect->h = image->height;
        rect->type = SUBTITLE_BITMAP;
        rect->linesize[0] = image->stride / 4;
        rect->nb_colors = 256;
    }

    return 0;

fail:
    if (sub->rects) {
        AVSubtitleRect *rect;
        for (int i = 0; i < sub->num_rects; i++) {
            rect = sub->rects[i];
            if (rect) {
                av_freep(&rect->data[0]);
                av_freep(&rect->data[1]);
            }
            av_freep(&sub->rects[i]);
        }
        av_freep(&sub->rects);
    }
    sub->num_rects = 0;

    return ret;
}

static int aribcaption_decode(AVCodecContext *avctx,
                              void *data, int *got_sub_ptr, AVPacket *avpkt)
{
    AribcaptionContext *ctx = avctx->priv_data;
    AVSubtitle *sub = data;
    aribcc_caption_t caption;
    int status;

    ff_dlog(ctx, "ARIB caption packet pts=%"PRIx64":\n", avpkt->pts);
    hex_dump_debug(ctx, avpkt->data, avpkt->size);

    ctx->sub = sub;
    ctx->time_base = avctx->pkt_timebase;
    if (ctx->time_base.num <= 0 || ctx->time_base.den <= 0) {
        av_log(ctx, AV_LOG_VERBOSE, "No timebase set. assuming 90kHz.\n");
        ctx->time_base = av_make_q(1, 90000);
    }
    if (avpkt->pts == AV_NOPTS_VALUE)
        ctx->pts = ARIBCC_PTS_NOPTS;
    else
        ctx->pts = av_rescale_q(avpkt->pts, ctx->time_base, (AVRational){1, 1000});

    status = aribcc_decoder_decode(ctx->decoder, avpkt->data, avpkt->size,
                                   ctx->pts, &caption);
    if (status == ARIBCC_DECODE_STATUS_ERROR) {
        av_log(ctx, AV_LOG_ERROR,
               "aribcc_decoder_decode() returned with error.\n");
        return AVERROR(EAGAIN);
    }
    if (status == ARIBCC_DECODE_STATUS_NO_CAPTION) {
        ff_dlog(ctx, "No caption.\n");
        return avpkt->size;
    }

    ff_dlog(ctx, "type=%02x, flags=%x, lang=%03x, %dx%d, pts=%"PRIx64", duration=%"PRIx64":\n",
            caption.type, caption.flags, caption.iso6392_language_code,
            caption.plane_width, caption.plane_height, caption.pts, caption.wait_duration);
    ff_dlog(ctx, "%s\n", caption.text);


    if (caption.plane_width > 0 && caption.plane_height > 0 &&
        caption.plane_width != ctx->plane_width && caption.plane_height != ctx->plane_height ) {
        if (!aribcc_renderer_set_frame_size(ctx->renderer,
                                            caption.plane_width, caption.plane_height)) {
            av_log(ctx, AV_LOG_ERROR,
                   "aribcc_renderer_set_frame_size() returned with error.\n");
            return AVERROR_EXTERNAL;
        }
        ctx->plane_width = caption.plane_width;
        ctx->plane_height = caption.plane_height;
    }

    ctx->duration = caption.wait_duration;
    status = aribcc_renderer_append_caption(ctx->renderer, &caption);
    aribcc_caption_cleanup(&caption);
    if (!status) {
        av_log(ctx, AV_LOG_ERROR,
               "aribcc_renderer_append_caption() returned with error.\n");
        return AVERROR_EXTERNAL;
    }

    if (sub->num_rects) {
        avpriv_request_sample(ctx, "Different Version of Segment asked Twice");
        return AVERROR_PATCHWELCOME;
    }

    status = aribcc_renderer_render(ctx->renderer, ctx->pts, &ctx->render_result);
    switch (status) {
    case ARIBCC_RENDER_STATUS_GOT_IMAGE:
        break;

    case ARIBCC_RENDER_STATUS_GOT_IMAGE_UNCHANGED:
        aribcc_render_result_cleanup(&ctx->render_result);
        ff_dlog(ctx, "got image unchanged\n");
        return avpkt->size;

    case ARIBCC_RENDER_STATUS_NO_IMAGE:
        ff_dlog(ctx, "no image\n");
        return avpkt->size;

    case ARIBCC_RENDER_STATUS_ERROR:
        av_log(ctx, AV_LOG_ERROR,
               "aribcc_renderer_render() returned with error.\n");
        return AVERROR_EXTERNAL;

    default:
        av_log(ctx, AV_LOG_ERROR,
               "aribcc_renderer_render() returned unknown status: %d\n", status);
        return AVERROR_EXTERNAL;
    }

    if (!ctx->render_result.image_count || ctx->render_result.images == NULL) {
        aribcc_render_result_cleanup(&ctx->render_result);
        ff_dlog(ctx, "no image (%d)\n", ctx->render_result.image_count);
        return avpkt->size;
    }

    *got_sub_ptr = 1;

    if ((status = aribcaption_trans_subtitle(ctx)) < 0) {
        aribcc_render_result_cleanup(&ctx->render_result);
        return status;
    }

    if (avpkt->pts != AV_NOPTS_VALUE)
        sub->pts = av_rescale_q(avpkt->pts,
                                ctx->time_base, AV_TIME_BASE_Q);
    if (ctx->duration == ARIBCC_DURATION_INDEFINITE)
        sub->end_display_time = ARIBCAPTION_MGMNT_TIMEOUT * 1000;
    else
        sub->end_display_time = (uint32_t)ctx->duration;

    ff_dlog(ctx, "got image count = %d, start=%d.%d, duration=%d.%d\n",
            ctx->render_result.image_count,
            (int)(ctx->pts / 1000), (int)(ctx->pts % 1000),
            (int)(sub->end_display_time / 1000), (int)(sub->end_display_time % 1000));

#ifdef DEBUG
    png_save(ctx);
#endif
    aribcc_render_result_cleanup(&ctx->render_result);
    return avpkt->size;
}

static void aribcaption_flush(AVCodecContext *avctx)
{
    AribcaptionContext *ctx = avctx->priv_data;
    if (!(avctx->flags2 & AV_CODEC_FLAG2_RO_FLUSH_NOOP)) {
        aribcc_decoder_flush(ctx->decoder);
        aribcc_renderer_flush(ctx->renderer);
    }
}

static int aribcaption_close(AVCodecContext *avctx)
{
    AribcaptionContext *ctx = avctx->priv_data;

    if (ctx->renderer)
        aribcc_renderer_free(ctx->renderer);
    if (ctx->decoder)
        aribcc_decoder_free(ctx->decoder);
    if (ctx->context)
        aribcc_context_free(ctx->context);

    return 0;
}

static int aribcaption_init(AVCodecContext *avctx)
{
    AribcaptionContext *ctx = avctx->priv_data;
    aribcc_profile_t profile;
    aribcc_encoding_scheme_t encode_scheme;

    switch (avctx->profile) {
    case FF_PROFILE_ARIB_PROFILE_A:
        profile = ARIBCC_PROFILE_A;
        /* assume 960x540 at initial state */
        ctx->plane_width = 960;
        ctx->plane_height = 540;
        break;
    case FF_PROFILE_ARIB_PROFILE_C:
        profile = ARIBCC_PROFILE_C;
        ctx->plane_width = 320;
        ctx->plane_height = 180;
        break;
    default:
        av_log(avctx, AV_LOG_ERROR, "Unknown or unsupported profile set.\n");
        return AVERROR(EINVAL);
    }
    encode_scheme = ARIBCC_ENCODING_SCHEME_AUTO;
    if (avctx->sub_charenc_mode == FF_SUB_CHARENC_MODE_DO_NOTHING)
        encode_scheme = ARIBCC_ENCODING_SCHEME_ARIB_STD_B24_UTF8;

    if (!(ctx->context = aribcc_context_alloc())) {
        av_log(avctx, AV_LOG_ERROR, "Failed to alloc libaribcaption context.\n");
        aribcaption_close(avctx);
        return AVERROR_EXTERNAL;
    }
    aribcc_context_set_logcat_callback(ctx->context, logcat_callback, avctx);
    if (!(ctx->decoder = aribcc_decoder_alloc(ctx->context))) {
        av_log(avctx, AV_LOG_ERROR, "Failed to alloc libaribcaption decoder.\n");
        aribcaption_close(avctx);
        return AVERROR_EXTERNAL;
    }
    if (!aribcc_decoder_initialize(ctx->decoder,
                                   encode_scheme,
                                   ARIBCC_CAPTIONTYPE_CAPTION,
                                   profile,
                                   ARIBCC_LANGUAGEID_FIRST)) {
        av_log(avctx, AV_LOG_ERROR, "Failed to initialize libaribcaption decoder.\n");
        aribcaption_close(avctx);
        return AVERROR_EXTERNAL;
    }
    if(!(ctx->renderer = aribcc_renderer_alloc(ctx->context))) {
        av_log(avctx, AV_LOG_ERROR, "Failed to alloc libaribcaption renderer.\n");
        aribcaption_close(avctx);
        return AVERROR_EXTERNAL;
    }
    aribcc_renderer_initialize(ctx->renderer,
                               ARIBCC_CAPTIONTYPE_CAPTION,
                               ARIBCC_FONTPROVIDER_TYPE_AUTO,
                               ARIBCC_TEXTRENDERER_TYPE_AUTO);
    if(!aribcc_renderer_initialize(ctx->renderer,
                                   ARIBCC_CAPTIONTYPE_CAPTION,
                                   ARIBCC_FONTPROVIDER_TYPE_AUTO,
                                   (aribcc_textrenderer_type_t)ctx->rendering_backend)) {
        av_log(avctx, AV_LOG_ERROR, "Failed to initialize libaribcaption renderer.\n");
        aribcaption_close(avctx);
        return AVERROR_EXTERNAL;
    }
    if (!aribcc_renderer_set_frame_size(ctx->renderer,
                                        ctx->plane_width, ctx->plane_height)) {
        av_log(ctx, AV_LOG_ERROR,
               "aribcc_renderer_set_frame_size() returned with error.\n");
        return AVERROR_EXTERNAL;
    }

    aribcc_renderer_set_storage_policy(ctx->renderer, ARIBCC_CAPTION_STORAGE_POLICY_MINIMUM, 0);
    aribcc_renderer_set_replace_drcs(ctx->renderer, ctx->replace_drcs);
    aribcc_renderer_set_force_stroke_text(ctx->renderer, ctx->force_stroke_text);
    aribcc_renderer_set_force_no_background(ctx->renderer, ctx->ignore_background);
    aribcc_renderer_set_force_no_ruby(ctx->renderer, ctx->ignore_ruby);
    aribcc_renderer_set_stroke_width(ctx->renderer, ctx->stroke_width);
    if (ctx->font_name && strlen(ctx->font_name) > 0) {
        const char* font_families[] = { ctx->font_name };
        aribcc_renderer_set_default_font_family(ctx->renderer, font_families, 1, true);
    }

    ctx->parent = avctx;
    ctx->last_mngmnt_pts = AV_NOPTS_VALUE;

    return 0;
}

#ifdef __APPLE__
# define DEFAULT_FAMILY "Hiragino Maru Gothic ProN"
#elif defined(_WIN32)
# define DEFAULT_FAMILY "MS Gothic"
#else
# define DEFAULT_FAMILY "sans-serif"
#endif

#define OFFSET(x) offsetof(AribcaptionContext, x)
#define SD AV_OPT_FLAG_SUBTITLE_PARAM | AV_OPT_FLAG_DECODING_PARAM
static const AVOption options[] = {
    { "aribcaption-rendering-backend", "text rendering backend",
      OFFSET(rendering_backend), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, 5, SD },
    { "aribcaption-font", "font name",
      OFFSET(font_name), AV_OPT_TYPE_STRING, { .str = DEFAULT_FAMILY }, 0, 0, SD },
    { "aribcaption-replace-drcs", "replace known DRCS",
      OFFSET(replace_drcs), AV_OPT_TYPE_BOOL, { .i64 = 1 }, 0, 1, SD },
    { "aribcaption-force-stroke-text", "always render characters with stroke",
      OFFSET(force_stroke_text), AV_OPT_TYPE_BOOL, { .i64 = 0 }, 0, 1, SD },
    { "aribcaption-ignore-background", "ignore rendering caption background",
      OFFSET(ignore_background), AV_OPT_TYPE_BOOL, { .i64 = 0 }, 0, 1, SD },
    { "aribcaption-ignore-ruby", "ignore ruby-like characters (furigana)",
      OFFSET(ignore_ruby), AV_OPT_TYPE_BOOL, { .i64 = 0 }, 0, 1, SD },
    { "aribcaption-fadeout", "enable Fadeout",
      OFFSET(fadeout), AV_OPT_TYPE_BOOL, { .i64 = 0 }, 0, 1, SD },
    { "aribcaption-stroke-width", "stroke width for stroke text",
      OFFSET(stroke_width), AV_OPT_TYPE_FLOAT, { .dbl = 1.5 }, 0.0, 3.0, SD },
    { NULL }
};

static const AVClass aribcaption_class = {
    .class_name = "aribcaption decoder",
    .item_name  = av_default_item_name,
    .option     = options,
    .version    = LIBAVUTIL_VERSION_INT,
};

const AVCodec ff_aribcaption_decoder = {
    .name      = "aribcaption",
    .long_name = NULL_IF_CONFIG_SMALL("ARIB STD-B24 caption decoder"),
    .type      = AVMEDIA_TYPE_SUBTITLE,
    .id        = AV_CODEC_ID_ARIB_CAPTION,
    .priv_data_size = sizeof(AribcaptionContext),
    .init      = aribcaption_init,
    .close     = aribcaption_close,
    .decode    = aribcaption_decode,
    .flush     = aribcaption_flush,
    .priv_class= &aribcaption_class,
//    .caps_internal = FF_CODEC_CAP_INIT_THREADSAFE,
};

typedef struct AribcaptionParseContext {
    int dummy;
} AribcaptionParseContext;

static int aribcaption_parse(AVCodecParserContext *s,
                            AVCodecContext *avctx,
                            const uint8_t **poutbuf, int *poutbuf_size,
                            const uint8_t *buf, int buf_size)
{
    ff_dlog(avctx, "ARIB caption parse pts=%"PRIx64", lpts=%"PRIx64", cpts=%"PRIx64":\n",
            s->pts, s->last_pts, s->cur_frame_pts[s->cur_frame_start_index]);

    /* Just pass the data to decoder */
    *poutbuf      = buf;
    *poutbuf_size = buf_size;

    return buf_size;
}

const AVCodecParser ff_aribcaption_parser = {
    .codec_ids      = { AV_CODEC_ID_ARIB_CAPTION },
    .priv_data_size = sizeof(AribcaptionParseContext),
    .parser_parse   = aribcaption_parse,
};
