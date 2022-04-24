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

#include "avcodec.h"
#include "internal.h"
#include "libavcodec/ass.h"
#include "libavutil/avstring.h"
#include "libavutil/avutil.h"
#include "libavutil/thread.h"
#include "libavutil/log.h"
#include "libavutil/opt.h"

#include <aribcaption/aribcaption.h>

#define ARIBCAPTION_BPRINT_SIZE_INIT   256
#define ARIBCAPTION_BPRINT_SIZE_MAX    (8 * 1024)

#define ARIBCC_COLOR_RGB(c)            ((c) & 0xFFFFFF)
#define ARIBCC_COLOR_DIFF_RGB(c1,c2)   (((c1) ^ (c2)) & 0x00FFFFFF)
#define ARIBCC_COLOR_DIFF_A(c1,c2)     (((c1) ^ (c2)) & 0xFF000000)

#define CLUT_RGBA(r,g,b,a) (((unsigned)(a) << 24) | ((r) << 16) | ((g) << 8) | (b))

typedef struct ARIBCaptionContext {
    AVClass *class;
    AVCodecContext *parent;
    AVPacket *avpkt;
    AVSubtitle *sub;

    aribcc_context_t *context;
    aribcc_decoder_t *decoder;
#ifdef ARIBCC_RENDER
    aribcc_renderer_t *renderer;
#endif

    enum AVSubtitleType subtitle_type;
    char *font_name;
    bool replace_drcs;
    bool force_stroke_text;
    bool ignore_background;
    bool ignore_ruby;
    bool fadeout;
    float stroke_width;
#ifdef ARIBCC_RENDER
    aribcc_textrenderer_type_t rendering_backend;
#endif

    int64_t pts;
    AVRational time_base;
    int plane_width;
    int plane_height;
    int font_size;
    int charstyle;
    int border_style;
    int readorder;

    aribcc_caption_t caption;
#ifdef ARIBCC_RENDER
    aribcc_render_result_t render_result;
    uint32_t *clut;
#endif
} ARIBCaptionContext;

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
    ARIBCaptionContext *ctx = userdata;
    int lvl;

    if (ctx->decoder != NULL) {
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

        av_log(ctx, lvl, "%s\n", message);
    }
}

static int set_ass_header(ARIBCaptionContext *ctx)
{
    AVCodecContext *avctx = ctx->parent;
    int outline, shadow;

    if (ctx->border_style == 4)
        shadow = 4;
    else
        shadow = 0;
    if (ctx->charstyle & ARIBCC_CHARSTYLE_STROKE)
        outline = 1;
    else
        outline = 0;
    if (ctx->force_stroke_text)
        outline = (int)(ctx->stroke_width * 4.0 / 3.0);

    if (avctx->subtitle_header)
        av_freep(&avctx->subtitle_header);
    avctx->subtitle_header = av_asprintf(
            "[Script Info]\r\n"
            "ScriptType: v4.00+\r\n"
            "PlayResX: %d\r\n"
            "PlayResY: %d\r\n"
            "WrapStyle: 2\r\n"      /* 2: no word wrapping */
            "\r\n"

            "[V4+ Styles]\r\n"
             "Format: Name, "
             "Fontname, Fontsize, "
             "PrimaryColour, SecondaryColour, OutlineColour, BackColour, "
             "Bold, Italic, Underline, StrikeOut, "
             "ScaleX, ScaleY, "
             "Spacing, Angle, "
             "BorderStyle, Outline, Shadow, "
             "Alignment, MarginL, MarginR, MarginV, "
             "Encoding\r\n"

             "Style: "
             "Default,"             /* Name */
             "%s,%d,"               /* Font{name,size} */
             "&H%x,&H%x,&H%x,&H%x," /* {Primary,Secondary,Outline,Back}Colour */
             "%d,%d,%d,0,"          /* Bold, Italic, Underline, StrikeOut */
             "100,100,"             /* Scale{X,Y} */
             "0,0,"                 /* Spacing, Angle */
             "%d,%d,%d,"            /* BorderStyle, Outline, Shadow */
             "%d,10,10,10,"         /* Alignment, Margin[LRV] */
             "0\r\n"                /* Encoding */
             "\r\n"

             "[Events]\r\n"
             "Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\r\n",
            ctx->plane_width, ctx->plane_height,
            ctx->font_name, ctx->font_size,
            ASS_DEFAULT_COLOR, ASS_DEFAULT_COLOR,
            ASS_DEFAULT_BACK_COLOR, ASS_DEFAULT_BACK_COLOR,
            -ASS_DEFAULT_BOLD, -ASS_DEFAULT_ITALIC, -ASS_DEFAULT_UNDERLINE,
            ctx->border_style, outline, shadow, ASS_DEFAULT_ALIGNMENT);

    if (!avctx->subtitle_header)
        return AVERROR(ENOMEM);
    avctx->subtitle_header_size = strlen(avctx->subtitle_header);
    return 0;
}

static int aribcaption_trans_bitmap_subtitle(ARIBCaptionContext *ctx)
{
    int ret = 0;
#ifdef ARIBCC_RENDER
    AVSubtitle *sub = ctx->sub;
    int status, rect_idx, clut_idx;
    uint32_t rgba;

    status = aribcc_renderer_append_caption(ctx->renderer, &ctx->caption);
    if (!status) {
        av_log(ctx, AV_LOG_ERROR,
               "aribcc_renderer_append_caption() returned with error.\n");
        return AVERROR_EXTERNAL;
    }

    if (ctx->caption.plane_width > 0 && ctx->caption.plane_height > 0 &&
        (ctx->caption.plane_width != ctx->plane_width ||
         ctx->caption.plane_height != ctx->plane_height)) {
        ctx->plane_width = ctx->caption.plane_width;
        ctx->plane_height = ctx->caption.plane_height;
        if (!aribcc_renderer_set_frame_size(ctx->renderer,
                                            ctx->plane_width, ctx->plane_height)) {
            av_log(ctx, AV_LOG_ERROR,
                   "aribcc_renderer_set_frame_size() returned with error.\n");
            return AVERROR_EXTERNAL;
        }
    }

    status = aribcc_renderer_render(ctx->renderer, ctx->pts, &ctx->render_result);
    switch (status) {
    case ARIBCC_RENDER_STATUS_GOT_IMAGE:
        break;

    case ARIBCC_RENDER_STATUS_GOT_IMAGE_UNCHANGED:
        aribcc_render_result_cleanup(&ctx->render_result);
        ff_dlog(ctx, "got image unchanged\n");
        return 0;

    case ARIBCC_RENDER_STATUS_NO_IMAGE:
        ff_dlog(ctx, "no image\n");
        return 0;

    case ARIBCC_RENDER_STATUS_ERROR:
        av_log(ctx, AV_LOG_ERROR,
               "aribcc_renderer_render() returned with error.\n");
        return AVERROR_EXTERNAL;

    default:
        aribcc_render_result_cleanup(&ctx->render_result);
        av_log(ctx, AV_LOG_ERROR,
               "aribcc_renderer_render() returned unknown status: %d\n", status);
        return AVERROR_EXTERNAL;
    }

    if (!ctx->render_result.image_count || ctx->render_result.images == NULL) {
        aribcc_render_result_cleanup(&ctx->render_result);
        ff_dlog(ctx, "no image (%d)\n", ctx->render_result.image_count);
        return 0;
    }

    ctx->clut[0] = CLUT_RGBA(0,0,0,0);
    clut_idx = 1;

    sub->format = 0; /* graphic */
    sub->rects = av_calloc(ctx->render_result.image_count, sizeof(*sub->rects));
    if (!sub->rects) {
        ret = AVERROR(ENOMEM);
        goto fail;
    }
    for (int i = 0; i < ctx->render_result.image_count; i++) {
        sub->rects[i] = av_mallocz(sizeof(*sub->rects[i]));
        if (!sub->rects[i]) {
            ret = AVERROR(ENOMEM);
            goto fail;
        }
    }

    for (rect_idx = 0; rect_idx < ctx->caption.region_count; rect_idx++) {
        AVSubtitleRect *rect = ctx->sub->rects[rect_idx];
        aribcc_image_t *image = &ctx->render_result.images[rect_idx];
        int w, h, n, idx, r, g, b, a;

        ff_dlog(ctx, "(%d, %d) %d x %d: stride = %d\n",
                image->dst_x, image->dst_y, image->width, image->height, image->stride);

        rect->data[0] = av_mallocz(image->bitmap_size / 4);
        if (!rect->data[0]) {
            ret = AVERROR(ENOMEM);
            goto fail;
        }

        n = 0;
        for (h = 0; h < image->height; h++) {
            for (w = 0; w < image->width; w++) {
                r = image->bitmap[n++];
                g = image->bitmap[n++];
                b = image->bitmap[n++];
                a = image->bitmap[n++];
                if (n > image->bitmap_size) {
                    av_log(ctx, AV_LOG_ERROR, "Bug: unexpectedly reach end of image\n");
                    ret = AVERROR_EXTERNAL;
                    goto fail;
                }
                rgba = CLUT_RGBA(r, g, b, a);
                for (idx = 0; idx < clut_idx; idx++) {
                    if (ctx->clut[idx] == rgba)
                        break;
                }
                if (idx >= clut_idx) {
                    clut_idx++;
                    if (clut_idx > 256) {
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
        rect->data[1] = av_memdup(ctx->clut, AVPALETTE_SIZE);
        if (!rect->data[1]) {
            ret = AVERROR(ENOMEM);
            goto fail;
        }

        rect->x = image->dst_x;
        rect->y = image->dst_y;
        rect->w = image->width;
        rect->h = image->height;
        rect->type = SUBTITLE_BITMAP;
        rect->linesize[0] = image->stride / 4;
        rect->nb_colors = 256;
    }
    sub->num_rects = rect_idx;

    return rect_idx;

fail:
    if (sub->rects) {
        for (int i = 0; i < ctx->caption.region_count; i++) {
            if (sub->rects[i]) {
                if (sub->rects[i]->data[0])
                    av_freep(&sub->rects[i]->data[0]);
                if (sub->rects[i]->data[1])
                    av_freep(&sub->rects[i]->data[1]);
                av_freep(&sub->rects[i]);
            }
        }
        av_freep(&sub->rects);
    }
    sub->num_rects = 0;
#endif

    return ret;
}

static void set_ass_color(AVBPrint *buf, int color_num,
                          aribcc_color_t new_color, aribcc_color_t old_color)
{
    if (ARIBCC_COLOR_DIFF_RGB(new_color, old_color))
        av_bprintf(buf, "{\\%dc&H%06x&}", color_num,
                                          ARIBCC_COLOR_RGB(new_color));
    if (ARIBCC_COLOR_DIFF_A(new_color, old_color) && ARIBCC_COLOR_A(new_color))
        av_bprintf(buf, "{\\%da&H%02x&}", color_num,
                                          ARIBCC_COLOR_A(new_color));
}

static int aribcaption_trans_ass_subtitle(ARIBCaptionContext *ctx)
{
    AVSubtitle *sub = ctx->sub;
    AVBPrint buf;
    int ret = 0, rect_idx;

    if (ctx->caption.plane_width > 0 && ctx->caption.plane_height > 0 &&
        (ctx->caption.plane_width != ctx->plane_width ||
         ctx->caption.plane_height != ctx->plane_height)) {
        ctx->plane_width = ctx->caption.plane_width;
        ctx->plane_height = ctx->caption.plane_height;
        if ((ret = set_ass_header(ctx)) != 0)
            goto fail;
    }

    av_bprint_init(&buf, ARIBCAPTION_BPRINT_SIZE_INIT, ARIBCAPTION_BPRINT_SIZE_MAX);

    sub->format = 0; /* graphic */
    sub->rects = av_calloc(ctx->caption.region_count, sizeof(*sub->rects));
    if (!sub->rects) {
        ret = AVERROR(ENOMEM);
        goto fail;
    }
    if (ctx->caption.region_count == 0) {
        sub->rects[0] = av_mallocz(sizeof(*sub->rects[0]));
        if (!sub->rects[0]) {
            ret = AVERROR(ENOMEM);
            goto fail;
        }
        /* clear previous caption for indefinite duration  */
        ff_ass_add_rect(sub, "{\\r}", ctx->readorder++, 0, NULL, NULL);
        sub->rects[0]->type = SUBTITLE_ASS;
        return 1;
    }
    for (int i = 0; i < ctx->caption.region_count; i++) {
        sub->rects[i] = av_mallocz(sizeof(*sub->rects[i]));
        if (!sub->rects[i]) {
            ret = AVERROR(ENOMEM);
            goto fail;
        }
    }

    rect_idx = 0;
    for (int i = 0; i < ctx->caption.region_count; i++) {
        AVSubtitleRect *rect = ctx->sub->rects[rect_idx];
        aribcc_caption_region_t *region = &ctx->caption.regions[i];
        aribcc_color_t text_color = ARIBCC_MAKE_RGBA(0xFF, 0xFF, 0xFF, 0xFF);
        aribcc_color_t stroke_color = ARIBCC_MAKE_RGBA(0, 0, 0, 0xFF);
        aribcc_color_t back_color = ARIBCC_MAKE_RGBA(0, 0, 0, 0x80);
        aribcc_charstyle_t charstyle = ctx->charstyle;
        int char_width = ctx->font_size;
        int char_height = ctx->font_size;

        if (region->is_ruby && ctx->ignore_ruby)
            continue;

        av_bprint_clear(&buf);
        if (region->x != 0 || region->y != 0)
            av_bprintf(&buf, "{\\an1\\pos(%d,%d)}", region->x, region->y);
        if (region->is_ruby)
            av_bprintf(&buf, "{\\fs%d}", char_height / 2);
        if (ctx->fadeout)
            av_bprintf(&buf, "{\\fad(0,1)}");

        for (int j = 0; j < region->char_count; j++) {
            aribcc_caption_char_t *ch = &region->chars[j];

            if (ch->back_color != back_color) {
                if (ctx->border_style == 4)
                    set_ass_color(&buf, 4, ch->back_color, back_color);
                else
                    set_ass_color(&buf, 3, ch->back_color, back_color);
                back_color = ch->back_color;
            }
            if (ch->stroke_color != stroke_color) {
                set_ass_color(&buf, 3, ch->stroke_color, stroke_color);
                stroke_color = ch->stroke_color;
            }
            if (ch->text_color != text_color) {
                set_ass_color(&buf, 1, ch->text_color, text_color);
                text_color = ch->text_color;
            }
            if (ch->char_width != char_width) {
                av_bprintf(&buf, "{\\fscx%"PRId64"}",
                           av_rescale(ch->char_width, 100, ctx->font_size));
                char_width = ch->char_width;
            }
            if (ch->char_height != char_height) {
                av_bprintf(&buf, "{\\fscy%"PRId64"}",
                           av_rescale(ch->char_height, 100, ctx->font_size));
                char_height = ch->char_height;
            }
            if (ch->style != charstyle) {
                aribcc_charstyle_t diff = ch->style ^ charstyle;
                if (diff & ARIBCC_CHARSTYLE_BOLD) {
                    if (charstyle & ARIBCC_CHARSTYLE_BOLD)
                        av_bprintf(&buf, "{\\b0}");
                    else
                        av_bprintf(&buf, "{\\b1}");
                }
                if (diff & ARIBCC_CHARSTYLE_ITALIC) {
                    if (charstyle & ARIBCC_CHARSTYLE_ITALIC)
                        av_bprintf(&buf, "{\\i0}");
                    else
                        av_bprintf(&buf, "{\\i1}");
                }
                if (diff & ARIBCC_CHARSTYLE_UNDERLINE) {
                    if (charstyle & ARIBCC_CHARSTYLE_UNDERLINE)
                        av_bprintf(&buf, "{\\u0}");
                    else
                        av_bprintf(&buf, "{\\u1}");
                }
#if 0
                if (diff & ARIBCC_CHARSTYLE_STROKE) {
                    /* there is no override codes for outline, thus use alpha channel instead */
                    if (charstyle & ARIBCC_CHARSTYLE_STROKE)
                        av_bprintf(&buf, "{\\3a}");
                    else
                        av_bprintf(&buf, "{\\3a&Hff&}");
                }
#endif
                charstyle = ch->style;
            }
            if (region->chars[j].type == ARIBCC_CHARTYPE_DRCS)
                av_bprintf(&buf, "〓");
            else
                ff_ass_bprint_text_event(&buf, ch->u8str, strlen(ch->u8str), "", 0);
        }
        av_bprintf(&buf, "{\\r%s}",
                         (i + 1 < ctx->caption.region_count) ? "\\N" : "");
        if (!av_bprint_is_complete(&buf)) {
            ret = AVERROR(ENOMEM);
            goto fail;
        }

        ff_dlog(ctx, "ASS subtitle%s (%d,%d) %dx%d [%d]: %s\n",
                (region->is_ruby) ? " (ruby)" : "",
                region->x, region->y, region->width, region->height,
                rect_idx, buf.str);

        ret = ff_ass_add_rect(sub, buf.str, ctx->readorder++, 0 , NULL, NULL);
        if (ret != 0)
            goto fail;

        rect->type = SUBTITLE_ASS;
        rect_idx++;
    }

    av_bprint_finalize(&buf, NULL);
    return rect_idx;

fail:
    if (sub->rects) {
        for (int i = 0; i < ctx->caption.region_count; i++) {
            if (sub->rects[i]) {
                if (sub->rects[i]->ass)
                    av_freep(&sub->rects[i]->ass);
                av_freep(&sub->rects[i]);
            }
        }
        av_freep(&sub->rects);
    }
    sub->num_rects = 0;
    av_bprint_finalize(&buf, NULL);

    return ret;
}

static int aribcaption_trans_text_subtitle(ARIBCaptionContext *ctx)
{
    AVSubtitle *sub = ctx->sub;
    AVSubtitleRect *rect;
    int ret = 0;
    const char *text;

    sub->rects = av_calloc(ctx->caption.region_count, sizeof(*sub->rects));
    if (!sub->rects) {
        ret = AVERROR(ENOMEM);
        goto fail;
    }
    sub->num_rects = 1;

    sub->rects[0] = av_mallocz(sizeof(*sub->rects[0]));
    if (!sub->rects[0]) {
        ret = AVERROR(ENOMEM);
        goto fail;
    }
    rect = sub->rects[0];

    if (ctx->caption.region_count > 0) {
        text = ctx->caption.text;
        ff_dlog(ctx, "text subtitle: %s\n", text);
    } else
        text = ""; /* clear previous caption */
    rect->text = av_strdup(text);
    if (!rect->text) {
        ret = AVERROR(ENOMEM);
        goto fail;
    }

    sub->format = 1; /* text */
    rect->type = SUBTITLE_TEXT;

    return 1;

fail:
    if (sub->rects) {
        rect = sub->rects[0];
        if (rect) {
            if (rect->text)
                av_freep(&rect->text);
            av_freep(&rect);
        }
        av_freep(&sub->rects);
    }
    sub->num_rects = 0;

    return ret;
}

static int aribcaption_decode(AVCodecContext *avctx,
                              void *data, int *got_sub_ptr, AVPacket *avpkt)
{
    ARIBCaptionContext *ctx = avctx->priv_data;
    AVSubtitle *sub = data;
    int status;

    ff_dlog(ctx, "ARIB caption packet pts=%"PRIx64":\n", avpkt->pts);
    if (sub->num_rects) {
        avpriv_request_sample(ctx, "Different Version of Segment asked Twice");
//        return AVERROR_PATCHWELCOME;
        return AVERROR(EAGAIN);
    }
    hex_dump_debug(ctx, avpkt->data, avpkt->size);

    ctx->sub = sub;
    ctx->avpkt = avpkt;
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
                                   ctx->pts, &ctx->caption);
    if (status == ARIBCC_DECODE_STATUS_ERROR) {
        av_log(ctx, AV_LOG_ERROR,
               "aribcc_decoder_decode() returned with error.\n");
        return AVERROR(EAGAIN);
    }
    if (status == ARIBCC_DECODE_STATUS_NO_CAPTION) {
        ff_dlog(ctx, "No caption.\n");
        return avpkt->size;
    } else {
        ff_dlog(ctx, "type=%02x, flags=%x, lang=%03x\n",
                ctx->caption.type, ctx->caption.flags, ctx->caption.iso6392_language_code);
        ff_dlog(ctx, "region count = %d, start=%d.%d, duration=%d.%d\n",
                ctx->caption.region_count,
                (int)(ctx->caption.pts / 1000), (int)(ctx->caption.pts % 1000),
                (int)((ctx->caption.wait_duration == ARIBCC_DURATION_INDEFINITE) ?
                      -1 : ctx->caption.wait_duration / 1000),
                (int)((ctx->caption.wait_duration == ARIBCC_DURATION_INDEFINITE) ?
                      0 : ctx->caption.wait_duration % 1000));
    }

    switch (ctx->subtitle_type) {
    case SUBTITLE_TEXT:
        status = aribcaption_trans_text_subtitle(ctx);
        break;

    case SUBTITLE_ASS:
        status = aribcaption_trans_ass_subtitle(ctx);
        break;

    case SUBTITLE_BITMAP:
        status = aribcaption_trans_bitmap_subtitle(ctx);
        break;

    case SUBTITLE_NONE:
    default:
        status = 0;
    }

    if (status < 0) {
        av_log(ctx, AV_LOG_ERROR, "Failed to set Subtitle: %s\n",
               av_err2str(status));
        aribcc_caption_cleanup(&ctx->caption);
        return status;
    }
    if (status > 0) {
        *got_sub_ptr = 1;
        if (ctx->avpkt->pts != AV_NOPTS_VALUE)
            sub->pts = av_rescale_q(ctx->avpkt->pts,
                                    ctx->time_base, AV_TIME_BASE_Q);
        if (ctx->caption.wait_duration == ARIBCC_DURATION_INDEFINITE)
            sub->end_display_time = UINT32_MAX;
        else
            sub->end_display_time = (uint32_t)ctx->caption.wait_duration;
    }

    aribcc_caption_cleanup(&ctx->caption);
    return avpkt->size;
}

static void aribcaption_flush(AVCodecContext *avctx)
{
    ARIBCaptionContext *ctx = avctx->priv_data;

    if (!(avctx->flags2 & AV_CODEC_FLAG2_RO_FLUSH_NOOP)) {
        if (ctx->decoder)
            aribcc_decoder_flush(ctx->decoder);
#ifdef ARIBCC_RENDER
        if (ctx->renderer)
            aribcc_renderer_flush(ctx->renderer);
#endif
        ctx->readorder = 0;
    }
}

static int aribcaption_close(AVCodecContext *avctx)
{
    ARIBCaptionContext *ctx = avctx->priv_data;

    aribcaption_flush(avctx);

    if (avctx->subtitle_header)
        av_freep(&avctx->subtitle_header);
#ifdef ARIBCC_RENDER
    if (ctx->clut)
        av_freep(&ctx->clut);
    if (ctx->renderer) {
        aribcc_renderer_free(ctx->renderer);
        ctx->decoder = NULL;
    }
#endif
    if (ctx->decoder) {
        aribcc_decoder_free(ctx->decoder);
        ctx->decoder = NULL;
    }
    if (ctx->context) {
        aribcc_context_free(ctx->context);
        ctx->context = NULL;
     }

    return 0;
}

static int aribcaption_init(AVCodecContext *avctx)
{
    ARIBCaptionContext *ctx = avctx->priv_data;
    aribcc_profile_t profile;
    aribcc_encoding_scheme_t encode_scheme;
    int ret = 0;

    ctx->parent = avctx;

    switch (avctx->profile) {
    case FF_PROFILE_ARIB_PROFILE_A:
        profile = ARIBCC_PROFILE_A;
        /* assume 960x540 at initial state */
        ctx->plane_width = 960;
        ctx->plane_height = 540;
        ctx->font_size = 36;
        break;
    case FF_PROFILE_ARIB_PROFILE_C:
        profile = ARIBCC_PROFILE_C;
        ctx->plane_width = 320;
        ctx->plane_height = 180;
        ctx->font_size = 16;
        break;
    default:
        av_log(avctx, AV_LOG_ERROR, "Unknown or unsupported profile set.\n");
        return AVERROR(EINVAL);
    }
    /* determine BorderStyle of ASS header */
    if (ctx->ignore_background)
        ctx->border_style = 1;
    else
        ctx->border_style = 4;
    ctx->charstyle = ARIBCC_CHARSTYLE_DEFAULT;
    if (ctx->force_stroke_text || ctx->ignore_background)
        ctx->charstyle |= ARIBCC_CHARSTYLE_STROKE;

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

    switch (ctx->subtitle_type) {
    case SUBTITLE_ASS:
        ret = set_ass_header(ctx);
        if (ret != 0) {
            av_log(avctx, AV_LOG_ERROR, "Failed to set ASS header: %s\n",
                                        av_err2str(ret));
            return ret;
        }
        break;

    case SUBTITLE_BITMAP:
#ifdef ARIBCC_RENDER
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
        if (!(ctx->clut = av_mallocz(AVPALETTE_SIZE)))
            return AVERROR(ENOMEM);

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
#else
        av_log(avctx, AV_LOG_ERROR, "bitmap renderer is not yet supported.\n");
        return AVERROR(EINVAL);
#endif
        break;

    case SUBTITLE_TEXT:
    case SUBTITLE_NONE:
    default:
        /* do nothing */ ;
    }

    ctx->readorder = 0;

    return 0;
}

#if defined(_WIN32)
# define DEFAULT_FAMILY ASS_DEFAULT_FONT
#else
# define DEFAULT_FAMILY "sans-serif"
#endif
#define OFFSET(x) offsetof(ARIBCaptionContext, x)
#define SD AV_OPT_FLAG_SUBTITLE_PARAM | AV_OPT_FLAG_DECODING_PARAM
static const AVOption options[] = {
    { "font", "font name",
      OFFSET(font_name), AV_OPT_TYPE_STRING, { .str = DEFAULT_FAMILY }, 0, 0, SD },
    { "force_outline_text", "always render characters with outline",
      OFFSET(force_stroke_text), AV_OPT_TYPE_BOOL, { .i64 = 0 }, 0, 1, SD },
    { "ignore_background", "ignore rendering caption background",
      OFFSET(ignore_background), AV_OPT_TYPE_BOOL, { .i64 = 0 }, 0, 1, SD },
    { "ignore_ruby", "ignore ruby-like characters (furigana)",
      OFFSET(ignore_ruby), AV_OPT_TYPE_BOOL, { .i64 = 0 }, 0, 1, SD },
    { "fadeout", "enable Fadeout",
      OFFSET(fadeout), AV_OPT_TYPE_BOOL, { .i64 = 0 }, 0, 1, SD },
    { "outline_width", "outline width of text",
      OFFSET(stroke_width), AV_OPT_TYPE_FLOAT, { .dbl = 1.5 }, 0.0, 3.0, SD },
#ifdef ARIBCC_RENDER
    { "replace_drcs", "replace known DRCS",
      OFFSET(replace_drcs), AV_OPT_TYPE_BOOL, { .i64 = 1 }, 0, 1, SD },
    { "rendering_backend", "text rendering backend",
      OFFSET(rendering_backend), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, 5, SD },
#endif
    { "sub_type", "subtitle rendering type",
      OFFSET(subtitle_type), AV_OPT_TYPE_INT,
      { .i64 = SUBTITLE_ASS }, SUBTITLE_NONE, SUBTITLE_ASS, SD, "type" },
    { "none",   "do nothing", 0, AV_OPT_TYPE_CONST,
        { .i64 = SUBTITLE_NONE }, .flags = SD, .unit = "type" },
    { "bitmap", "bitmap rendering (not implemented yet)", 0, AV_OPT_TYPE_CONST,
        { .i64 = SUBTITLE_BITMAP }, .flags = SD, .unit = "type" },
    { "text",   "plain text", 0, AV_OPT_TYPE_CONST,
        { .i64 = SUBTITLE_TEXT }, .flags = SD, .unit = "type" },
    { "ass",    "formatted text", 0, AV_OPT_TYPE_CONST,
        { .i64 = SUBTITLE_ASS }, .flags = SD, .unit = "type" },
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
    .priv_data_size = sizeof(ARIBCaptionContext),
    .init      = aribcaption_init,
    .close     = aribcaption_close,
    .decode    = aribcaption_decode,
    .flush     = aribcaption_flush,
    .priv_class= &aribcaption_class,
    .caps_internal = FF_CODEC_CAP_INIT_THREADSAFE,
};
