/*
 * Copyright (c) 2021 Ronan LE MEILLAT
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

#include "libavutil/log.h"
#include "libavutil/mem.h"
#include "libavutil/pixdesc.h"
#include "libavutil/opt.h"

#include "avfilter.h"
#include "framesync.h"
#include "internal.h"
#include "opencl.h"
#include "opencl_source.h"
#include "video.h"
#include "v360.h"

#define _WIDTH 5376
#define _HEIGHT 2688
#define OVERLAP 64
#define CUT 688
#define BASESIZE 4096 //OVERLAP and CUT are based on this size

typedef struct GoProMaxOpenCLContext {
    OpenCLFilterContext ocf;

    int              initialised;
    cl_kernel        kernel;
    cl_command_queue command_queue;

    FFFrameSync      fs;

    int              nb_planes;

    int              out;
} GoProMaxOpenCLContext;

static int gopromax_opencl_load(AVFilterContext *avctx,
                                enum AVPixelFormat front_format,
                                enum AVPixelFormat rear_format)
{
    GoProMaxOpenCLContext *ctx = avctx->priv;
    cl_int cle;
    const char *source = ff_opencl_source_gopromax;
    const char *kernel;
    const AVPixFmtDescriptor *front_desc, *rear_desc;
    int err, i, front_planes, rear_planes;

    front_desc = av_pix_fmt_desc_get(front_format);
    rear_desc  = av_pix_fmt_desc_get(rear_format);
    front_planes = rear_planes = 0;
    for (i = 0; i < front_desc->nb_components; i++)
        front_planes = FFMAX(front_planes,
                             front_desc->comp[i].plane + 1);
    for (i = 0; i < rear_desc->nb_components; i++)
        rear_planes = FFMAX(rear_planes,
                            rear_desc->comp[i].plane + 1);

    ctx->nb_planes = front_planes;

    switch (ctx->out) {
    case EQUIRECTANGULAR:
        kernel = "gopromax_equirectangular";
        break;
    case EQUIANGULAR:
        kernel = "gopromax_stack";
        break;
    default:
        av_log(ctx, AV_LOG_ERROR, "Specified output format is not handled.\n");
        return AVERROR_BUG;
    }

    av_log(avctx, AV_LOG_DEBUG, "Using kernel %s.\n", kernel);

    err = ff_opencl_filter_load_program(avctx, &source, 1);
    if (err < 0)
        goto fail;

    ctx->command_queue = clCreateCommandQueue(ctx->ocf.hwctx->context,
                                              ctx->ocf.hwctx->device_id,
                                              0, &cle);
    CL_FAIL_ON_ERROR(AVERROR(EIO), "Failed to create OpenCL "
                     "command queue %d.\n", cle);

    ctx->kernel = clCreateKernel(ctx->ocf.program, kernel, &cle);
    CL_FAIL_ON_ERROR(AVERROR(EIO), "Failed to create kernel %d.\n", cle);

    ctx->initialised = 1;
    return 0;

fail:
    if (ctx->command_queue)
        clReleaseCommandQueue(ctx->command_queue);
    if (ctx->kernel)
        clReleaseKernel(ctx->kernel);
    return err;
}

static int gopromax_opencl_stack(FFFrameSync *fs)
{
    AVFilterContext     *avctx = fs->parent;
    AVFilterLink      *outlink = avctx->outputs[0];
    GoProMaxOpenCLContext *ctx = avctx->priv;
    AVFrame *input_front, *input_rear;
    AVFrame *output;
    cl_mem mem;
    cl_int cle;
    size_t global_work[2];
    int kernel_arg = 0;
    int err, plane;

    err = ff_framesync_get_frame(fs, 0, &input_front, 0);
    if (err < 0)
        return err;
    err = ff_framesync_get_frame(fs, 1, &input_rear, 0);
    if (err < 0)
        return err;

    if (!ctx->initialised) {
        AVHWFramesContext *front_fc =
            (AVHWFramesContext*)input_front->hw_frames_ctx->data;
        AVHWFramesContext *rear_fc =
            (AVHWFramesContext*)input_rear->hw_frames_ctx->data;

        err = gopromax_opencl_load(avctx, front_fc->sw_format,
                                   rear_fc->sw_format);
        if (err < 0)
            return err;
    }

    output = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    if (!output) {
        err = AVERROR(ENOMEM);
        goto fail;
    }

    for (plane = 0; plane < ctx->nb_planes; plane++) {
        kernel_arg = 0;

        mem = (cl_mem)output->data[plane];
        CL_SET_KERNEL_ARG(ctx->kernel, kernel_arg, cl_mem, &mem);
        kernel_arg++;

        mem = (cl_mem)input_front->data[plane];
        CL_SET_KERNEL_ARG(ctx->kernel, kernel_arg, cl_mem, &mem);
        kernel_arg++;

        mem = (cl_mem)input_rear->data[plane];
        CL_SET_KERNEL_ARG(ctx->kernel, kernel_arg, cl_mem, &mem);
        kernel_arg++;

        err = ff_opencl_filter_work_size_from_image(avctx, global_work,
                                                    output, plane, 0);
        if (err < 0)
            goto fail;

        av_log(avctx, AV_LOG_VERBOSE,
               "In gopromax_opencl_stack for plane:%d %lux%lu frame size %dx%d\n",
               plane, global_work[0], global_work[1], outlink->w, outlink->h);

        cle = clEnqueueNDRangeKernel(ctx->command_queue, ctx->kernel, 2, NULL,
                                     global_work, NULL, 0, NULL, NULL);
        CL_FAIL_ON_ERROR(AVERROR(EIO), "Failed to enqueue gopromax kernel "
                         "for plane %d: %d.\n", plane, cle);
    }

    cle = clFinish(ctx->command_queue);
    CL_FAIL_ON_ERROR(AVERROR(EIO), "Failed to finish command queue: %d.\n", cle);

    err = av_frame_copy_props(output, input_front);

    av_log(avctx, AV_LOG_DEBUG, "Filter output: %s, %ux%u (%"PRId64").\n",
           av_get_pix_fmt_name(output->format),
           output->width, output->height, output->pts);

    return ff_filter_frame(outlink, output);

fail:
    av_frame_free(&output);
    return err;
}

static int gopromax_opencl_config_output(AVFilterLink *outlink)
{
    AVFilterContext *avctx = outlink->src;
    GoProMaxOpenCLContext *ctx = avctx->priv;
    int height = avctx->inputs[0]->h;
    int width = avctx->inputs[0]->w;
    int err;

    switch (ctx->out) {
    case EQUIRECTANGULAR:
        ctx->ocf.output_width = 4 * height;
        ctx->ocf.output_height = 2 * height;
        break;
    case EQUIANGULAR:
        int overlap = width * OVERLAP / BASESIZE;
        ctx->ocf.output_width = width - 2 * overlap;
        ctx->ocf.output_height = 2 * height;
        break;
    default:
        av_log(ctx, AV_LOG_ERROR, "Specified output format is not supported.\n");
        return AVERROR(EINVAL);
    }

    err = ff_opencl_filter_config_output(outlink);
    if (err < 0)
        return err;

    err = ff_framesync_init_dualinput(&ctx->fs, avctx);
    if (err < 0)
        return err;

    return ff_framesync_configure(&ctx->fs);
}

static av_cold int gopromax_opencl_init(AVFilterContext *avctx)
{
    GoProMaxOpenCLContext *ctx = avctx->priv;

    ctx->fs.on_event = &gopromax_opencl_stack;

    return ff_opencl_filter_init(avctx);
}

static int gopromax_opencl_activate(AVFilterContext *avctx)
{
    GoProMaxOpenCLContext *ctx = avctx->priv;

    return ff_framesync_activate(&ctx->fs);
}

static av_cold void gopromax_opencl_uninit(AVFilterContext *avctx)
{
    GoProMaxOpenCLContext *ctx = avctx->priv;
    cl_int cle;

    if (ctx->kernel) {
        cle = clReleaseKernel(ctx->kernel);
        if (cle != CL_SUCCESS)
            av_log(avctx, AV_LOG_ERROR, "Failed to release "
                   "kernel: %d.\n", cle);
    }

    if (ctx->command_queue) {
        cle = clReleaseCommandQueue(ctx->command_queue);
        if (cle != CL_SUCCESS)
            av_log(avctx, AV_LOG_ERROR, "Failed to release "
                   "command queue: %d.\n", cle);
    }

    ff_opencl_filter_uninit(avctx);

    ff_framesync_uninit(&ctx->fs);
}

#define OFFSET(x) offsetof(GoProMaxOpenCLContext, x)
#define FLAGS (AV_OPT_FLAG_FILTERING_PARAM | AV_OPT_FLAG_VIDEO_PARAM)
static const AVOption gopromax_opencl_options[] = {
    {   "output", "set output projection",  OFFSET(out), AV_OPT_TYPE_INT,   {.i64=EQUIRECTANGULAR}, 0, NB_PROJECTIONS-1, FLAGS, .unit = "out" },
    {        "e", "equirectangular",                  0, AV_OPT_TYPE_CONST, {.i64=EQUIRECTANGULAR}, 0,                0, FLAGS, .unit = "out" },
    { "equirect", "equirectangular",                  0, AV_OPT_TYPE_CONST, {.i64=EQUIRECTANGULAR}, 0,                0, FLAGS, .unit = "out" },
    {      "eac", "equi-angular cubemap",             0, AV_OPT_TYPE_CONST, {.i64=EQUIANGULAR},     0,                0, FLAGS, .unit = "out" },
    { NULL },
};

AVFILTER_DEFINE_CLASS(gopromax_opencl);

static const AVFilterPad gopromax_opencl_inputs[] = {
    {
        .name         = "front",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = &ff_opencl_filter_config_input,
    },
    {
        .name         = "rear",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = &ff_opencl_filter_config_input,
    },
};

static const AVFilterPad gopromax_opencl_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .config_props  = &gopromax_opencl_config_output,
    },
};

const AVFilter ff_vf_gopromax_opencl = {
    .name            = "gopromax_opencl",
    .description     = NULL_IF_CONFIG_SMALL("GoProMax .360 to equirectangular projection"),
    .priv_size       = sizeof(GoProMaxOpenCLContext),
    .priv_class      = &gopromax_opencl_class,
    .init            = &gopromax_opencl_init,
    .uninit          = &gopromax_opencl_uninit,
    .activate        = &gopromax_opencl_activate,
    FILTER_INPUTS(gopromax_opencl_inputs),
    FILTER_OUTPUTS(gopromax_opencl_outputs),
    FILTER_SINGLE_PIXFMT(AV_PIX_FMT_OPENCL),
    .flags_internal  = FF_FILTER_FLAG_HWFRAME_AWARE,
};
