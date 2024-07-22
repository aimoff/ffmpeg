/*
 * Copyright (c) 2021 Ronan LE MEILLAT
 * Copyright (c) 2024 TADANO Tokumei
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

enum Faces {
    TOP_LEFT,
    TOP_MIDDLE,
    TOP_RIGHT,
    BOTTOM_LEFT,
    BOTTOM_MIDDLE,
    BOTTOM_RIGHT,
    NB_FACES,
};

enum Direction {
    RIGHT,
    LEFT,
    UP,
    DOWN,
    FRONT,
    BACK,
    NB_DIRECTIONS,
};

enum Rotation {
    ROT_0,
    ROT_90,
    ROT_180,
    ROT_270,
    NB_ROTATIONS,
};

static float2 rotate_cube_face(float2 uv, int rotation)
{
    float2 ret_uv;

    switch (rotation) {
    case ROT_0:
        ret_uv = uv;
        break;
    case ROT_90:
        ret_uv.x = -uv.y;
        ret_uv.y =  uv.x;
        break;
    case ROT_180:
        ret_uv.x = -uv.x;
        ret_uv.y = -uv.y;
        break;
    case ROT_270:
        ret_uv.x =  uv.y;
        ret_uv.y =  -uv.x;
        break;
    }

    return ret_uv;
}

static float3 equirect_to_xyz(int2 xy, int2 size)
{
    float3 xyz;
    float phi   = ((2.f * ((float)xy.x) + 1.f) / ((float)size.x) - 1.f) * M_PI_F ;
    float theta = ((2.f * ((float)xy.y) + 1.f) / ((float)size.y) - 1.f) * M_PI_2_F;

    xyz.x = cos(theta) * sin(phi);
    xyz.y = sin(theta);
    xyz.z = cos(theta) * cos(phi);

    return xyz;
}

static float2 xyz_to_cube(float3 xyz, int *face)
{
    float phi   = atan2(xyz.x, xyz.z);
    float theta = asin(xyz.y);
    float phi_norm, theta_threshold;
    float2 uv;
    int direction; 

    if (phi >= -M_PI_4_F && phi < M_PI_4_F) {
        direction = FRONT;
        phi_norm = phi;
    } else if (phi >= -(M_PI_2_F + M_PI_4_F) && phi < -M_PI_4_F) {
        direction = LEFT;
        phi_norm = phi + M_PI_2_F;
    } else if (phi >= M_PI_4_F && phi < M_PI_2_F + M_PI_4_F) {
        direction = RIGHT;
        phi_norm = phi - M_PI_2_F;
    } else {
        direction = BACK;
        phi_norm = phi + ((phi > 0.f) ? -M_PI_F : M_PI_F);
    }

    theta_threshold = atan(cos(phi_norm));
    if (theta > theta_threshold) {
        direction = DOWN;
    } else if (theta < -theta_threshold) {
        direction = UP;
    }

    switch (direction) {
    case RIGHT:
        uv.x = -xyz.z / xyz.x;
        uv.y =  xyz.y / xyz.x;
        *face = TOP_RIGHT;
        break;
    case LEFT:
        uv.x = -xyz.z / xyz.x;
        uv.y = -xyz.y / xyz.x;
        *face = TOP_LEFT;
        break;
    case UP:
        uv.x = -xyz.x / xyz.y;
        uv.y = -xyz.z / xyz.y;
        *face = BOTTOM_RIGHT;
        uv = rotate_cube_face(uv, ROT_270);
        break;
    case DOWN:
        uv.x =  xyz.x / xyz.y;
        uv.y = -xyz.z / xyz.y;
        *face = BOTTOM_LEFT;
        uv = rotate_cube_face(uv, ROT_270);
        break;
    case FRONT:
        uv.x =  xyz.x / xyz.z;
        uv.y =  xyz.y / xyz.z;
        *face = TOP_MIDDLE;
        break;
    case BACK:
        uv.x =  xyz.x / xyz.z;
        uv.y = -xyz.y / xyz.z;
        *face = BOTTOM_MIDDLE;
        uv = rotate_cube_face(uv, ROT_90);
        break;
    }

    return uv;
}

static float2 xyz_to_eac(float3 xyz, int2 size)
{
    float pixel_pad = 2;
    float u_pad = pixel_pad / size.x;
    float v_pad = pixel_pad / size.y;

    int face;
    int u_face, v_face;
    float2 uv = xyz_to_cube(xyz, &face);

    u_face = face % 3;
    v_face = face / 3;
    //eac expansion
    uv = M_2_PI_F * atan(uv) + 0.5f;

    uv.x = (uv.x + u_face) * (1.f - 2.f * u_pad) / 3.f + u_pad;
    uv.y = uv.y * (0.5f - 2.f * v_pad) + v_pad + 0.5f * v_face;

    uv.x *= size.x;
    uv.y *= size.y;

    return uv;
}

const sampler_t sampler_nearest = (CLK_NORMALIZED_COORDS_FALSE |
                                   CLK_ADDRESS_CLAMP_TO_EDGE   |
                                   CLK_FILTER_NEAREST);

const sampler_t sampler_linear  = (CLK_NORMALIZED_COORDS_FALSE |
                                   CLK_ADDRESS_CLAMP_TO_EDGE   |
                                   CLK_FILTER_LINEAR);

static float4 gopromax_to_eac(float2 uv, int overlap, __read_only image2d_t src)
{
    int2 dim = get_image_dim(src);
    int cube_size = dim.y;
    int gap = (cube_size * 3 + overlap * 2 - dim.x) / 2;
    float2 uv2 = uv;
    float a = 0.f;
    float4 val;
    bool is_aligned;

    if (uv.x < cube_size || uv.x > cube_size * 2) {
        int dx = 0;
        int cs = cube_size - gap;
        float cx = fmod(uv.x, cube_size) * cs / cube_size;
        if (uv.x >= cube_size * 2)
            dx = cube_size * 2 + overlap - gap;
        if (cx >= (cs + overlap) / 2)
            dx += overlap;
        uv2.x = cx + dx;
        if (cx > (cs - overlap) / 2 && cx < (cs + overlap) / 2)
            a = (cx - (cs - overlap) / 2) / overlap;
    } else {
        uv2.x += overlap - gap;
    }

    {
        int2 d = convert_int2(ceil(uv2) - floor(uv2));
        is_aligned = (d.x == 0 && d.y == 0);
    }
    if (is_aligned)
        val = read_imagef(src, sampler_nearest, uv2);
    else
        val = read_imagef(src, sampler_linear, uv2);
    if (a > 0.f) {
        float4 val2;
        uv2.x += overlap;
        if (is_aligned)
            val2 = read_imagef(src, sampler_nearest, uv2);
        else
            val2 = read_imagef(src, sampler_linear, uv2);
        val = mix(val, val2, a);
    }

    return val;
}

__kernel void gopromax_equirectangular(__write_only image2d_t dst,
                                       __read_only  image2d_t front,
                                       __read_only  image2d_t rear,
                                       int overlap)
{
    float4 val;
    int2 loc = (int2)(get_global_id(0), get_global_id(1));

    int2 dst_size = get_image_dim(dst);
    int2 src_size = get_image_dim(front);
    int2 eac_size = (int2)(src_size.y * 3, src_size.y * 2);

    float3 xyz = equirect_to_xyz(loc, dst_size);
    float2 uv = xyz_to_eac(xyz, eac_size);

    if (uv.y >= src_size.y) {
        uv.y -= src_size.y;
        val = gopromax_to_eac(uv, overlap, rear);
    } else {
        val = gopromax_to_eac(uv, overlap, front);
    }

    write_imagef(dst, loc, val);
}

__kernel void gopromax_stack(__write_only image2d_t dst,
                             __read_only  image2d_t front,
                             __read_only  image2d_t rear,
                             int overlap)
{
    float4 val;
    int2 loc = (int2)(get_global_id(0), get_global_id(1));
    int2 dst_size = get_image_dim(dst);
    int2 src_size = get_image_dim(front);
    float2 uv = convert_float2(loc);

    uv *= (float)src_size.y * 2 / dst_size.y;

    if (uv.y >= src_size.y) {
        uv.y -= src_size.y;
        val = gopromax_to_eac(uv, overlap, rear);
    } else {
        val = gopromax_to_eac(uv, overlap, front);
    }

    write_imagef(dst, loc, val);
}
