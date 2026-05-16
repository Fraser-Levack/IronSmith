#version 450
layout(set = 0, binding = 0) uniform Uniforms {
    vec2 u_resolution;
    float u_time;
    float u_camera_dist;
    vec2 u_rotation; 
    vec2 _padding; 
    vec4 u_target_pos; 
};

// FETCH VEC4 CHUNKS INSTEAD OF FLOATS
layout(std430, set = 0, binding = 1) readonly buffer SceneData {
    vec4 instructions[];
};

layout(location = 0) out vec4 out_color;

// --- SDF PRIMITIVES & HELPERS ---
struct Hit { float d; vec3 col; int mat; };
Hit opU(Hit h1, Hit h2) { return (h1.d < h2.d) ? h1 : h2; }
Hit opS(Hit h1, Hit h2) { return (h1.d > -h2.d) ? h1 : Hit(-h2.d, h1.col, h1.mat); }
Hit opI(Hit h1, Hit h2) { return (h1.d > h2.d) ? h1 : h2; }

float sdSphere(vec3 p, float s) { return length(p)-s; }
float sdBox(vec3 p, vec3 b) { vec3 q = abs(p) - b; return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0); }
float sdCylinder(vec3 p, float h, float r) { vec2 d = abs(vec2(length(p.xz),p.y)) - vec2(r,h); return min(max(d.x,d.y),0.0) + length(max(d,0.0)); }
float sdTorus(vec3 p, vec2 t) { vec2 q = vec2(length(p.xz)-t.x,p.y); return length(q)-t.y; }
float sdCappedCone(vec3 p, float h, float r1, float r2) {
    vec2 q = vec2(length(p.xz), p.y); vec2 k1 = vec2(r2,h); vec2 k2 = vec2(r2-r1,2.0*h);
    vec2 ca = vec2(q.x-min(q.x,(q.y<0.0)?r1:r2), abs(q.y)-h);
    vec2 cb = q - k1 + k2*clamp(dot(k1-q,k2)/dot(k2,k2), 0.0, 1.0);
    float s = (cb.x<0.0 && ca.y<0.0) ? -1.0 : 1.0;
    return s*sqrt(min(dot(ca,ca),dot(cb,cb)));
}

vec3 opRep(vec3 p, vec3 c) {
    vec3 q = p;
    if (c.x > 0.0) q.x = mod(p.x + 0.5*c.x, c.x) - 0.5*c.x;
    if (c.y > 0.0) q.y = mod(p.y + 0.5*c.y, c.y) - 0.5*c.y;
    if (c.z > 0.0) q.z = mod(p.z + 0.5*c.z, c.z) - 0.5*c.z;
    return q;
}

// --- THE VIRTUAL MACHINE ---
Hit map(vec3 p) {
    // DESTROY REGISTER SPILLING: Use explicit variables instead of arrays
    Hit s0, s1, s2, s3, s4, s5, s6, s7;
    int sp = 0; 
    
    vec3 p0, p1, p2, p3, p4, p5, p6, p7;
    float sc0, sc1, sc2, sc3, sc4, sc5, sc6, sc7;
    int tsp = 0; 
    
    vec3 c0, c1, c2, c3, c4, c5, c6, c7;
    int csp = 0; 

    int pc = 0; 
    vec3 current_p = p;
    float current_scale = 1.0;
    vec3 current_col = vec3(0.8, 0.8, 0.8);

    // MACROS: Forces the GLSL compiler to assign these to fast GPU hardware registers
    #define PUSH_HIT(h) if(sp==0)s0=h; else if(sp==1)s1=h; else if(sp==2)s2=h; else if(sp==3)s3=h; else if(sp==4)s4=h; else if(sp==5)s5=h; else if(sp==6)s6=h; else if(sp==7)s7=h; sp++
    #define POP_HIT(out_h) sp--; if(sp==0)out_h=s0; else if(sp==1)out_h=s1; else if(sp==2)out_h=s2; else if(sp==3)out_h=s3; else if(sp==4)out_h=s4; else if(sp==5)out_h=s5; else if(sp==6)out_h=s6; else if(sp==7)out_h=s7

    #define PUSH_TRANSFORM(pt, s) if(tsp==0){p0=pt;sc0=s;} else if(tsp==1){p1=pt;sc1=s;} else if(tsp==2){p2=pt;sc2=s;} else if(tsp==3){p3=pt;sc3=s;} else if(tsp==4){p4=pt;sc4=s;} tsp++
    #define POP_TRANSFORM(out_p, out_s) tsp--; if(tsp==0){out_p=p0;out_s=sc0;} else if(tsp==1){out_p=p1;out_s=sc1;} else if(tsp==2){out_p=p2;out_s=sc2;} else if(tsp==3){out_p=p3;out_s=sc3;} else if(tsp==4){out_p=p4;out_s=sc4;}

    #define PUSH_COLOR(c) if(csp==0)c0=c; else if(csp==1)c1=c; else if(csp==2)c2=c; else if(csp==3)c3=c; csp++
    #define POP_COLOR(out_c) csp--; if(csp==0)out_c=c0; else if(csp==1)out_c=c1; else if(csp==2)out_c=c2; else if(csp==3)out_c=c3;

    while (pc < 1000) { // Safety limit
        vec4 inst1 = instructions[pc];
        int iop = int(inst1.x);
        
        if (iop >= 20 && iop <= 26 && iop != 25) {
            PUSH_TRANSFORM(current_p, current_scale);
        }
        
        switch (iop) {
            case 0: { pc = 9999; break; }
            case 1: { // SPHERE [1.0, r, mat, 0.0]
                PUSH_HIT(Hit(sdSphere(current_p, inst1.y) * current_scale, current_col, int(inst1.z))); 
                pc += 1; break; 
            }
            case 2: { // BOX [2.0, w, h, d] [mat, 0.0, 0.0, 0.0]
                vec4 inst2 = instructions[pc+1];
                PUSH_HIT(Hit(sdBox(current_p, inst1.yzw) * current_scale, current_col, int(inst2.x))); 
                pc += 2; break; 
            }
            case 3: { // CYLINDER [3.0, r, h, mat]
                PUSH_HIT(Hit(sdCylinder(current_p, inst1.z, inst1.y) * current_scale, current_col, int(inst1.w))); 
                pc += 1; break; 
            }
            case 4: { // CONE [4.0, r1, r2, h] [mat, 0.0, 0.0, 0.0]
                vec4 inst2 = instructions[pc+1];
                PUSH_HIT(Hit(sdCappedCone(current_p, inst1.w, inst1.y, inst1.z) * current_scale, current_col, int(inst2.x))); 
                pc += 2; break; 
            }
            case 5: { // TORUS [5.0, r, tr, mat]
                PUSH_HIT(Hit(sdTorus(current_p, inst1.yz) * current_scale, current_col, int(inst1.w))); 
                pc += 1; break; 
            }
            case 10: { Hit a, b; POP_HIT(b); POP_HIT(a); PUSH_HIT(opU(a, b)); pc += 1; break; }
            case 11: { Hit a, b; POP_HIT(b); POP_HIT(a); PUSH_HIT(opS(a, b)); pc += 1; break; }
            case 12: { Hit a, b; POP_HIT(b); POP_HIT(a); PUSH_HIT(opI(a, b)); pc += 1; break; }
            case 20: { // ROTATE X
                float a = inst1.y; float c = cos(-a); float s = sin(-a);
                current_p = mat3(1,0,0, 0,c,s, 0,-s,c) * current_p; pc += 1; break; 
            }
            case 21: { // ROTATE Y
                float a = inst1.y; float c = cos(-a); float s = sin(-a);
                current_p = mat3(c,0,-s, 0,1,0, s,0,c) * current_p; pc += 1; break; 
            }
            case 22: { // ROTATE Z
                float a = inst1.y; float c = cos(-a); float s = sin(-a);
                current_p = mat3(c,s,0, -s,c,0, 0,0,1) * current_p; pc += 1; break; 
            }
            case 23: { // SCALE
                current_scale *= min(inst1.y, min(inst1.z, inst1.w));
                current_p /= inst1.yzw; pc += 1; break; 
            }
            case 24: { // MOVE
                current_p -= inst1.yzw; pc += 1; break; 
            }
            case 26: { // REPEAT
                current_p = opRep(current_p, inst1.yzw); pc += 1; break; 
            }
            case 25: { POP_TRANSFORM(current_p, current_scale); pc += 1; break; }
            case 30: { PUSH_COLOR(current_col); current_col = inst1.yzw; pc += 1; break; }
            case 31: { POP_COLOR(current_col); pc += 1; break; }
            default: { pc = 9999; break; }
        }
    }
    
    if (sp == 0) return Hit(999999.0, vec3(0.0), 0);
    Hit res; POP_HIT(res);
    return res; 
}

// 33% Faster Tetrahedral Normal Approximation
vec3 calcNormal(vec3 p) {
    vec2 e = vec2(1.0, -1.0) * 0.0005;
    return normalize(
        e.xyy * map(p + e.xyy).d +
        e.yyx * map(p + e.yyx).d +
        e.yxy * map(p + e.yxy).d +
        e.xxx * map(p + e.xxx).d
    );
}

void main() {
    vec2 uv = gl_FragCoord.xy / u_resolution.xy;
    vec2 st = (2.0 * uv - 1.0) * vec2(u_resolution.x / u_resolution.y, -1.0);
    
    float p_angle = u_rotation.x; 
    float y_angle = u_rotation.y;

    vec3 target_pos = u_target_pos.xyz;
    vec3 ro = target_pos + vec3(u_camera_dist * cos(p_angle) * sin(y_angle),
                                u_camera_dist * sin(p_angle),
                                u_camera_dist * cos(p_angle) * cos(y_angle));
    
    vec3 ww = normalize(target_pos - ro);
    vec3 uu = normalize(cross(ww, vec3(0.0, 1.0, 0.0)));
    vec3 vv = normalize(cross(uu, ww));
    vec3 rd = normalize(st.x * uu + st.y * vv + 1.5 * ww);

    float t = 0.0;
    vec3 material_col = vec3(0.0);
    int material_id = 0;
    bool hit = false; 

    // --- OPTIMIZED GLOBAL BOUNDING SPHERE ---
    float scene_radius = 35.0; 
    vec3 scene_center = vec3(0.0, 0.0, 0.0);
    vec3 oc = ro - scene_center;
    float b = dot(oc, rd);
    float c = dot(oc, oc) - scene_radius * scene_radius;
    float h = b * b - c;

    // Ray vs Sphere math: skip entirely if it misses!
    if (h >= 0.0) {
        t = max(0.0, -b - sqrt(h)); 
        for(int i = 0; i < 60; i++) { // Max steps reduced
            vec3 p = ro + rd * t;
            Hit res = map(p);
            if(res.d < 0.001) {
                material_col = res.col; 
                material_id = res.mat;
                hit = true; 
                break;
            }
            if(t > 50.0) break;
            t += max(res.d, 0.005); 
        }
    }

    vec3 bg_color = vec3(0.02, 0.02, 0.05);
    vec3 col = bg_color;
    vec3 light_dir = normalize(vec3(1.0, 2.0, 1.0));
    
    if(hit) {
        vec3 pos = ro + rd * t;
        vec3 normal = calcNormal(pos);
        vec3 view_dir = normalize(ro - pos);
        
        if (material_id == 0) {
            float diff = max(dot(normal, light_dir), 0.0);
            col = material_col * diff + material_col * 0.1; 
        } 
        else if (material_id == 1) {
            float diff = max(dot(normal, light_dir), 0.0);
            vec3 half_dir = normalize(light_dir + view_dir);
            float spec = pow(max(dot(normal, half_dir), 0.0), 64.0);
            col = (material_col * diff) + (material_col * 0.1) + vec3(1.0) * spec;
        } 
        else if (material_id == 2) {
            col = material_col * 1.8; 
        }
        else if (material_id == 3) {
            vec3 ref_rd = reflect(rd, normal);
            vec3 ref_ro = pos + normal * 0.01; 
            float ref_t = 0.0;
            bool ref_hit = false;
            vec3 ref_col = vec3(0.0);
            int ref_mat = 0; 

            // Fast exit reflections
            for(int i = 0; i < 15; i++) { // Cut to 15!
                Hit ref_res = map(ref_ro + ref_rd * ref_t);
                if(ref_res.d < 0.001) {
                    ref_col = ref_res.col;
                    ref_mat = ref_res.mat;
                    ref_hit = true;
                    break;
                }
                if(ref_t > 30.0) break; // Cut to 30!
                ref_t += max(ref_res.d, 0.01); // Increased min step!
            }

            if (ref_hit) {
                vec3 ref_pos = ref_ro + ref_rd * ref_t;
                vec3 ref_normal = calcNormal(ref_pos);
                vec3 ref_view_dir = normalize(ref_ro - ref_pos);
                vec3 shaded_ref = vec3(0.0);

                if (ref_mat == 0) {
                    float ref_diff = max(dot(ref_normal, light_dir), 0.0);
                    shaded_ref = ref_col * ref_diff + ref_col * 0.1;
                } else if (ref_mat == 1) {
                    float ref_diff = max(dot(ref_normal, light_dir), 0.0);
                    vec3 half_dir = normalize(light_dir + ref_view_dir);
                    float spec = pow(max(dot(ref_normal, half_dir), 0.0), 64.0);
                    shaded_ref = (ref_col * ref_diff) + (ref_col * 0.1) + vec3(1.0) * spec;
                } else if (ref_mat == 2) {
                    shaded_ref = ref_col * 1.8;
                } else if (ref_mat == 3) {
                    vec3 fake_sky = bg_color + max(reflect(ref_rd, ref_normal).y, 0.0) * 0.3;
                    vec3 half_dir = normalize(light_dir + ref_view_dir);
                    float spec = pow(max(dot(ref_normal, half_dir), 0.0), 128.0);
                    shaded_ref = mix(ref_col * 0.2, fake_sky, 0.8) + vec3(1.0) * spec;
                }
                col = mix(material_col * 0.2, shaded_ref, 0.8);
            } else {
                vec3 fake_sky = bg_color + max(ref_rd.y, 0.0) * 0.3;
                col = mix(material_col * 0.2, fake_sky, 0.8);
            }
            
            vec3 half_dir = normalize(light_dir + view_dir);
            float spec = pow(max(dot(normal, half_dir), 0.0), 128.0);
            col += vec3(1.0) * spec;
        }
    }
    
    // Gamma Correction
    out_color = vec4(pow(col, vec3(0.4545)), 1.0); 
}