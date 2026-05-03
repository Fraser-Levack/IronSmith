#version 450
layout(set = 0, binding = 0) uniform Uniforms {
    vec2 u_resolution;
    float u_time;
    float u_camera_dist;
    vec2 u_rotation; 
    vec2 _padding; 
    vec4 u_target_pos; 
};

// The SSBO binding for our instruction stream
layout(std430, set = 0, binding = 1) readonly buffer SceneData {
    float instructions[];
};

layout(location = 0) out vec4 out_color;

// --- SDF PRIMITIVES & HELPERS ---
struct Hit { float d; vec3 col; int mat; };
Hit opU(Hit h1, Hit h2) { return (h1.d < h2.d) ? h1 : h2; }
Hit opS(Hit h1, Hit h2) { return (h1.d > -h2.d) ? h1 : Hit(-h2.d, h1.col, h1.mat); }
Hit opI(Hit h1, Hit h2) { return (h1.d > h2.d) ? h1 : h2; }
Hit opScale(Hit h, float s) { return Hit(h.d * s, h.col, h.mat); }
float sdSphere(vec3 p, float s) { return length(p)-s; }
float sdBox(vec3 p, vec3 b) { vec3 q = abs(p) - b; return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0); }

// --- THE VIRTUAL MACHINE ---
Hit map(vec3 p) {
    Hit stack[10]; // Our memory stack
    int sp = 0;    // Stack Pointer
    int pc = 0;    // Program Counter
    
    while (pc < 1000) {
        float op = instructions[pc];
        
        if (op == 0.0) { // OP_HALT
            break;
        } 
        else if (op == 1.0) { // OP_SPHERE
            vec3 pos = vec3(instructions[pc+1], instructions[pc+2], instructions[pc+3]);
            float r = instructions[pc+4];
            int mat = int(instructions[pc+5]);
            
            stack[sp++] = Hit(sdSphere(p - pos, r), vec3(0.8, 0.8, 0.8), mat);
            pc += 6; 
        } 
        else if (op == 2.0) { // OP_BOX
            vec3 pos = vec3(instructions[pc+1], instructions[pc+2], instructions[pc+3]);
            vec3 ext = vec3(instructions[pc+4], instructions[pc+5], instructions[pc+6]);
            int mat = int(instructions[pc+7]);
            
            stack[sp++] = Hit(sdBox(p - pos, ext), vec3(0.8, 0.4, 0.1), mat);
            pc += 8;
        } 
        else if (op == 10.0) { // OP_UNION
            Hit b = stack[--sp]; 
            Hit a = stack[--sp]; 
            stack[sp++] = opU(a, b);
            pc += 1;
        }
        else if (op == 11.0) { // OP_DIFF
            Hit b = stack[--sp]; 
            Hit a = stack[--sp]; 
            stack[sp++] = opS(a, b);
            pc += 1;
        } else {
            break; 
        }
    }
    
    if (sp == 0) return Hit(999999.0, vec3(0.0), 0);
    return stack[0]; 
}

vec3 calcNormal(vec3 p) {
    vec2 e = vec2(0.001, 0.0);
    return normalize(vec3(
        map(p + e.xyy).d - map(p - e.xyy).d,
        map(p + e.yxy).d - map(p - e.yxy).d,
        map(p + e.yyx).d - map(p - e.yyx).d
    ));
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

    // Primary Raymarch
    for(int i = 0; i < 256; i++) {
        Hit res = map(ro + rd * t);
        if(res.d < 0.001) {
            material_col = res.col; 
            material_id = res.mat;
            hit = true; 
            break;
        }
        if(t > 100.0) break;
        t += max(res.d, 0.005); 
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

            for(int i = 0; i < 100; i++) {
                Hit ref_res = map(ref_ro + ref_rd * ref_t);
                if(ref_res.d < 0.001) {
                    ref_col = ref_res.col;
                    ref_mat = ref_res.mat;
                    ref_hit = true;
                    break;
                }
                if(ref_t > 50.0) break;
                ref_t += max(ref_res.d, 0.005);
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
    
    out_color = vec4(pow(col, vec3(0.4545)), 1.0); 
}