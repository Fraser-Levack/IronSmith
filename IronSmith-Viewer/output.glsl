#version 450

layout(set = 0, binding = 0) uniform Uniforms {
    vec2 u_resolution;
    float u_time;
    float u_camera_dist;
    vec2 u_rotation; // x = pitch, y = yaw
    vec2 _padding; 
};

layout(location = 0) out vec4 out_color;

// --- IronSmith SDF Library ---
float sdSphere(vec3 p, float s) { return length(p)-s; }
float sdBox(vec3 p, vec3 b) { vec3 q = abs(p) - b; return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0); }

// --- IronSmith Generated Scene ---
float map(vec3 p) {
    return min(
        sdBox((p - vec3(10.0, 0.0, 0.0)), vec3(2.0, 2.0, 2.0)), 
        sdSphere((p - vec3(-10.0, 0.0, 0.0)), 2.0)
    );
}

// --- Raymarching Engine ---
vec3 calcNormal(vec3 p) {
    vec2 e = vec2(0.001, 0.0);
    return normalize(vec3(
        map(p + e.xyy) - map(p - e.xyy),
        map(p + e.yxy) - map(p - e.yxy),
        map(p + e.yyx) - map(p - e.yyx)
    ));
}

void main() {
    vec2 uv = gl_FragCoord.xy / u_resolution.xy;
    vec2 st = (2.0 * uv - 1.0) * vec2(u_resolution.x / u_resolution.y, -1.0);

    // 1. Unpack camera variables
    float yaw = u_rotation.y;
    float pitch = u_rotation.x;
    float dist = u_camera_dist;

    // 2. Spherical coordinates to Cartesian (Orbiting the center)
    vec3 ro = vec3(
        dist * cos(pitch) * sin(yaw),
        dist * sin(pitch),
        dist * cos(pitch) * cos(yaw)
    );

    // 3. Create a "Look-At" Matrix
    vec3 ta = vec3(0.0, 0.0, 0.0); // Target: Look at the center of the world
    vec3 ww = normalize(ta - ro); // Forward vector
    vec3 uu = normalize(cross(ww, vec3(0.0, 1.0, 0.0))); // Right vector
    vec3 vv = normalize(cross(uu, ww)); // Up vector

    // 4. Calculate final Ray Direction
    vec3 rd = normalize(st.x * uu + st.y * vv + 1.5 * ww); // 1.5 acts as Field of View (Zoom)

    // ... (Keep your Raymarching Loop and Lighting exactly as it was) ...
    float t = 0.0; 
    for(int i = 0; i < 100; i++) {
        vec3 p = ro + rd * t; 
        float d = map(p);     
        if(d < 0.001) break;  
        if(t > 100.0) break;  
        t += d;               
    }

    vec3 col = vec3(0.05, 0.05, 0.1); 
    if(t < 100.0) { 
        vec3 p = ro + rd * t;
        vec3 normal = calcNormal(p);
        vec3 lightPos = normalize(vec3(1.0, 1.0, 1.0)); 
        float diffuse = max(dot(normal, lightPos), 0.0); 
        col = vec3(0.8, 0.4, 0.1) * diffuse + vec3(0.1); 
    }

    out_color = vec4(col, 1.0);
}