#ifndef SRC_UNIFORMS_GLSL_
#define SRC_UNIFORMS_GLSL_

uniform vec3      iResolution;           // viewport resolution (in pixels)
uniform float     iGlobalTime;           // shader playback time (in seconds)
uniform float     iTimeDelta;            // render time (in seconds)
uniform int       iFrame;                // shader playback frame
uniform float     iChannelTime[4];       // channel playback time (in seconds)
uniform vec3      iChannelResolution[4]; // channel resolution (in pixels)
uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
uniform vec4      iDate;                 // (year, month, day, time in seconds)
uniform float     iSampleRate;           // sound sample rate (i.e., 44100)

uniform samplerCube iChannel1; 		//! texture["../cube/yokohama_0.jpg"]

uniform vec3 pEye_slider; 			//! slider[(-19,-19,-19), (0,1,2), (19,19,19)]
uniform vec3 pLookAt_slider; 		//! slider[(-19,-19,-19), (0,0,0), (19,19,19)]
uniform float reflection_minStep; 	//! slider[(0.5), (2), (300)]
uniform float normEps_s; 			//! slider[(0.01), (1), (100)]
uniform float fieldBallSize; 		//! slider[(0.01), (0.6), (4)]
uniform vec3 fieldBallPos; 			//! slider[(-3,-3,-3), (1,1,1), (3,3,3)]

#endif /* SRC_UNIFORMS_GLSL_ */
