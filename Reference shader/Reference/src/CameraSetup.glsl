vec3 vRayDir_orig; //stores original ray from camera direction

//! <camera type="3D"/>
// Position of the camera:
//uniform vec3 Eye;
//// Forward/direction vector of the camera:
//uniform vec3 Direction;
//// Up vector of the camera:
//uniform vec3 Up;
//
//uniform float synth_Zoom;
//
//uniform mat4 synth_ViewMatrix;
//uniform mat3 synth_NormalMatrix;

void	CameraSetup( in vec3 pEye, in vec3 pLookAt, in vec2 uv, out vec3 vRayDir )
{    vec3 vLookAt, vUp	= vec3( 0.0,
                        		1.0,
								0.0 );

    //calc ray direction - rectangular projection
    vLookAt		= normalize(pLookAt - pEye);
    vec3 vRight	= normalize( cross(vLookAt, vUp) );
	vUp 		= normalize( cross(vRight, vLookAt ) );
    vRayDir		= normalize( vLookAt + vRight*uv.x + vUp*uv.y );
    //vRayDir *= synth_NormalMatrix;
	return;
}
