#ifndef SRC_MATERIALS_GLSL_
#define SRC_MATERIALS_GLSL_

#define Mat_SolidColor		1
#define Mat_Lambert			2
#define Mat_Phong			3
#define Mat_Blinn			4
#define Mat_Reflection		5

vec3 Shader_SolidColor(vec3 color, in float curAlpha)
{	return color*curAlpha;	}

vec3 Shader_Reflection(float reflectivity, DistInfo di, vec3 pRayPos, inout vec3 vRayDir, out vec3 pEye, inout float curAlpha)
{	vec3 norm = GetNormalByObj(di.obj, pRayPos, di.d);

	//make some fun colors
	vec3 c = vec3(0,0,0);
	c.r = clamp( sin(2.6-dot(norm, -vRayDir)*4.3)*1 ,0,1);
	c.gb = clamp( sin(.6-dot(-vRayDir,norm*7.0)*1.4)*0.85 ,0,1).xx;
	c *= (1.0-reflectivity)*curAlpha;
	curAlpha *= reflectivity;

	//reflect the ray through the surface normal of the object
	pEye = pRayPos;
    vRayDir = 2*dot(-vRayDir,norm)*(norm) +vRayDir;

    //if(di.d < 0.0) pEye += vRayDir * di.d*20;  //- eftersom även dist2 alltid är negativ här  //mer korrekt, men verkar inte behövas om man kör utan dynminstep
    //step out from the surface, a minimum amount before continuing the marching, otherwise the ray would get stuck since the distance to the object(and thus next step size) is approximately zero
    pEye += vRayDir *MinStep*reflection_minStep;
    //marchedDist=0.0; //should be done here or in the loop?.
	return c;
}


#endif /* SRC_MATERIALS_GLSL_ */
