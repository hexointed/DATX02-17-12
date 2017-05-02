#define NumObjects	12


bool RenderList[NumObjects+1]; //is not initialised here since we will do it later at start of each ray
vec3 objectCoords[NumObjects+1];
float objectRadii[NumObjects+1];

uint  matIdList[NumObjects+1] = {0,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,
		Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,
		Mat_SolidColor,Mat_SolidColor};
//		Mat_Reflection,Mat_Reflection,Mat_Reflection,Mat_Reflection,Mat_Reflection,Mat_Reflection,
//		Mat_Reflection,Mat_Reflection,Mat_Reflection,Mat_Reflection,
//		Mat_Reflection,Mat_Reflection,Mat_Reflection,Mat_Reflection,Mat_Reflection,Mat_Reflection,
//		Mat_Reflection};

vec3  s1Coord	= vec3(0, 1.5, 0.0 );
float s1Radius	= 0.5;
vec3  s2Coord	= vec3(0, 1.5, 0.0);
float s2Radius	= 0.4;

void InitObjDefs()
{
	// USED WHEN RENDERING METABALLS

	//vec3  s1Coord	= vec3(-0.5, 1.5, 0.0 );
	//float s1Radius	= 0.5;
	//animation
	//s1Coord.y += sin( mod(iGlobalTime*0.57, 3.1416) );
	//s2Coord.x += sin(iGlobalTime/3)*0.6;
	//s2Coord.z += cos(iGlobalTime/3)*0.6;

	objectCoords[1] = vec3(-5,4,0); //(s1Coord+s2Coord)/2;
	objectRadii[1] = 0.5; //(length(s1Coord-s2Coord)/2)+max(s1Radius,s2Radius);


	objectCoords[2] = vec3(-4,4,0);
	objectRadii[2] = 0.5;

	objectCoords[3] = vec3(-3,4,0); //vec3(-2+cos(iGlobalTime),2+sin(iGlobalTime),0);
	objectRadii[3]=0.5;

	objectCoords[4] = vec3(-2,4,0);
	objectRadii[4] = 0.5;

	objectCoords[5] = vec3(-1,4,0);
	objectRadii[5] = 0.5;

	objectCoords[6] = vec3(0,4,0);
	objectRadii[6] = 0.5;

	objectCoords[7] = vec3(1,4,0);
	objectRadii[7] = 0.5;

	objectCoords[8] = vec3(2,4,0);
	objectRadii[8] = 0.5;

	objectCoords[9] = vec3(3,4,0);
	objectRadii[9] = 0.5;

	objectCoords[10] = vec3(4,4,0);
	objectRadii[10] = 0.5;

	objectCoords[11] = vec3(-5,5,0);
	objectRadii[11] = 0.5;

	objectCoords[12] = vec3(-4,5,0);
	objectRadii[12] = 0.5;

//	objectCoords[13] = vec3(-3,5,0);
//	objectRadii[13] = 0.5;
//
//	objectCoords[14] = vec3(-2,5,0);
//	objectRadii[14] = 0.5;
//
//	objectCoords[15] = vec3(-1,5,0);
//	objectRadii[15] = 0.5;
//
//	objectCoords[16] = vec3(0,5,0);
//	objectRadii[16] = 0.5;
//
//	objectCoords[17] = vec3(1,5,0);
//	objectRadii[17] = 0.5;
//
//	objectCoords[18] = vec3(2,5,0);
//	objectRadii[18] = 0.5;
//
//	objectCoords[19] = vec3(3,5,0);
//	objectRadii[19] = 0.5;
//
//	objectCoords[20] = vec3(4,5,0);
//	objectRadii[20] = 0.5;






}

vec3  s4Coord	= vec3(0, 1.5, 0 );
float s4Radius	= 0.5;

vec3  skysphereCoord	= vec3(0, 0, 0 );
float skysphereRadius	= 20;


float DistToModfield(vec3 p)
{
	//p += vec3(1,0,1);
	//float xsec = cos(floor(p.x*0.5)*1+iGlobalTime*0.4)*0.925+0.55;
	//p.y += xsec;// cos(xsec)*0.1;
	//vec3 bp = fieldBallPos;


	p.xz = mod(p.xz, 2.0);
	p = p-fieldBallPos;
	//p.y+=atan(cos(p.x*56+iGlobalTime)*sin(p.z*56+iGlobalTime*1.31)*8.2)*0.01;
	//	p.z+=atan(cos(p.x*56+iGlobalTime)*sin(p.y*56+iGlobalTime*1.31)*8.2)*0.001;
	//	p.x+=atan(cos(p.z*56+iGlobalTime)*sin(p.y*56+iGlobalTime*1.31)*8.2)*0.001;


	return length( p )-fieldBallSize;

	//return sdSphere( p, bp, fieldBallSize );
}


float DistToMetaballs(vec3 p)
{   float dist = length( (p-s1Coord) ) - s1Radius;
    float dist2 = length( (p-s2Coord) ) - s2Radius;
    float dist3 = length(p - vec3(0,20,0)) - 18.5;

    dist = max(-dist2,(max(dist,-dist3)));


    //dist = smin(dist, dist2, 0.35  );


    //dist = smin(dist,sdPlane(p, vec4(0, 1.0, 0, 0.0) ) , 0.15 ) ;
    //dist = min(dist,sdPlane(p, vec4(0, 1.0, 0, 0.0) )  ) ;
    //dist = smin(dist, sdBox( p+vec3(4,0.3,3), vec3(30,0.1,30) )	, 1 );

    //dist = min(dist, sdSphere( p+vec3(0,3,0), s3Coord, 1.5 ) );

    //if(p.y>-1.0005)
    //{
    	//dist += sin(p.y*10  +iGlobalTime )*sin(p.y*11)*0.025;
    	//dist += tan ( cos(p.x*14 +iGlobalTime*5 )*sin(p.y*14 )*sin(p.z*14  +iGlobalTime*5  )*1.30 )*0.0145;
    	//dist += sin(p.z*20 +iGlobalTime*4 )*cos(p.z*31  +iGlobalTime*7)*0.025;
    //}
    return dist;
}

float DistToObjectId(vec3 p, uint objId)
{
	switch (objId)
	{
		case 1:
			return sdSphere( p, objectCoords[1], objectRadii[1]);
			//return DistToMetaballs(p);
			//return DistToModfield(p);
			//return sdSphere( p, s4Coord, s4Radius);
		case 2:
			return sdSphere( p, objectCoords[2], objectRadii[2]);
		case 3:
			return sdSphere( p, objectCoords[3], objectRadii[3]);
		case 4:
			return sdSphere( p, objectCoords[4], objectRadii[4]);
		case 5:
			return sdSphere( p, objectCoords[5], objectRadii[5]);
		case 6:
			return sdSphere( p, objectCoords[6], objectRadii[6]);
		case 7:
			return sdSphere( p, objectCoords[7], objectRadii[7]);
		case 8:
			return sdSphere( p, objectCoords[8], objectRadii[8]);
		case 9:
			return sdSphere( p, objectCoords[9], objectRadii[9]);
		case 10:
			return sdSphere( p, objectCoords[10], objectRadii[10]);
		case 11:
			return sdSphere( p, objectCoords[11], objectRadii[11]);
		case 12:
			return sdSphere( p, objectCoords[12], objectRadii[12]);
//		case 13:
//			return sdSphere( p, objectCoords[13], objectRadii[13]);
//		case 14:
//			return sdSphere( p, objectCoords[14], objectRadii[14]);
//		case 15:
//			return sdSphere( p, objectCoords[15], objectRadii[15]);
//		case 16:
//			return sdSphere( p, objectCoords[16], objectRadii[16]);
//		case 17:
//			return sdSphere( p, objectCoords[17], objectRadii[17]);
//		case 18:
//			return sdSphere( p, objectCoords[18], objectRadii[18]);
//		case 19:
//			return sdSphere( p, objectCoords[19], objectRadii[19]);
//		case 20:
//			return sdSphere( p, objectCoords[20], objectRadii[20]);








#ifdef DEBUG_SAFETY_TESTS
		case 0: //hit ska den inte komma
			//return 0; //remove
			return 9.0; //

		default:
			//hit ska den inte komma, så de ska inte spela nån roll vad vi returnerar
			//return 0.0; //remove
			return 1.0;  //1.0 for test, will hang if it gets that
#endif
	}
}
