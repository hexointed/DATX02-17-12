#define NumObjects	1
#define NumSpheres	4

bool RenderList[NumObjects+1]; //is not initialised here since we will do it later at start of each ray
vec3 objectCoords[NumObjects+1];
float objectRadii[NumObjects+1];

uint  matIdList[NumObjects+1] = {0
		,Mat_SolidColor//,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor
//		,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor
//		,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor
//		,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor
//		,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor
//		,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor
//		,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor
//		,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor
//		,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor,Mat_SolidColor
		};

vec3  s1Coord	= vec3(-0.5,-0.5,0);
float s1Radius	= 0.5;
vec3  s2Coord	= vec3(0.5,0-0.5,0);
float s2Radius	= 0.5;
vec3  s3Coord	= vec3(-0.5,0.5,0);
float s3Radius	= 0.5;
vec3  s4Coord	= vec3(0.5,0.5,0);
float s4Radius	= 0.5;

float sphereRadii[NumSpheres] = {s1Radius,s2Radius,s3Radius,s4Radius};
vec3 spherePos[NumSpheres] = {s1Coord,s2Coord,s3Coord,s4Coord};

void InitObjDefs()
{
//	 USED WHEN RENDERING METABALLS
//
//	vec3  s1Coord	= vec3(-0.5, 1.5, 0.0 );
//	float s1Radius	= 0.5;
//	animation
//	s1Coord.y += sin( mod(iGlobalTime*0.57, 3.1416) );
//	s2Coord.x += sin(iGlobalTime/3)*0.6;
//	s2Coord.z += cos(iGlobalTime/3)*0.6;

//	objectCoords[1] = vec3(-0.5,0.5,0);
//	objectRadii[1] = 0.5;
//
//	objectCoords[2] = vec3(-0.5,-0.5,0);
//	objectRadii[2] = 0.5;
//
//	objectCoords[3] = vec3(0.5,0.5,0); //vec3(-2+cos(iGlobalTime),2+sin(iGlobalTime),0);
//	objectRadii[3]=0.5;
//
//	objectCoords[4] = vec3(0.5,-0.5,0);
//	objectRadii[4] = 0.5;
//
//	objectCoords[5] = vec3(-1,4,0);
//	objectRadii[5] = 0.5;
//
//	objectCoords[6] = vec3(0,4,0);
//	objectRadii[6] = 0.5;
//
//	objectCoords[7] = vec3(1,4,0);
//	objectRadii[7] = 0.5;
//
//	objectCoords[8] = vec3(2,4,0);
//	objectRadii[8] = 0.5;
//
//	objectCoords[9] = vec3(3,4,0);
//	objectRadii[9] = 0.5;
//
//	objectCoords[10] = vec3(4,4,0);
//	objectRadii[10] = 0.5;
//
//	objectCoords[11] = vec3(-5,5,0);
//	objectRadii[11] = 0.5;
//
//	objectCoords[12] = vec3(-4,5,0);
//	objectRadii[12] = 0.5;
//
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
//
//	objectCoords[21] = vec3(-5,6,0);
//	objectRadii[21] = 0.5;
//
//	objectCoords[22] = vec3(-4,6,0);
//	objectRadii[22] = 0.5;
//
//	objectCoords[23] = vec3(-3,6,0);
//	objectRadii[23] = 0.5;
//
//	objectCoords[24] = vec3(-2,6,0);
//	objectRadii[24] = 0.5;
//
//	objectCoords[25] = vec3(-1,6,0);
//	objectRadii[25] = 0.5;
//
//	objectCoords[26] = vec3(0,6,0);
//	objectRadii[26] = 0.5;
//
//	objectCoords[27] = vec3(1,6,0);
//	objectRadii[27] = 0.5;
//
//	objectCoords[28] = vec3(2,6,0);
//	objectRadii[28] = 0.5;
//
//	objectCoords[29] = vec3(3,6,0);
//	objectRadii[29] = 0.5;
//
//	objectCoords[30] = vec3(4,6,0);
//	objectRadii[30] = 0.5;
//
//	objectCoords[31] = vec3(-5,3,0);
//	objectRadii[31] = 0.5;
//
//	objectCoords[32] = vec3(-4,3,0);
//	objectRadii[32] = 0.5;
//
//	objectCoords[33] = vec3(-3,3,0);
//	objectRadii[33] = 0.5;
//
//	objectCoords[34] = vec3(-2,3,0);
//	objectRadii[34] = 0.5;
//
//	objectCoords[35] = vec3(-1,3,0);
//	objectRadii[35] = 0.5;
//
//	objectCoords[36] = vec3(0,3,0);
//	objectRadii[36] = 0.5;
//
//	objectCoords[37] = vec3(1,3,0);
//	objectRadii[37] = 0.5;
//
//	objectCoords[38] = vec3(2,3,0);
//	objectRadii[38] = 0.5;
//
//	objectCoords[39] = vec3(3,3,0);
//	objectRadii[39] = 0.5;
//
//	objectCoords[40] = vec3(4,3,0);
//	objectRadii[40] = 0.5;
//
//	objectCoords[41] = vec3(-5,7,0);
//	objectRadii[41] = 0.5;
//
//	objectCoords[42] = vec3(-4,7,0);
//	objectRadii[42] = 0.5;
//
//	objectCoords[43] = vec3(-3,7,0);
//	objectRadii[43] = 0.5;
//
//	objectCoords[44] = vec3(-2,7,0);
//	objectRadii[44] = 0.5;
//
//	objectCoords[45] = vec3(-1,7,0);
//	objectRadii[45] = 0.5;

}

//vec3  skysphereCoord	= vec3(0, 0, 0 );
//float skysphereRadius	= 20;

vec3 boundingSpherePos = vec3(0,0,0);
float boundingSphereRad = 0.7;

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


//    dist = smin(dist, dist2, 0.35  );
//
//
//    dist = smin(dist,sdPlane(p, vec4(0, 1.0, 0, 0.0) ) , 0.15 ) ;
//    dist = min(dist,sdPlane(p, vec4(0, 1.0, 0, 0.0) )  ) ;
//    dist = smin(dist, sdBox( p+vec3(4,0.3,3), vec3(30,0.1,30) )	, 1 );
//
//    dist = min(dist, sdSphere( p+vec3(0,3,0), s3Coord, 1.5 ) );
//
//    if(p.y>-1.0005)
//    {
//    	dist += sin(p.y*10  +iGlobalTime )*sin(p.y*11)*0.025;
//    	dist += tan ( cos(p.x*14 +iGlobalTime*5 )*sin(p.y*14 )*sin(p.z*14  +iGlobalTime*5  )*1.30 )*0.0145;
//    	dist += sin(p.z*20 +iGlobalTime*4 )*cos(p.z*31  +iGlobalTime*7)*0.025;
//    }
    return dist;
}


float BoundingSphere(vec3 p){

	float dist = sdSphere(p,boundingSpherePos,boundingSphereRad);
	if (dist > 0.5)
	{
		return dist;
	}
	else
	{
		float dist2 = sdSphere(p,spherePos[0],sphereRadii[0]);
		int i = 1;
		while(i < NumSpheres)
		{
			dist2 = min(dist2, sdSphere(p,spherePos[i],sphereRadii[i]));
			i++;
		}
		return dist2;
	}
}


float DistToObjectId(vec3 p, uint objId)
{
	switch (objId)
	{
		case 1:
			return BoundingSphere(p);
//			return sdSphere(p, s1Coord, s1Radius);
//			return DistToMetaballs(p);
//			return DistToModfield(p);
//
//
//			return sdSphere( p, objectCoords[1], objectRadii[1]);
//		case 2:
//			return sdSphere( p, objectCoords[2], objectRadii[2]);
//		case 3:
//			return sdSphere( p, objectCoords[3], objectRadii[3]);
//		case 4:
//			return sdSphere( p, objectCoords[4], objectRadii[4]);
//		case 5:
//			return sdSphere( p, objectCoords[5], objectRadii[5]);
//		case 6:
//			return sdSphere( p, objectCoords[6], objectRadii[6]);
//		case 7:
//			return sdSphere( p, objectCoords[7], objectRadii[7]);
//		case 8:
//			return sdSphere( p, objectCoords[8], objectRadii[8]);
//		case 9:
//			return sdSphere( p, objectCoords[9], objectRadii[9]);
//		case 10:
//			return sdSphere( p, objectCoords[10], objectRadii[10]);
//		case 11:
//			return sdSphere( p, objectCoords[11], objectRadii[11]);
//		case 12:
//			return sdSphere( p, objectCoords[12], objectRadii[12]);
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
//		case 21:
//			return sdSphere( p, objectCoords[21], objectRadii[21]);
//		case 22:
//			return sdSphere( p, objectCoords[22], objectRadii[22]);
//		case 23:
//			return sdSphere( p, objectCoords[23], objectRadii[23]);
//		case 24:
//			return sdSphere( p, objectCoords[24], objectRadii[24]);
//		case 25:
//			return sdSphere( p, objectCoords[25], objectRadii[25]);
//		case 26:
//			return sdSphere( p, objectCoords[26], objectRadii[26]);
//		case 27:
//			return sdSphere( p, objectCoords[27], objectRadii[27]);
//		case 28:
//			return sdSphere( p, objectCoords[28], objectRadii[28]);
//		case 29:
//			return sdSphere( p, objectCoords[29], objectRadii[29]);
//		case 30:
//			return sdSphere( p, objectCoords[30], objectRadii[30]);
//		case 31:
//			return sdSphere( p, objectCoords[31], objectRadii[31]);
//		case 32:
//			return sdSphere( p, objectCoords[32], objectRadii[32]);
//		case 33:
//			return sdSphere( p, objectCoords[33], objectRadii[33]);
//		case 34:
//			return sdSphere( p, objectCoords[34], objectRadii[34]);
//		case 35:
//			return sdSphere( p, objectCoords[35], objectRadii[35]);
//		case 36:
//			return sdSphere( p, objectCoords[36], objectRadii[36]);
//		case 37:
//			return sdSphere( p, objectCoords[37], objectRadii[37]);
//		case 38:
//			return sdSphere( p, objectCoords[38], objectRadii[38]);
//		case 39:
//			return sdSphere( p, objectCoords[39], objectRadii[39]);
//		case 40:
//			return sdSphere( p, objectCoords[40], objectRadii[40]);
//		case 41:
//			return sdSphere( p, objectCoords[41], objectRadii[41]);
//		case 42:
//			return sdSphere( p, objectCoords[42], objectRadii[42]);
//		case 43:
//			return sdSphere( p, objectCoords[43], objectRadii[43]);
//		case 44:
//			return sdSphere( p, objectCoords[44], objectRadii[44]);
//		case 45:
//			return sdSphere( p, objectCoords[45], objectRadii[45]);




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
