float sdBox( vec3 p, vec3 b )
{
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) + length(max(d,0.0))-0.25;
}

float sdSphere(vec3 p, vec3 centerPos, float size)
{	return length(p-centerPos)-size;		}

float sdPlane( vec3 p, vec4 n )
{
  // n must be normalized
  return dot(p,n.xyz) + n.w;
}

float sdBox2(vec3 r, vec3 c, vec3 p){
	return max(abs(p.x - c.x) - r.x, max(abs(p.y - c.y) - r.y, abs(p.z - c.z) - r.z));
}
