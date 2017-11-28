function sRGB2linear(num){
	// https://en.wikipedia.org/wiki/SRGB#The_reverse_transformation
	var a = 0.055;
	var c = num/255;
	if(c <= 0.04045) {
		return c/12.92;
	} else {
		return Math.pow((c+a)/(1+a), 2.4);
	}
}

function linear2sRGB(c){
	var a = 0.055;
	if(c <= 0.0031308){
		return 12.92*c*255;
	} else {
		return ((1+a) * Math.pow(c,1/2.4) - a)*255;
	}
}

function normalize_sRGB(x)
{
	var k = Math.round(linear2sRGB(x));
	if(k<0){k=0}
	if(k>255){k=255;}
	return k;
}

function jRGBlinear2sRGB(jrl, jgl, jbl)
{
	var sr = normalize_sRGB(jrl * 7/8 + jgl *     123/1024 + jbl *      5/1024);
	var sg = normalize_sRGB(jrl * 11/2048 + jgl * 127/128 + jbl *    5/2048);
	var sb = normalize_sRGB(jrl * 1/256 + jgl *   1/16 + jbl *  239/256);
	return [sr,sg,sb];
}

function sRGB2jRGBlinear(r,g,b)
{
	var rl = sRGB2linear(r);
	var gl = sRGB2linear(g);
	var bl = sRGB2linear(b);
	return [
		(rl*1942272-gl*234536-bl*9545)/1698191,
		(-rl*10496+gl*1713112-bl*4425)/1698191,
		(-rl*7424-gl*113704+bl*1819319)/1698191];
}
