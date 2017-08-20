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


function jRGBlinear2sRGB(jrl, jgl, jbl)
{
	var sr = linear2sRGB((jrl * 530 + jgl *     72 + jbl *      3)/   605);
	var sg = linear2sRGB((jrl * 961 + jgl * 155937 + jbl *    402)/157300);
	var sb = linear2sRGB((jrl * 661 + jgl *   9936 + jbl * 146703)/157300);
	return [Math.round(sr),sg,sb];
}
