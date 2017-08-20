function bar(num){
	// https://en.wikipedia.org/wiki/SRGB#The_reverse_transformation
	var a = 0.055;
	var c = num/255;
	if(c <= 0.04045) {
		return c/12.92;
	} else {
		return Math.pow((c+a)/(1+a), 2.4);
	}
}

function bar2(c){
	var a = 0.055;
	if(c <= 0.0031308){
		return 12.92*c*255;
	} else {
		return ((1+a) * Math.pow(c,1/2.4) - a)*255;
	}
}

function combine(a,b,c,d,e,f){return [bar2(bar(a)+bar(d)), bar2(bar(b)+bar(e)), bar2(bar(c)+bar(f))]}
