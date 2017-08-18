var GLOB = {
	cvs: null,
	ctx: null,
	screen_width: 512,
	screen_height: 360,
	image_data_1px: null
}


// code used solely to initialize the VM
function init()
{
	GLOB.cvs = document.getElementById("main");
	GLOB.ctx = GLOB.cvs.getContext("2d");
	GLOB.ctx.fillRect(0, 0, GLOB.screen_width, GLOB.screen_height);
	GLOB.image_data_1px = GLOB.ctx.createImageData(1,1);
}

// doubleplusungood hack
function writeRectangle(x1, y1, x2, y2, r, g, b)
{   
	GLOB.ctx.fillStyle = "rgb(" + r + "," + g + "," + b + ")";             
	GLOB.ctx.fillRect( x1, y1, x2-x1, y2-y1);
}

function writePixel(x, y, r, g, b)
{                
	GLOB.image_data_1px.data[0]   = r;
	GLOB.image_data_1px.data[1]   = g;
	GLOB.image_data_1px.data[2]   = b;
	GLOB.image_data_1px.data[3]   = 255;
	GLOB.ctx.putImageData(GLOB.image_data_1px, x, y ); 
}


