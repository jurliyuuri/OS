var GLOB = {
	cvs: null,
	ctx: null,
	screen_width: 512,
	screen_height: 360,
	sRGB_table: null,
	image_data_1px: null
}


// code used solely to initialize the VM
function init()
{
	GLOB.cvs = document.getElementById("main");
	GLOB.ctx = GLOB.cvs.getContext("2d");
	GLOB.ctx.fillRect(0, 0, GLOB.screen_width, GLOB.screen_height);
	GLOB.image_data_1px = GLOB.ctx.createImageData(1,1);
	GLOB.sRGB_table = [null, [240, 16, 4], [30, 252, 10], [2, 6, 247]];
}

// doubleplusungood hack
function writeRectangle(x1, y1, x2, y2, color_code)
{   
	GLOB.ctx.fillStyle = "rgb(" + 
		get_sRGB_from_colorCode(color_code)[0] + "," + 
		get_sRGB_from_colorCode(color_code)[1] + "," + 
		get_sRGB_from_colorCode(color_code)[2] + ")";             
	GLOB.ctx.fillRect( x1, y1, x2-x1, y2-y1);
}

function writePixel(x, y, color_code)
{                
	GLOB.image_data_1px.data[0]   = get_sRGB_from_colorCode(color_code)[0];
	GLOB.image_data_1px.data[1]   = get_sRGB_from_colorCode(color_code)[1];
	GLOB.image_data_1px.data[2]   = get_sRGB_from_colorCode(color_code)[2];
	GLOB.image_data_1px.data[3]   = 255;
	GLOB.ctx.putImageData(GLOB.image_data_1px, x, y ); 
}

// VM
function get_sRGB_from_colorCode(color_code)
{
	return GLOB.sRGB_table[color_code].slice(); //cannot be overwritten
}

