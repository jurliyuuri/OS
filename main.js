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


function writePixel(x, y, r, g, b)
{                
	GLOB.image_data_1px.data[0]   = r;
	GLOB.image_data_1px.data[1]   = g;
	GLOB.image_data_1px.data[2]   = b;
	GLOB.image_data_1px.data[3]   = 255;
	GLOB.ctx.putImageData(GLOB.image_data_1px, x, y ); 
}


function chgCol()
{
  for ( var i = 0 ; i < 9 ; i++ )
  {
    var r = Math.floor(Math.random()*256);
    var g = Math.floor(Math.random()*256);
    var b = Math.floor(Math.random()*256);

    GLOB.ctx.fillStyle = "rgb(" + r + "," + g + "," + b + ")";
    GLOB.ctx.fillRect( 40*i, 0, 40, 240 );
  }
}
