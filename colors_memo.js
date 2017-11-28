function foo(str){return [
	parseInt(str.slice(0,2),16),
	parseInt(str.slice(2,4),16),
	parseInt(str.slice(4,6),16)];}

function gen_table(arr,output_id){
var str=`
<table class="t u" style="font-family: monospace; text-align:right;">
<thead>
<tr><td style="text-align:center;">色</td><td>R_srgb</td><td>G_srgb</td><td>B_srgb</td><td>R_linear</td><td>G_linear</td><td>B_linear</td></tr>
</thead>
<tbody>`;
for(var i=0; i<arr.length; i++){
	var f = foo(arr[i]);
	var t = [`<div style="background-color:#`+arr[i]+`">#`+arr[i]+`</div>`,f[0],f[1],f[2],sRGB2linear(f[0]).toFixed(6),sRGB2linear(f[1]).toFixed(6),sRGB2linear(f[2]).toFixed(6)];
	str += '<tr><td>' + t.join('</td><td>') + '</td></tr>';
}

str+="</tbody></table>";
if(output_id){document.getElementById(output_id).innerHTML=str;}else{document.write(str);}

}

function convert_and_draw(A,B,C , D,E,F, G,H,I)
{
	var arrr = [A,B,C , D,E,F, G,H,I].map(function(x){
		var k = normalize_sRGB(x);
		return (k+256).toString(16).slice(1,3);
	});
	gen_table([arrr[0]+arrr[3]+arrr[6],arrr[1]+arrr[4]+arrr[7],arrr[2]+arrr[5]+arrr[8]]);	
}

function toHex(arr){
		return (arr[0]+256).toString(16).slice(1,3) + (arr[1]+256).toString(16).slice(1,3) + (arr[2]+256).toString(16).slice(1,3);
	}

function $gefja()
{
	var jrl = document.getElementById("isg").value/100;
	var jgl = document.getElementById("nkla").value/100;
	var jbl = document.getElementById("gds").value/100;
	var hex = toHex(jRGBlinear2sRGB(jrl,jgl,jbl));
	document.getElementById("njw4").value = hex;
	gen_table([hex],"jndf")
}

function $gbfs()
{
	var [r,g,b] = foo(document.getElementById("thew5").value);
	var [jrl,jgl,jbl] = sRGB2jRGBlinear(r,g,b);
	document.getElementById("gsdfvcx").value = jrl*100;
	document.getElementById("irytr").value = jgl*100;
	document.getElementById("jerd").value = jbl*100;
}
