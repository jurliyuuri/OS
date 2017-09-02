function toggle(isJietoden)
{
	var arr = document.getElementsByClassName("jietoden");
	for(var i=0; i<arr.length; i++) {
		arr[i].style.fontFamily = isJietoden ? "jietoden" : "monospace";
	}
}

