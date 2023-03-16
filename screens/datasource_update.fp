// place into init()
function(VARS,element,context){
	function update(){
	    console.log("todo update");
	    element.source().read();
	    context.setTimeout(update, 10000)
	}
	update();
}